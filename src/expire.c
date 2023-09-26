/* Implementation of EXPIRE (keys with fixed time to live).
 *
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 2009-2016, Salvatore Sanfilippo <antirez at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "server.h"

/*-----------------------------------------------------------------------------
 * Incremental collection of expired keys.
 *
 * When keys are accessed they are expired on-access. However we need a
 * mechanism in order to ensure keys are eventually removed when expired even
 * if no access is performed on them.
 *----------------------------------------------------------------------------*/

/* Helper function for the activeExpireCycle() function.
 * This function will try to expire the key that is stored in the hash table
 * entry 'de' of the 'expires' hash table of a Redis database.
 *
 * If the key is found to be expired, it is removed from the database and
 * 1 is returned. Otherwise no operation is performed and 0 is returned.
 *
 * When a key is expired, server.stat_expiredkeys is incremented.
 *
 * The parameter 'now' is the current time in milliseconds as is passed
 * to the function to avoid too many gettimeofday() syscalls. */
// 此函数尝试淘汰保存在redis数据库的'expires'字典中的hash表元素de中的键
int activeExpireCycleTryExpire(redisDb *db, dictEntry *de, long long now) {
    long long t = dictGetSignedIntegerVal(de);
    if (now > t) {
        sds key = dictGetKey(de);
        robj *keyobj = createStringObject(key,sdslen(key));
        deleteExpiredKeyAndPropagate(db,keyobj);
        decrRefCount(keyobj);
        return 1;
    } else {
        return 0;
    }
}

/* Try to expire a few timed out keys. The algorithm used is adaptive and
 * will use few CPU cycles if there are few expiring keys, otherwise
 * it will get more aggressive to avoid that too much memory is used by
 * keys that can be removed from the keyspace.
 *
 * Every expire cycle tests multiple databases: the next call will start
 * again from the next db. No more than CRON_DBS_PER_CALL databases are
 * tested at every iteration.
 *
 * The function can perform more or less work, depending on the "type"
 * argument. It can execute a "fast cycle" or a "slow cycle". The slow
 * cycle is the main way we collect expired cycles: this happens with
 * the "server.hz" frequency (usually 10 hertz).
 *
 * However the slow cycle can exit for timeout, since it used too much time.
 * For this reason the function is also invoked to perform a fast cycle
 * at every event loop cycle, in the beforeSleep() function. The fast cycle
 * will try to perform less work, but will do it much more often.
 *
 * The following are the details of the two expire cycles and their stop
 * conditions:
 *
 * If type is ACTIVE_EXPIRE_CYCLE_FAST the function will try to run a
 * "fast" expire cycle that takes no longer than ACTIVE_EXPIRE_CYCLE_FAST_DURATION
 * microseconds, and is not repeated again before the same amount of time.
 * The cycle will also refuse to run at all if the latest slow cycle did not
 * terminate because of a time limit condition.
 *
 * If type is ACTIVE_EXPIRE_CYCLE_SLOW, that normal expire cycle is
 * executed, where the time limit is a percentage of the REDIS_HZ period
 * as specified by the ACTIVE_EXPIRE_CYCLE_SLOW_TIME_PERC define. In the
 * fast cycle, the check of every database is interrupted once the number
 * of already expired keys in the database is estimated to be lower than
 * a given percentage, in order to avoid doing too much work to gain too
 * little memory.
 *
 * The configured expire "effort" will modify the baseline parameters in
 * order to do more work in both the fast and slow expire cycles.
 */

#define ACTIVE_EXPIRE_CYCLE_KEYS_PER_LOOP 20 /* Keys for each DB loop. */
#define ACTIVE_EXPIRE_CYCLE_FAST_DURATION 1000 /* Microseconds. */
#define ACTIVE_EXPIRE_CYCLE_SLOW_TIME_PERC 25 /* Max % of CPU to use. */
#define ACTIVE_EXPIRE_CYCLE_ACCEPTABLE_STALE 10 /* % of stale keys after which
                                                   we do extra efforts. */
// 删除过期的键
void activeExpireCycle(int type) {
    /* Adjust the running parameters according to the configured expire
     * effort. The default effort is 1, and the maximum configurable effort
     * is 10. */
    // 根据配置的过期工作量调整运行参数。默认工作量为1，最大可配置工作量为10。
    unsigned long
    effort = server.active_expire_effort-1, /* Rescale from 0 to 9. */
    config_keys_per_loop = ACTIVE_EXPIRE_CYCLE_KEYS_PER_LOOP +
                           ACTIVE_EXPIRE_CYCLE_KEYS_PER_LOOP/4*effort,
    config_cycle_fast_duration = ACTIVE_EXPIRE_CYCLE_FAST_DURATION +
                                 ACTIVE_EXPIRE_CYCLE_FAST_DURATION/4*effort,
    config_cycle_slow_time_perc = ACTIVE_EXPIRE_CYCLE_SLOW_TIME_PERC +
                                  2*effort,
    config_cycle_acceptable_stale = ACTIVE_EXPIRE_CYCLE_ACCEPTABLE_STALE-
                                    effort;

    /* This function has some global state in order to continue the work
     * incrementally across calls. */
    // 这个函数有一些用于多次调用时继续工作的全局状态
    // 下一个测试的数据库
    static unsigned int current_db = 0; /* Next DB to test. */
    // 上一次调用是否达到了时间上限
    static int timelimit_exit = 0;      /* Time limit hit in previous call? */
    // 上次快速循环运行的时间
    static long long last_fast_cycle = 0; /* When last fast cycle ran. */

    int j, iteration = 0;
    int dbs_per_call = CRON_DBS_PER_CALL;
    long long start = ustime(), timelimit, elapsed;

    /* If 'expire' action is paused, for whatever reason, then don't expire any key.
     * Typically, at the end of the pause we will properly expire the key OR we
     * will have failed over and the new primary will send us the expire. */
    // 如果由于任何原因暂停了“过期”操作，则不要使任何key过期。通常，在暂停结束时，我们将使密钥正确过期，或者我们将进行故障转移，新的主密钥将向我们发送过期信息。
    if (isPausedActionsWithUpdate(PAUSE_ACTION_EXPIRE)) return;

    // 快速过期键删除
    if (type == ACTIVE_EXPIRE_CYCLE_FAST) {
        /* Don't start a fast cycle if the previous cycle did not exit
         * for time limit, unless the percentage of estimated stale keys is
         * too high. Also never repeat a fast cycle for the same period
         * as the fast cycle total duration itself. */
        // 如果前一个循环没有在时间限制内退出，则不要启动快速循环，
        // 除非估计的过期密钥的百分比过高。
        if (!timelimit_exit &&
            server.stat_expired_stale_perc < config_cycle_acceptable_stale)
            return;

        // 也不要在与快速循环总持续时间相同的周期内重复快速循环。
        if (start < last_fast_cycle + (long long)config_cycle_fast_duration*2)
            return;

        last_fast_cycle = start;
    }

    /* We usually should test CRON_DBS_PER_CALL per iteration, with
     * two exceptions:
     * 我们应该每一次遍历都测试CRON_DBS_PER_CALL，主要是两个期望：
     * 
     * 1) Don't test more DBs than we have.
     * 不要测试数据库的数目比我们拥有的还多
     * 
     * 2) If last time we hit the time limit, we want to scan all DBs
     * in this iteration, as there is work to do in some DB and we don't want
     * expired keys to use memory for too much time. 
     * 如果上一次我们达到了时间限制，在这一次遍历中我们需要扫描所有的数据库。
     * 因为在某些数据库中还有工作要做，而且我们不希望清理占用内存的过期key太多时间。
     * */
    if (dbs_per_call > server.dbnum || timelimit_exit)
        dbs_per_call = server.dbnum;

    /* We can use at max 'config_cycle_slow_time_perc' percentage of CPU
     * time per iteration. Since this function gets called with a frequency of
     * server.hz times per second, the following is the max amount of
     * microseconds we can spend in this function. */
    // 我们可以使用每次迭代CPU时间的最大百分比“config_cycle_slow_time_perc”。
    // 由于此函数的调用频率为每秒server.hz次，因此以下是我们在该函数中可以花费的最大微秒数。
    timelimit = config_cycle_slow_time_perc*1000000/server.hz/100;
    timelimit_exit = 0;
    // 至少1微秒
    if (timelimit <= 0) timelimit = 1;

    // 快速过期键删除，使用快速的周期
    if (type == ACTIVE_EXPIRE_CYCLE_FAST)
        timelimit = config_cycle_fast_duration; /* in microseconds. */

    /* Accumulate some global stats as we expire keys, to have some idea
     * about the number of keys that are already logically expired, but still
     * existing inside the database. */
    // key过期时积累一些全局统计数据，以了解逻辑上已经过期但仍存在于数据库中的key的数量。
    long total_sampled = 0;
    long total_expired = 0;

    /* Try to smoke-out bugs (server.also_propagate should be empty here) */
    serverAssert(server.also_propagate.numops == 0);
    // 最大遍历dbs_per_call个数据库
    for (j = 0; j < dbs_per_call && timelimit_exit == 0; j++) {
        /* Expired and checked in a single loop. */
        // key过期和检查在一个简单的循环
        unsigned long expired, sampled;
        // 得到当前遍历的数据库
        redisDb *db = server.db+(current_db % server.dbnum);

        /* Increment the DB now so we are sure if we run out of time
         * in the current DB we'll restart from the next. This allows to
         * distribute the time evenly across DBs. */
        // 现在增加数据库，这样我们就可以确定，如果当前数据库的时间用完，
        // 我们将从下一个数据库重新启动。这允许在数据库之间均匀地分配时间。
        // 注意这里不是必须处理完当前才能进入下一个
        current_db++;

        /* Continue to expire if at the end of the cycle there are still
         * a big percentage of keys to expire, compared to the number of keys
         * we scanned. The percentage, stored in config_cycle_acceptable_stale
         * is not fixed, but depends on the Redis configured "expire effort". */
        // 如果在循环的结尾还是有很大百分比的key需要去淘汰那就继续淘汰，和我们扫描的键的数量相比。
        // 根据redis配置的"expire effort"，保存在config_cycle_acceptable_stale的百分比不是固定的
        do {
            unsigned long num, slots;
            long long now, ttl_sum;
            int ttl_samples;
            iteration++;

            /* If there is nothing to expire try next DB ASAP. */
            // 如果当前的数据库没有过期的key，就尽快进入下一个数据库
            if ((num = dictSize(db->expires)) == 0) {
                db->avg_ttl = 0;
                break;
            }
            // 槽位的数量
            slots = dictSlots(db->expires);
            now = mstime();

            /* When there are less than 1% filled slots, sampling the key
             * space is expensive, so stop here waiting for better times...
             * The dictionary will be resized asap. */
            // 如果小于1%的槽位填充了数据，对键空间的采样是昂贵的，所以直接略过以等到更好的时候处理...
            // 该字典会尽快重新调整大小
            if (slots > DICT_HT_INITIAL_SIZE &&
                (num*100/slots < 1)) break;

            /* The main collection cycle. Sample random keys among keys
             * with an expire set, checking for expired ones. */
            // 主要的收集循环，在过期键集合中随机采样键，检查过期的key。
            expired = 0;
            sampled = 0;
            ttl_sum = 0;
            ttl_samples = 0;

            // 数目最大不能大于配置的每次循环处理的键的数目
            if (num > config_keys_per_loop)
                num = config_keys_per_loop;

            /* Here we access the low level representation of the hash table
             * for speed concerns: this makes this code coupled with dict.c,
             * but it hardly changed in ten years.
             * 在这里出于速度的考虑我们需要访问hash table的低级别表示：这导致这些代码需要
             * 和dict.c结合在一起，但是它基本上十年来都没什么改变。
             * 
             * Note that certain places of the hash table may be empty,
             * so we want also a stop condition about the number of
             * buckets that we scanned. However scanning for free buckets
             * is very fast: we are in the cache line scanning a sequential
             * array of NULL pointers, so we can scan a lot more buckets
             * than keys in the same time. 
             * 注意：某些地方的hash表可能是空的，所以我们需要一个关于扫描槽位数的停止条件。
             * 然而扫描空闲的槽位是很快的：我们在缓存行中扫描一个NULL指针的顺序数组。
             * 所以我们可以同时扫描比键多得多的槽位。
             * */
            long max_buckets = num*20;
            long checked_buckets = 0;

            // 扫描到满足条件
            while (sampled < num && checked_buckets < max_buckets) {
                for (int table = 0; table < 2; table++) {
                    // 如果不在重新Hash中就不需要扫描table 1
                    if (table == 1 && !dictIsRehashing(db->expires)) break;

                    unsigned long idx = db->expires_cursor;
                    idx &= DICTHT_SIZE_MASK(db->expires->ht_size_exp[table]);
                    // 得到对应的元素
                    dictEntry *de = db->expires->ht_table[table][idx];
                    long long ttl;

                    /* Scan the current bucket of the current table. */
                    // 扫描当前table中的当前槽位
                    checked_buckets++;
                    while(de) {
                        /* Get the next entry now since this entry may get
                         * deleted. */
                        dictEntry *e = de;
                        de = de->next;
                        ttl = dictGetSignedIntegerVal(e)-now;
                         // 淘汰单个的键
                        if (activeExpireCycleTryExpire(db,e,now)) {
                            // 增加淘汰数目
                            expired++;
                            /* Propagate the DEL command */
                            // 传播删除命令（DEL）
                            postExecutionUnitOperations();
                        }
                        // 统计TTL
                        if (ttl > 0) {
                            /* We want the average TTL of keys yet
                             * not expired. */
                            ttl_sum += ttl;
                            ttl_samples++;
                        }
                        sampled++;
                    }
                }
                // 增加过期的游标
                db->expires_cursor++;
            }
            total_expired += expired;
            total_sampled += sampled;

            /* Update the average TTL stats for this database. */
            // 更新数据库的平均TTL统计
            if (ttl_samples) {
                long long avg_ttl = ttl_sum/ttl_samples;

                /* Do a simple running average with a few samples.
                 * We just use the current estimate with a weight of 2%
                 * and the previous estimate with a weight of 98%. */
                if (db->avg_ttl == 0) db->avg_ttl = avg_ttl;
                db->avg_ttl = (db->avg_ttl/50)*49 + (avg_ttl/50);
            }

            /* We can't block forever here even if there are many keys to
             * expire. So after a given amount of milliseconds return to the
             * caller waiting for the other active expire cycle. */
            // 每16个槽位遍历完检查一下时间，如果时间超过了，就终止
            if ((iteration & 0xf) == 0) { /* check once every 16 iterations. */
                elapsed = ustime()-start;
                if (elapsed > timelimit) {
                    timelimit_exit = 1;
                    server.stat_expired_time_cap_reached_count++;
                    break;
                }
            }
            /* We don't repeat the cycle for the current database if there are
             * an acceptable amount of stale keys (logically expired but yet
             * not reclaimed). */
            // 如果存在可接受数量的过期keys（逻辑上已过期但尚未回收），我们不会对当前数据库重复该循环。
        } while (sampled == 0 ||
                 (expired*100/sampled) > config_cycle_acceptable_stale);
    }
    // 这次淘汰键使用的事件
    elapsed = ustime()-start;
    // 增加累积的淘汰键使用的时间
    server.stat_expire_cycle_time_used += elapsed;
    latencyAddSampleIfNeeded("expire-cycle",elapsed/1000);

    /* Update our estimate of keys existing but yet to be expired.
     * Running average with this sample accounting for 5%. */
    // 更新我们估计的已经过去但是还在数据库的键数量，
    double current_perc;
    if (total_sampled) {
        current_perc = (double)total_expired/total_sampled;
    } else
        current_perc = 0;
    // 可能过期的key的百分比计算，本次占比重为5%
    server.stat_expired_stale_perc = (current_perc*0.05)+
                                     (server.stat_expired_stale_perc*0.95);
}

/*-----------------------------------------------------------------------------
 * Expires of keys created in writable slaves
 *
 * Normally slaves do not process expires: they wait the masters to synthesize
 * DEL operations in order to retain consistency. However writable slaves are
 * an exception: if a key is created in the slave and an expire is assigned
 * to it, we need a way to expire such a key, since the master does not know
 * anything about such a key.
 *
 * In order to do so, we track keys created in the slave side with an expire
 * set, and call the expireSlaveKeys() function from time to time in order to
 * reclaim the keys if they already expired.
 *
 * Note that the use case we are trying to cover here, is a popular one where
 * slaves are put in writable mode in order to compute slow operations in
 * the slave side that are mostly useful to actually read data in a more
 * processed way. Think at sets intersections in a tmp key, with an expire so
 * that it is also used as a cache to avoid intersecting every time.
 *
 * This implementation is currently not perfect but a lot better than leaking
 * the keys as implemented in 3.2.
 *----------------------------------------------------------------------------*/

/* The dictionary where we remember key names and database ID of keys we may
 * want to expire from the slave. Since this function is not often used we
 * don't even care to initialize the database at startup. We'll do it once
 * the feature is used the first time, that is, when rememberSlaveKeyWithExpire()
 * is called.
 *
 * The dictionary has an SDS string representing the key as the hash table
 * key, while the value is a 64 bit unsigned integer with the bits corresponding
 * to the DB where the keys may exist set to 1. Currently the keys created
 * with a DB id > 63 are not expired, but a trivial fix is to set the bitmap
 * to the max 64 bit unsigned value when we know there is a key with a DB
 * ID greater than 63, and check all the configured DBs in such a case. */
dict *slaveKeysWithExpire = NULL;

/* Check the set of keys created by the master with an expire set in order to
 * check if they should be evicted. */
void expireSlaveKeys(void) {
    if (slaveKeysWithExpire == NULL ||
        dictSize(slaveKeysWithExpire) == 0) return;

    int cycles = 0, noexpire = 0;
    mstime_t start = mstime();
    while(1) {
        dictEntry *de = dictGetRandomKey(slaveKeysWithExpire);
        sds keyname = dictGetKey(de);
        uint64_t dbids = dictGetUnsignedIntegerVal(de);
        uint64_t new_dbids = 0;

        /* Check the key against every database corresponding to the
         * bits set in the value bitmap. */
        int dbid = 0;
        while(dbids && dbid < server.dbnum) {
            if ((dbids & 1) != 0) {
                redisDb *db = server.db+dbid;
                dictEntry *expire = dictFind(db->expires,keyname);
                int expired = 0;

                if (expire &&
                    activeExpireCycleTryExpire(server.db+dbid,expire,start))
                {
                    expired = 1;
                    /* DELs aren't propagated, but modules may want their hooks. */
                    postExecutionUnitOperations();
                }

                /* If the key was not expired in this DB, we need to set the
                 * corresponding bit in the new bitmap we set as value.
                 * At the end of the loop if the bitmap is zero, it means we
                 * no longer need to keep track of this key. */
                if (expire && !expired) {
                    noexpire++;
                    new_dbids |= (uint64_t)1 << dbid;
                }
            }
            dbid++;
            dbids >>= 1;
        }

        /* Set the new bitmap as value of the key, in the dictionary
         * of keys with an expire set directly in the writable slave. Otherwise
         * if the bitmap is zero, we no longer need to keep track of it. */
        if (new_dbids)
            dictSetUnsignedIntegerVal(de,new_dbids);
        else
            dictDelete(slaveKeysWithExpire,keyname);

        /* Stop conditions: found 3 keys we can't expire in a row or
         * time limit was reached. */
        cycles++;
        if (noexpire > 3) break;
        if ((cycles % 64) == 0 && mstime()-start > 1) break;
        if (dictSize(slaveKeysWithExpire) == 0) break;
    }
}

/* Track keys that received an EXPIRE or similar command in the context
 * of a writable slave. */
void rememberSlaveKeyWithExpire(redisDb *db, robj *key) {
    if (slaveKeysWithExpire == NULL) {
        static dictType dt = {
            dictSdsHash,                /* hash function */
            NULL,                       /* key dup */
            NULL,                       /* val dup */
            dictSdsKeyCompare,          /* key compare */
            dictSdsDestructor,          /* key destructor */
            NULL,                       /* val destructor */
            NULL                        /* allow to expand */
        };
        slaveKeysWithExpire = dictCreate(&dt);
    }
    if (db->id > 63) return;

    dictEntry *de = dictAddOrFind(slaveKeysWithExpire,key->ptr);
    /* If the entry was just created, set it to a copy of the SDS string
     * representing the key: we don't want to need to take those keys
     * in sync with the main DB. The keys will be removed by expireSlaveKeys()
     * as it scans to find keys to remove. */
    if (de->key == key->ptr) {
        de->key = sdsdup(key->ptr);
        dictSetUnsignedIntegerVal(de,0);
    }

    uint64_t dbids = dictGetUnsignedIntegerVal(de);
    dbids |= (uint64_t)1 << db->id;
    dictSetUnsignedIntegerVal(de,dbids);
}

/* Return the number of keys we are tracking. */
size_t getSlaveKeyWithExpireCount(void) {
    if (slaveKeysWithExpire == NULL) return 0;
    return dictSize(slaveKeysWithExpire);
}

/* Remove the keys in the hash table. We need to do that when data is
 * flushed from the server. We may receive new keys from the master with
 * the same name/db and it is no longer a good idea to expire them.
 *
 * Note: technically we should handle the case of a single DB being flushed
 * but it is not worth it since anyway race conditions using the same set
 * of key names in a writable slave and in its master will lead to
 * inconsistencies. This is just a best-effort thing we do. */
void flushSlaveKeysWithExpireList(void) {
    if (slaveKeysWithExpire) {
        dictRelease(slaveKeysWithExpire);
        slaveKeysWithExpire = NULL;
    }
}

int checkAlreadyExpired(long long when) {
    /* EXPIRE with negative TTL, or EXPIREAT with a timestamp into the past
     * should never be executed as a DEL when load the AOF or in the context
     * of a slave instance.
     *
     * Instead we add the already expired key to the database with expire time
     * (possibly in the past) and wait for an explicit DEL from the master. */
    return (when <= commandTimeSnapshot() && !server.loading && !server.masterhost);
}

#define EXPIRE_NX (1<<0)
#define EXPIRE_XX (1<<1)
#define EXPIRE_GT (1<<2)
#define EXPIRE_LT (1<<3)

/* Parse additional flags of expire commands
 *
 * Supported flags:
 * - NX: set expiry only when the key has no expiry
 * - XX: set expiry only when the key has an existing expiry
 * - GT: set expiry only when the new expiry is greater than current one
 * - LT: set expiry only when the new expiry is less than current one */
int parseExtendedExpireArgumentsOrReply(client *c, int *flags) {
    int nx = 0, xx = 0, gt = 0, lt = 0;

    int j = 3;
    while (j < c->argc) {
        char *opt = c->argv[j]->ptr;
        if (!strcasecmp(opt,"nx")) {
            *flags |= EXPIRE_NX;
            nx = 1;
        } else if (!strcasecmp(opt,"xx")) {
            *flags |= EXPIRE_XX;
            xx = 1;
        } else if (!strcasecmp(opt,"gt")) {
            *flags |= EXPIRE_GT;
            gt = 1;
        } else if (!strcasecmp(opt,"lt")) {
            *flags |= EXPIRE_LT;
            lt = 1;
        } else {
            addReplyErrorFormat(c, "Unsupported option %s", opt);
            return C_ERR;
        }
        j++;
    }

    if ((nx && xx) || (nx && gt) || (nx && lt)) {
        addReplyError(c, "NX and XX, GT or LT options at the same time are not compatible");
        return C_ERR;
    }

    if (gt && lt) {
        addReplyError(c, "GT and LT options at the same time are not compatible");
        return C_ERR;
    }

    return C_OK;
}

/*-----------------------------------------------------------------------------
 * Expires Commands
 *----------------------------------------------------------------------------*/

/* This is the generic command implementation for EXPIRE, PEXPIRE, EXPIREAT
 * and PEXPIREAT. Because the command second argument may be relative or absolute
 * the "basetime" argument is used to signal what the base time is (either 0
 * for *AT variants of the command, or the current time for relative expires).
 *
 * unit is either UNIT_SECONDS or UNIT_MILLISECONDS, and is only used for
 * the argv[2] parameter. The basetime is always specified in milliseconds.
 *
 * Additional flags are supported and parsed via parseExtendedExpireArguments */

// 这是EXPIRE,PEXPIRE,EXPIREAT和PEXPIREAT的通用命令实现。因为第二个参数可能是相对的或者上绝对的，
// basetime参数用于基于什么时间（基于0用于AT结尾的命令，相对的过期时间就是基于当前时间）
void expireGenericCommand(client *c, long long basetime, int unit) {
    robj *key = c->argv[1], *param = c->argv[2];
    long long when; /* unix time in milliseconds when the key will expire. */
    long long current_expire = -1;
    int flag = 0;

    /* checking optional flags */
    // 检查选项标志
    if (parseExtendedExpireArgumentsOrReply(c, &flag) != C_OK) {
        return;
    }

    // 得到传入的时间相关的参数
    if (getLongLongFromObjectOrReply(c, param, &when, NULL) != C_OK)
        return;

    /* EXPIRE allows negative numbers, but we can at least detect an
     * overflow by either unit conversion or basetime addition. */
    // EXPIRE允许负数,但我们至少可以通过单位转换或basetime加法检测溢出
    if (unit == UNIT_SECONDS) {
        // 检测溢出
        if (when > LLONG_MAX / 1000 || when < LLONG_MIN / 1000) {
            addReplyErrorExpireTime(c);
            return;
        }
        when *= 1000;
    }

    // when时间太大
    if (when > LLONG_MAX - basetime) {
        addReplyErrorExpireTime(c);
        return;
    }
    // 得到最终时间
    when += basetime;

    /* No key, return zero. */
    // 没有对应的键值，返回0
    if (lookupKeyWrite(c->db,key) == NULL) {
        addReply(c,shared.czero);
        return;
    }

    // 设置了标志
    if (flag) {
        // 得到目前的过期时间
        current_expire = getExpire(c->db, key);

        /* NX option is set, check current expiry */
        // 如果NX选项被设置，如果以前设置过过期时间，就返回0
        if (flag & EXPIRE_NX) {
            if (current_expire != -1) {
                addReply(c,shared.czero);
                return;
            }
        }

        /* XX option is set, check current expiry */
        // 如果XX选项被设置，如果以前没有设置过过期时间，就返回0
        if (flag & EXPIRE_XX) {
            if (current_expire == -1) {
                /* reply 0 when the key has no expiry */
                addReply(c,shared.czero);
                return;
            }
        }

        /* GT option is set, check current expiry */
        // 如果GT选项设置，如果设置的时间比当前的还小，或者以前本来没有过期时间，返回0
        if (flag & EXPIRE_GT) {
            /* When current_expire is -1, we consider it as infinite TTL,
             * so expire command with gt always fail the GT. */
            if (when <= current_expire || current_expire == -1) {
                /* reply 0 when the new expiry is not greater than current */
                addReply(c,shared.czero);
                return;
            }
        }

        /* LT option is set, check current expiry */
        // 如果LT选项设置了，如果设置的时间比当前的过期时间还大，返回0
        if (flag & EXPIRE_LT) {
            /* When current_expire -1, we consider it as infinite TTL,
             * but 'when' can still be negative at this point, so if there is
             * an expiry on the key and it's not less than current, we fail the LT. */
            if (current_expire != -1 && when >= current_expire) {
                /* reply 0 when the new expiry is not less than current */
                addReply(c,shared.czero);
                return;
            }
        }
    }

    // 如果过期时间以及到了
    if (checkAlreadyExpired(when)) {
        robj *aux;
        // 删除
        int deleted = dbGenericDelete(c->db,key,server.lazyfree_lazy_expire,DB_FLAG_KEY_EXPIRED);
        serverAssertWithInfo(c,key,deleted);
        // 增加脏键的数量
        server.dirty++;

        /* Replicate/AOF this as an explicit DEL or UNLINK. */
        // 以一个显式的DEL或者UNLINK复制Relicate/AOF
        aux = server.lazyfree_lazy_expire ? shared.unlink : shared.del;
        rewriteClientCommandVector(c,2,aux,key);
        signalModifiedKey(c,c->db,key);
        notifyKeyspaceEvent(NOTIFY_GENERIC,"del",key,c->db->id);
        addReply(c, shared.cone);
        return;
    } else {
        // 向redisdb的expires字典里添加键值对
        setExpire(c,c->db,key,when);
        addReply(c,shared.cone);
        /* Propagate as PEXPIREAT millisecond-timestamp
         * Only rewrite the command arg if not already PEXPIREAT */
        if (c->cmd->proc != pexpireatCommand) {
            rewriteClientCommandArgument(c,0,shared.pexpireat);
        }

        /* Avoid creating a string object when it's the same as argv[2] parameter  */
        if (basetime != 0 || unit == UNIT_SECONDS) {
            robj *when_obj = createStringObjectFromLongLong(when);
            rewriteClientCommandArgument(c,2,when_obj);
            decrRefCount(when_obj);
        }
        // 修改键的信号
        signalModifiedKey(c,c->db,key);
        // 通知键空间的事件
        notifyKeyspaceEvent(NOTIFY_GENERIC,"expire",key,c->db->id);
        // 增加脏键数量
        server.dirty++;
        return;
    }
}

/* EXPIRE key seconds [ NX | XX | GT | LT] */
void expireCommand(client *c) {
    expireGenericCommand(c,commandTimeSnapshot(),UNIT_SECONDS);
}

/* EXPIREAT key unix-time-seconds [ NX | XX | GT | LT] */
void expireatCommand(client *c) {
    expireGenericCommand(c,0,UNIT_SECONDS);
}

/* PEXPIRE key milliseconds [ NX | XX | GT | LT] */
void pexpireCommand(client *c) {
    expireGenericCommand(c,commandTimeSnapshot(),UNIT_MILLISECONDS);
}

/* PEXPIREAT key unix-time-milliseconds [ NX | XX | GT | LT] */
void pexpireatCommand(client *c) {
    expireGenericCommand(c,0,UNIT_MILLISECONDS);
}

/* Implements TTL, PTTL, EXPIRETIME and PEXPIRETIME */
// 实现TTL，PTTL，EXPIRETIME和PEXPIRETIME
void ttlGenericCommand(client *c, int output_ms, int output_abs) {
    long long expire, ttl = -1;

    /* If the key does not exist at all, return -2 */
    // 如果键压根就不存在，返回-2
    if (lookupKeyReadWithFlags(c->db,c->argv[1],LOOKUP_NOTOUCH) == NULL) {
        addReplyLongLong(c,-2);
        return;
    }

    /* The key exists. Return -1 if it has no expire, or the actual
     * TTL value otherwise. */
    // 如果键存储，但是没有过期时间，就返回-1，或者返回真正的TTL值
    expire = getExpire(c->db,c->argv[1]);
    if (expire != -1) {
        // 计算时间
        ttl = output_abs ? expire : expire-commandTimeSnapshot();
        if (ttl < 0) ttl = 0;
    }
    // 没有设置过期时间，就返回-1
    if (ttl == -1) {
        addReplyLongLong(c,-1);
    } else {
        // 如果输出为ms，那就进行四舍五入
        addReplyLongLong(c,output_ms ? ttl : ((ttl+500)/1000));
    }
}

/* TTL key */
void ttlCommand(client *c) {
    ttlGenericCommand(c, 0, 0);
}

/* PTTL key */
void pttlCommand(client *c) {
    ttlGenericCommand(c, 1, 0);
}

/* EXPIRETIME key */
void expiretimeCommand(client *c) {
    ttlGenericCommand(c, 0, 1);
}

/* PEXPIRETIME key */
void pexpiretimeCommand(client *c) {
    ttlGenericCommand(c, 1, 1);
}

/* PERSIST key */
// persist命令
void persistCommand(client *c) {
    // 为写操作查找key的对象
    if (lookupKeyWrite(c->db,c->argv[1])) {
        // 从过期字典里删除key的过期时间
        if (removeExpire(c->db,c->argv[1])) {
            // 调用键修改的钩子函数
            signalModifiedKey(c,c->db,c->argv[1]);
            // 通知键空间事件
            notifyKeyspaceEvent(NOTIFY_GENERIC,"persist",c->argv[1],c->db->id);
            addReply(c,shared.cone);
            server.dirty++;
        } else {
            addReply(c,shared.czero);
        }
    } else {
        addReply(c,shared.czero);
    }
}

/* TOUCH key1 [key2 key3 ... keyN] */
void touchCommand(client *c) {
    int touched = 0;
    for (int j = 1; j < c->argc; j++)
        if (lookupKeyRead(c->db,c->argv[j]) != NULL) touched++;
    addReplyLongLong(c,touched);
}
