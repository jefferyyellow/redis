/* Hash Tables Implementation.
 *
 * This file implements in memory hash tables with insert/del/replace/find/
 * get-random-element operations. Hash tables will auto resize if needed
 * tables of power of two in size are used, collisions are handled by
 * chaining. See the source code for more information... :)
 *
 * Copyright (c) 2006-2012, Salvatore Sanfilippo <antirez at gmail dot com>
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

#include "fmacros.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>
#include <sys/time.h>

#include "dict.h"
#include "zmalloc.h"
#include "redisassert.h"

/* Using dictEnableResize() / dictDisableResize() we make possible to
 * enable/disable resizing of the hash table as needed. This is very important
 * for Redis, as we use copy-on-write and don't want to move too much memory
 * around when there is a child performing saving operations.
 *
 * Note that even when dict_can_resize is set to 0, not all resizes are
 * prevented: a hash table is still allowed to grow if the ratio between
 * the number of elements and the buckets > dict_force_resize_ratio. */
static int dict_can_resize = 1;
static unsigned int dict_force_resize_ratio = 5;

/* -------------------------- private prototypes ---------------------------- */

static int _dictExpandIfNeeded(dict *d);
static signed char _dictNextExp(unsigned long size);
static long _dictKeyIndex(dict *d, const void *key, uint64_t hash, dictEntry **existing);
static int _dictInit(dict *d, dictType *type);

/* -------------------------- hash functions -------------------------------- */

static uint8_t dict_hash_function_seed[16];

void dictSetHashFunctionSeed(uint8_t *seed) {
    memcpy(dict_hash_function_seed,seed,sizeof(dict_hash_function_seed));
}

uint8_t *dictGetHashFunctionSeed(void) {
    return dict_hash_function_seed;
}

/* The default hashing function uses SipHash implementation
 * in siphash.c. */

uint64_t siphash(const uint8_t *in, const size_t inlen, const uint8_t *k);
uint64_t siphash_nocase(const uint8_t *in, const size_t inlen, const uint8_t *k);

uint64_t dictGenHashFunction(const void *key, size_t len) {
    return siphash(key,len,dict_hash_function_seed);
}

uint64_t dictGenCaseHashFunction(const unsigned char *buf, size_t len) {
    return siphash_nocase(buf,len,dict_hash_function_seed);
}

/* ----------------------------- API implementation ------------------------- */

/* Reset hash table parameters already initialized with _dictInit()*/
// 重置Hashtable相关的参数，htidx：两个hash table的索引[0,1]
static void _dictReset(dict *d, int htidx)
{
    d->ht_table[htidx] = NULL;
    d->ht_size_exp[htidx] = -1;
    d->ht_used[htidx] = 0;
}

/* Create a new hash table */
// 创建指定类型的字典
dict *dictCreate(dictType *type)
{
    dict *d = zmalloc(sizeof(*d));

    _dictInit(d,type);
    return d;
}

/* Initialize the hash table */
// 初始化字典
int _dictInit(dict *d, dictType *type)
{
    // 重置两个Hash table
    _dictReset(d, 0);
    _dictReset(d, 1);
    d->type = type;
    d->rehashidx = -1;
    d->pauserehash = 0;
    return DICT_OK;
}

/* Resize the table to the minimal size that contains all the elements,
 * but with the invariant of a USED/BUCKETS ratio near to <= 1 */
 // 重新设置哈希表的大小到最小尺寸以至于能容纳所有的元素，
 // 但USED/BUCKETS比率的不变性接近<=1
int dictResize(dict *d)
{
    unsigned long minimal;
    // 如果不能重新设置大小，或者字典正在重新hash
    if (!dict_can_resize || dictIsRehashing(d)) return DICT_ERR;
    // 至少要能比现在已经用的大
    minimal = d->ht_used[0];
    // 能不能小于最小的尺寸
    if (minimal < DICT_HT_INITIAL_SIZE)
        minimal = DICT_HT_INITIAL_SIZE;
    return dictExpand(d, minimal);
}

/* Expand or create the hash table,
 * when malloc_failed is non-NULL, it'll avoid panic if malloc fails (in which case it'll be set to 1).
 * Returns DICT_OK if expand was performed, and DICT_ERR if skipped. */
// 创建或者扩展Hash表，如果malloc_failed不为NULL，如果分配失败用来避免异常（那种情况它将会设置为1）
// 如果扩展成功返回DICT_OK，如果跳过就返回DICT_ERR
int _dictExpand(dict *d, unsigned long size, int* malloc_failed)
{
    if (malloc_failed) *malloc_failed = 0;

    /* the size is invalid if it is smaller than the number of
     * elements already inside the hash table */
    // 如果大小比现在的已经在哈希表的数目还小，那就是非法的
    if (dictIsRehashing(d) || d->ht_used[0] > size)
        return DICT_ERR;

    /* the new hash table */
    // 新的Hash表
    dictEntry **new_ht_table;
    unsigned long new_ht_used;
    // 计算新的Hash表尺寸
    signed char new_ht_size_exp = _dictNextExp(size);

    /* Detect overflows */
    // 得到新的大小
    size_t newsize = 1ul<<new_ht_size_exp;
    // 如果溢出，直接返回错误
    if (newsize < size || newsize * sizeof(dictEntry*) < newsize)
        return DICT_ERR;

    /* Rehashing to the same table size is not useful. */
    // 重新哈希到相同的尺寸就直接返回DICT_ERR了
    if (new_ht_size_exp == d->ht_size_exp[0]) return DICT_ERR;

    /* Allocate the new hash table and initialize all pointers to NULL */
    // 分配新的Hash表并且初始化所有的指针到NULL
    if (malloc_failed) {
        new_ht_table = ztrycalloc(newsize*sizeof(dictEntry*));
        *malloc_failed = new_ht_table == NULL;
        if (*malloc_failed)
            return DICT_ERR;
    } else
        new_ht_table = zcalloc(newsize*sizeof(dictEntry*));

    new_ht_used = 0;

    /* Is this the first initialization? If so it's not really a rehashing
     * we just set the first hash table so that it can accept keys. */
    // 如果是初始化，不是重新哈希，只需要设置第一个哈希表就行
    if (d->ht_table[0] == NULL) {
        d->ht_size_exp[0] = new_ht_size_exp;
        d->ht_used[0] = new_ht_used;
        d->ht_table[0] = new_ht_table;
        return DICT_OK;
    }

    /* Prepare a second hash table for incremental rehashing */
    // 设置到第二个表并且开始增量重新Hash
    d->ht_size_exp[1] = new_ht_size_exp;
    d->ht_used[1] = new_ht_used;
    d->ht_table[1] = new_ht_table;
    // 设置重新开始的索引
    d->rehashidx = 0;
    return DICT_OK;
}

/* return DICT_ERR if expand was not performed */
// 将字典扩展到指定大小，如果不成功返回DICT_ERR
int dictExpand(dict *d, unsigned long size) {
    return _dictExpand(d, size, NULL);
}

/* return DICT_ERR if expand failed due to memory allocation failure */
// 如果由于内存分配失败导致的扩展失败就返回DICT_ERR
int dictTryExpand(dict *d, unsigned long size) {
    int malloc_failed;
    _dictExpand(d, size, &malloc_failed);
    return malloc_failed? DICT_ERR : DICT_OK;
}

/* Performs N steps of incremental rehashing. Returns 1 if there are still
 * keys to move from the old to the new hash table, otherwise 0 is returned.
 * 执行N步增量重新哈希。 如果仍有键从旧哈希表移动到新哈希表，则返回1，否则返回0。
 * Note that a rehashing step consists in moving a bucket (that may have more
 * than one key as we use chaining) from the old to the new hash table, however
 * since part of the hash table may be composed of empty spaces, it is not
 * guaranteed that this function will rehash even a single bucket, since it
 * will visit at max N*10 empty buckets in total, otherwise the amount of
 * work it does would be unbound and the function may block for a long time. */
int dictRehash(dict *d, int n) {
    // 访问最多的空桶位
    int empty_visits = n*10; /* Max number of empty buckets to visit. */
    // 不是重新Hash状态就直接返回0
    if (!dictIsRehashing(d)) return 0;
    // 只执行N步，并且哈希表中还有数据
    while(n-- && d->ht_used[0] != 0) {
        dictEntry *de, *nextde;

        /* Note that rehashidx can't overflow as we are sure there are more
         * elements because ht[0].used != 0 */
        assert(DICTHT_SIZE(d->ht_size_exp[0]) > (unsigned long)d->rehashidx);
        // 桶位为空
        while(d->ht_table[0][d->rehashidx] == NULL) {
            // 下一个桶位
            d->rehashidx++;
            // 如果已经达到最大空桶位处理数，直接返回1
            if (--empty_visits == 0) return 1;
        }
        // 处理一个桶位的
        de = d->ht_table[0][d->rehashidx];
        /* Move all the keys in this bucket from the old to the new hash HT */
        // 将桶位中的所有键从老的移动到新的Hash
        while(de) {
            uint64_t h;

            nextde = de->next;
            /* Get the index in the new hash table */
            // 如果比原来的大，需要重新计算hash
            if (d->ht_size_exp[1] > d->ht_size_exp[0]) {
                h = dictHashKey(d, de->key) & DICTHT_SIZE_MASK(d->ht_size_exp[1]);
            } else {
                /* We're shrinking the table. The tables sizes are powers of
                 * two, so we simply mask the bucket index in the larger table
                 * to get the bucket index in the smaller table. 
                 * 我们正在收缩表，表的大小都是2的幂，因此我们只需较大表中的存储桶索引的掩码即可获取较小表中的存储桶索引。
                 * */
                h = d->rehashidx & DICTHT_SIZE_MASK(d->ht_size_exp[1]);
            }
            // 挂接到新表中
            de->next = d->ht_table[1][h];
            d->ht_table[1][h] = de;
            // 老表的使用数减少而新表的使用数增加
            d->ht_used[0]--;
            d->ht_used[1]++;
            de = nextde;
        }
        // 将桶位置空
        d->ht_table[0][d->rehashidx] = NULL;
        // 开始下一个
        d->rehashidx++;
    }

    /* Check if we already rehashed the whole table... */
    // 是否已经重新Hash了整个表
    if (d->ht_used[0] == 0) {
        // 删除老表
        zfree(d->ht_table[0]);
        /* Copy the new ht onto the old one */
        // 将新版赋值给老表
        d->ht_table[0] = d->ht_table[1];
        d->ht_used[0] = d->ht_used[1];
        d->ht_size_exp[0] = d->ht_size_exp[1];
        _dictReset(d, 1);
        // 不再处于重新hash的状态
        d->rehashidx = -1;
        return 0;
    }

    /* More to rehash... */
    // 还需要下一次的重新hash
    return 1;
}

// 得到毫秒级的当前时间
long long timeInMilliseconds(void) {
    struct timeval tv;

    gettimeofday(&tv,NULL);
    return (((long long)tv.tv_sec)*1000)+(tv.tv_usec/1000);
}

/* Rehash in ms+"delta" milliseconds. The value of "delta" is larger 
 * than 0, and is smaller than 1 in most cases. The exact upper bound 
 * depends on the running time of dictRehash(d,100).*/
// 以ms+"delta"毫秒重新哈希。 “delta”的值大于0，大多数情况下小于1。 确切的上限取决于dictRehash(d,100)的运行时间
int dictRehashMilliseconds(dict *d, int ms) {
    // 如果是暂停重新Hash状态
    if (d->pauserehash > 0) return 0;

    long long start = timeInMilliseconds();
    int rehashes = 0;
    // 每次重新Hash 100个桶位
    while(dictRehash(d,100)) {
        rehashes += 100;
        // 如果超时，就退出
        if (timeInMilliseconds()-start > ms) break;
    }
    return rehashes;
}

/* This function performs just a step of rehashing, and only if hashing has
 * not been paused for our hash table. When we have iterators in the
 * middle of a rehashing we can't mess with the two hash tables otherwise
 * some elements can be missed or duplicated.
 *
 * This function is called by common lookup or update operations in the
 * dictionary so that the hash table automatically migrates from H1 to H2
 * while it is actively used. 
 *  该函数由字典中常见的查找或更新操作调用，以便哈希表在主动使用时自动从H1迁移到H2
 * */
// 重新hash 1步
static void _dictRehashStep(dict *d) {
    if (d->pauserehash == 0) dictRehash(d,1);
}

/* Add an element to the target hash table */
// 字典中增加一个元素
int dictAdd(dict *d, void *key, void *val)
{
    // 增加一个元素
    dictEntry *entry = dictAddRaw(d,key,NULL);

    if (!entry) return DICT_ERR;
    // 设置元素的值
    dictSetVal(d, entry, val);
    return DICT_OK;
}

/* Low level add or find:
 * This function adds the entry but instead of setting a value returns the
 * dictEntry structure to the user, that will make sure to fill the value
 * field as they wish.
 *
 * This function is also directly exposed to the user API to be called
 * mainly in order to store non-pointers inside the hash value, example:
 *
 * entry = dictAddRaw(dict,mykey,NULL);
 * if (entry != NULL) dictSetSignedIntegerVal(entry,1000);
 *
 * Return values:
 *
 * If key already exists NULL is returned, and "*existing" is populated
 * with the existing entry if existing is not NULL.
 *
 * If key was added, the hash entry is returned to be manipulated by the caller.
 * 
 * 下面是gpt的对问题的回答：dictGenericDelete中调用_dictRehashStep的目的是什么？
 * 在dictGenericDelete中调用_dictRehashStep的目的是为了在删除字典中的元素时，
 * 尽可能地保持字典的负载因子（load factor）在一个合理的范围内，以及在字典的大小发生变化时，保持其性能。
 * 负载因子是指字典中已使用的桶与总桶数之比。当字典中的元素增加时，负载因子会增加，这可能导致查找、插入和删除操作的性能下降。
 * 为了维护性能，字典需要在一定条件下进行哈希表的重哈希（rehashing）操作，即重新分配桶并将元素重新插入到新的哈希表中。
 * _dictRehashStep函数是一个增量重哈希操作，它会在每次调用时将一定数量的桶从旧哈希表迁移到新哈希表。这样做的好处是，
 * 重哈希操作可以在多次调用中分摊开，而不是一次性完成，从而避免了在某一时刻产生较大的计算开销。
 * 在dictGenericDelete中调用_dictRehashStep是为了确保在删除元素后，字典能够维护一个合理的负载因子，从而保持较好的性能。
 * 如果不进行重哈希操作，负载因子可能会过低，导致字典占用的内存空间浪费。通过在删除元素时进行增量重哈希操作，
 * 可以在不影响性能的前提下，使字典的大小和负载因子保持在一个合理的范围内。
 */
// 低级的增加或者查找函数，该函数不是通过设置值来增加一个元素，而是把增加的元素结构体返回给调用者，
// 然后由调用者去设置它们希望的值。

dictEntry *dictAddRaw(dict *d, void *key, dictEntry **existing)
{
    long index;
    dictEntry *entry;
    int htidx;
    // 如果正在重新hash种，那就执行一步
    if (dictIsRehashing(d)) _dictRehashStep(d);

    /* Get the index of the new element, or -1 if
     * the element already exists. */
    // 得到新元素的索引，-1表示元素已经存在
    if ((index = _dictKeyIndex(d, key, dictHashKey(d,key), existing)) == -1)
        return NULL;

    /* Allocate the memory and store the new entry.
     * Insert the element in top, with the assumption that in a database
     * system it is more likely that recently added entries are accessed
     * more frequently. */
    // 分配内存并存储新条目。将元素插入顶部，假设在数据库系统中最近添加的条目更有可能被更频繁地访问
    htidx = dictIsRehashing(d) ? 1 : 0;
    // 得到元数据的值
    size_t metasize = dictMetadataSize(d);
    entry = zmalloc(sizeof(*entry) + metasize);
    // 将元数据部分清零
    if (metasize > 0) {
        memset(dictMetadata(entry), 0, metasize);
    }
    // 挂在桶位的最前面
    entry->next = d->ht_table[htidx][index];
    d->ht_table[htidx][index] = entry;
    // 增加使用数
    d->ht_used[htidx]++;

    /* Set the hash entry fields. */
    // 设置元素的键
    dictSetKey(d, entry, key);
    return entry;
}

/* Add or Overwrite:
 * Add an element, discarding the old value if the key already exists.
 * Return 1 if the key was added from scratch, 0 if there was already an
 * element with such key and dictReplace() just performed a value update
 * operation. */
// 增加或者替代,没有就增加，有就重新赋值
int dictReplace(dict *d, void *key, void *val)
{
    dictEntry *entry, *existing, auxentry;

    /* Try to add the element. If the key
     * does not exists dictAdd will succeed. */
    // 尝试增加
    entry = dictAddRaw(d,key,&existing);
    // 增加成功就赋值
    if (entry) {
        dictSetVal(d, entry, val);
        return 1;
    }

    /* Set the new value and free the old one. Note that it is important
     * to do that in this order, as the value may just be exactly the same
     * as the previous one. In this context, think to reference counting,
     * you want to increment (set), and then decrement (free), and not the
     * reverse. */
    // 已经存在
    auxentry = *existing;
    // 设置新值
    dictSetVal(d, existing, val);
    // 释放老值
    dictFreeVal(d, &auxentry);
    return 0;
}

/* Add or Find:
 * dictAddOrFind() is simply a version of dictAddRaw() that always
 * returns the hash entry of the specified key, even if the key already
 * exists and can't be added (in that case the entry of the already
 * existing key is returned.)
 *
 * See dictAddRaw() for more information. */
// 增加或者查找，就是如果没有就增加，如果有就返回
dictEntry *dictAddOrFind(dict *d, void *key) {
    dictEntry *entry, *existing;
    entry = dictAddRaw(d,key,&existing);
    return entry ? entry : existing;
}

/* Search and remove an element. This is a helper function for
 * dictDelete() and dictUnlink(), please check the top comment
 * of those functions. */
// 查找并且删除元素
static dictEntry *dictGenericDelete(dict *d, const void *key, int nofree) {
    uint64_t h, idx;
    dictEntry *he, *prevHe;
    int table;

    /* dict is empty */
    // 如果字典为空，直接返回
    if (dictSize(d) == 0) return NULL;
    // 正在重新哈希，就执行一步
    if (dictIsRehashing(d)) _dictRehashStep(d);
    // 得到哈希值
    h = dictHashKey(d, key);
    // 从两个哈希表中去删除
    for (table = 0; table <= 1; table++) {
        idx = h & DICTHT_SIZE_MASK(d->ht_size_exp[table]);
        he = d->ht_table[table][idx];
        // 单向链表，需要记录前一个节点
        prevHe = NULL;
        while(he) {
            // 找到了
            if (key==he->key || dictCompareKeys(d, key, he->key)) {
                /* Unlink the element from the list */
                // 从链表中断开
                if (prevHe)
                    prevHe->next = he->next;
                else
                    d->ht_table[table][idx] = he->next;
                // 需要释放节点
                if (!nofree) {
                    dictFreeUnlinkedEntry(d, he);
                }
                // 减少使用计数
                d->ht_used[table]--;
                return he;
            }
            prevHe = he;
            he = he->next;
        }
        // 如果不在重新hash种，直接删除
        if (!dictIsRehashing(d)) break;
    }
    return NULL; /* not found */
}

/* Remove an element, returning DICT_OK on success or DICT_ERR if the
 * element was not found. */
// 删除一个元素
int dictDelete(dict *ht, const void *key) {
    return dictGenericDelete(ht,key,0) ? DICT_OK : DICT_ERR;
}

/* Remove an element from the table, but without actually releasing
 * the key, value and dictionary entry. The dictionary entry is returned
 * if the element was found (and unlinked from the table), and the user
 * should later call `dictFreeUnlinkedEntry()` with it in order to release it.
 * Otherwise if the key is not found, NULL is returned.
 *
 * This function is useful when we want to remove something from the hash
 * table but want to use its value before actually deleting the entry.
 * Without this function the pattern would require two lookups:
 *
 *  entry = dictFind(...);
 *  // Do something with entry
 *  dictDelete(dictionary,entry);
 *
 * Thanks to this function it is possible to avoid this, and use
 * instead:
 *
 * entry = dictUnlink(dictionary,entry);
 * // Do something with entry
 * dictFreeUnlinkedEntry(entry); // <- This does not need to lookup again.
 */
// 删除一个元素，并且将元素返回了，并没有释放元素的资源
dictEntry *dictUnlink(dict *d, const void *key) {
    return dictGenericDelete(d,key,1);
}

/* You need to call this function to really free the entry after a call
 * to dictUnlink(). It's safe to call this function with 'he' = NULL. */
// 调用这个真正释放一个元素
void dictFreeUnlinkedEntry(dict *d, dictEntry *he) {
    if (he == NULL) return;
    dictFreeKey(d, he);
    dictFreeVal(d, he);
    zfree(he);
}

/* Destroy an entire dictionary */
// 销毁整个字典
int _dictClear(dict *d, int htidx, void(callback)(dict*)) {
    unsigned long i;

    /* Free all the elements */
    // 释放所有的元素
    for (i = 0; i < DICTHT_SIZE(d->ht_size_exp[htidx]) && d->ht_used[htidx] > 0; i++) {
        dictEntry *he, *nextHe;
        // 桶位所有是655536的倍数就调用一次回调
        if (callback && (i & 65535) == 0) callback(d);

        if ((he = d->ht_table[htidx][i]) == NULL) continue;
        // 销毁桶位中的元素
        while(he) {
            nextHe = he->next;
            dictFreeKey(d, he);
            dictFreeVal(d, he);
            zfree(he);
            d->ht_used[htidx]--;
            he = nextHe;
        }
    }
    /* Free the table and the allocated cache structure */
    // 删除然后重置哈希表
    zfree(d->ht_table[htidx]);
    /* Re-initialize the table */
    _dictReset(d, htidx);
    return DICT_OK; /* never fails */
}

/* Clear & Release the hash table */
// 清空然后释放字典
void dictRelease(dict *d)
{
    _dictClear(d,0,NULL);
    _dictClear(d,1,NULL);
    zfree(d);
}

// 字典查找
dictEntry *dictFind(dict *d, const void *key)
{
    dictEntry *he;
    uint64_t h, idx, table;

    if (dictSize(d) == 0) return NULL; /* dict is empty */
    // 如果在重新哈希中，执行一步
    if (dictIsRehashing(d)) _dictRehashStep(d);
    // 得到桶位
    h = dictHashKey(d, key);
    for (table = 0; table <= 1; table++) {
        idx = h & DICTHT_SIZE_MASK(d->ht_size_exp[table]);
        he = d->ht_table[table][idx];
        // 遍历桶位的元素
        while(he) {
            if (key==he->key || dictCompareKeys(d, key, he->key))
                return he;
            he = he->next;
        }
        // 如果不在重新Hash中，就不需要考虑第二个表
        if (!dictIsRehashing(d)) return NULL;
    }
    return NULL;
}

// 获取字典中对应键的值，注意返回值是void*,不是void
void *dictFetchValue(dict *d, const void *key) {
    dictEntry *he;

    he = dictFind(d,key);
    return he ? dictGetVal(he) : NULL;
}

/* Find an element from the table, also get the plink of the entry. The entry
 * is returned if the element is found, and the user should later call
 * `dictTwoPhaseUnlinkFree` with it in order to unlink and release it. Otherwise if
 * the key is not found, NULL is returned. These two functions should be used in pair.
 * `dictTwoPhaseUnlinkFind` pauses rehash and `dictTwoPhaseUnlinkFree` resumes rehash.
 * 从表中查找一个元素，并获取该条目的plink。 如果找到该元素，则返回该条目，
 * 并且用户稍后应使用它调用“dictTwoPhaseUnlinkFree”以取消链接并释放它。 否则，如果未找到该键，
 * 则返回 NULL。 这两个函数应该成对使用。`dictTwoPhaseUnlinkFind`暂停rehash，
 * 而`dictTwoPhaseUnlinkFree`恢复rehash。
 * We can use like this:
 *
 * dictEntry *de = dictTwoPhaseUnlinkFind(db->dict,key->ptr,&plink, &table);
 * // Do something, but we can't modify the dict
 * dictTwoPhaseUnlinkFree(db->dict,de,plink,table); // We don't need to lookup again
 *
 * If we want to find an entry before delete this entry, this an optimization to avoid
 * dictFind followed by dictDelete. i.e. the first API is a find, and it gives some info
 * to the second one to avoid repeating the lookup
 */
// plink里面装的是指向下一个元素的地址，就是上一个元素next的地址
dictEntry *dictTwoPhaseUnlinkFind(dict *d, const void *key, dictEntry ***plink, int *table_index) {
    uint64_t h, idx, table;

    if (dictSize(d) == 0) return NULL; /* dict is empty */
    // 重新Hash中，执行一步
    if (dictIsRehashing(d)) _dictRehashStep(d);
    h = dictHashKey(d, key);
    // 遍历两个表
    for (table = 0; table <= 1; table++) {
        idx = h & DICTHT_SIZE_MASK(d->ht_size_exp[table]);
        dictEntry **ref = &d->ht_table[table][idx];
        while(*ref) {
            if (key==(*ref)->key || dictCompareKeys(d, key, (*ref)->key)) {
                *table_index = table;
                // 得到元素的指针
                *plink = ref;
                // 暂停hash，避免内存失效
                dictPauseRehashing(d);
                return *ref;
            }
            ref = &(*ref)->next;
        }
        if (!dictIsRehashing(d)) return NULL;
    }
    return NULL;
}
// 释放元素de, plink里面装的是指向下一个元素的地址，就是上一个元素next的地址
void dictTwoPhaseUnlinkFree(dict *d, dictEntry *he, dictEntry **plink, int table_index) {
    if (he == NULL) return;
    // 减少已经使用的数目
    d->ht_used[table_index]--;
    *plink = he->next;
    dictFreeKey(d, he);
    dictFreeVal(d, he);
    zfree(he);
    // 回复重新哈希
    dictResumeRehashing(d);
}

/* A fingerprint is a 64 bit number that represents the state of the dictionary
 * at a given time, it's just a few dict properties xored together.
 * When an unsafe iterator is initialized, we get the dict fingerprint, and check
 * the fingerprint again when the iterator is released.
 * If the two fingerprints are different it means that the user of the iterator
 * performed forbidden operations against the dictionary while iterating. */
// 指纹是一个64位的数字，用于表示给定时间字典状态的表示，只是很少属性的异或值。
unsigned long long dictFingerprint(dict *d) {
    unsigned long long integers[6], hash = 0;
    int j;

    integers[0] = (long) d->ht_table[0];
    integers[1] = d->ht_size_exp[0];
    integers[2] = d->ht_used[0];
    integers[3] = (long) d->ht_table[1];
    integers[4] = d->ht_size_exp[1];
    integers[5] = d->ht_used[1];

    /* We hash N integers by summing every successive integer with the integer
     * hashing of the previous sum. Basically:
     *
     * Result = hash(hash(hash(int1)+int2)+int3) ...
     *
     * This way the same set of integers in a different order will (likely) hash
     * to a different number. */
    for (j = 0; j < 6; j++) {
        hash += integers[j];
        /* For the hashing step we use Tomas Wang's 64 bit integer hash. */
        hash = (~hash) + (hash << 21); // hash = (hash << 21) - hash - 1;
        hash = hash ^ (hash >> 24);
        hash = (hash + (hash << 3)) + (hash << 8); // hash * 265
        hash = hash ^ (hash >> 14);
        hash = (hash + (hash << 2)) + (hash << 4); // hash * 21
        hash = hash ^ (hash >> 28);
        hash = hash + (hash << 31);
    }
    return hash;
}
// 初始化字典的迭代器
void dictInitIterator(dictIterator *iter, dict *d)
{
    iter->d = d;
    iter->table = 0;
    iter->index = -1;
    iter->safe = 0;
    iter->entry = NULL;
    iter->nextEntry = NULL;
}

// 初始化安全迭代器
void dictInitSafeIterator(dictIterator *iter, dict *d)
{
    dictInitIterator(iter, d);
    iter->safe = 1;
}

// 重置迭代器
void dictResetIterator(dictIterator *iter)
{
    if (!(iter->index == -1 && iter->table == 0)) {
        // 如果是安全迭代器
        if (iter->safe)
            // 回复重新哈希
            dictResumeRehashing(iter->d);
        else
            assert(iter->fingerprint == dictFingerprint(iter->d));
    }
}

// 得到一个迭代器
dictIterator *dictGetIterator(dict *d)
{
    dictIterator *iter = zmalloc(sizeof(*iter));
    dictInitIterator(iter, d);
    return iter;
}

// 得到一个迭安全代器
dictIterator *dictGetSafeIterator(dict *d) {
    dictIterator *i = dictGetIterator(d);

    i->safe = 1;
    return i;
}

// 遍历下一个元素
dictEntry *dictNext(dictIterator *iter)
{
    while (1) {
        // 元素位空，需要遍历下一个桶位
        if (iter->entry == NULL) {
            // 如果还没开始
            if (iter->index == -1 && iter->table == 0) {
                // 暂停重新哈希
                if (iter->safe)
                    dictPauseRehashing(iter->d);
                else
                    iter->fingerprint = dictFingerprint(iter->d);
            }
            // 下一个桶位
            iter->index++;
            // 遍历完了当前hash表
            if (iter->index >= (long) DICTHT_SIZE(iter->d->ht_size_exp[iter->table])) {
                // 如果是重新哈希，接着下一个哈希表
                if (dictIsRehashing(iter->d) && iter->table == 0) {
                    iter->table++;
                    iter->index = 0;
                } else {
                    break;
                }
            }
            // 第一个元素
            iter->entry = iter->d->ht_table[iter->table][iter->index];
        } else {
            // 下一个元素
            iter->entry = iter->nextEntry;
        }
        if (iter->entry) {
            /* We need to save the 'next' here, the iterator user
             * may delete the entry we are returning. */
            // 缓存下一个元素，因为迭代器的中元素可能返回后被删除
            iter->nextEntry = iter->entry->next;
            return iter->entry;
        }
    }
    return NULL;
}
// 释放迭代器
void dictReleaseIterator(dictIterator *iter)
{
    dictResetIterator(iter);
    zfree(iter);
}

/* Return a random entry from the hash table. Useful to
 * implement randomized algorithms */
// 从hash表中随机返回一个元素，实现随机算法很有用
dictEntry *dictGetRandomKey(dict *d)
{
    dictEntry *he, *orighe;
    unsigned long h;
    int listlen, listele;
    // 没有元素，返回空
    if (dictSize(d) == 0) return NULL;
    // 正在重新hash，重新Hash一步
    if (dictIsRehashing(d)) _dictRehashStep(d);
    // 重新Hash中
    if (dictIsRehashing(d)) {
        unsigned long s0 = DICTHT_SIZE(d->ht_size_exp[0]);
        do {
            /* We are sure there are no elements in indexes from 0
             * to rehashidx-1 */
            // 得到所有能用的桶位
            h = d->rehashidx + (randomULong() % (dictSlots(d) - d->rehashidx));
            // 得到对应的桶位
            he = (h >= s0) ? d->ht_table[1][h - s0] : d->ht_table[0][h];
        } while(he == NULL);
    } else {
        unsigned long m = DICTHT_SIZE_MASK(d->ht_size_exp[0]);
        do {
            // 随机桶位
            h = randomULong() & m;
            he = d->ht_table[0][h];
        } while(he == NULL);
    }

    /* Now we found a non empty bucket, but it is a linked
     * list and we need to get a random element from the list.
     * The only sane way to do so is counting the elements and
     * select a random index. */
    listlen = 0;
    orighe = he;
    // 计算桶位中的元素数目
    while(he) {
        he = he->next;
        listlen++;
    }
    // 桶位内随机
    listele = random() % listlen;
    he = orighe;
    // 得到随机的元素
    while(listele--) he = he->next;
    return he;
}

/* This function samples the dictionary to return a few keys from random
 * locations.
 *
 * It does not guarantee to return all the keys specified in 'count', nor
 * it does guarantee to return non-duplicated elements, however it will make
 * some effort to do both things.
 *
 * Returned pointers to hash table entries are stored into 'des' that
 * points to an array of dictEntry pointers. The array must have room for
 * at least 'count' elements, that is the argument we pass to the function
 * to tell how many random elements we need.
 *
 * The function returns the number of items stored into 'des', that may
 * be less than 'count' if the hash table has less than 'count' elements
 * inside, or if not enough elements were found in a reasonable amount of
 * steps.
 *
 * Note that this function is not suitable when you need a good distribution
 * of the returned items, but only when you need to "sample" a given number
 * of continuous elements to run some kind of algorithm or to produce
 * statistics. However the function is much faster than dictGetRandomKey()
 * at producing N elements. */
// 该函数随机对字典随机采样并返回随机位置的几个很少的键
unsigned int dictGetSomeKeys(dict *d, dictEntry **des, unsigned int count) {
    unsigned long j; /* internal hash table id, 0 or 1. */
    unsigned long tables; /* 1 or 2 tables? */
    unsigned long stored = 0, maxsizemask;
    unsigned long maxsteps;
    // 得到字典大小
    if (dictSize(d) < count) count = dictSize(d);
    maxsteps = count*10;

    /* Try to do a rehashing work proportional to 'count'. */
    // 如果是重新hash，就执行count次
    for (j = 0; j < count; j++) {
        if (dictIsRehashing(d))
            _dictRehashStep(d);
        else
            break;
    }
    // 如果是重新hash，有两个表
    tables = dictIsRehashing(d) ? 2 : 1;
    maxsizemask = DICTHT_SIZE_MASK(d->ht_size_exp[0]);
    // 选一个大的掩码
    if (tables > 1 && maxsizemask < DICTHT_SIZE_MASK(d->ht_size_exp[1]))
        maxsizemask = DICTHT_SIZE_MASK(d->ht_size_exp[1]);

    /* Pick a random point inside the larger table. */
    // 在大表中随机一个点
    unsigned long i = randomULong() & maxsizemask;
    unsigned long emptylen = 0; /* Continuous empty entries so far. */
    while(stored < count && maxsteps--) {
        // 遍历表
        for (j = 0; j < tables; j++) {
            /* Invariant of the dict.c rehashing: up to the indexes already
             * visited in ht[0] during the rehashing, there are no populated
             * buckets, so we can skip ht[0] for indexes between 0 and idx-1. */
            // 两个表，现在是第一个，比rehashidx小，那部分已经不可用
            if (tables == 2 && j == 0 && i < (unsigned long) d->rehashidx) {
                /* Moreover, if we are currently out of range in the second
                 * table, there will be no elements in both tables up to
                 * the current rehashing index, so we jump if possible.
                 * (this happens when going from big to small table). */
                if (i >= DICTHT_SIZE(d->ht_size_exp[1]))
                    i = d->rehashidx;
                else
                    continue;
            }
            if (i >= DICTHT_SIZE(d->ht_size_exp[j])) continue; /* Out of range for this table. */
            dictEntry *he = d->ht_table[j][i];

            /* Count contiguous empty buckets, and jump to other
             * locations if they reach 'count' (with a minimum of 5). */
            if (he == NULL) {
                emptylen++;
                if (emptylen >= 5 && emptylen > count) {
                    i = randomULong() & maxsizemask;
                    emptylen = 0;
                }
            } else {
                emptylen = 0;
                while (he) {
                    /* Collect all the elements of the buckets found non
                     * empty while iterating. */
                    *des = he;
                    des++;
                    he = he->next;
                    stored++;
                    if (stored == count) return stored;
                }
            }
        }
        i = (i+1) & maxsizemask;
    }
    return stored;
}

/* This is like dictGetRandomKey() from the POV of the API, but will do more
 * work to ensure a better distribution of the returned element.
 *
 * This function improves the distribution because the dictGetRandomKey()
 * problem is that it selects a random bucket, then it selects a random
 * element from the chain in the bucket. However elements being in different
 * chain lengths will have different probabilities of being reported. With
 * this function instead what we do is to consider a "linear" range of the table
 * that may be constituted of N buckets with chains of different lengths
 * appearing one after the other. Then we report a random element in the range.
 * In this way we smooth away the problem of different chain lengths. */
#define GETFAIR_NUM_ENTRIES 15
dictEntry *dictGetFairRandomKey(dict *d) {
    dictEntry *entries[GETFAIR_NUM_ENTRIES];
    unsigned int count = dictGetSomeKeys(d,entries,GETFAIR_NUM_ENTRIES);
    /* Note that dictGetSomeKeys() may return zero elements in an unlucky
     * run() even if there are actually elements inside the hash table. So
     * when we get zero, we call the true dictGetRandomKey() that will always
     * yield the element if the hash table has at least one. */
    if (count == 0) return dictGetRandomKey(d);
    unsigned int idx = rand() % count;
    return entries[idx];
}

/* Function to reverse bits. Algorithm from:
 * http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel */
static unsigned long rev(unsigned long v) {
    unsigned long s = CHAR_BIT * sizeof(v); // bit size; must be power of 2
    unsigned long mask = ~0UL;
    while ((s >>= 1) > 0) {
        mask ^= (mask << s);
        v = ((v >> s) & mask) | ((v << s) & ~mask);
    }
    return v;
}

/* dictScan() is used to iterate over the elements of a dictionary.
 *
 * Iterating works the following way:
 *
 * 1) Initially you call the function using a cursor (v) value of 0.
 * 2) The function performs one step of the iteration, and returns the
 *    new cursor value you must use in the next call.
 * 3) When the returned cursor is 0, the iteration is complete.
 *
 * The function guarantees all elements present in the
 * dictionary get returned between the start and end of the iteration.
 * However it is possible some elements get returned multiple times.
 *
 * For every element returned, the callback argument 'fn' is
 * called with 'privdata' as first argument and the dictionary entry
 * 'de' as second argument.
 *
 * HOW IT WORKS.
 *
 * The iteration algorithm was designed by Pieter Noordhuis.
 * The main idea is to increment a cursor starting from the higher order
 * bits. That is, instead of incrementing the cursor normally, the bits
 * of the cursor are reversed, then the cursor is incremented, and finally
 * the bits are reversed again.
 *
 * This strategy is needed because the hash table may be resized between
 * iteration calls.
 *
 * dict.c hash tables are always power of two in size, and they
 * use chaining, so the position of an element in a given table is given
 * by computing the bitwise AND between Hash(key) and SIZE-1
 * (where SIZE-1 is always the mask that is equivalent to taking the rest
 *  of the division between the Hash of the key and SIZE).
 *
 * For example if the current hash table size is 16, the mask is
 * (in binary) 1111. The position of a key in the hash table will always be
 * the last four bits of the hash output, and so forth.
 *
 * WHAT HAPPENS IF THE TABLE CHANGES IN SIZE?
 *
 * If the hash table grows, elements can go anywhere in one multiple of
 * the old bucket: for example let's say we already iterated with
 * a 4 bit cursor 1100 (the mask is 1111 because hash table size = 16).
 *
 * If the hash table will be resized to 64 elements, then the new mask will
 * be 111111. The new buckets you obtain by substituting in ??1100
 * with either 0 or 1 can be targeted only by keys we already visited
 * when scanning the bucket 1100 in the smaller hash table.
 *
 * By iterating the higher bits first, because of the inverted counter, the
 * cursor does not need to restart if the table size gets bigger. It will
 * continue iterating using cursors without '1100' at the end, and also
 * without any other combination of the final 4 bits already explored.
 *
 * Similarly when the table size shrinks over time, for example going from
 * 16 to 8, if a combination of the lower three bits (the mask for size 8
 * is 111) were already completely explored, it would not be visited again
 * because we are sure we tried, for example, both 0111 and 1111 (all the
 * variations of the higher bit) so we don't need to test it again.
 *
 * WAIT... YOU HAVE *TWO* TABLES DURING REHASHING!
 *
 * Yes, this is true, but we always iterate the smaller table first, then
 * we test all the expansions of the current cursor into the larger
 * table. For example if the current cursor is 101 and we also have a
 * larger table of size 16, we also test (0)101 and (1)101 inside the larger
 * table. This reduces the problem back to having only one table, where
 * the larger one, if it exists, is just an expansion of the smaller one.
 *
 * LIMITATIONS
 *
 * This iterator is completely stateless, and this is a huge advantage,
 * including no additional memory used.
 *
 * The disadvantages resulting from this design are:
 *
 * 1) It is possible we return elements more than once. However this is usually
 *    easy to deal with in the application level.
 * 2) The iterator must return multiple elements per call, as it needs to always
 *    return all the keys chained in a given bucket, and all the expansions, so
 *    we are sure we don't miss keys moving during rehashing.
 * 3) The reverse cursor is somewhat hard to understand at first, but this
 *    comment is supposed to help.
 */
unsigned long dictScan(dict *d,
                       unsigned long v,
                       dictScanFunction *fn,
                       dictScanBucketFunction* bucketfn,
                       void *privdata)
{
    int htidx0, htidx1;
    const dictEntry *de, *next;
    unsigned long m0, m1;

    if (dictSize(d) == 0) return 0;

    /* This is needed in case the scan callback tries to do dictFind or alike. */
    dictPauseRehashing(d);

    if (!dictIsRehashing(d)) {
        htidx0 = 0;
        m0 = DICTHT_SIZE_MASK(d->ht_size_exp[htidx0]);

        /* Emit entries at cursor */
        if (bucketfn) bucketfn(d, &d->ht_table[htidx0][v & m0]);
        de = d->ht_table[htidx0][v & m0];
        while (de) {
            next = de->next;
            fn(privdata, de);
            de = next;
        }

        /* Set unmasked bits so incrementing the reversed cursor
         * operates on the masked bits */
        v |= ~m0;

        /* Increment the reverse cursor */
        v = rev(v);
        v++;
        v = rev(v);

    } else {
        htidx0 = 0;
        htidx1 = 1;

        /* Make sure t0 is the smaller and t1 is the bigger table */
        if (DICTHT_SIZE(d->ht_size_exp[htidx0]) > DICTHT_SIZE(d->ht_size_exp[htidx1])) {
            htidx0 = 1;
            htidx1 = 0;
        }

        m0 = DICTHT_SIZE_MASK(d->ht_size_exp[htidx0]);
        m1 = DICTHT_SIZE_MASK(d->ht_size_exp[htidx1]);

        /* Emit entries at cursor */
        if (bucketfn) bucketfn(d, &d->ht_table[htidx0][v & m0]);
        de = d->ht_table[htidx0][v & m0];
        while (de) {
            next = de->next;
            fn(privdata, de);
            de = next;
        }

        /* Iterate over indices in larger table that are the expansion
         * of the index pointed to by the cursor in the smaller table */
        do {
            /* Emit entries at cursor */
            if (bucketfn) bucketfn(d, &d->ht_table[htidx1][v & m1]);
            de = d->ht_table[htidx1][v & m1];
            while (de) {
                next = de->next;
                fn(privdata, de);
                de = next;
            }

            /* Increment the reverse cursor not covered by the smaller mask.*/
            v |= ~m1;
            v = rev(v);
            v++;
            v = rev(v);

            /* Continue while bits covered by mask difference is non-zero */
        } while (v & (m0 ^ m1));
    }

    dictResumeRehashing(d);

    return v;
}

/* ------------------------- private functions ------------------------------ */

/* Because we may need to allocate huge memory chunk at once when dict
 * expands, we will check this allocation is allowed or not if the dict
 * type has expandAllowed member function. */
static int dictTypeExpandAllowed(dict *d) {
    if (d->type->expandAllowed == NULL) return 1;
    return d->type->expandAllowed(
                    DICTHT_SIZE(_dictNextExp(d->ht_used[0] + 1)) * sizeof(dictEntry*),
                    (double)d->ht_used[0] / DICTHT_SIZE(d->ht_size_exp[0]));
}

/* Expand the hash table if needed */
static int _dictExpandIfNeeded(dict *d)
{
    /* Incremental rehashing already in progress. Return. */
    if (dictIsRehashing(d)) return DICT_OK;

    /* If the hash table is empty expand it to the initial size. */
    if (DICTHT_SIZE(d->ht_size_exp[0]) == 0) return dictExpand(d, DICT_HT_INITIAL_SIZE);

    /* If we reached the 1:1 ratio, and we are allowed to resize the hash
     * table (global setting) or we should avoid it but the ratio between
     * elements/buckets is over the "safe" threshold, we resize doubling
     * the number of buckets. */
    if (d->ht_used[0] >= DICTHT_SIZE(d->ht_size_exp[0]) &&
        (dict_can_resize ||
         d->ht_used[0]/ DICTHT_SIZE(d->ht_size_exp[0]) > dict_force_resize_ratio) &&
        dictTypeExpandAllowed(d))
    {
        return dictExpand(d, d->ht_used[0] + 1);
    }
    return DICT_OK;
}

/* TODO: clz optimization */
/* Our hash table capability is a power of two */
// 得到尺寸对应的2的幂指数
static signed char _dictNextExp(unsigned long size)
{
    unsigned char e = DICT_HT_INITIAL_EXP;
    // 如果超过长整型的最大值，得到最大值对应的幂指数
    if (size >= LONG_MAX) return (8*sizeof(long)-1);
    while(1) {
        // 一旦可以放得下就返回
        if (((unsigned long)1<<e) >= size)
            return e;
        e++;
    }
}

/* Returns the index of a free slot that can be populated with
 * a hash entry for the given 'key'.
 * If the key already exists, -1 is returned
 * and the optional output parameter may be filled.
 *
 * Note that if we are in the process of rehashing the hash table, the
 * index is always returned in the context of the second (new) hash table. */
// 返回可以填充的空闲槽的索引给定“键”的哈希条目。
static long _dictKeyIndex(dict *d, const void *key, uint64_t hash, dictEntry **existing)
{
    unsigned long idx, table;
    dictEntry *he;
    if (existing) *existing = NULL;

    /* Expand the hash table if needed */
    // 如果空间不够，需要扩展
    if (_dictExpandIfNeeded(d) == DICT_ERR)
        return -1;
    // 遍历两个表，因为可能在重新哈希中
    for (table = 0; table <= 1; table++) {
        // 得到哈希表中的索引
        idx = hash & DICTHT_SIZE_MASK(d->ht_size_exp[table]);
        /* Search if this slot does not already contain the given key */
        // 搜索桶位中有没有包含给定的键
        he = d->ht_table[table][idx];
        // 遍历桶位中所有的元素
        while(he) {
            // 找到了
            if (key==he->key || dictCompareKeys(d, key, he->key)) {
                if (existing) *existing = he;
                return -1;
            }
            he = he->next;
        }
        // 如果不在重新哈希中，就不需要遍历第二张hash表
        if (!dictIsRehashing(d)) break;
    }
    return idx;
}

void dictEmpty(dict *d, void(callback)(dict*)) {
    _dictClear(d,0,callback);
    _dictClear(d,1,callback);
    d->rehashidx = -1;
    d->pauserehash = 0;
}

void dictEnableResize(void) {
    dict_can_resize = 1;
}

void dictDisableResize(void) {
    dict_can_resize = 0;
}

uint64_t dictGetHash(dict *d, const void *key) {
    return dictHashKey(d, key);
}

/* Finds the dictEntry reference by using pointer and pre-calculated hash.
 * oldkey is a dead pointer and should not be accessed.
 * the hash value should be provided using dictGetHash.
 * no string / key comparison is performed.
 * return value is the reference to the dictEntry if found, or NULL if not found. */
dictEntry **dictFindEntryRefByPtrAndHash(dict *d, const void *oldptr, uint64_t hash) {
    dictEntry *he, **heref;
    unsigned long idx, table;

    if (dictSize(d) == 0) return NULL; /* dict is empty */
    for (table = 0; table <= 1; table++) {
        idx = hash & DICTHT_SIZE_MASK(d->ht_size_exp[table]);
        heref = &d->ht_table[table][idx];
        he = *heref;
        while(he) {
            if (oldptr==he->key)
                return heref;
            heref = &he->next;
            he = *heref;
        }
        if (!dictIsRehashing(d)) return NULL;
    }
    return NULL;
}

/* ------------------------------- Debugging ---------------------------------*/

#define DICT_STATS_VECTLEN 50
size_t _dictGetStatsHt(char *buf, size_t bufsize, dict *d, int htidx) {
    unsigned long i, slots = 0, chainlen, maxchainlen = 0;
    unsigned long totchainlen = 0;
    unsigned long clvector[DICT_STATS_VECTLEN];
    size_t l = 0;

    if (d->ht_used[htidx] == 0) {
        return snprintf(buf,bufsize,
            "No stats available for empty dictionaries\n");
    }

    /* Compute stats. */
    for (i = 0; i < DICT_STATS_VECTLEN; i++) clvector[i] = 0;
    for (i = 0; i < DICTHT_SIZE(d->ht_size_exp[htidx]); i++) {
        dictEntry *he;

        if (d->ht_table[htidx][i] == NULL) {
            clvector[0]++;
            continue;
        }
        slots++;
        /* For each hash entry on this slot... */
        chainlen = 0;
        he = d->ht_table[htidx][i];
        while(he) {
            chainlen++;
            he = he->next;
        }
        clvector[(chainlen < DICT_STATS_VECTLEN) ? chainlen : (DICT_STATS_VECTLEN-1)]++;
        if (chainlen > maxchainlen) maxchainlen = chainlen;
        totchainlen += chainlen;
    }

    /* Generate human readable stats. */
    l += snprintf(buf+l,bufsize-l,
        "Hash table %d stats (%s):\n"
        " table size: %lu\n"
        " number of elements: %lu\n"
        " different slots: %lu\n"
        " max chain length: %lu\n"
        " avg chain length (counted): %.02f\n"
        " avg chain length (computed): %.02f\n"
        " Chain length distribution:\n",
        htidx, (htidx == 0) ? "main hash table" : "rehashing target",
        DICTHT_SIZE(d->ht_size_exp[htidx]), d->ht_used[htidx], slots, maxchainlen,
        (float)totchainlen/slots, (float)d->ht_used[htidx]/slots);

    for (i = 0; i < DICT_STATS_VECTLEN-1; i++) {
        if (clvector[i] == 0) continue;
        if (l >= bufsize) break;
        l += snprintf(buf+l,bufsize-l,
            "   %ld: %ld (%.02f%%)\n",
            i, clvector[i], ((float)clvector[i]/DICTHT_SIZE(d->ht_size_exp[htidx]))*100);
    }

    /* Unlike snprintf(), return the number of characters actually written. */
    if (bufsize) buf[bufsize-1] = '\0';
    return strlen(buf);
}

void dictGetStats(char *buf, size_t bufsize, dict *d) {
    size_t l;
    char *orig_buf = buf;
    size_t orig_bufsize = bufsize;

    l = _dictGetStatsHt(buf,bufsize,d,0);
    buf += l;
    bufsize -= l;
    if (dictIsRehashing(d) && bufsize > 0) {
        _dictGetStatsHt(buf,bufsize,d,1);
    }
    /* Make sure there is a NULL term at the end. */
    if (orig_bufsize) orig_buf[orig_bufsize-1] = '\0';
}

/* ------------------------------- Benchmark ---------------------------------*/

#ifdef REDIS_TEST
#include "testhelp.h"

#define UNUSED(V) ((void) V)

uint64_t hashCallback(const void *key) {
    return dictGenHashFunction((unsigned char*)key, strlen((char*)key));
}

int compareCallback(dict *d, const void *key1, const void *key2) {
    int l1,l2;
    UNUSED(d);

    l1 = strlen((char*)key1);
    l2 = strlen((char*)key2);
    if (l1 != l2) return 0;
    return memcmp(key1, key2, l1) == 0;
}

void freeCallback(dict *d, void *val) {
    UNUSED(d);

    zfree(val);
}

char *stringFromLongLong(long long value) {
    char buf[32];
    int len;
    char *s;

    len = snprintf(buf,sizeof(buf),"%lld",value);
    s = zmalloc(len+1);
    memcpy(s, buf, len);
    s[len] = '\0';
    return s;
}

dictType BenchmarkDictType = {
    hashCallback,
    NULL,
    NULL,
    compareCallback,
    freeCallback,
    NULL,
    NULL
};

#define start_benchmark() start = timeInMilliseconds()
#define end_benchmark(msg) do { \
    elapsed = timeInMilliseconds()-start; \
    printf(msg ": %ld items in %lld ms\n", count, elapsed); \
} while(0)

/* ./redis-server test dict [<count> | --accurate] */
int dictTest(int argc, char **argv, int flags) {
    long j;
    long long start, elapsed;
    dict *dict = dictCreate(&BenchmarkDictType);
    long count = 0;
    int accurate = (flags & REDIS_TEST_ACCURATE);

    if (argc == 4) {
        if (accurate) {
            count = 5000000;
        } else {
            count = strtol(argv[3],NULL,10);
        }
    } else {
        count = 5000;
    }

    start_benchmark();
    for (j = 0; j < count; j++) {
        int retval = dictAdd(dict,stringFromLongLong(j),(void*)j);
        assert(retval == DICT_OK);
    }
    end_benchmark("Inserting");
    assert((long)dictSize(dict) == count);

    /* Wait for rehashing. */
    while (dictIsRehashing(dict)) {
        dictRehashMilliseconds(dict,100);
    }

    start_benchmark();
    for (j = 0; j < count; j++) {
        char *key = stringFromLongLong(j);
        dictEntry *de = dictFind(dict,key);
        assert(de != NULL);
        zfree(key);
    }
    end_benchmark("Linear access of existing elements");

    start_benchmark();
    for (j = 0; j < count; j++) {
        char *key = stringFromLongLong(j);
        dictEntry *de = dictFind(dict,key);
        assert(de != NULL);
        zfree(key);
    }
    end_benchmark("Linear access of existing elements (2nd round)");

    start_benchmark();
    for (j = 0; j < count; j++) {
        char *key = stringFromLongLong(rand() % count);
        dictEntry *de = dictFind(dict,key);
        assert(de != NULL);
        zfree(key);
    }
    end_benchmark("Random access of existing elements");

    start_benchmark();
    for (j = 0; j < count; j++) {
        dictEntry *de = dictGetRandomKey(dict);
        assert(de != NULL);
    }
    end_benchmark("Accessing random keys");

    start_benchmark();
    for (j = 0; j < count; j++) {
        char *key = stringFromLongLong(rand() % count);
        key[0] = 'X';
        dictEntry *de = dictFind(dict,key);
        assert(de == NULL);
        zfree(key);
    }
    end_benchmark("Accessing missing");

    start_benchmark();
    for (j = 0; j < count; j++) {
        char *key = stringFromLongLong(j);
        int retval = dictDelete(dict,key);
        assert(retval == DICT_OK);
        key[0] += 17; /* Change first number to letter. */
        retval = dictAdd(dict,key,(void*)j);
        assert(retval == DICT_OK);
    }
    end_benchmark("Removing and adding");
    dictRelease(dict);
    return 0;
}
#endif
