/*
 * Copyright (c) 2009-2012, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 * Copyright (c) 2009-2012, Salvatore Sanfilippo <antirez at gmail dot com>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "intset.h"
#include "zmalloc.h"
#include "endianconv.h"
#include "redisassert.h"

/* Note that these encodings are ordered, so:
 * INTSET_ENC_INT16 < INTSET_ENC_INT32 < INTSET_ENC_INT64. */
// 注意编码顺序：INTSET_ENC_INT16 < INTSET_ENC_INT32 < INTSET_ENC_INT64
#define INTSET_ENC_INT16 (sizeof(int16_t))
#define INTSET_ENC_INT32 (sizeof(int32_t))
#define INTSET_ENC_INT64 (sizeof(int64_t))

/* Return the required encoding for the provided value. */
// 根据提供的值返回要求的编码
static uint8_t _intsetValueEncoding(int64_t v) {
    // 如果小于32位整数的最小值或者大于32位整数的最大值，
    // 使用64位整数编码
    if (v < INT32_MIN || v > INT32_MAX)
        return INTSET_ENC_INT64;
    // 如果小于16位整数的最小值或者大于16位整数的最大值，
    // 使用32位整数编码  
    else if (v < INT16_MIN || v > INT16_MAX)
        return INTSET_ENC_INT32;
    else
        // 使用16位整数编码
        return INTSET_ENC_INT16;
}

/* Return the value at pos, given an encoding. */
// 根据编码返回指定位置的值
static int64_t _intsetGetEncoded(intset *is, int pos, uint8_t enc) {
    int64_t v64;
    int32_t v32;
    int16_t v16;
    // 编码为64位整数
    if (enc == INTSET_ENC_INT64) {
        memcpy(&v64,((int64_t*)is->contents)+pos,sizeof(v64));
        memrev64ifbe(&v64);
        return v64;
    } 
    // 编码为32位整数
    else if (enc == INTSET_ENC_INT32) {
        memcpy(&v32,((int32_t*)is->contents)+pos,sizeof(v32));
        memrev32ifbe(&v32);
        return v32;
    }
    // 编码为16位整数 
    else {
        memcpy(&v16,((int16_t*)is->contents)+pos,sizeof(v16));
        memrev16ifbe(&v16);
        return v16;
    }
}

/* Return the value at pos, using the configured encoding. */
// 使用配置的编码，返回指定位置的值
static int64_t _intsetGet(intset *is, int pos) {
    return _intsetGetEncoded(is,pos,intrev32ifbe(is->encoding));
}

/* Set the value at pos, using the configured encoding. */
// 使用配置的编码格式，设置指定位置的值
static void _intsetSet(intset *is, int pos, int64_t value) {
    uint32_t encoding = intrev32ifbe(is->encoding);
    // 64位整数编码
    if (encoding == INTSET_ENC_INT64) {
        ((int64_t*)is->contents)[pos] = value;
        memrev64ifbe(((int64_t*)is->contents)+pos);
    } 
    // 32位整数编码
    else if (encoding == INTSET_ENC_INT32) {
        ((int32_t*)is->contents)[pos] = value;
        memrev32ifbe(((int32_t*)is->contents)+pos);
    } 
    // 16位整数编码
    else {
        ((int16_t*)is->contents)[pos] = value;
        memrev16ifbe(((int16_t*)is->contents)+pos);
    }
}

/* Create an empty intset. */
// 创建一个空的整数集合
intset *intsetNew(void) {
    intset *is = zmalloc(sizeof(intset));
    // 默认使用16位整数编码
    is->encoding = intrev32ifbe(INTSET_ENC_INT16);
    is->length = 0;
    return is;
}

/* Resize the intset */
// 重新设置一个整数集合的大小
static intset *intsetResize(intset *is, uint32_t len) {
    // 根据长度和编码计算内容所需内存大小
    uint64_t size = (uint64_t)len*intrev32ifbe(is->encoding);
    assert(size <= SIZE_MAX - sizeof(intset));
    // 加上结构体本身的消息，重新分配
    is = zrealloc(is,sizeof(intset)+size);
    return is;
}

/* Search for the position of "value". Return 1 when the value was found and
 * sets "pos" to the position of the value within the intset. Return 0 when
 * the value is not present in the intset and sets "pos" to the position
 * where "value" can be inserted. */
// 搜索指定值value的位置，如果找到就返回1，并且将pos设置为整数集合中的value所在的位置。
// 如果没有找到就返回0，并且将pos设置为value可以插入的位置。
static uint8_t intsetSearch(intset *is, int64_t value, uint32_t *pos) {
    int min = 0, max = intrev32ifbe(is->length)-1, mid = -1;
    int64_t cur = -1;

    /* The value can never be found when the set is empty */
    // 如果集合为空，肯定找不到value，将pos设置为0，并且返回0
    if (intrev32ifbe(is->length) == 0) {
        if (pos) *pos = 0;
        return 0;
    } else {
        /* Check for the case where we know we cannot find the value,
         * but do know the insert position. */
        // 如果大于末尾的值，肯定找不到，插入位置就是末尾的后面一个
        if (value > _intsetGet(is,max)) {
            if (pos) *pos = intrev32ifbe(is->length);
            return 0;
        } 
        // 如果小于最开头的值，肯定找不到，插入位置就是第一个
        else if (value < _intsetGet(is,0)) {
            if (pos) *pos = 0;
            return 0;
        }
    }

    // 二分法查找
    while(max >= min) {
        mid = ((unsigned int)min + (unsigned int)max) >> 1;
        cur = _intsetGet(is,mid);
        if (value > cur) {
            min = mid+1;
        } else if (value < cur) {
            max = mid-1;
        } else {
            break;
        }
    }
    // 如果当前值就是要找的
    if (value == cur) {
        // 找到了，将pos设置为找到的位置
        if (pos) *pos = mid;
        // 返回1
        return 1;
    } else {
        // 没找到，返回可以插入的位置
        if (pos) *pos = min;
        return 0;
    }
}

/* Upgrades the intset to a larger encoding and inserts the given integer. */
// 插入给定的整数，升级整数集合到一个更大的编码
// 为啥不需要找到合适的位置呢？你想，因为这个值都引起编码变化了，
// 要么就是一个很大的值，要么就是一个很小的值，所以要么在最前面，要么在最后面。
static intset *intsetUpgradeAndAdd(intset *is, int64_t value) {
    // 当前的编码
    uint8_t curenc = intrev32ifbe(is->encoding);
    // 给定值需要的编码
    uint8_t newenc = _intsetValueEncoding(value);
    int length = intrev32ifbe(is->length);
    int prepend = value < 0 ? 1 : 0;

    /* First set new encoding and resize */
    // 设置为新的编码和长度
    is->encoding = intrev32ifbe(newenc);
    is = intsetResize(is,intrev32ifbe(is->length)+1);

    /* Upgrade back-to-front so we don't overwrite values.
     * Note that the "prepend" variable is used to make sure we have an empty
     * space at either the beginning or the end of the intset. */
    // 从后向前升级，这样我们就不会覆盖值。
    // 请注意，“prepend”变量用于确保我们在intset的开头或结尾处有一个空白空间
    while(length--)
        _intsetSet(is,length+prepend,_intsetGetEncoded(is,length,curenc));

    /* Set the value at the beginning or the end. */
    // 如果在前面增加，将值设置为第一个位置
    if (prepend)
        _intsetSet(is,0,value);
    else
        // 后面增加，放在最后一个位置
        _intsetSet(is,intrev32ifbe(is->length),value);
    // 增加长度
    is->length = intrev32ifbe(intrev32ifbe(is->length)+1);
    return is;
}
 
 // 
static void intsetMoveTail(intset *is, uint32_t from, uint32_t to) {
    void *src, *dst;
    // 计算移动的元素数目
    uint32_t bytes = intrev32ifbe(is->length)-from;
    uint32_t encoding = intrev32ifbe(is->encoding);
    // 根据编码，计算开始位置，结束位置和拷贝的字节数
    if (encoding == INTSET_ENC_INT64) {
        src = (int64_t*)is->contents+from;
        dst = (int64_t*)is->contents+to;
        bytes *= sizeof(int64_t);
    } else if (encoding == INTSET_ENC_INT32) {
        src = (int32_t*)is->contents+from;
        dst = (int32_t*)is->contents+to;
        bytes *= sizeof(int32_t);
    } else {
        src = (int16_t*)is->contents+from;
        dst = (int16_t*)is->contents+to;
        bytes *= sizeof(int16_t);
    }
    memmove(dst,src,bytes);
}

/* Insert an integer in the intset */
// 在整数集合中插入一个整数
intset *intsetAdd(intset *is, int64_t value, uint8_t *success) {
    // 得到值的编码
    uint8_t valenc = _intsetValueEncoding(value);
    uint32_t pos;
    if (success) *success = 1;

    /* Upgrade encoding if necessary. If we need to upgrade, we know that
     * this value should be either appended (if > 0) or prepended (if < 0),
     * because it lies outside the range of existing values. */
    // 如果比现有的编码大，还需要升级整数集合的编码
    if (valenc > intrev32ifbe(is->encoding)) {
        /* This always succeeds, so we don't need to curry *success. */
        // 这种情况下肯定会成功,这种情况不需要管success的值，因为已经设置为1了
        return intsetUpgradeAndAdd(is,value);
    } else {
        /* Abort if the value is already present in the set.
         * This call will populate "pos" with the right position to insert
         * the value when it cannot be found. */
        // 如果已经在整数集里了，就不需要插入了，将success设置为0
        if (intsetSearch(is,value,&pos)) {
            if (success) *success = 0;
            return is;
        }
        // 将整数集的大小增加1
        is = intsetResize(is,intrev32ifbe(is->length)+1);
        // 然后将指定位置开始的数据整体往后移动一个位置
        if (pos < intrev32ifbe(is->length)) intsetMoveTail(is,pos,pos+1);
    }
    // 将值设置到指定位置
    _intsetSet(is,pos,value);
    // 增加长度
    is->length = intrev32ifbe(intrev32ifbe(is->length)+1);
    return is;
}

/* Delete integer from intset */
// 从整数集合删除一个整数
intset *intsetRemove(intset *is, int64_t value, int *success) {
    uint8_t valenc = _intsetValueEncoding(value);
    uint32_t pos;
    if (success) *success = 0;
    // 值的编码小于等于整数集合的编码才有可能在整数集合中
    // intsetSearch返回1表示找到
    if (valenc <= intrev32ifbe(is->encoding) && intsetSearch(is,value,&pos)) {
        uint32_t len = intrev32ifbe(is->length);

        /* We know we can delete */
        // 将成功设为1
        if (success) *success = 1;

        /* Overwrite value with tail and update length */
        // 如果不是最后一个，就移动后面的元素将其覆盖
        if (pos < (len-1)) intsetMoveTail(is,pos+1,pos);
        // 更新长度
        is = intsetResize(is,len-1);
        is->length = intrev32ifbe(len-1);
    }
    return is;
}

/* Determine whether a value belongs to this set */
// 确定一个值是否在整数集合中
uint8_t intsetFind(intset *is, int64_t value) {
    uint8_t valenc = _intsetValueEncoding(value);
    // 值的编码小于等于整数集合的编码才有可能在整数集合中,然后再去查找
    return valenc <= intrev32ifbe(is->encoding) && intsetSearch(is,value,NULL);
}

/* Return random member */
// 返回一个随机的成员
int64_t intsetRandom(intset *is) {
    uint32_t len = intrev32ifbe(is->length);
    assert(len); /* avoid division by zero on corrupt intset payload. */
    // 根据长度，随机取一个位置
    return _intsetGet(is,rand()%len);
}

/* Return the largest member. */
// 返回整数集合中最大的成员
int64_t intsetMax(intset *is) {
    uint32_t len = intrev32ifbe(is->length);
    return _intsetGet(is, len - 1);
}

/* Return the smallest member. */
// 返回整数集合中最小的成员
int64_t intsetMin(intset *is) {
    return _intsetGet(is, 0);
}

/* Get the value at the given position. When this position is
 * out of range the function returns 0, when in range it returns 1. */
// 得到指定位置的值，如果指定位置已经超出整数集合的范围，返回0，在范围中返回1。
uint8_t intsetGet(intset *is, uint32_t pos, int64_t *value) {
    // 位置小于长度
    if (pos < intrev32ifbe(is->length)) {
        // 取出值并返回1
        *value = _intsetGet(is,pos);
        return 1;
    }
    return 0;
}

/* Return intset length */
// 返回整数集合的长度
uint32_t intsetLen(const intset *is) {
    return intrev32ifbe(is->length);
}

/* Return intset blob size in bytes. */
// 返回整数集合的所需要的字节数
size_t intsetBlobLen(intset *is) {
    return sizeof(intset)+(size_t)intrev32ifbe(is->length)*intrev32ifbe(is->encoding);
}

/* Validate the integrity of the data structure.
 * when `deep` is 0, only the integrity of the header is validated.
 * when `deep` is 1, we make sure there are no duplicate or out of order records. */
// 验证数据结构的完整性。 
// 当 `deep` 为 0 时，仅验证标头的完整性。
// 当 `deep` 为 1 时，我们确保没有重复或乱序的记录。
int intsetValidateIntegrity(const unsigned char *p, size_t size, int deep) {
    intset *is = (intset *)p;
    /* check that we can actually read the header. */
    // size如果比最小的尺寸还小，直接返回0
    if (size < sizeof(*is))
        return 0;

    uint32_t encoding = intrev32ifbe(is->encoding);

    // 单个元素的字节数
    size_t record_size;
    if (encoding == INTSET_ENC_INT64) {
        record_size = INTSET_ENC_INT64;
    } else if (encoding == INTSET_ENC_INT32) {
        record_size = INTSET_ENC_INT32;
    } else if (encoding == INTSET_ENC_INT16){
        record_size = INTSET_ENC_INT16;
    } else {
        return 0;
    }

    /* check that the size matches (all records are inside the buffer). */
    // 整个字节数是否符合预期
    uint32_t count = intrev32ifbe(is->length);
    // 不相等就返回0
    if (sizeof(*is) + count*record_size != size)
        return 0;

    /* check that the set is not empty. */
    // 检查集合不为空
    if (count==0)
        return 0;

    // 如果深度为0，检查到此结束
    if (!deep)
        return 1;

    /* check that there are no dup or out of order records. */
    // 检查是否有重复的或者乱序
    int64_t prev = _intsetGet(is,0);
    for (uint32_t i=1; i<count; i++) {
        int64_t cur = _intsetGet(is,i);
        // 如果后面的小于或等于前一个，就返回0
        if (cur <= prev)
            return 0;
        prev = cur;
    }
    // 都没有问题返回1 
    return 1;
}

#ifdef REDIS_TEST
#include <sys/time.h>
#include <time.h>

#if 0
static void intsetRepr(intset *is) {
    for (uint32_t i = 0; i < intrev32ifbe(is->length); i++) {
        printf("%lld\n", (uint64_t)_intsetGet(is,i));
    }
    printf("\n");
}

static void error(char *err) {
    printf("%s\n", err);
    exit(1);
}
#endif

static void ok(void) {
    printf("OK\n");
}

static long long usec(void) {
    struct timeval tv;
    gettimeofday(&tv,NULL);
    return (((long long)tv.tv_sec)*1000000)+tv.tv_usec;
}

static intset *createSet(int bits, int size) {
    uint64_t mask = (1<<bits)-1;
    uint64_t value;
    intset *is = intsetNew();

    for (int i = 0; i < size; i++) {
        if (bits > 32) {
            value = (rand()*rand()) & mask;
        } else {
            value = rand() & mask;
        }
        is = intsetAdd(is,value,NULL);
    }
    return is;
}

static void checkConsistency(intset *is) {
    for (uint32_t i = 0; i < (intrev32ifbe(is->length)-1); i++) {
        uint32_t encoding = intrev32ifbe(is->encoding);

        if (encoding == INTSET_ENC_INT16) {
            int16_t *i16 = (int16_t*)is->contents;
            assert(i16[i] < i16[i+1]);
        } else if (encoding == INTSET_ENC_INT32) {
            int32_t *i32 = (int32_t*)is->contents;
            assert(i32[i] < i32[i+1]);
        } else {
            int64_t *i64 = (int64_t*)is->contents;
            assert(i64[i] < i64[i+1]);
        }
    }
}

#define UNUSED(x) (void)(x)
int intsetTest(int argc, char **argv, int flags) {
    uint8_t success;
    int i;
    intset *is;
    srand(time(NULL));

    UNUSED(argc);
    UNUSED(argv);
    UNUSED(flags);

    printf("Value encodings: "); {
        assert(_intsetValueEncoding(-32768) == INTSET_ENC_INT16);
        assert(_intsetValueEncoding(+32767) == INTSET_ENC_INT16);
        assert(_intsetValueEncoding(-32769) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(+32768) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(-2147483648) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(+2147483647) == INTSET_ENC_INT32);
        assert(_intsetValueEncoding(-2147483649) == INTSET_ENC_INT64);
        assert(_intsetValueEncoding(+2147483648) == INTSET_ENC_INT64);
        assert(_intsetValueEncoding(-9223372036854775808ull) ==
                    INTSET_ENC_INT64);
        assert(_intsetValueEncoding(+9223372036854775807ull) ==
                    INTSET_ENC_INT64);
        ok();
    }

    printf("Basic adding: "); {
        is = intsetNew();
        is = intsetAdd(is,5,&success); assert(success);
        is = intsetAdd(is,6,&success); assert(success);
        is = intsetAdd(is,4,&success); assert(success);
        is = intsetAdd(is,4,&success); assert(!success);
        assert(6 == intsetMax(is));
        assert(4 == intsetMin(is));
        ok();
        zfree(is);
    }

    printf("Large number of random adds: "); {
        uint32_t inserts = 0;
        is = intsetNew();
        for (i = 0; i < 1024; i++) {
            is = intsetAdd(is,rand()%0x800,&success);
            if (success) inserts++;
        }
        assert(intrev32ifbe(is->length) == inserts);
        checkConsistency(is);
        ok();
        zfree(is);
    }

    printf("Upgrade from int16 to int32: "); {
        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        assert(intsetFind(is,32));
        assert(intsetFind(is,65535));
        checkConsistency(is);
        zfree(is);

        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,-65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        assert(intsetFind(is,32));
        assert(intsetFind(is,-65535));
        checkConsistency(is);
        ok();
        zfree(is);
    }

    printf("Upgrade from int16 to int64: "); {
        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,32));
        assert(intsetFind(is,4294967295));
        checkConsistency(is);
        zfree(is);

        is = intsetNew();
        is = intsetAdd(is,32,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT16);
        is = intsetAdd(is,-4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,32));
        assert(intsetFind(is,-4294967295));
        checkConsistency(is);
        ok();
        zfree(is);
    }

    printf("Upgrade from int32 to int64: "); {
        is = intsetNew();
        is = intsetAdd(is,65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        is = intsetAdd(is,4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,65535));
        assert(intsetFind(is,4294967295));
        checkConsistency(is);
        zfree(is);

        is = intsetNew();
        is = intsetAdd(is,65535,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT32);
        is = intsetAdd(is,-4294967295,NULL);
        assert(intrev32ifbe(is->encoding) == INTSET_ENC_INT64);
        assert(intsetFind(is,65535));
        assert(intsetFind(is,-4294967295));
        checkConsistency(is);
        ok();
        zfree(is);
    }

    printf("Stress lookups: "); {
        long num = 100000, size = 10000;
        int i, bits = 20;
        long long start;
        is = createSet(bits,size);
        checkConsistency(is);

        start = usec();
        for (i = 0; i < num; i++) intsetSearch(is,rand() % ((1<<bits)-1),NULL);
        printf("%ld lookups, %ld element set, %lldusec\n",
               num,size,usec()-start);
        zfree(is);
    }

    printf("Stress add+delete: "); {
        int i, v1, v2;
        is = intsetNew();
        for (i = 0; i < 0xffff; i++) {
            v1 = rand() % 0xfff;
            is = intsetAdd(is,v1,NULL);
            assert(intsetFind(is,v1));

            v2 = rand() % 0xfff;
            is = intsetRemove(is,v2,NULL);
            assert(!intsetFind(is,v2));
        }
        checkConsistency(is);
        ok();
        zfree(is);
    }

    return 0;
}
#endif
