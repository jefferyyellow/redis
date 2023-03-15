/* Hash table implementation.
 *
 * This file implements in memory hash tables with insert/del/replace/find/
 * get-random-element operations. Hash tables will auto resize if needed
 * tables of power of two in size are used, collisions are handled by
 * chaining. See the source code for more information... :)
 *
 * Copyright (c) 2006-2010, Salvatore Sanfilippo <antirez at gmail dot com>
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
#include "alloc.h"
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include "dict.h"

/* -------------------------- private prototypes ---------------------------- */

static int _dictExpandIfNeeded(dict *ht);
static unsigned long _dictNextPower(unsigned long size);
static int _dictKeyIndex(dict *ht, const void *key);
static int _dictInit(dict *ht, dictType *type, void *privDataPtr);

/* -------------------------- hash functions -------------------------------- */

/* Generic hash function (a popular one from Bernstein).
 * I tested a few and this was the best. */
// 通用的hash算法（time33 hash算法）
static unsigned int dictGenHashFunction(const unsigned char *buf, int len) {
    unsigned int hash = 5381;

    while (len--)
        hash = ((hash << 5) + hash) + (*buf++); /* hash * 33 + c */
    return hash;
}

/* ----------------------------- API implementation ------------------------- */

/* Reset an hashtable already initialized with ht_init().
 * NOTE: This function should only called by ht_destroy(). */
// 重置字典的各个字段
static void _dictReset(dict *ht) {
    ht->table = NULL;
    ht->size = 0;
    ht->sizemask = 0;
    ht->used = 0;
}

/* Create a new hash table */
// 创建新的字典
static dict *dictCreate(dictType *type, void *privDataPtr) {
    dict *ht = hi_malloc(sizeof(*ht));
    if (ht == NULL)
        return NULL;
    // 初始化
    _dictInit(ht,type,privDataPtr);
    return ht;
}

/* Initialize the hash table */
// 初始化字典
static int _dictInit(dict *ht, dictType *type, void *privDataPtr) {
    _dictReset(ht);
    ht->type = type;
    ht->privdata = privDataPtr;
    return DICT_OK;
}

/* Expand or create the hashtable */
// 扩展或者创建hash表
static int dictExpand(dict *ht, unsigned long size) {
    dict n; /* the new hashtable */
    // 得到大于等于size的最小2的幂来作为hash表的大小
    unsigned long realsize = _dictNextPower(size), i;

    /* the size is invalid if it is smaller than the number of
     * elements already inside the hashtable */
    // 如果大小比hash表中现有的元素数目还小，直接返回错误
    if (ht->used > size)
        return DICT_ERR;
    // 初始化新的
    _dictInit(&n, ht->type, ht->privdata);
    n.size = realsize;
    n.sizemask = realsize-1;
    // 根据容量的大小分配节点的内存
    n.table = hi_calloc(realsize,sizeof(dictEntry*));
    if (n.table == NULL)
        return DICT_ERR;

    /* Copy all the elements from the old to the new table:
     * note that if the old hash table is empty ht->size is zero,
     * so dictExpand just creates an hash table. */
    // 已经使用的数量直接拷贝
    n.used = ht->used;
    // 遍历hash表，进行重新hash和链接
    for (i = 0; i < ht->size && ht->used > 0; i++) {
        dictEntry *he, *nextHe;
        // 当前的槽位没用元素链接
        if (ht->table[i] == NULL) continue;

        /* For each hash entry on this slot... */
        // 出来槽位中的每一个条目
        he = ht->table[i];
        while(he) {
            unsigned int h;

            nextHe = he->next;
            /* Get the new element index */
            // 在新的hash table中重新hash
            h = dictHashKey(ht, he->key) & n.sizemask;
            // 链接入新的槽位中
            he->next = n.table[h];
            n.table[h] = he;
            // 减少当前没处理的节点
            ht->used--;
            /* Pass to the next element */
            // 进入到下一个条目中
            he = nextHe;
        }
    }
    assert(ht->used == 0);
    // 原来的hash table释放掉
    hi_free(ht->table);

    /* Remap the new hashtable in the old */
    // 将新的赋值给原来的字典
    *ht = n;
    return DICT_OK;
}

/* Add an element to the target hash table */
// hash表增加一个元素
static int dictAdd(dict *ht, void *key, void *val) {
    int index;
    dictEntry *entry;

    /* Get the index of the new element, or -1 if
     * the element already exists. */
    // 得到新元素的槽位索引，-1表示已经存在
    if ((index = _dictKeyIndex(ht, key)) == -1)
        return DICT_ERR;

    /* Allocates the memory and stores key */
    // 分配条目所需的内存
    entry = hi_malloc(sizeof(*entry));
    if (entry == NULL)
        return DICT_ERR;

    // 加入到槽位的开头处
    entry->next = ht->table[index];
    ht->table[index] = entry;

    /* Set the hash entry fields. */
    // 设置条目的键、值
    dictSetHashKey(ht, entry, key);
    dictSetHashVal(ht, entry, val);
    // 增加已经存在的条目数量
    ht->used++;
    return DICT_OK;
}

/* Add an element, discarding the old if the key already exists.
 * Return 1 if the key was added from scratch, 0 if there was already an
 * element with such key and dictReplace() just performed a value update
 * operation. */
// 增加一个元素，如果键已经存在则覆盖原来的值。
// 如果增加一个元素，就返回1，如果是更新值，就返回0
static int dictReplace(dict *ht, void *key, void *val) {
    dictEntry *entry, auxentry;

    /* Try to add the element. If the key
     * does not exists dictAdd will succeed. */
    // 先尝试增加元素，如果键不存在一般都会增加成功
    if (dictAdd(ht, key, val) == DICT_OK)
        return 1;
    /* It already exists, get the entry */
    // 如果键对应的条目已经存在，获得条目
    entry = dictFind(ht, key);
    if (entry == NULL)
        return 0;

    /* Free the old value and set the new one */
    /* Set the new value and free the old one. Note that it is important
     * to do that in this order, as the value may just be exactly the same
     * as the previous one. In this context, think to reference counting,
     * you want to increment (set), and then decrement (free), and not the
     * reverse. */
    // 设置新的值，释放掉原来的值，注意下面的顺序，如果新的值和老的值是同一个，
    // 并且值是通过引用计数来维护，如果先释放会导致引用计数减少，可能回导致
    // 资源真的释放，所以得先设置新得值，然后释放老的值，这个顺序不能反过来。
    auxentry = *entry;
    dictSetHashVal(ht, entry, val);
    dictFreeEntryVal(ht, &auxentry);
    return 0;
}

/* Search and remove an element */
// 搜索并并删除一个元素
static int dictDelete(dict *ht, const void *key) {
    unsigned int h;
    dictEntry *de, *prevde;
    // 没用元素，直接返回错误
    if (ht->size == 0)
        return DICT_ERR;
    // 得到槽位索引
    h = dictHashKey(ht, key) & ht->sizemask;
    // 槽位的首个条目
    de = ht->table[h];

    prevde = NULL;
    // 遍历所有的条目
    while(de) {
        if (dictCompareHashKeys(ht,key,de->key)) {
            /* Unlink the element from the list */
            // 从列表中断开，有前一个条目，链接起来
            // 没有前一个条目，说明删除的就是第一个条目，直接放入槽位中
            if (prevde)
                prevde->next = de->next;
            else
                ht->table[h] = de->next;
            // 释放条目键、值、节点本身
            dictFreeEntryKey(ht,de);
            dictFreeEntryVal(ht,de);
            hi_free(de);
            // 减少当前节点的数量
            ht->used--;
            return DICT_OK;
        }
        prevde = de;
        de = de->next;
    }
    // 没找到，返回错误
    return DICT_ERR; /* not found */
}

/* Destroy an entire hash table */
// 销毁整个hash表
static int _dictClear(dict *ht) {
    unsigned long i;

    /* Free all the elements */
    // 遍历每一个槽位，释放所有的元素
    for (i = 0; i < ht->size && ht->used > 0; i++) {
        dictEntry *he, *nextHe;
        
        if ((he = ht->table[i]) == NULL) continue;
        // 有元素的槽位，遍历释放
        while(he) {
            nextHe = he->next;
            //释放键、值、和本身
            dictFreeEntryKey(ht, he);
            dictFreeEntryVal(ht, he);
            hi_free(he);
            ht->used--;
            he = nextHe;
        }
    }
    /* Free the table and the allocated cache structure */
    // 释放哈希表本身
    hi_free(ht->table);
    /* Re-initialize the table */
    // 重置表
    _dictReset(ht);
    return DICT_OK; /* never fails */
}

/* Clear & Release the hash table */
// 释放字典，清空和释放hash表
static void dictRelease(dict *ht) {
    // 清空hast表
    _dictClear(ht);
    // 将字典本身释放
    hi_free(ht);
}

// 搜索键对应的条目
static dictEntry *dictFind(dict *ht, const void *key) {
    dictEntry *he;
    unsigned int h;
    
    if (ht->size == 0) return NULL;
    // 计数hash槽位
    h = dictHashKey(ht, key) & ht->sizemask;
    // 遍历槽位上所有的条目
    he = ht->table[h];
    while(he) {
        // 比较键是否相等
        if (dictCompareHashKeys(ht, key, he->key))
            // 返回找到的条目
            return he;
        he = he->next;
    }
    // 没找到
    return NULL;
}

// 初始化迭代器
static void dictInitIterator(dictIterator *iter, dict *ht) {
    iter->ht = ht;
    iter->index = -1;
    iter->entry = NULL;
    iter->nextEntry = NULL;
}

// 取下一个条目，可以用于遍历整个字典
static dictEntry *dictNext(dictIterator *iter) {
    while (1) {
        // 如果到了结尾，就下一个槽位
        if (iter->entry == NULL) {
            iter->index++;
            // 如果已经遍历完整个槽位，终结循环
            if (iter->index >=
                    (signed)iter->ht->size) break;
            // 得到下一个槽位的首条目
            iter->entry = iter->ht->table[iter->index];
        } else {
            // 进入下一条
            iter->entry = iter->nextEntry;
        }
        // 由于玩家可能删除当前的条目，所有在返回之前，将下一条目保存
        if (iter->entry) {
            /* We need to save the 'next' here, the iterator user
             * may delete the entry we are returning. */
            iter->nextEntry = iter->entry->next;
            return iter->entry;
        }
    }
    return NULL;
}

/* ------------------------- private functions ------------------------------ */

/* Expand the hash table if needed */
// 如有需要扩展hash表
static int _dictExpandIfNeeded(dict *ht) {
    /* If the hash table is empty expand it to the initial size,
     * if the table is "full" double its size. */
    // 如果是空的，就扩展到初始大小
    if (ht->size == 0)
        return dictExpand(ht, DICT_HT_INITIAL_SIZE);
    // 如果满了，就扩展成2倍现在的大小
    if (ht->used == ht->size)
        return dictExpand(ht, ht->size*2);
    return DICT_OK;
}

/* Our hash table capability is a power of two */
// hash table的容量是2的幂
static unsigned long _dictNextPower(unsigned long size) {
    unsigned long i = DICT_HT_INITIAL_SIZE;
    // 最大不超过整数的上限
    if (size >= LONG_MAX) return LONG_MAX;
    // 得到大于等于size的最小的2的幂
    while(1) {
        if (i >= size)
            return i;
        i *= 2;
    }
}

/* Returns the index of a free slot that can be populated with
 * an hash entry for the given 'key'.
 * If the key already exists, -1 is returned. */
// 返回可用槽位的索引，该槽位可以用给定“键”的哈希条目来填充。
// 如果hash表中已经存在该值，就返回-1
static int _dictKeyIndex(dict *ht, const void *key) {
    unsigned int h;
    dictEntry *he;

    /* Expand the hashtable if needed */
    // 如果需要的话扩展hash表
    if (_dictExpandIfNeeded(ht) == DICT_ERR)
        return -1;
    /* Compute the key hash value */
    // 计数槽位值
    h = dictHashKey(ht, key) & ht->sizemask;
    /* Search if this slot does not already contain the given key */
    // 搜索槽位中是否已经包含给定的键
    he = ht->table[h];
    while(he) {
        // 比较条目的键是否相等
        if (dictCompareHashKeys(ht, key, he->key))
            return -1;
        // 下一个条目
        he = he->next;
    }
    // 没用就返回槽位索引
    return h;
}

