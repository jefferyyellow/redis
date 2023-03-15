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

#ifndef __DICT_H
#define __DICT_H

#define DICT_OK 0
#define DICT_ERR 1

/* Unused arguments generate annoying warnings... */
#define DICT_NOTUSED(V) ((void) V)

// 字典节点，键值对和链表结构
typedef struct dictEntry {
    // 键
    void *key;
    // 值
    void *val;
    // 链表结构
    struct dictEntry *next;
} dictEntry;

// 字典类型
typedef struct dictType {
    // 字典对应的hash函数
    unsigned int (*hashFunction)(const void *key);
    // 键复制函数
    void *(*keyDup)(void *privdata, const void *key);
    // 值复制函数
    void *(*valDup)(void *privdata, const void *obj);
    // 键比较函数
    int (*keyCompare)(void *privdata, const void *key1, const void *key2);
    // 键销毁函数
    void (*keyDestructor)(void *privdata, void *key);
    // 值销毁函数
    void (*valDestructor)(void *privdata, void *obj);
} dictType;

// 字典
typedef struct dict {
    // 指针数组(hash桶)
    dictEntry **table;
    // 字典对应的特定操作函数
    dictType *type;
    // table数组的大小
    unsigned long size;
    // 掩码 = size - 1
    unsigned long sizemask;
    // table数组已存元素个数，包含next单链表
    unsigned long used;
    // 私有数据，配合type字段指向的函数一起使用
    void *privdata;
} dict;

// 字典迭代器
typedef struct dictIterator {
    // 操作的字典
    dict *ht;
    // 字典中table的索引
    int index;
    // 当前节点和下一个节点
    dictEntry *entry, *nextEntry;
} dictIterator;

/* This is the initial size of every hash table */
#define DICT_HT_INITIAL_SIZE     4

/* ------------------------------- Macros ------------------------------------*/
// 释放节点的值
#define dictFreeEntryVal(ht, entry) \
    if ((ht)->type->valDestructor) \
        (ht)->type->valDestructor((ht)->privdata, (entry)->val)

// 设置节点的值
#define dictSetHashVal(ht, entry, _val_) do { \
    if ((ht)->type->valDup) \
        entry->val = (ht)->type->valDup((ht)->privdata, _val_); \
    else \
        entry->val = (_val_); \
} while(0)

// 释放节点的键
#define dictFreeEntryKey(ht, entry) \
    if ((ht)->type->keyDestructor) \
        (ht)->type->keyDestructor((ht)->privdata, (entry)->key)

// 设置节点的键
#define dictSetHashKey(ht, entry, _key_) do { \
    if ((ht)->type->keyDup) \
        entry->key = (ht)->type->keyDup((ht)->privdata, _key_); \
    else \
        entry->key = (_key_); \
} while(0)

// 节点的键比较
#define dictCompareHashKeys(ht, key1, key2) \
    (((ht)->type->keyCompare) ? \
        (ht)->type->keyCompare((ht)->privdata, key1, key2) : \
        (key1) == (key2))

// 对键进行hash
#define dictHashKey(ht, key) (ht)->type->hashFunction(key)

// 得到节点的键
#define dictGetEntryKey(he) ((he)->key)
// 得到节点的值
#define dictGetEntryVal(he) ((he)->val)
// 得到字典的插槽（桶位）大小
#define dictSlots(ht) ((ht)->size)
// 得到字典的节点数（已经使用的数）
#define dictSize(ht) ((ht)->used)

/* API */
// 通用的hash算法（time33 hash算法）
static unsigned int dictGenHashFunction(const unsigned char *buf, int len);
// 创建新的字典
static dict *dictCreate(dictType *type, void *privDataPtr);
// 扩展或者创建hash表
static int dictExpand(dict *ht, unsigned long size);
// hash表增加一个元素
static int dictAdd(dict *ht, void *key, void *val);
// 增加一个元素，如果键已经存在则覆盖原来的值。
static int dictReplace(dict *ht, void *key, void *val);
// 搜索并并删除一个元素
static int dictDelete(dict *ht, const void *key);
// 释放字典，清空和释放hash表
static void dictRelease(dict *ht);
// 搜索键对应的条目
static dictEntry * dictFind(dict *ht, const void *key);
// 初始化迭代器
static void dictInitIterator(dictIterator *iter, dict *ht);
// 取下一个条目，可以用于遍历整个字典
static dictEntry *dictNext(dictIterator *iter);

#endif /* __DICT_H */
