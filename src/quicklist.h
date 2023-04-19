/* quicklist.h - A generic doubly linked quicklist implementation
 *
 * Copyright (c) 2014, Matt Stancliff <matt@genges.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this quicklist of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this quicklist of conditions and the following disclaimer in the
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

#include <stdint.h> // for UINTPTR_MAX

#ifndef __QUICKLIST_H__
#define __QUICKLIST_H__

/* Node, quicklist, and Iterator are the only data structures used currently. */

/* quicklistNode is a 32 byte struct describing a listpack for a quicklist.
 * We use bit fields keep the quicklistNode at 32 bytes.
 * count: 16 bits, max 65536 (max lp bytes is 65k, so max count actually < 32k).
 * encoding: 2 bits, RAW=1, LZF=2.
 * container: 2 bits, PLAIN=1 (a single item as char array), PACKED=2 (listpack with multiple items).
 * recompress: 1 bit, bool, true if node is temporary decompressed for usage.
 * attempted_compress: 1 bit, boolean, used for verifying during testing.
 * extra: 10 bits, free for future use; pads out the remainder of 32 bits */
// 快速列表节点
typedef struct quicklistNode {
    // 前一个节点
    struct quicklistNode *prev;
    // 后一个节点
    struct quicklistNode *next;
    // 条目的指针(list pack)
    unsigned char *entry;
    // 条目的总字节大小
    size_t sz;             /* entry size in bytes */
    // 紧凑列表中数据项的数目
    unsigned int count : 16;     /* count of items in listpack */
    // 编码方式：元素为1，LZF为2
    unsigned int encoding : 2;   /* RAW==1 or LZF==2 */
    // 平坦的为1，紧凑的为2
    unsigned int container : 2;  /* PLAIN==1 or PACKED==2 */
    // 这个节点之前压缩过，标志临时性解压，后面操作完要压缩回去
    unsigned int recompress : 1; /* was this node previous compressed? */
    // 节点太小不需要压缩
    unsigned int attempted_compress : 1; /* node can't compress; too small */
    // 防止压缩稍后将使用的条目
    unsigned int dont_compress : 1; /* prevent compression of entry that will be used later */
    // 预留的位，用于以后的扩展
    unsigned int extra : 9; /* more bits to steal for future usage */
} quicklistNode;

/* quicklistLZF is a 8+N byte struct holding 'sz' followed by 'compressed'.
 * 'sz' is byte length of 'compressed' field.
 * 'compressed' is LZF data with total (compressed) length 'sz'
 * NOTE: uncompressed length is stored in quicklistNode->sz.
 * When quicklistNode->entry is compressed, node->entry points to a quicklistLZF */
// 没压缩的长度保存在quicklistNode的sz中，然而quicklistNode的entry是压缩状态时，
// node->entry指向了一个quicklistLZF结构
typedef struct quicklistLZF {
    // 压缩内容(compressed)所占字节大小
    size_t sz; /* LZF size in bytes*/
    // 压缩内容
    char compressed[];
} quicklistLZF;

/* Bookmarks are padded with realloc at the end of of the quicklist struct.
 * They should only be used for very big lists if thousands of nodes were the
 * excess memory usage is negligible, and there's a real need to iterate on them
 * in portions.
 * When not used, they don't add any memory overhead, but when used and then
 * deleted, some overhead remains (to avoid resonance).
 * The number of bookmarks used should be kept to minimum since it also adds
 * overhead on node deletion (searching for a bookmark to update). */
// 书签分配在快速列表结构的末尾。
// 它们应该只用于非常大的列表，如果有数千个节点，那么多出来的内存使用可以忽略不计，
// 快速链表书签，并且确实需要的时候遍历其中的一部分。
// 不使用时，它们不会增加任何内存开销，但在使用后删除时，一些开销仍然存在（为了来回的分配内存）
// 使用的书签数量应保持在最低限度，因为它还会增加节点删除的开销（搜索要更新的书签）
typedef struct quicklistBookmark {
    // 书签指向的快速列表的节点
    quicklistNode *node;
    // 书签名字
    char *name;
} quicklistBookmark;

#if UINTPTR_MAX == 0xffffffff
/* 32-bit */
#   define QL_FILL_BITS 14
#   define QL_COMP_BITS 14
#   define QL_BM_BITS 4
#elif UINTPTR_MAX == 0xffffffffffffffff
/* 64-bit */
#   define QL_FILL_BITS 16
#   define QL_COMP_BITS 16
#   define QL_BM_BITS 4 /* we can encode more, but we rather limit the user
                           since they cause performance degradation. */
#else
#   error unknown arch bits count
#endif

/* quicklist is a 40 byte struct (on 64-bit systems) describing a quicklist.
 * 'count' is the number of total entries.
 * 'len' is the number of quicklist nodes.
 * 'compress' is: 0 if compression disabled, otherwise it's the number
 *                of quicklistNodes to leave uncompressed at ends of quicklist.
 * 'fill' is the user-requested (or default) fill factor.
 * 'bookmarks are an optional feature that is used by realloc this struct,
 *      so that they don't consume memory when not used. */
// 书签是重新分配这个结构使用的一个可选特性，这样它们在不使用时就不会消耗内存。

// 快速链表结构
typedef struct quicklist {
    // 首节点
    quicklistNode *head;
    // 尾节点
    quicklistNode *tail;
    // 数据项总数
    unsigned long count;        /* total count of all entries in all listpacks */
    // quicklistNodes的节点数目
    unsigned long len;          /* number of quicklistNodes */
    // 单独节点的填充因子
    // 当fill为正数时，表明每个ziplist最多含有的数据项数，
    // 当fill为负数时: -1：Ziplist节点最大为4KB     -2：Ziplist节点最大为8KB
    // -3：Ziplist节点最大为16KB，-4：Ziplist节点最大为32KB，-5：Ziplist节点最大为64KB
    // 默认为-2
    signed int fill : QL_FILL_BITS;       /* fill factor for individual nodes */
    // 压缩深度，就是ziplist两端各有多少个节点不压缩
    unsigned int compress : QL_COMP_BITS; /* depth of end nodes not to compress;0=off */
    // 书签数组的最大长度
    unsigned int bookmark_count: QL_BM_BITS;
    // 书签的可扩展数组
    quicklistBookmark bookmarks[];
} quicklist;

// 快速列表的迭代器
typedef struct quicklistIter {
    // 当前元素所在的快速列表
    quicklist *quicklist;
    // 当前元素所在的节点
    quicklistNode *current;
    // 指向当前的元素
    unsigned char *zi; /* points to the current element */
    // 在紧凑表中的偏移
    long offset; /* offset in current listpack */
    // 迭代器的方向
    int direction;
} quicklistIter;

// 快速列表节点中的条目
typedef struct quicklistEntry {
    // 所在的快速列表
    const quicklist *quicklist;
    // 所在的快速列表节点
    quicklistNode *node;
    // 所在的list pack的偏移
    unsigned char *zi;
    // 指向该节点的字符串内容
    unsigned char *value;
    // 该节点的整型值
    long long longval;
    // 该节点的大小
    size_t sz;
    // 该条目的偏移
    int offset;
} quicklistEntry;

// 快速列表的头，尾
#define QUICKLIST_HEAD 0
#define QUICKLIST_TAIL -1

/* quicklist node encodings */
// 快速列表的节点编码
#define QUICKLIST_NODE_ENCODING_RAW 1
#define QUICKLIST_NODE_ENCODING_LZF 2

/* quicklist compression disable */
// 快速列表禁用压缩
#define QUICKLIST_NOCOMPRESS 0

/* quicklist node container formats */
// 快速列表节点容器格式
// 普通的，就是直接一块内存空间
#define QUICKLIST_NODE_CONTAINER_PLAIN 1
#define QUICKLIST_NODE_CONTAINER_PACKED 2

// 快速列表节点是否平坦
#define QL_NODE_IS_PLAIN(node) ((node)->container == QUICKLIST_NODE_CONTAINER_PLAIN)

// 快速列表节点是否压缩
#define quicklistNodeIsCompressed(node)                                        \
    ((node)->encoding == QUICKLIST_NODE_ENCODING_LZF)

/* Prototypes */
quicklist *quicklistCreate(void);
quicklist *quicklistNew(int fill, int compress);
void quicklistSetCompressDepth(quicklist *quicklist, int depth);
void quicklistSetFill(quicklist *quicklist, int fill);
void quicklistSetOptions(quicklist *quicklist, int fill, int depth);
void quicklistRelease(quicklist *quicklist);
int quicklistPushHead(quicklist *quicklist, void *value, const size_t sz);
int quicklistPushTail(quicklist *quicklist, void *value, const size_t sz);
void quicklistPush(quicklist *quicklist, void *value, const size_t sz,
                   int where);
void quicklistAppendListpack(quicklist *quicklist, unsigned char *zl);
void quicklistAppendPlainNode(quicklist *quicklist, unsigned char *data, size_t sz);
void quicklistInsertAfter(quicklistIter *iter, quicklistEntry *entry,
                          void *value, const size_t sz);
void quicklistInsertBefore(quicklistIter *iter, quicklistEntry *entry,
                           void *value, const size_t sz);
void quicklistDelEntry(quicklistIter *iter, quicklistEntry *entry);
void quicklistReplaceEntry(quicklistIter *iter, quicklistEntry *entry,
                           void *data, size_t sz);
int quicklistReplaceAtIndex(quicklist *quicklist, long index, void *data,
                            const size_t sz);
int quicklistDelRange(quicklist *quicklist, const long start, const long stop);
quicklistIter *quicklistGetIterator(quicklist *quicklist, int direction);
quicklistIter *quicklistGetIteratorAtIdx(quicklist *quicklist,
                                         int direction, const long long idx);
quicklistIter *quicklistGetIteratorEntryAtIdx(quicklist *quicklist, const long long index,
                                              quicklistEntry *entry);
int quicklistNext(quicklistIter *iter, quicklistEntry *entry);
void quicklistSetDirection(quicklistIter *iter, int direction);
void quicklistReleaseIterator(quicklistIter *iter);
quicklist *quicklistDup(quicklist *orig);
void quicklistRotate(quicklist *quicklist);
int quicklistPopCustom(quicklist *quicklist, int where, unsigned char **data,
                       size_t *sz, long long *sval,
                       void *(*saver)(unsigned char *data, size_t sz));
int quicklistPop(quicklist *quicklist, int where, unsigned char **data,
                 size_t *sz, long long *slong);
unsigned long quicklistCount(const quicklist *ql);
int quicklistCompare(quicklistEntry *entry, unsigned char *p2, const size_t p2_len);
size_t quicklistGetLzf(const quicklistNode *node, void **data);
void quicklistNodeLimit(int fill, size_t *size, unsigned int *count);
int quicklistNodeExceedsLimit(int fill, size_t new_sz, unsigned int new_count);
void quicklistRepr(unsigned char *ql, int full);

/* bookmarks */
int quicklistBookmarkCreate(quicklist **ql_ref, const char *name, quicklistNode *node);
int quicklistBookmarkDelete(quicklist *ql, const char *name);
quicklistNode *quicklistBookmarkFind(quicklist *ql, const char *name);
void quicklistBookmarksClear(quicklist *ql);
int quicklistisSetPackedThreshold(size_t sz);

#ifdef REDIS_TEST
int quicklistTest(int argc, char *argv[], int flags);
#endif

/* Directions for iterators */
#define AL_START_HEAD 0
#define AL_START_TAIL 1

#endif /* __QUICKLIST_H__ */
