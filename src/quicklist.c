/* quicklist.c - A doubly linked list of listpacks
 *
 * Copyright (c) 2014, Matt Stancliff <matt@genges.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must start the above copyright notice,
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

#include <stdio.h>
#include <string.h> /* for memcpy */
#include <limits.h>
#include "quicklist.h"
#include "zmalloc.h"
#include "config.h"
#include "listpack.h"
#include "util.h" /* for ll2string */
#include "lzf.h"
#include "redisassert.h"

#ifndef REDIS_STATIC
#define REDIS_STATIC static
#endif

/* Optimization levels for size-based filling.
 * Note that the largest possible limit is 64k, so even if each record takes
 * just one byte, it still won't overflow the 16 bit count field. */
// 基于大小填充的优化等级
// 注意：最大可能的限制是 64k，因此即使每条记录只占用一个字节，它仍然不会溢出16位计数的字段。
static const size_t optimization_level[] = {4096, 8192, 16384, 32768, 65536};

/* packed_threshold is initialized to 1gb*/
// 打包的阈值初始化位1GB
static size_t packed_threshold = (1 << 30);

/* set threshold for PLAIN nodes, the real limit is 4gb */
// 平台节点的阈值，真正的限制是4GB
#define isLargeElement(size) ((size) >= packed_threshold)

int quicklistisSetPackedThreshold(size_t sz) {
    /* Don't allow threshold to be set above or even slightly below 4GB */
    // 不允许将阈值设置为高于或略低于4GB
    if (sz > (1ull<<32) - (1<<20)) {
        return 0;
    } 
    // 0意味着重置阈值，就是1GB
    else if (sz == 0) { /* 0 means restore threshold */
        sz = (1 << 30);
    }
    // 设置阈值
    packed_threshold = sz;
    return 1;
}

/* Maximum size in bytes of any multi-element listpack.
 * Larger values will live in their own isolated listpacks.
 * This is used only if we're limited by record count. when we're limited by
 * size, the maximum limit is bigger, but still safe.
 * 8k is a recommended / default size limit */
 // 任何多元素紧凑列表的最大大小（以字节为单位）。比这个还大的话将存在于它们自己的独立紧凑列表中。
 // 仅当我们受记录数限制时才使用。 当我们受大小限制时，最大限制更大，但仍然安全。
 // 8k是推荐/默认大小限制
#define SIZE_SAFETY_LIMIT 8192

/* Maximum estimate of the listpack entry overhead.
 * Although in the worst case(sz < 64), we will waste 6 bytes in one
 * quicklistNode, but can avoid memory waste due to internal fragmentation
 * when the listpack exceeds the size limit by a few bytes (e.g. being 16388). */
// 紧凑列表条目开销的最大估计
// 虽然在最坏的情况下（sz < 64)，我们需要在一个quicklistNode上浪费6个字节，
// 但是可以避免当紧凑表超出尺寸限制几个字节（比如：16388）由于内部碎片而造成的内存浪费。
#define SIZE_ESTIMATE_OVERHEAD 8

/* Minimum listpack size in bytes for attempting compression. */
// 尝试压缩的紧凑列表的最小尺寸
#define MIN_COMPRESS_BYTES 48

/* Minimum size reduction in bytes to store compressed quicklistNode data.
 * This also prevents us from storing compression if the compression
 * resulted in a larger size than the original data. */
// 保存压缩的quicklistNode数据减少的最小尺寸。
// 如果压缩后的尺寸比原数据的尺寸还大将会阻止我们保存压缩的数据
#define MIN_COMPRESS_IMPROVE 8

/* If not verbose testing, remove all debug printing. */
#ifndef REDIS_TEST_VERBOSE
#define D(...)
#else
#define D(...)                                                                 \
    do {                                                                       \
        printf("%s:%s:%d:\t", __FILE__, __func__, __LINE__);                   \
        printf(__VA_ARGS__);                                                   \
        printf("\n");                                                          \
    } while (0)
#endif

/* Bookmarks forward declarations */
// 书签的前置声明
#define QL_MAX_BM ((1 << QL_BM_BITS)-1)
quicklistBookmark *_quicklistBookmarkFindByName(quicklist *ql, const char *name);
quicklistBookmark *_quicklistBookmarkFindByNode(quicklist *ql, quicklistNode *node);
void _quicklistBookmarkDelete(quicklist *ql, quicklistBookmark *bm);

/* Simple way to give quicklistEntry structs default values with one call. */
// 一个简单的方式给quicklistEntry结构体赋初值
#define initEntry(e)                                                           \
    do {                                                                       \
        (e)->zi = (e)->value = NULL;                                           \
        (e)->longval = -123456789;                                             \
        (e)->quicklist = NULL;                                                 \
        (e)->node = NULL;                                                      \
        (e)->offset = 123456789;                                               \
        (e)->sz = 0;                                                           \
    } while (0)

/* Reset the quicklistIter to prevent it from being used again after
 * insert, replace, or other against quicklist operation. */
// 在插入，替换或者其他相冲突的快速列表操作之后，为了防止重复使用，重置快速列表的迭代器。
#define resetIterator(iter)                                                    \
    do {                                                                       \
        (iter)->current = NULL;                                                \
        (iter)->zi = NULL;                                                     \
    } while (0)

/* Create a new quicklist.
 * Free with quicklistRelease(). */
// 创建一个新的quicklist。使用quicklistRelease释放
quicklist *quicklistCreate(void) {
    struct quicklist *quicklist;

    quicklist = zmalloc(sizeof(*quicklist));
    quicklist->head = quicklist->tail = NULL;
    quicklist->len = 0;
    quicklist->count = 0;
    quicklist->compress = 0;
    quicklist->fill = -2;
    quicklist->bookmark_count = 0;
    return quicklist;
}

#define COMPRESS_MAX ((1 << QL_COMP_BITS)-1)
// 设置快速列表的压缩深度（两端多少个节点不压缩）
void quicklistSetCompressDepth(quicklist *quicklist, int compress) {
    // 不能超过最大的压缩深度
    if (compress > COMPRESS_MAX) {
        compress = COMPRESS_MAX;
    } else if (compress < 0) {
        compress = 0;
    }
    quicklist->compress = compress;
}

#define FILL_MAX ((1 << (QL_FILL_BITS-1))-1)
// 设置每个节点最大的数据项的数目
void quicklistSetFill(quicklist *quicklist, int fill) {
    if (fill > FILL_MAX) {
        fill = FILL_MAX;
    } 
    // 最小不能小于-5，小于0的值是特殊含义
    else if (fill < -5) {
        fill = -5;
    }
    quicklist->fill = fill;
}

// 设置快速列表的选项，设置节点的填充因子和压缩深度
void quicklistSetOptions(quicklist *quicklist, int fill, int depth) {
    quicklistSetFill(quicklist, fill);
    quicklistSetCompressDepth(quicklist, depth);
}

/* Create a new quicklist with some default parameters. */
// 创建一个新的快速列表，设置一些默认的参数
quicklist *quicklistNew(int fill, int compress) {
    quicklist *quicklist = quicklistCreate();
    quicklistSetOptions(quicklist, fill, compress);
    return quicklist;
}

// 创建快速列表的节点，并赋初值
REDIS_STATIC quicklistNode *quicklistCreateNode(void) {
    quicklistNode *node;
    node = zmalloc(sizeof(*node));
    node->entry = NULL;
    node->count = 0;
    node->sz = 0;
    node->next = node->prev = NULL;
    node->encoding = QUICKLIST_NODE_ENCODING_RAW;
    node->container = QUICKLIST_NODE_CONTAINER_PACKED;
    node->recompress = 0;
    node->dont_compress = 0;
    return node;
}

/* Return cached quicklist count */
// 返回快速列表的数据项总数
unsigned long quicklistCount(const quicklist *ql) { return ql->count; }

/* Free entire quicklist. */
// 释放整个快速列表
void quicklistRelease(quicklist *quicklist) {
    unsigned long len;
    quicklistNode *current, *next;

    current = quicklist->head;
    len = quicklist->len;
    // 遍历所有的节点
    while (len--) {
        next = current->next;
        // 释放当前节点的数据条目
        zfree(current->entry);
        quicklist->count -= current->count;
        // 释放当前节点本身
        zfree(current);
        // 减少节点数目
        quicklist->len--;
        // 进入下一个节点
        current = next;
    }
    // 释放快速列表的书签
    quicklistBookmarksClear(quicklist);
    // 释放快速列表本身 
    zfree(quicklist);
}

/* Compress the listpack in 'node' and update encoding details.
 * Returns 1 if listpack compressed successfully.
 * Returns 0 if compression failed or if listpack too small to compress. */
// 压缩节点中的紧凑表并且更新编码细节。
// 如果紧凑表压缩成功返回1，如果压缩失败或者紧凑表太小而不压缩返回0
REDIS_STATIC int __quicklistCompressNode(quicklistNode *node) {
#ifdef REDIS_TEST
    node->attempted_compress = 1;
#endif
    // 如果节点设置位不能压缩，直接返回0
    if (node->dont_compress) return 0;

    /* validate that the node is neither
     * tail nor head (it has prev and next)*/
    // 确定当前节点不是头节点或者尾节点
    assert(node->prev && node->next);

    // 将以前压缩过临时解压的标志去掉
    node->recompress = 0;
    /* Don't bother compressing small values */
    // 如果节点的大小比最小的压缩阈值还小，就放弃
    if (node->sz < MIN_COMPRESS_BYTES)
        return 0;

    // 为lzf压缩分配和原来一样的空间
    quicklistLZF *lzf = zmalloc(sizeof(*lzf) + node->sz);

    /* Cancel if compression fails or doesn't compress small enough */
    // 如果压缩失败或者压缩后的尺寸不够小（不够MIN_COMPRESS_IMPROVE字节），就取消
    if (((lzf->sz = lzf_compress(node->entry, node->sz, lzf->compressed,
                                 node->sz)) == 0) ||
        lzf->sz + MIN_COMPRESS_IMPROVE >= node->sz) {
        /* lzf_compress aborts/rejects compression if value not compressible. */
        // 释放为压缩分配的空间，然后返回0
        zfree(lzf);
        return 0;
    }
    // 调整到实际大小
    lzf = zrealloc(lzf, sizeof(*lzf) + lzf->sz);
    // 删除原来的紧凑表
    zfree(node->entry);
    // 设置为lzf数据和编码
    node->entry = (unsigned char *)lzf;
    node->encoding = QUICKLIST_NODE_ENCODING_LZF;
    return 1;
}

/* Compress only uncompressed nodes. */
// 只压缩目前还没有压缩的节点
#define quicklistCompressNode(_node)                                           \
    do {                                                                       \
        if ((_node) && (_node)->encoding == QUICKLIST_NODE_ENCODING_RAW) {     \
            __quicklistCompressNode((_node));                                  \
        }                                                                      \
    } while (0)

/* Uncompress the listpack in 'node' and update encoding details.
 * Returns 1 on successful decode, 0 on failure to decode. */
// 解压“节点”中的紧凑表并更新编码细节。 解码成功返回1，解码失败返回0
REDIS_STATIC int __quicklistDecompressNode(quicklistNode *node) {
#ifdef REDIS_TEST
    node->attempted_compress = 0;
#endif
    // 将以前压缩过临时解压的标志去掉
    node->recompress = 0;
    // 分配解压后的内存
    void *decompressed = zmalloc(node->sz);
    quicklistLZF *lzf = (quicklistLZF *)node->entry;
    // 解压
    if (lzf_decompress(lzf->compressed, lzf->sz, decompressed, node->sz) == 0) {
        /* Someone requested decompress, but we can't decompress.  Not good. */
        // 解压失败返回0
        zfree(decompressed);
        return 0;
    }
    // 将压缩后的内存释放
    zfree(lzf);
    // 更新为解压的紧凑表方式和编码类型
    node->entry = decompressed;
    node->encoding = QUICKLIST_NODE_ENCODING_RAW;
    return 1;
}

/* Decompress only compressed nodes. */
// 只解压已经压缩了的节点
#define quicklistDecompressNode(_node)                                         \
    do {                                                                       \
        if ((_node) && (_node)->encoding == QUICKLIST_NODE_ENCODING_LZF) {     \
            __quicklistDecompressNode((_node));                                \
        }                                                                      \
    } while (0)

/* Force node to not be immediately re-compressible */
// 强制节点不能立即重新压缩
#define quicklistDecompressNodeForUse(_node)                                   \
    do {                                                                       \
        if ((_node) && (_node)->encoding == QUICKLIST_NODE_ENCODING_LZF) {     \
            __quicklistDecompressNode((_node));                                \
            (_node)->recompress = 1;                                           \
        }                                                                      \
    } while (0)

/* Extract the raw LZF data from this quicklistNode.
 * Pointer to LZF data is assigned to '*data'.
 * Return value is the length of compressed LZF data. */
// 从quicklistNode中提取原始的LZF数据。*data指向LZF的数据，返回值是LZF的数据长度
size_t quicklistGetLzf(const quicklistNode *node, void **data) {
    quicklistLZF *lzf = (quicklistLZF *)node->entry;
    *data = lzf->compressed;
    return lzf->sz;
}

// 快速列表是否允许压缩（就是压缩深度不为0）
#define quicklistAllowsCompression(_ql) ((_ql)->compress != 0)

/* Force 'quicklist' to meet compression guidelines set by compress depth.
 * The only way to guarantee interior nodes get compressed is to iterate
 * to our "interior" compress depth then compress the next node we find.
 * If compress depth is larger than the entire list, we return immediately. */
// 强制“快速列表”以满足压缩深度设置的压缩准则。保证内部节点被压缩的唯一方法是迭代到
// 我们的“内部”压缩深度，然后压缩我们找到的下一个节点。如果压缩深度大于整个列表，我们会立即返回。
REDIS_STATIC void __quicklistCompress(const quicklist *quicklist,
                                      quicklistNode *node) {
    if (quicklist->len == 0) return;

    /* The head and tail should never be compressed (we should not attempt to recompress them) */
    // 头、尾节点从不应该被压缩
    assert(quicklist->head->recompress == 0 && quicklist->tail->recompress == 0);

    /* If length is less than our compress depth (from both sides),
     * we can't compress anything. */
    // 如果压缩深度比整个长度还大，压根就不需要压缩
    if (!quicklistAllowsCompression(quicklist) ||
        quicklist->len < (unsigned int)(quicklist->compress * 2))
        return;

#if 0
    /* Optimized cases for small depth counts */
    if (quicklist->compress == 1) {
        quicklistNode *h = quicklist->head, *t = quicklist->tail;
        quicklistDecompressNode(h);
        quicklistDecompressNode(t);
        if (h != node && t != node)
            quicklistCompressNode(node);
        return;
    } else if (quicklist->compress == 2) {
        quicklistNode *h = quicklist->head, *hn = h->next, *hnn = hn->next;
        quicklistNode *t = quicklist->tail, *tp = t->prev, *tpp = tp->prev;
        quicklistDecompressNode(h);
        quicklistDecompressNode(hn);
        quicklistDecompressNode(t);
        quicklistDecompressNode(tp);
        if (h != node && hn != node && t != node && tp != node) {
            quicklistCompressNode(node);
        }
        if (hnn != t) {
            quicklistCompressNode(hnn);
        }
        if (tpp != h) {
            quicklistCompressNode(tpp);
        }
        return;
    }
#endif

    /* Iterate until we reach compress depth for both sides of the list.a
     * Note: because we do length checks at the *top* of this function,
     *       we can skip explicit null checks below. Everything exists. */
    // 循环遍历直到列表两边的压缩深度
    // 我们在该游戏的开头部分已经做了长度的校验，所以后面可以跳过明显的判空检查，一切都存在。
    quicklistNode *forward = quicklist->head;
    quicklistNode *reverse = quicklist->tail;
    int depth = 0;
    int in_depth = 0;
    // 遍历深度
    while (depth++ < quicklist->compress) {
        // 在深度范围内，解压节点
        quicklistDecompressNode(forward);
        quicklistDecompressNode(reverse);

        if (forward == node || reverse == node)
            in_depth = 1;

        /* We passed into compress depth of opposite side of the quicklist
         * so there's no need to compress anything and we can exit. */
        // 从两边往中间走，已经走到了中间了
        if (forward == reverse || forward->next == reverse)
            return;

        forward = forward->next;
        reverse = reverse->prev;
    }

    // 传入的节点不在压缩深度范围内，压缩节点
    if (!in_depth)
        quicklistCompressNode(node);

    /* At this point, forward and reverse are one node beyond depth */
    // 这时候，forward和reverse这两个节点都是深度之外了，压缩他们
    quicklistCompressNode(forward);
    quicklistCompressNode(reverse);
}

// 快速列表压缩节点
#define quicklistCompress(_ql, _node)                                          \
    do {                                                                       \
        if ((_node)->recompress)                                               \
            quicklistCompressNode((_node));                                    \
        else                                                                   \
            __quicklistCompress((_ql), (_node));                               \
    } while (0)

/* If we previously used quicklistDecompressNodeForUse(), just recompress. */
// 如果我们以前使用过quicklistDecompressNodeForUse（），只需重新压缩
#define quicklistRecompressOnly(_node)                                         \
    do {                                                                       \
        if ((_node)->recompress)                                               \
            quicklistCompressNode((_node));                                    \
    } while (0)

/* Insert 'new_node' after 'old_node' if 'after' is 1.
 * Insert 'new_node' before 'old_node' if 'after' is 0.
 * Note: 'new_node' is *always* uncompressed, so if we assign it to
 *       head or tail, we do not need to uncompress it. */
// 如果after为1，表示在old_node的节点后面插入new_node
// 如果after为0，表示在old_node的节点前面插入new_node
// 注意：new_node总是非压缩的，如果放在头或者尾，不需要解压缩
REDIS_STATIC void __quicklistInsertNode(quicklist *quicklist,
                                        quicklistNode *old_node,
                                        quicklistNode *new_node, int after) {
    // 后面插入，
    if (after) {
        // 插入双向链表
        new_node->prev = old_node;
        if (old_node) {
            new_node->next = old_node->next;
            if (old_node->next)
                old_node->next->prev = new_node;
            old_node->next = new_node;
        }
        // 如果老节点是尾结点，将新节点设置为尾节点
        if (quicklist->tail == old_node)
            quicklist->tail = new_node;
    } else {
        // 插入双向链表
        new_node->next = old_node;
        if (old_node) {
            new_node->prev = old_node->prev;
            if (old_node->prev)
                old_node->prev->next = new_node;
            old_node->prev = new_node;
        }
        // 如果老节点是头节点，将新节点设置为头节点
        if (quicklist->head == old_node)
            quicklist->head = new_node;
    }
    /* If this insert creates the only element so far, initialize head/tail. */
    // 如果插入的节点是目前唯一的元素，那就是首尾节点都指向它
    if (quicklist->len == 0) {
        quicklist->head = quicklist->tail = new_node;
    }

    /* Update len first, so in __quicklistCompress we know exactly len */
    // 增加节点数目
    quicklist->len++;

    // 对新老节点根据压缩深度看情况进行压缩
    if (old_node)
        quicklistCompress(quicklist, old_node);

    quicklistCompress(quicklist, new_node);
}

/* Wrappers for node inserting around existing node. */
// 在old_node节点前插入new_node
REDIS_STATIC void _quicklistInsertNodeBefore(quicklist *quicklist,
                                             quicklistNode *old_node,
                                             quicklistNode *new_node) {
    __quicklistInsertNode(quicklist, old_node, new_node, 0);
}

// 在old_node节点后插入new_node
REDIS_STATIC void _quicklistInsertNodeAfter(quicklist *quicklist,
                                            quicklistNode *old_node,
                                            quicklistNode *new_node) {
    __quicklistInsertNode(quicklist, old_node, new_node, 1);
}

// 判断是否满足安全限制
#define sizeMeetsSafetyLimit(sz) ((sz) <= SIZE_SAFETY_LIMIT)

/* Calculate the size limit or length limit of the quicklist node
 * based on 'fill', and is also used to limit list listpack. */
// 根据fill的值计算quicklist节点的尺寸限制和长度限制，也用于限制listpack
void quicklistNodeLimit(int fill, size_t *size, unsigned int *count) {
    *size = SIZE_MAX;
    *count = UINT_MAX;

    if (fill >= 0) {
        /* Ensure that one node have at least one entry */
        // 保证一个条目至少有一个节点
        *count = (fill == 0) ? 1 : fill;
    } else {
        size_t offset = (-fill) - 1;
        // 根据fill得到对应的尺寸限制
        size_t max_level = sizeof(optimization_level) / sizeof(*optimization_level);
        if (offset >= max_level) offset = max_level - 1;
        *size = optimization_level[offset];
    }
}

/* Check if the limit of the quicklist node has been reached to determine if
 * insertions, merges or other operations that would increase the size of
 * the node can be performed.
 * Return 1 if exceeds the limit, otherwise 0. */
// 在插入、合并或者其他操作会引起节点的尺寸增加时，检查是否达到了quicklist的限制。
// 如果超出了现在就返回1，否则返回0
int quicklistNodeExceedsLimit(int fill, size_t new_sz, unsigned int new_count) {
    size_t sz_limit;
    unsigned int count_limit;
    // 得到节点的限制
    quicklistNodeLimit(fill, &sz_limit, &count_limit);
    // 如果尺寸不是非法值，那就比较尺寸是否超限
    if (likely(sz_limit != SIZE_MAX)) {
        return new_sz > sz_limit;
    } 
    // 如果节点数不是非法制，毕竟节点数是否超限
    else if (count_limit != UINT_MAX) {
        /* when we reach here we know that the limit is a size limit (which is
         * safe, see comments next to optimization_level and SIZE_SAFETY_LIMIT) */
        // 尺寸不能超过安全尺寸
        if (!sizeMeetsSafetyLimit(new_sz)) return 1;
        return new_count > count_limit;
    }

    redis_unreachable();
}

// 是否可以插入节点node
// 不可以返回0，可以返回1
REDIS_STATIC int _quicklistNodeAllowInsert(const quicklistNode *node,
                                           const int fill, const size_t sz) {
    if (unlikely(!node))
        return 0;
    // 如果node为普通的或者节点本身过大，不能插入
    if (unlikely(QL_NODE_IS_PLAIN(node) || isLargeElement(sz)))
        return 0;

    /* Estimate how many bytes will be added to the listpack by this one entry.
     * We prefer an overestimation, which would at worse lead to a few bytes
     * below the lowest limit of 4k (see optimization_level).
     * Note: No need to check for overflow below since both `node->sz` and
     * `sz` are to be less than 1GB after the plain/large element check above. */
     // 估计此条目将向listpack中添加多少字节,我们更喜欢过高的估计，这会导致最坏的情况下小于最低限制4k减去
     // 几个字节（参阅optimization_level）。注意：不需要检查下面的溢出，因为在上面的普通和大型元素检查
     // 之后，“node->sz”和“sz”都将小于1GB
     // 新的节点加入后的尺寸
    size_t new_sz = node->sz + sz + SIZE_ESTIMATE_OVERHEAD;
    if (unlikely(quicklistNodeExceedsLimit(fill, new_sz, node->count + 1)))
        return 0;
    return 1;
}

// 快速表的两个节点是否允许合并
REDIS_STATIC int _quicklistNodeAllowMerge(const quicklistNode *a,
                                          const quicklistNode *b,
                                          const int fill) {
    if (!a || !b)
        return 0;

    // a和b都是普通的，不能合并
    if (unlikely(QL_NODE_IS_PLAIN(a) || QL_NODE_IS_PLAIN(b)))
        return 0;

    /* approximate merged listpack size (- 7 to remove one listpack
     * header/trailer, see LP_HDR_SIZE and LP_EOF) */
     // 计算合并后的尺寸
    unsigned int merge_sz = a->sz + b->sz - 7;
    if (unlikely(quicklistNodeExceedsLimit(fill, merge_sz, a->count + b->count)))
        return 0;
    return 1;
}

// 更新快速列表节点尺寸
#define quicklistNodeUpdateSz(node)                                            \
    do {                                                                       \
        (node)->sz = lpBytes((node)->entry);                                   \
    } while (0)

// 创建一个普通的快速列表节点，然后将传入的值初始化
static quicklistNode* __quicklistCreatePlainNode(void *value, size_t sz) {
    quicklistNode *new_node = quicklistCreateNode();
    new_node->entry = zmalloc(sz);
    new_node->container = QUICKLIST_NODE_CONTAINER_PLAIN;
    // 直接分配内存将数据拷贝进去
    memcpy(new_node->entry, value, sz);
    new_node->sz = sz;
    new_node->count++;
    return new_node;
}

// 快速列表插入普通节点
static void __quicklistInsertPlainNode(quicklist *quicklist, quicklistNode *old_node,
                                       void *value, size_t sz, int after) {
    // 先创建一个普通节点，然后初始化，在插入到快速列表中
    __quicklistInsertNode(quicklist, old_node, __quicklistCreatePlainNode(value, sz), after);
    quicklist->count++;
}

/* Add new entry to head node of quicklist.
 *
 * Returns 0 if used existing head.
 * Returns 1 if new head created. */
// 给快速表的头节点增加一个条目
// 如果使用已经存在的节点，返回0，
// 如果创建了一个新的头结点，返回1，
int quicklistPushHead(quicklist *quicklist, void *value, size_t sz) {
    quicklistNode *orig_head = quicklist->head;

    // 如果数据尺寸符合一个大元素的尺寸
    if (unlikely(isLargeElement(sz))) {
        // 创建一个普通节点，然后插在最前面
        __quicklistInsertPlainNode(quicklist, quicklist->head, value, sz, 0);
        return 1;
    }

    // 如果头节点允许插入
    if (likely(
            _quicklistNodeAllowInsert(quicklist->head, quicklist->fill, sz))) {
        // 加在listpack的前面
        quicklist->head->entry = lpPrepend(quicklist->head->entry, value, sz);
        // 更新尺寸
        quicklistNodeUpdateSz(quicklist->head);
    } 
    // 头结点不允许插入
    else {
        // 创建一个新的节点
        quicklistNode *node = quicklistCreateNode();
        // 附加到listpack前面
        node->entry = lpPrepend(lpNew(0), value, sz);
        // 更新尺寸
        quicklistNodeUpdateSz(node);
        // 插入到快速列表的最前面
        _quicklistInsertNodeBefore(quicklist, quicklist->head, node);
    }
    // 快速列表和调节点的元素数目都增加
    quicklist->count++;
    quicklist->head->count++;
    return (orig_head != quicklist->head);
}

/* Add new entry to tail node of quicklist.
 *
 * Returns 0 if used existing tail.
 * Returns 1 if new tail created. */
// 在快速列表的尾节点加入一个新的条目
int quicklistPushTail(quicklist *quicklist, void *value, size_t sz) {
    quicklistNode *orig_tail = quicklist->tail;
    // 如果数据尺寸符合一个大元素的尺寸
    if (unlikely(isLargeElement(sz))) {
        // 创建一个普通节点，然后插在最后面
        __quicklistInsertPlainNode(quicklist, quicklist->tail, value, sz, 1);
        return 1;
    }
    // 如果尾结点允许插入
    if (likely(
            _quicklistNodeAllowInsert(quicklist->tail, quicklist->fill, sz))) {
        // 附加在节点后面
        quicklist->tail->entry = lpAppend(quicklist->tail->entry, value, sz);
        quicklistNodeUpdateSz(quicklist->tail);
    } else {
        // 创建一个新的快速列表节点
        quicklistNode *node = quicklistCreateNode();
        // 附加在lispack后面
        node->entry = lpAppend(lpNew(0), value, sz);

        quicklistNodeUpdateSz(node);
        // 插入在尾结点后面
        _quicklistInsertNodeAfter(quicklist, quicklist->tail, node);
    }
    // 快速列表元素数目加1,尾结点元素加1
    quicklist->count++;
    quicklist->tail->count++;
    return (orig_tail != quicklist->tail);
}

/* Create new node consisting of a pre-formed listpack.
 * Used for loading RDBs where entire listpacks have been stored
 * to be retrieved later. */
// 创建由预先形成的listpack组成的新节点。用于加载存储了整个listpacks以便以后检索的RDBs。
void quicklistAppendListpack(quicklist *quicklist, unsigned char *zl) {
    // 创建一个节点
    quicklistNode *node = quicklistCreateNode();

    // 初始化节点
    node->entry = zl;
    node->count = lpLength(node->entry);
    node->sz = lpBytes(zl);
    // 插入尾结点后面
    _quicklistInsertNodeAfter(quicklist, quicklist->tail, node);
    quicklist->count += node->count;
}

/* Create new node consisting of a pre-formed plain node.
 * Used for loading RDBs where entire plain node has been stored
 * to be retrieved later.
 * data - the data to add (pointer becomes the responsibility of quicklist) */
// 创建由预先形成的普通节点组成的新节点。用于加载RDB，其中存储了整个普通节点以便以后检索。
// data-要添加的数据（指针成为quicklist的责任）*/
void quicklistAppendPlainNode(quicklist *quicklist, unsigned char *data, size_t sz) {
    // 创建节点
    quicklistNode *node = quicklistCreateNode();
    // 初始化
    node->entry = data;
    node->count = 1;
    node->sz = sz;
    node->container = QUICKLIST_NODE_CONTAINER_PLAIN;
    // 插入尾结点的后面
    _quicklistInsertNodeAfter(quicklist, quicklist->tail, node);
    // 快速表元素数目增加
    quicklist->count += node->count;
}

// 如果节点的元素数目为0，删除节点
#define quicklistDeleteIfEmpty(ql, n)                                          \
    do {                                                                       \
        if ((n)->count == 0) {                                                 \
            __quicklistDelNode((ql), (n));                                     \
            (n) = NULL;                                                        \
        }                                                                      \
    } while (0)

// 快速列表删除节点
REDIS_STATIC void __quicklistDelNode(quicklist *quicklist,
                                     quicklistNode *node) {
    /* Update the bookmark if any */
    // 如果有书签，更新书签
    quicklistBookmark *bm = _quicklistBookmarkFindByNode(quicklist, node);
    if (bm) {
        bm->node = node->next;
        /* if the bookmark was to the last node, delete it. */
        // 如果书签指向最后一个节点，请将其删除。
        if (!bm->node)
            _quicklistBookmarkDelete(quicklist, bm);
    }

    // 从双向链表中删除
    if (node->next)
        node->next->prev = node->prev;
    if (node->prev)
        node->prev->next = node->next;

    // 如果是尾结点，更新尾结点
    if (node == quicklist->tail) {
        quicklist->tail = node->prev;
    }

    // 如果是头节点，更新头结点
    if (node == quicklist->head) {
        quicklist->head = node->next;
    }

    /* Update len first, so in __quicklistCompress we know exactly len */
    // 更新节点数量和元素数量
    quicklist->len--;
    quicklist->count -= node->count;

    /* If we deleted a node within our compress depth, we
     * now have compressed nodes needing to be decompressed. */
    //如果删除的节点是我们压缩深度里的，那么我们就有需要解压缩的压缩节点
    __quicklistCompress(quicklist, NULL);

    // 删除数据和节点本身
    zfree(node->entry);
    zfree(node);
}

/* Delete one entry from list given the node for the entry and a pointer
 * to the entry in the node.
 *
 * Note: quicklistDelIndex() *requires* uncompressed nodes because you
 *       already had to get *p from an uncompressed node somewhere.
 *
 * Returns 1 if the entire node was deleted, 0 if node still exists.
 * Also updates in/out param 'p' with the next offset in the listpack. */
 // 在给定条目的节点和指向节点内条目指针的情况下，删除一个条目。
 // 注意：quicklistDelIndex要求没有压缩的节点，因为你必须已经从未压缩的节点某处得到p
 // 如果整个节点都删除就返回1，如果节点还存在就返回0.也使用listpack的下一个偏移来更新输入输出参数p
REDIS_STATIC int quicklistDelIndex(quicklist *quicklist, quicklistNode *node,
                                   unsigned char **p) {
    int gone = 0;
    // 如果是一个普通节点，直接删除
    if (unlikely(QL_NODE_IS_PLAIN(node))) {
        __quicklistDelNode(quicklist, node);
        return 1;
    }
    // 删除*p指向的节点
    node->entry = lpDelete(node->entry, *p, p);
    node->count--;
    // 节点里的元素数目为0，直接将节点删除
    if (node->count == 0) {
        gone = 1;
        __quicklistDelNode(quicklist, node);
    } else {
        // 更新尺寸
        quicklistNodeUpdateSz(node);
    }
    // 减少数量
    quicklist->count--;
    /* If we deleted the node, the original node is no longer valid */
    // 如果删除了节点，原始节点就不再有效
    return gone ? 1 : 0;
}

/* Delete one element represented by 'entry'
 *
 * 'entry' stores enough metadata to delete the proper position in
 * the correct listpack in the correct quicklist node. */
// 删除一个由“entry”表示的元素，“entry”存储了足够的元数据
// 可以在正确的quicklist中和正确的listpack中的正确位置去删除条目
void quicklistDelEntry(quicklistIter *iter, quicklistEntry *entry) {
    quicklistNode *prev = entry->node->prev;
    quicklistNode *next = entry->node->next;
    // 删除对应的条目
    int deleted_node = quicklistDelIndex((quicklist *)entry->quicklist,
                                         entry->node, &entry->zi);

    /* after delete, the zi is now invalid for any future usage. */
    // 删除了以后，迭代器中zi就不再有效
    iter->zi = NULL;

    /* If current node is deleted, we must update iterator node and offset. */
    // 如果当前节点删除了，必须更新迭代中的节点和偏移
    if (deleted_node) {
        // 迭代器的方向是从头往后
        if (iter->direction == AL_START_HEAD) {
            iter->current = next;
            iter->offset = 0;
        } 
        // 迭代器的方向是从后往头
        else if (iter->direction == AL_START_TAIL) {
            iter->current = prev;
            iter->offset = -1;
        }
    }
    /* else if (!deleted_node), no changes needed.
     * we already reset iter->zi above, and the existing iter->offset
     * doesn't move again because:
     *   - [1, 2, 3] => delete offset 1 => [1, 3]: next element still offset 1
     *   - [1, 2, 3] => delete offset 0 => [2, 3]: next element still offset 0
     *  if we deleted the last element at offset N and now
     *  length of this listpack is N-1, the next call into
     *  quicklistNext() will jump to the next node. */
}

/* Replace quicklist entry by 'data' with length 'sz'. */
// 用sz长度的数据data替代quicklist的条目
void quicklistReplaceEntry(quicklistIter *iter, quicklistEntry *entry,
                           void *data, size_t sz)
{
    quicklist* quicklist = iter->quicklist;
    // 如果不是普通节点也不是大节点元素
    if (likely(!QL_NODE_IS_PLAIN(entry->node) && !isLargeElement(sz))) {
        // listpack进行条目替代
        entry->node->entry = lpReplace(entry->node->entry, &entry->zi, data, sz);
        // 更新大小
        quicklistNodeUpdateSz(entry->node);
        /* quicklistNext() and quicklistGetIteratorEntryAtIdx() provide an uncompressed node */
        // 压缩节点
        quicklistCompress(quicklist, entry->node);
    } 
    // 普通节点
    else if (QL_NODE_IS_PLAIN(entry->node)) {
        // 大节点尺寸，删了重新分配
        if (isLargeElement(sz)) {
            zfree(entry->node->entry);
            entry->node->entry = zmalloc(sz);
            entry->node->sz = sz;
            memcpy(entry->node->entry, data, sz);
            quicklistCompress(quicklist, entry->node);
        } 
        // 普通节点替换就是插入新的，但是不能放在当前节点，所以插入后把当前节点删除
        else {
            // 直接在条目后面插入
            quicklistInsertAfter(iter, entry, data, sz);
            // 然后把当前节点删除
            __quicklistDelNode(quicklist, entry->node);
        }
    } else {
        // 设置不压缩
        entry->node->dont_compress = 1; /* Prevent compression in quicklistInsertAfter() */
        // 插入在后面
        quicklistInsertAfter(iter, entry, data, sz);
        // 插入在后面，但是元素的数目还是1，那就将原来的删除
        if (entry->node->count == 1) {
            __quicklistDelNode(quicklist, entry->node);
        } else {
            unsigned char *p = lpSeek(entry->node->entry, -1);
            // 将条目删除
            quicklistDelIndex(quicklist, entry->node, &p);
            // 开启压缩
            entry->node->dont_compress = 0; /* Re-enable compression */
            quicklistCompress(quicklist, entry->node);
            quicklistCompress(quicklist, entry->node->next);
        }
    }

    /* In any case, we reset iterator to forbid use of iterator after insert.
     * Notice: iter->current has been compressed above. */
    // 重置迭代器
    resetIterator(iter);
}

/* Replace quicklist entry at offset 'index' by 'data' with length 'sz'.
 *
 * Returns 1 if replace happened.
 * Returns 0 if replace failed and no changes happened. */
// 使用长度为sz的数据data来替代quicklist中偏移为index的条目
// 返回1表示发生了替换
// 返回0表示替换失败或者没有变化产生。
int quicklistReplaceAtIndex(quicklist *quicklist, long index, void *data,
                            size_t sz) {
    quicklistEntry entry;
    // 得到偏移为index的条目
    quicklistIter *iter = quicklistGetIteratorEntryAtIdx(quicklist, index, &entry);
    // 如果找到
    if (likely(iter)) {
        // 替换条目的数据
        quicklistReplaceEntry(iter, &entry, data, sz);
        // 释放迭代器
        quicklistReleaseIterator(iter);
        return 1;
    } else {
        return 0;
    }
}

/* Given two nodes, try to merge their listpacks.
 *
 * This helps us not have a quicklist with 3 element listpacks if
 * our fill factor can handle much higher levels.
 *
 * Note: 'a' must be to the LEFT of 'b'.
 *
 * After calling this function, both 'a' and 'b' should be considered
 * unusable.  The return value from this function must be used
 * instead of re-using any of the quicklistNode input arguments.
 *
 * Returns the input node picked to merge against or NULL if
 * merging was not possible. */
// 给定两个节点，尝试合并他们的listpacks
// 如果我们的填充系数可以处理更高的层级，这可以帮助我们避免quicklist中只有3个元素的listpacks，
REDIS_STATIC quicklistNode *_quicklistListpackMerge(quicklist *quicklist,
                                                    quicklistNode *a,
                                                    quicklistNode *b) {
    D("Requested merge (a,b) (%u, %u)", a->count, b->count);
    // 解压节点a和b
    quicklistDecompressNode(a);
    quicklistDecompressNode(b);
    // listpack合并
    if ((lpMerge(&a->entry, &b->entry))) {
        /* We merged listpacks! Now remove the unused quicklistNode. */
        // 因为可以合并在两个节点的任意一个节点，需要将另外一个移除
        quicklistNode *keep = NULL, *nokeep = NULL;
        if (!a->entry) {
            nokeep = a;
            keep = b;
        } else if (!b->entry) {
            nokeep = b;
            keep = a;
        }
        // 重新获取元素的数量
        keep->count = lpLength(keep->entry);
        // 更新节点的尺寸
        quicklistNodeUpdateSz(keep);
        // 删除没用的节点
        nokeep->count = 0;
        __quicklistDelNode(quicklist, nokeep);
        // 压缩
        quicklistCompress(quicklist, keep);
        return keep;
    } else {
        /* else, the merge returned NULL and nothing changed. */
        return NULL;
    }
}

/* Attempt to merge listpacks within two nodes on either side of 'center'.
 *
 * We attempt to merge:
 *   - (center->prev->prev, center->prev)
 *   - (center->next, center->next->next)
 *   - (center->prev, center)
 *   - (center, center->next)
 */
// 尝试将center两侧的两个节点内的listpacks合并
REDIS_STATIC void _quicklistMergeNodes(quicklist *quicklist,
                                       quicklistNode *center) {
    int fill = quicklist->fill;
    quicklistNode *prev, *prev_prev, *next, *next_next, *target;
    prev = prev_prev = next = next_next = target = NULL;
    // 前一个节点存在
    if (center->prev) {
        prev = center->prev;
        // 前前一个节点存在
        if (center->prev->prev)
            prev_prev = center->prev->prev;
    }
    // 后一个节点存在
    if (center->next) {
        next = center->next;
        // 后后一个节点存在
        if (center->next->next)
            next_next = center->next->next;
    }

    /* Try to merge prev_prev and prev */
    // 尝试合并前一个节点和前前一个节点
    if (_quicklistNodeAllowMerge(prev, prev_prev, fill)) {
        _quicklistListpackMerge(quicklist, prev_prev, prev);
        prev_prev = prev = NULL; /* they could have moved, invalidate them. */
    }

    /* Try to merge next and next_next */
    // 尝试合并后一个节点和后后一个节点
    if (_quicklistNodeAllowMerge(next, next_next, fill)) {
        _quicklistListpackMerge(quicklist, next, next_next);
        next = next_next = NULL; /* they could have moved, invalidate them. */
    }

    /* Try to merge center node and previous node */
    // 尝试合并center节点和前一个节点
    if (_quicklistNodeAllowMerge(center, center->prev, fill)) {
        target = _quicklistListpackMerge(quicklist, center->prev, center);
        center = NULL; /* center could have been deleted, invalidate it. */
    } else {
        /* else, we didn't merge here, but target needs to be valid below. */
        target = center;
    }

    /* Use result of center merge (or original) to merge with next node. */
    // 使用center节点合并的结果，和下一个节点合并
    if (_quicklistNodeAllowMerge(target, target->next, fill)) {
        _quicklistListpackMerge(quicklist, target, target->next);
    }
}

/* Split 'node' into two parts, parameterized by 'offset' and 'after'.
 *
 * The 'after' argument controls which quicklistNode gets returned.
 * If 'after'==1, returned node has elements after 'offset'.
 *                input node keeps elements up to 'offset', including 'offset'.
 * If 'after'==0, returned node has elements up to 'offset'.
 *                input node keeps elements after 'offset', including 'offset'.
 *
 * Or in other words:
 * If 'after'==1, returned node will have elements after 'offset'.
 *                The returned node will have elements [OFFSET+1, END].
 *                The input node keeps elements [0, OFFSET].
 * If 'after'==0, returned node will keep elements up to but not including 'offset'.
 *                The returned node will have elements [0, OFFSET-1].
 *                The input node keeps elements [OFFSET, END].
 *
 * The input node keeps all elements not taken by the returned node.
 *
 * Returns newly created node or NULL if split not possible. */
// 通过参数offset和after将node拆分成两部分
// after参数控制那个quicklistNode返回
// 如果after == 1，返回offset后面元素的节点，也就是[OFFSET+1, END]，
//                 输入node保持着从开始一直到offset的元素，包含offset,也就是[0, OFFSET].
// 如果after == 0, 返回从开始一直到offset节点的元素，也就是[0, OFFSET-1].
//                 输入node保持着offset后面的元素，也就是 [OFFSET, END].
// 输入的节点包含了返回节点以为的所有元素
// 返回新创建的节点，如果不能拆分就返回NULL
REDIS_STATIC quicklistNode *_quicklistSplitNode(quicklistNode *node, int offset,
                                                int after) {
    size_t zl_sz = node->sz;
    // 创建一个新的节点
    quicklistNode *new_node = quicklistCreateNode();
    // 分配了同样大小的内存
    new_node->entry = zmalloc(zl_sz);

    /* Copy original listpack so we can split it */
    // 将listpack的数据也拷贝进去
    memcpy(new_node->entry, node->entry, zl_sz);

    /* Need positive offset for calculating extent below. */
    // 如果偏移为负数，需要转换为正数
    if (offset < 0) offset = node->count + offset;

    /* Ranges to be trimmed: -1 here means "continue deleting until the list ends" */
    // 上面产生了2个一模一样的全的listpack，下面计算裁剪的范围,-1表示一直删到最后
    int orig_start = after ? offset + 1 : 0;
    int orig_extent = after ? -1 : offset;
    int new_start = after ? 0 : offset;
    int new_extent = after ? offset + 1 : -1;

    D("After %d (%d); ranges: [%d, %d], [%d, %d]", after, offset, orig_start,
      orig_extent, new_start, new_extent);
    // 删除当前节点对应的部分，更新元素数量和尺寸
    node->entry = lpDeleteRange(node->entry, orig_start, orig_extent);
    node->count = lpLength(node->entry);
    quicklistNodeUpdateSz(node);

    // 删除新节点对应的部分，更新元素的数量和尺寸
    new_node->entry = lpDeleteRange(new_node->entry, new_start, new_extent);
    new_node->count = lpLength(new_node->entry);
    quicklistNodeUpdateSz(new_node);

    D("After split lengths: orig (%d), new (%d)", node->count, new_node->count);
    return new_node;
}

/* Insert a new entry before or after existing entry 'entry'.
 *
 * If after==1, the new value is inserted after 'entry', otherwise
 * the new value is inserted before 'entry'. */
// 在已经存在的条目前面或者后面插入一个新的条目
// 如果after==1,新的值插入到entry得后面，否则新的值插入到entry的前面
REDIS_STATIC void _quicklistInsert(quicklistIter *iter, quicklistEntry *entry,
                                   void *value, const size_t sz, int after)
{
    quicklist *quicklist = iter->quicklist;
    int full = 0, at_tail = 0, at_head = 0, avail_next = 0, avail_prev = 0;
    int fill = quicklist->fill;
    quicklistNode *node = entry->node;
    quicklistNode *new_node = NULL;

    // 没有引用的节点
    if (!node) {
        /* we have no reference node, so let's create only node in the list */
        D("No node given!");
        // 如果是大尺寸元素，创建一个普通节点插入到快速列表
        if (unlikely(isLargeElement(sz))) {
            __quicklistInsertPlainNode(quicklist, quicklist->tail, value, sz, after);
            return;
        }
        // 创建列表，然后插入到列表中
        new_node = quicklistCreateNode();
        new_node->entry = lpPrepend(lpNew(0), value, sz);
        __quicklistInsertNode(quicklist, NULL, new_node, after);
        // 增加元素数量
        new_node->count++;
        quicklist->count++;
        return;
    }

    /* Populate accounting flags for easier boolean checks later */
    // 检查该节点是否允许插入
    if (!_quicklistNodeAllowInsert(node, fill, sz)) {
        D("Current node is full with count %d with requested fill %d",
          node->count, fill);
        full = 1;
    }
    // 如果是插在后面，并且原条目是最后的条目
    if (after && (entry->offset == node->count - 1 || entry->offset == -1)) {
        D("At Tail of current listpack");
        at_tail = 1;
        // 后面的节点存在，并且可以插入
        if (_quicklistNodeAllowInsert(node->next, fill, sz)) {
            D("Next node is available.");
            avail_next = 1;
        }
    }

    // 如果插在前面，且条目为最前面的条目
    if (!after && (entry->offset == 0 || entry->offset == -(node->count))) {
        D("At Head");
        at_head = 1;
        if (_quicklistNodeAllowInsert(node->prev, fill, sz)) {
            D("Prev node is available.");
            avail_prev = 1;
        }
    }
    // 如果是大尺寸节点
    if (unlikely(isLargeElement(sz))) {
        // 如果是普通节点，或者在末尾条目后面插入，或者在首条目前面插入，插入一个新的节点
        if (QL_NODE_IS_PLAIN(node) || (at_tail && after) || (at_head && !after)) {
            __quicklistInsertPlainNode(quicklist, node, value, sz, after);
        } else {
            // 解压节点
            quicklistDecompressNodeForUse(node);
            // 将节点拆分成两个节点
            new_node = _quicklistSplitNode(node, entry->offset, after);
            // 连接节点
            quicklistNode *entry_node = __quicklistCreatePlainNode(value, sz);
            __quicklistInsertNode(quicklist, node, entry_node, after);
            __quicklistInsertNode(quicklist, entry_node, new_node, after);
            quicklist->count++;
        }
        return;
    }

    /* Now determine where and how to insert the new element */
    // 现在决定在那里怎么样插入新的元素
    if (!full && after) {
        D("Not full, inserting after current position.");
        // 解压
        quicklistDecompressNodeForUse(node);
        // 插入条目
        node->entry = lpInsertString(node->entry, value, sz, entry->zi, LP_AFTER, NULL);
        // 更新条目数，节点尺寸
        node->count++;
        quicklistNodeUpdateSz(node);
        // 重新压缩
        quicklistRecompressOnly(node);
    } else if (!full && !after) {
        D("Not full, inserting before current position.");
        // 解压
        quicklistDecompressNodeForUse(node);
        // 插入条码并且更新条目数和尺寸
        node->entry = lpInsertString(node->entry, value, sz, entry->zi, LP_BEFORE, NULL);
        node->count++;
        quicklistNodeUpdateSz(node);
        // 重新解压
        quicklistRecompressOnly(node);
    } 
    // 当前节点满了，并且条目在最后，并且在后面插入，有下一个节点，
    else if (full && at_tail && avail_next && after) {
        /* If we are: at tail, next has free space, and inserting after:
         *   - insert entry at head of next node. */
        D("Full and tail, but next isn't full; inserting next node head");
        // 在该节点的最后条目的后面插入，但是该节点已经满了，后面一个节点还有空闲位置
        // 那就插入在下一个节点的开始条目之前
        new_node = node->next;
        // 解压
        quicklistDecompressNodeForUse(new_node);
        // 在下一个节点的开始条目之前插入
        new_node->entry = lpPrepend(new_node->entry, value, sz);
        new_node->count++;
        // 更新尺寸和重新压缩
        quicklistNodeUpdateSz(new_node);
        quicklistRecompressOnly(new_node);
        quicklistRecompressOnly(node);
    } 
    // 当前节点满了，并且条目在开始处，插入在开始条目之前，并且还有前一个节点
    else if (full && at_head && avail_prev && !after) {
        /* If we are: at head, previous has free space, and inserting before:
         *   - insert entry at tail of previous node. */
        D("Full and head, but prev isn't full, inserting prev node tail");
        // 在该节点的最开始的条目前面插入，但是该节点已经满了，前面一个节点还有空闲位置
        // 那就插入在前一个节点的末尾条目之后
        new_node = node->prev;
        quicklistDecompressNodeForUse(new_node);
        // 插入在前一个节点的末尾条目之后
        new_node->entry = lpAppend(new_node->entry, value, sz);
        new_node->count++;
        // 更新尺寸然后重新压缩
        quicklistNodeUpdateSz(new_node);
        quicklistRecompressOnly(new_node);
        quicklistRecompressOnly(node);
    } 
    // 当前节点满了，插入末尾条目之后，后续节点不能插入
    // 当前节点满了，插入开始条目之前，前面的节点不能插入，
    else if (full && ((at_tail && !avail_next && after) ||
                        (at_head && !avail_prev && !after))) {
        /* If we are: full, and our prev/next has no available space, then:
         *   - create new node and attach to quicklist */
        // 创建新的节点，挂接在当前节点之后
        D("\tprovisioning new node...");
        new_node = quicklistCreateNode();
        // 传入的值为当前节点的条目
        new_node->entry = lpPrepend(lpNew(0), value, sz);
        new_node->count++;
        // 更新尺寸
        quicklistNodeUpdateSz(new_node);
        // 插入列表
        __quicklistInsertNode(quicklist, node, new_node, after);
    } else if (full) {
        /* else, node is full we need to split it. */
        /* covers both after and !after cases */
        // 节点满了，需要拆分
        D("\tsplitting node...");
        quicklistDecompressNodeForUse(node);
        // 拆分成两个节点
        new_node = _quicklistSplitNode(node, entry->offset, after);
        // 在条目后或者前插入
        if (after)
            new_node->entry = lpPrepend(new_node->entry, value, sz);
        else
            new_node->entry = lpAppend(new_node->entry, value, sz);
        new_node->count++;
        quicklistNodeUpdateSz(new_node);
        // 节点插入到快速列表中
        __quicklistInsertNode(quicklist, node, new_node, after);
        _quicklistMergeNodes(quicklist, node);
    }
    // 快速列表元素增加
    quicklist->count++;

    /* In any case, we reset iterator to forbid use of iterator after insert.
     * Notice: iter->current has been compressed in _quicklistInsert(). */
    resetIterator(iter); 
}
// 在条目entry的前面插入一个条目
void quicklistInsertBefore(quicklistIter *iter, quicklistEntry *entry,
                           void *value, const size_t sz)
{
    _quicklistInsert(iter, entry, value, sz, 0);
}
// 在条目entry的后面插入一个条目
void quicklistInsertAfter(quicklistIter *iter, quicklistEntry *entry,
                          void *value, const size_t sz)
{
    _quicklistInsert(iter, entry, value, sz, 1);
}

/* Delete a range of elements from the quicklist.
 *
 * elements may span across multiple quicklistNodes, so we
 * have to be careful about tracking where we start and end.
 *
 * Returns 1 if entries were deleted, 0 if nothing was deleted. */
// 从quicklist中删除一个范围的元素
// 元素可能会跨越多个quicklistNodes，所以我们需要注意它们从哪开始到哪结束
// 如果条目已经删除返回1，如果没东西删除返回0
int quicklistDelRange(quicklist *quicklist, const long start,
                      const long count) {
    if (count <= 0)
        return 0;

    unsigned long extent = count; /* range is inclusive of start position */
    // 如果count超出了，需要限制大小
    if (start >= 0 && extent > (quicklist->count - start)) {
        /* if requesting delete more elements than exist, limit to list size. */
        extent = quicklist->count - start;
    } else if (start < 0 && extent > (unsigned long)(-start)) {
        /* else, if at negative offset, limit max size to rest of list. */
        extent = -start; /* c.f. LREM -29 29; just delete until end. */
    }

    // 得到索引对应的迭代器
    quicklistIter *iter = quicklistGetIteratorAtIdx(quicklist, AL_START_TAIL, start);
    if (!iter)
        return 0;

    D("Quicklist delete request for start %ld, count %ld, extent: %ld", start,
      count, extent);
    // 从迭代器里面取出当前节点和偏移，然后释放迭代器
    quicklistNode *node = iter->current;
    long offset = iter->offset;
    quicklistReleaseIterator(iter);

    /* iterate over next nodes until everything is deleted. */
    // 遍历节点直到一切都删除掉
    while (extent) {
        quicklistNode *next = node->next;

        unsigned long del;
        int delete_entire_node = 0;
        // 如果从头开始删除，并且剩余的要删除的元素数目大于整个节点的元素数目,
        // 那就删除整个节点
        if (offset == 0 && extent >= node->count) {
            /* If we are deleting more than the count of this node, we
             * can just delete the entire node without listpack math. */
            delete_entire_node = 1;
            del = node->count;
        } 
        // 如果在此节点之后删除更多节点，则根据当前节点的大小计算删除。
        else if (offset >= 0 && extent + offset >= node->count) {
            /* If deleting more nodes after this one, calculate delete based
             * on size of current node. */
            del = node->count - offset;
        } else if (offset < 0) {
            /* If offset is negative, we are in the first run of this loop
             * and we are deleting the entire range
             * from this start offset to end of list.  Since the Negative
             * offset is the number of elements until the tail of the list,
             * just use it directly as the deletion count. */
            // 计算删除的数量，如果数量超过剩余数量，也只删除剩余数量
            del = -offset;

            /* If the positive offset is greater than the remaining extent,
             * we only delete the remaining extent, not the entire offset.
             */
            if (del > extent)
                del = extent;
        } else {
            /* else, we are deleting less than the extent of this node, so
             * use extent directly. */
            // 删除剩余的数量
            del = extent;
        }

        D("[%ld]: asking to del: %ld because offset: %d; (ENTIRE NODE: %d), "
          "node count: %u",
          extent, del, offset, delete_entire_node, node->count);

        // 如果需要删除整个节点，或者是普通节点，就直接将节点删除
        if (delete_entire_node || QL_NODE_IS_PLAIN(node)) {
            __quicklistDelNode(quicklist, node);
        } else {
            // 解压节点
            quicklistDecompressNodeForUse(node);
            // 删除从偏移处开始的del个元素
            node->entry = lpDeleteRange(node->entry, offset, del);
            quicklistNodeUpdateSz(node);
            node->count -= del;
            quicklist->count -= del;
            // 如果节点为空，就将节点删除
            quicklistDeleteIfEmpty(quicklist, node);
            // 不为空，需要重新压缩
            if (node)
                quicklistRecompressOnly(node);
        }
        // 减少剩余要删除的节点
        extent -= del;
        // 下一个节点
        node = next;

        offset = 0;
    }
    return 1;
}

/* compare between a two entries */
// 比较两个条目
int quicklistCompare(quicklistEntry* entry, unsigned char *p2, const size_t p2_len) {
    // 如果是普通节点，直接对长度和内存进行比较
    if (unlikely(QL_NODE_IS_PLAIN(entry->node))) {
        return ((entry->sz == p2_len) && (memcmp(entry->value, p2, p2_len) == 0));
    }
    // 对listpack进行比较
    return lpCompare(entry->zi, p2, p2_len);
}

/* Returns a quicklist iterator 'iter'. After the initialization every
 * call to quicklistNext() will return the next element of the quicklist. */
// 返回一个quicklist的迭代器iter.在初始化以后，每一次调用quicklistNext将会返回quicklist下一个元素
quicklistIter *quicklistGetIterator(quicklist *quicklist, int direction) {
    quicklistIter *iter;
    
    iter = zmalloc(sizeof(*iter));
    // 根据方向初始化当前的节点和偏移
    if (direction == AL_START_HEAD) {
        iter->current = quicklist->head;
        iter->offset = 0;
    } else if (direction == AL_START_TAIL) {
        iter->current = quicklist->tail;
        iter->offset = -1;
    }
    // 初始化方向和遍历的quicklist
    iter->direction = direction;
    iter->quicklist = quicklist;

    iter->zi = NULL;

    return iter;
}

/* Initialize an iterator at a specific offset 'idx' and make the iterator
 * return nodes in 'direction' direction. */
// 用特定偏移量“idx”处的元素初始化迭代器，并使迭代器返回“direction”方向的节点
quicklistIter *quicklistGetIteratorAtIdx(quicklist *quicklist,
                                         const int direction,
                                         const long long idx)
{
    quicklistNode *n;
    unsigned long long accum = 0;
    unsigned long long index;
    // 根据索引的正负值，往前找，还是往后找
    int forward = idx < 0 ? 0 : 1; /* < 0 -> reverse, 0+ -> forward */

    // 计算索引
    index = forward ? idx : (-idx) - 1;
    // 如果超过元素数量，直接返回NULL
    if (index >= quicklist->count)
        return NULL;

    /* Seek in the other direction if that way is shorter. */
    int seek_forward = forward;
    unsigned long long seek_index = index;
    // 如果换一个方向路径更短，那就调整方向
    if (index > (quicklist->count - 1) / 2) {
        seek_forward = !forward;
        seek_index = quicklist->count - 1 - index;
    }

    n = seek_forward ? quicklist->head : quicklist->tail;
    // 开始遍历节点
    while (likely(n)) {
        // 如果数量已经达到偏移，找到
        if ((accum + n->count) > seek_index) {
            break;
        } else {
            D("Skipping over (%p) %u at accum %lld", (void *)n, n->count,
              accum);
            // 累计节点
            accum += n->count;
            // 下一个节点
            n = seek_forward ? n->next : n->prev;
        }
    }

    // 如果没找到节点直接返回NULL
    if (!n)
        return NULL;

    /* Fix accum so it looks like we seeked in the other direction. */
    // 根据搜索的方向是否和原方向一致，调整accum的值
    if (seek_forward != forward) accum = quicklist->count - n->count - accum;

    D("Found node: %p at accum %llu, idx %llu, sub+ %llu, sub- %llu", (void *)n,
      accum, index, index - accum, (-index) - 1 + accum);

    // 设置当前节点和偏移
    quicklistIter *iter = quicklistGetIterator(quicklist, direction);
    iter->current = n;
    if (forward) {
        /* forward = normal head-to-tail offset. */
        iter->offset = index - accum;
    } else {
        /* reverse = need negative offset for tail-to-head, so undo
         * the result of the original index = (-idx) - 1 above. */
        iter->offset = (-index) - 1 + accum;
    }
    // 返回迭代器
    return iter;
}

/* Release iterator.
 * If we still have a valid current node, then re-encode current node. */
// 释放迭代器，如果迭代器还有一个有效的当前节点，对当前节点重新压缩
void quicklistReleaseIterator(quicklistIter *iter) {
    if (!iter) return;
    if (iter->current)
        quicklistCompress(iter->quicklist, iter->current);

    zfree(iter);
}

/* Get next element in iterator.
 *
 * Note: You must NOT insert into the list while iterating over it.
 * You *may* delete from the list while iterating using the
 * quicklistDelEntry() function.
 * If you insert into the quicklist while iterating, you should
 * re-create the iterator after your addition.
 *
 * iter = quicklistGetIterator(quicklist,<direction>);
 * quicklistEntry entry;
 * while (quicklistNext(iter, &entry)) {
 *     if (entry.value)
 *          [[ use entry.value with entry.sz ]]
 *     else
 *          [[ use entry.longval ]]
 * }
 *
 * Populates 'entry' with values for this iteration.
 * Returns 0 when iteration is complete or if iteration not possible.
 * If return value is 0, the contents of 'entry' are not valid.
 */
// 得到迭代器的下一个元素
int quicklistNext(quicklistIter *iter, quicklistEntry *entry) {
    initEntry(entry);

    if (!iter) {
        D("Returning because no iter!");
        return 0;
    }
    // 初始化快速列表和节点
    entry->quicklist = iter->quicklist;
    entry->node = iter->current;

    if (!iter->current) {
        D("Returning because current node is NULL");
        return 0;
    }

    unsigned char *(*nextFn)(unsigned char *, unsigned char *) = NULL;
    int offset_update = 0;
    // 是否为普通节点
    int plain = QL_NODE_IS_PLAIN(iter->current);
    // 如果没有指向某个元素
    if (!iter->zi) {
        /* If !zi, use current index. */
        quicklistDecompressNodeForUse(iter->current);
        // 如果是普通节点，指向节点的条目
        if (unlikely(plain))
            iter->zi = iter->current->entry;
        else
            // 指向节点的偏移条目
            iter->zi = lpSeek(iter->current->entry, iter->offset);
    } 
    // 如果是普通节点
    else if (unlikely(plain)) {
        // 节点中没有下一个元素，清空指向的当前元素
        iter->zi = NULL;
    } else {
        /* else, use existing iterator offset and get prev/next as necessary. */
        // 从前往后
        if (iter->direction == AL_START_HEAD) {
            nextFn = lpNext;
            offset_update = 1;
        } 
        // 从后往前
        else if (iter->direction == AL_START_TAIL) {
            nextFn = lpPrev;
            offset_update = -1;
        }
        // 得到指向的元素和偏移
        iter->zi = nextFn(iter->current->entry, iter->zi);
        iter->offset += offset_update;
    }
    // 更新到条目
    entry->zi = iter->zi;
    entry->offset = iter->offset;

    // 指向某个条目
    if (iter->zi) {
        // 普通节点，
        if (unlikely(plain)) {
            entry->value = entry->node->entry;
            entry->sz = entry->node->sz;
            return 1;
        }
        /* Populate value from existing listpack position */
        // 得到lispack中条目的值和大小
        unsigned int sz = 0;
        entry->value = lpGetValue(entry->zi, &sz, &entry->longval);
        entry->sz = sz;
        return 1;
    } 
    // 没有指向某一个条目，说明到了节点的末尾
    else {
        /* We ran out of listpack entries.
         * Pick next node, update offset, then re-run retrieval. */
        quicklistCompress(iter->quicklist, iter->current);
        // 从头到尾，取下一个元素
        if (iter->direction == AL_START_HEAD) {
            /* Forward traversal */
            D("Jumping to start of next node");
            iter->current = iter->current->next;
            iter->offset = 0;
        } 
        // 从尾到头，取上一个元素
        else if (iter->direction == AL_START_TAIL) {
            /* Reverse traversal */
            D("Jumping to end of previous node");
            iter->current = iter->current->prev;
            iter->offset = -1;
        }
        iter->zi = NULL;
        // 然后递归重新取一次
        return quicklistNext(iter, entry);
    }
}

/* Sets the direction of a quicklist iterator. */
// 设置quicklist迭代器的方向
void quicklistSetDirection(quicklistIter *iter, int direction) {
    iter->direction = direction;
}

/* Duplicate the quicklist.
 * On success a copy of the original quicklist is returned.
 *
 * The original quicklist both on success or error is never modified.
 *
 * Returns newly allocated quicklist. */
// 复制quicklist
// 如果成功一个原始quicklist的拷贝将会返回
// 该原始的quicklist不管成功还是失败都不会被修改
// 返回新分配的quicklist
quicklist *quicklistDup(quicklist *orig) {
    quicklist *copy;
    // 创建一个新的quicklist，设置fill和compress
    copy = quicklistNew(orig->fill, orig->compress);

    // 遍历节点，然后拷贝
    for (quicklistNode *current = orig->head; current;
         current = current->next) {
        // 创建节点
        quicklistNode *node = quicklistCreateNode();
        // LZF压缩节点
        if (current->encoding == QUICKLIST_NODE_ENCODING_LZF) {
            quicklistLZF *lzf = (quicklistLZF *)current->entry;
            size_t lzf_sz = sizeof(*lzf) + lzf->sz;
            node->entry = zmalloc(lzf_sz);
            memcpy(node->entry, current->entry, lzf_sz);
        } 
        // 原生节点
        else if (current->encoding == QUICKLIST_NODE_ENCODING_RAW) {
            node->entry = zmalloc(current->sz);
            memcpy(node->entry, current->entry, current->sz);
        }

        // 设置节点的元素个数，尺寸，编码，存储方式等
        node->count = current->count;
        copy->count += node->count;
        node->sz = current->sz;
        node->encoding = current->encoding;
        node->container = current->container;
        // 节点插入到列表后面
        _quicklistInsertNodeAfter(copy, copy->tail, node);
    }

    /* copy->count must equal orig->count here */
    return copy;
}

/* Populate 'entry' with the element at the specified zero-based index
 * where 0 is the head, 1 is the element next to head
 * and so on. Negative integers are used in order to count
 * from the tail, -1 is the last element, -2 the penultimate
 * and so on. If the index is out of range 0 is returned.
 *
 * Returns an iterator at a specific offset 'idx' if element found
 * Returns NULL if element not found */
// 将“entry”填充为指定基于0位为索引的元素，其中0是头部，1是紧跟头部的元素，
// 以此类推。 使用负整数从尾部开始计数，-1是最后一个元素，-2是倒数第二个元素，以此类推。如果索引超出范围，将返回0。
// 
// 返回值：找到指定偏移idx上的元素并返回一个迭代器，如果元素没有找到就返回NULL
quicklistIter *quicklistGetIteratorEntryAtIdx(quicklist *quicklist, const long long idx,
                                              quicklistEntry *entry)
{
    // 得到idx处的元素，并返回迭代器
    quicklistIter *iter = quicklistGetIteratorAtIdx(quicklist, AL_START_TAIL, idx);
    if (!iter) return NULL;
    // 得到对应元素放入entry中
    assert(quicklistNext(iter, entry));
    return iter;
}

// 快速列表旋转
static void quicklistRotatePlain(quicklist *quicklist) {
    // 尾结点变成新的头节点
    quicklistNode *new_head = quicklist->tail;
    // 尾结点的前一个节点变成新的尾节点
    quicklistNode *new_tail = quicklist->tail->prev;
    
    // 处理原来的头、尾，新的头、尾链表关系
    quicklist->head->prev = new_head;
    new_tail->next = NULL;
    new_head->next = quicklist->head;
    new_head->prev = NULL;
    // 设置快速表的头，尾节点
    quicklist->head = new_head;
    quicklist->tail = new_tail;
}

/* Rotate quicklist by moving the tail element to the head. */
// 通过将尾部元素移动到头部来旋转quicklist
void quicklistRotate(quicklist *quicklist) {
    // 如果元素数目小于等于1，怎么旋转都一样
    if (quicklist->count <= 1)
        return;

    // 如果尾部节点是一个普通节点，直接旋转节点就行
    if (unlikely(QL_NODE_IS_PLAIN(quicklist->tail))) {
        quicklistRotatePlain(quicklist);
        return;
    }

    /* First, get the tail entry */
    // 首先，得到尾部元素的数据
    unsigned char *p = lpSeek(quicklist->tail->entry, -1);
    unsigned char *value, *tmp;
    long long longval;
    unsigned int sz;
    char longstr[32] = {0};
    tmp = lpGetValue(p, &sz, &longval);

    /* If value found is NULL, then lpGet populated longval instead */
    // 如果找到的值为NULL，则改为lpGet填充longval
    if (!tmp) {
        /* Write the longval as a string so we can re-add it */
        // 将longval写成字符串，以便我们可以重新添加它
        sz = ll2string(longstr, sizeof(longstr), longval);
        value = (unsigned char *)longstr;
    }
    // 如果quicklist只有一个节点
    else if (quicklist->len == 1) {
        /* Copy buffer since there could be a memory overlap when move
         * entity from tail to head in the same listpack. */
        // 复制缓冲区，因为在同一个listpack中将实体从尾部移动到头部时可能存在内存重叠
        value = zmalloc(sz);
        memcpy(value, tmp, sz);
    } else {
        value = tmp;
    }

    /* Add tail entry to head (must happen before tail is deleted). */
    // 将尾元素加到头部（必须在尾元素删除之前）
    quicklistPushHead(quicklist, value, sz);

    /* If quicklist has only one node, the head listpack is also the
     * tail listpack and PushHead() could have reallocated our single listpack,
     * which would make our pre-existing 'p' unusable. */
    // 如果quicklist只有一个节点，那么开始的listpack也是末尾的listpack，PushHead可能会
    // 重新分配我们的单个列表包，这将使我们之前存在的“p”不可用，所以得重新取一次
    if (quicklist->len == 1) {
        p = lpSeek(quicklist->tail->entry, -1);
    }

    /* Remove tail entry. */
    // 将尾部元素删除
    quicklistDelIndex(quicklist, quicklist->tail, &p);
    // 释放临时分配的内存
    if (value != (unsigned char*)longstr && value != tmp)
        zfree(value);
}

/* pop from quicklist and return result in 'data' ptr.  Value of 'data'
 * is the return value of 'saver' function pointer if the data is NOT a number.
 *
 * If the quicklist element is a long long, then the return value is returned in
 * 'sval'.
 *
 * Return value of 0 means no elements available.
 * Return value of 1 means check 'data' and 'sval' for values.
 * If 'data' is set, use 'data' and 'sz'.  Otherwise, use 'sval'. */
// 从quicklist中弹出并在“data”指针中返回结果。如果数据不是数字，则“data”的值是“saver”函数的返回值
// 如果quicklist元素是long-long，则返回值以'sval形式返回
// 返回值0表示没有元素可用
// 返回值1表示检查data和sval
// 如果data被设置了值，这使用data和sz。否则使用sval
int quicklistPopCustom(quicklist *quicklist, int where, unsigned char **data,
                       size_t *sz, long long *sval,
                       void *(*saver)(unsigned char *data, size_t sz)) {
    unsigned char *p;
    unsigned char *vstr;
    unsigned int vlen;
    long long vlong;
    int pos = (where == QUICKLIST_HEAD) ? 0 : -1;
    // 如果没有元素，直接返回0
    if (quicklist->count == 0)
        return 0;

    // 将这些可以承载弹出数据的
    if (data)
        *data = NULL;
    if (sz)
        *sz = 0;
    if (sval)
        *sval = -123456789;

    // 更加从头还是尾弹出，取出节点
    quicklistNode *node;
    if (where == QUICKLIST_HEAD && quicklist->head) {
        node = quicklist->head;
    } else if (where == QUICKLIST_TAIL && quicklist->tail) {
        node = quicklist->tail;
    } else {
        return 0;
    }

    /* The head and tail should never be compressed */
    // 头和尾节点都不应该压缩
    assert(node->encoding != QUICKLIST_NODE_ENCODING_LZF);

    // 如果是普通节点，保存数据
    if (unlikely(QL_NODE_IS_PLAIN(node))) {
        if (data)
            *data = saver(node->entry, node->sz);
        if (sz)
            *sz = node->sz;
        // 删除节点
        quicklistDelIndex(quicklist, node, NULL);
        return 1;
    }
    // 移动到指定位置
    p = lpSeek(node->entry, pos);
    // 获取指定位置的值
    vstr = lpGetValue(p, &vlen, &vlong);
    // 如果是二进制流
    if (vstr) {
        // 通过传入的saver函数处理
        if (data)
            *data = saver(vstr, vlen);
        if (sz)
            *sz = vlen;
    } else {
        // 得到数字相关的数据
        if (data)
            *data = NULL;
        if (sval)
            *sval = vlong;
    }
    // 删除对应的元素
    quicklistDelIndex(quicklist, node, &p);
    return 1;
}

/* Return a malloc'd copy of data passed in */
// 返回一个传入数据的分配内存的拷贝
REDIS_STATIC void *_quicklistSaver(unsigned char *data, size_t sz) {
    unsigned char *vstr;
    if (data) {
        vstr = zmalloc(sz);
        memcpy(vstr, data, sz);
        return vstr;
    }
    return NULL;
}

/* Default pop function
 *
 * Returns malloc'd value from quicklist */
// 默认的pop函数，返回一个分配好的值给quicklist
int quicklistPop(quicklist *quicklist, int where, unsigned char **data,
                 size_t *sz, long long *slong) {
    unsigned char *vstr = NULL;
    size_t vlen = 0;
    long long vlong = 0;
    if (quicklist->count == 0)
        return 0;
    // 弹出数据，转换为外面的参数
    int ret = quicklistPopCustom(quicklist, where, &vstr, &vlen, &vlong,
                                 _quicklistSaver);
    if (data)
        *data = vstr;
    if (slong)
        *slong = vlong;
    if (sz)
        *sz = vlen;
    return ret;
}

/* Wrapper to allow argument-based switching between HEAD/TAIL pop */
// 包装器，允许在HEAD/TAIL push之间进行基于参数的切换
void quicklistPush(quicklist *quicklist, void *value, const size_t sz,
                   int where) {
    /* The head and tail should never be compressed (we don't attempt to decompress them) */
    // 头，尾节点不能压缩
    if (quicklist->head)
        assert(quicklist->head->encoding != QUICKLIST_NODE_ENCODING_LZF);
    if (quicklist->tail)
        assert(quicklist->tail->encoding != QUICKLIST_NODE_ENCODING_LZF);

    // 从头或者尾压入数据
    if (where == QUICKLIST_HEAD) {
        quicklistPushHead(quicklist, value, sz);
    } else if (where == QUICKLIST_TAIL) {
        quicklistPushTail(quicklist, value, sz);
    }
}

/* Print info of quicklist which is used in debugCommand. */
void quicklistRepr(unsigned char *ql, int full) {
    int i = 0;
    quicklist *quicklist  = (struct quicklist*) ql;
    printf("{count : %ld}\n", quicklist->count);
    printf("{len : %ld}\n", quicklist->len);
    printf("{fill : %d}\n", quicklist->fill);
    printf("{compress : %d}\n", quicklist->compress);
    printf("{bookmark_count : %d}\n", quicklist->bookmark_count);
    quicklistNode* node = quicklist->head;

    while(node != NULL) {
        printf("{quicklist node(%d)\n", i++);
        printf("{container : %s, encoding: %s, size: %zu, count: %d, recompress: %d, attempted_compress: %d}\n",
               QL_NODE_IS_PLAIN(node) ? "PLAIN": "PACKED",
               (node->encoding == QUICKLIST_NODE_ENCODING_RAW) ? "RAW": "LZF",
               node->sz,
               node->count,
               node->recompress,
               node->attempted_compress);

        if (full) {
            quicklistDecompressNode(node);
            if (node->container == QUICKLIST_NODE_CONTAINER_PACKED) {
                printf("{ listpack:\n");
                lpRepr(node->entry);
                printf("}\n");

            } else if (QL_NODE_IS_PLAIN(node)) {
                printf("{ entry : %s }\n", node->entry);
            }
            printf("}\n");
            quicklistRecompressOnly(node);
        }
        node = node->next;
    }
}

/* Create or update a bookmark in the list which will be updated to the next node
 * automatically when the one referenced gets deleted.
 * Returns 1 on success (creation of new bookmark or override of an existing one).
 * Returns 0 on failure (reached the maximum supported number of bookmarks).
 * NOTE: use short simple names, so that string compare on find is quick.
 * NOTE: bookmark creation may re-allocate the quicklist, so the input pointer
         may change and it's the caller responsibility to update the reference.
 */
int quicklistBookmarkCreate(quicklist **ql_ref, const char *name, quicklistNode *node) {
    quicklist *ql = *ql_ref;
    if (ql->bookmark_count >= QL_MAX_BM)
        return 0;
    quicklistBookmark *bm = _quicklistBookmarkFindByName(ql, name);
    if (bm) {
        bm->node = node;
        return 1;
    }
    ql = zrealloc(ql, sizeof(quicklist) + (ql->bookmark_count+1) * sizeof(quicklistBookmark));
    *ql_ref = ql;
    ql->bookmarks[ql->bookmark_count].node = node;
    ql->bookmarks[ql->bookmark_count].name = zstrdup(name);
    ql->bookmark_count++;
    return 1;
}

/* Find the quicklist node referenced by a named bookmark.
 * When the bookmarked node is deleted the bookmark is updated to the next node,
 * and if that's the last node, the bookmark is deleted (so find returns NULL). */
quicklistNode *quicklistBookmarkFind(quicklist *ql, const char *name) {
    quicklistBookmark *bm = _quicklistBookmarkFindByName(ql, name);
    if (!bm) return NULL;
    return bm->node;
}

/* Delete a named bookmark.
 * returns 0 if bookmark was not found, and 1 if deleted.
 * Note that the bookmark memory is not freed yet, and is kept for future use. */
int quicklistBookmarkDelete(quicklist *ql, const char *name) {
    quicklistBookmark *bm = _quicklistBookmarkFindByName(ql, name);
    if (!bm)
        return 0;
    _quicklistBookmarkDelete(ql, bm);
    return 1;
}

quicklistBookmark *_quicklistBookmarkFindByName(quicklist *ql, const char *name) {
    unsigned i;
    for (i=0; i<ql->bookmark_count; i++) {
        if (!strcmp(ql->bookmarks[i].name, name)) {
            return &ql->bookmarks[i];
        }
    }
    return NULL;
}

quicklistBookmark *_quicklistBookmarkFindByNode(quicklist *ql, quicklistNode *node) {
    unsigned i;
    for (i=0; i<ql->bookmark_count; i++) {
        if (ql->bookmarks[i].node == node) {
            return &ql->bookmarks[i];
        }
    }
    return NULL;
}

void _quicklistBookmarkDelete(quicklist *ql, quicklistBookmark *bm) {
    int index = bm - ql->bookmarks;
    zfree(bm->name);
    ql->bookmark_count--;
    memmove(bm, bm+1, (ql->bookmark_count - index)* sizeof(*bm));
    /* NOTE: We do not shrink (realloc) the quicklist yet (to avoid resonance,
     * it may be re-used later (a call to realloc may NOP). */
}

void quicklistBookmarksClear(quicklist *ql) {
    while (ql->bookmark_count)
        zfree(ql->bookmarks[--ql->bookmark_count].name);
    /* NOTE: We do not shrink (realloc) the quick list. main use case for this
     * function is just before releasing the allocation. */
}

/* The rest of this file is test cases and test helpers. */
#ifdef REDIS_TEST
#include <stdint.h>
#include <sys/time.h>
#include "testhelp.h"
#include <stdlib.h>

#define yell(str, ...) printf("ERROR! " str "\n\n", __VA_ARGS__)

#define ERROR                                                                  \
    do {                                                                       \
        printf("\tERROR!\n");                                                  \
        err++;                                                                 \
    } while (0)

#define ERR(x, ...)                                                            \
    do {                                                                       \
        printf("%s:%s:%d:\t", __FILE__, __func__, __LINE__);                   \
        printf("ERROR! " x "\n", __VA_ARGS__);                                 \
        err++;                                                                 \
    } while (0)

#define TEST(name) printf("test — %s\n", name);
#define TEST_DESC(name, ...) printf("test — " name "\n", __VA_ARGS__);

#define QL_TEST_VERBOSE 0

#define UNUSED(x) (void)(x)
static void ql_info(quicklist *ql) {
#if QL_TEST_VERBOSE
    printf("Container length: %lu\n", ql->len);
    printf("Container size: %lu\n", ql->count);
    if (ql->head)
        printf("\t(zsize head: %lu)\n", lpLength(ql->head->entry));
    if (ql->tail)
        printf("\t(zsize tail: %lu)\n", lpLength(ql->tail->entry));
    printf("\n");
#else
    UNUSED(ql);
#endif
}

/* Return the UNIX time in microseconds */
static long long ustime(void) {
    struct timeval tv;
    long long ust;

    gettimeofday(&tv, NULL);
    ust = ((long long)tv.tv_sec) * 1000000;
    ust += tv.tv_usec;
    return ust;
}

/* Return the UNIX time in milliseconds */
static long long mstime(void) { return ustime() / 1000; }

/* Iterate over an entire quicklist.
 * Print the list if 'print' == 1.
 *
 * Returns physical count of elements found by iterating over the list. */
static int _itrprintr(quicklist *ql, int print, int forward) {
    quicklistIter *iter =
        quicklistGetIterator(ql, forward ? AL_START_HEAD : AL_START_TAIL);
    quicklistEntry entry;
    int i = 0;
    int p = 0;
    quicklistNode *prev = NULL;
    while (quicklistNext(iter, &entry)) {
        if (entry.node != prev) {
            /* Count the number of list nodes too */
            p++;
            prev = entry.node;
        }
        if (print) {
            int size = (entry.sz > (1<<20)) ? 1<<20 : entry.sz;
            printf("[%3d (%2d)]: [%.*s] (%lld)\n", i, p, size,
                   (char *)entry.value, entry.longval);
        }
        i++;
    }
    quicklistReleaseIterator(iter);
    return i;
}
static int itrprintr(quicklist *ql, int print) {
    return _itrprintr(ql, print, 1);
}

static int itrprintr_rev(quicklist *ql, int print) {
    return _itrprintr(ql, print, 0);
}

#define ql_verify(a, b, c, d, e)                                               \
    do {                                                                       \
        err += _ql_verify((a), (b), (c), (d), (e));                            \
    } while (0)

static int _ql_verify_compress(quicklist *ql) {
    int errors = 0;
    if (quicklistAllowsCompression(ql)) {
        quicklistNode *node = ql->head;
        unsigned int low_raw = ql->compress;
        unsigned int high_raw = ql->len - ql->compress;

        for (unsigned int at = 0; at < ql->len; at++, node = node->next) {
            if (node && (at < low_raw || at >= high_raw)) {
                if (node->encoding != QUICKLIST_NODE_ENCODING_RAW) {
                    yell("Incorrect compression: node %d is "
                         "compressed at depth %d ((%u, %u); total "
                         "nodes: %lu; size: %zu; recompress: %d)",
                         at, ql->compress, low_raw, high_raw, ql->len, node->sz,
                         node->recompress);
                    errors++;
                }
            } else {
                if (node->encoding != QUICKLIST_NODE_ENCODING_LZF &&
                    !node->attempted_compress) {
                    yell("Incorrect non-compression: node %d is NOT "
                         "compressed at depth %d ((%u, %u); total "
                         "nodes: %lu; size: %zu; recompress: %d; attempted: %d)",
                         at, ql->compress, low_raw, high_raw, ql->len, node->sz,
                         node->recompress, node->attempted_compress);
                    errors++;
                }
            }
        }
    }
    return errors;
}

/* Verify list metadata matches physical list contents. */
static int _ql_verify(quicklist *ql, uint32_t len, uint32_t count,
                      uint32_t head_count, uint32_t tail_count) {
    int errors = 0;

    ql_info(ql);
    if (len != ql->len) {
        yell("quicklist length wrong: expected %d, got %lu", len, ql->len);
        errors++;
    }

    if (count != ql->count) {
        yell("quicklist count wrong: expected %d, got %lu", count, ql->count);
        errors++;
    }

    int loopr = itrprintr(ql, 0);
    if (loopr != (int)ql->count) {
        yell("quicklist cached count not match actual count: expected %lu, got "
             "%d",
             ql->count, loopr);
        errors++;
    }

    int rloopr = itrprintr_rev(ql, 0);
    if (loopr != rloopr) {
        yell("quicklist has different forward count than reverse count!  "
             "Forward count is %d, reverse count is %d.",
             loopr, rloopr);
        errors++;
    }

    if (ql->len == 0 && !errors) {
        return errors;
    }

    if (ql->head && head_count != ql->head->count &&
        head_count != lpLength(ql->head->entry)) {
        yell("quicklist head count wrong: expected %d, "
             "got cached %d vs. actual %lu",
             head_count, ql->head->count, lpLength(ql->head->entry));
        errors++;
    }

    if (ql->tail && tail_count != ql->tail->count &&
        tail_count != lpLength(ql->tail->entry)) {
        yell("quicklist tail count wrong: expected %d, "
             "got cached %u vs. actual %lu",
             tail_count, ql->tail->count, lpLength(ql->tail->entry));
        errors++;
    }

    errors += _ql_verify_compress(ql);
    return errors;
}

/* Release iterator and verify compress correctly. */
static void ql_release_iterator(quicklistIter *iter) {
    quicklist *ql = NULL;
    if (iter) ql = iter->quicklist;
    quicklistReleaseIterator(iter);
    if (ql) assert(!_ql_verify_compress(ql));
}

/* Generate new string concatenating integer i against string 'prefix' */
static char *genstr(char *prefix, int i) {
    static char result[64] = {0};
    snprintf(result, sizeof(result), "%s%d", prefix, i);
    return result;
}

static void randstring(unsigned char *target, size_t sz) {
    size_t p = 0;
    int minval, maxval;
    switch(rand() % 3) {
    case 0:
        minval = 'a';
        maxval = 'z';
    break;
    case 1:
        minval = '0';
        maxval = '9';
    break;
    case 2:
        minval = 'A';
        maxval = 'Z';
    break;
    default:
        assert(NULL);
    }

    while(p < sz)
        target[p++] = minval+rand()%(maxval-minval+1);
}

/* main test, but callable from other files */
int quicklistTest(int argc, char *argv[], int flags) {
    UNUSED(argc);
    UNUSED(argv);

    int accurate = (flags & REDIS_TEST_ACCURATE);
    unsigned int err = 0;
    int optimize_start =
        -(int)(sizeof(optimization_level) / sizeof(*optimization_level));

    printf("Starting optimization offset at: %d\n", optimize_start);

    int options[] = {0, 1, 2, 3, 4, 5, 6, 10};
    int fills[] = {-5, -4, -3, -2, -1, 0,
                   1, 2, 32, 66, 128, 999};
    size_t option_count = sizeof(options) / sizeof(*options);
    int fill_count = (int)(sizeof(fills) / sizeof(*fills));
    long long runtime[option_count];

    for (int _i = 0; _i < (int)option_count; _i++) {
        printf("Testing Compression option %d\n", options[_i]);
        long long start = mstime();
        quicklistIter *iter;

        TEST("create list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("add to tail of empty list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushTail(ql, "hello", 6);
            /* 1 for head and 1 for tail because 1 node = head = tail */
            ql_verify(ql, 1, 1, 1, 1);
            quicklistRelease(ql);
        }

        TEST("add to head of empty list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushHead(ql, "hello", 6);
            /* 1 for head and 1 for tail because 1 node = head = tail */
            ql_verify(ql, 1, 1, 1, 1);
            quicklistRelease(ql);
        }

        TEST_DESC("add to tail 5x at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 5; i++)
                    quicklistPushTail(ql, genstr("hello", i), 32);
                if (ql->count != 5)
                    ERROR;
                if (fills[f] == 32)
                    ql_verify(ql, 1, 5, 5, 5);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("add to head 5x at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 5; i++)
                    quicklistPushHead(ql, genstr("hello", i), 32);
                if (ql->count != 5)
                    ERROR;
                if (fills[f] == 32)
                    ql_verify(ql, 1, 5, 5, 5);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("add to tail 500x at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushTail(ql, genstr("hello", i), 64);
                if (ql->count != 500)
                    ERROR;
                if (fills[f] == 32)
                    ql_verify(ql, 16, 500, 32, 20);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("add to head 500x at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushHead(ql, genstr("hello", i), 32);
                if (ql->count != 500)
                    ERROR;
                if (fills[f] == 32)
                    ql_verify(ql, 16, 500, 20, 32);
                quicklistRelease(ql);
            }
        }

        TEST("rotate empty") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistRotate(ql);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("Comprassion Plain node") {
            char buf[256];
            quicklistisSetPackedThreshold(1);
            quicklist *ql = quicklistNew(-2, 1);
            for (int i = 0; i < 500; i++) {
                /* Set to 256 to allow the node to be triggered to compress,
                 * if it is less than 48(nocompress), the test will be successful. */
                snprintf(buf, sizeof(buf), "hello%d", i);
                quicklistPushHead(ql, buf, 256);
            }

            quicklistIter *iter = quicklistGetIterator(ql, AL_START_TAIL);
            quicklistEntry entry;
            int i = 0;
            while (quicklistNext(iter, &entry)) {
                snprintf(buf, sizeof(buf), "hello%d", i);
                if (strcmp((char *)entry.value, buf))
                    ERR("value [%s] didn't match [%s] at position %d",
                        entry.value, buf, i);
                i++;
            }
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("NEXT plain node")
        {
            packed_threshold = 3;
            quicklist *ql = quicklistNew(-2, options[_i]);
            char *strings[] = {"hello1", "hello2", "h3", "h4", "hello5"};

            for (int i = 0; i < 5; ++i)
                quicklistPushHead(ql, strings[i], strlen(strings[i]));

            quicklistEntry entry;
            quicklistIter *iter = quicklistGetIterator(ql, AL_START_TAIL);
            int j = 0;

            while(quicklistNext(iter, &entry) != 0) {
                assert(strncmp(strings[j], (char *)entry.value, strlen(strings[j])) == 0);
                j++;
            }
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("rotate plain node ") {
            unsigned char *data = NULL;
            size_t sz;
            long long lv;
            int i =0;
            packed_threshold = 5;
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushHead(ql, "hello1", 6);
            quicklistPushHead(ql, "hello4", 6);
            quicklistPushHead(ql, "hello3", 6);
            quicklistPushHead(ql, "hello2", 6);
            quicklistRotate(ql);

            for(i = 1 ; i < 5; i++) {
                quicklistPop(ql, QUICKLIST_HEAD, &data, &sz, &lv);
                int temp_char = data[5];
                zfree(data);
                assert(temp_char == ('0' + i));
            }

            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
            packed_threshold = (1 << 30);
        }

        TEST("rotate one val once") {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                quicklistPushHead(ql, "hello", 6);
                quicklistRotate(ql);
                /* Ignore compression verify because listpack is
                 * too small to compress. */
                ql_verify(ql, 1, 1, 1, 1);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("rotate 500 val 5000 times at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                quicklistPushHead(ql, "900", 3);
                quicklistPushHead(ql, "7000", 4);
                quicklistPushHead(ql, "-1200", 5);
                quicklistPushHead(ql, "42", 2);
                for (int i = 0; i < 500; i++)
                    quicklistPushHead(ql, genstr("hello", i), 64);
                ql_info(ql);
                for (int i = 0; i < 5000; i++) {
                    ql_info(ql);
                    quicklistRotate(ql);
                }
                if (fills[f] == 1)
                    ql_verify(ql, 504, 504, 1, 1);
                else if (fills[f] == 2)
                    ql_verify(ql, 252, 504, 2, 2);
                else if (fills[f] == 32)
                    ql_verify(ql, 16, 504, 32, 24);
                quicklistRelease(ql);
            }
        }

        TEST("pop empty") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPop(ql, QUICKLIST_HEAD, NULL, NULL, NULL);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("pop 1 string from 1") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            char *populate = genstr("hello", 331);
            quicklistPushHead(ql, populate, 32);
            unsigned char *data;
            size_t sz;
            long long lv;
            ql_info(ql);
            assert(quicklistPop(ql, QUICKLIST_HEAD, &data, &sz, &lv));
            assert(data != NULL);
            assert(sz == 32);
            if (strcmp(populate, (char *)data)) {
                int size = sz;
                ERR("Pop'd value (%.*s) didn't equal original value (%s)", size,
                    data, populate);
            }
            zfree(data);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("pop head 1 number from 1") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushHead(ql, "55513", 5);
            unsigned char *data;
            size_t sz;
            long long lv;
            ql_info(ql);
            assert(quicklistPop(ql, QUICKLIST_HEAD, &data, &sz, &lv));
            assert(data == NULL);
            assert(lv == 55513);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("pop head 500 from 500") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            for (int i = 0; i < 500; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            ql_info(ql);
            for (int i = 0; i < 500; i++) {
                unsigned char *data;
                size_t sz;
                long long lv;
                int ret = quicklistPop(ql, QUICKLIST_HEAD, &data, &sz, &lv);
                assert(ret == 1);
                assert(data != NULL);
                assert(sz == 32);
                if (strcmp(genstr("hello", 499 - i), (char *)data)) {
                    int size = sz;
                    ERR("Pop'd value (%.*s) didn't equal original value (%s)",
                        size, data, genstr("hello", 499 - i));
                }
                zfree(data);
            }
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("pop head 5000 from 500") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            for (int i = 0; i < 500; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            for (int i = 0; i < 5000; i++) {
                unsigned char *data;
                size_t sz;
                long long lv;
                int ret = quicklistPop(ql, QUICKLIST_HEAD, &data, &sz, &lv);
                if (i < 500) {
                    assert(ret == 1);
                    assert(data != NULL);
                    assert(sz == 32);
                    if (strcmp(genstr("hello", 499 - i), (char *)data)) {
                        int size = sz;
                        ERR("Pop'd value (%.*s) didn't equal original value "
                            "(%s)",
                            size, data, genstr("hello", 499 - i));
                    }
                    zfree(data);
                } else {
                    assert(ret == 0);
                }
            }
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("iterate forward over 500 list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            quicklistIter *iter = quicklistGetIterator(ql, AL_START_HEAD);
            quicklistEntry entry;
            int i = 499, count = 0;
            while (quicklistNext(iter, &entry)) {
                char *h = genstr("hello", i);
                if (strcmp((char *)entry.value, h))
                    ERR("value [%s] didn't match [%s] at position %d",
                        entry.value, h, i);
                i--;
                count++;
            }
            if (count != 500)
                ERR("Didn't iterate over exactly 500 elements (%d)", i);
            ql_verify(ql, 16, 500, 20, 32);
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("iterate reverse over 500 list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            quicklistIter *iter = quicklistGetIterator(ql, AL_START_TAIL);
            quicklistEntry entry;
            int i = 0;
            while (quicklistNext(iter, &entry)) {
                char *h = genstr("hello", i);
                if (strcmp((char *)entry.value, h))
                    ERR("value [%s] didn't match [%s] at position %d",
                        entry.value, h, i);
                i++;
            }
            if (i != 500)
                ERR("Didn't iterate over exactly 500 elements (%d)", i);
            ql_verify(ql, 16, 500, 20, 32);
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("insert after 1 element") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushHead(ql, "hello", 6);
            quicklistEntry entry;
            iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
            quicklistInsertAfter(iter, &entry, "abc", 4);
            ql_release_iterator(iter);
            ql_verify(ql, 1, 2, 2, 2);

            /* verify results */
            iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
            int sz = entry.sz;
            if (strncmp((char *)entry.value, "hello", 5)) {
                ERR("Value 0 didn't match, instead got: %.*s", sz,
                    entry.value);
            }
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, 1, &entry);
            sz = entry.sz;
            if (strncmp((char *)entry.value, "abc", 3)) {
                ERR("Value 1 didn't match, instead got: %.*s", sz,
                    entry.value);
            }
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("insert before 1 element") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushHead(ql, "hello", 6);
            quicklistEntry entry;
            iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
            quicklistInsertBefore(iter, &entry, "abc", 4);
            ql_release_iterator(iter);
            ql_verify(ql, 1, 2, 2, 2);

            /* verify results */
            iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
            int sz = entry.sz;
            if (strncmp((char *)entry.value, "abc", 3)) {
                ERR("Value 0 didn't match, instead got: %.*s", sz,
                    entry.value);
            }
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, 1, &entry);
            sz = entry.sz;
            if (strncmp((char *)entry.value, "hello", 5)) {
                ERR("Value 1 didn't match, instead got: %.*s", sz,
                    entry.value);
            }
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("insert head while head node is full") {
            quicklist *ql = quicklistNew(4, options[_i]);
            for (int i = 0; i < 10; i++)
                quicklistPushTail(ql, genstr("hello", i), 6);
            quicklistSetFill(ql, -1);
            quicklistEntry entry;
            iter = quicklistGetIteratorEntryAtIdx(ql, -10, &entry);
            char buf[4096] = {0};
            quicklistInsertBefore(iter, &entry, buf, 4096);
            ql_release_iterator(iter);
            ql_verify(ql, 4, 11, 1, 2);
            quicklistRelease(ql);
        }

        TEST("insert tail while tail node is full") {
            quicklist *ql = quicklistNew(4, options[_i]);
            for (int i = 0; i < 10; i++)
                quicklistPushHead(ql, genstr("hello", i), 6);
            quicklistSetFill(ql, -1);
            quicklistEntry entry;
            iter = quicklistGetIteratorEntryAtIdx(ql, -1, &entry);
            char buf[4096] = {0};
            quicklistInsertAfter(iter, &entry, buf, 4096);
            ql_release_iterator(iter);
            ql_verify(ql, 4, 11, 2, 1);
            quicklistRelease(ql);
        }

        TEST_DESC("insert once in elements while iterating at compress %d",
                  options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                quicklistPushTail(ql, "abc", 3);
                quicklistSetFill(ql, 1);
                quicklistPushTail(ql, "def", 3); /* force to unique node */
                quicklistSetFill(ql, f);
                quicklistPushTail(ql, "bob", 3); /* force to reset for +3 */
                quicklistPushTail(ql, "foo", 3);
                quicklistPushTail(ql, "zoo", 3);

                itrprintr(ql, 0);
                /* insert "bar" before "bob" while iterating over list. */
                quicklistIter *iter = quicklistGetIterator(ql, AL_START_HEAD);
                quicklistEntry entry;
                while (quicklistNext(iter, &entry)) {
                    if (!strncmp((char *)entry.value, "bob", 3)) {
                        /* Insert as fill = 1 so it spills into new node. */
                        quicklistInsertBefore(iter, &entry, "bar", 3);
                        break; /* didn't we fix insert-while-iterating? */
                    }
                }
                ql_release_iterator(iter);
                itrprintr(ql, 0);

                /* verify results */
                iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
                int sz = entry.sz;

                if (strncmp((char *)entry.value, "abc", 3))
                    ERR("Value 0 didn't match, instead got: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, 1, &entry);
                if (strncmp((char *)entry.value, "def", 3))
                    ERR("Value 1 didn't match, instead got: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, 2, &entry);
                if (strncmp((char *)entry.value, "bar", 3))
                    ERR("Value 2 didn't match, instead got: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, 3, &entry);
                if (strncmp((char *)entry.value, "bob", 3))
                    ERR("Value 3 didn't match, instead got: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, 4, &entry);
                if (strncmp((char *)entry.value, "foo", 3))
                    ERR("Value 4 didn't match, instead got: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, 5, &entry);
                if (strncmp((char *)entry.value, "zoo", 3))
                    ERR("Value 5 didn't match, instead got: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("insert [before] 250 new in middle of 500 elements at compress %d",
                  options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushTail(ql, genstr("hello", i), 32);
                for (int i = 0; i < 250; i++) {
                    quicklistEntry entry;
                    iter = quicklistGetIteratorEntryAtIdx(ql, 250, &entry);
                    quicklistInsertBefore(iter, &entry, genstr("abc", i), 32);
                    ql_release_iterator(iter);
                }
                if (fills[f] == 32)
                    ql_verify(ql, 25, 750, 32, 20);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("insert [after] 250 new in middle of 500 elements at compress %d",
                  options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushHead(ql, genstr("hello", i), 32);
                for (int i = 0; i < 250; i++) {
                    quicklistEntry entry;
                    iter = quicklistGetIteratorEntryAtIdx(ql, 250, &entry);
                    quicklistInsertAfter(iter, &entry, genstr("abc", i), 32);
                    ql_release_iterator(iter);
                }

                if (ql->count != 750)
                    ERR("List size not 750, but rather %ld", ql->count);

                if (fills[f] == 32)
                    ql_verify(ql, 26, 750, 20, 32);
                quicklistRelease(ql);
            }
        }

        TEST("duplicate empty list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            ql_verify(ql, 0, 0, 0, 0);
            quicklist *copy = quicklistDup(ql);
            ql_verify(copy, 0, 0, 0, 0);
            quicklistRelease(ql);
            quicklistRelease(copy);
        }

        TEST("duplicate list of 1 element") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushHead(ql, genstr("hello", 3), 32);
            ql_verify(ql, 1, 1, 1, 1);
            quicklist *copy = quicklistDup(ql);
            ql_verify(copy, 1, 1, 1, 1);
            quicklistRelease(ql);
            quicklistRelease(copy);
        }

        TEST("duplicate list of 500") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            ql_verify(ql, 16, 500, 20, 32);

            quicklist *copy = quicklistDup(ql);
            ql_verify(copy, 16, 500, 20, 32);
            quicklistRelease(ql);
            quicklistRelease(copy);
        }

        for (int f = 0; f < fill_count; f++) {
            TEST_DESC("index 1,200 from 500 list at fill %d at compress %d", f,
                      options[_i]) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushTail(ql, genstr("hello", i + 1), 32);
                quicklistEntry entry;
                iter = quicklistGetIteratorEntryAtIdx(ql, 1, &entry);
                if (strcmp((char *)entry.value, "hello2") != 0)
                    ERR("Value: %s", entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, 200, &entry);
                if (strcmp((char *)entry.value, "hello201") != 0)
                    ERR("Value: %s", entry.value);
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }

            TEST_DESC("index -1,-2 from 500 list at fill %d at compress %d",
                      fills[f], options[_i]) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushTail(ql, genstr("hello", i + 1), 32);
                quicklistEntry entry;
                iter = quicklistGetIteratorEntryAtIdx(ql, -1, &entry);
                if (strcmp((char *)entry.value, "hello500") != 0)
                    ERR("Value: %s", entry.value);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, -2, &entry);
                if (strcmp((char *)entry.value, "hello499") != 0)
                    ERR("Value: %s", entry.value);
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }

            TEST_DESC("index -100 from 500 list at fill %d at compress %d",
                      fills[f], options[_i]) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 500; i++)
                    quicklistPushTail(ql, genstr("hello", i + 1), 32);
                quicklistEntry entry;
                iter = quicklistGetIteratorEntryAtIdx(ql, -100, &entry);
                if (strcmp((char *)entry.value, "hello401") != 0)
                    ERR("Value: %s", entry.value);
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }

            TEST_DESC("index too big +1 from 50 list at fill %d at compress %d",
                      fills[f], options[_i]) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                for (int i = 0; i < 50; i++)
                    quicklistPushTail(ql, genstr("hello", i + 1), 32);
                quicklistEntry entry;
                int sz = entry.sz;
                iter = quicklistGetIteratorEntryAtIdx(ql, 50, &entry);
                if (iter)
                    ERR("Index found at 50 with 50 list: %.*s", sz,
                        entry.value);
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }
        }

        TEST("delete range empty list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistDelRange(ql, 5, 20);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("delete range of entire node in list of one node") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            for (int i = 0; i < 32; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            ql_verify(ql, 1, 32, 32, 32);
            quicklistDelRange(ql, 0, 32);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("delete range of entire node with overflow counts") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            for (int i = 0; i < 32; i++)
                quicklistPushHead(ql, genstr("hello", i), 32);
            ql_verify(ql, 1, 32, 32, 32);
            quicklistDelRange(ql, 0, 128);
            ql_verify(ql, 0, 0, 0, 0);
            quicklistRelease(ql);
        }

        TEST("delete middle 100 of 500 list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushTail(ql, genstr("hello", i + 1), 32);
            ql_verify(ql, 16, 500, 32, 20);
            quicklistDelRange(ql, 200, 100);
            ql_verify(ql, 14, 400, 32, 20);
            quicklistRelease(ql);
        }

        TEST("delete less than fill but across nodes") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushTail(ql, genstr("hello", i + 1), 32);
            ql_verify(ql, 16, 500, 32, 20);
            quicklistDelRange(ql, 60, 10);
            ql_verify(ql, 16, 490, 32, 20);
            quicklistRelease(ql);
        }

        TEST("delete negative 1 from 500 list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushTail(ql, genstr("hello", i + 1), 32);
            ql_verify(ql, 16, 500, 32, 20);
            quicklistDelRange(ql, -1, 1);
            ql_verify(ql, 16, 499, 32, 19);
            quicklistRelease(ql);
        }

        TEST("delete negative 1 from 500 list with overflow counts") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushTail(ql, genstr("hello", i + 1), 32);
            ql_verify(ql, 16, 500, 32, 20);
            quicklistDelRange(ql, -1, 128);
            ql_verify(ql, 16, 499, 32, 19);
            quicklistRelease(ql);
        }

        TEST("delete negative 100 from 500 list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 500; i++)
                quicklistPushTail(ql, genstr("hello", i + 1), 32);
            quicklistDelRange(ql, -100, 100);
            ql_verify(ql, 13, 400, 32, 16);
            quicklistRelease(ql);
        }

        TEST("delete -10 count 5 from 50 list") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            for (int i = 0; i < 50; i++)
                quicklistPushTail(ql, genstr("hello", i + 1), 32);
            ql_verify(ql, 2, 50, 32, 18);
            quicklistDelRange(ql, -10, 5);
            ql_verify(ql, 2, 45, 32, 13);
            quicklistRelease(ql);
        }

        TEST("numbers only list read") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushTail(ql, "1111", 4);
            quicklistPushTail(ql, "2222", 4);
            quicklistPushTail(ql, "3333", 4);
            quicklistPushTail(ql, "4444", 4);
            ql_verify(ql, 1, 4, 4, 4);
            quicklistEntry entry;
            iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
            if (entry.longval != 1111)
                ERR("Not 1111, %lld", entry.longval);
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, 1, &entry);
            if (entry.longval != 2222)
                ERR("Not 2222, %lld", entry.longval);
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, 2, &entry);
            if (entry.longval != 3333)
                ERR("Not 3333, %lld", entry.longval);
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, 3, &entry);
            if (entry.longval != 4444)
                ERR("Not 4444, %lld", entry.longval);
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, 4, &entry);
            if (iter)
                ERR("Index past elements: %lld", entry.longval);
            ql_release_iterator(iter);
            
            iter = quicklistGetIteratorEntryAtIdx(ql, -1, &entry);
            if (entry.longval != 4444)
                ERR("Not 4444 (reverse), %lld", entry.longval);
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, -2, &entry);
            if (entry.longval != 3333)
                ERR("Not 3333 (reverse), %lld", entry.longval);
            ql_release_iterator(iter);

            iter = quicklistGetIteratorEntryAtIdx(ql, -3, &entry);
            if (entry.longval != 2222)
                ERR("Not 2222 (reverse), %lld", entry.longval);
            ql_release_iterator(iter);
            
            iter = quicklistGetIteratorEntryAtIdx(ql, -4, &entry);
            if (entry.longval != 1111)
                ERR("Not 1111 (reverse), %lld", entry.longval);
            ql_release_iterator(iter);
            
            iter = quicklistGetIteratorEntryAtIdx(ql, -5, &entry);
            if (iter)
                ERR("Index past elements (reverse), %lld", entry.longval);
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("numbers larger list read") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistSetFill(ql, 32);
            char num[32];
            long long nums[5000];
            for (int i = 0; i < 5000; i++) {
                nums[i] = -5157318210846258176 + i;
                int sz = ll2string(num, sizeof(num), nums[i]);
                quicklistPushTail(ql, num, sz);
            }
            quicklistPushTail(ql, "xxxxxxxxxxxxxxxxxxxx", 20);
            quicklistEntry entry;
            for (int i = 0; i < 5000; i++) {
                iter = quicklistGetIteratorEntryAtIdx(ql, i, &entry);
                if (entry.longval != nums[i])
                    ERR("[%d] Not longval %lld but rather %lld", i, nums[i],
                        entry.longval);
                entry.longval = 0xdeadbeef;
                ql_release_iterator(iter);
            }
            iter = quicklistGetIteratorEntryAtIdx(ql, 5000, &entry);
            if (strncmp((char *)entry.value, "xxxxxxxxxxxxxxxxxxxx", 20))
                ERR("String val not match: %s", entry.value);
            ql_verify(ql, 157, 5001, 32, 9);
            ql_release_iterator(iter);
            quicklistRelease(ql);
        }

        TEST("numbers larger list read B") {
            quicklist *ql = quicklistNew(-2, options[_i]);
            quicklistPushTail(ql, "99", 2);
            quicklistPushTail(ql, "98", 2);
            quicklistPushTail(ql, "xxxxxxxxxxxxxxxxxxxx", 20);
            quicklistPushTail(ql, "96", 2);
            quicklistPushTail(ql, "95", 2);
            quicklistReplaceAtIndex(ql, 1, "foo", 3);
            quicklistReplaceAtIndex(ql, -1, "bar", 3);
            quicklistRelease(ql);
        }

        TEST_DESC("lrem test at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                char *words[] = {"abc", "foo", "bar",  "foobar", "foobared",
                                 "zap", "bar", "test", "foo"};
                char *result[] = {"abc", "foo",  "foobar", "foobared",
                                  "zap", "test", "foo"};
                char *resultB[] = {"abc",      "foo", "foobar",
                                   "foobared", "zap", "test"};
                for (int i = 0; i < 9; i++)
                    quicklistPushTail(ql, words[i], strlen(words[i]));

                /* lrem 0 bar */
                quicklistIter *iter = quicklistGetIterator(ql, AL_START_HEAD);
                quicklistEntry entry;
                int i = 0;
                while (quicklistNext(iter, &entry)) {
                    if (quicklistCompare(&entry, (unsigned char *)"bar", 3)) {
                        quicklistDelEntry(iter, &entry);
                    }
                    i++;
                }
                ql_release_iterator(iter);

                /* check result of lrem 0 bar */
                iter = quicklistGetIterator(ql, AL_START_HEAD);
                i = 0;
                while (quicklistNext(iter, &entry)) {
                    /* Result must be: abc, foo, foobar, foobared, zap, test,
                     * foo */
                    int sz = entry.sz;
                    if (strncmp((char *)entry.value, result[i], entry.sz)) {
                        ERR("No match at position %d, got %.*s instead of %s",
                            i, sz, entry.value, result[i]);
                    }
                    i++;
                }
                ql_release_iterator(iter);

                quicklistPushTail(ql, "foo", 3);

                /* lrem -2 foo */
                iter = quicklistGetIterator(ql, AL_START_TAIL);
                i = 0;
                int del = 2;
                while (quicklistNext(iter, &entry)) {
                    if (quicklistCompare(&entry, (unsigned char *)"foo", 3)) {
                        quicklistDelEntry(iter, &entry);
                        del--;
                    }
                    if (!del)
                        break;
                    i++;
                }
                ql_release_iterator(iter);

                /* check result of lrem -2 foo */
                /* (we're ignoring the '2' part and still deleting all foo
                 * because
                 * we only have two foo) */
                iter = quicklistGetIterator(ql, AL_START_TAIL);
                i = 0;
                size_t resB = sizeof(resultB) / sizeof(*resultB);
                while (quicklistNext(iter, &entry)) {
                    /* Result must be: abc, foo, foobar, foobared, zap, test,
                     * foo */
                    int sz = entry.sz;
                    if (strncmp((char *)entry.value, resultB[resB - 1 - i],
                                sz)) {
                        ERR("No match at position %d, got %.*s instead of %s",
                            i, sz, entry.value, resultB[resB - 1 - i]);
                    }
                    i++;
                }

                ql_release_iterator(iter);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("iterate reverse + delete at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                quicklistPushTail(ql, "abc", 3);
                quicklistPushTail(ql, "def", 3);
                quicklistPushTail(ql, "hij", 3);
                quicklistPushTail(ql, "jkl", 3);
                quicklistPushTail(ql, "oop", 3);

                quicklistEntry entry;
                quicklistIter *iter = quicklistGetIterator(ql, AL_START_TAIL);
                int i = 0;
                while (quicklistNext(iter, &entry)) {
                    if (quicklistCompare(&entry, (unsigned char *)"hij", 3)) {
                        quicklistDelEntry(iter, &entry);
                    }
                    i++;
                }
                ql_release_iterator(iter);

                if (i != 5)
                    ERR("Didn't iterate 5 times, iterated %d times.", i);

                /* Check results after deletion of "hij" */
                iter = quicklistGetIterator(ql, AL_START_HEAD);
                i = 0;
                char *vals[] = {"abc", "def", "jkl", "oop"};
                while (quicklistNext(iter, &entry)) {
                    if (!quicklistCompare(&entry, (unsigned char *)vals[i],
                                          3)) {
                        ERR("Value at %d didn't match %s\n", i, vals[i]);
                    }
                    i++;
                }
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("iterator at index test at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                char num[32];
                long long nums[5000];
                for (int i = 0; i < 760; i++) {
                    nums[i] = -5157318210846258176 + i;
                    int sz = ll2string(num, sizeof(num), nums[i]);
                    quicklistPushTail(ql, num, sz);
                }

                quicklistEntry entry;
                quicklistIter *iter =
                    quicklistGetIteratorAtIdx(ql, AL_START_HEAD, 437);
                int i = 437;
                while (quicklistNext(iter, &entry)) {
                    if (entry.longval != nums[i])
                        ERR("Expected %lld, but got %lld", entry.longval,
                            nums[i]);
                    i++;
                }
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("ltrim test A at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                char num[32];
                long long nums[5000];
                for (int i = 0; i < 32; i++) {
                    nums[i] = -5157318210846258176 + i;
                    int sz = ll2string(num, sizeof(num), nums[i]);
                    quicklistPushTail(ql, num, sz);
                }
                if (fills[f] == 32)
                    ql_verify(ql, 1, 32, 32, 32);
                /* ltrim 25 53 (keep [25,32] inclusive = 7 remaining) */
                quicklistDelRange(ql, 0, 25);
                quicklistDelRange(ql, 0, 0);
                quicklistEntry entry;
                for (int i = 0; i < 7; i++) {
                    iter = quicklistGetIteratorEntryAtIdx(ql, i, &entry);
                    if (entry.longval != nums[25 + i])
                        ERR("Deleted invalid range!  Expected %lld but got "
                            "%lld",
                            entry.longval, nums[25 + i]);
                    ql_release_iterator(iter);
                }
                if (fills[f] == 32)
                    ql_verify(ql, 1, 7, 7, 7);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("ltrim test B at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                /* Force-disable compression because our 33 sequential
                 * integers don't compress and the check always fails. */
                quicklist *ql = quicklistNew(fills[f], QUICKLIST_NOCOMPRESS);
                char num[32];
                long long nums[5000];
                for (int i = 0; i < 33; i++) {
                    nums[i] = i;
                    int sz = ll2string(num, sizeof(num), nums[i]);
                    quicklistPushTail(ql, num, sz);
                }
                if (fills[f] == 32)
                    ql_verify(ql, 2, 33, 32, 1);
                /* ltrim 5 16 (keep [5,16] inclusive = 12 remaining) */
                quicklistDelRange(ql, 0, 5);
                quicklistDelRange(ql, -16, 16);
                if (fills[f] == 32)
                    ql_verify(ql, 1, 12, 12, 12);
                quicklistEntry entry;

                iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
                if (entry.longval != 5)
                    ERR("A: longval not 5, but %lld", entry.longval);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, -1, &entry);
                if (entry.longval != 16)
                    ERR("B! got instead: %lld", entry.longval);
                quicklistPushTail(ql, "bobobob", 7);
                ql_release_iterator(iter);

                iter = quicklistGetIteratorEntryAtIdx(ql, -1, &entry);
                int sz = entry.sz;
                if (strncmp((char *)entry.value, "bobobob", 7))
                    ERR("Tail doesn't match bobobob, it's %.*s instead",
                        sz, entry.value);
                ql_release_iterator(iter);

                for (int i = 0; i < 12; i++) {
                    iter = quicklistGetIteratorEntryAtIdx(ql, i, &entry);
                    if (entry.longval != nums[5 + i])
                        ERR("Deleted invalid range!  Expected %lld but got "
                            "%lld",
                            entry.longval, nums[5 + i]);
                    ql_release_iterator(iter);
                }
                quicklistRelease(ql);
            }
        }

        TEST_DESC("ltrim test C at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                char num[32];
                long long nums[5000];
                for (int i = 0; i < 33; i++) {
                    nums[i] = -5157318210846258176 + i;
                    int sz = ll2string(num, sizeof(num), nums[i]);
                    quicklistPushTail(ql, num, sz);
                }
                if (fills[f] == 32)
                    ql_verify(ql, 2, 33, 32, 1);
                /* ltrim 3 3 (keep [3,3] inclusive = 1 remaining) */
                quicklistDelRange(ql, 0, 3);
                quicklistDelRange(ql, -29,
                                  4000); /* make sure not loop forever */
                if (fills[f] == 32)
                    ql_verify(ql, 1, 1, 1, 1);
                quicklistEntry entry;
                iter = quicklistGetIteratorEntryAtIdx(ql, 0, &entry);
                if (entry.longval != -5157318210846258173)
                    ERROR;
                ql_release_iterator(iter);
                quicklistRelease(ql);
            }
        }

        TEST_DESC("ltrim test D at compress %d", options[_i]) {
            for (int f = 0; f < fill_count; f++) {
                quicklist *ql = quicklistNew(fills[f], options[_i]);
                char num[32];
                long long nums[5000];
                for (int i = 0; i < 33; i++) {
                    nums[i] = -5157318210846258176 + i;
                    int sz = ll2string(num, sizeof(num), nums[i]);
                    quicklistPushTail(ql, num, sz);
                }
                if (fills[f] == 32)
                    ql_verify(ql, 2, 33, 32, 1);
                quicklistDelRange(ql, -12, 3);
                if (ql->count != 30)
                    ERR("Didn't delete exactly three elements!  Count is: %lu",
                        ql->count);
                quicklistRelease(ql);
            }
        }

        long long stop = mstime();
        runtime[_i] = stop - start;
    }

    /* Run a longer test of compression depth outside of primary test loop. */
    int list_sizes[] = {250, 251, 500, 999, 1000};
    long long start = mstime();
    int list_count = accurate ? (int)(sizeof(list_sizes) / sizeof(*list_sizes)) : 1;
    for (int list = 0; list < list_count; list++) {
        TEST_DESC("verify specific compression of interior nodes with %d list ",
                  list_sizes[list]) {
            for (int f = 0; f < fill_count; f++) {
                for (int depth = 1; depth < 40; depth++) {
                    /* skip over many redundant test cases */
                    quicklist *ql = quicklistNew(fills[f], depth);
                    for (int i = 0; i < list_sizes[list]; i++) {
                        quicklistPushTail(ql, genstr("hello TAIL", i + 1), 64);
                        quicklistPushHead(ql, genstr("hello HEAD", i + 1), 64);
                    }

                    for (int step = 0; step < 2; step++) {
                        /* test remove node */
                        if (step == 1) {
                            for (int i = 0; i < list_sizes[list] / 2; i++) {
                                unsigned char *data;
                                assert(quicklistPop(ql, QUICKLIST_HEAD, &data,
                                                    NULL, NULL));
                                zfree(data);
                                assert(quicklistPop(ql, QUICKLIST_TAIL, &data,
                                                    NULL, NULL));
                                zfree(data);
                            }
                        }
                        quicklistNode *node = ql->head;
                        unsigned int low_raw = ql->compress;
                        unsigned int high_raw = ql->len - ql->compress;

                        for (unsigned int at = 0; at < ql->len;
                            at++, node = node->next) {
                            if (at < low_raw || at >= high_raw) {
                                if (node->encoding != QUICKLIST_NODE_ENCODING_RAW) {
                                    ERR("Incorrect compression: node %d is "
                                        "compressed at depth %d ((%u, %u); total "
                                        "nodes: %lu; size: %zu)",
                                        at, depth, low_raw, high_raw, ql->len,
                                        node->sz);
                                }
                            } else {
                                if (node->encoding != QUICKLIST_NODE_ENCODING_LZF) {
                                    ERR("Incorrect non-compression: node %d is NOT "
                                        "compressed at depth %d ((%u, %u); total "
                                        "nodes: %lu; size: %zu; attempted: %d)",
                                        at, depth, low_raw, high_raw, ql->len,
                                        node->sz, node->attempted_compress);
                                }
                            }
                        }
                    }

                    quicklistRelease(ql);
                }
            }
        }
    }
    long long stop = mstime();

    printf("\n");
    for (size_t i = 0; i < option_count; i++)
        printf("Test Loop %02d: %0.2f seconds.\n", options[i],
               (float)runtime[i] / 1000);
    printf("Compressions: %0.2f seconds.\n", (float)(stop - start) / 1000);
    printf("\n");

    TEST("bookmark get updated to next item") {
        quicklist *ql = quicklistNew(1, 0);
        quicklistPushTail(ql, "1", 1);
        quicklistPushTail(ql, "2", 1);
        quicklistPushTail(ql, "3", 1);
        quicklistPushTail(ql, "4", 1);
        quicklistPushTail(ql, "5", 1);
        assert(ql->len==5);
        /* add two bookmarks, one pointing to the node before the last. */
        assert(quicklistBookmarkCreate(&ql, "_dummy", ql->head->next));
        assert(quicklistBookmarkCreate(&ql, "_test", ql->tail->prev));
        /* test that the bookmark returns the right node, delete it and see that the bookmark points to the last node */
        assert(quicklistBookmarkFind(ql, "_test") == ql->tail->prev);
        assert(quicklistDelRange(ql, -2, 1));
        assert(quicklistBookmarkFind(ql, "_test") == ql->tail);
        /* delete the last node, and see that the bookmark was deleted. */
        assert(quicklistDelRange(ql, -1, 1));
        assert(quicklistBookmarkFind(ql, "_test") == NULL);
        /* test that other bookmarks aren't affected */
        assert(quicklistBookmarkFind(ql, "_dummy") == ql->head->next);
        assert(quicklistBookmarkFind(ql, "_missing") == NULL);
        assert(ql->len==3);
        quicklistBookmarksClear(ql); /* for coverage */
        assert(quicklistBookmarkFind(ql, "_dummy") == NULL);
        quicklistRelease(ql);
    }

    TEST("bookmark limit") {
        int i;
        quicklist *ql = quicklistNew(1, 0);
        quicklistPushHead(ql, "1", 1);
        for (i=0; i<QL_MAX_BM; i++)
            assert(quicklistBookmarkCreate(&ql, genstr("",i), ql->head));
        /* when all bookmarks are used, creation fails */
        assert(!quicklistBookmarkCreate(&ql, "_test", ql->head));
        /* delete one and see that we can now create another */
        assert(quicklistBookmarkDelete(ql, "0"));
        assert(quicklistBookmarkCreate(&ql, "_test", ql->head));
        /* delete one and see that the rest survive */
        assert(quicklistBookmarkDelete(ql, "_test"));
        for (i=1; i<QL_MAX_BM; i++)
            assert(quicklistBookmarkFind(ql, genstr("",i)) == ql->head);
        /* make sure the deleted ones are indeed gone */
        assert(!quicklistBookmarkFind(ql, "0"));
        assert(!quicklistBookmarkFind(ql, "_test"));
        quicklistRelease(ql);
    }

    if (flags & REDIS_TEST_LARGE_MEMORY) {
        TEST("compress and decompress quicklist listpack node") {
            quicklistNode *node = quicklistCreateNode();
            node->entry = lpNew(0);

            /* Just to avoid triggering the assertion in __quicklistCompressNode(),
             * it disables the passing of quicklist head or tail node. */
            node->prev = quicklistCreateNode();
            node->next = quicklistCreateNode();
            
            /* Create a rand string */
            size_t sz = (1 << 25); /* 32MB per one entry */
            unsigned char *s = zmalloc(sz);
            randstring(s, sz);

            /* Keep filling the node, until it reaches 1GB */
            for (int i = 0; i < 32; i++) {
                node->entry = lpAppend(node->entry, s, sz);
                quicklistNodeUpdateSz(node);

                long long start = mstime();
                assert(__quicklistCompressNode(node));
                assert(__quicklistDecompressNode(node));
                printf("Compress and decompress: %zu MB in %.2f seconds.\n",
                       node->sz/1024/1024, (float)(mstime() - start) / 1000);
            }

            zfree(s);
            zfree(node->prev);
            zfree(node->next);
            zfree(node->entry);
            zfree(node);
        }

#if ULONG_MAX >= 0xffffffffffffffff
        TEST("compress and decomress quicklist plain node large than UINT32_MAX") {
            size_t sz = (1ull << 32);
            unsigned char *s = zmalloc(sz);
            randstring(s, sz);
            memcpy(s, "helloworld", 10);
            memcpy(s + sz - 10, "1234567890", 10);

            quicklistNode *node = __quicklistCreatePlainNode(s, sz);

            /* Just to avoid triggering the assertion in __quicklistCompressNode(),
             * it disables the passing of quicklist head or tail node. */
            node->prev = quicklistCreateNode();
            node->next = quicklistCreateNode();

            long long start = mstime();
            assert(__quicklistCompressNode(node));
            assert(__quicklistDecompressNode(node));
            printf("Compress and decompress: %zu MB in %.2f seconds.\n",
                   node->sz/1024/1024, (float)(mstime() - start) / 1000);

            assert(memcmp(node->entry, "helloworld", 10) == 0);
            assert(memcmp(node->entry + sz - 10, "1234567890", 10) == 0);
            zfree(node->prev);
            zfree(node->next);
            zfree(node->entry);
            zfree(node);
        }
#endif
    }

    if (!err)
        printf("ALL TESTS PASSED!\n");
    else
        ERR("Sorry, not all tests passed!  In fact, %d tests failed.", err);

    return err;
}
#endif
