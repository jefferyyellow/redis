/* Rax -- A radix tree implementation.
 *
 * Copyright (c) 2017-2018, Salvatore Sanfilippo <antirez at gmail dot com>
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

#ifndef RAX_H
#define RAX_H

#include <stdint.h>

/* Representation of a radix tree as implemented in this file, that contains
 * the strings "foo", "foobar" and "footer" after the insertion of each
 * word. When the node represents a key inside the radix tree, we write it
 * between [], otherwise it is written between ().
 *
 * This is the vanilla representation:
 *
 *              (f) ""
 *                \
 *                (o) "f"
 *                  \
 *                  (o) "fo"
 *                    \
 *                  [t   b] "foo"
 *                  /     \
 *         "foot" (e)     (a) "foob"
 *                /         \
 *      "foote" (r)         (r) "fooba"
 *              /             \
 *    "footer" []             [] "foobar"
 *
 * However, this implementation implements a very common optimization where
 * successive nodes having a single child are "compressed" into the node
 * itself as a string of characters, each representing a next-level child,
 * and only the link to the node representing the last character node is
 * provided inside the representation. So the above representation is turned
 * into:
 *
 *                  ["foo"] ""
 *                     |
 *                  [t   b] "foo"
 *                  /     \
 *        "foot" ("er")    ("ar") "foob"
 *                 /          \
 *       "footer" []          [] "foobar"
 *
 * However this optimization makes the implementation a bit more complex.
 * For instance if a key "first" is added in the above radix tree, a
 * "node splitting" operation is needed, since the "foo" prefix is no longer
 * composed of nodes having a single child one after the other. This is the
 * above tree and the resulting node splitting after this event happens:
 *
 *
 *                    (f) ""
 *                    /
 *                 (i o) "f"
 *                 /   \
 *    "firs"  ("rst")  (o) "fo"
 *              /        \
 *    "first" []       [t   b] "foo"
 *                     /     \
 *           "foot" ("er")    ("ar") "foob"
 *                    /          \
 *          "footer" []          [] "foobar"
 *
 * Similarly after deletion, if a new chain of nodes having a single child
 * is created (the chain must also not include nodes that represent keys),
 * it must be compressed back into a single node.
 *
 */

#define RAX_NODE_MAX_SIZE ((1<<29)-1)
// Rax树节点
typedef struct raxNode {
    // 当前的节点是否包含一个key
    uint32_t iskey:1;     /* Does this node contain a key? */
    // 当前的key对应的value是否为空
    uint32_t isnull:1;    /* Associated value is NULL (don't store it). */
    // 当前节点是否为压缩节点
    uint32_t iscompr:1;   /* Node is compressed. */
    // 压缩节点压缩的字符串长度或者非压缩节点的子节点个数
    uint32_t size:29;     /* Number of children, or compressed string len. */
    /* Data layout is as follows:
     * 数据层如下：
     * If node is not compressed we have 'size' bytes, one for each children
     * character, and 'size' raxNode pointers, point to each child node.
     * Note how the character is not stored in the children but in the
     * edge of the parents:
     * 如果节点没有被压缩，我们有“size”字节，每个子字符一个，“size”raxNode指针指向每个子节点。
     * 请注意，字符不是存储在子节点中，而是存储在父节点的边缘：
     * 
     * [header iscompr=0][abc][a-ptr][b-ptr][c-ptr](value-ptr?)
     * 
     * 
     * if node is compressed (iscompr bit is 1) the node has 1 children.
     * In that case the 'size' bytes of the string stored immediately at
     * the start of the data section, represent a sequence of successive
     * nodes linked one after the other, for which only the last one in
     * the sequence is actually represented as a node, and pointed to by
     * the current compressed node.
     * 如果节点被压缩（iscompr位置1），则该节点具有1个子节点。在这种情况下，
     * “size”字节的字符串直接存储在数据段开头，表示一系列一个接一个连接在一起的连续节点，
     * 其中只有序列中的最后一个节点实际表示为节点，并由当前压缩节点指向。
     * 
     * [header iscompr=1][xyz][z-ptr](value-ptr?)
     *
     * Both compressed and not compressed nodes can represent a key
     * with associated data in the radix tree at any level (not just terminal
     * nodes).
     * 不管是不是压缩节点都能表示radix树中任何层级的带有关联数据的键值
     * 
     * If the node has an associated key (iskey=1) and is not NULL
     * (isnull=0), then after the raxNode pointers pointing to the
     * children, an additional value pointer is present (as you can see
     * in the representation above as "value-ptr" field).
     * 如果节点有一个关联的键（iskey=1），并且不是NULL（isnul=0），那么在指向子节点的raxNode指针之后，
     * 会出现一个额外的值指针（正如您在上面的“value ptr”字段表示中所看到的那样）。
     */
    // 包含填充字段，同时存储了当前节点包含的字符串以及子节点的指针、key对应的value指针。
    unsigned char data[];
} raxNode;

// rax结构代表一个Rax树
typedef struct rax {
    // 头结点指针
    raxNode *head;
    // 元素个数(key的个数)
    uint64_t numele;
    // 节点个数
    uint64_t numnodes;
} rax;

/* Stack data structure used by raxLowWalk() in order to, optionally, return
 * a list of parent nodes to the caller. The nodes do not have a "parent"
 * field for space concerns, so we use the auxiliary stack when needed. */
#define RAX_STACK_STATIC_ITEMS 32
// raxStack结构用于存储从根节点到当前节点的路径
typedef struct raxStack {
    // 用于记录路径，该指针可能指向static_items(较短路径)，或者对空间内存
    void **stack; /* Points to static_items or an heap allocated array. */
    // stack指向的空间的已经使用的数量和最大数量
    size_t items, maxitems; /* Number of items contained and total space. */
    /* Up to RAXSTACK_STACK_ITEMS items we avoid to allocate on the heap
     * and use this static array of pointers instead. */
    // 数组中的每个元素都是指针，用于存储路径
    void *static_items[RAX_STACK_STATIC_ITEMS];
    // 当前栈是否出现过内存溢出
    int oom; /* True if pushing into this stack failed for OOM at some point. */
} raxStack;

/* Optional callback used for iterators and be notified on each rax node,
 * including nodes not representing keys. If the callback returns true
 * the callback changed the node pointer in the iterator structure, and the
 * iterator implementation will have to replace the pointer in the radix tree
 * internals. This allows the callback to reallocate the node to perform
 * very special operations, normally not needed by normal applications.
 *
 * This callback is used to perform very low level analysis of the radix tree
 * structure, scanning each possible node (but the root node), or in order to
 * reallocate the nodes to reduce the allocation fragmentation (this is the
 * Redis application for this callback).
 *
 * This is currently only supported in forward iterations (raxNext) */
typedef int (*raxNodeCallback)(raxNode **noderef);

/* Radix tree iterator state is encapsulated into this data structure. */
// Radix树迭代器状态被封装到这个数据结构中
#define RAX_ITER_STATIC_LEN 128
/* Iterator was just seeked. Return current element for the first iteration and clear the flag. */
// 迭代程序刚刚被找到。返回第一次迭代的当前元素并清除标志
#define RAX_ITER_JUST_SEEKED (1<<0)     
/* End of iteration reached. */
// 迭代器达到了末尾
#define RAX_ITER_EOF (1<<1) 
/* Safe iterator, allows operations while iterating. But it is slower. */
// 安全迭代器，允许在迭代时进行操作。但速度较慢
#define RAX_ITER_SAFE (1<<2)   
// 用于遍历Rax树中所有的key
typedef struct raxIterator {
    // 当前迭代器标志位，目前有3种：RAX_ITER_JUST_SEEEKED,RAX_ITER_EOF,RAX_ITER_SAFE
    int flags;
    // 遍历的rax树
    rax *rt;                /* Radix tree we are iterating. */
    // 当前的迭代器遍历到的键值，该指针指向key_static_string或者从堆内存中申请的内存。
    unsigned char *key;     /* The current string. */
    // 指向当前key关联的value值。
    void *data;             /* Data associated to this key. */
    // key指向的空间已经使用的长度
    size_t key_len;         /* Current key length. */
    // key指向的空间最大的长度
    size_t key_max;         /* Max key len the current key buffer can hold. */
    // 默认的存储空间，如果key比较大时，回动态从堆空间分配内存
    unsigned char key_static_string[RAX_ITER_STATIC_LEN];
    // 当前的rax节点
    raxNode *node;          /* Current node. Only for unsafe iteration. */
    // 栈记录了根节点到当前节点的路径，用于raxNode的向上遍历
    raxStack stack;         /* Stack used for unsafe iteration. */
    // 节点的回调函数，通常为空
    raxNodeCallback node_cb; /* Optional node callback. Normally set to NULL. */
} raxIterator;

/* A special pointer returned for not found items. */
extern void *raxNotFound;

/* Exported API. */
rax *raxNew(void);
int raxInsert(rax *rax, unsigned char *s, size_t len, void *data, void **old);
int raxTryInsert(rax *rax, unsigned char *s, size_t len, void *data, void **old);
int raxRemove(rax *rax, unsigned char *s, size_t len, void **old);
void *raxFind(rax *rax, unsigned char *s, size_t len);
void raxFree(rax *rax);
void raxFreeWithCallback(rax *rax, void (*free_callback)(void*));
void raxStart(raxIterator *it, rax *rt);
int raxSeek(raxIterator *it, const char *op, unsigned char *ele, size_t len);
int raxNext(raxIterator *it);
int raxPrev(raxIterator *it);
int raxRandomWalk(raxIterator *it, size_t steps);
int raxCompare(raxIterator *iter, const char *op, unsigned char *key, size_t key_len);
void raxStop(raxIterator *it);
int raxEOF(raxIterator *it);
void raxShow(rax *rax);
uint64_t raxSize(rax *rax);
unsigned long raxTouch(raxNode *n);
void raxSetDebugMsg(int onoff);

/* Internal API. May be used by the node callback in order to access rax nodes
 * in a low level way, so this function is exported as well. */
void raxSetData(raxNode *n, void *data);

#endif
