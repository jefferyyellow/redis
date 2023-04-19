/* Rax -- A radix tree implementation.
 *
 * Version 1.2 -- 7 February 2019
 *
 * Copyright (c) 2017-2019, Salvatore Sanfilippo <antirez at gmail dot com>
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include "rax.h"

#ifndef RAX_MALLOC_INCLUDE
#define RAX_MALLOC_INCLUDE "rax_malloc.h"
#endif

#include RAX_MALLOC_INCLUDE

/* This is a special pointer that is guaranteed to never have the same value
 * of a radix tree node. It's used in order to report "not found" error without
 * requiring the function to have multiple return values. */
void *raxNotFound = (void*)"rax-not-found-pointer";

/* -------------------------------- Debugging ------------------------------ */

void raxDebugShowNode(const char *msg, raxNode *n);

/* Turn debugging messages on/off by compiling with RAX_DEBUG_MSG macro on.
 * When RAX_DEBUG_MSG is defined by default Rax operations will emit a lot
 * of debugging info to the standard output, however you can still turn
 * debugging on/off in order to enable it only when you suspect there is an
 * operation causing a bug using the function raxSetDebugMsg(). */
#ifdef RAX_DEBUG_MSG
#define debugf(...)                                                            \
    if (raxDebugMsg) {                                                         \
        printf("%s:%s:%d:\t", __FILE__, __func__, __LINE__);                   \
        printf(__VA_ARGS__);                                                   \
        fflush(stdout);                                                        \
    }

#define debugnode(msg,n) raxDebugShowNode(msg,n)
#else
#define debugf(...)
#define debugnode(msg,n)
#endif

/* By default log debug info if RAX_DEBUG_MSG is defined. */
static int raxDebugMsg = 1;

/* When debug messages are enabled, turn them on/off dynamically. By
 * default they are enabled. Set the state to 0 to disable, and 1 to
 * re-enable. */
void raxSetDebugMsg(int onoff) {
    raxDebugMsg = onoff;
}

/* ------------------------- raxStack functions --------------------------
 * The raxStack is a simple stack of pointers that is capable of switching
 * from using a stack-allocated array to dynamic heap once a given number of
 * items are reached. It is used in order to retain the list of parent nodes
 * while walking the radix tree in order to implement certain operations that
 * need to navigate the tree upward.
 * ------------------------------------------------------------------------- */

/* Initialize the stack. */
static inline void raxStackInit(raxStack *ts) {
    ts->stack = ts->static_items;
    ts->items = 0;
    ts->maxitems = RAX_STACK_STATIC_ITEMS;
    ts->oom = 0;
}

/* Push an item into the stack, returns 1 on success, 0 on out of memory. */
static inline int raxStackPush(raxStack *ts, void *ptr) {
    if (ts->items == ts->maxitems) {
        if (ts->stack == ts->static_items) {
            ts->stack = rax_malloc(sizeof(void*)*ts->maxitems*2);
            if (ts->stack == NULL) {
                ts->stack = ts->static_items;
                ts->oom = 1;
                errno = ENOMEM;
                return 0;
            }
            memcpy(ts->stack,ts->static_items,sizeof(void*)*ts->maxitems);
        } else {
            void **newalloc = rax_realloc(ts->stack,sizeof(void*)*ts->maxitems*2);
            if (newalloc == NULL) {
                ts->oom = 1;
                errno = ENOMEM;
                return 0;
            }
            ts->stack = newalloc;
        }
        ts->maxitems *= 2;
    }
    ts->stack[ts->items] = ptr;
    ts->items++;
    return 1;
}

/* Pop an item from the stack, the function returns NULL if there are no
 * items to pop. */
static inline void *raxStackPop(raxStack *ts) {
    if (ts->items == 0) return NULL;
    ts->items--;
    return ts->stack[ts->items];
}

/* Return the stack item at the top of the stack without actually consuming
 * it. */
static inline void *raxStackPeek(raxStack *ts) {
    if (ts->items == 0) return NULL;
    return ts->stack[ts->items-1];
}

/* Free the stack in case we used heap allocation. */
static inline void raxStackFree(raxStack *ts) {
    if (ts->stack != ts->static_items) rax_free(ts->stack);
}

/* ----------------------------------------------------------------------------
 * Radix tree implementation
 * --------------------------------------------------------------------------*/

/* Return the padding needed in the characters section of a node having size
 * 'nodesize'. The padding is needed to store the child pointers to aligned
 * addresses. Note that we add 4 to the node size because the node has a four
 * bytes header. */
// 返回大小为“nodesize”的节点的字符部分所需的填充。需要填充来存储指向对齐地址的子指针。
// 注意，我们在节点大小上加4，因为节点有一个四字节的头。
#define raxPadding(nodesize) ((sizeof(void*)-(((nodesize)+4) % sizeof(void*))) & (sizeof(void*)-1))

/* Return the pointer to the last child pointer in a node. For the compressed
 * nodes this is the only child pointer. */
// 返回节点的最后一个子节点，对于压缩节点只有唯一的一个子节点。
#define raxNodeLastChildPtr(n) ((raxNode**) ( \
    ((char*)(n)) + \
    raxNodeCurrentLength(n) - \
    sizeof(raxNode*) - \
    (((n)->iskey && !(n)->isnull) ? sizeof(void*) : 0) \
))

/* Return the pointer to the first child pointer. */
// 返回节点的第一个子节点
#define raxNodeFirstChildPtr(n) ((raxNode**) ( \
    (n)->data + \
    (n)->size + \
    raxPadding((n)->size)))

/* Return the current total size of the node. Note that the second line
 * computes the padding after the string of characters, needed in order to
 * save pointers to aligned addresses. */
// 返回节点的当前总大小。请注意，第二行计算字符串后的填充，这是保存指向对齐地址的指针所必需的
/*  // 基本大小+字符串长度
    sizeof(raxNode)+(n)->size+ \
    // 字符串后的填充
    raxPadding((n)->size)+ \
    // 如果是压缩节点，只有一个子节点，如果是非压缩节点，就是n->size个字节点
    ((n)->iscompr ? sizeof(raxNode*) : sizeof(raxNode*)*(n)->size)+ \
    // 如果是非空的键节点，再加上一个指针大小
    (((n)->iskey && !(n)->isnull)*sizeof(void*)) \
*/
#define raxNodeCurrentLength(n) ( \
    sizeof(raxNode)+(n)->size+ \
    raxPadding((n)->size)+ \
    ((n)->iscompr ? sizeof(raxNode*) : sizeof(raxNode*)*(n)->size)+ \
    (((n)->iskey && !(n)->isnull)*sizeof(void*)) \
)

/* Allocate a new non compressed node with the specified number of children.
 * If datafield is true, the allocation is made large enough to hold the
 * associated data pointer.
 * Returns the new node pointer. On out of memory NULL is returned. */
// 分配一个具有指定子节点数的新的非压缩节点。如果数据字段为true，
// 则分配的大小足以容纳关联的数据指针。返回新的节点指针。内存不足时返回NULL。
raxNode *raxNewNode(size_t children, int datafield) {
    // 节点大小,计算方式查看raxNode的data字段的注释
    size_t nodesize = sizeof(raxNode)+children+raxPadding(children)+
                      sizeof(raxNode*)*children;
    // 如果有数据字段
    if (datafield) nodesize += sizeof(void*);
    // 分配节点内存
    raxNode *node = rax_malloc(nodesize);
    if (node == NULL) return NULL;
    node->iskey = 0;
    node->isnull = 0;
    node->iscompr = 0;
    node->size = children;
    return node;
}

/* Allocate a new rax and return its pointer. On out of memory the function
 * returns NULL. */
// 创建一个新的rax树并且返回指针，如果内存不足就返回NULL
rax *raxNew(void) {
    rax *rax = rax_malloc(sizeof(*rax));
    if (rax == NULL) return NULL;
    rax->numele = 0;
    // 默认有一个头节点
    rax->numnodes = 1;
    rax->head = raxNewNode(0,0);
    // 头结点没有分配成功，就释放整颗树
    if (rax->head == NULL) {
        // 释放整棵树
        rax_free(rax);
        return NULL;
    } else {
        return rax;
    }
}

/* realloc the node to make room for auxiliary data in order
 * to store an item in that node. On out of memory NULL is returned. */
// 重新分配节点，为该辅助数据腾出空间。内存不足时返回NULL。
raxNode *raxReallocForData(raxNode *n, void *data) {
    // 如果数据为空，不需要重新分配
    if (data == NULL) return n; /* No reallocation needed, setting isnull=1 */
    // 当前节点的尺寸
    size_t curlen = raxNodeCurrentLength(n);
    // 增加辅助数据地址的长度
    return rax_realloc(n,curlen+sizeof(void*));
}

/* Set the node auxiliary data to the specified pointer. */
// 将节点辅助数据设置为指定的指针
void raxSetData(raxNode *n, void *data) {
    // 设置辅助数据，肯定为key节点
    n->iskey = 1;
    if (data != NULL) {
        // 设置不为空
        n->isnull = 0;
        // 找到存放的地方
        void **ndata = (void**)
            ((char*)n+raxNodeCurrentLength(n)-sizeof(void*));
        memcpy(ndata,&data,sizeof(data));
    } else {
        n->isnull = 1;
    }
}

/* Get the node auxiliary data. */
// 得到节点的辅助数据
void *raxGetData(raxNode *n) {
    if (n->isnull) return NULL;
    // 找到存放的地方
    void **ndata =(void**)((char*)n+raxNodeCurrentLength(n)-sizeof(void*));
    void *data;
    // 拷贝指针
    memcpy(&data,ndata,sizeof(data));
    return data;
}

/* Add a new child to the node 'n' representing the character 'c' and return
 * its new pointer, as well as the child pointer by reference. Additionally
 * '***parentlink' is populated with the raxNode pointer-to-pointer of where
 * the new child was stored, which is useful for the caller to replace the
 * child pointer if it gets reallocated.
 *
 * On success the new parent node pointer is returned (it may change because
 * of the realloc, so the caller should discard 'n' and use the new value).
 * On out of memory NULL is returned, and the old node is still valid. */
raxNode *raxAddChild(raxNode *n, unsigned char c, raxNode **childptr, raxNode ***parentlink) {
    assert(n->iscompr == 0);

    size_t curlen = raxNodeCurrentLength(n);
    n->size++;
    size_t newlen = raxNodeCurrentLength(n);
    n->size--; /* For now restore the original size. We'll update it only on
                  success at the end. */

    /* Alloc the new child we will link to 'n'. */
    raxNode *child = raxNewNode(0,0);
    if (child == NULL) return NULL;

    /* Make space in the original node. */
    raxNode *newn = rax_realloc(n,newlen);
    if (newn == NULL) {
        rax_free(child);
        return NULL;
    }
    n = newn;

    /* After the reallocation, we have up to 8/16 (depending on the system
     * pointer size, and the required node padding) bytes at the end, that is,
     * the additional char in the 'data' section, plus one pointer to the new
     * child, plus the padding needed in order to store addresses into aligned
     * locations.
     *
     * So if we start with the following node, having "abde" edges.
     *
     * Note:
     * - We assume 4 bytes pointer for simplicity.
     * - Each space below corresponds to one byte
     *
     * [HDR*][abde][Aptr][Bptr][Dptr][Eptr]|AUXP|
     *
     * After the reallocation we need: 1 byte for the new edge character
     * plus 4 bytes for a new child pointer (assuming 32 bit machine).
     * However after adding 1 byte to the edge char, the header + the edge
     * characters are no longer aligned, so we also need 3 bytes of padding.
     * In total the reallocation will add 1+4+3 bytes = 8 bytes:
     *
     * (Blank bytes are represented by ".")
     *
     * [HDR*][abde][Aptr][Bptr][Dptr][Eptr]|AUXP|[....][....]
     *
     * Let's find where to insert the new child in order to make sure
     * it is inserted in-place lexicographically. Assuming we are adding
     * a child "c" in our case pos will be = 2 after the end of the following
     * loop. */
    int pos;
    for (pos = 0; pos < n->size; pos++) {
        if (n->data[pos] > c) break;
    }

    /* Now, if present, move auxiliary data pointer at the end
     * so that we can mess with the other data without overwriting it.
     * We will obtain something like that:
     *
     * [HDR*][abde][Aptr][Bptr][Dptr][Eptr][....][....]|AUXP|
     */
    unsigned char *src, *dst;
    if (n->iskey && !n->isnull) {
        src = ((unsigned char*)n+curlen-sizeof(void*));
        dst = ((unsigned char*)n+newlen-sizeof(void*));
        memmove(dst,src,sizeof(void*));
    }

    /* Compute the "shift", that is, how many bytes we need to move the
     * pointers section forward because of the addition of the new child
     * byte in the string section. Note that if we had no padding, that
     * would be always "1", since we are adding a single byte in the string
     * section of the node (where now there is "abde" basically).
     *
     * However we have padding, so it could be zero, or up to 8.
     *
     * Another way to think at the shift is, how many bytes we need to
     * move child pointers forward *other than* the obvious sizeof(void*)
     * needed for the additional pointer itself. */
    size_t shift = newlen - curlen - sizeof(void*);

    /* We said we are adding a node with edge 'c'. The insertion
     * point is between 'b' and 'd', so the 'pos' variable value is
     * the index of the first child pointer that we need to move forward
     * to make space for our new pointer.
     *
     * To start, move all the child pointers after the insertion point
     * of shift+sizeof(pointer) bytes on the right, to obtain:
     *
     * [HDR*][abde][Aptr][Bptr][....][....][Dptr][Eptr]|AUXP|
     */
    src = n->data+n->size+
          raxPadding(n->size)+
          sizeof(raxNode*)*pos;
    memmove(src+shift+sizeof(raxNode*),src,sizeof(raxNode*)*(n->size-pos));

    /* Move the pointers to the left of the insertion position as well. Often
     * we don't need to do anything if there was already some padding to use. In
     * that case the final destination of the pointers will be the same, however
     * in our example there was no pre-existing padding, so we added one byte
     * plus three bytes of padding. After the next memmove() things will look
     * like that:
     *
     * [HDR*][abde][....][Aptr][Bptr][....][Dptr][Eptr]|AUXP|
     */
    if (shift) {
        src = (unsigned char*) raxNodeFirstChildPtr(n);
        memmove(src+shift,src,sizeof(raxNode*)*pos);
    }

    /* Now make the space for the additional char in the data section,
     * but also move the pointers before the insertion point to the right
     * by shift bytes, in order to obtain the following:
     *
     * [HDR*][ab.d][e...][Aptr][Bptr][....][Dptr][Eptr]|AUXP|
     */
    src = n->data+pos;
    memmove(src+1,src,n->size-pos);

    /* We can now set the character and its child node pointer to get:
     *
     * [HDR*][abcd][e...][Aptr][Bptr][....][Dptr][Eptr]|AUXP|
     * [HDR*][abcd][e...][Aptr][Bptr][Cptr][Dptr][Eptr]|AUXP|
     */
    n->data[pos] = c;
    n->size++;
    src = (unsigned char*) raxNodeFirstChildPtr(n);
    raxNode **childfield = (raxNode**)(src+sizeof(raxNode*)*pos);
    memcpy(childfield,&child,sizeof(child));
    *childptr = child;
    *parentlink = childfield;
    return n;
}

/* Turn the node 'n', that must be a node without any children, into a
 * compressed node representing a set of nodes linked one after the other
 * and having exactly one child each. The node can be a key or not: this
 * property and the associated value if any will be preserved.
 *
 * The function also returns a child node, since the last node of the
 * compressed chain cannot be part of the chain: it has zero children while
 * we can only compress inner nodes with exactly one child each. */
raxNode *raxCompressNode(raxNode *n, unsigned char *s, size_t len, raxNode **child) {
    assert(n->size == 0 && n->iscompr == 0);
    void *data = NULL; /* Initialized only to avoid warnings. */
    size_t newsize;

    debugf("Compress node: %.*s\n", (int)len,s);

    /* Allocate the child to link to this node. */
    *child = raxNewNode(0,0);
    if (*child == NULL) return NULL;

    /* Make space in the parent node. */
    newsize = sizeof(raxNode)+len+raxPadding(len)+sizeof(raxNode*);
    if (n->iskey) {
        data = raxGetData(n); /* To restore it later. */
        if (!n->isnull) newsize += sizeof(void*);
    }
    raxNode *newn = rax_realloc(n,newsize);
    if (newn == NULL) {
        rax_free(*child);
        return NULL;
    }
    n = newn;

    n->iscompr = 1;
    n->size = len;
    memcpy(n->data,s,len);
    if (n->iskey) raxSetData(n,data);
    raxNode **childfield = raxNodeLastChildPtr(n);
    memcpy(childfield,child,sizeof(*child));
    return n;
}

/* Low level function that walks the tree looking for the string
 * 's' of 'len' bytes. The function returns the number of characters
 * of the key that was possible to process: if the returned integer
 * is the same as 'len', then it means that the node corresponding to the
 * string was found (however it may not be a key in case the node->iskey is
 * zero or if simply we stopped in the middle of a compressed node, so that
 * 'splitpos' is non zero).
 *
 * Otherwise if the returned integer is not the same as 'len', there was an
 * early stop during the tree walk because of a character mismatch.
 *
 * The node where the search ended (because the full string was processed
 * or because there was an early stop) is returned by reference as
 * '*stopnode' if the passed pointer is not NULL. This node link in the
 * parent's node is returned as '*plink' if not NULL. Finally, if the
 * search stopped in a compressed node, '*splitpos' returns the index
 * inside the compressed node where the search ended. This is useful to
 * know where to split the node for insertion.
 *
 * Note that when we stop in the middle of a compressed node with
 * a perfect match, this function will return a length equal to the
 * 'len' argument (all the key matched), and will return a *splitpos which is
 * always positive (that will represent the index of the character immediately
 * *after* the last match in the current compressed node).
 *
 * When instead we stop at a compressed node and *splitpos is zero, it
 * means that the current node represents the key (that is, none of the
 * compressed node characters are needed to represent the key, just all
 * its parents nodes). */
// *stopnode为查找过程中的终止节点，也就意味着，当rax查找到该节点时，待查找的key已经匹配完成，或者当前节点无法与带查找的key匹配；
// *plink用于记录父节点中指向*stopnode的指针的位置，当*stopnode变化时，也需要修改父节点指向该节点的指针；
// *splitpos用于记录压缩节点的匹配位置;
// 当ts不为空时，会将查找该key的路径写入该变量。
static inline size_t raxLowWalk(rax *rax, unsigned char *s, size_t len, raxNode **stopnode, raxNode ***plink, int *splitpos, raxStack *ts) {
    raxNode *h = rax->head;
    raxNode **parentlink = &rax->head;

    size_t i = 0; /* Position in the string. */
    size_t j = 0; /* Position in the node children (or bytes if compressed).*/
    // 开始遍历
    while(h->size && i < len) {
        debugnode("Lookup current node",h);
        unsigned char *v = h->data;
        // 如果是压缩 
        if (h->iscompr) {
            // 压缩的话，只要某一个字符不匹配，就不匹配
            for (j = 0; j < h->size && i < len; j++, i++) {
                if (v[j] != s[i]) break;
            }
            // 没有完全匹配，跳出
            if (j != h->size) break;
        } 
        // 非压缩
        else {
            /* Even when h->size is large, linear scan provides good
             * performances compared to other approaches that are in theory
             * more sounding, like performing a binary search. */
            // 即使h->大小很大，与其他理论上更合理的方法（如执行二进制搜索）相比，线性扫描也能提供良好的性能
            for (j = 0; j < h->size; j++) {
                if (v[j] == s[i]) break;
            }
            // 如果最后一个节点都没有找到，跳出循环
            if (j == h->size) break;
            i++;
        }
        // 保存父节点
        if (ts) raxStackPush(ts,h); /* Save stack of parent nodes. */
        // 取出父节点的第一个子节点
        raxNode **children = raxNodeFirstChildPtr(h);
        //压缩只有一个子节点
        if (h->iscompr) j = 0; /* Compressed node only child is at index 0. */
        // 更新父节点
        memcpy(&h,children+j,sizeof(h));
        // 指向父节点的地方
        parentlink = children+j;
        // 如果新节点是非压缩的，并且我们不再迭代（因为i==len），则将拆分位置设置为0，以表示该节点表示搜索到的关键字。
        j = 0; /* If the new node is non compressed and we do not
                  iterate again (since i == len) set the split
                  position to 0 to signal this node represents
                  the searched key. */
    }
    debugnode("Lookup stop node is",h);
    if (stopnode) *stopnode = h;
    if (plink) *plink = parentlink;
    if (splitpos && h->iscompr) *splitpos = j;
    return i;
}

/* Insert the element 's' of size 'len', setting as auxiliary data
 * the pointer 'data'. If the element is already present, the associated
 * data is updated (only if 'overwrite' is set to 1), and 0 is returned,
 * otherwise the element is inserted and 1 is returned. On out of memory the
 * function returns 0 as well but sets errno to ENOMEM, otherwise errno will
 * be set to 0.
 */
// 插入大小为“len”的元素“s”，将指针“data”设置为辅助数据。如果元素已经存在，则更新关联的数据（仅当overwrite'设置为1时），
// 并返回0，否则插入元素并返回1。当内存不足时，函数也会返回0，但会将errno设置为ENOMEM，否则errno将设置为0。
int raxGenericInsert(rax *rax, unsigned char *s, size_t len, void *data, void **old, int overwrite) {
    size_t i;
    int j = 0; /* Split position. If raxLowWalk() stops in a compressed
                  node, the index 'j' represents the char we stopped within the
                  compressed node, that is, the position where to split the
                  node for insertion. */
    raxNode *h, **parentlink;

    debugf("### Insert %.*s with value %p\n", (int)len, s, data);
    // 先查找，j用于记录压缩节点的拆分位置
    i = raxLowWalk(rax,s,len,&h,&parentlink,&j,NULL);

    /* If i == len we walked following the whole string. If we are not
     * in the middle of a compressed node, the string is either already
     * inserted or this middle node is currently not a key, but can represent
     * our key. We have just to reallocate the node and make space for the
     * data pointer. */
    // 如果 i == len表示我们走过了整个字符串。如果我们不在压缩节点的中间，该字符串要么已经插入，
    // 要么该中间节点不是一个键，但是可以表示我们的键。我们只需重新分配节点并且为数据指针腾出空间
    if (i == len && (!h->iscompr || j == 0 /* not in the middle if j is 0 */)) {
        debugf("### Insert: node representing key exists\n");
        /* Make space for the value pointer if needed. */
        // 如果需要，为数据指针分配空间
        if (!h->iskey || (h->isnull && overwrite)) {
            h = raxReallocForData(h,data);
            if (h) memcpy(parentlink,&h,sizeof(h));
        }
        // 如果分配失败，将错误码置为内存不足
        if (h == NULL) {
            errno = ENOMEM;
            return 0;
        }

        /* Update the existing key if there is already one. */
        // 如果是已经存在的键值，更新
        if (h->iskey) {
            // 如果需要保存老的数据，
            if (old) *old = raxGetData(h);
            // 如果覆盖，就设置新的数据
            if (overwrite) raxSetData(h,data);
            errno = 0;
            return 0; /* Element already exists. */
        }

        /* Otherwise set the node as a key. Note that raxSetData()
         * will set h->iskey. */
        // 将节点设置为键节点
        raxSetData(h,data);
        // 增加节点数
        rax->numele++;
        return 1; /* Element inserted. */
    }

    /* If the node we stopped at is a compressed node, we need to
     * split it before to continue.
     * 如果我们在一个压缩节点上停止，我们需要再接下来的处理之前将节点拆分
     *
     * Splitting a compressed node have a few possible cases.
     * Imagine that the node 'h' we are currently at is a compressed
     * node containing the string "ANNIBALE" (it means that it represents
     * nodes A -> N -> N -> I -> B -> A -> L -> E with the only child
     * pointer of this node pointing at the 'E' node, because remember that
     * we have characters at the edges of the graph, not inside the nodes
     * themselves.
     * 拆分压缩节点有几种可能的情况。假设我们当前所在的节点“h”是一个包含字符串“ANNIBALE”的压缩节点
     * （这意味着它表示节点A->N->N->I->B->A->L->E，该节点的唯一子指针指向“E”节点，因为请记住，
     * 我们在图的边缘有字符，而不是在节点本身内部。
     * 
     * In order to show a real case imagine our node to also point to
     * another compressed node, that finally points at the node without
     * children, representing 'O':
     * 为了显示真实情况，想象我们的节点也指向另一个压缩节点，该节点最终指向没有子节点的节点，表示“O”：
     *
     *     "ANNIBALE" -> "SCO" -> []
     *
     * When inserting we may face the following cases. Note that all the cases
     * require the insertion of a non compressed node with exactly two
     * children, except for the last case which just requires splitting a
     * compressed node.
     * 插入时，我们可能会面临以下情况。请注意，除了最后一种情况只需要拆分一个压缩节点外，所有情况都需要插入一个正好有两个子节点的非压缩节点。
     * 
     * 1) Inserting "ANNIENTARE"
     *
     *               |B| -> "ALE" -> "SCO" -> []
     *     "ANNI" -> |-|
     *               |E| -> (... continue algo ...) "NTARE" -> []
     *
     * 2) Inserting "ANNIBALI"
     *
     *                  |E| -> "SCO" -> []
     *     "ANNIBAL" -> |-|
     *                  |I| -> (... continue algo ...) []
     *
     * 3) Inserting "AGO" (Like case 1, but set iscompr = 0 into original node)
     *
     *            |N| -> "NIBALE" -> "SCO" -> []
     *     |A| -> |-|
     *            |G| -> (... continue algo ...) |O| -> []
     *
     * 4) Inserting "CIAO"
     *
     *     |A| -> "NNIBALE" -> "SCO" -> []
     *     |-|
     *     |C| -> (... continue algo ...) "IAO" -> []
     *
     * 5) Inserting "ANNI"
     *
     *     "ANNI" -> "BALE" -> "SCO" -> []
     *
     * The final algorithm for insertion covering all the above cases is as
     * follows.
     * 覆盖上述所有情况的插入的最终算法如下。
     * 
     * ============================= ALGO 1 =============================
     *
     * For the above cases 1 to 4, that is, all cases where we stopped in
     * the middle of a compressed node for a character mismatch, do:
     * 对于上述情况1到4，也就是说，我们在压缩节点中间停止字符不匹配的所有情况，请执行以下操作：
     * 
     * Let $SPLITPOS be the zero-based index at which, in the
     * compressed node array of characters, we found the mismatching
     * character. For example if the node contains "ANNIBALE" and we add
     * "ANNIENTARE" the $SPLITPOS is 4, that is, the index at which the
     * mismatching character is found.
     * 让$SPLITPOS是从零开始的索引，在压缩的字符节点数组中，我们发现了不匹配的字符。
     * 例如，如果节点包含“ANNIBALE”，然后我们添加“ANNIENTARE”，则$SPLITPOS为4，即找到不匹配字符的索引。
     * 
     * 1. Save the current compressed node $NEXT pointer (the pointer to the
     *    child element, that is always present in compressed nodes).
     * 保存当前压缩节点$NEXT指针（指向子元素的指针，该指针始终存在于压缩节点中）。
     * 
     * 2. Create "split node" having as child the non common letter
     *    at the compressed node. The other non common letter (at the key)
     *    will be added later as we continue the normal insertion algorithm
     *    at step "6".
     * 创建“拆分节点”，将压缩节点上的非公共字母作为子节点。当我们在步骤“6”继续正
     * 常插入算法时，稍后添加其他的非公共字母（键）。
     * 
     * 3a. IF $SPLITPOS == 0:
     *     Replace the old node with the split node, by copying the auxiliary
     *     data if any. Fix parent's reference. Free old node eventually
     *     (we still need its data for the next steps of the algorithm).
     * 通过复制辅助数据（如果有），将旧节点替换为拆分节点。修复父项的引用。最终释放旧节点（我们仍然需要它的数据用于算法的下一步）。     
     *
     * 3b. IF $SPLITPOS != 0:
     *     Trim the compressed node (reallocating it as well) in order to
     *     contain $splitpos characters. Change child pointer in order to link
     *     to the split node. If new compressed node len is just 1, set
     *     iscompr to 0 (layout is the same). Fix parent's reference.
     * 修剪压缩的节点（也重新分配它），以便包含$splippos字符。更改子指针以便链接到拆分节点。
     * 如果新的压缩节点len仅为1，则将iscompr设置为0（布局相同）。修复父项的引用。
     * 
     * 4a. IF the postfix len (the length of the remaining string of the
     *     original compressed node after the split character) is non zero,
     *     create a "postfix node". If the postfix node has just one character
     *     set iscompr to 0, otherwise iscompr to 1. Set the postfix node
     *     child pointer to $NEXT.
     * 如果后缀len（拆分字符后原始压缩节点的剩余字符串的长度）不是零，则创建一个“后缀节点”。
     * 如果后缀节点只有一个字符将iscompr设置为0，否则iscompr将设置为1。将$NEXT设置为后缀节点子指针。
     * 
     * 4b. IF the postfix len is zero, just use $NEXT as postfix pointer.
     * 如果后缀长度为零，只需使用$NEXT作为后缀指针
     *  
     * 5. Set child[0] of split node to postfix node.
     * 将拆分节点的child[0]设置为后缀节点。
     * 
     * 6. Set the split node as the current node, set current index at child[1]
     *    and continue insertion algorithm as usually.
     * 将拆分节点设置为当前节点，将当前索引设置为子节点[1]，并照常继续插入算法。
     *
     * ============================= ALGO 2 =============================
     *
     * For case 5, that is, if we stopped in the middle of a compressed
     * node but no mismatch was found, do:
     * 对于情况5，也就是说，如果我们在压缩节点的中间停止，但没有发现不匹配，请执行以下操作：
     * 
     * Let $SPLITPOS be the zero-based index at which, in the
     * compressed node array of characters, we stopped iterating because
     * there were no more keys character to match. So in the example of
     * the node "ANNIBALE", adding the string "ANNI", the $SPLITPOS is 4.
     * 让$SPLITPOS是从零开始的索引，在压缩的字符节点数组中，因为没有更多的键字符可以匹配，
     * 所以我们停止遍历。因此，在节点“ANNIBALE”的示例中，添加字符串“ANNI”，$SPLITPOS为4。
     * 
     * 1. Save the current compressed node $NEXT pointer (the pointer to the
     *    child element, that is always present in compressed nodes).
     * 保存当前压缩节点$NEXT指针（指向子元素的指针，该指针始终存在于压缩节点中）。
     *
     * 2. Create a "postfix node" containing all the characters from $SPLITPOS
     *    to the end. Use $NEXT as the postfix node child pointer.
     *    If the postfix node length is 1, set iscompr to 0.
     *    Set the node as a key with the associated value of the new
     *    inserted key.
     * 创建一个“后缀节点”，包含从$SPLITPOS到结尾的所有字符。使用$NEXT作为后缀节点子指针。
     * 如果后缀节点长度为1，则将iscompr设置为0。将节点设置为带有新插入键的关联值的key节点。
     * 
     * 3. Trim the current node to contain the first $SPLITPOS characters.
     *    As usually if the new node length is just 1, set iscompr to 0.
     *    Take the iskey / associated value as it was in the original node.
     *    Fix the parent's reference.
     * 修剪当前节点以包含第一个$SPLITPOS字符。与通常情况一样，如果新节点长度仅为1，
     * 则将iscompr设置为0。将iskey/关联值作为原始节点中的值。修复父对象的引用。
     * 
     * 4. Set the postfix node as the only child pointer of the trimmed
     *    node created at step 1.
     * 将后缀节点设置为在步骤1中创建的修剪节点的唯一子节点。
     */

    /* ------------------------- ALGORITHM 1 --------------------------- */
    // 如果是压缩节点，并且匹配了一部分
    if (h->iscompr && i != len) {
        debugf("ALGO 1: Stopped at compressed node %.*s (%p)\n",
            h->size, h->data, (void*)h);
        debugf("Still to insert: %.*s\n", (int)(len-i), s+i);
        debugf("Splitting at %d: '%c'\n", j, ((char*)h->data)[j]);
        debugf("Other (key) letter is '%c'\n", s[i]);

        /* 1: Save next pointer. */
        // 保存子节点
        raxNode **childfield = raxNodeLastChildPtr(h);
        raxNode *next;
        memcpy(&next,childfield,sizeof(next));
        debugf("Next is %p\n", (void*)next);
        debugf("iskey %d\n", h->iskey);
        if (h->iskey) {
            debugf("key value is %p\n", raxGetData(h));
        }

        /* Set the length of the additional nodes we will need. */
        // 设置我们需要额外节点的长度
        // 相同字符串的长度
        size_t trimmedlen = j;
        // 后缀的长度
        size_t postfixlen = h->size - j - 1;
        // 
        int split_node_is_key = !trimmedlen && h->iskey && !h->isnull;
        size_t nodesize;

        /* 2: Create the split node. Also allocate the other nodes we'll need
         *    ASAP, so that it will be simpler to handle OOM. */
        // 创建拆分节点。还要尽快分配我们需要的其他节点，这样处理OOM就更简单了
        raxNode *splitnode = raxNewNode(1, split_node_is_key);
        // 裁剪后的节点
        raxNode *trimmed = NULL;
        raxNode *postfix = NULL;

        // 如果从中间拆分，原始节点需要压缩
        if (trimmedlen) {
            // 计算节点尺寸
            nodesize = sizeof(raxNode)+trimmedlen+raxPadding(trimmedlen)+
                       sizeof(raxNode*);
            if (h->iskey && !h->isnull) nodesize += sizeof(void*);
            // 重新分配返回裁剪后的节点
            trimmed = rax_malloc(nodesize);
        }

        // 后缀长度
        if (postfixlen) {
            // 后缀节点尺寸
            nodesize = sizeof(raxNode)+postfixlen+raxPadding(postfixlen)+
                       sizeof(raxNode*);
            // 分配后缀节点
            postfix = rax_malloc(nodesize);
        }

        /* OOM? Abort now that the tree is untouched. */
        // 如果任一一个节点的为空，表示内存不足
        if (splitnode == NULL ||
            (trimmedlen && trimmed == NULL) ||
            (postfixlen && postfix == NULL))
        {
            rax_free(splitnode);
            rax_free(trimmed);
            rax_free(postfix);
            errno = ENOMEM;
            return 0;
        }
        // 从拆分处开始赋值
        splitnode->data[0] = h->data[j];
        // 如果第一个字符就不符合
        if (j == 0) {
            /* 3a: Replace the old node with the split node. */
            // 使用拆分节点来替换老的节点
            if (h->iskey) {
                void *ndata = raxGetData(h);
                // 将老节点的数据取出来设置到新节点里面去
                raxSetData(splitnode,ndata);
            }
            memcpy(parentlink,&splitnode,sizeof(splitnode));
        } else {
            /* 3b: Trim the compressed node. */
            // 裁剪压缩节点
            trimmed->size = j;
            // 拷贝相同字符串的数据
            memcpy(trimmed->data,h->data,j);
            // 如果共同前缀长度大于1，就置为压缩节点
            trimmed->iscompr = j > 1 ? 1 : 0;
            // 考虑原始节点的属性
            trimmed->iskey = h->iskey;
            trimmed->isnull = h->isnull;
            // 如果是键节点，值不为空，需要拷贝值
            if (h->iskey && !h->isnull) {
                void *ndata = raxGetData(h);
                raxSetData(trimmed,ndata);
            }
            // 得到原始节点的最后一个节点的指针存放处
            raxNode **cp = raxNodeLastChildPtr(trimmed);
            // 拆分后的节点变成了原始节点的子节点
            memcpy(cp,&splitnode,sizeof(splitnode));
            memcpy(parentlink,&trimmed,sizeof(trimmed));
            // 设置parentlink为拆分节点的父节点
            parentlink = cp; /* Set parentlink to splitnode parent. */
            // 增加节点数目
            rax->numnodes++;
        }

        /* 4: Create the postfix node: what remains of the original
         * compressed node after the split. */
        // 创建后缀节点：拆分后原始压缩节点的剩余部分。
        if (postfixlen) {
            /* 4a: create a postfix node. */
            // 设置后缀节点
            postfix->iskey = 0;
            postfix->isnull = 0;
            postfix->size = postfixlen;
            postfix->iscompr = postfixlen > 1;
            // 拷贝后缀的值
            memcpy(postfix->data,h->data+j+1,postfixlen);
            // 最后一个子节点的指针存放处
            raxNode **cp = raxNodeLastChildPtr(postfix);
            // 原始节点的子节点节点变成了后缀节点的子节点
            memcpy(cp,&next,sizeof(next));
            rax->numnodes++;
        } else {
            /* 4b: just use next as postfix node. */
            postfix = next;
        }

        /* 5: Set splitnode first child as the postfix node. */
        // 后缀节点变成了拆分节点的子节点
        raxNode **splitchild = raxNodeLastChildPtr(splitnode);
        memcpy(splitchild,&postfix,sizeof(postfix));

        /* 6. Continue insertion: this will cause the splitnode to
         * get a new child (the non common character at the currently
         * inserted key). */
        rax_free(h);
        h = splitnode;
    } 
    // 压缩节点，我们在压缩节点的中间停止，但没有发现不匹配
    else if (h->iscompr && i == len) {
    /* ------------------------- ALGORITHM 2 --------------------------- */
        debugf("ALGO 2: Stopped at compressed node %.*s (%p) j = %d\n",
            h->size, h->data, (void*)h, j);

        /* Allocate postfix & trimmed nodes ASAP to fail for OOM gracefully. */
        // 尽快分配后缀节点和裁剪节点，以便失败时优雅的OOM。

        // 后缀的长度
        size_t postfixlen = h->size - j;
        // 计算后缀节点的长度
        size_t nodesize = sizeof(raxNode)+postfixlen+raxPadding(postfixlen)+
                          sizeof(raxNode*);
        // 如果有数据，预留数据的空间
        if (data != NULL) nodesize += sizeof(void*);
        raxNode *postfix = rax_malloc(nodesize);
        // 裁剪的节点
        nodesize = sizeof(raxNode)+j+raxPadding(j)+sizeof(raxNode*);
        // 释放有时间
        if (h->iskey && !h->isnull) nodesize += sizeof(void*);
        raxNode *trimmed = rax_malloc(nodesize);

        if (postfix == NULL || trimmed == NULL) {
            rax_free(postfix);
            rax_free(trimmed);
            errno = ENOMEM;
            return 0;
        }

        /* 1: Save next pointer. */
        // 保存原始节点的下一个节点
        raxNode **childfield = raxNodeLastChildPtr(h);
        raxNode *next;
        memcpy(&next,childfield,sizeof(next));

        /* 2: Create the postfix node. */
        // 初始化后缀节点
        postfix->size = postfixlen;
        // 如果长度大于1就是压缩节点
        postfix->iscompr = postfixlen > 1;
        postfix->iskey = 1;
        postfix->isnull = 0;
        // 拷贝数据
        memcpy(postfix->data,h->data+j,postfixlen);
        raxSetData(postfix,data);
        raxNode **cp = raxNodeLastChildPtr(postfix);
        // 将原始节点的子节点变成后缀节点的子节点
        memcpy(cp,&next,sizeof(next));
        // 增加节点数
        rax->numnodes++;

        /* 3: Trim the compressed node. */
        // 裁剪压缩节点
        trimmed->size = j;
        trimmed->iscompr = j > 1;
        trimmed->iskey = 0;
        trimmed->isnull = 0;
        // 拷贝数据
        memcpy(trimmed->data,h->data,j);
        memcpy(parentlink,&trimmed,sizeof(trimmed));
        // 设置数据
        if (h->iskey) {
            void *aux = raxGetData(h);
            raxSetData(trimmed,aux);
        }

        /* Fix the trimmed node child pointer to point to
         * the postfix node. */
        // 后缀节点变成裁剪节点的子节点
        cp = raxNodeLastChildPtr(trimmed);
        memcpy(cp,&postfix,sizeof(postfix));

        /* Finish! We don't need to continue with the insertion
         * algorithm for ALGO 2. The key is already inserted. */
        // 完成!我们不需要继续使用ALGO 2的插入算法。key已经插入
        rax->numele++;
        rax_free(h);
        return 1; /* Key inserted. */
    }

    /* We walked the radix tree as far as we could, but still there are left
     * chars in our string. We need to insert the missing nodes. */
    // 我们沿着radix树走得尽可能远，但字符串中仍然有剩余的字符。我们需要插入缺失的节点。
    while(i < len) {
        raxNode *child;
        /* If this node is going to have a single child, and there
         * are other characters, so that that would result in a chain
         * of single-childed nodes, turn it into a compressed node. */
        // 如果这个节点将有一个子节点，并且还有其他字符，那么这将导致一个单个子节点链，将其转换为一个压缩节点
        if (h->size == 0 && len-i > 1) {
            debugf("Inserting compressed node\n");
            size_t comprsize = len-i;
            if (comprsize > RAX_NODE_MAX_SIZE)
                comprsize = RAX_NODE_MAX_SIZE;
            // 创建一个压缩节点
            raxNode *newh = raxCompressNode(h,s+i,comprsize,&child);
            if (newh == NULL) goto oom;
            h = newh;
            memcpy(parentlink,&h,sizeof(h));
            parentlink = raxNodeLastChildPtr(h);
            i += comprsize;
        } else {
            debugf("Inserting normal node\n");
            raxNode **new_parentlink;
            // 增加一个非压缩节点
            raxNode *newh = raxAddChild(h,s[i],&child,&new_parentlink);
            if (newh == NULL) goto oom;
            h = newh;
            memcpy(parentlink,&h,sizeof(h));
            parentlink = new_parentlink;
            i++;
        }
        // 增加节点数
        rax->numnodes++;
        h = child;
    }
    // 为数据腾出空间
    raxNode *newh = raxReallocForData(h,data);
    if (newh == NULL) goto oom;
    h = newh;
    // 如果是数据节点，增加数据得数目
    if (!h->iskey) rax->numele++;
    // 设置数据
    raxSetData(h,data);
    memcpy(parentlink,&h,sizeof(h));
    return 1; /* Element inserted. */

oom:
    /* This code path handles out of memory after part of the sub-tree was
     * already modified. Set the node as a key, and then remove it. However we
     * do that only if the node is a terminal node, otherwise if the OOM
     * happened reallocating a node in the middle, we don't need to free
     * anything. */
    // 此代码路径处理部分子树已被修改后内存不足的问题。将节点设置为键，然后将其删除。
    // 但是，只有当节点是终端节点时才这样做，否则，如果OOM在中间重新分配了一个节点，我们不需要释放任何内容
    if (h->size == 0) {
        h->isnull = 1;
        h->iskey = 1;
        rax->numele++; /* Compensate the next remove. */
        assert(raxRemove(rax,s,i,NULL) != 0);
    }
    errno = ENOMEM;
    return 0;
}

/* Overwriting insert. Just a wrapper for raxGenericInsert() that will
 * update the element if there is already one for the same key. */
// 覆盖插入，只是raxGenericInsert的一个包装函数，如果已经有相同的键，就会更新该项
int raxInsert(rax *rax, unsigned char *s, size_t len, void *data, void **old) {
    return raxGenericInsert(rax,s,len,data,old,1);
}

/* Non overwriting insert function: if an element with the same key
 * exists, the value is not updated and the function returns 0.
 * This is just a wrapper for raxGenericInsert(). */
int raxTryInsert(rax *rax, unsigned char *s, size_t len, void *data, void **old) {
    return raxGenericInsert(rax,s,len,data,old,0);
}

/* Find a key in the rax, returns raxNotFound special void pointer value
 * if the item was not found, otherwise the value associated with the
 * item is returned. */
// 在rax树中查找key对应的项,如果对应的项没有找到，返回一个叫做raxNotFound特殊指针。
// 或者关联的项的值被返回
void *raxFind(rax *rax, unsigned char *s, size_t len) {
    raxNode *h;

    debugf("### Lookup: %.*s\n", (int)len, s);
    int splitpos = 0;
    size_t i = raxLowWalk(rax,s,len,&h,NULL,&splitpos,NULL);
    if (i != len || (h->iscompr && splitpos != 0) || !h->iskey)
        return raxNotFound;
    return raxGetData(h);
}

/* Return the memory address where the 'parent' node stores the specified
 * 'child' pointer, so that the caller can update the pointer with another
 * one if needed. The function assumes it will find a match, otherwise the
 * operation is an undefined behavior (it will continue scanning the
 * memory without any bound checking). */
raxNode **raxFindParentLink(raxNode *parent, raxNode *child) {
    raxNode **cp = raxNodeFirstChildPtr(parent);
    raxNode *c;
    while(1) {
        memcpy(&c,cp,sizeof(c));
        if (c == child) break;
        cp++;
    }
    return cp;
}

/* Low level child removal from node. The new node pointer (after the child
 * removal) is returned. Note that this function does not fix the pointer
 * of the parent node in its parent, so this task is up to the caller.
 * The function never fails for out of memory. */
raxNode *raxRemoveChild(raxNode *parent, raxNode *child) {
    debugnode("raxRemoveChild before", parent);
    /* If parent is a compressed node (having a single child, as for definition
     * of the data structure), the removal of the child consists into turning
     * it into a normal node without children. */
    if (parent->iscompr) {
        void *data = NULL;
        if (parent->iskey) data = raxGetData(parent);
        parent->isnull = 0;
        parent->iscompr = 0;
        parent->size = 0;
        if (parent->iskey) raxSetData(parent,data);
        debugnode("raxRemoveChild after", parent);
        return parent;
    }

    /* Otherwise we need to scan for the child pointer and memmove()
     * accordingly.
     *
     * 1. To start we seek the first element in both the children
     *    pointers and edge bytes in the node. */
    raxNode **cp = raxNodeFirstChildPtr(parent);
    raxNode **c = cp;
    unsigned char *e = parent->data;

    /* 2. Search the child pointer to remove inside the array of children
     *    pointers. */
    while(1) {
        raxNode *aux;
        memcpy(&aux,c,sizeof(aux));
        if (aux == child) break;
        c++;
        e++;
    }

    /* 3. Remove the edge and the pointer by memmoving the remaining children
     *    pointer and edge bytes one position before. */
    int taillen = parent->size - (e - parent->data) - 1;
    debugf("raxRemoveChild tail len: %d\n", taillen);
    memmove(e,e+1,taillen);

    /* Compute the shift, that is the amount of bytes we should move our
     * child pointers to the left, since the removal of one edge character
     * and the corresponding padding change, may change the layout.
     * We just check if in the old version of the node there was at the
     * end just a single byte and all padding: in that case removing one char
     * will remove a whole sizeof(void*) word. */
    size_t shift = ((parent->size+4) % sizeof(void*)) == 1 ? sizeof(void*) : 0;

    /* Move the children pointers before the deletion point. */
    if (shift)
        memmove(((char*)cp)-shift,cp,(parent->size-taillen-1)*sizeof(raxNode**));

    /* Move the remaining "tail" pointers at the right position as well. */
    size_t valuelen = (parent->iskey && !parent->isnull) ? sizeof(void*) : 0;
    memmove(((char*)c)-shift,c+1,taillen*sizeof(raxNode**)+valuelen);

    /* 4. Update size. */
    parent->size--;

    /* realloc the node according to the theoretical memory usage, to free
     * data if we are over-allocating right now. */
    raxNode *newnode = rax_realloc(parent,raxNodeCurrentLength(parent));
    if (newnode) {
        debugnode("raxRemoveChild after", newnode);
    }
    /* Note: if rax_realloc() fails we just return the old address, which
     * is valid. */
    return newnode ? newnode : parent;
}

/* Remove the specified item. Returns 1 if the item was found and
 * deleted, 0 otherwise. */
// 删除指定得元素，如果元素找到就返回1并且删除，或者返回0
int raxRemove(rax *rax, unsigned char *s, size_t len, void **old) {
    raxNode *h;
    raxStack ts;

    debugf("### Delete: %.*s\n", (int)len, s);
    // 初始化栈
    raxStackInit(&ts);
    int splitpos = 0;
    // 查找指定元素
    size_t i = raxLowWalk(rax,s,len,&h,NULL,&splitpos,&ts);
    // 没找到，返回0
    if (i != len || (h->iscompr && splitpos != 0) || !h->iskey) {
        raxStackFree(&ts);
        return 0;
    }
    // 找到了，返回原来的数据值
    if (old) *old = raxGetData(h);
    h->iskey = 0;
    rax->numele--;

    /* If this node has no children, the deletion needs to reclaim the
     * no longer used nodes. This is an iterative process that needs to
     * walk the three upward, deleting all the nodes with just one child
     * that are not keys, until the head of the rax is reached or the first
     * node with more than one child is found. */
    // 如果此节点没有子节点，则删除操作需要回收不再使用的节点。这是一个迭代过程，
    // 需要向上遍历三个节点，删除所有只有一个子节点不是关键节点的节点，
    // 直到到达rax的头部或找到第一个有多个子节点的节点。

    // 如果我们应该尝试优化删除后的树，则将设置为1。
    int trycompress = 0; /* Will be set to 1 if we should try to optimize the
                            tree resulting from the deletion. */

    // 如果是没有子节点的节点
    if (h->size == 0) {
        debugf("Key deleted in node without children. Cleanup needed.\n");
        raxNode *child = NULL;
        // 如果不是根节点
        while(h != rax->head) {
            child = h;
            debugf("Freeing child %p [%.*s] key:%d\n", (void*)child,
                (int)child->size, (char*)child->data, child->iskey);
            // 释放节点
            rax_free(child);
            // 减少节点数
            rax->numnodes--;
            // 向上遍历节点
            h = raxStackPop(&ts);
             /* If this node has more then one child, or actually holds
              * a key, stop here. */
             // 如果保存了一个键值，或者是一个有多个子节点的非压缩节点
            if (h->iskey || (!h->iscompr && h->size != 1)) break;
        }
        // 如果子节点存在
        if (child) {
            debugf("Unlinking child %p from parent %p\n",
                (void*)child, (void*)h);
            // 从父节点中删除
            raxNode *new = raxRemoveChild(h,child);
            if (new != h) {
                raxNode *parent = raxStackPeek(&ts);
                raxNode **parentlink;
                if (parent == NULL) {
                    parentlink = &rax->head;
                } else {
                    parentlink = raxFindParentLink(parent,h);
                }
                memcpy(parentlink,&new,sizeof(new));
            }

            /* If after the removal the node has just a single child
             * and is not a key, we need to try to compress it. */
            // 如果删除节点后，只有一个子节点，尝试压缩
            if (new->size == 1 && new->iskey == 0) {
                trycompress = 1;
                h = new;
            }
        }
    } 
    // 如果节点只有一个子节点，则在移除key之后，可能会对相邻节点进行进一步压缩。
    else if (h->size == 1) {
        /* If the node had just one child, after the removal of the key
         * further compression with adjacent nodes is potentially possible. */
        trycompress = 1;
    }

    /* Don't try node compression if our nodes pointers stack is not
     * complete because of OOM while executing raxLowWalk() */
    // 如果在执行raxLowWalk（）时由于OOM而导致节点指针堆栈不完整，请不要尝试节点压缩
    if (trycompress && ts.oom) trycompress = 0;

    /* Recompression: if trycompress is true, 'h' points to a radix tree node
     * that changed in a way that could allow to compress nodes in this
     * sub-branch. Compressed nodes represent chains of nodes that are not
     * keys and have a single child, so there are two deletion events that
     * may alter the tree so that further compression is needed:
     * 重新压缩：如果trycompression为true，则“h”指向一个基数树节点，
     * 该节点以允许压缩该分支中的节点的方式进行了更改。压缩节点表示不是键的节点链，
     * 并且只有一个子节点，因此有两个删除事件可能会改变树，因此需要进一步压缩：
     * 
     * 1) A node with a single child was a key and now no longer is a key.
     * 2) A node with two children now has just one child.
     * 1）一个节点只有一个子节点，原来是一个键节点，现在不再是一个键节点了。
     * 2) 一个节点原来有两个子节点，现在只有一个了
     *
     * We try to navigate upward till there are other nodes that can be
     * compressed, when we reach the upper node which is not a key and has
     * a single child, we scan the chain of children to collect the
     * compressible part of the tree, and replace the current node with the
     * new one, fixing the child pointer to reference the first non
     * compressible node.
     * 我们试图向上导航，直到有其他可以压缩的节点，当我们到达不是键且只有一个子节点的上层节点时，
     * 我们扫描子节点链以收集树的可压缩部分，并用新节点替换当前节点，固定子指针以引用第一个不可压缩节点。
     * 
     * Example of case "1". A tree stores the keys "FOO" = 1 and
     * "FOOBAR" = 2:
     * 案例“1”的示例。一个树存储键“FOO”=1和“FOOBAR”=2：
     *
     * "FOO" -> "BAR" -> [] (2)
     *           (1)
     *
     * After the removal of "FOO" the tree can be compressed as:
     * 删除"FOO"以后树可以压缩成如下：
     * "FOOBAR" -> [] (2)
     *
     *
     * Example of case "2". A tree stores the keys "FOOBAR" = 1 and
     * "FOOTER" = 2:
     * 案例2的示例：一棵树存储键“FOOBAR”=1和“FOOTER”=2
     * 
     *          |B| -> "AR" -> [] (1)
     * "FOO" -> |-|
     *          |T| -> "ER" -> [] (2)
     *
     * After the removal of "FOOTER" the resulting tree is:
     * 删除“FOOTER”以后，结果树变成了如下：
     * "FOO" -> |B| -> "AR" -> [] (1)
     *
     * That can be compressed into:
     * 可以压缩成一个节点：
     * 
     * "FOOBAR" -> [] (1)
     */
    // 尝试压缩
    if (trycompress) {
        debugf("After removing %.*s:\n", (int)len, s);
        debugnode("Compression may be needed",h);
        debugf("Seek start node\n");

        /* Try to reach the upper node that is compressible.
         * At the end of the loop 'h' will point to the first node we
         * can try to compress and 'parent' to its parent. */
        raxNode *parent;
        while(1) {
            parent = raxStackPop(&ts);
            // 如果是键节点或者是子节点大于1的非压缩节点
            if (!parent || parent->iskey ||
                (!parent->iscompr && parent->size != 1)) break;
            h = parent;
            debugnode("Going up to",h);
        }
        raxNode *start = h; /* Compression starting node. */

        /* Scan chain of nodes we can compress. */
        size_t comprsize = h->size;
        int nodes = 1;
        // 扫描可以压缩的一连串节点
        while(h->size != 0) {
            raxNode **cp = raxNodeLastChildPtr(h);
            memcpy(&h,cp,sizeof(h));
            if (h->iskey || (!h->iscompr && h->size != 1)) break;
            /* Stop here if going to the next node would result into
             * a compressed node larger than h->size can hold. */
            // 如果转到下一个节点会导致压缩节点大于h->size所能容纳的大小，请在此停止
            if (comprsize + h->size > RAX_NODE_MAX_SIZE) break;
            nodes++;
            comprsize += h->size;
        }
        // 可以压缩的节点大于1
        if (nodes > 1) {
            /* If we can compress, create the new node and populate it. */
            // 如果我们可以压缩，创建新节点并填充它
            // 计算节点尺寸
            size_t nodesize =
                sizeof(raxNode)+comprsize+raxPadding(comprsize)+sizeof(raxNode*);
            // 分配节点
            raxNode *new = rax_malloc(nodesize);
            /* An out of memory here just means we cannot optimize this
             * node, but the tree is left in a consistent state. */
            if (new == NULL) {
                raxStackFree(&ts);
                return 1;
            }
            // 初始化节点
            new->iskey = 0;
            new->isnull = 0;
            new->iscompr = 1;
            new->size = comprsize;
            rax->numnodes++;

            /* Scan again, this time to populate the new node content and
             * to fix the new node child pointer. At the same time we free
             * all the nodes that we'll no longer use. */
            // 再次扫描，这一次是为了填充新的节点内容并修复新的节点子指针。同时，我们释放了所有不再使用的节点。
            comprsize = 0;
            h = start;
            // 遍历，开始压缩
            while(h->size != 0) {
                // 拷贝数据
                memcpy(new->data+comprsize,h->data,h->size);
                comprsize += h->size;
                // 得到子节点
                raxNode **cp = raxNodeLastChildPtr(h);
                raxNode *tofree = h;
                memcpy(&h,cp,sizeof(h));
                // 释放当前节点，并且减少节点数
                rax_free(tofree); rax->numnodes--;
                if (h->iskey || (!h->iscompr && h->size != 1)) break;
            }
            debugnode("New node",new);

            /* Now 'h' points to the first node that we still need to use,
             * so our new node child pointer will point to it. */
            // 现在“h”指向我们仍然需要使用的第一个节点，所以我们的新节点子指针将指向它
            raxNode **cp = raxNodeLastChildPtr(new);
            memcpy(cp,&h,sizeof(h));

            /* Fix parent link. */
            // 修复父链接
            if (parent) {
                raxNode **parentlink = raxFindParentLink(parent,start);
                memcpy(parentlink,&new,sizeof(new));
            } else {
                rax->head = new;
            }

            debugf("Compressed %d nodes, %d total bytes\n",
                nodes, (int)comprsize);
        }
    }
    // 释放堆栈
    raxStackFree(&ts);
    return 1;
}

/* This is the core of raxFree(): performs a depth-first scan of the
 * tree and releases all the nodes found. */
void raxRecursiveFree(rax *rax, raxNode *n, void (*free_callback)(void*)) {
    debugnode("free traversing",n);
    int numchildren = n->iscompr ? 1 : n->size;
    raxNode **cp = raxNodeLastChildPtr(n);
    while(numchildren--) {
        raxNode *child;
        memcpy(&child,cp,sizeof(child));
        raxRecursiveFree(rax,child,free_callback);
        cp--;
    }
    debugnode("free depth-first",n);
    if (free_callback && n->iskey && !n->isnull)
        free_callback(raxGetData(n));
    rax_free(n);
    rax->numnodes--;
}

/* Free a whole radix tree, calling the specified callback in order to
 * free the auxiliary data. */
void raxFreeWithCallback(rax *rax, void (*free_callback)(void*)) {
    raxRecursiveFree(rax,rax->head,free_callback);
    assert(rax->numnodes == 0);
    rax_free(rax);
}

/* Free a whole radix tree. */
void raxFree(rax *rax) {
    raxFreeWithCallback(rax,NULL);
}

/* ------------------------------- Iterator --------------------------------- */

/* Initialize a Rax iterator. This call should be performed a single time
 * to initialize the iterator, and must be followed by a raxSeek() call,
 * otherwise the raxPrev()/raxNext() functions will just return EOF. */
 // 初始化Rax迭代器。这个调用应该执行一次来初始化迭代器，并且必须后跟一个raxSeek（）调用，
 // 否则raxRev（）/raxNext（）函数将只返回EOF。
void raxStart(raxIterator *it, rax *rt) {
    it->flags = RAX_ITER_EOF; /* No crash if the iterator is not seeked. */
    it->rt = rt;
    it->key_len = 0;
    it->key = it->key_static_string;
    it->key_max = RAX_ITER_STATIC_LEN;
    it->data = NULL;
    it->node_cb = NULL;
    // 初始化路径堆栈
    raxStackInit(&it->stack);
}

/* Append characters at the current key string of the iterator 'it'. This
 * is a low level function used to implement the iterator, not callable by
 * the user. Returns 0 on out of memory, otherwise 1 is returned. */
// 在迭代器it的键字符串后面附加字符。这是一个用于实现迭代器的低级函数，不能被用户调用。
// 内存不足返回0，否则返回1。
int raxIteratorAddChars(raxIterator *it, unsigned char *s, size_t len) {
    // 长度为0，直接成功
    if (len == 0) return 1;
    // 超过当前键的容量
    if (it->key_max < it->key_len+len) {
        // 是否用的默认的字符串缓存
        unsigned char *old = (it->key == it->key_static_string) ? NULL :
                                                                  it->key;
        // 成倍增加
        size_t new_max = (it->key_len+len)*2;
        // 重新分配
        it->key = rax_realloc(old,new_max);
        // 如果用的默认的字符串缓存
        if (it->key == NULL) {
            // 分配不成功，就还原
            it->key = (!old) ? it->key_static_string : old;
            errno = ENOMEM;
            return 0;
        }
        // 将原来的拷贝进新的
        if (old == NULL) memcpy(it->key,it->key_static_string,it->key_len);
        it->key_max = new_max;
    }
    /* Use memmove since there could be an overlap between 's' and
     * it->key when we use the current key in order to re-seek. */
    // 将键附加在原来键的后面
    memmove(it->key+it->key_len,s,len);
    // 增加键的长度
    it->key_len += len;
    return 1;
}

/* Remove the specified number of chars from the right of the current
 * iterator key. */
void raxIteratorDelChars(raxIterator *it, size_t count) {
    it->key_len -= count;
}

/* Do an iteration step towards the next element. At the end of the step the
 * iterator key will represent the (new) current key. If it is not possible
 * to step in the specified direction since there are no longer elements, the
 * iterator is flagged with RAX_ITER_EOF.
 * 朝着下一个元素执行迭代步骤。在步骤结束时，迭代器键key表示（新的）当前key。
 * 如果由于不再有元素而无法沿指定方向前进，则迭代器将标记为RAX_ITER_EOF。
 * 
 * If 'noup' is true the function starts directly scanning for the next
 * lexicographically smaller children, and the current node is already assumed
 * to be the parent of the last key node, so the first operation to go back to
 * the parent will be skipped. This option is used by raxSeek() when
 * implementing seeking a non existing element with the ">" or "<" options:
 * the starting node is not a key in that particular case, so we start the scan
 * from a node that does not represent the key set.
 * 如果“noup”为true，则函数将开始直接扫描下一个字典较小的子节点，并且当前节点已被假定为最后一个关键节点的父节点，
 * 因此将跳过返回父节点的第一个操作。raxSeek（）在使用“>”或“<”选项实现查找不存在的元素时使用此选项：在特定情况下，
 * 起始节点不是键，因此我们从不代表key集的节点开始扫描。
 * 
 * The function returns 1 on success or 0 on out of memory. 
 * 如果成功返回1，内存不足返回0
 * */

int raxIteratorNextStep(raxIterator *it, int noup) {
    // 已经到末尾了，直接返回1
    if (it->flags & RAX_ITER_EOF) {
        return 1;
    } 
    // 如果是RAX_ITER_JUST_SEEKED，直接清除标记就行
    else if (it->flags & RAX_ITER_JUST_SEEKED) {
        it->flags &= ~RAX_ITER_JUST_SEEKED;
        return 1;
    }

    /* Save key len, stack items and the node where we are currently
     * so that on iterator EOF we can restore the current key and state. */
    // 保存key的长度、堆栈的数目和我们当前所在的节点，以便在迭代器EOF上恢复当前key和状态。
    size_t orig_key_len = it->key_len;
    size_t orig_stack_items = it->stack.items;
    raxNode *orig_node = it->node;

    while(1) {
        // 子节点的数目
        int children = it->node->iscompr ? 1 : it->node->size;
        // 如果noup为flase，并且有子节点
        if (!noup && children) {
            debugf("GO DEEPER\n");
            /* Seek the lexicographically smaller key in this subtree, which
             * is the first one found always going towards the first child
             * of every successive node. */
            // 在这个子树中查找字典上较小的key，这是第一个总是指向每个连续节点的第一个子节点的key。
            if (!raxStackPush(&it->stack,it->node)) return 0;
            // 找到第一个子节点
            raxNode **cp = raxNodeFirstChildPtr(it->node);
            // 加入节点的键
            if (!raxIteratorAddChars(it,it->node->data,
                it->node->iscompr ? it->node->size : 1)) return 0;
            // 设置为当前节点
            memcpy(&it->node,cp,sizeof(it->node));
            /* Call the node callback if any, and replace the node pointer
             * if the callback returns true. */
            // 如果有节点的回调，调用回调
            if (it->node_cb && it->node_cb(&it->node))
                memcpy(cp,&it->node,sizeof(it->node));
            /* For "next" step, stop every time we find a key along the
             * way, since the key is lexicographically smaller compared to
             * what follows in the sub-children. */
            // 对于“下一步”，每次我们找到一个键时都要停止，因为这个键在字典上比子级中的键要小。
            if (it->node->iskey) {
                it->data = raxGetData(it->node);
                return 1;
            }
        } else {
            /* If we finished exploring the previous sub-tree, switch to the
             * new one: go upper until a node is found where there are
             * children representing keys lexicographically greater than the
             * current key. */
            // 如果我们完成了对前一子树的探索，请切换到新的子树：向上移动，
            // 直到找到一个节点，其中有子节点以字典方式表示大于当前关键字的关键字。
            while(1) {
                int old_noup = noup;

                /* Already on head? Can't go up, iteration finished. */
                // 如果已经到根节点了，不能再往上了，遍历结束
                if (!noup && it->node == it->rt->head) {
                    it->flags |= RAX_ITER_EOF;
                    it->stack.items = orig_stack_items;
                    it->key_len = orig_key_len;
                    it->node = orig_node;
                    return 1;
                }
                /* If there are no children at the current node, try parent's
                 * next child. */
                // 如果当前节点上没有子节点，请尝试父节点的下一个子节点
                unsigned char prevchild = it->key[it->key_len-1];
                if (!noup) {
                    it->node = raxStackPop(&it->stack);
                } else {
                    noup = 0;
                }
                /* Adjust the current key to represent the node we are
                 * at. */
                // 调整当前key以表示我们所处的节点。
                int todel = it->node->iscompr ? it->node->size : 1;
                // 从迭代器的键中删除指定数量的字符
                raxIteratorDelChars(it,todel);

                /* Try visiting the next child if there was at least one
                 * additional child. */
                // 如果有至少一个额外的子节点，尝试访问下一个子节点
                if (!it->node->iscompr && it->node->size > (old_noup ? 0 : 1)) {
                    raxNode **cp = raxNodeFirstChildPtr(it->node);
                    int i = 0;
                    // 找到小于prevchild的最大值
                    while (i < it->node->size) {
                        debugf("SCAN NEXT %c\n", it->node->data[i]);
                        if (it->node->data[i] > prevchild) break;
                        i++;
                        cp++;
                    }
                    // 找到新的节点
                    if (i != it->node->size) {
                        debugf("SCAN found a new node\n");
                        // 将节点的键加入
                        raxIteratorAddChars(it,it->node->data+i,1);
                        if (!raxStackPush(&it->stack,it->node)) return 0;
                        // 将找到的节点设置为当前节点
                        memcpy(&it->node,cp,sizeof(it->node));
                        /* Call the node callback if any, and replace the node
                         * pointer if the callback returns true. */
                        // 调用回调
                        if (it->node_cb && it->node_cb(&it->node))
                            memcpy(cp,&it->node,sizeof(it->node));
                        // 如果是键节点，获取值
                        if (it->node->iskey) {
                            it->data = raxGetData(it->node);
                            return 1;
                        }
                        break;
                    }
                }
            }
        }
    }
}

/* Seek the greatest key in the subtree at the current node. Return 0 on
 * out of memory, otherwise 1. This is a helper function for different
 * iteration functions below. */
int raxSeekGreatest(raxIterator *it) {
    while(it->node->size) {
        if (it->node->iscompr) {
            if (!raxIteratorAddChars(it,it->node->data,
                it->node->size)) return 0;
        } else {
            if (!raxIteratorAddChars(it,it->node->data+it->node->size-1,1))
                return 0;
        }
        raxNode **cp = raxNodeLastChildPtr(it->node);
        if (!raxStackPush(&it->stack,it->node)) return 0;
        memcpy(&it->node,cp,sizeof(it->node));
    }
    return 1;
}

/* Like raxIteratorNextStep() but implements an iteration step moving
 * to the lexicographically previous element. The 'noup' option has a similar
 * effect to the one of raxIteratorNextStep(). */
// 类似raxIteratorNextStep，但是实现一个迭代器移动到字典的额上一个元素。
// noup选项有着和raxIteratorNextStep函数相似的效果。
int raxIteratorPrevStep(raxIterator *it, int noup) {
    // 已经到末尾了，直接返回1
    if (it->flags & RAX_ITER_EOF) {
        return 1;
    } 
    // 如果是RAX_ITER_JUST_SEEKED，直接清除标记就行
    else if (it->flags & RAX_ITER_JUST_SEEKED) {
        it->flags &= ~RAX_ITER_JUST_SEEKED;
        return 1;
    }

    /* Save key len, stack items and the node where we are currently
     * so that on iterator EOF we can restore the current key and state. */
    // 保存key的长度、堆栈的数目和我们当前所在的节点，以便在迭代器EOF上恢复当前key和状态。
    size_t orig_key_len = it->key_len;
    size_t orig_stack_items = it->stack.items;
    raxNode *orig_node = it->node;

    while(1) {
        int old_noup = noup;

        /* Already on head? Can't go up, iteration finished. */
        // 如果已经到头了，不能再往上走了，遍历结束
        if (!noup && it->node == it->rt->head) {
            it->flags |= RAX_ITER_EOF;
            it->stack.items = orig_stack_items;
            it->key_len = orig_key_len;
            it->node = orig_node;
            return 1;
        }
        // 前一个子节点
        unsigned char prevchild = it->key[it->key_len-1];
        // 如果noup为false，从栈中弹出一个节点
        if (!noup) {
            it->node = raxStackPop(&it->stack);
        } else {
            noup = 0;
        }

        /* Adjust the current key to represent the node we are
         * at. */
        int todel = it->node->iscompr ? it->node->size : 1;
        raxIteratorDelChars(it,todel);

        /* Try visiting the prev child if there is at least one
         * child. */
        // 如果至少有一个子节点，尝试访问前一个子节点
        if (!it->node->iscompr && it->node->size > (old_noup ? 0 : 1)) {
            raxNode **cp = raxNodeLastChildPtr(it->node);
            int i = it->node->size-1;
            // 找到大于等于prevchild的字符
            while (i >= 0) {
                debugf("SCAN PREV %c\n", it->node->data[i]);
                if (it->node->data[i] < prevchild) break;
                i--;
                cp--;
            }
            /* If we found a new subtree to explore in this node,
             * go deeper following all the last children in order to
             * find the key lexicographically greater. */
            // 如果我们在这个节点中找到了一个新的子树来探索，那么就深入跟踪所有最后的子树，以便在字典中找到更大的关键字
            if (i != -1) {
                debugf("SCAN found a new node\n");
                /* Enter the node we just found. */
                // 进入刚才找到得节点
                if (!raxIteratorAddChars(it,it->node->data+i,1)) return 0;
                if (!raxStackPush(&it->stack,it->node)) return 0;
                memcpy(&it->node,cp,sizeof(it->node));
                /* Seek sub-tree max. */
                // 搜索子树
                if (!raxSeekGreatest(it)) return 0;
            }
        }

        /* Return the key: this could be the key we found scanning a new
         * subtree, or if we did not find a new subtree to explore here,
         * before giving up with this node, check if it's a key itself. */
        // 返回key：这可能是我们在扫描一个新子树时发现的key，或者如果我们没有在这里
        // 找到一个新的子树来探索，在放弃这个节点之前，检查它本身是否是key。
        if (it->node->iskey) {
            // 获取值
            it->data = raxGetData(it->node);
            return 1;
        }
    }
}

/* Seek an iterator at the specified element.
 * Return 0 if the seek failed for syntax error or out of memory. Otherwise
 * 1 is returned. When 0 is returned for out of memory, errno is set to
 * the ENOMEM value. */
// 在指定元素处查找迭代器。如果由于语法错误或内存不足而查找失败，
// 则返回0。否则返回1。当由于内存不足而返回0时，errno将设置为ENOMEM值。
int raxSeek(raxIterator *it, const char *op, unsigned char *ele, size_t len) {
    int eq = 0, lt = 0, gt = 0, first = 0, last = 0;

    // 初始化迭代器
    it->stack.items = 0; /* Just resetting. Initialized by raxStart(). */
    it->flags |= RAX_ITER_JUST_SEEKED;
    it->flags &= ~RAX_ITER_EOF;
    it->key_len = 0;
    it->node = NULL;

    /* Set flags according to the operator used to perform the seek. */
    // 根据用于执行查找的运算符设置标志
    if (op[0] == '>') {
        gt = 1;
        if (op[1] == '=') eq = 1;
    } else if (op[0] == '<') {
        lt = 1;
        if (op[1] == '=') eq = 1;
    } else if (op[0] == '=') {
        eq = 1;
    } else if (op[0] == '^') {
        first = 1;
    } else if (op[0] == '$') {
        last = 1;
    } else {
        errno = 0;
        return 0; /* Error. */
    }

    /* If there are no elements, set the EOF condition immediately and
     * return. */
    // 如果没键，直接设置EOF条件并且返回
    if (it->rt->numele == 0) {
        it->flags |= RAX_ITER_EOF;
        return 1;
    }

    
    if (first) {
        /* Seeking the first key greater or equal to the empty string
         * is equivalent to seeking the smaller key available. */
        // 查找第一个key大于或等于空字符串等价于查找可用的较小key。
        return raxSeek(it,">=",NULL,0);
    }

    // 
    if (last) {
        /* Find the greatest key taking always the last child till a
         * final node is found. */
        // 查找最大的键总是取最后一个子节点直到最终的节点被找到。
        it->node = it->rt->head;
        if (!raxSeekGreatest(it)) return 0;
        assert(it->node->iskey);
        // 得到节点的数据
        it->data = raxGetData(it->node);
        return 1;
    }

    /* We need to seek the specified key. What we do here is to actually
     * perform a lookup, and later invoke the prev/next key code that
     * we already use for iteration. */
    int splitpos = 0;
    // 查找指定的键
    size_t i = raxLowWalk(it->rt,ele,len,&it->node,NULL,&splitpos,&it->stack);

    /* Return OOM on incomplete stack info. */
    // 不完整的栈信息返回OOM
    if (it->stack.oom) return 0;

    // 完全匹配
    if (eq && i == len && (!it->node->iscompr || splitpos == 0) &&
        it->node->iskey)
    {
        /* We found our node, since the key matches and we have an
         * "equal" condition. */
        // 我们找到了我们的节点，因为键匹配，我们有一个“相等”的条件。
        if (!raxIteratorAddChars(it,ele,len)) return 0; /* OOM. */
        // 得到节点的数据
        it->data = raxGetData(it->node);
    } else if (lt || gt) {
        /* Exact key not found or eq flag not set. We have to set as current
         * key the one represented by the node we stopped at, and perform
         * a next/prev operation to seek. */
        // 找不到精确key或未设置eq标志。我们必须将停止的节点所代表的key设置为当前key，并执行next/prev操作进行查找。
        raxIteratorAddChars(it, ele, i-splitpos);

        /* We need to set the iterator in the correct state to call next/prev
         * step in order to seek the desired element. */
        debugf("After initial seek: i=%d len=%d key=%.*s\n",
            (int)i, (int)len, (int)it->key_len, it->key);
        // 如果字符串没有搜索完，并且节点是非压缩节点
        if (i != len && !it->node->iscompr) {
            /* If we stopped in the middle of a normal node because of a
             * mismatch, add the mismatching character to the current key
             * and call the iterator with the 'noup' flag so that it will try
             * to seek the next/prev child in the current node directly based
             * on the mismatching character. */
            // 如果由于不匹配而停止在正常节点的中间，请将不匹配字符添加到当前键，
            // 并使用“noup”标记调用迭代器，这样它将尝试直接基于不匹配字符在当前节点中查找下一个/prev子级
            if (!raxIteratorAddChars(it,ele+i,1)) return 0;
            debugf("Seek normal node on mismatch: %.*s\n",
                (int)it->key_len, (char*)it->key);

            it->flags &= ~RAX_ITER_JUST_SEEKED;
            if (lt && !raxIteratorPrevStep(it,1)) return 0;
            if (gt && !raxIteratorNextStep(it,1)) return 0;
            // 忽略next调用
            it->flags |= RAX_ITER_JUST_SEEKED; /* Ignore next call. */
        } else if (i != len && it->node->iscompr) {
            debugf("Compressed mismatch: %.*s\n",
                (int)it->key_len, (char*)it->key);
            /* In case of a mismatch within a compressed node. */
            // 在压缩节点内不匹配的情况下
            int nodechar = it->node->data[splitpos];
            int keychar = ele[i];
            it->flags &= ~RAX_ITER_JUST_SEEKED;
            // 大于
            if (gt) {
                /* If the key the compressed node represents is greater
                 * than our seek element, continue forward, otherwise set the
                 * state in order to go back to the next sub-tree. */
                // 如果压缩节点表示的key大于我们的查找元素，则继续前进，否则设置状态以返回到下一个子树。
                if (nodechar > keychar) {
                    if (!raxIteratorNextStep(it,0)) return 0;
                } else {
                    // 将键附加到当前键后面
                    if (!raxIteratorAddChars(it,it->node->data,it->node->size))
                        return 0;
                    // 遍历下一个元素
                    if (!raxIteratorNextStep(it,1)) return 0;
                }
            }
            // 小于
            if (lt) {
                /* If the key the compressed node represents is smaller
                 * than our seek element, seek the greater key in this
                 * subtree, otherwise set the state in order to go back to
                 * the previous sub-tree. */
                if (nodechar < keychar) {
                    if (!raxSeekGreatest(it)) return 0;
                    it->data = raxGetData(it->node);
                } else {
                    if (!raxIteratorAddChars(it,it->node->data,it->node->size))
                        return 0;
                    if (!raxIteratorPrevStep(it,1)) return 0;
                }
            }
            it->flags |= RAX_ITER_JUST_SEEKED; /* Ignore next call. */
        } 
        // 不匹配
        else {
            debugf("No mismatch: %.*s\n",
                (int)it->key_len, (char*)it->key);
            /* If there was no mismatch we are into a node representing the
             * key, (but which is not a key or the seek operator does not
             * include 'eq'), or we stopped in the middle of a compressed node
             * after processing all the key. Continue iterating as this was
             * a legitimate key we stopped at. */
            it->flags &= ~RAX_ITER_JUST_SEEKED;
            // 如果是带键的压缩节点
            if (it->node->iscompr && it->node->iskey && splitpos && lt) {
                /* If we stopped in the middle of a compressed node with
                 * perfect match, and the condition is to seek a key "<" than
                 * the specified one, then if this node is a key it already
                 * represents our match. For instance we may have nodes:
                 *
                 * "f" -> "oobar" = 1 -> "" = 2
                 *
                 * Representing keys "f" = 1, "foobar" = 2. A seek for
                 * the key < "foo" will stop in the middle of the "oobar"
                 * node, but will be our match, representing the key "f".
                 *
                 * So in that case, we don't seek backward. */
                it->data = raxGetData(it->node);
            } 
            else {
                if (gt && !raxIteratorNextStep(it,0)) return 0;
                if (lt && !raxIteratorPrevStep(it,0)) return 0;
            }
            it->flags |= RAX_ITER_JUST_SEEKED; /* Ignore next call. */
        }
    } else {
        /* If we are here just eq was set but no match was found. */
        // 如果找相等的，但是没有找到匹配的节点
        it->flags |= RAX_ITER_EOF;
        return 1;
    }
    return 1;
}

/* Go to the next element in the scope of the iterator 'it'.
 * If EOF (or out of memory) is reached, 0 is returned, otherwise 1 is
 * returned. In case 0 is returned because of OOM, errno is set to ENOMEM. */
// 转到迭代器“it”作用域中的下一个元素。如果达到EOF（或内存不足），则返回0，否则返回1
// 如果由于OOM而返回0，则将errno设置为ENOMEM
int raxNext(raxIterator *it) {
    if (!raxIteratorNextStep(it,0)) {
        errno = ENOMEM;
        return 0;
    }
    if (it->flags & RAX_ITER_EOF) {
        errno = 0;
        return 0;
    }
    return 1;
}

/* Go to the previous element in the scope of the iterator 'it'.
 * If EOF (or out of memory) is reached, 0 is returned, otherwise 1 is
 * returned. In case 0 is returned because of OOM, errno is set to ENOMEM. */
// 转到迭代器it作用域下的前一个元素。如果达到EOF（或者内存不足）则返回0，否则返回1
// 如果由于OOM而返回0，则将errno设置为ENOMEM
int raxPrev(raxIterator *it) {
    if (!raxIteratorPrevStep(it,0)) {
        errno = ENOMEM;
        return 0;
    }
    if (it->flags & RAX_ITER_EOF) {
        errno = 0;
        return 0;
    }
    return 1;
}

/* Perform a random walk starting in the current position of the iterator.
 * Return 0 if the tree is empty or on out of memory. Otherwise 1 is returned
 * and the iterator is set to the node reached after doing a random walk
 * of 'steps' steps. If the 'steps' argument is 0, the random walk is performed
 * using a random number of steps between 1 and two times the logarithm of
 * the number of elements.
 *
 * NOTE: if you use this function to generate random elements from the radix
 * tree, expect a disappointing distribution. A random walk produces good
 * random elements if the tree is not sparse, however in the case of a radix
 * tree certain keys will be reported much more often than others. At least
 * this function should be able to explore every possible element eventually. */
int raxRandomWalk(raxIterator *it, size_t steps) {
    if (it->rt->numele == 0) {
        it->flags |= RAX_ITER_EOF;
        return 0;
    }

    if (steps == 0) {
        size_t fle = 1+floor(log(it->rt->numele));
        fle *= 2;
        steps = 1 + rand() % fle;
    }

    raxNode *n = it->node;
    while(steps > 0 || !n->iskey) {
        int numchildren = n->iscompr ? 1 : n->size;
        int r = rand() % (numchildren+(n != it->rt->head));

        if (r == numchildren) {
            /* Go up to parent. */
            n = raxStackPop(&it->stack);
            int todel = n->iscompr ? n->size : 1;
            raxIteratorDelChars(it,todel);
        } else {
            /* Select a random child. */
            if (n->iscompr) {
                if (!raxIteratorAddChars(it,n->data,n->size)) return 0;
            } else {
                if (!raxIteratorAddChars(it,n->data+r,1)) return 0;
            }
            raxNode **cp = raxNodeFirstChildPtr(n)+r;
            if (!raxStackPush(&it->stack,n)) return 0;
            memcpy(&n,cp,sizeof(n));
        }
        if (n->iskey) steps--;
    }
    it->node = n;
    it->data = raxGetData(it->node);
    return 1;
}

/* Compare the key currently pointed by the iterator to the specified
 * key according to the specified operator. Returns 1 if the comparison is
 * true, otherwise 0 is returned. */
int raxCompare(raxIterator *iter, const char *op, unsigned char *key, size_t key_len) {
    int eq = 0, lt = 0, gt = 0;

    if (op[0] == '=' || op[1] == '=') eq = 1;
    if (op[0] == '>') gt = 1;
    else if (op[0] == '<') lt = 1;
    else if (op[1] != '=') return 0; /* Syntax error. */

    size_t minlen = key_len < iter->key_len ? key_len : iter->key_len;
    int cmp = memcmp(iter->key,key,minlen);

    /* Handle == */
    if (lt == 0 && gt == 0) return cmp == 0 && key_len == iter->key_len;

    /* Handle >, >=, <, <= */
    if (cmp == 0) {
        /* Same prefix: longer wins. */
        if (eq && key_len == iter->key_len) return 1;
        else if (lt) return iter->key_len < key_len;
        else if (gt) return iter->key_len > key_len;
        else return 0; /* Avoid warning, just 'eq' is handled before. */
    } else if (cmp > 0) {
        return gt ? 1 : 0;
    } else /* (cmp < 0) */ {
        return lt ? 1 : 0;
    }
}

/* Free the iterator. */
// 释放迭代器
void raxStop(raxIterator *it) {
    if (it->key != it->key_static_string) rax_free(it->key);
    // 释放遍历栈
    raxStackFree(&it->stack);
}

/* Return if the iterator is in an EOF state. This happens when raxSeek()
 * failed to seek an appropriate element, so that raxNext() or raxPrev()
 * will return zero, or when an EOF condition was reached while iterating
 * with raxNext() and raxPrev(). */
// 返回迭代器是否处于EOF状态。当raxSeek()未能找到适当的元素，从而raxNext()或raxRev()将返回零时，
// 或者当使用raxNext()和raxRev()迭代时达到EOF条件时，就会发生这种情况。
int raxEOF(raxIterator *it) {
    return it->flags & RAX_ITER_EOF;
}

/* Return the number of elements inside the radix tree. */
uint64_t raxSize(rax *rax) {
    return rax->numele;
}

/* ----------------------------- Introspection ------------------------------ */

/* This function is mostly used for debugging and learning purposes.
 * It shows an ASCII representation of a tree on standard output, outline
 * all the nodes and the contained keys.
 *
 * The representation is as follow:
 *
 *  "foobar" (compressed node)
 *  [abc] (normal node with three children)
 *  [abc]=0x12345678 (node is a key, pointing to value 0x12345678)
 *  [] (a normal empty node)
 *
 *  Children are represented in new indented lines, each children prefixed by
 *  the "`-(x)" string, where "x" is the edge byte.
 *
 *  [abc]
 *   `-(a) "ladin"
 *   `-(b) [kj]
 *   `-(c) []
 *
 *  However when a node has a single child the following representation
 *  is used instead:
 *
 *  [abc] -> "ladin" -> []
 */

/* The actual implementation of raxShow(). */
void raxRecursiveShow(int level, int lpad, raxNode *n) {
    char s = n->iscompr ? '"' : '[';
    char e = n->iscompr ? '"' : ']';

    int numchars = printf("%c%.*s%c", s, n->size, n->data, e);
    if (n->iskey) {
        numchars += printf("=%p",raxGetData(n));
    }

    int numchildren = n->iscompr ? 1 : n->size;
    /* Note that 7 and 4 magic constants are the string length
     * of " `-(x) " and " -> " respectively. */
    if (level) {
        lpad += (numchildren > 1) ? 7 : 4;
        if (numchildren == 1) lpad += numchars;
    }
    raxNode **cp = raxNodeFirstChildPtr(n);
    for (int i = 0; i < numchildren; i++) {
        char *branch = " `-(%c) ";
        if (numchildren > 1) {
            printf("\n");
            for (int j = 0; j < lpad; j++) putchar(' ');
            printf(branch,n->data[i]);
        } else {
            printf(" -> ");
        }
        raxNode *child;
        memcpy(&child,cp,sizeof(child));
        raxRecursiveShow(level+1,lpad,child);
        cp++;
    }
}

/* Show a tree, as outlined in the comment above. */
void raxShow(rax *rax) {
    raxRecursiveShow(0,0,rax->head);
    putchar('\n');
}

/* Used by debugnode() macro to show info about a given node. */
void raxDebugShowNode(const char *msg, raxNode *n) {
    if (raxDebugMsg == 0) return;
    printf("%s: %p [%.*s] key:%u size:%u children:",
        msg, (void*)n, (int)n->size, (char*)n->data, n->iskey, n->size);
    int numcld = n->iscompr ? 1 : n->size;
    raxNode **cldptr = raxNodeLastChildPtr(n) - (numcld-1);
    while(numcld--) {
        raxNode *child;
        memcpy(&child,cldptr,sizeof(child));
        cldptr++;
        printf("%p ", (void*)child);
    }
    printf("\n");
    fflush(stdout);
}

/* Touch all the nodes of a tree returning a check sum. This is useful
 * in order to make Valgrind detect if there is something wrong while
 * reading the data structure.
 *
 * This function was used in order to identify Rax bugs after a big refactoring
 * using this technique:
 *
 * 1. The rax-test is executed using Valgrind, adding a printf() so that for
 *    the fuzz tester we see what iteration in the loop we are in.
 * 2. After every modification of the radix tree made by the fuzz tester
 *    in rax-test.c, we add a call to raxTouch().
 * 3. Now as soon as an operation will corrupt the tree, raxTouch() will
 *    detect it (via Valgrind) immediately. We can add more calls to narrow
 *    the state.
 * 4. At this point a good idea is to enable Rax debugging messages immediately
 *    before the moment the tree is corrupted, to see what happens.
 */
unsigned long raxTouch(raxNode *n) {
    debugf("Touching %p\n", (void*)n);
    unsigned long sum = 0;
    if (n->iskey) {
        sum += (unsigned long)raxGetData(n);
    }

    int numchildren = n->iscompr ? 1 : n->size;
    raxNode **cp = raxNodeFirstChildPtr(n);
    int count = 0;
    for (int i = 0; i < numchildren; i++) {
        if (numchildren > 1) {
            sum += (long)n->data[i];
        }
        raxNode *child;
        memcpy(&child,cp,sizeof(child));
        if (child == (void*)0x65d1760) count++;
        if (count > 1) exit(1);
        sum += raxTouch(child);
        cp++;
    }
    return sum;
}
