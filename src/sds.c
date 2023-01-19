/* SDSLib 2.0 -- A C dynamic strings library
 *
 * Copyright (c) 2006-2015, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2015, Oran Agra
 * Copyright (c) 2015, Redis Labs, Inc
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
#include <ctype.h>
#include <assert.h>
#include <limits.h>
#include "sds.h"
#include "sdsalloc.h"

const char *SDS_NOINIT = "SDS_NOINIT";

// 得到各种类型头的尺寸
static inline int sdsHdrSize(char type) {
    switch(type&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
            return sizeof(struct sdshdr5);
        case SDS_TYPE_8:
            return sizeof(struct sdshdr8);
        case SDS_TYPE_16:
            return sizeof(struct sdshdr16);
        case SDS_TYPE_32:
            return sizeof(struct sdshdr32);
        case SDS_TYPE_64:
            return sizeof(struct sdshdr64);
    }
    return 0;
}

// 根据字符串的长度计算需要的类型
static inline char sdsReqType(size_t string_size) {
    // 如果字符串长度小于32，类型5
    if (string_size < 1<<5)
        return SDS_TYPE_5;
    // 如果字符串长度小于256，类型8
    if (string_size < 1<<8)
        return SDS_TYPE_8;
    // 如果字符串长度小于65536，类型16
    if (string_size < 1<<16)
        return SDS_TYPE_16;
#if (LONG_MAX == LLONG_MAX)
    if (string_size < 1ll<<32)
        return SDS_TYPE_32;
    return SDS_TYPE_64;
#else
    return SDS_TYPE_32;
#endif
}

// 每种类型可以容纳的最大字符串长度
static inline size_t sdsTypeMaxSize(char type) {
    // 类型5的长度为31
    if (type == SDS_TYPE_5)
        return (1<<5) - 1;
    // 类型8的长度为255
    if (type == SDS_TYPE_8)
        return (1<<8) - 1;
    // 类型16的长度为65535
    if (type == SDS_TYPE_16)
        return (1<<16) - 1;
#if (LONG_MAX == LLONG_MAX)
    if (type == SDS_TYPE_32)
        return (1ll<<32) - 1;
#endif
    // 使用最大的类型，如果定义了LLONG_MAX，就是SDS_TYPE_64，否则SDS_TYPE_32
    return -1; /* this is equivalent to the max SDS_TYPE_64 or SDS_TYPE_32 */
}

/* Create a new sds string with the content specified by the 'init' pointer
 * and 'initlen'.
 * If NULL is used for 'init' the string is initialized with zero bytes.
 * If SDS_NOINIT is used, the buffer is left uninitialized;
 *
 * The string is always null-terminated (all the sds strings are, always) so
 * even if you create an sds string with:
 *
 * mystring = sdsnewlen("abc",3);
 *
 * You can print the string with printf() as there is an implicit \0 at the
 * end of the string. However the string is binary safe and can contain
 * \0 characters in the middle, as the length is stored in the sds header. */
// 使用init初始字符串和初始长度创建新的sds字符串
//      init有三种情况，
//      第一种是为空，那新的sds清零，
//      第二种是为SDS_NOINIT，那么表示不需要初始化
//      第三种是不是上面两种，就是指向初始化的东西，那这个值去初始化新创建的sds
sds _sdsnewlen(const void *init, size_t initlen, int trymalloc) {
    void *sh;
    sds s;
    // 根据初始长度得到类型
    char type = sdsReqType(initlen);
    /* Empty strings are usually created in order to append. Use type 8
     * since type 5 is not good at this. */
    // 空串一般创建的目的是为了后期增加内容，所以使用类型8，因为类型5不方便增长
    if (type == SDS_TYPE_5 && initlen == 0) type = SDS_TYPE_8;
    // 根据类型得到头部的长度
    int hdrlen = sdsHdrSize(type);
    unsigned char *fp; /* flags pointer. */
    size_t usable;

    // 判断长度是否溢出，+1表示终止符
    assert(initlen + hdrlen + 1 > initlen); /* Catch size_t overflow */
    // 分配内存
    sh = trymalloc?
        s_trymalloc_usable(hdrlen+initlen+1, &usable) :
        s_malloc_usable(hdrlen+initlen+1, &usable);
    if (sh == NULL) return NULL;
    // 如果初始化的字符串是SDS_NOINIT，那就是不初始化
    if (init==SDS_NOINIT)
        init = NULL;
    // 如果init为NULL，得将分配的内存清零
    else if (!init)
        memset(sh, 0, hdrlen+initlen+1);
    // s是用于保存真实数据的起始处
    s = (char*)sh+hdrlen;
    // 得到类型的flag
    fp = ((unsigned char*)s)-1;
    // 没有使用的长度就是分配的长度-结构头长度-1
    usable = usable-hdrlen-1;
    // 如果没有使用的长度超过了类型可以容纳的最大值，强制设置为最大值
    if (usable > sdsTypeMaxSize(type))
        usable = sdsTypeMaxSize(type);
    // 设置类型对应结构的详细数据
    switch(type) {
        case SDS_TYPE_5: {
            *fp = type | (initlen << SDS_TYPE_BITS);
            break;
        }
        case SDS_TYPE_8: {
            SDS_HDR_VAR(8,s);
            sh->len = initlen;
            sh->alloc = usable;
            *fp = type;
            break;
        }
        case SDS_TYPE_16: {
            SDS_HDR_VAR(16,s);
            sh->len = initlen;
            sh->alloc = usable;
            *fp = type;
            break;
        }
        case SDS_TYPE_32: {
            SDS_HDR_VAR(32,s);
            sh->len = initlen;
            sh->alloc = usable;
            *fp = type;
            break;
        }
        case SDS_TYPE_64: {
            SDS_HDR_VAR(64,s);
            sh->len = initlen;
            sh->alloc = usable;
            *fp = type;
            break;
        }
    }
    // 如果有初始化数据，就初始化字符串
    if (initlen && init)
        memcpy(s, init, initlen);
    // 加上字符串终止符
    s[initlen] = '\0';
    return s;
}

// 创建新的简单字符串(SDS)
sds sdsnewlen(const void *init, size_t initlen) {
    return _sdsnewlen(init, initlen, 0);
}

// 尝试创建新的简单字符串（SDS）
sds sdstrynewlen(const void *init, size_t initlen) {
    return _sdsnewlen(init, initlen, 1);
}

/* Create an empty (zero length) sds string. Even in this case the string
 * always has an implicit null term. */
// 创建一个空的（0长度）sds字符串，即使在这种情况下，字符串总有一个隐式的空项
sds sdsempty(void) {
    return sdsnewlen("",0);
}

/* Create a new sds string starting from a null terminated C string. */
// 从一个C字符串创建一个SDS字符串
sds sdsnew(const char *init) {
    size_t initlen = (init == NULL) ? 0 : strlen(init);
    return sdsnewlen(init, initlen);
}

/* Duplicate an sds string. */
// 复制一个SDS字符串
sds sdsdup(const sds s) {
    return sdsnewlen(s, sdslen(s));
}

/* Free an sds string. No operation is performed if 's' is NULL. */
// 释放一个SDS字符串，
void sdsfree(sds s) {
    if (s == NULL) return;
    s_free((char*)s-sdsHdrSize(s[-1]));
}

/* Set the sds string length to the length as obtained with strlen(), so
 * considering as content only up to the first null term character.
 *
 * This function is useful when the sds string is hacked manually in some
 * way, like in the following example:
 *
 * s = sdsnew("foobar");
 * s[2] = '\0';
 * sdsupdatelen(s);
 * printf("%d\n", sdslen(s));
 *
 * The output will be "2", but if we comment out the call to sdsupdatelen()
 * the output will be "6" as the string was modified but the logical length
 * remains 6 bytes. */
// 通过strlen得到字符串的长度，然后更新到sds的长度，只包含到第一个终止符的内容
void sdsupdatelen(sds s) {
    size_t reallen = strlen(s);
    sdssetlen(s, reallen);
}

/* Modify an sds string in-place to make it empty (zero length).
 * However all the existing buffer is not discarded but set as free space
 * so that next append operations will not require allocations up to the
 * number of bytes previously available. */
// 就地修改sds字符串，使其成为空串
// 然而所有的已经存在的缓冲区没有被丢弃，而是将其设置为空闲空间
// 所有下次追加操作将可能根据以前已经拥有的空闲空间而不需要分配内存
void sdsclear(sds s) {
    sdssetlen(s, 0);
    s[0] = '\0';
}

/* Enlarge the free space at the end of the sds string so that the caller
 * is sure that after calling this function can overwrite up to addlen
 * bytes after the end of the string, plus one more byte for nul term.
 * If there's already sufficient free space, this function returns without any
 * action, if there isn't sufficient free space, it'll allocate what's missing,
 * and possibly more:
 * When greedy is 1, enlarge more than needed, to avoid need for future reallocs
 * on incremental growth.
 * When greedy is 0, enlarge just enough so that there's free space for 'addlen'.
 *
 * Note: this does not change the *length* of the sds string as returned
 * by sdslen(), but only the free buffer space we have. */
// 在sds字符串的尾部扩大空间，以便调用者保证在调用以后能覆写在字符串后面增加的addlen字节，
// 加上额外的一个nul终止符。
// 如果已经拥有足够的空闲空间，该函数没有任何操作直接返回，
// 注意：这不会改变通过 sdslen()返回的 sds 字符串的 *length*，但是会让我们拥有的可用缓冲区空间。
// 意思就是addlen是我们预留的空间，已经使用的空间没有发生变化
// greedy：如果为1，    一：如果新的长度小于最大的预分配的长度，就成倍增长
//                     二：如果超过最大预分配长度，每次都增加预分配长度
sds _sdsMakeRoomFor(sds s, size_t addlen, int greedy) {
    void *sh, *newsh;
    // 得到可用空间的大小
    size_t avail = sdsavail(s);
    size_t len, newlen, reqlen;
    // 新的和老的SDS类型
    char type, oldtype = s[-1] & SDS_TYPE_MASK;
    int hdrlen;
    size_t usable;

    /* Return ASAP if there is enough space left. */
    // 如果后面已经有足够的空间了，就直接返回
    if (avail >= addlen) return s;
    // 目前的长度
    len = sdslen(s);
    // 得到包括头在内的最开始的地方
    sh = (char*)s-sdsHdrSize(oldtype);
    // 新的长度
    reqlen = newlen = (len+addlen);
    // 考虑溢出
    assert(newlen > len);   /* Catch size_t overflow */
    // 是否贪心分配，就是会在后面有分配算法的空闲
    if (greedy == 1) {
        // 如果新的长度小于最大的预分配的长度，就成倍增长
        if (newlen < SDS_MAX_PREALLOC)
            newlen *= 2;
        else
            // 如果超过最大预分配长度，每次都增加预分配长度
            newlen += SDS_MAX_PREALLOC;
    }
    // 新的长度对应的类型
    type = sdsReqType(newlen);

    /* Don't use type 5: the user is appending to the string and type 5 is
     * not able to remember empty space, so sdsMakeRoomFor() must be called
     * at every appending operation. */
    // 如果新的类型为类型5，强制设置为类型8
    // 不使用类型5：用户正在追加字符串，类型5不能记录空闲空间，所以每次追加操作都得调用sdsMakeRoomFor
    if (type == SDS_TYPE_5) type = SDS_TYPE_8;

    // 得到新类型对应头的尺寸
    hdrlen = sdsHdrSize(type);
    // 再一次确认是否溢出
    assert(hdrlen + newlen + 1 > reqlen);  /* Catch size_t overflow */
    // 如果没有更换类型
    if (oldtype==type) {
        newsh = s_realloc_usable(sh, hdrlen+newlen+1, &usable);
        if (newsh == NULL) return NULL;
        s = (char*)newsh+hdrlen;
    } else {
        /* Since the header size changes, need to move the string forward,
         * and can't use realloc */
        // 由于头的尺寸变化，需要将字符串前移，所以不能使用realloc
        newsh = s_malloc_usable(hdrlen+newlen+1, &usable);
        if (newsh == NULL) return NULL;
        // 将数据拷贝进去
        memcpy((char*)newsh+hdrlen, s, len+1);
        // 将原来的释放
        s_free(sh);
        // 设置头的信息
        s = (char*)newsh+hdrlen;
        s[-1] = type;
        sdssetlen(s, len);
    }
    // 设置新的分配长度
    usable = usable-hdrlen-1;
    // 得到类型最大的尺寸
    if (usable > sdsTypeMaxSize(type))
        usable = sdsTypeMaxSize(type);
    sdssetalloc(s, usable);
    return s;
}

/* Enlarge the free space at the end of the sds string more than needed,
 * This is useful to avoid repeated re-allocations when repeatedly appending to the sds. */
// 将sds字符串末尾的可用空间扩大到比我们所需要的长度更大的尺寸，
// 当我们重复追加字符串到SDS，这个对于防止重复重新分配非常有用
sds sdsMakeRoomFor(sds s, size_t addlen) {
    return _sdsMakeRoomFor(s, addlen, 1);
}

/* Unlike sdsMakeRoomFor(), this one just grows to the necessary size. */
// 和sdsMakeRoomFor不一样，这个函数只增长到需要的尺寸
sds sdsMakeRoomForNonGreedy(sds s, size_t addlen) {
    return _sdsMakeRoomFor(s, addlen, 0);
}

/* Reallocate the sds string so that it has no free space at the end. The
 * contained string remains not altered, but next concatenation operations
 * will require a reallocation.
 *
 * After the call, the passed sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// 重新分配sds字符串让其尾部没用剩余空间。这包含的字符串保持不变，但下一个连接操作将需要重新分配。
// 调用后，传入的sds字符串不再有效，所有的引用必须替换为调用返回的新指针
sds sdsRemoveFreeSpace(sds s) {
    void *sh, *newsh;
    // 新的和老的类型
    char type, oldtype = s[-1] & SDS_TYPE_MASK;
    // 新的头部长度和老的头部长度
    int hdrlen, oldhdrlen = sdsHdrSize(oldtype);
    // 当前的长度
    size_t len = sdslen(s);
    // 当前的可用长度
    size_t avail = sdsavail(s);
    // 整个结构的开始的处
    sh = (char*)s-oldhdrlen;

    /* Return ASAP if there is no space left. */
    // 如果空闲的尺寸为0，就直接返回
    if (avail == 0) return s;

    /* Check what would be the minimum SDS header that is just good enough to
     * fit this string. */
    // 能包含当前数据长度的最小类型和头部长度
    type = sdsReqType(len);
    hdrlen = sdsHdrSize(type);

    /* If the type is the same, or at least a large enough type is still
     * required, we just realloc(), letting the allocator to do the copy
     * only if really needed. Otherwise if the change is huge, we manually
     * reallocate the string to use the different header type. */
    // 如果前后类型一致，并且当前的类型仍然满足需求，只需要调用realloc。
    if (oldtype==type || type > SDS_TYPE_8) {
        newsh = s_realloc(sh, oldhdrlen+len+1);
        if (newsh == NULL) return NULL;
        s = (char*)newsh+oldhdrlen;
    } else {
        // 分配新的内存
        newsh = s_malloc(hdrlen+len+1);
        if (newsh == NULL) return NULL;
        // 将数据拷贝过去
        memcpy((char*)newsh+hdrlen, s, len+1);
        // 释放原来的字符串
        s_free(sh);
        // 
        s = (char*)newsh+hdrlen;
        // 设置类型
        s[-1] = type;
        // 设置长度
        sdssetlen(s, len);
    }
    // 设置内存分配的长度
    sdssetalloc(s, len);
    return s;
}

/* Resize the allocation, this can make the allocation bigger or smaller,
 * if the size is smaller than currently used len, the data will be truncated */
// 调整分配长度，
// 这可以使分配更大或更小，如果大小小于当前使用的len，则数据将被截断
sds sdsResize(sds s, size_t size) {
    void *sh, *newsh;
    // 新的和老的类型
    char type, oldtype = s[-1] & SDS_TYPE_MASK;
    // 新的和老的头部长度
    int hdrlen, oldhdrlen = sdsHdrSize(oldtype);
    // 当前的长度
    size_t len = sdslen(s);
    sh = (char*)s-oldhdrlen;

    /* Return ASAP if the size is already good. */
    // 当前的分配长度和目标size一致，直接返回
    if (sdsalloc(s) == size) return s;

    /* Truncate len if needed. */
    // 如果需要就直接截断
    if (size < len) len = size;

    /* Check what would be the minimum SDS header that is just good enough to
     * fit this string. */
    // 计算新尺寸对应的类型
    type = sdsReqType(size);
    /* Don't use type 5, it is not good for strings that are resized. */
    // 不要用类型5，它不方便重新调整大小
    if (type == SDS_TYPE_5) type = SDS_TYPE_8;
    // 得到新的类型对应的头
    hdrlen = sdsHdrSize(type);

    /* If the type is the same, or can hold the size in it with low overhead
     * (larger than SDS_TYPE_8), we just realloc(), letting the allocator
     * to do the copy only if really needed. Otherwise if the change is
     * huge, we manually reallocate the string to use the different header
     * type. */
    // 如果前后类型一样，或者新类型大于类型8,并且比原来的还小，直接用relloc调整内存
    if (oldtype==type || (type < oldtype && type > SDS_TYPE_8)) {
        newsh = s_realloc(sh, oldhdrlen+size+1);
        if (newsh == NULL) return NULL;
        s = (char*)newsh+oldhdrlen;
    } else {
        // 直接分配新的内存
        newsh = s_malloc(hdrlen+size+1);
        if (newsh == NULL) return NULL;
        // 拷贝数据
        memcpy((char*)newsh+hdrlen, s, len);
        // 释放原来的结构
        s_free(sh);
        // 新的数据处
        s = (char*)newsh+hdrlen;
        // 设置类型
        s[-1] = type;
    }
    // 设置长度终止符
    s[len] = 0;
    // 设置数据长度
    sdssetlen(s, len);
    // 设置分配长度
    sdssetalloc(s, size);
    return s;
}

/* Return the total size of the allocation of the specified sds string,
 * including:
 * 1) The sds header before the pointer.
 * 2) The string.
 * 3) The free buffer at the end if any.
 * 4) The implicit null term.
 */
// 返回指定sds字符串分配的总大小
// 包括：
// * 1) 指针前的 sds 标头。
// * 2) 字符串实际占的空间。
// * 3) 后面的空闲缓冲区（如果有）。
// * 4) 隐含的空项。
size_t sdsAllocSize(sds s) {
    size_t alloc = sdsalloc(s);
    return sdsHdrSize(s[-1])+alloc+1;
}

/* Return the pointer of the actual SDS allocation (normally SDS strings
 * are referenced by the start of the string buffer). */
// 返回实际SDS内存分配的指针，即SDS结构头部首地址（通常SDS字符串由字符串缓冲区的开头引用）
void *sdsAllocPtr(sds s) {
    return (void*) (s-sdsHdrSize(s[-1]));
}

/* Increment the sds length and decrements the left free space at the
 * end of the string according to 'incr'. Also set the null term
 * in the new end of the string.
 *
 * This function is used in order to fix the string length after the
 * user calls sdsMakeRoomFor(), writes something after the end of
 * the current string, and finally needs to set the new length.
 *
 * Note: it is possible to use a negative increment in order to
 * right-trim the string.
 *
 * Usage example:
 *
 * Using sdsIncrLen() and sdsMakeRoomFor() it is possible to mount the
 * following schema, to cat bytes coming from the kernel to the end of an
 * sds string without copying into an intermediate buffer:
 *
 * oldlen = sdslen(s);
 * s = sdsMakeRoomFor(s, BUFFER_SIZE);
 * nread = read(fd, s+oldlen, BUFFER_SIZE);
 * ... check for nread <= 0 and handle it ...
 * sdsIncrLen(s, nread);
 */
// 根据incr增加sds的长度，减少字符串尾部的剩余可用空间，并且在字符串的新末尾设置终止符。
// 该函数用于在用户调用 sdsMakeRoomFor() 后固定字符串长度，在当前字符串末尾写入一些内容，最后需要设置新长度。
// 注意：可以使用负的增长来向右裁剪字符串。
// 使用示例：
// 使用sdsIncrLen()和sdsMakeRoomFor()可以遵循这种模式，将来自内核的字节附加到sds末尾，字符串而不复制到中间的缓冲区：
// oldlen = sdslen(s);
// s = sdsMakeRoomFor(s, BUFFER_SIZE);
// nread = read(fd, s+oldlen, BUFFER_SIZE);
// ... 确认 nread <= 0 并且处理它 ...
// sdsIncrLen(s, nread);

// 注意：这个减少和增加需要保证在当前的允许范围内，这个函数不会根据长度调整字符串的类型
void sdsIncrLen(sds s, ssize_t incr) {
    unsigned char flags = s[-1];
    size_t len;
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5: {
            unsigned char *fp = ((unsigned char*)s)-1;
            unsigned char oldlen = SDS_TYPE_5_LEN(flags);
            // 调整后的长度在0<= newlen < 32
            assert((incr > 0 && oldlen+incr < 32) || (incr < 0 && oldlen >= (unsigned int)(-incr)));
            *fp = SDS_TYPE_5 | ((oldlen+incr) << SDS_TYPE_BITS);
            len = oldlen+incr;
            break;
        }
        case SDS_TYPE_8: {
            SDS_HDR_VAR(8,s);
            // 调整后的长度 0 <= newlen <= sh->alloc
            assert((incr >= 0 && sh->alloc-sh->len >= incr) || (incr < 0 && sh->len >= (unsigned int)(-incr)));
            len = (sh->len += incr);
            break;
        }
        case SDS_TYPE_16: {
            SDS_HDR_VAR(16,s);
            // 调整后的长度 0 <= newlen <= sh->alloc
            assert((incr >= 0 && sh->alloc-sh->len >= incr) || (incr < 0 && sh->len >= (unsigned int)(-incr)));
            len = (sh->len += incr);
            break;
        }
        case SDS_TYPE_32: {
            SDS_HDR_VAR(32,s);
            // 调整后的长度 0 <= newlen <= sh->alloc
            assert((incr >= 0 && sh->alloc-sh->len >= (unsigned int)incr) || (incr < 0 && sh->len >= (unsigned int)(-incr)));
            len = (sh->len += incr);
            break;
        }
        case SDS_TYPE_64: {
            SDS_HDR_VAR(64,s);
            // 调整后的长度 0 <= newlen <= sh->alloc
            assert((incr >= 0 && sh->alloc-sh->len >= (uint64_t)incr) || (incr < 0 && sh->len >= (uint64_t)(-incr)));
            len = (sh->len += incr);
            break;
        }
        default: len = 0; /* Just to avoid compilation warnings. */
    }
    // 增加终止符
    s[len] = '\0';
}

/* Grow the sds to have the specified length. Bytes that were not part of
 * the original length of the sds will be set to zero.
 *
 * if the specified length is smaller than the current length, no operation
 * is performed. */
// 将 sds 增长到指定的长度。 不属于的字节sds 的原始长度将设置为零。
// 如果指定长度小于当前长度，则不操作被执行。
sds sdsgrowzero(sds s, size_t len) {
    size_t curlen = sdslen(s);
    // 如果要求的长度比当前的还小，直接返回
    if (len <= curlen) return s;
    // 增长SDS的空闲空间
    s = sdsMakeRoomFor(s,len-curlen);
    if (s == NULL) return NULL;

    /* Make sure added region doesn't contain garbage */
    // 确保增加的部分不包含垃圾
    // 将新增的部分清零处理
    memset(s+curlen,0,(len-curlen+1)); /* also set trailing \0 byte */
    // 设置长度
    sdssetlen(s, len);
    return s;
}

/* Append the specified binary-safe string pointed by 't' of 'len' bytes to the
 * end of the specified sds string 's'.
 *
 * After the call, the passed sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// 将长为len，t指向的二进制安全的字符串附加到指定sds字符串s的结尾
sds sdscatlen(sds s, const void *t, size_t len) {
    // 当前长度
    size_t curlen = sdslen(s);
    // 确保后边有len的空闲空间
    s = sdsMakeRoomFor(s,len);
    if (s == NULL) return NULL;
    // 将追加的字符串拷贝到末尾
    memcpy(s+curlen, t, len);
    // 设置长度
    sdssetlen(s, curlen+len);
    // 设置新的终止符
    s[curlen+len] = '\0';
    return s;
}

/* Append the specified null terminated C string to the sds string 's'.
 *
 * After the call, the passed sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// 追加指定的以null终止的C风格的字符串到sds字符串's'
// 调用后，传递的 sds 字符串不再有效，所有引用必须替换为调用返回的新指针。
sds sdscat(sds s, const char *t) {
    return sdscatlen(s, t, strlen(t));
}

/* Append the specified sds 't' to the existing sds 's'.
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// 追加指定的sds字符串't'到已经存在的sds字符串s后面
sds sdscatsds(sds s, const sds t) {
    return sdscatlen(s, t, sdslen(t));
}

/* Destructively modify the sds string 's' to hold the specified binary
 * safe string pointed by 't' of length 'len' bytes. */
// 破坏性的修改sds字符串s去承载指定的二进制安全的用于len长度的字符串指针t
// 破坏性的意思就是原来的数据就不存在了
sds sdscpylen(sds s, const char *t, size_t len) {
    // 当前的总空间是否可以承载传入的数据
    if (sdsalloc(s) < len) {
        // 进行扩展
        s = sdsMakeRoomFor(s,len-sdslen(s));
        if (s == NULL) return NULL;
    }
    // 将后面的数据拷贝进去
    memcpy(s, t, len);
    // 设置终止符
    s[len] = '\0';
    // 设置新的长度
    sdssetlen(s, len);
    return s;
}

/* Like sdscpylen() but 't' must be a null-terminated string so that the length
 * of the string is obtained with strlen(). */
// 和sdscpylen类似，但是t必须是一个null终止的字符串，所以字符串的长度可以通过strlen获得
sds sdscpy(sds s, const char *t) {
    return sdscpylen(s, t, strlen(t));
}

/* Helper for sdscatlonglong() doing the actual number -> string
 * conversion. 's' must point to a string with room for at least
 * SDS_LLSTR_SIZE bytes.
 *
 * The function returns the length of the null-terminated string
 * representation stored at 's'. */
// sdscatlonglong的辅助函数，用于long long类型的实际数字到字符串的转换
// s必须是一个拥有至少SDS_LLSTR_SIZE字节的字符串
// 该函数返回存储在“s”处的以空字符结尾的字符串的长度。
#define SDS_LLSTR_SIZE 21
int sdsll2str(char *s, long long value) {
    char *p, aux;
    unsigned long long v;
    size_t l;

    /* Generate the string representation, this method produces
     * a reversed string. */
    // 生成字符串表示形式时，此方法将生成反向字符串。
    if (value < 0) {
        /* Since v is unsigned, if value==LLONG_MIN, -LLONG_MIN will overflow. */
        if (value != LLONG_MIN) {
            v = -value;
        } else {
            v = ((unsigned long long)LLONG_MAX) + 1;
        }
    } else {
        v = value;
    }

    p = s;
    // 得到一个反向的字符串，比如数字-1234，得到的s就是4321-
    do {
        *p++ = '0'+(v%10);
        v /= 10;
    } while(v);
    if (value < 0) *p++ = '-';

    /* Compute length and add null term. */
    // 计算长度
    l = p-s;
    // 设置终止符
    *p = '\0';

    /* Reverse the string. */
    // 翻转字符串
    p--;
    while(s < p) {
        aux = *s;
        *s = *p;
        *p = aux;
        s++;
        p--;
    }
    return l;
}

/* Identical sdsll2str(), but for unsigned long long type. */
// 和sdsll2str完全相同，只是取用了无符号的long long type
int sdsull2str(char *s, unsigned long long v) {
    char *p, aux;
    size_t l;

    /* Generate the string representation, this method produces
     * a reversed string. */
    // 得到一个反向的字符串，比如数字1234，得到的s就是4321
    p = s;
    do {
        *p++ = '0'+(v%10);
        v /= 10;
    } while(v);

    /* Compute length and add null term. */
    l = p-s;
    *p = '\0';

    /* Reverse the string. */
    // 翻转字符串
    p--;
    while(s < p) {
        aux = *s;
        *s = *p;
        *p = aux;
        s++;
        p--;
    }
    return l;
}

/* Create an sds string from a long long value. It is much faster than:
 *
 * sdscatprintf(sdsempty(),"%lld\n", value);
 */
// 从一个long long值创建一个sds字符串。比sdscatprintf的方式快得多
sds sdsfromlonglong(long long value) {
    char buf[SDS_LLSTR_SIZE];
    int len = sdsll2str(buf,value);

    return sdsnewlen(buf,len);
}

/* Like sdscatprintf() but gets va_list instead of being variadic. */
// 类似于sdscatprintf但是从va_list获取参数而不是不定长参数
sds sdscatvprintf(sds s, const char *fmt, va_list ap) {
    va_list cpy;
    char staticbuf[1024], *buf = staticbuf, *t;
    // 预分配的长度为格式化字符串的2倍长
    size_t buflen = strlen(fmt)*2;
    int bufstrlen;

    /* We try to start using a static buffer for speed.
     * If not possible we revert to heap allocation. */
    // 如果所需的长度超过1024，需要动态分配，否则使用堆栈上的缓存区
    // 我们尝试使用堆栈上的缓冲区来提高速度，如果不可能就换成堆分配
    if (buflen > sizeof(staticbuf)) {
        buf = s_malloc(buflen);
        if (buf == NULL) return NULL;
    } else {
        buflen = sizeof(staticbuf);
    }

    /* Alloc enough space for buffer and \0 after failing to
     * fit the string in the current buffer size. */
    // 在无法将字符串放入当前缓冲区大小后，需要为缓冲区和\0分配足够的空间
    while(1) {
        // 拷贝一份va_list
        va_copy(cpy,ap);
        // 格式化
        bufstrlen = vsnprintf(buf, buflen, fmt, cpy);
        va_end(cpy);
        // 如果格式化失败，则需要释放缓冲区
        if (bufstrlen < 0) {
            if (buf != staticbuf) s_free(buf);
            return NULL;
        }
        // 如果需要的长度大于等于当前的缓冲区长度
        // （等于也不行，因为bufstrlen是没有包括终止符的长度）
        if (((size_t)bufstrlen) >= buflen) {
            // 如果不是栈内存，就释放掉
            if (buf != staticbuf) s_free(buf);
            // 加上终止符的长度
            buflen = ((size_t)bufstrlen) + 1;
            // 分配好后，重新序列化
            buf = s_malloc(buflen);
            if (buf == NULL) return NULL;
            continue;
        }
        break;
    }

    /* Finally concat the obtained string to the SDS string and return it. */
    // 最后将格式化好的字符串追加到sds字符串的后面
    t = sdscatlen(s, buf, bufstrlen);
    if (buf != staticbuf) s_free(buf);
    return t;
}

/* Append to the sds string 's' a string obtained using printf-alike format
 * specifier.
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call.
 *
 * Example:
 *
 * s = sdsnew("Sum is: ");
 * s = sdscatprintf(s,"%d+%d = %d",a,b,a+b).
 *
 * Often you need to create a string from scratch with the printf-alike
 * format. When this is the need, just use sdsempty() as the target string:
 *
 * s = sdscatprintf(sdsempty(), "... your format ...", args);
 */
// 将使用类似printf格式说明符获得的字符串附加到sds字符串's'后面。
// 在调用之后，修改过的sds字符串可能不再合法，所有引用都必须替换为调用返回的新指针。
sds sdscatprintf(sds s, const char *fmt, ...) {
    va_list ap;
    char *t;
    va_start(ap, fmt);
    t = sdscatvprintf(s,fmt,ap);
    va_end(ap);
    return t;
}

/* This function is similar to sdscatprintf, but much faster as it does
 * not rely on sprintf() family functions implemented by the libc that
 * are often very slow. Moreover directly handling the sds string as
 * new data is concatenated provides a performance improvement.
 *
 * However this function only handles an incompatible subset of printf-alike
 * format specifiers:
 *
 * %s - C String
 * %S - SDS string
 * %i - signed int
 * %I - 64 bit signed integer (long long, int64_t)
 * %u - unsigned int
 * %U - 64 bit unsigned integer (unsigned long long, uint64_t)
 * %% - Verbatim "%" character.
 */
// 此函数类似于sdscatprintf，但速度要快得多，因为它不依赖于libc实现的通常非常慢的sprintf()系列函数。
// 此外，在连接新数据时直接处理sds字符串以提高性能。
// 然而，这个函数只处理 printf-alike 格式说明符的不兼容子集：
/*
 * %s - C字符串
 * %S - SDS字符串
 * %i - 有符合整数
 * %I - 64位的有符号整数(long long, int64_t)
 * %u - 无符号整数
 * %U - 64位的无符号整数(unsigned long long, uint64_t)
 * %% - 逐字的"%"字符
 * */
sds sdscatfmt(sds s, char const *fmt, ...) {
    size_t initlen = sdslen(s);
    const char *f = fmt;
    long i;
    va_list ap;

    /* To avoid continuous reallocations, let's start with a buffer that
     * can hold at least two times the format string itself. It's not the
     * best heuristic but seems to work in practice. */
    // 为了避免连续的重新分配，让我们从一个缓冲区开始至少可以容纳格式字符串本身的两倍。 
    // 这不是最好的启发式方法，但似乎在实践中有效
    s = sdsMakeRoomFor(s, strlen(fmt)*2);
    va_start(ap,fmt);
    f = fmt;    /* Next format specifier byte to process. */
    i = initlen; /* Position of the next byte to write to dest str. */
    while(*f) {
        char next, *str;
        size_t l;
        long long num;
        unsigned long long unum;

        /* Make sure there is always space for at least 1 char. */
        // 确保至少有一个字符的空闲空间
        if (sdsavail(s)==0) {
            s = sdsMakeRoomFor(s,1);
        }

        switch(*f) {
        // 如果是百分号，就得看看下一个字符是什么
        case '%':
            next = *(f+1);
            if (next == '\0') break;
            f++;
            switch(next) {
            case 's':
            case 'S':
                // 以字符串的方式解释参数
                str = va_arg(ap,char*);
                // 分情况得到字符串的长度
                l = (next == 's') ? strlen(str) : sdslen(str);
                // 如果空间太少，那就得增加
                if (sdsavail(s) < l) {
                    s = sdsMakeRoomFor(s,l);
                }
                // 将数据拷贝进去
                memcpy(s+i,str,l);
                // 然后增加长度
                sdsinclen(s,l);
                // 增加sds的偏移
                i += l;
                break;
            case 'i':
            case 'I':
                // 解析int还是long long
                if (next == 'i')
                    num = va_arg(ap,int);
                else
                    num = va_arg(ap,long long);
                {
                    // 将数字转换为字符串
                    char buf[SDS_LLSTR_SIZE];
                    l = sdsll2str(buf,num);
                    // 如果空闲空间不够，需要增加空间
                    if (sdsavail(s) < l) {
                        s = sdsMakeRoomFor(s,l);
                    }
                    // 拷贝数据
                    memcpy(s+i,buf,l);
                    // 增加长度
                    sdsinclen(s,l);
                    i += l;
                }
                break;
            case 'u':
            case 'U':
                // 无符号的整数或者无符号long long整数
                if (next == 'u')
                    unum = va_arg(ap,unsigned int);
                else
                    unum = va_arg(ap,unsigned long long);
                {
                    char buf[SDS_LLSTR_SIZE];
                    l = sdsull2str(buf,unum);
                    if (sdsavail(s) < l) {
                        s = sdsMakeRoomFor(s,l);
                    }
                    memcpy(s+i,buf,l);
                    sdsinclen(s,l);
                    i += l;
                }
                break;
            // 处理%%和%后面跟未知的东西
            default: /* Handle %% and generally %<unknown>. */
                s[i++] = next;
                sdsinclen(s,1);
                break;
            }
            break;
        // 字符追加进去
        default:
            s[i++] = *f;
            sdsinclen(s,1);
            break;
        }
        f++;
    }
    va_end(ap);

    /* Add null-term */
    // 终止符
    s[i] = '\0';
    return s;
}

/* Remove the part of the string from left and from right composed just of
 * contiguous characters found in 'cset', that is a null terminated C string.
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call.
 *
 * Example:
 *
 * s = sdsnew("AA...AA.a.aa.aHelloWorld     :::");
 * s = sdstrim(s,"Aa. :");
 * printf("%s\n", s);
 *
 * Output will be just "HelloWorld".
 */
// 从左侧和右侧删除仅由在“cset”中找到的连续字符组成的字符串部分，cset是以null结尾的C字符串。
sds sdstrim(sds s, const char *cset) {
    char *end, *sp, *ep;
    size_t len;

    sp = s;
    ep = end = s+sdslen(s)-1;
    // 如果在cset中找到sp指向的字符，就跳过
    while(sp <= end && strchr(cset, *sp)) sp++;
    // 如果cset中找到ep指向的字符，就跳过
    while(ep > sp && strchr(cset, *ep)) ep--;
    // 得到中间的部分
    len = (ep-sp)+1;
    // 如果前面有跳过的字符，就移动
    if (s != sp) memmove(s, sp, len);
    // 终止符
    s[len] = '\0';
    // 设置长度
    sdssetlen(s,len);
    return s;
}

/* Changes the input string to be a subset of the original.
 * It does not release the free space in the string, so a call to
 * sdsRemoveFreeSpace may be wise after. */
// 将输入字符串更改为原始字符串的子集。它不会释放字符串中的可用空间，
// 因此调用sdsRemoveFreeSpace可能是明智的*/
void sdssubstr(sds s, size_t start, size_t len) {
    /* Clamp out of range input */
    // 得到开始的长度
    size_t oldlen = sdslen(s);
    // 如果开始的地方比长度还长，那就sds长度清空
    if (start >= oldlen) start = len = 0;
    // 如果长度比开始到结尾的长度还长，就将其设置为开始到结尾的长度
    if (len > oldlen-start) len = oldlen-start;

    /* Move the data */
    // 移动数据
    if (len) memmove(s, s+start, len);
    s[len] = 0;
    // 设置长度
    sdssetlen(s,len);
}

/* Turn the string into a smaller (or equal) string containing only the
 * substring specified by the 'start' and 'end' indexes.
 *
 * start and end can be negative, where -1 means the last character of the
 * string, -2 the penultimate character, and so forth.
 *
 * The interval is inclusive, so the start and end characters will be part
 * of the resulting string.
 *
 * The string is modified in-place.
 *
 * NOTE: this function can be misleading and can have unexpected behaviour,
 * specifically when you want the length of the new string to be 0.
 * Having start==end will result in a string with one character.
 * please consider using sdssubstr instead.
 *
 * Example:
 *
 * s = sdsnew("Hello World");
 * sdsrange(s,1,-1); => "ello World"
 */
// 将字符串变成一个更小的（或者相等）的字符串只包含在'start'和'end'索引的子串
// start 和 end 可以是负数，其中 -1 表示字符串的最后一个字符，-2 表示倒数第二个字符，依此类推。
// 区间包含在内，因此开始和结束字符将是一部分结果字符串。
void sdsrange(sds s, ssize_t start, ssize_t end) {
    size_t newlen, len = sdslen(s);
    if (len == 0) return;
    // 处理start为负数的情况
    if (start < 0)
        start = len + start;
    // 处理end为负数的情况
    if (end < 0)
        end = len + end;
    // 如果开始处还在结束处的后面，就将长度置为0
    newlen = (start > end) ? 0 : (end-start)+1;
    // 取子串
    sdssubstr(s, start, newlen);
}

/* Apply tolower() to every character of the sds string 's'. */
// 对sds字符串中的每一个字符，调用tolower
void sdstolower(sds s) {
    size_t len = sdslen(s), j;

    for (j = 0; j < len; j++) s[j] = tolower(s[j]);
}

/* Apply toupper() to every character of the sds string 's'. */
// 对sds字符串中的每一个字符都调用toupper
void sdstoupper(sds s) {
    size_t len = sdslen(s), j;

    for (j = 0; j < len; j++) s[j] = toupper(s[j]);
}

/* Compare two sds strings s1 and s2 with memcmp().
 *
 * Return value:
 *
 *     positive if s1 > s2.
 *     negative if s1 < s2.
 *     0 if s1 and s2 are exactly the same binary string.
 *
 * If two strings share exactly the same prefix, but one of the two has
 * additional characters, the longer string is considered to be greater than
 * the smaller one. */
// 使用memcmp比较s1和s2两个字符串
int sdscmp(const sds s1, const sds s2) {
    size_t l1, l2, minlen;
    int cmp;
    // 得到两个字符串的长度
    l1 = sdslen(s1);
    l2 = sdslen(s2);
    // 得到最小长度
    minlen = (l1 < l2) ? l1 : l2;
    // 比较字符串
    cmp = memcmp(s1,s2,minlen);
    // 如果相等再对长度做比较
    if (cmp == 0) return l1>l2? 1: (l1<l2? -1: 0);
    return cmp;
}

/* Split 's' with separator in 'sep'. An array
 * of sds strings is returned. *count will be set
 * by reference to the number of tokens returned.
 *
 * On out of memory, zero length string, zero length
 * separator, NULL is returned.
 *
 * Note that 'sep' is able to split a string using
 * a multi-character separator. For example
 * sdssplit("foo_-_bar","_-_"); will return two
 * elements "foo" and "bar".
 *
 * This version of the function is binary-safe but
 * requires length arguments. sdssplit() is just the
 * same function but for zero-terminated strings.
 */
// 使用分隔符“sep”来拆分“s”。返回一个sds字符串数组。count将被设置为元素的个数
sds *sdssplitlen(const char *s, ssize_t len, const char *sep, int seplen, int *count) {
    int elements = 0, slots = 5;
    long start = 0, j;
    sds *tokens;
    // 如果分隔符长度小于等于0，或者字符串的长度小于等于0
    if (seplen < 1 || len <= 0) {
        *count = 0;
        return NULL;
    }
    // 
    tokens = s_malloc(sizeof(sds)*slots);
    if (tokens == NULL) return NULL;

    // 遍历字符串，从0一直到放不下一个sep的时候结束
    for (j = 0; j < (len-(seplen-1)); j++) {
        /* make sure there is room for the next element and the final one */
        // 确保有空间放下下一个和最后一个元素
        if (slots < elements+2) {
            sds *newtokens;
            // 成倍增长
            slots *= 2;
            newtokens = s_realloc(tokens,sizeof(sds)*slots);
            if (newtokens == NULL) goto cleanup;
            tokens = newtokens;
        }
        /* search the separator */
        // 如果sep长度为1，则直接进行字符比较，或者使用memcmp比较
        if ((seplen == 1 && *(s+j) == sep[0]) || (memcmp(s+j,sep,seplen) == 0)) {
            // 将上一个sep后开始到当前字符，作为一个sds字符串
            tokens[elements] = sdsnewlen(s+start,j-start);
            if (tokens[elements] == NULL) goto cleanup;
            // 元素个数增加
            elements++;
            // 下一个字符串的开始处是，跳过这个sep
            start = j+seplen;
            // 直接跳过sep再处理
            j = j+seplen-1; /* skip the separator */
        }
    }
    /* Add the final element. We are sure there is room in the tokens array. */
    // 最后一个元素的处理，我们确定tokens数组有空间
    tokens[elements] = sdsnewlen(s+start,len-start);
    if (tokens[elements] == NULL) goto cleanup;
    // 增加元素个数
    elements++;
    // 放入返回的参数中
    *count = elements;
    return tokens;

cleanup:
    {
        int i;
        // 释放所有的sds字符串
        for (i = 0; i < elements; i++) sdsfree(tokens[i]);
        s_free(tokens);
        *count = 0;
        return NULL;
    }
}

/* Free the result returned by sdssplitlen(), or do nothing if 'tokens' is NULL. */
// 
void sdsfreesplitres(sds *tokens, int count) {
    if (!tokens) return;
    while(count--)
        sdsfree(tokens[count]);
    s_free(tokens);
}

/* Append to the sds string "s" an escaped string representation where
 * all the non-printable characters (tested with isprint()) are turned into
 * escapes in the form "\n\r\a...." or "\x<hex-number>".
 *
 * After the call, the modified sds string is no longer valid and all the
 * references must be substituted with the new pointer returned by the call. */
// 在sds字符串“s”后面附加一个转义字符串表示形式，
// 其中所有不可打印字符（使用isprint()判断）都将转换为“\n\r\na.…”或“\x<十六进制数>”形式的转义。
sds sdscatrepr(sds s, const char *p, size_t len) {
    // 
    s = sdscatlen(s,"\"",1);
    while(len--) {
        switch(*p) {
        case '\\':
        case '"':
            s = sdscatprintf(s,"\\%c",*p);
            break;
        // 下面这些通过字符的表示方式将换行表示为\n的字符
        case '\n': s = sdscatlen(s,"\\n",2); break;
        case '\r': s = sdscatlen(s,"\\r",2); break;
        case '\t': s = sdscatlen(s,"\\t",2); break;
        case '\a': s = sdscatlen(s,"\\a",2); break;
        case '\b': s = sdscatlen(s,"\\b",2); break;
        default:
            // 如果是打印字符，以字符的形式附加在后面
            if (isprint(*p))
                s = sdscatprintf(s,"%c",*p);
            else
                // 不是打印字符就以16进制的方式附加
                s = sdscatprintf(s,"\\x%02x",(unsigned char)*p);
            break;
        }
        p++;
    }
    return sdscatlen(s,"\"",1);
}

/* Returns one if the string contains characters to be escaped
 * by sdscatrepr(), zero otherwise.
 *
 * Typically, this should be used to help protect aggregated strings in a way
 * that is compatible with sdssplitargs(). For this reason, also spaces will be
 * treated as needing an escape.
 */
// 如果字符串包含要由sdscatrer（）转义的字符，则返回1，否则返回0。
int sdsneedsrepr(const sds s) {
    size_t len = sdslen(s);
    const char *p = s;
    // 如果有转义字符，空格和不可打印字符，返回1
    while (len--) {
        if (*p == '\\' || *p == '"' || *p == '\n' || *p == '\r' ||
            *p == '\t' || *p == '\a' || *p == '\b' || !isprint(*p) || isspace(*p)) return 1;
        p++;
    }

    return 0;
}

/* Helper function for sdssplitargs() that returns non zero if 'c'
 * is a valid hex digit. */
// sdssplitargs的辅助函数，如果字符是一个合法的16进制字符，就返回1
int is_hex_digit(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

/* Helper function for sdssplitargs() that converts a hex digit into an
 * integer from 0 to 15 */
// sdssplitargs的辅助函数，将一个字符转换成16进制的数字 
int hex_digit_to_int(char c) {
    switch(c) {
    case '0': return 0;
    case '1': return 1;
    case '2': return 2;
    case '3': return 3;
    case '4': return 4;
    case '5': return 5;
    case '6': return 6;
    case '7': return 7;
    case '8': return 8;
    case '9': return 9;
    case 'a': case 'A': return 10;
    case 'b': case 'B': return 11;
    case 'c': case 'C': return 12;
    case 'd': case 'D': return 13;
    case 'e': case 'E': return 14;
    case 'f': case 'F': return 15;
    default: return 0;
    }
}

/* Split a line into arguments, where every argument can be in the
 * following programming-language REPL-alike form:
 *
 * foo bar "newline are supported\n" and "\xff\x00otherstuff"
 *
 * The number of arguments is stored into *argc, and an array
 * of sds is returned.
 *
 * The caller should free the resulting array of sds strings with
 * sdsfreesplitres().
 *
 * Note that sdscatrepr() is able to convert back a string into
 * a quoted string in the same format sdssplitargs() is able to parse.
 *
 * The function returns the allocated tokens on success, even when the
 * input string is empty, or NULL if the input contains unbalanced
 * quotes or closed quotes followed by non space characters
 * as in: "foo"bar or "foo'
 */
// 将一行拆分为多个参数，其中每个参数都可以采用以下类似REPL的编程语言形式：
// REPL:是一个简单的交互式的编程环境。表示：“读取-求值-输出-循环”。
sds *sdssplitargs(const char *line, int *argc) {
    const char *p = line;
    char *current = NULL;
    char **vector = NULL;

    *argc = 0;
    while(1) {
        /* skip blanks */
        // 跳过空白字符
        while(*p && isspace(*p)) p++;
        if (*p) {
            /* get a token */
            // 如果在引号中，设置为1
            int inq=0;  /* set to 1 if we are in "quotes" */
            // 如果在单引号中，设置为1
            int insq=0; /* set to 1 if we are in 'single quotes' */
            int done=0;

            if (current == NULL) current = sdsempty();
            while(!done) {
                if (inq) {
                    if (*p == '\\' && *(p+1) == 'x' &&
                                             is_hex_digit(*(p+2)) &&
                                             is_hex_digit(*(p+3)))
                    {
                        unsigned char byte;

                        byte = (hex_digit_to_int(*(p+2))*16)+
                                hex_digit_to_int(*(p+3));
                        current = sdscatlen(current,(char*)&byte,1);
                        p += 3;
                    } else if (*p == '\\' && *(p+1)) {
                        char c;

                        p++;
                        switch(*p) {
                        case 'n': c = '\n'; break;
                        case 'r': c = '\r'; break;
                        case 't': c = '\t'; break;
                        case 'b': c = '\b'; break;
                        case 'a': c = '\a'; break;
                        default: c = *p; break;
                        }
                        current = sdscatlen(current,&c,1);
                    } else if (*p == '"') {
                        /* closing quote must be followed by a space or
                         * nothing at all. */
                        if (*(p+1) && !isspace(*(p+1))) goto err;
                        done=1;
                    } else if (!*p) {
                        /* unterminated quotes */
                        goto err;
                    } else {
                        current = sdscatlen(current,p,1);
                    }
                } else if (insq) {
                    if (*p == '\\' && *(p+1) == '\'') {
                        p++;
                        current = sdscatlen(current,"'",1);
                    } else if (*p == '\'') {
                        /* closing quote must be followed by a space or
                         * nothing at all. */
                        if (*(p+1) && !isspace(*(p+1))) goto err;
                        done=1;
                    } else if (!*p) {
                        /* unterminated quotes */
                        goto err;
                    } else {
                        current = sdscatlen(current,p,1);
                    }
                } else {
                    switch(*p) {
                    case ' ':
                    case '\n':
                    case '\r':
                    case '\t':
                    case '\0':
                        done=1;
                        break;
                    case '"':
                        inq=1;
                        break;
                    case '\'':
                        insq=1;
                        break;
                    default:
                        current = sdscatlen(current,p,1);
                        break;
                    }
                }
                if (*p) p++;
            }
            /* add the token to the vector */
            vector = s_realloc(vector,((*argc)+1)*sizeof(char*));
            vector[*argc] = current;
            (*argc)++;
            current = NULL;
        } else {
            /* Even on empty input string return something not NULL. */
            if (vector == NULL) vector = s_malloc(sizeof(void*));
            return vector;
        }
    }

err:
    while((*argc)--)
        sdsfree(vector[*argc]);
    s_free(vector);
    if (current) sdsfree(current);
    *argc = 0;
    return NULL;
}

/* Modify the string substituting all the occurrences of the set of
 * characters specified in the 'from' string to the corresponding character
 * in the 'to' array.
 *
 * For instance: sdsmapchars(mystring, "ho", "01", 2)
 * will have the effect of turning the string "hello" into "0ell1".
 *
 * The function returns the sds string pointer, that is always the same
 * as the input pointer since no resize is needed. */
// 修改字符串，将源字符串中所有出现在指定的字符集“from”的字符替换为“to”数组中的对应字符
sds sdsmapchars(sds s, const char *from, const char *to, size_t setlen) {
    size_t j, i, l = sdslen(s);
    // 遍历整个sds
    for (j = 0; j < l; j++) {
        // 对sds字符串中的每一个字符，判定是否出现在from字符串中
        for (i = 0; i < setlen; i++) {
            // 如果出现在from字符串中，替换成to字符串中对应的字符
            if (s[j] == from[i]) {
                s[j] = to[i];
                break;
            }
        }
    }
    return s;
}

/* Join an array of C strings using the specified separator (also a C string).
 * Returns the result as an sds string. */
// 使用指定的间隔符(也是一个C字符串)将一个C字符串数组连接成一个sds
sds sdsjoin(char **argv, int argc, char *sep) {
    // 创建新的结果字符串
    sds join = sdsempty();
    int j;
    // 循环将参数中的C字符串数组依次连接到结果字符串中
    for (j = 0; j < argc; j++) {
        join = sdscat(join, argv[j]);
        // 如果不是最后一个，就需要在连接一个C字符串之后，再连接一个间隔字符串
        if (j != argc-1) join = sdscat(join,sep);
    }
    return join;
}

/* Like sdsjoin, but joins an array of SDS strings. */
// 将一个sds数组的连接成一个sds，并且使用指定的间隔字符串
sds sdsjoinsds(sds *argv, int argc, const char *sep, size_t seplen) {
    // 创建新的结果字符串
    sds join = sdsempty();
    int j;
    // 循环将参数中的SDS数组依次连接到结果字符串中
    for (j = 0; j < argc; j++) {
        join = sdscatsds(join, argv[j]);
        // 如果不是最后一个，就需要在连接一个SDS之后，再连接一个间隔字符串
        if (j != argc-1) join = sdscatlen(join,sep,seplen);
    }
    return join;
}

/* Wrappers to the allocators used by SDS. Note that SDS will actually
 * just use the macros defined into sdsalloc.h in order to avoid to pay
 * the overhead of function calls. Here we define these wrappers only for
 * the programs SDS is linked to, if they want to touch the SDS internals
 * even if they use a different allocator. */
void *sds_malloc(size_t size) { return s_malloc(size); }
void *sds_realloc(void *ptr, size_t size) { return s_realloc(ptr,size); }
void sds_free(void *ptr) { s_free(ptr); }

/* Perform expansion of a template string and return the result as a newly
 * allocated sds.
 *
 * Template variables are specified using curly brackets, e.g. {variable}.
 * An opening bracket can be quoted by repeating it twice.
 */
// 执行模板字符串的扩展并且将一个新分配的sds作为结果返回
// 模板变量使用大括号指定，例如｛variable｝。可以通过重复两次来引用一个大括号。
sds sdstemplate(const char *template, sdstemplate_callback_t cb_func, void *cb_arg)
{
    // 创建一个新的空sds
    sds res = sdsempty();
    const char *p = template;

    while (*p) {
        /* Find next variable, copy everything until there */
        // 查找下一个变量，拷贝前面的数据
        const char *sv = strchr(p, '{');
        if (!sv) {
            /* Not found: copy till rest of template and stop */
            // 没有找到，拷贝模板剩余的部分
            res = sdscat(res, p);
            break;
        } else if (sv > p) {
            /* Found: copy anything up to the beginning of the variable */
            // 找到了，就一直拷贝到变量开始处
            res = sdscatlen(res, p, sv - p);
        }

        /* Skip into variable name, handle premature end or quoting */
        // 跳过变量名，处理过早结束或引用
        sv++;
        // 模板过早结束
        if (!*sv) goto error;       /* Premature end of template */
        // 如果大括号后面再跟着一个大括号
        if (*sv == '{') {
            /* Quoted '{' */
            // 处理从大括号下一个处开始
            p = sv + 1;
            // 在结果里面追加一个大括号
            res = sdscat(res, "{");
            continue;
        }

        /* Find end of variable name, handle premature end of template */
        // 找到变量名的结束处，处理模板过早结束
        const char *ev = strchr(sv, '}');
        if (!ev) goto error;

        /* Pass variable name to callback and obtain value. If callback failed,
         * abort. */
        // 将变量名传入回调函数得到对应的值，如果调用失败，直接终止
        sds varname = sdsnewlen(sv, ev - sv);
        sds value = cb_func(varname, cb_arg);
        // 将名字释放
        sdsfree(varname);
        if (!value) goto error;

        /* Append value to result and continue */
        // 将变量值追加到结果sds中然后继续
        res = sdscat(res, value);
        // 将结果也释放
        sdsfree(value);
        p = ev + 1;
    }

    return res;

error:
    // 如果出错，将结果释放，返回NULL
    sdsfree(res);
    return NULL;
}

#ifdef REDIS_TEST
#include <stdio.h>
#include <limits.h>
#include "testhelp.h"

#define UNUSED(x) (void)(x)

static sds sdsTestTemplateCallback(sds varname, void *arg) {
    UNUSED(arg);
    static const char *_var1 = "variable1";
    static const char *_var2 = "variable2";

    if (!strcmp(varname, _var1)) return sdsnew("value1");
    else if (!strcmp(varname, _var2)) return sdsnew("value2");
    else return NULL;
}

int sdsTest(int argc, char **argv, int flags) {
    UNUSED(argc);
    UNUSED(argv);
    UNUSED(flags);

    {
        sds x = sdsnew("foo"), y;

        test_cond("Create a string and obtain the length",
            sdslen(x) == 3 && memcmp(x,"foo\0",4) == 0);

        sdsfree(x);
        x = sdsnewlen("foo",2);
        test_cond("Create a string with specified length",
            sdslen(x) == 2 && memcmp(x,"fo\0",3) == 0);

        x = sdscat(x,"bar");
        test_cond("Strings concatenation",
            sdslen(x) == 5 && memcmp(x,"fobar\0",6) == 0);

        x = sdscpy(x,"a");
        test_cond("sdscpy() against an originally longer string",
            sdslen(x) == 1 && memcmp(x,"a\0",2) == 0);

        x = sdscpy(x,"xyzxxxxxxxxxxyyyyyyyyyykkkkkkkkkk");
        test_cond("sdscpy() against an originally shorter string",
            sdslen(x) == 33 &&
            memcmp(x,"xyzxxxxxxxxxxyyyyyyyyyykkkkkkkkkk\0",33) == 0);

        sdsfree(x);
        x = sdscatprintf(sdsempty(),"%d",123);
        test_cond("sdscatprintf() seems working in the base case",
            sdslen(x) == 3 && memcmp(x,"123\0",4) == 0);

        sdsfree(x);
        x = sdscatprintf(sdsempty(),"a%cb",0);
        test_cond("sdscatprintf() seems working with \\0 inside of result",
            sdslen(x) == 3 && memcmp(x,"a\0""b\0",4) == 0);

        {
            sdsfree(x);
            char etalon[1024*1024];
            for (size_t i = 0; i < sizeof(etalon); i++) {
                etalon[i] = '0';
            }
            x = sdscatprintf(sdsempty(),"%0*d",(int)sizeof(etalon),0);
            test_cond("sdscatprintf() can print 1MB",
                sdslen(x) == sizeof(etalon) && memcmp(x,etalon,sizeof(etalon)) == 0);
        }

        sdsfree(x);
        x = sdsnew("--");
        x = sdscatfmt(x, "Hello %s World %I,%I--", "Hi!", LLONG_MIN,LLONG_MAX);
        test_cond("sdscatfmt() seems working in the base case",
            sdslen(x) == 60 &&
            memcmp(x,"--Hello Hi! World -9223372036854775808,"
                     "9223372036854775807--",60) == 0);
        printf("[%s]\n",x);

        sdsfree(x);
        x = sdsnew("--");
        x = sdscatfmt(x, "%u,%U--", UINT_MAX, ULLONG_MAX);
        test_cond("sdscatfmt() seems working with unsigned numbers",
            sdslen(x) == 35 &&
            memcmp(x,"--4294967295,18446744073709551615--",35) == 0);

        sdsfree(x);
        x = sdsnew(" x ");
        sdstrim(x," x");
        test_cond("sdstrim() works when all chars match",
            sdslen(x) == 0);

        sdsfree(x);
        x = sdsnew(" x ");
        sdstrim(x," ");
        test_cond("sdstrim() works when a single char remains",
            sdslen(x) == 1 && x[0] == 'x');

        sdsfree(x);
        x = sdsnew("xxciaoyyy");
        sdstrim(x,"xy");
        test_cond("sdstrim() correctly trims characters",
            sdslen(x) == 4 && memcmp(x,"ciao\0",5) == 0);

        y = sdsdup(x);
        sdsrange(y,1,1);
        test_cond("sdsrange(...,1,1)",
            sdslen(y) == 1 && memcmp(y,"i\0",2) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,1,-1);
        test_cond("sdsrange(...,1,-1)",
            sdslen(y) == 3 && memcmp(y,"iao\0",4) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,-2,-1);
        test_cond("sdsrange(...,-2,-1)",
            sdslen(y) == 2 && memcmp(y,"ao\0",3) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,2,1);
        test_cond("sdsrange(...,2,1)",
            sdslen(y) == 0 && memcmp(y,"\0",1) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,1,100);
        test_cond("sdsrange(...,1,100)",
            sdslen(y) == 3 && memcmp(y,"iao\0",4) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,100,100);
        test_cond("sdsrange(...,100,100)",
            sdslen(y) == 0 && memcmp(y,"\0",1) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,4,6);
        test_cond("sdsrange(...,4,6)",
            sdslen(y) == 0 && memcmp(y,"\0",1) == 0);

        sdsfree(y);
        y = sdsdup(x);
        sdsrange(y,3,6);
        test_cond("sdsrange(...,3,6)",
            sdslen(y) == 1 && memcmp(y,"o\0",2) == 0);

        sdsfree(y);
        sdsfree(x);
        x = sdsnew("foo");
        y = sdsnew("foa");
        test_cond("sdscmp(foo,foa)", sdscmp(x,y) > 0);

        sdsfree(y);
        sdsfree(x);
        x = sdsnew("bar");
        y = sdsnew("bar");
        test_cond("sdscmp(bar,bar)", sdscmp(x,y) == 0);

        sdsfree(y);
        sdsfree(x);
        x = sdsnew("aar");
        y = sdsnew("bar");
        test_cond("sdscmp(bar,bar)", sdscmp(x,y) < 0);

        sdsfree(y);
        sdsfree(x);
        x = sdsnewlen("\a\n\0foo\r",7);
        y = sdscatrepr(sdsempty(),x,sdslen(x));
        test_cond("sdscatrepr(...data...)",
            memcmp(y,"\"\\a\\n\\x00foo\\r\"",15) == 0);

        {
            unsigned int oldfree;
            char *p;
            int i;
            size_t step = 10, j;

            sdsfree(x);
            sdsfree(y);
            x = sdsnew("0");
            test_cond("sdsnew() free/len buffers", sdslen(x) == 1 && sdsavail(x) == 0);

            /* Run the test a few times in order to hit the first two
             * SDS header types. */
            for (i = 0; i < 10; i++) {
                size_t oldlen = sdslen(x);
                x = sdsMakeRoomFor(x,step);
                int type = x[-1]&SDS_TYPE_MASK;

                test_cond("sdsMakeRoomFor() len", sdslen(x) == oldlen);
                if (type != SDS_TYPE_5) {
                    test_cond("sdsMakeRoomFor() free", sdsavail(x) >= step);
                    oldfree = sdsavail(x);
                    UNUSED(oldfree);
                }
                p = x+oldlen;
                for (j = 0; j < step; j++) {
                    p[j] = 'A'+j;
                }
                sdsIncrLen(x,step);
            }
            test_cond("sdsMakeRoomFor() content",
                memcmp("0ABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJABCDEFGHIJ",x,101) == 0);
            test_cond("sdsMakeRoomFor() final length",sdslen(x)==101);

            sdsfree(x);
        }

        /* Simple template */
        x = sdstemplate("v1={variable1} v2={variable2}", sdsTestTemplateCallback, NULL);
        test_cond("sdstemplate() normal flow",
                  memcmp(x,"v1=value1 v2=value2",19) == 0);
        sdsfree(x);

        /* Template with callback error */
        x = sdstemplate("v1={variable1} v3={doesnotexist}", sdsTestTemplateCallback, NULL);
        test_cond("sdstemplate() with callback error", x == NULL);

        /* Template with empty var name */
        x = sdstemplate("v1={", sdsTestTemplateCallback, NULL);
        test_cond("sdstemplate() with empty var name", x == NULL);

        /* Template with truncated var name */
        x = sdstemplate("v1={start", sdsTestTemplateCallback, NULL);
        test_cond("sdstemplate() with truncated var name", x == NULL);

        /* Template with quoting */
        x = sdstemplate("v1={{{variable1}} {{} v2={variable2}", sdsTestTemplateCallback, NULL);
        test_cond("sdstemplate() with quoting",
                  memcmp(x,"v1={value1} {} v2=value2",24) == 0);
        sdsfree(x);

        /* Test sdsresize - extend */
        x = sdsnew("1234567890123456789012345678901234567890");
        x = sdsResize(x, 200);
        test_cond("sdsrezie() expand len", sdslen(x) == 40);
        test_cond("sdsrezie() expand strlen", strlen(x) == 40);
        test_cond("sdsrezie() expand alloc", sdsalloc(x) == 200);
        /* Test sdsresize - trim free space */
        x = sdsResize(x, 80);
        test_cond("sdsrezie() shrink len", sdslen(x) == 40);
        test_cond("sdsrezie() shrink strlen", strlen(x) == 40);
        test_cond("sdsrezie() shrink alloc", sdsalloc(x) == 80);
        /* Test sdsresize - crop used space */
        x = sdsResize(x, 30);
        test_cond("sdsrezie() crop len", sdslen(x) == 30);
        test_cond("sdsrezie() crop strlen", strlen(x) == 30);
        test_cond("sdsrezie() crop alloc", sdsalloc(x) == 30);
        /* Test sdsresize - extend to different class */
        x = sdsResize(x, 400);
        test_cond("sdsrezie() expand len", sdslen(x) == 30);
        test_cond("sdsrezie() expand strlen", strlen(x) == 30);
        test_cond("sdsrezie() expand alloc", sdsalloc(x) == 400);
        /* Test sdsresize - shrink to different class */
        x = sdsResize(x, 4);
        test_cond("sdsrezie() crop len", sdslen(x) == 4);
        test_cond("sdsrezie() crop strlen", strlen(x) == 4);
        test_cond("sdsrezie() crop alloc", sdsalloc(x) == 4);
        sdsfree(x);
    }
    return 0;
}
#endif
