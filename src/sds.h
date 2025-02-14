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

/*动态的字符串库*/

#ifndef __SDS_H
#define __SDS_H

// 1KB的大小
#define SDS_MAX_PREALLOC (1024*1024)
extern const char *SDS_NOINIT;

#include <sys/types.h>
#include <stdarg.h>
#include <stdint.h>

typedef char *sds;

/* Note: sdshdr5 is never used, we just access the flags byte directly.
// 注意：sdshdr5从没使用过，我们只是直接访问flags字节
 * However is here to document the layout of type 5 SDS strings. */
// 不过这里有类型5 SDS字符串的布局文档
struct __attribute__ ((__packed__)) sdshdr5 {
    // 低3位为类型，高5位为字符串长度
    unsigned char flags; /* 3 lsb of type, and 5 msb of string length */
    // 存放实际字符串的地方
    char buf[];
};
struct __attribute__ ((__packed__)) sdshdr8 {
    // 已经使用的长度
    uint8_t len; /* used */
    // 分配的长度，除去头和null终止符
    uint8_t alloc; /* excluding the header and null terminator */
    // 低3位为类型，高5位保留
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    // 存放实际字符串的地方
    char buf[];
};
struct __attribute__ ((__packed__)) sdshdr16 {
    // 已经使用的长度
    uint16_t len; /* used */
    // 分配的长度，除去头和null终止符
    uint16_t alloc; /* excluding the header and null terminator */
    // 低3位为类型，高5位保留
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    // 存放实际字符串的地方
    char buf[];
};
struct __attribute__ ((__packed__)) sdshdr32 {
    // 已经使用的长度
    uint32_t len; /* used */
    // 分配的长度，除去头和null终止符
    uint32_t alloc; /* excluding the header and null terminator */
    // 低3位为类型，高5位保留
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    // 存放实际字符串的地方
    char buf[];
};
struct __attribute__ ((__packed__)) sdshdr64 {
    // 已经使用的长度
    uint64_t len; /* used */
    // 分配的长度，除去头和null终止符
    uint64_t alloc; /* excluding the header and null terminator */
    // 低3位为类型，高5位保留
    unsigned char flags; /* 3 lsb of type, 5 unused bits */
    // 存放实际字符串的地方
    char buf[];
};

// 简单动态字符串(SDS的五种类型)，
// 分别对应sdshdr5，sdshdr8，sdshdr16，sdshdr32，sdshdr64
#define SDS_TYPE_5  0
#define SDS_TYPE_8  1
#define SDS_TYPE_16 2
#define SDS_TYPE_32 3
#define SDS_TYPE_64 4
// 取SDS类型的掩码（低三位的掩码)
// 二进制：00000111
#define SDS_TYPE_MASK 7
// SDS类型占的位数
#define SDS_TYPE_BITS 3
// 从buff到对应的sds的具体类型的指针转换，就是从sdshdr*结构中的buf转换到sdshdr*结构
// 然后赋值给一个变量
#define SDS_HDR_VAR(T,s) struct sdshdr##T *sh = (void*)((s)-(sizeof(struct sdshdr##T)));
// 与上一个作用相同，只是只做转换
#define SDS_HDR(T,s) ((struct sdshdr##T *)((s)-(sizeof(struct sdshdr##T))))
// sds类型5的长度,是通过flags的高5位得到的
#define SDS_TYPE_5_LEN(f) ((f)>>SDS_TYPE_BITS)

// 获取sds的长度，这个s就是buff的地址
static inline size_t sdslen(const sds s) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        case SDS_TYPE_5:
        // 类型5的需要取flag的高5位，
            return SDS_TYPE_5_LEN(flags);
        // 其他类型取结构中的len就行
        case SDS_TYPE_8:
            return SDS_HDR(8,s)->len;
        case SDS_TYPE_16:
            return SDS_HDR(16,s)->len;
        case SDS_TYPE_32:
            return SDS_HDR(32,s)->len;
        case SDS_TYPE_64:
            return SDS_HDR(64,s)->len;
    }
    return 0;
}

// 得到可用空间的大小，这个s就是buff的地址
static inline size_t sdsavail(const sds s) {
    // s的前面1个char就是flags
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        //  SDS_TYPE_5是不预留空间的
        case SDS_TYPE_5: {
            return 0;
        }
        // 其他类型的都是分配的减去已经用掉的
        case SDS_TYPE_8: {
            SDS_HDR_VAR(8,s);
            return sh->alloc - sh->len;
        }
        case SDS_TYPE_16: {
            SDS_HDR_VAR(16,s);
            return sh->alloc - sh->len;
        }
        case SDS_TYPE_32: {
            SDS_HDR_VAR(32,s);
            return sh->alloc - sh->len;
        }
        case SDS_TYPE_64: {
            SDS_HDR_VAR(64,s);
            return sh->alloc - sh->len;
        }
    }
    return 0;
}

// 设置新的已经使用的长度
static inline void sdssetlen(sds s, size_t newlen) {
    // s的前面1个char就是flags
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        // 类型5表示先得取得flag的地址，然后长度左移3位与类型或
        case SDS_TYPE_5:
            {
                unsigned char *fp = ((unsigned char*)s)-1;
                *fp = SDS_TYPE_5 | (newlen << SDS_TYPE_BITS);
            }
            break;
        // 其他类型，直接设置len的值就行
        case SDS_TYPE_8:
            SDS_HDR(8,s)->len = newlen;
            break;
        case SDS_TYPE_16:
            SDS_HDR(16,s)->len = newlen;
            break;
        case SDS_TYPE_32:
            SDS_HDR(32,s)->len = newlen;
            break;
        case SDS_TYPE_64:
            SDS_HDR(64,s)->len = newlen;
            break;
    }
}

// 增加已经使用的长度
static inline void sdsinclen(sds s, size_t inc) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        // 类型5表示先得取得flag的地址，然后得到原来的长度和增加的长度之和，
        // 然后新长度长度左移3位与类型或
        case SDS_TYPE_5:
            {
                unsigned char *fp = ((unsigned char*)s)-1;
                unsigned char newlen = SDS_TYPE_5_LEN(flags)+inc;
                *fp = SDS_TYPE_5 | (newlen << SDS_TYPE_BITS);
            }
            break;
        // 其他类型，直接在len上面加值就行
        case SDS_TYPE_8:
            SDS_HDR(8,s)->len += inc;
            break;
        case SDS_TYPE_16:
            SDS_HDR(16,s)->len += inc;
            break;
        case SDS_TYPE_32:
            SDS_HDR(32,s)->len += inc;
            break;
        case SDS_TYPE_64:
            SDS_HDR(64,s)->len += inc;
            break;
    }
}

/* sdsalloc() = sdsavail() + sdslen() */
// 得到分配的长度
static inline size_t sdsalloc(const sds s) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        // 类型5直接得到长度
        case SDS_TYPE_5:
            return SDS_TYPE_5_LEN(flags);
        // 其他类型得到分配的长度
        case SDS_TYPE_8:
            return SDS_HDR(8,s)->alloc;
        case SDS_TYPE_16:
            return SDS_HDR(16,s)->alloc;
        case SDS_TYPE_32:
            return SDS_HDR(32,s)->alloc;
        case SDS_TYPE_64:
            return SDS_HDR(64,s)->alloc;
    }
    return 0;
}

// 设置新的分配长度
static inline void sdssetalloc(sds s, size_t newlen) {
    unsigned char flags = s[-1];
    switch(flags&SDS_TYPE_MASK) {
        // 类型5没有总的分配长度，所以啥事都不用做
        case SDS_TYPE_5:
            /* Nothing to do, this type has no total allocation info. */
            break;
        // 其他的类型直接设置分配长度就行
        case SDS_TYPE_8:
            SDS_HDR(8,s)->alloc = newlen;
            break;
        case SDS_TYPE_16:
            SDS_HDR(16,s)->alloc = newlen;
            break;
        case SDS_TYPE_32:
            SDS_HDR(32,s)->alloc = newlen;
            break;
        case SDS_TYPE_64:
            SDS_HDR(64,s)->alloc = newlen;
            break;
    }
}
// 创建新的简单字符串(SDS)
sds sdsnewlen(const void *init, size_t initlen);
// 尝试创建新的简单字符串（SDS）
sds sdstrynewlen(const void *init, size_t initlen);
// 从一个C字符串创建一个SDS字符串
sds sdsnew(const char *init);
// 创建一个空的（0长度）sds字符串
sds sdsempty(void);
// 复制一个SDS字符串
sds sdsdup(const sds s);
// 释放一个SDS字符串，
void sdsfree(sds s);
sds sdsgrowzero(sds s, size_t len);
sds sdscatlen(sds s, const void *t, size_t len);
sds sdscat(sds s, const char *t);
sds sdscatsds(sds s, const sds t);
sds sdscpylen(sds s, const char *t, size_t len);
sds sdscpy(sds s, const char *t);

sds sdscatvprintf(sds s, const char *fmt, va_list ap);
#ifdef __GNUC__
sds sdscatprintf(sds s, const char *fmt, ...)
    __attribute__((format(printf, 2, 3)));
#else
sds sdscatprintf(sds s, const char *fmt, ...);
#endif

sds sdscatfmt(sds s, char const *fmt, ...);
sds sdstrim(sds s, const char *cset);
void sdssubstr(sds s, size_t start, size_t len);
void sdsrange(sds s, ssize_t start, ssize_t end);
// 通过strlen得到字符串的长度，然后更新到sds的长度
void sdsupdatelen(sds s);
// 就地修改sds字符串，使其成为空串
void sdsclear(sds s);
int sdscmp(const sds s1, const sds s2);
sds *sdssplitlen(const char *s, ssize_t len, const char *sep, int seplen, int *count);
void sdsfreesplitres(sds *tokens, int count);
void sdstolower(sds s);
void sdstoupper(sds s);
sds sdsfromlonglong(long long value);
sds sdscatrepr(sds s, const char *p, size_t len);
sds *sdssplitargs(const char *line, int *argc);
sds sdsmapchars(sds s, const char *from, const char *to, size_t setlen);
sds sdsjoin(char **argv, int argc, char *sep);
sds sdsjoinsds(sds *argv, int argc, const char *sep, size_t seplen);
int sdsneedsrepr(const sds s);

/* Callback for sdstemplate. The function gets called by sdstemplate
 * every time a variable needs to be expanded. The variable name is
 * provided as variable, and the callback is expected to return a
 * substitution value. Returning a NULL indicates an error.
 */
typedef sds (*sdstemplate_callback_t)(const sds variable, void *arg);
sds sdstemplate(const char *template, sdstemplate_callback_t cb_func, void *cb_arg);

/* Low level functions exposed to the user API */
sds sdsMakeRoomFor(sds s, size_t addlen);
sds sdsMakeRoomForNonGreedy(sds s, size_t addlen);
void sdsIncrLen(sds s, ssize_t incr);
sds sdsRemoveFreeSpace(sds s);
sds sdsResize(sds s, size_t size);
size_t sdsAllocSize(sds s);
void *sdsAllocPtr(sds s);

/* Export the allocator used by SDS to the program using SDS.
 * Sometimes the program SDS is linked to, may use a different set of
 * allocators, but may want to allocate or free things that SDS will
 * respectively free or allocate. */
void *sds_malloc(size_t size);
void *sds_realloc(void *ptr, size_t size);
void sds_free(void *ptr);

#ifdef REDIS_TEST
int sdsTest(int argc, char *argv[], int flags);
#endif

#endif
