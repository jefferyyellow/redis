/* The ziplist is a specially encoded dually linked list that is designed
 * to be very memory efficient. It stores both strings and integer values,
 * where integers are encoded as actual integers instead of a series of
 * characters. It allows push and pop operations on either side of the list
 * in O(1) time. However, because every operation requires a reallocation of
 * the memory used by the ziplist, the actual complexity is related to the
 * amount of memory used by the ziplist.
 *
 * ----------------------------------------------------------------------------
 *
 * ZIPLIST OVERALL LAYOUT
 * ======================
 *
 * The general layout of the ziplist is as follows:
 *
 * <zlbytes> <zltail> <zllen> <entry> <entry> ... <entry> <zlend>
 *
 * NOTE: all fields are stored in little endian, if not specified otherwise.
 *
 * <uint32_t zlbytes> is an unsigned integer to hold the number of bytes that
 * the ziplist occupies, including the four bytes of the zlbytes field itself.
 * This value needs to be stored to be able to resize the entire structure
 * without the need to traverse it first.
 *
 * <uint32_t zltail> is the offset to the last entry in the list. This allows
 * a pop operation on the far side of the list without the need for full
 * traversal.
 *
 * <uint16_t zllen> is the number of entries. When there are more than
 * 2^16-2 entries, this value is set to 2^16-1 and we need to traverse the
 * entire list to know how many items it holds.
 *
 * <uint8_t zlend> is a special entry representing the end of the ziplist.
 * Is encoded as a single byte equal to 255. No other normal entry starts
 * with a byte set to the value of 255.
 *
 * ZIPLIST ENTRIES
 * ===============
 *
 * Every entry in the ziplist is prefixed by metadata that contains two pieces
 * of information. First, the length of the previous entry is stored to be
 * able to traverse the list from back to front. Second, the entry encoding is
 * provided. It represents the entry type, integer or string, and in the case
 * of strings it also represents the length of the string payload.
 * So a complete entry is stored like this:
 *
 * <prevlen> <encoding> <entry-data>
 *
 * Sometimes the encoding represents the entry itself, like for small integers
 * as we'll see later. In such a case the <entry-data> part is missing, and we
 * could have just:
 *
 * <prevlen> <encoding>
 *
 * The length of the previous entry, <prevlen>, is encoded in the following way:
 * If this length is smaller than 254 bytes, it will only consume a single
 * byte representing the length as an unsigned 8 bit integer. When the length
 * is greater than or equal to 254, it will consume 5 bytes. The first byte is
 * set to 254 (FE) to indicate a larger value is following. The remaining 4
 * bytes take the length of the previous entry as value.
 *
 * So practically an entry is encoded in the following way:
 *
 * <prevlen from 0 to 253> <encoding> <entry>
 *
 * Or alternatively if the previous entry length is greater than 253 bytes
 * the following encoding is used:
 *
 * 0xFE <4 bytes unsigned little endian prevlen> <encoding> <entry>
 *
 * The encoding field of the entry depends on the content of the
 * entry. When the entry is a string, the first 2 bits of the encoding first
 * byte will hold the type of encoding used to store the length of the string,
 * followed by the actual length of the string. When the entry is an integer
 * the first 2 bits are both set to 1. The following 2 bits are used to specify
 * what kind of integer will be stored after this header. An overview of the
 * different types and encodings is as follows. The first byte is always enough
 * to determine the kind of entry.
 *
 * |00pppppp| - 1 byte
 *      String value with length less than or equal to 63 bytes (6 bits).
 *      "pppppp" represents the unsigned 6 bit length.
 * |01pppppp|qqqqqqqq| - 2 bytes
 *      String value with length less than or equal to 16383 bytes (14 bits).
 *      IMPORTANT: The 14 bit number is stored in big endian.
 * |10000000|qqqqqqqq|rrrrrrrr|ssssssss|tttttttt| - 5 bytes
 *      String value with length greater than or equal to 16384 bytes.
 *      Only the 4 bytes following the first byte represents the length
 *      up to 2^32-1. The 6 lower bits of the first byte are not used and
 *      are set to zero.
 *      IMPORTANT: The 32 bit number is stored in big endian.
 * |11000000| - 3 bytes
 *      Integer encoded as int16_t (2 bytes).
 * |11010000| - 5 bytes
 *      Integer encoded as int32_t (4 bytes).
 * |11100000| - 9 bytes
 *      Integer encoded as int64_t (8 bytes).
 * |11110000| - 4 bytes
 *      Integer encoded as 24 bit signed (3 bytes).
 * |11111110| - 2 bytes
 *      Integer encoded as 8 bit signed (1 byte).
 * |1111xxxx| - (with xxxx between 0001 and 1101) immediate 4 bit integer.
 *      Unsigned integer from 0 to 12. The encoded value is actually from
 *      1 to 13 because 0000 and 1111 can not be used, so 1 should be
 *      subtracted from the encoded 4 bit value to obtain the right value.
 * |11111111| - End of ziplist special entry.
 *
 * Like for the ziplist header, all the integers are represented in little
 * endian byte order, even when this code is compiled in big endian systems.
 *
 * EXAMPLES OF ACTUAL ZIPLISTS
 * ===========================
 *
 * The following is a ziplist containing the two elements representing
 * the strings "2" and "5". It is composed of 15 bytes, that we visually
 * split into sections:
 *
 *  [0f 00 00 00] [0c 00 00 00] [02 00] [00 f3] [02 f6] [ff]
 *        |             |          |       |       |     |
 *     zlbytes        zltail     zllen    "2"     "5"   end
 *
 * The first 4 bytes represent the number 15, that is the number of bytes
 * the whole ziplist is composed of. The second 4 bytes are the offset
 * at which the last ziplist entry is found, that is 12, in fact the
 * last entry, that is "5", is at offset 12 inside the ziplist.
 * The next 16 bit integer represents the number of elements inside the
 * ziplist, its value is 2 since there are just two elements inside.
 * Finally "00 f3" is the first entry representing the number 2. It is
 * composed of the previous entry length, which is zero because this is
 * our first entry, and the byte F3 which corresponds to the encoding
 * |1111xxxx| with xxxx between 0001 and 1101. We need to remove the "F"
 * higher order bits 1111, and subtract 1 from the "3", so the entry value
 * is "2". The next entry has a prevlen of 02, since the first entry is
 * composed of exactly two bytes. The entry itself, F6, is encoded exactly
 * like the first entry, and 6-1 = 5, so the value of the entry is 5.
 * Finally the special entry FF signals the end of the ziplist.
 *
 * Adding another element to the above string with the value "Hello World"
 * allows us to show how the ziplist encodes small strings. We'll just show
 * the hex dump of the entry itself. Imagine the bytes as following the
 * entry that stores "5" in the ziplist above:
 *
 * [02] [0b] [48 65 6c 6c 6f 20 57 6f 72 6c 64]
 *
 * The first byte, 02, is the length of the previous entry. The next
 * byte represents the encoding in the pattern |00pppppp| that means
 * that the entry is a string of length <pppppp>, so 0B means that
 * an 11 bytes string follows. From the third byte (48) to the last (64)
 * there are just the ASCII characters for "Hello World".
 *
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 2009-2012, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 * Copyright (c) 2009-2017, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2020, Redis Labs, Inc
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
#include <stdint.h>
#include <limits.h>
#include "zmalloc.h"
#include "util.h"
#include "ziplist.h"
#include "config.h"
#include "endianconv.h"
#include "redisassert.h"

#define ZIP_END 255         /* Special "end of ziplist" entry. */
#define ZIP_BIG_PREVLEN 254 /* ZIP_BIG_PREVLEN - 1 is the max number of bytes of
                               the previous entry, for the "prevlen" field prefixing
                               each entry, to be represented with just a single byte.
                               Otherwise it is represented as FE AA BB CC DD, where
                               AA BB CC DD are a 4 bytes unsigned integer
                               representing the previous entry len. */

/* Different encoding/length possibilities */
// 不同的编码/长度的可能性（当content存储的是字节数组时，后续字节标识字节数组的实际长度；当content
// 存储的是整数时，可根据第3、第4位判断整数的具体类型）
// 1100 0000，取得字符的高两位
#define ZIP_STR_MASK 0xc0
// 0011 0000，取得字符的次高两位
#define ZIP_INT_MASK 0x30
// 最大长度位63的字符数组
#define ZIP_STR_06B (0 << 6)
// 最大长度为2的14 - 1的字节数组
#define ZIP_STR_14B (1 << 6)
// 最大长度为2的32次的字节数组
#define ZIP_STR_32B (2 << 6)
// 16位整数
#define ZIP_INT_16B (0xc0 | 0<<4)
// 32位整数
#define ZIP_INT_32B (0xc0 | 1<<4)
// 64位整数
#define ZIP_INT_64B (0xc0 | 2<<4)
// 24位整数
#define ZIP_INT_24B (0xc0 | 3<<4)
// 8位整数
#define ZIP_INT_8B 0xfe

/* 4 bit integer immediate encoding |1111xxxx| with xxxx between
 * 0001 and 1101. */
// 立即数的掩码
#define ZIP_INT_IMM_MASK 0x0f   /* Mask to extract the 4 bits value. To add
                                   one is needed to reconstruct the value. */
// 编码后的立即数最小值和最大值
#define ZIP_INT_IMM_MIN 0xf1    /* 11110001 */
#define ZIP_INT_IMM_MAX 0xfd    /* 11111101 */

#define INT24_MAX 0x7fffff
#define INT24_MIN (-INT24_MAX - 1)

/* Macro to determine if the entry is a string. String entries never start
 * with "11" as most significant bits of the first byte. */
#define ZIP_IS_STR(enc) (((enc) & ZIP_STR_MASK) < ZIP_STR_MASK)

/* Utility macros.*/

/* Return total bytes a ziplist is composed of. */
// 返回压缩列表组成的总的字节数
#define ZIPLIST_BYTES(zl)       (*((uint32_t*)(zl)))

/* Return the offset of the last item inside the ziplist. */
// 压缩列表尾条目在整个列表中的偏移
#define ZIPLIST_TAIL_OFFSET(zl) (*((uint32_t*)((zl)+sizeof(uint32_t))))

/* Return the length of a ziplist, or UINT16_MAX if the length cannot be
 * determined without scanning the whole ziplist. */
// 返回压缩列表的长度，如果是UINT16_MAX则表示除了扫描整个压缩列表不能决定长度
#define ZIPLIST_LENGTH(zl)      (*((uint16_t*)((zl)+sizeof(uint32_t)*2)))

/* The size of a ziplist header: two 32 bit integers for the total
 * bytes count and last item offset. One 16 bit integer for the number
 * of items field. */
// 压缩列表表头的大小：2个32为的整数分别为总的字节数目和最后条目的偏移。一个16位整数表示条目的个数
#define ZIPLIST_HEADER_SIZE     (sizeof(uint32_t)*2+sizeof(uint16_t))

/* Size of the "end of ziplist" entry. Just one byte. */
// ziplist结尾条目的大小。就一个字节
#define ZIPLIST_END_SIZE        (sizeof(uint8_t))

/* Return the pointer to the first entry of a ziplist. */
// 返回压缩列表中第一个条目的指针
#define ZIPLIST_ENTRY_HEAD(zl)  ((zl)+ZIPLIST_HEADER_SIZE)

/* Return the pointer to the last entry of a ziplist, using the
 * last entry offset inside the ziplist header. */
// 返回压缩列表的最后条目的指针
#define ZIPLIST_ENTRY_TAIL(zl)  ((zl)+intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl)))

/* Return the pointer to the last byte of a ziplist, which is, the
 * end of ziplist FF entry. */
// 返回压缩列表中的最后一个字节，就是压缩列表的FF条目
#define ZIPLIST_ENTRY_END(zl)   ((zl)+intrev32ifbe(ZIPLIST_BYTES(zl))-ZIPLIST_END_SIZE)

/* Increment the number of items field in the ziplist header. Note that this
 * macro should never overflow the unsigned 16 bit integer, since entries are
 * always pushed one at a time. When UINT16_MAX is reached we want the count
 * to stay there to signal that a full scan is needed to get the number of
 * items inside the ziplist. */
// 增加压缩列表的表头中条目数目字段的值。请注意，此宏不应溢出无符号16位整数，因为条目总是一次被压入一个。 
// 当达到 UINT16_MAX 时，我们希望计数保持在那里以表示需要进行全面扫描以获取ziplist中的项目数。
#define ZIPLIST_INCR_LENGTH(zl,incr) { \
    if (intrev16ifbe(ZIPLIST_LENGTH(zl)) < UINT16_MAX) \
        ZIPLIST_LENGTH(zl) = intrev16ifbe(intrev16ifbe(ZIPLIST_LENGTH(zl))+incr); \
}

/* Don't let ziplists grow over 1GB in any case, don't wanna risk overflow in
 * zlbytes */
#define ZIPLIST_MAX_SAFETY_SIZE (1<<30)
int ziplistSafeToAdd(unsigned char* zl, size_t add) {
    size_t len = zl? ziplistBlobLen(zl): 0;
    if (len + add > ZIPLIST_MAX_SAFETY_SIZE)
        return 0;
    return 1;
}

// 压缩列表的结构示意图
/*---------------------------------------------------------------
* |   previous_entry_length   |   encoding     |    content     |
* ---------------------------------------------------------------
*/
/* We use this function to receive information about a ziplist entry.
 * Note that this is not how the data is actually encoded, is just what we
 * get filled by a function in order to operate more easily. */
// 我们使用此函数接收有关ziplist条目的信息。请注意，这并不是数据的实际编码方式，只是我们通过函数填充的内容，以便于操作。
typedef struct zlentry {
    // 前一个编码长度的编码长度
    unsigned int prevrawlensize; /* Bytes used to encode the previous entry len*/
    // 前一个条目长度
    unsigned int prevrawlen;     /* Previous entry len. */
    // 用于编码此条目类型/长度的字节。例如，字符串具有 1、2 或 5 字节的标头。 整数总是使用一个字节。
    unsigned int lensize;        /* Bytes used to encode this entry type/len.
                                    For example strings have a 1, 2 or 5 bytes
                                    header. Integers always use a single byte.*/
    // 用于表示实际条目的字节数。 对于字符串，这就是字符串长度，而对于整数，它是 1、2、3、4、8 
    // 或 0个字节（对于 4 位立即数），具体取决于数字范围
    unsigned int len;            /* Bytes used to represent the actual entry.
                                    For strings this is just the string length
                                    while for integers it is 1, 2, 3, 4, 8 or
                                    0 (for 4 bit immediate) depending on the
                                    number range. */
    // 当前条目的首部长度，就是prevrawlensize+lensize
    unsigned int headersize;     /* prevrawlensize + lensize. */
    // 根据条目编码设置为ZIP_STR_*或ZIP_INT_*。然而，对于 4 位立即整数，这可以假定一个值范围并且必须进行范围检查。
    unsigned char encoding;      /* Set to ZIP_STR_* or ZIP_INT_* depending on
                                    the entry encoding. However for 4 bits
                                    immediate integers this can assume a range
                                    of values and must be range-checked. */
    // 当前条目首地址,即改指针指向prev-entry-len字段
    unsigned char *p;            /* Pointer to the very start of the entry, that
                                    is, this points to prev-entry-len field. */
} zlentry;

// 压缩列表条目清零
#define ZIPLIST_ENTRY_ZERO(zle) { \
    (zle)->prevrawlensize = (zle)->prevrawlen = 0; \
    (zle)->lensize = (zle)->len = (zle)->headersize = 0; \
    (zle)->encoding = 0; \
    (zle)->p = NULL; \
}

/* Extract the encoding from the byte pointed by 'ptr' and set it into
 * 'encoding' field of the zlentry structure. */
// 从'ptr'指向的字节中提取编码并将其设置到zlentry结构的'encoding'字段中。
#define ZIP_ENTRY_ENCODING(ptr, encoding) do {  \
    (encoding) = ((ptr)[0]); \
    if ((encoding) < ZIP_STR_MASK) (encoding) &= ZIP_STR_MASK; \
} while(0)

#define ZIP_ENCODING_SIZE_INVALID 0xff
/* Return the number of bytes required to encode the entry type + length.
 * On error, return ZIP_ENCODING_SIZE_INVALID */
// 返回编码条目类型+长度的字节数
static inline unsigned int zipEncodingLenSize(unsigned char encoding) {
    // 整数编码长度为1
    if (encoding == ZIP_INT_16B || encoding == ZIP_INT_32B ||
        encoding == ZIP_INT_24B || encoding == ZIP_INT_64B ||
        encoding == ZIP_INT_8B)
        return 1;
    // 立即数编码长度为1
    if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX)
        return 1;
    // 字符串编码根据内容长度不同而不同
    if (encoding == ZIP_STR_06B)
        return 1;
    if (encoding == ZIP_STR_14B)
        return 2;
    if (encoding == ZIP_STR_32B)
        return 5;
    return ZIP_ENCODING_SIZE_INVALID;
}
// 确定编码是否非法
#define ZIP_ASSERT_ENCODING(encoding) do {                                     \
    assert(zipEncodingLenSize(encoding) != ZIP_ENCODING_SIZE_INVALID);         \
} while (0)

/* Return bytes needed to store integer encoded by 'encoding' */
// 根据编码类型得到存储一个整数所需要的字节数
static inline unsigned int zipIntSize(unsigned char encoding) {
    switch(encoding) {
    case ZIP_INT_8B:  return 1;
    case ZIP_INT_16B: return 2;
    case ZIP_INT_24B: return 3;
    case ZIP_INT_32B: return 4;
    case ZIP_INT_64B: return 8;
    }
    if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX)
        return 0; /* 4 bit immediate */
    /* bad encoding, covered by a previous call to ZIP_ASSERT_ENCODING */
    redis_unreachable();
    return 0;
}

/* Write the encoding header of the entry in 'p'. If p is NULL it just returns
 * the amount of bytes required to encode such a length. Arguments:
 *
 * 'encoding' is the encoding we are using for the entry. It could be
 * ZIP_INT_* or ZIP_STR_* or between ZIP_INT_IMM_MIN and ZIP_INT_IMM_MAX
 * for single-byte small immediate integers.
 *
 * 'rawlen' is only used for ZIP_STR_* encodings and is the length of the
 * string that this entry represents.
 *
 * The function returns the number of bytes used by the encoding/length
 * header stored in 'p'. */
// 在条目p处写入一个编码的头，如果p为空指针只是返回编码该长度的数据需要的字节数。参数如下：
// encoding：条目编码，可以是ZIP_INT_*或者ZIP_STR_*或者是ZIP_INT_IMM_MIN和ZIP_INT_IMM_MAX之间的立即数。
// rawlen：只用于ZIP_STR_*编码，是该条目的字符串的长度。
// 该函数返回保存在指针p处的编码/长度所需要的字节数。
unsigned int zipStoreEntryEncoding(unsigned char *p, unsigned char encoding, unsigned int rawlen) {
    unsigned char len = 1, buf[5];
    // 字符串编码
    if (ZIP_IS_STR(encoding)) {
        /* Although encoding is given it may not be set for strings,
         * so we determine it here using the raw length. */
        // 如果小于0x3f，只需要1个字节存储
        if (rawlen <= 0x3f) {
            if (!p) return len;
            buf[0] = ZIP_STR_06B | rawlen;
        } 
        // 如果小于0x3fff，第一个字节保存类型+长度的高6位
        else if (rawlen <= 0x3fff) {
            len += 1;
            if (!p) return len;
            buf[0] = ZIP_STR_14B | ((rawlen >> 8) & 0x3f);
            buf[1] = rawlen & 0xff;
        } 
        // 大于0x3fff，直接用5个字节存储，第一个字节存储类型，后4个字节存储长度
        else {
            len += 4;
            if (!p) return len;
            buf[0] = ZIP_STR_32B;
            buf[1] = (rawlen >> 24) & 0xff;
            buf[2] = (rawlen >> 16) & 0xff;
            buf[3] = (rawlen >> 8) & 0xff;
            buf[4] = rawlen & 0xff;
        }
    } else {
        // 整数的编码/长度的存储字节数总是1
        /* Implies integer encoding, so length is always 1. */
        if (!p) return len;
        buf[0] = encoding;
    }

    /* Store this length at p. */
    memcpy(p,buf,len);
    return len;
}

/* Decode the entry encoding type and data length (string length for strings,
 * number of bytes used for the integer for integer entries) encoded in 'ptr'.
 * The 'encoding' variable is input, extracted by the caller, the 'lensize'
 * variable will hold the number of bytes required to encode the entry
 * length, and the 'len' variable will hold the entry length.
 * On invalid encoding error, lensize is set to 0. */
// 解码出ptr指针处的条目的编码类型和数据长度（字符串条目中字符串的长度，整数条目中整数的字节数）
// encoding变量是输入变量，被调用者提取出来，lensize用于保存“lensize”变量将保存编码条
// 目编码类型/长度所需的字节数，“len”变量将保存条目长度。 在无效编码错误时，lensize 设置为 0。 */
// lensize：编码+长度需要的编码字节数
// len：实际内容需要的字节数
#define ZIP_DECODE_LENGTH(ptr, encoding, lensize, len) do {                    \
    if ((encoding) < ZIP_STR_MASK) {                 /*字符串编码*/             \
        if ((encoding) == ZIP_STR_06B) {                                       \
            (lensize) = 1;                                                     \
            (len) = (ptr)[0] & 0x3f;                                           \
        } else if ((encoding) == ZIP_STR_14B) {                                \
            (lensize) = 2;                                                     \
            (len) = (((ptr)[0] & 0x3f) << 8) | (ptr)[1];                       \
        } else if ((encoding) == ZIP_STR_32B) {                                \
            (lensize) = 5;                                                     \
            (len) = ((uint32_t)(ptr)[1] << 24) |                               \
                    ((uint32_t)(ptr)[2] << 16) |                               \
                    ((uint32_t)(ptr)[3] <<  8) |                               \
                    ((uint32_t)(ptr)[4]);                                      \
        } else {                                                               \
            (lensize) = 0; /* bad encoding, should be covered by a previous */ \
            (len) = 0;     /* ZIP_ASSERT_ENCODING / zipEncodingLenSize, or  */ \
                           /* match the lensize after this macro with 0.    */ \
        }                                                                      \
    } else {                                    /*整数编码*/                    \
        (lensize) = 1;                                                         \
        if ((encoding) == ZIP_INT_8B)  (len) = 1;                              \
        else if ((encoding) == ZIP_INT_16B) (len) = 2;                         \
        else if ((encoding) == ZIP_INT_24B) (len) = 3;                         \
        else if ((encoding) == ZIP_INT_32B) (len) = 4;                         \
        else if ((encoding) == ZIP_INT_64B) (len) = 8;                         \
        else if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX)   \
            (len) = 0; /* 4 bit immediate */                                   \
        else                                                                   \
            (lensize) = (len) = 0; /* bad encoding */                          \
    }                                                                          \
} while(0)

/* Encode the length of the previous entry and write it to "p". This only
 * uses the larger encoding (required in __ziplistCascadeUpdate). */
// 编码前一个条目的长度并且写入p。这只用于比较大的数字的编码(大于或等于ZIP_BIG_PREVLEN)
// 返回需要的字节数
int zipStorePrevEntryLengthLarge(unsigned char *p, unsigned int len) {
    uint32_t u32;
    if (p != NULL) {
        // 第一个字节估定为ZIP_BIG_PREVLEN
        p[0] = ZIP_BIG_PREVLEN;
        u32 = len;
        // 后面的字节写入长度
        memcpy(p+1,&u32,sizeof(u32));
        // 进行大小端调整
        memrev32ifbe(p+1);
    }
    return 1 + sizeof(uint32_t);
}

/* Encode the length of the previous entry and write it to "p". Return the
 * number of bytes needed to encode this length if "p" is NULL. */
// 编码前一个条目的长度并且写入p。如果p为空指针，返回编码长度需要的字节数
unsigned int zipStorePrevEntryLength(unsigned char *p, unsigned int len) {
    // 如果p为空，直接计算字节数
    if (p == NULL) {
        return (len < ZIP_BIG_PREVLEN) ? 1 : sizeof(uint32_t) + 1;
    } else {
        // 小于ZIP_BIG_PREVLEN就用一个字节，放不下就是sizeof(uint32_t) + 1
        if (len < ZIP_BIG_PREVLEN) {
            p[0] = len;
            return 1;
        } else {
            // 编码大于ZIP_BIG_PREVLEN的长度
            return zipStorePrevEntryLengthLarge(p,len);
        }
    }
}

/* Return the number of bytes used to encode the length of the previous
 * entry. The length is returned by setting the var 'prevlensize'. */
// 返回上一个条目长度的编码字节数，该长度通过设置变量prevlensize返回
#define ZIP_DECODE_PREVLENSIZE(ptr, prevlensize) do {                          \
    if ((ptr)[0] < ZIP_BIG_PREVLEN) {                                          \
        (prevlensize) = 1;                                                     \
    } else {                                                                   \
        (prevlensize) = 5;                                                     \
    }                                                                          \
} while(0)

/* Return the length of the previous element, and the number of bytes that
 * are used in order to encode the previous element length.
 * 'ptr' must point to the prevlen prefix of an entry (that encodes the
 * length of the previous entry in order to navigate the elements backward).
 * The length of the previous entry is stored in 'prevlen', the number of
 * bytes needed to encode the previous entry length are stored in
 * 'prevlensize'. */
// 返回前一个元素的长度，以及用于编码前一个元素长度的字节数
// ptr必须指向条目的prevlen的开始处（编码前一个条目的长度以便向反向导航元素）。
// 前一个条目的长度保存在prevlen中，编码前一个条目长度所需的字节数保存在prevlensize中
#define ZIP_DECODE_PREVLEN(ptr, prevlensize, prevlen) do {                     \
    ZIP_DECODE_PREVLENSIZE(ptr, prevlensize);                                  \
    if ((prevlensize) == 1) {                                                  \
        (prevlen) = (ptr)[0];                                                  \
    } else { /* prevlensize == 5 */                                            \
        (prevlen) = ((ptr)[4] << 24) |                                         \
                    ((ptr)[3] << 16) |                                         \
                    ((ptr)[2] <<  8) |                                         \
                    ((ptr)[1]);                                                \
    }                                                                          \
} while(0)

/* Given a pointer 'p' to the prevlen info that prefixes an entry, this
 * function returns the difference in number of bytes needed to encode
 * the prevlen if the previous entry changes of size.
 *
 * So if A is the number of bytes used right now to encode the 'prevlen'
 * field.
 *
 * And B is the number of bytes that are needed in order to encode the
 * 'prevlen' if the previous element will be updated to one of size 'len'.
 *
 * Then the function returns B - A
 *
 * So the function returns a positive number if more space is needed,
 * a negative number if less space is needed, or zero if the same space
 * is needed. */
// 给定指针p指向前一个条目的长度的开始处，该函数返回如果前一个条目的长度发生变化，
// 那么编码前一个条目的长度在字节数上的变化
int zipPrevLenByteDiff(unsigned char *p, unsigned int len) {
    unsigned int prevlensize;
    // 得到编码当前的前一个条目所需要的字节数
    ZIP_DECODE_PREVLENSIZE(p, prevlensize);
    // 计算前一个条目新的长度需要的字节数和现在的差异
    return zipStorePrevEntryLength(NULL, len) - prevlensize;
}

/* Check if string pointed to by 'entry' can be encoded as an integer.
 * Stores the integer value in 'v' and its encoding in 'encoding'. */
// 检查entry指向的字符串是否可以解码为整数,将整数的值保存为v，并且将编码的信息保存到encoding
// 返回编码+长度需要的字节数
int zipTryEncoding(unsigned char *entry, unsigned int entrylen, long long *v, unsigned char *encoding) {
    long long value;

    if (entrylen >= 32 || entrylen == 0) return 0;
    // 将一个字符串转换成long long整数
    if (string2ll((char*)entry,entrylen,&value)) {
        /* Great, the string can be encoded. Check what's the smallest
         * of our encoding types that can hold this value. */
        // 解析成功后进行编码，encoding保存编码类型，v保存值，并返回编码/长度需要的字节数
        if (value >= 0 && value <= 12) {
            *encoding = ZIP_INT_IMM_MIN+value;
        } else if (value >= INT8_MIN && value <= INT8_MAX) {
            *encoding = ZIP_INT_8B;
        } else if (value >= INT16_MIN && value <= INT16_MAX) {
            *encoding = ZIP_INT_16B;
        } else if (value >= INT24_MIN && value <= INT24_MAX) {
            *encoding = ZIP_INT_24B;
        } else if (value >= INT32_MIN && value <= INT32_MAX) {
            *encoding = ZIP_INT_32B;
        } else {
            *encoding = ZIP_INT_64B;
        }
        *v = value;
        return 1;
    }
    return 0;
}

/* Store integer 'value' at 'p', encoded as 'encoding' */
// 在p指针处保存一个编码为encoding的整数值value
void zipSaveInteger(unsigned char *p, int64_t value, unsigned char encoding) {
    int16_t i16;
    int32_t i32;
    int64_t i64;
    // 一个字节直接赋值，多个字节拷贝进入以后，需要调整大小端
    if (encoding == ZIP_INT_8B) {
        ((int8_t*)p)[0] = (int8_t)value;
    } else if (encoding == ZIP_INT_16B) {
        i16 = value;
        memcpy(p,&i16,sizeof(i16));
        memrev16ifbe(p);
    } else if (encoding == ZIP_INT_24B) {
        i32 = ((uint64_t)value)<<8;
        memrev32ifbe(&i32);
        memcpy(p,((uint8_t*)&i32)+1,sizeof(i32)-sizeof(uint8_t));
    } else if (encoding == ZIP_INT_32B) {
        i32 = value;
        memcpy(p,&i32,sizeof(i32));
        memrev32ifbe(p);
    } else if (encoding == ZIP_INT_64B) {
        i64 = value;
        memcpy(p,&i64,sizeof(i64));
        memrev64ifbe(p);
    } 
    // 如果是[ZIP_INT_IMM_MIN, ZIP_INT_IMM_MAX]之间的立即数，数值保存在encoding里面，所以不需要处理
    else if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX) {
        /* Nothing to do, the value is stored in the encoding itself. */
    } else {
        assert(NULL);
    }
}

/* Read integer encoded as 'encoding' from 'p' */
// 根据编码，从指针p处读取一个整数
int64_t zipLoadInteger(unsigned char *p, unsigned char encoding) {
    int16_t i16;
    int32_t i32;
    int64_t i64, ret = 0;
    // 一个字节直接赋值，多个字节拷贝进入以后，需要调整大小端
    if (encoding == ZIP_INT_8B) {
        ret = ((int8_t*)p)[0];
    } else if (encoding == ZIP_INT_16B) {
        memcpy(&i16,p,sizeof(i16));
        memrev16ifbe(&i16);
        ret = i16;
    } else if (encoding == ZIP_INT_32B) {
        memcpy(&i32,p,sizeof(i32));
        memrev32ifbe(&i32);
        ret = i32;
    } else if (encoding == ZIP_INT_24B) {
        i32 = 0;
        memcpy(((uint8_t*)&i32)+1,p,sizeof(i32)-sizeof(uint8_t));
        memrev32ifbe(&i32);
        ret = i32>>8;
    } else if (encoding == ZIP_INT_64B) {
        memcpy(&i64,p,sizeof(i64));
        memrev64ifbe(&i64);
        ret = i64;
    } 
    // 如果是[ZIP_INT_IMM_MIN, ZIP_INT_IMM_MAX]之间的立即数，数值直接从encoding中提取，不需要读取p
    else if (encoding >= ZIP_INT_IMM_MIN && encoding <= ZIP_INT_IMM_MAX) {
        ret = (encoding & ZIP_INT_IMM_MASK)-1;
    } else {
        assert(NULL);
    }
    return ret;
}

/* Fills a struct with all information about an entry.
 * This function is the "unsafe" alternative to the one below.
 * Generally, all function that return a pointer to an element in the ziplist
 * will assert that this element is valid, so it can be freely used.
 * Generally functions such ziplistGet assume the input pointer is already
 * validated (since it's the return value of another function). */
// 使用一个条目的所有信息来填充一个结构体
// 此函数是下面函数的“不安全”替代函数。 通常，所有返回指向ziplist中元素的指针的
// 函数都会断言该元素是有效的，因此可以自由使用。通常，ziplistGet等函数假设输入指针已经验证
// （因为它是另一个函数的返回值） .
static inline void zipEntry(unsigned char *p, zlentry *e) {
    // 解码前一个条目的长度的编码字节数和前一个条目的长度
    ZIP_DECODE_PREVLEN(p, e->prevrawlensize, e->prevrawlen);
    // 解码当前条目的编码
    ZIP_ENTRY_ENCODING(p + e->prevrawlensize, e->encoding);
    // 解码出条目的编码类型和编码/长度的字节数
    ZIP_DECODE_LENGTH(p + e->prevrawlensize, e->encoding, e->lensize, e->len);
    assert(e->lensize != 0); /* check that encoding was valid. */
    // 得到头部的大小
    e->headersize = e->prevrawlensize + e->lensize;
    // 设置条目的开始处指针
    e->p = p;
}

/* Fills a struct with all information about an entry.
 * This function is safe to use on untrusted pointers, it'll make sure not to
 * try to access memory outside the ziplist payload.
 * Returns 1 if the entry is valid, and 0 otherwise. */
// 使用一个条目的所有信息来填充一个结构体
// 这个函数可以安全地用于不受信任的指针，它会确保不会尝试访问ziplist有效负载之外的内存。
// 如果条目有效则返回1，否则返回0。
static inline int zipEntrySafe(unsigned char* zl, size_t zlbytes, unsigned char *p, zlentry *e, int validate_prevlen) {
    // 第一个条目的地址
    unsigned char *zlfirst = zl + ZIPLIST_HEADER_SIZE;
    // 最后一个条目的地址
    unsigned char *zllast = zl + zlbytes - ZIPLIST_END_SIZE;
#define OUT_OF_RANGE(p) (unlikely((p) < zlfirst || (p) > zllast))

    /* If there's no possibility for the header to reach outside the ziplist,
     * take the fast path. (max lensize and prevrawlensize are both 5 bytes) */
    // 这种情况下不可能头部能碰到压缩表的外面，这是最快的路径
    // （len的最大字节数和前一个节点长度的最大字节数都是5）
    if (p >= zlfirst && p + 10 < zllast) {
        // 解析出来prevrawlensize和prevrawlen
        ZIP_DECODE_PREVLEN(p, e->prevrawlensize, e->prevrawlen);
        // 解析出来encoding
        ZIP_ENTRY_ENCODING(p + e->prevrawlensize, e->encoding);
        // 解析出来lensize和len
        ZIP_DECODE_LENGTH(p + e->prevrawlensize, e->encoding, e->lensize, e->len);
        // 计算头部的长度
        e->headersize = e->prevrawlensize + e->lensize;
        // 设置条目起始位置
        e->p = p;
        // 下面校验解析出来的数据的正确性
        /* We didn't call ZIP_ASSERT_ENCODING, so we check lensize was set to 0. */
        if (unlikely(e->lensize == 0))
            return 0;
        /* Make sure the entry doesn't reach outside the edge of the ziplist */
        // 确定解析出来的数据没有超出压缩表的范围
        if (OUT_OF_RANGE(p + e->headersize + e->len))
            return 0;
        /* Make sure prevlen doesn't reach outside the edge of the ziplist */
        // 确认前一个节点也没有超出压缩表的范围
        if (validate_prevlen && OUT_OF_RANGE(p - e->prevrawlen))
            return 0;
        return 1;
    }

    /* Make sure the pointer doesn't reach outside the edge of the ziplist */
    // 确定p没有超出压缩表的范围
    if (OUT_OF_RANGE(p))
        return 0;

    /* Make sure the encoded prevlen header doesn't reach outside the allocation */
    // 检查解析完前一个条目的长度后，是否超出了压缩表的范围
    ZIP_DECODE_PREVLENSIZE(p, e->prevrawlensize);
    if (OUT_OF_RANGE(p + e->prevrawlensize))
        return 0;

    /* Make sure encoded entry header is valid. */
    // 解析条目的编码是否合法
    ZIP_ENTRY_ENCODING(p + e->prevrawlensize, e->encoding);
    // 根据编码得到长度的字节数
    e->lensize = zipEncodingLenSize(e->encoding);
    // 校验长度的字节数是否合法
    if (unlikely(e->lensize == ZIP_ENCODING_SIZE_INVALID))
        return 0;

    /* Make sure the encoded entry header doesn't reach outside the allocation */
    // 确定解析完的前一个节点和当前节点的编码/长度后，没有超出压缩表的范围
    if (OUT_OF_RANGE(p + e->prevrawlensize + e->lensize))
        return 0;

    /* Decode the prevlen and entry len headers. */
    // 解析prevrawlensize和prevrawlen
    ZIP_DECODE_PREVLEN(p, e->prevrawlensize, e->prevrawlen);
    // 解析lensize和len
    ZIP_DECODE_LENGTH(p + e->prevrawlensize, e->encoding, e->lensize, e->len);
    // 设置条目头部的尺寸
    e->headersize = e->prevrawlensize + e->lensize;

    /* Make sure the entry doesn't reach outside the edge of the ziplist */
    // 确定条目的头部+数值内容的范围没有超出压缩表的范围
    if (OUT_OF_RANGE(p + e->headersize + e->len))
        return 0;

    /* Make sure prevlen doesn't reach outside the edge of the ziplist */
    // 校验前一个条目没有超出压缩表的范围
    if (validate_prevlen && OUT_OF_RANGE(p - e->prevrawlen))
        return 0;
    // 设置条目的其实位置
    e->p = p;
    // 成功解析返回1
    return 1;
#undef OUT_OF_RANGE
}

/* Return the total number of bytes used by the entry pointed to by 'p'. */
// 返回条目指针p指向的条目所占的总的字节数
static inline unsigned int zipRawEntryLengthSafe(unsigned char* zl, size_t zlbytes, unsigned char *p) {
    zlentry e;
    assert(zipEntrySafe(zl, zlbytes, p, &e, 0));
    return e.headersize + e.len;
}

/* Return the total number of bytes used by the entry pointed to by 'p'. */
// 返回条目指针p指向的条目所占的总的字节数
static inline unsigned int zipRawEntryLength(unsigned char *p) {
    zlentry e;
    zipEntry(p, &e);
    return e.headersize + e.len;
}

/* Validate that the entry doesn't reach outside the ziplist allocation. */
// 校验条目没有超出压缩表的整个范围
static inline void zipAssertValidEntry(unsigned char* zl, size_t zlbytes, unsigned char *p) {
    zlentry e;
    assert(zipEntrySafe(zl, zlbytes, p, &e, 1));
}

/* Create a new empty ziplist. */
// 创建一个空的压缩表
unsigned char *ziplistNew(void) {
    // 计算空表需要的字节数 = 头部长度+尾部长度
    unsigned int bytes = ZIPLIST_HEADER_SIZE+ZIPLIST_END_SIZE;
    // 分配长度
    unsigned char *zl = zmalloc(bytes);
    // 设置压缩表整个长度
    ZIPLIST_BYTES(zl) = intrev32ifbe(bytes);
    // 设置最后条目在压缩表中的偏移
    ZIPLIST_TAIL_OFFSET(zl) = intrev32ifbe(ZIPLIST_HEADER_SIZE);
    // 设置条目数
    ZIPLIST_LENGTH(zl) = 0;
    // 设置尾部的值
    zl[bytes-1] = ZIP_END;
    return zl;
}

/* Resize the ziplist. */
// 调整压缩表的尺寸
unsigned char *ziplistResize(unsigned char *zl, size_t len) {
    assert(len < UINT32_MAX);
    // 重新分配
    zl = zrealloc(zl,len);
    // 设置压缩表的长度
    ZIPLIST_BYTES(zl) = intrev32ifbe(len);
    // 设置尾部
    zl[len-1] = ZIP_END;
    return zl;
}

/* When an entry is inserted, we need to set the prevlen field of the next
 * entry to equal the length of the inserted entry. It can occur that this
 * length cannot be encoded in 1 byte and the next entry needs to be grow
 * a bit larger to hold the 5-byte encoded prevlen. This can be done for free,
 * because this only happens when an entry is already being inserted (which
 * causes a realloc and memmove). However, encoding the prevlen may require
 * that this entry is grown as well. This effect may cascade throughout
 * the ziplist when there are consecutive entries with a size close to
 * ZIP_BIG_PREVLEN, so we need to check that the prevlen can be encoded in
 * every consecutive entry.
 *
 * Note that this effect can also happen in reverse, where the bytes required
 * to encode the prevlen field can shrink. This effect is deliberately ignored,
 * because it can cause a "flapping" effect where a chain prevlen fields is
 * first grown and then shrunk again after consecutive inserts. Rather, the
 * field is allowed to stay larger than necessary, because a large prevlen
 * field implies the ziplist is holding large entries anyway.
 *
 * The pointer "p" points to the first entry that does NOT need to be
 * updated, i.e. consecutive fields MAY need an update. */
// 插入条目时，我们需要将下一个条目的prevelen字段设置为等于插入条目的长度。可能发生的情况是，
// 该长度不能以1字节编码，下一个条目需要增长一点以容纳5字节编码的prevelen。这可以免费完成，
// 因为这只在已经插入条目时发生（这会导致realloc和memmove）。但是，对prevelen进行编码可能
// 需要同时增长此条目。当存在大小接近ZIP_BIG_PREVLEN的连续条目时，此效果可能会在整个ziplist中级联，
// 因此我们需要检查prevelen是否可以在每个连续条目中编码。

// 请注意，这种影响也可能反过来发生，因为编码prevelen字段所需的字节可能会减少。
// 当链条的prevlen字段开始增长，然后在连续的插入然后收缩，它会导致一种“拍打”效应，
// 这种效应被故意忽略了.相反，允许该字段保持大于所需的大小，因为较大的prevelen字段意味着压缩列表
// 无论如何都包含较大的条目。
// 指针p指向第一个不需要更新的条目
unsigned char *__ziplistCascadeUpdate(unsigned char *zl, unsigned char *p) {
    zlentry cur;
    size_t prevlen, prevlensize, prevoffset; /* Informat of the last changed entry. */
    size_t firstentrylen; /* Used to handle insert at head. */
    size_t rawlen, curlen = intrev32ifbe(ZIPLIST_BYTES(zl));
    size_t extra = 0, cnt = 0, offset;
    // 更新条目的prevelen所需的额外字节
    size_t delta = 4; /* Extra bytes needed to update a entry's prevlen (5-1). */
    unsigned char *tail = zl + intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl));

    /* Empty ziplist */
    // 空的压缩表
    if (p[0] == ZIP_END) return zl;
    // 解析p指向的条目
    zipEntry(p, &cur); /* no need for "safe" variant since the input pointer was validated by the function that returned it. */
    // 得到第一个条目的长度
    firstentrylen = prevlen = cur.headersize + cur.len;
    // 得到条目长度需要的字节数
    prevlensize = zipStorePrevEntryLength(NULL, prevlen);
    prevoffset = p - zl;
    // prevlen目前等于当前节点的整个长度，
    // 下面表示跳到下一个节点
    p += prevlen;

    /* Iterate ziplist to find out how many extra bytes do we need to update it. */
    // 遍历压缩表，找出需要更新的条目
    while (p[0] != ZIP_END) {
        // 解析p指向的条目的信息
        assert(zipEntrySafe(zl, curlen, p, &cur, 0));

        /* Abort when "prevlen" has not changed. */
        // 如果前一个条目的长度没有变化，更新到此结束
        if (cur.prevrawlen == prevlen) break;

        /* Abort when entry's "prevlensize" is big enough. */
        // 如果条目的prevlensize比需要的还大
        if (cur.prevrawlensize >= prevlensize) {
            // 如果前一个条目长度的字节数合适，保存
            if (cur.prevrawlensize == prevlensize) {
                zipStorePrevEntryLength(p, prevlen);
            } else {
                /* This would result in shrinking, which we want to avoid.
                 * So, set "prevlen" in the available bytes. */
                // 这会导致我们所要避免的收缩，所以在可用的字节处设置prevlen
                zipStorePrevEntryLengthLarge(p, prevlen);
            }
            break;
        }

        /* cur.prevrawlen means cur is the former head entry. */
        assert(cur.prevrawlen == 0 || cur.prevrawlen + delta == prevlen);

        /* Update prev entry's info and advance the cursor. */
        // 更新上一个条目的信息并推进指针
        // 如果更新这个节点更新，就是原来是1个字节的prevlen
        rawlen = cur.headersize + cur.len;
        // 所以现在该节点的prevlen变成5个字节，需要增加4个字节
        prevlen = rawlen + delta; 
        // 下一个条目中保存的前一个条目长度需要的字节数
        prevlensize = zipStorePrevEntryLength(NULL, prevlen);
        prevoffset = p - zl;
        p += rawlen;
        extra += delta;
        cnt++;
    }

    /* Extra bytes is zero all update has been done(or no need to update). */
    // 所有的都更新完成（或者不需要更新）
    if (extra == 0) return zl;

    /* Update tail offset after loop. */
    // 在循环过后，更新tail
    if (tail == zl + prevoffset) {
        /* When the last entry we need to update is also the tail, update tail offset
         * unless this is the only entry that was updated (so the tail offset didn't change). */
        // 当我们需要更新的最后一个条目也是尾部时，更新尾部偏移，除非这是唯一更新的条目（因此尾部偏移没有改变）。
        // 需要将多加的一个delta在下面减去
        if (extra - delta != 0) {
            ZIPLIST_TAIL_OFFSET(zl) =
                intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+extra-delta);
        }
    } else {
        /* Update the tail offset in cases where the last entry we updated is not the tail. */
        // 如果我们更新的最后一个条目不是尾部，则更新尾部偏移
        ZIPLIST_TAIL_OFFSET(zl) =
            intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+extra);
    }

    /* Now "p" points at the first unchanged byte in original ziplist,
     * move data after that to new ziplist. */
    // 得到不需要变化的偏移处
    offset = p - zl;
    // 调整压缩表的大小
    zl = ziplistResize(zl, curlen + extra);
    // 得到新的开始处
    p = zl + offset;
    // 拷贝后面那一截不变化的
    memmove(p + extra, p, curlen - offset - 1);
    p += extra;

    /* Iterate all entries that need to be updated tail to head. */
    // 遍历所有从尾到头需要更新的条目
    while (cnt) {
        // 解码条目
        zipEntry(zl + prevoffset, &cur); /* no need for "safe" variant since we already iterated on all these entries above. */
        // 得到整个的长度
        rawlen = cur.headersize + cur.len;
        /* Move entry to tail and reset prevlen. */
        // 将除了前一个条目长度的部分拷贝过去
        memmove(p - (rawlen - cur.prevrawlensize), 
                zl + prevoffset + cur.prevrawlensize, 
                rawlen - cur.prevrawlensize);
        // 移动条目开始处
        p -= (rawlen + delta);
        // 
        if (cur.prevrawlen == 0) {
            /* "cur" is the previous head entry, update its prevlen with firstentrylen. */
            // “cur”是上一个head条目，用第一个条目长度更新其prevelen
            zipStorePrevEntryLength(p, firstentrylen);
        } else {
            /* An entry's prevlen can only increment 4 bytes. */
            // 变化的条目，就是在其长度上增加4个字节
            zipStorePrevEntryLength(p, cur.prevrawlen+delta);
        }
        /* Forward to previous entry. */
        // 进入前一个条目
        prevoffset -= cur.prevrawlen;
        cnt--;
    }
    return zl;
}

/* Delete "num" entries, starting at "p". Returns pointer to the ziplist. */
// 删除num个条目，从p开始。返回压缩表的指针
unsigned char *__ziplistDelete(unsigned char *zl, unsigned char *p, unsigned int num) {
    unsigned int i, totlen, deleted = 0;
    size_t offset;
    int nextdiff = 0;
    zlentry first, tail;
    // 得到总长度
    size_t zlbytes = intrev32ifbe(ZIPLIST_BYTES(zl));
    // 解析第一个要删除的条目
    zipEntry(p, &first); /* no need for "safe" variant since the input pointer was validated by the function that returned it. */
    // 遍历所有的删除条目，p指向最后一个删除的条目后面一个条目
    for (i = 0; p[0] != ZIP_END && i < num; i++) {
        p += zipRawEntryLengthSafe(zl, zlbytes, p);
        deleted++;
    }

    assert(p >= first.p);
    // 得到删除的字节数
    totlen = p-first.p; /* Bytes taken by the element(s) to delete. */
    if (totlen > 0) {
        uint32_t set_tail;
        // 如果没有删除到最后
        if (p[0] != ZIP_END) {
            /* Storing `prevrawlen` in this entry may increase or decrease the
             * number of bytes required compare to the current `prevrawlen`.
             * There always is room to store this, because it was previously
             * stored by an entry that is now being deleted. */
            // 得到最后一个删除节点后面的一个节点保存前一个节点长度的字节数变化
            nextdiff = zipPrevLenByteDiff(p,first.prevrawlen);

            /* Note that there is always space when p jumps backward: if
             * the new previous entry is large, one of the deleted elements
             * had a 5 bytes prevlen header, so there is for sure at least
             * 5 bytes free and we need just 4. */
            p -= nextdiff;
            assert(p >= first.p && p<zl+zlbytes-1);
            // 修改最后一个删除节点后面的一个节点中前一个节点长度的值
            zipStorePrevEntryLength(p,first.prevrawlen);

            /* Update offset for tail */
            // 更新最后一个节点的偏移
            set_tail = intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))-totlen;

            /* When the tail contains more than one entry, we need to take
             * "nextdiff" in account as well. Otherwise, a change in the
             * size of prevlen doesn't have an effect on the *tail* offset. */
            assert(zipEntrySafe(zl, zlbytes, p, &tail, 1));
            // 如果当前的条目不是最后一个条目
            // 如果只剩下一个节点，那set_tail就是第一个删除的其实处，不需要考虑nextdiff
            // 如果不只一个节点，那么得考虑最后一个删除节点后面的一个节点中因为前一个长度节点数的变化导致的整个条目的长度变化。
            if (p[tail.headersize+tail.len] != ZIP_END) {
                // 修正最后一个条目偏移
                set_tail = set_tail + nextdiff;
            }

            /* Move tail to the front of the ziplist */
            /* since we asserted that p >= first.p. we know totlen >= 0,
             * so we know that p > first.p and this is guaranteed not to reach
             * beyond the allocation, even if the entries lens are corrupted. */
            // p已经调整到新的位置，拷贝剩下的部分
            size_t bytes_to_move = zlbytes-(p-zl)-1;
            memmove(first.p,p,bytes_to_move);
        }
        // 删除到最后的条目
        else {
            /* The entire tail was deleted. No need to move memory. */
            // 得到最后一个条目的开始处
            set_tail = (first.p-zl)-first.prevrawlen;
        }

        /* Resize the ziplist */
        // 得到第一个删除条目的偏移
        offset = first.p-zl;
        // 计算删除后的压缩表的长度
        zlbytes -= totlen - nextdiff;
        // 重新调整压缩表长度
        zl = ziplistResize(zl, zlbytes);
        // 指针重新指向第一个删除的位置
        p = zl+offset;

        /* Update record count */
        // 更新压缩表中条目的数目
        ZIPLIST_INCR_LENGTH(zl,-deleted);

        /* Set the tail offset computed above */
        assert(set_tail <= zlbytes - ZIPLIST_END_SIZE);
        // 设置最后一个指针的开始处
        ZIPLIST_TAIL_OFFSET(zl) = intrev32ifbe(set_tail);

        /* When nextdiff != 0, the raw length of the next entry has changed, so
         * we need to cascade the update throughout the ziplist */
        // 最后一个删除条目后面的一个条目的整个条目长度发生变化，需要更新后面的
        if (nextdiff != 0)
            zl = __ziplistCascadeUpdate(zl,p);
    }
    return zl;
}

/* Insert item at "p". */
// 在p处插入一个条目
unsigned char *__ziplistInsert(unsigned char *zl, unsigned char *p, unsigned char *s, unsigned int slen) {
    size_t curlen = intrev32ifbe(ZIPLIST_BYTES(zl)), reqlen, newlen;
    unsigned int prevlensize, prevlen = 0;
    size_t offset;
    int nextdiff = 0;
    unsigned char encoding = 0;
    long long value = 123456789; /* initialized to avoid warning. Using a value
                                    that is easy to see if for some reason
                                    we use it uninitialized. */
    zlentry tail;

    /* Find out prevlen for the entry that is inserted. */
    // 不是插入在最后
    if (p[0] != ZIP_END) {
        ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
    } else {
        // 插入在最后，成为最后条目
        unsigned char *ptail = ZIPLIST_ENTRY_TAIL(zl);
        if (ptail[0] != ZIP_END) {
            prevlen = zipRawEntryLengthSafe(zl, curlen, ptail);
        }
    }

    /* See if the entry can be encoded */
    // 是否可以解码为整数
    if (zipTryEncoding(s,slen,&value,&encoding)) {
        /* 'encoding' is set to the appropriate integer encoding */
        // encoding设置为合适的整数编码
        reqlen = zipIntSize(encoding);
    } else {
        /* 'encoding' is untouched, however zipStoreEntryEncoding will use the
         * string length to figure out how to encode it. */
        // encoding未受影响，但zipStoreEntryEncoding将使用字符串长度来确定如何对其进行编码
        reqlen = slen;
    }
    /* We need space for both the length of the previous entry and
     * the length of the payload. */
    // 前一个条目长度需要的编码
    reqlen += zipStorePrevEntryLength(NULL,prevlen);
    // 当前的编码/长度需要的字节数目
    reqlen += zipStoreEntryEncoding(NULL,encoding,slen);

    /* When the insert position is not equal to the tail, we need to
     * make sure that the next entry can hold this entry's length in
     * its prevlen field. */
    // 如果位置不为最后面，需要确认下一个条目保存插入条目的长度，是否需要改变尺寸
    int forcelarge = 0;
    nextdiff = (p[0] != ZIP_END) ? zipPrevLenByteDiff(p,reqlen) : 0;
    // 收缩的大小比整个条目还大
    if (nextdiff == -4 && reqlen < 4) {
        nextdiff = 0;
        forcelarge = 1;
    }

    /* Store offset because a realloc may change the address of zl. */
    // 计算插入点的偏移
    offset = p-zl;
    // 得到新的长度
    newlen = curlen+reqlen+nextdiff;
    // 重新调整尺寸
    zl = ziplistResize(zl,newlen);
    // 重新计算插入点的指针
    p = zl+offset;

    /* Apply memory move when necessary and update tail offset. */
    // 移动需要的内存并且更新尾部偏移
    if (p[0] != ZIP_END) {
        /* Subtract one because of the ZIP_END bytes */
        // 移动后面的条目
        memmove(p+reqlen,p-nextdiff,curlen-offset-1+nextdiff);

        /* Encode this entry's raw length in the next entry. */
        // 强制增大，使用5个字节的前一个节点的长度
        if (forcelarge)
            zipStorePrevEntryLengthLarge(p+reqlen,reqlen);
        else
            zipStorePrevEntryLength(p+reqlen,reqlen);

        /* Update offset for tail */
        // 更新最后一个条目的偏移
        ZIPLIST_TAIL_OFFSET(zl) =
            intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+reqlen);

        /* When the tail contains more than one entry, we need to take
         * "nextdiff" in account as well. Otherwise, a change in the
         * size of prevlen doesn't have an effect on the *tail* offset. */
        // 解析原来在p位置的条目
        assert(zipEntrySafe(zl, newlen, p+reqlen, &tail, 1));
        // 原来p指向的不是最后一个条目，最后一个条目的偏移需要修正
        if (p[reqlen+tail.headersize+tail.len] != ZIP_END) {
            ZIPLIST_TAIL_OFFSET(zl) =
                intrev32ifbe(intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl))+nextdiff);
        }
    } else {
        /* This element will be the new tail. */
        // 调整最后一个条目的偏移
        ZIPLIST_TAIL_OFFSET(zl) = intrev32ifbe(p-zl);
    }

    /* When nextdiff != 0, the raw length of the next entry has changed, so
     * we need to cascade the update throughout the ziplist */
    // 如果p后面紧接着的条目的前一个条目长度编码的字节数发生变化，后面的条目可能多个需要修正
    if (nextdiff != 0) {
        offset = p-zl;
        zl = __ziplistCascadeUpdate(zl,p+reqlen);
        p = zl+offset;
    }

    /* Write the entry */
    // 将当前的条目的数据写入
    p += zipStorePrevEntryLength(p,prevlen);
    p += zipStoreEntryEncoding(p,encoding,slen);
    if (ZIP_IS_STR(encoding)) {
        memcpy(p,s,slen);
    } else {
        zipSaveInteger(p,value,encoding);
    }
    ZIPLIST_INCR_LENGTH(zl,1);
    return zl;
}

/* Merge ziplists 'first' and 'second' by appending 'second' to 'first'.
 *
 * NOTE: The larger ziplist is reallocated to contain the new merged ziplist.
 * Either 'first' or 'second' can be used for the result.  The parameter not
 * used will be free'd and set to NULL.
 *
 * After calling this function, the input parameters are no longer valid since
 * they are changed and free'd in-place.
 *
 * The result ziplist is the contents of 'first' followed by 'second'.
 *
 * On failure: returns NULL if the merge is impossible.
 * On success: returns the merged ziplist (which is expanded version of either
 * 'first' or 'second', also frees the other unused input ziplist, and sets the
 * input ziplist argument equal to newly reallocated ziplist return value. */
// 通过将压缩表first追加到second的后面进行合并
unsigned char *ziplistMerge(unsigned char **first, unsigned char **second) {
    /* If any params are null, we can't merge, so NULL. */
    if (first == NULL || *first == NULL || second == NULL || *second == NULL)
        return NULL;

    /* Can't merge same list into itself. */
    if (*first == *second)
        return NULL;
    // 第一个压缩列表的字节数
    size_t first_bytes = intrev32ifbe(ZIPLIST_BYTES(*first));
    // 第一个压缩列表的条目数
    size_t first_len = intrev16ifbe(ZIPLIST_LENGTH(*first));

    // 第二个压缩列表的字节数
    size_t second_bytes = intrev32ifbe(ZIPLIST_BYTES(*second));
    // 第二个压缩列表的条目数
    size_t second_len = intrev16ifbe(ZIPLIST_LENGTH(*second));

    int append;
    unsigned char *source, *target;
    size_t target_bytes, source_bytes;
    /* Pick the largest ziplist so we can resize easily in-place.
     * We must also track if we are now appending or prepending to
     * the target ziplist. */
    // 选择最大的压缩表以便更容易就地调整大小，我们必须决定现在是从后面追加还是前面追加到目标压缩表。
    if (first_len >= second_len) {
        /* retain first, append second to first. */
        // 保留第一个，然后将第二个追加到第一个
        target = *first;
        target_bytes = first_bytes;
        source = *second;
        source_bytes = second_bytes;
        append = 1;
    } else {
        /* else, retain second, prepend first to second. */
        // 保留第二个，然后将第一个追加到前面
        target = *second;
        target_bytes = second_bytes;
        source = *first;
        source_bytes = first_bytes;
        append = 0;
    }

    /* Calculate final bytes (subtract one pair of metadata) */
    // 计算最终的字节数
    size_t zlbytes = first_bytes + second_bytes -
                     ZIPLIST_HEADER_SIZE - ZIPLIST_END_SIZE;
    // 计算最终的条目数
    size_t zllength = first_len + second_len;

    /* Combined zl length should be limited within UINT16_MAX */
    // 压缩表的长度限制在UINT16_MAX
    zllength = zllength < UINT16_MAX ? zllength : UINT16_MAX;

    /* larger values can't be stored into ZIPLIST_BYTES */
    // 确定整个压缩表的字节数小于UINT32_MAX
    assert(zlbytes < UINT32_MAX);

    /* Save offset positions before we start ripping memory apart. */
    // 保存压缩表最后一个条目在压缩表中的偏移
    size_t first_offset = intrev32ifbe(ZIPLIST_TAIL_OFFSET(*first));
    size_t second_offset = intrev32ifbe(ZIPLIST_TAIL_OFFSET(*second));

    /* Extend target to new zlbytes then append or prepend source. */
    // 扩展目标到新的字节数，然后追加或者前面加入源
    target = zrealloc(target, zlbytes);
    // 追加
    if (append) {
        /* append == appending to target */
        /* Copy source after target (copying over original [END]):
         *   [TARGET - END, SOURCE - HEADER] */
        // 直接追加拷贝
        memcpy(target + target_bytes - ZIPLIST_END_SIZE,
               source + ZIPLIST_HEADER_SIZE,
               source_bytes - ZIPLIST_HEADER_SIZE);
    } else {
        /* !append == prepending to target */
        /* Move target *contents* exactly size of (source - [END]),
         * then copy source into vacated space (source - [END]):
         *   [SOURCE - END, TARGET - HEADER] */
        // 将目标压缩表移动到新的压缩表的后部
        memmove(target + source_bytes - ZIPLIST_END_SIZE,
                target + ZIPLIST_HEADER_SIZE,
                target_bytes - ZIPLIST_HEADER_SIZE);
        // 拷贝进前面
        memcpy(target, source, source_bytes - ZIPLIST_END_SIZE);
    }

    /* Update header metadata. */
    // 更新总的字节数和条目数
    ZIPLIST_BYTES(target) = intrev32ifbe(zlbytes);
    ZIPLIST_LENGTH(target) = intrev16ifbe(zllength);
    /* New tail offset is:
     *   + N bytes of first ziplist
     *   - 1 byte for [END] of first ziplist
     *   + M bytes for the offset of the original tail of the second ziplist
     *   - J bytes for HEADER because second_offset keeps no header. */
    // 计算新的最后一个条目的偏移
    ZIPLIST_TAIL_OFFSET(target) = intrev32ifbe(
                                   (first_bytes - ZIPLIST_END_SIZE) +
                                   (second_offset - ZIPLIST_HEADER_SIZE));

    /* __ziplistCascadeUpdate just fixes the prev length values until it finds a
     * correct prev length value (then it assumes the rest of the list is okay).
     * We tell CascadeUpdate to start at the first ziplist's tail element to fix
     * the merge seam. */
    // 因为原来后面的压缩表现在需要考虑前一个条目的偏移的字节数，所以需要更新
    target = __ziplistCascadeUpdate(target, target+first_offset);

    /* Now free and NULL out what we didn't realloc */
    // 根据在前面追加还是后面，释放对应的压缩表
    if (append) {
        zfree(*second);
        *second = NULL;
        *first = target;
    } else {
        zfree(*first);
        *first = NULL;
        *second = target;
    }
    return target;
}

// 根据where的值，在压缩表的前面还是后面插入一个条目
unsigned char *ziplistPush(unsigned char *zl, unsigned char *s, unsigned int slen, int where) {
    unsigned char *p;
    p = (where == ZIPLIST_HEAD) ? ZIPLIST_ENTRY_HEAD(zl) : ZIPLIST_ENTRY_END(zl);
    return __ziplistInsert(zl,p,s,slen);
}

/* Returns an offset to use for iterating with ziplistNext. When the given
 * index is negative, the list is traversed back to front. When the list
 * doesn't contain an element at the provided index, NULL is returned. */
// 返回一个用于ZiplistNext遍历的指定索引对应条目的偏移。当给定的索引为负数，列表从后到前遍历。
// 如果列表不能包含一个指定索引的元素，返回NULL。
unsigned char *ziplistIndex(unsigned char *zl, int index) {
    unsigned char *p;
    unsigned int prevlensize, prevlen = 0;
    // 得到压缩表总的字节数
    size_t zlbytes = intrev32ifbe(ZIPLIST_BYTES(zl));
    // 从后到前遍历
    if (index < 0) {
        index = (-index)-1;
        // 得到最后一个条目
        p = ZIPLIST_ENTRY_TAIL(zl);
        // 如果不是空表
        if (p[0] != ZIP_END) {
            /* No need for "safe" check: when going backwards, we know the header
             * we're parsing is in the range, we just need to assert (below) that
             * the size we take doesn't cause p to go outside the allocation. */
            // 解码前一个条码的字节数
            ZIP_DECODE_PREVLENSIZE(p, prevlensize);
            // 确定范围还在压缩表范围内
            assert(p + prevlensize < zl + zlbytes - ZIPLIST_END_SIZE);
            ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
            // 没有遍历到最前面一个，并且索引值还大于0，继续遍历
            while (prevlen > 0 && index--) {
                // 遍历到前一个条目
                p -= prevlen;
                // 确定没有超出压缩表的范围
                assert(p >= zl + ZIPLIST_HEADER_SIZE && p < zl + zlbytes - ZIPLIST_END_SIZE);
                // 解析前一个条目的字节数
                ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
            }
        }
    } else {
        p = ZIPLIST_ENTRY_HEAD(zl);
        // 从前到后遍历
        while (index--) {
            /* Use the "safe" length: When we go forward, we need to be careful
             * not to decode an entry header if it's past the ziplist allocation. */
            // 遍历下一个条目
            p += zipRawEntryLengthSafe(zl, zlbytes, p);
            // 如果是尾部，表示后面没有条目了
            if (p[0] == ZIP_END)
                break;
        }
    }
    // 如果在尾部或者索引还没遍历完，返回NULL
    if (p[0] == ZIP_END || index > 0)
        return NULL;
    // 检查条目没有超出压缩表的返回
    zipAssertValidEntry(zl, zlbytes, p);
    return p;
}

/* Return pointer to next entry in ziplist.
 *
 * zl is the pointer to the ziplist
 * p is the pointer to the current element
 *
 * The element after 'p' is returned, otherwise NULL if we are at the end. */
// 返回压缩表中下一个条目的指针
// z1是压缩表的指针
// p是当前条目的指针
unsigned char *ziplistNext(unsigned char *zl, unsigned char *p) {
    ((void) zl);
    // 得到压缩包所有的字节数
    size_t zlbytes = intrev32ifbe(ZIPLIST_BYTES(zl));

    /* "p" could be equal to ZIP_END, caused by ziplistDelete,
     * and we should return NULL. Otherwise, we should return NULL
     * when the *next* element is ZIP_END (there is no next entry). */
    // 当前的条目是否已经是末尾
    if (p[0] == ZIP_END) {
        return NULL;
    }
    // 下一个条目的起始指针 = 当前条目的起始指针 + 当前条目占的字节数
    p += zipRawEntryLength(p);
    // 下一个条目是末尾
    if (p[0] == ZIP_END) {
        return NULL;
    }
    // 校验下一个条目指针的合法性（是否超出压缩表的整个范围）
    zipAssertValidEntry(zl, zlbytes, p);
    return p;
}

/* Return pointer to previous entry in ziplist. */
// 返回压缩表中当前条目前一个条目的指针
unsigned char *ziplistPrev(unsigned char *zl, unsigned char *p) {
    unsigned int prevlensize, prevlen = 0;

    /* Iterating backwards from ZIP_END should return the tail. When "p" is
     * equal to the first element of the list, we're already at the head,
     * and should return NULL. */
    // 如果当前指针是末尾
    if (p[0] == ZIP_END) {
        // 直接用压缩表头的信息得到最好一个条目
        p = ZIPLIST_ENTRY_TAIL(zl);
        // 如果是空表，返回NULL，否则返回最后条目的指针
        return (p[0] == ZIP_END) ? NULL : p;
    } 
    // 如果p的最开始条目的指针，压根就没有前一个条目，直接返回NULL
    else if (p == ZIPLIST_ENTRY_HEAD(zl)) {
        return NULL;
    } else {
        // 解码当前条目中保存的上一个条目的长度
        ZIP_DECODE_PREVLEN(p, prevlensize, prevlen);
        assert(prevlen > 0);
        // 移动到上一个条目的开始处
        p-=prevlen;
        // 校验上一个条目的指针合法性
        size_t zlbytes = intrev32ifbe(ZIPLIST_BYTES(zl));
        zipAssertValidEntry(zl, zlbytes, p);
        return p;
    }
}

/* Get entry pointed to by 'p' and store in either '*sstr' or 'sval' depending
 * on the encoding of the entry. '*sstr' is always set to NULL to be able
 * to find out whether the string pointer or the integer value was set.
 * Return 0 if 'p' points to the end of the ziplist, 1 otherwise. */
// 获取p指向的条目，并根据条目的编码将内容存储在*sstr或sval中。 当sstr设置为NULL用于查明是否设置了字符串指针或整数值
// 如果p指向ziplist的末尾则返回0，否则返回1。
unsigned int ziplistGet(unsigned char *p, unsigned char **sstr, unsigned int *slen, long long *sval) {
    zlentry entry;
    // 如果p为空或者指向末尾，直接返回0
    if (p == NULL || p[0] == ZIP_END) return 0;
    // 如果sstr不为空，给它一个初值
    if (sstr) *sstr = NULL;
    // 解析当前的条目
    zipEntry(p, &entry); /* no need for "safe" variant since the input pointer was validated by the function that returned it. */
    // 如果编码是字符串，返回字符串的首地址和长度
    if (ZIP_IS_STR(entry.encoding)) {
        if (sstr) {
            *slen = entry.len;
            *sstr = p+entry.headersize;
        }
    } 
    // 如果编码是整数，返回整数的值
    else {
        if (sval) {
            *sval = zipLoadInteger(p+entry.headersize,entry.encoding);
        }
    }
    return 1;
}

/* Insert an entry at "p". */
// 在p处插入一个条目
unsigned char *ziplistInsert(unsigned char *zl, unsigned char *p, unsigned char *s, unsigned int slen) {
    return __ziplistInsert(zl,p,s,slen);
}

/* Delete a single entry from the ziplist, pointed to by *p.
 * Also update *p in place, to be able to iterate over the
 * ziplist, while deleting entries. */
// 从压缩表中删除一个p指向的简单条目。
// 同时更新*p，以便能够遍历ziplist，同时删除条目
unsigned char *ziplistDelete(unsigned char *zl, unsigned char **p) {
    // 保存偏移
    size_t offset = *p-zl;
    // 删除条目
    zl = __ziplistDelete(zl,*p,1);

    /* Store pointer to current element in p, because ziplistDelete will
     * do a realloc which might result in a different "zl"-pointer.
     * When the delete direction is back to front, we might delete the last
     * entry and end up with "p" pointing to ZIP_END, so check this. */
    // 得到原偏移对应的新指针
    *p = zl+offset;
    return zl;
}

/* Delete a range of entries from the ziplist. */
// 从压缩表中删除一个范围的条目
unsigned char *ziplistDeleteRange(unsigned char *zl, int index, unsigned int num) {
    // 得到索引开始的指针
    unsigned char *p = ziplistIndex(zl,index);
    return (p == NULL) ? zl : __ziplistDelete(zl,p,num);
}

/* Replaces the entry at p. This is equivalent to a delete and an insert,
 * but avoids some overhead when replacing a value of the same size. */
// 替代p处的条目，这相当于删除然后插入，但在替换相同大小的值时避免了一些开销
unsigned char *ziplistReplace(unsigned char *zl, unsigned char *p, unsigned char *s, unsigned int slen) {

    /* get metadata of the current entry */
    // 解码当前条目
    zlentry entry;
    zipEntry(p, &entry);

    /* compute length of entry to store, excluding prevlen */
    unsigned int reqlen;
    unsigned char encoding = 0;
    long long value = 123456789; /* initialized to avoid warning. */
    // 尝试将s指向的slen长度的数据编码为整数
    if (zipTryEncoding(s,slen,&value,&encoding)) {
        reqlen = zipIntSize(encoding); /* encoding is set */
    } else {
        reqlen = slen; /* encoding == 0 */
    }
    // 得到编码/长度需要的字节数
    reqlen += zipStoreEntryEncoding(NULL,encoding,slen);
    // 如果替换的条目和原条目的长度一致
    if (reqlen == entry.lensize + entry.len) {
        /* Simply overwrite the element. */
        // 简单的覆盖条目
        p += entry.prevrawlensize;
        p += zipStoreEntryEncoding(p,encoding,slen);
        if (ZIP_IS_STR(encoding)) {
            memcpy(p,s,slen);
        } else {
            zipSaveInteger(p,value,encoding);
        }
    } 
    // 长度不一致，就删除条目，重新插入
    else {
        /* Fallback. */
        zl = ziplistDelete(zl,&p);
        zl = ziplistInsert(zl,p,s,slen);
    }
    return zl;
}

/* Compare entry pointer to by 'p' with 'sstr' of length 'slen'. */
/* Return 1 if equal. */
// 比较p指向的条目和长度为slen的字符串sstr
// 如果相等返回1
unsigned int ziplistCompare(unsigned char *p, unsigned char *sstr, unsigned int slen) {
    zlentry entry;
    unsigned char sencoding;
    long long zval, sval;
    if (p[0] == ZIP_END) return 0;
    // 解码当前条目
    zipEntry(p, &entry); /* no need for "safe" variant since the input pointer was validated by the function that returned it. */
    // 字符串
    if (ZIP_IS_STR(entry.encoding)) {
        /* Raw compare */
        // 长度相等的情况下比较内存
        if (entry.len == slen) {
            return memcmp(p+entry.headersize,sstr,slen) == 0;
        } else {
            return 0;
        }
    } else {
        /* Try to compare encoded values. Don't compare encoding because
         * different implementations may encoded integers differently. */
        // 尝试将传入的数据进行整数编码
        if (zipTryEncoding(sstr,slen,&sval,&sencoding)) {
          // 得到整数值，然后比较
          zval = zipLoadInteger(p+entry.headersize,entry.encoding);
          return zval == sval;
        }
    }
    return 0;
}

/* Find pointer to the entry equal to the specified entry. Skip 'skip' entries
 * between every comparison. Returns NULL when the field could not be found. */
// 找到等于指定条目的指针.skip表示每次比较之间跳过多少个条目。
// 如果没用找到就返回NULL
unsigned char *ziplistFind(unsigned char *zl, unsigned char *p, unsigned char *vstr, unsigned int vlen, unsigned int skip) {
    int skipcnt = 0;
    unsigned char vencoding = 0;
    long long vll = 0;
    // 得到压缩表的总字节数
    size_t zlbytes = ziplistBlobLen(zl);
    // 指针p还没有指向末尾
    while (p[0] != ZIP_END) {
        struct zlentry e;
        unsigned char *q;
        // 解析条目
        assert(zipEntrySafe(zl, zlbytes, p, &e, 1));
        q = p + e.prevrawlensize + e.lensize;
        // 如果跳过条目数为0，进行比较
        if (skipcnt == 0) {
            /* Compare current entry with specified entry */
            // 比较当前的条目和指定条目
            // 字符串编码
            if (ZIP_IS_STR(e.encoding)) {
                // 比较长度和数据
                if (e.len == vlen && memcmp(q, vstr, vlen) == 0) {
                    return p;
                }
            } else {
                /* Find out if the searched field can be encoded. Note that
                 * we do it only the first time, once done vencoding is set
                 * to non-zero and vll is set to the integer value. */
                // 如果从没解析过，解析指定的条目
                if (vencoding == 0) {
                    if (!zipTryEncoding(vstr, vlen, &vll, &vencoding)) {
                        /* If the entry can't be encoded we set it to
                         * UCHAR_MAX so that we don't retry again the next
                         * time. */
                        // 如果解析不成功，就设置成一个非法值
                        vencoding = UCHAR_MAX;
                    }
                    /* Must be non-zero by now */
                    assert(vencoding);
                }

                /* Compare current entry with specified entry, do it only
                 * if vencoding != UCHAR_MAX because if there is no encoding
                 * possible for the field it can't be a valid integer. */
                // 如果是合法的整数编码，解析成整数进行比较
                if (vencoding != UCHAR_MAX) {
                    long long ll = zipLoadInteger(q, e.encoding);
                    if (ll == vll) {
                        return p;
                    }
                }
            }
            // 重置跳过条目计数
            /* Reset skip count */
            skipcnt = skip;
        } 
        // 如果跳过条目数计数不为0，减少计数
        else {
            /* Skip entry */
            skipcnt--;
        }

        /* Move to next entry */
        // 移动到下一个条目进行处理
        p = q + e.len;
    }

    return NULL;
}

/* Return length of ziplist. */
// 返回压缩表的条目数
unsigned int ziplistLen(unsigned char *zl) {
    unsigned int len = 0;
    // 如果条目数小于0xFFFF，那就是记录的正确的数据
    if (intrev16ifbe(ZIPLIST_LENGTH(zl)) < UINT16_MAX) {
        len = intrev16ifbe(ZIPLIST_LENGTH(zl));
    } 
    // 大于等于0xFFFF的条目，记录一直都是0xFFFF，需要遍历整个压缩表计算
    else {
        unsigned char *p = zl+ZIPLIST_HEADER_SIZE;
        size_t zlbytes = intrev32ifbe(ZIPLIST_BYTES(zl));
        // 遍历整个压缩表，数一数条目的数目
        while (*p != ZIP_END) {
            p += zipRawEntryLengthSafe(zl, zlbytes, p);
            len++;
        }

        /* Re-store length if small enough */
        // 如果条目的数目小于0xFFFF，还需要回存
        if (len < UINT16_MAX) ZIPLIST_LENGTH(zl) = intrev16ifbe(len);
    }
    return len;
}

/* Return ziplist blob size in bytes. */
// 返回压缩表的总字节数
size_t ziplistBlobLen(unsigned char *zl) {
    return intrev32ifbe(ZIPLIST_BYTES(zl));
}

// 将一个压缩表格式化成一个可读的方式
void ziplistRepr(unsigned char *zl) {
    unsigned char *p;
    int index = 0;
    zlentry entry;
    size_t zlbytes = ziplistBlobLen(zl);

    printf(
        "{total bytes %u} "
        "{num entries %u}\n"
        "{tail offset %u}\n",
        intrev32ifbe(ZIPLIST_BYTES(zl)),
        intrev16ifbe(ZIPLIST_LENGTH(zl)),
        intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl)));
    p = ZIPLIST_ENTRY_HEAD(zl);
    while(*p != ZIP_END) {
        assert(zipEntrySafe(zl, zlbytes, p, &entry, 1));
        printf(
            "{\n"
                "\taddr 0x%08lx,\n"
                "\tindex %2d,\n"
                "\toffset %5lu,\n"
                "\thdr+entry len: %5u,\n"
                "\thdr len%2u,\n"
                "\tprevrawlen: %5u,\n"
                "\tprevrawlensize: %2u,\n"
                "\tpayload %5u\n",
            (long unsigned)p,
            index,
            (unsigned long) (p-zl),
            entry.headersize+entry.len,
            entry.headersize,
            entry.prevrawlen,
            entry.prevrawlensize,
            entry.len);
        printf("\tbytes: ");
        for (unsigned int i = 0; i < entry.headersize+entry.len; i++) {
            printf("%02x|",p[i]);
        }
        printf("\n");
        p += entry.headersize;
        if (ZIP_IS_STR(entry.encoding)) {
            printf("\t[str]");
            if (entry.len > 40) {
                if (fwrite(p,40,1,stdout) == 0) perror("fwrite");
                printf("...");
            } else {
                if (entry.len &&
                    fwrite(p,entry.len,1,stdout) == 0) perror("fwrite");
            }
        } else {
            printf("\t[int]%lld", (long long) zipLoadInteger(p,entry.encoding));
        }
        printf("\n}\n");
        p += entry.len;
        index++;
    }
    printf("{end}\n\n");
}

/* Validate the integrity of the data structure.
 * when `deep` is 0, only the integrity of the header is validated.
 * when `deep` is 1, we scan all the entries one by one. */
// 验证数据结构的完整性。 当deep为0时，仅验证标头的完整性。当deep为1时，我们逐一扫描所有条目。
int ziplistValidateIntegrity(unsigned char *zl, size_t size, int deep,
    ziplistValidateEntryCB entry_cb, void *cb_userdata) {
    /* check that we can actually read the header. (and ZIP_END) */
    // 确定我们是否可以读取头，不能比一个空压缩表的长度还小
    if (size < ZIPLIST_HEADER_SIZE + ZIPLIST_END_SIZE)
        return 0;

    /* check that the encoded size in the header must match the allocated size. */
    // 得到压缩表的字节数，检验是否匹配
    size_t bytes = intrev32ifbe(ZIPLIST_BYTES(zl));
    if (bytes != size)
        return 0;

    /* the last byte must be the terminator. */
    // 最后一个字节是否是末尾字节
    if (zl[size - ZIPLIST_END_SIZE] != ZIP_END)
        return 0;

    /* make sure the tail offset isn't reaching outside the allocation. */
    // 确定最后一个条目是否超出的压缩表的范围
    if (intrev32ifbe(ZIPLIST_TAIL_OFFSET(zl)) > size - ZIPLIST_END_SIZE)
        return 0;
    // 如果不是深度校验，到此就返回了
    if (!deep)
        return 1;

    unsigned int count = 0;
    unsigned int header_count = intrev16ifbe(ZIPLIST_LENGTH(zl));
    unsigned char *p = ZIPLIST_ENTRY_HEAD(zl);
    unsigned char *prev = NULL;
    size_t prev_raw_size = 0;
    // 循环遍历压缩表所有的条目，进行校验
    while(*p != ZIP_END) {
        struct zlentry e;
        /* Decode the entry headers and fail if invalid or reaches outside the allocation */
        // 条目解析校验和是否会超出压缩表的范围
        if (!zipEntrySafe(zl, size, p, &e, 1))
            return 0;

        /* Make sure the record stating the prev entry size is correct. */
        // 当前保存的前一个条目的字节数和前一个条目真实的字节数是否匹配
        if (e.prevrawlen != prev_raw_size)
            return 0;

        /* Optionally let the caller validate the entry too. */
        // 选择性地让调用者也验证条目。
        if (entry_cb && !entry_cb(p, header_count, cb_userdata))
            return 0;

        /* Move to the next entry */
        // 移动到下一条目
        prev_raw_size = e.headersize + e.len;
        prev = p;
        p += e.headersize + e.len;
        count++;
    }

    /* Make sure 'p' really does point to the end of the ziplist. */
    // 确定p是否真的到了压缩表的最后一个字节
    if (p != zl + bytes - ZIPLIST_END_SIZE)
        return 0;

    /* Make sure the <zltail> entry really do point to the start of the last entry. */
    // 确定当前的前一个条目是否是压缩表的最后一个条目
    if (prev != NULL && prev != ZIPLIST_ENTRY_TAIL(zl))
        return 0;

    /* Check that the count in the header is correct */
    // 如果条目数没用超过UINT16_MAX，数值是否对的上
    if (header_count != UINT16_MAX && count != header_count)
        return 0;

    return 1;
}

/* Randomly select a pair of key and value.
 * total_count is a pre-computed length/2 of the ziplist (to avoid calls to ziplistLen)
 * 'key' and 'val' are used to store the result key value pair.
 * 'val' can be NULL if the value is not needed. */
// 随机选择一对键和值。 total_count是ziplist的长度/2（避免调用ziplistlen）
// 键和val来存储结果键值对。 如果不需要值，则将val设置为null
// 压缩表中保存的是多对key-value的数据，key-value-key-value-key-value的数据格式
void ziplistRandomPair(unsigned char *zl, unsigned long total_count, ziplistEntry *key, ziplistEntry *val) {
    int ret;
    unsigned char *p;

    /* Avoid div by zero on corrupt ziplist */
    assert(total_count);

    /* Generate even numbers, because ziplist saved K-V pair */
    // 产生一个偶数的随机索引，因为压缩表保存键-值对
    int r = (rand() % total_count) * 2;
    p = ziplistIndex(zl, r);
    // 得到指定索引的条目中的值
    ret = ziplistGet(p, &key->sval, &key->slen, &key->lval);
    assert(ret != 0);

    if (!val)
        return;
    // 得到下一个条目，就是其value
    p = ziplistNext(zl, p);
    // 得到value条目中的值
    ret = ziplistGet(p, &val->sval, &val->slen, &val->lval);
    assert(ret != 0);
}

/* int compare for qsort */
// 用于快排的整数比较..........................
int uintCompare(const void *a, const void *b) {
    return (*(unsigned int *) a - *(unsigned int *) b);
}

/* Helper method to store a string into from val or lval into dest */
// 保存一个条目的值
static inline void ziplistSaveValue(unsigned char *val, unsigned int len, long long lval, ziplistEntry *dest) {
    dest->sval = val;
    dest->slen = len;
    dest->lval = lval;
}

/* Randomly select count of key value pairs and store into 'keys' and
 * 'vals' args. The order of the picked entries is random, and the selections
 * are non-unique (repetitions are possible).
 * The 'vals' arg can be NULL in which case we skip these. */
// 随机选择钥匙值对的计数，然后将其存储到keys和vals的参数中。 
// 挑选条目的顺序是随机的，选择是非唯一的（可能会出现重复）。
// 如果vals参数为NULL，我们会跳过这些。
void ziplistRandomPairs(unsigned char *zl, unsigned int count, ziplistEntry *keys, ziplistEntry *vals) {
    unsigned char *p, *key, *value;
    unsigned int klen = 0, vlen = 0;
    long long klval = 0, vlval = 0;

    /* Notice: the index member must be first due to the use in uintCompare */
    typedef struct {
        unsigned int index;
        unsigned int order;
    } rand_pick;
    rand_pick *picks = zmalloc(sizeof(rand_pick)*count);
    // 得到键值对的对数
    unsigned int total_size = ziplistLen(zl)/2;

    /* Avoid div by zero on corrupt ziplist */
    assert(total_size);

    /* create a pool of random indexes (some may be duplicate). */
    // 创建一个池用于保存随机的索引（可能会有重复）
    for (unsigned int i = 0; i < count; i++) {
        // 保存随机索引
        picks[i].index = (rand() % total_size) * 2; /* Generate even indexes */
        /* keep track of the order we picked them */
        // 保存顺序
        picks[i].order = i;
    }

    /* sort by indexes. */
    // 根据索引进行快速排序
    qsort(picks, count, sizeof(rand_pick), uintCompare);

    /* fetch the elements form the ziplist into a output array respecting the original order. */
    // 根据压缩表的索引顺序将条目取出放入输出的队列中
    unsigned int zipindex = picks[0].index, pickindex = 0;
    // 得到最前面的随机索引的指针
    p = ziplistIndex(zl, zipindex);
    // 根据选择列表，先取出key的条目
    while (ziplistGet(p, &key, &klen, &klval) && pickindex < count) {
        // 取出值的条目
        p = ziplistNext(zl, p);
        assert(ziplistGet(p, &value, &vlen, &vlval));
        // 选择索引相等，就放入输出队列中
        while (pickindex < count && zipindex == picks[pickindex].index) {
            // 得到排序之前的顺序
            int storeorder = picks[pickindex].order;
            // 保存键和值的数据到输出列表中
            ziplistSaveValue(key, klen, klval, &keys[storeorder]);
            if (vals)
                ziplistSaveValue(value, vlen, vlval, &vals[storeorder]);
             pickindex++;
        }
        // 每次增加2的索引（键一个索引，值一个索引）
        zipindex += 2;
        p = ziplistNext(zl, p);
    }

    zfree(picks);
}

/* Randomly select count of key value pairs and store into 'keys' and
 * 'vals' args. The selections are unique (no repetitions), and the order of
 * the picked entries is NOT-random.
 * The 'vals' arg can be NULL in which case we skip these.
 * The return value is the number of items picked which can be lower than the
 * requested count if the ziplist doesn't hold enough pairs. */
// 随机选择钥匙值对的计数，然后将其存储到keys和vals的参数中。 
// 挑选条目的顺序是随机的，选择是唯一的（不能重复）。并且所选条目的顺序不是随机的
// 如果vals参数为NULL，我们会跳过这些。
// 返回值是选择的条目数，如果压缩表中没有足够的键值对，那么它可能比要求的数目要少，
unsigned int ziplistRandomPairsUnique(unsigned char *zl, unsigned int count, ziplistEntry *keys, ziplistEntry *vals) {
    unsigned char *p, *key;
    unsigned int klen = 0;
    long long klval = 0;
    // 得到键值对的数目
    unsigned int total_size = ziplistLen(zl)/2;
    unsigned int index = 0;
    if (count > total_size)
        count = total_size;

    /* To only iterate once, every time we try to pick a member, the probability
     * we pick it is the quotient of the count left we want to pick and the
     * count still we haven't visited in the dict, this way, we could make every
     * member be equally picked.*/
    // 从第0个索引开始
    p = ziplistIndex(zl, 0);
    unsigned int picked = 0, remaining = count;
    while (picked < count && p) {
        // 得到随机数
        double randomDouble = ((double)rand()) / RAND_MAX;
        // 计算阈值
        double threshold = ((double)remaining) / (total_size - index);
        // 小于阈值就算选中
        if (randomDouble <= threshold) {
            // 得到键的数据
            assert(ziplistGet(p, &key, &klen, &klval));
            ziplistSaveValue(key, klen, klval, &keys[picked]);
            // 跳过键的条目
            p = ziplistNext(zl, p);
            assert(p);
            // 保存值的数据
            if (vals) {
                assert(ziplistGet(p, &key, &klen, &klval));
                ziplistSaveValue(key, klen, klval, &vals[picked]);
            }
            remaining--;
            picked++;
        } else {
            // 如果没有选中，跳过键的条目
            p = ziplistNext(zl, p);
            assert(p);
        }
        // 跳过值的条目
        p = ziplistNext(zl, p);
        index++;
    }
    return picked;
}

#ifdef REDIS_TEST
#include <sys/time.h>
#include "adlist.h"
#include "sds.h"
#include "testhelp.h"

#define debug(f, ...) { if (DEBUG) printf(f, __VA_ARGS__); }

static unsigned char *createList() {
    unsigned char *zl = ziplistNew();
    zl = ziplistPush(zl, (unsigned char*)"foo", 3, ZIPLIST_TAIL);
    zl = ziplistPush(zl, (unsigned char*)"quux", 4, ZIPLIST_TAIL);
    zl = ziplistPush(zl, (unsigned char*)"hello", 5, ZIPLIST_HEAD);
    zl = ziplistPush(zl, (unsigned char*)"1024", 4, ZIPLIST_TAIL);
    return zl;
}

static unsigned char *createIntList() {
    unsigned char *zl = ziplistNew();
    char buf[32];

    snprintf(buf, sizeof(buf), "100");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    snprintf(buf, sizeof(buf), "128000");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    snprintf(buf, sizeof(buf), "-100");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_HEAD);
    snprintf(buf, sizeof(buf), "4294967296");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_HEAD);
    snprintf(buf, sizeof(buf), "non integer");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    snprintf(buf,sizeof(buf), "much much longer non integer");
    zl = ziplistPush(zl, (unsigned char*)buf, strlen(buf), ZIPLIST_TAIL);
    return zl;
}

static long long usec(void) {
    struct timeval tv;
    gettimeofday(&tv,NULL);
    return (((long long)tv.tv_sec)*1000000)+tv.tv_usec;
}

static void stress(int pos, int num, int maxsize, int dnum) {
    int i,j,k;
    unsigned char *zl;
    char posstr[2][5] = { "HEAD", "TAIL" };
    long long start;
    for (i = 0; i < maxsize; i+=dnum) {
        zl = ziplistNew();
        for (j = 0; j < i; j++) {
            zl = ziplistPush(zl,(unsigned char*)"quux",4,ZIPLIST_TAIL);
        }

        /* Do num times a push+pop from pos */
        start = usec();
        for (k = 0; k < num; k++) {
            zl = ziplistPush(zl,(unsigned char*)"quux",4,pos);
            zl = ziplistDeleteRange(zl,0,1);
        }
        printf("List size: %8d, bytes: %8d, %dx push+pop (%s): %6lld usec\n",
            i,intrev32ifbe(ZIPLIST_BYTES(zl)),num,posstr[pos],usec()-start);
        zfree(zl);
    }
}

static unsigned char *pop(unsigned char *zl, int where) {
    unsigned char *p, *vstr;
    unsigned int vlen;
    long long vlong = 0;

    p = ziplistIndex(zl,where == ZIPLIST_HEAD ? 0 : -1);
    if (ziplistGet(p,&vstr,&vlen,&vlong)) {
        if (where == ZIPLIST_HEAD)
            printf("Pop head: ");
        else
            printf("Pop tail: ");

        if (vstr) {
            if (vlen && fwrite(vstr,vlen,1,stdout) == 0) perror("fwrite");
        }
        else {
            printf("%lld", vlong);
        }

        printf("\n");
        return ziplistDelete(zl,&p);
    } else {
        printf("ERROR: Could not pop\n");
        exit(1);
    }
}

static int randstring(char *target, unsigned int min, unsigned int max) {
    int p = 0;
    int len = min+rand()%(max-min+1);
    int minval, maxval;
    switch(rand() % 3) {
    case 0:
        minval = 0;
        maxval = 255;
    break;
    case 1:
        minval = 48;
        maxval = 122;
    break;
    case 2:
        minval = 48;
        maxval = 52;
    break;
    default:
        assert(NULL);
    }

    while(p < len)
        target[p++] = minval+rand()%(maxval-minval+1);
    return len;
}

static void verify(unsigned char *zl, zlentry *e) {
    int len = ziplistLen(zl);
    zlentry _e;

    ZIPLIST_ENTRY_ZERO(&_e);

    for (int i = 0; i < len; i++) {
        memset(&e[i], 0, sizeof(zlentry));
        zipEntry(ziplistIndex(zl, i), &e[i]);

        memset(&_e, 0, sizeof(zlentry));
        zipEntry(ziplistIndex(zl, -len+i), &_e);

        assert(memcmp(&e[i], &_e, sizeof(zlentry)) == 0);
    }
}

static unsigned char *insertHelper(unsigned char *zl, char ch, size_t len, unsigned char *pos) {
    assert(len <= ZIP_BIG_PREVLEN);
    unsigned char data[ZIP_BIG_PREVLEN] = {0};
    memset(data, ch, len);
    return ziplistInsert(zl, pos, data, len);
}

static int compareHelper(unsigned char *zl, char ch, size_t len, int index) {
    assert(len <= ZIP_BIG_PREVLEN);
    unsigned char data[ZIP_BIG_PREVLEN] = {0};
    memset(data, ch, len);
    unsigned char *p = ziplistIndex(zl, index);
    assert(p != NULL);
    return ziplistCompare(p, data, len);
}

static size_t strEntryBytesSmall(size_t slen) {
    return slen + zipStorePrevEntryLength(NULL, 0) + zipStoreEntryEncoding(NULL, 0, slen);
}

static size_t strEntryBytesLarge(size_t slen) {
    return slen + zipStorePrevEntryLength(NULL, ZIP_BIG_PREVLEN) + zipStoreEntryEncoding(NULL, 0, slen);
}

/* ./redis-server test ziplist <randomseed> */
int ziplistTest(int argc, char **argv, int flags) {
    int accurate = (flags & REDIS_TEST_ACCURATE);
    unsigned char *zl, *p;
    unsigned char *entry;
    unsigned int elen;
    long long value;
    int iteration;

    /* If an argument is given, use it as the random seed. */
    if (argc >= 4)
        srand(atoi(argv[3]));

    zl = createIntList();
    ziplistRepr(zl);

    zfree(zl);

    zl = createList();
    ziplistRepr(zl);

    zl = pop(zl,ZIPLIST_TAIL);
    ziplistRepr(zl);

    zl = pop(zl,ZIPLIST_HEAD);
    ziplistRepr(zl);

    zl = pop(zl,ZIPLIST_TAIL);
    ziplistRepr(zl);

    zl = pop(zl,ZIPLIST_TAIL);
    ziplistRepr(zl);

    zfree(zl);

    printf("Get element at index 3:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 3);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("ERROR: Could not access index 3\n");
            return 1;
        }
        if (entry) {
            if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            printf("\n");
        } else {
            printf("%lld\n", value);
        }
        printf("\n");
        zfree(zl);
    }

    printf("Get element at index 4 (out of range):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 4);
        if (p == NULL) {
            printf("No entry\n");
        } else {
            printf("ERROR: Out of range index should return NULL, returned offset: %ld\n", (long)(p-zl));
            return 1;
        }
        printf("\n");
        zfree(zl);
    }

    printf("Get element at index -1 (last element):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -1);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("ERROR: Could not access index -1\n");
            return 1;
        }
        if (entry) {
            if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            printf("\n");
        } else {
            printf("%lld\n", value);
        }
        printf("\n");
        zfree(zl);
    }

    printf("Get element at index -4 (first element):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -4);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("ERROR: Could not access index -4\n");
            return 1;
        }
        if (entry) {
            if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            printf("\n");
        } else {
            printf("%lld\n", value);
        }
        printf("\n");
        zfree(zl);
    }

    printf("Get element at index -5 (reverse out of range):\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -5);
        if (p == NULL) {
            printf("No entry\n");
        } else {
            printf("ERROR: Out of range index should return NULL, returned offset: %ld\n", (long)(p-zl));
            return 1;
        }
        printf("\n");
        zfree(zl);
    }

    printf("Iterate list from 0 to end:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 0);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistNext(zl,p);
            printf("\n");
        }
        printf("\n");
        zfree(zl);
    }

    printf("Iterate list from 1 to end:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 1);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistNext(zl,p);
            printf("\n");
        }
        printf("\n");
        zfree(zl);
    }

    printf("Iterate list from 2 to end:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 2);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistNext(zl,p);
            printf("\n");
        }
        printf("\n");
        zfree(zl);
    }

    printf("Iterate starting out of range:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, 4);
        if (!ziplistGet(p, &entry, &elen, &value)) {
            printf("No entry\n");
        } else {
            printf("ERROR\n");
        }
        printf("\n");
        zfree(zl);
    }

    printf("Iterate from back to front:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -1);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            p = ziplistPrev(zl,p);
            printf("\n");
        }
        printf("\n");
        zfree(zl);
    }

    printf("Iterate from back to front, deleting all items:\n");
    {
        zl = createList();
        p = ziplistIndex(zl, -1);
        while (ziplistGet(p, &entry, &elen, &value)) {
            printf("Entry: ");
            if (entry) {
                if (elen && fwrite(entry,elen,1,stdout) == 0) perror("fwrite");
            } else {
                printf("%lld", value);
            }
            zl = ziplistDelete(zl,&p);
            p = ziplistPrev(zl,p);
            printf("\n");
        }
        printf("\n");
        zfree(zl);
    }

    printf("Delete inclusive range 0,0:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 0, 1);
        ziplistRepr(zl);
        zfree(zl);
    }

    printf("Delete inclusive range 0,1:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 0, 2);
        ziplistRepr(zl);
        zfree(zl);
    }

    printf("Delete inclusive range 1,2:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 1, 2);
        ziplistRepr(zl);
        zfree(zl);
    }

    printf("Delete with start index out of range:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 5, 1);
        ziplistRepr(zl);
        zfree(zl);
    }

    printf("Delete with num overflow:\n");
    {
        zl = createList();
        zl = ziplistDeleteRange(zl, 1, 5);
        ziplistRepr(zl);
        zfree(zl);
    }

    printf("Delete foo while iterating:\n");
    {
        zl = createList();
        p = ziplistIndex(zl,0);
        while (ziplistGet(p,&entry,&elen,&value)) {
            if (entry && strncmp("foo",(char*)entry,elen) == 0) {
                printf("Delete foo\n");
                zl = ziplistDelete(zl,&p);
            } else {
                printf("Entry: ");
                if (entry) {
                    if (elen && fwrite(entry,elen,1,stdout) == 0)
                        perror("fwrite");
                } else {
                    printf("%lld",value);
                }
                p = ziplistNext(zl,p);
                printf("\n");
            }
        }
        printf("\n");
        ziplistRepr(zl);
        zfree(zl);
    }

    printf("Replace with same size:\n");
    {
        zl = createList(); /* "hello", "foo", "quux", "1024" */
        unsigned char *orig_zl = zl;
        p = ziplistIndex(zl, 0);
        zl = ziplistReplace(zl, p, (unsigned char*)"zoink", 5);
        p = ziplistIndex(zl, 3);
        zl = ziplistReplace(zl, p, (unsigned char*)"yy", 2);
        p = ziplistIndex(zl, 1);
        zl = ziplistReplace(zl, p, (unsigned char*)"65536", 5);
        p = ziplistIndex(zl, 0);
        assert(!memcmp((char*)p,
                       "\x00\x05zoink"
                       "\x07\xf0\x00\x00\x01" /* 65536 as int24 */
                       "\x05\x04quux" "\x06\x02yy" "\xff",
                       23));
        assert(zl == orig_zl); /* no reallocations have happened */
        zfree(zl);
        printf("SUCCESS\n\n");
    }

    printf("Replace with different size:\n");
    {
        zl = createList(); /* "hello", "foo", "quux", "1024" */
        p = ziplistIndex(zl, 1);
        zl = ziplistReplace(zl, p, (unsigned char*)"squirrel", 8);
        p = ziplistIndex(zl, 0);
        assert(!strncmp((char*)p,
                        "\x00\x05hello" "\x07\x08squirrel" "\x0a\x04quux"
                        "\x06\xc0\x00\x04" "\xff",
                        28));
        zfree(zl);
        printf("SUCCESS\n\n");
    }

    printf("Regression test for >255 byte strings:\n");
    {
        char v1[257] = {0}, v2[257] = {0};
        memset(v1,'x',256);
        memset(v2,'y',256);
        zl = ziplistNew();
        zl = ziplistPush(zl,(unsigned char*)v1,strlen(v1),ZIPLIST_TAIL);
        zl = ziplistPush(zl,(unsigned char*)v2,strlen(v2),ZIPLIST_TAIL);

        /* Pop values again and compare their value. */
        p = ziplistIndex(zl,0);
        assert(ziplistGet(p,&entry,&elen,&value));
        assert(strncmp(v1,(char*)entry,elen) == 0);
        p = ziplistIndex(zl,1);
        assert(ziplistGet(p,&entry,&elen,&value));
        assert(strncmp(v2,(char*)entry,elen) == 0);
        printf("SUCCESS\n\n");
        zfree(zl);
    }

    printf("Regression test deleting next to last entries:\n");
    {
        char v[3][257] = {{0}};
        zlentry e[3] = {{.prevrawlensize = 0, .prevrawlen = 0, .lensize = 0,
                         .len = 0, .headersize = 0, .encoding = 0, .p = NULL}};
        size_t i;

        for (i = 0; i < (sizeof(v)/sizeof(v[0])); i++) {
            memset(v[i], 'a' + i, sizeof(v[0]));
        }

        v[0][256] = '\0';
        v[1][  1] = '\0';
        v[2][256] = '\0';

        zl = ziplistNew();
        for (i = 0; i < (sizeof(v)/sizeof(v[0])); i++) {
            zl = ziplistPush(zl, (unsigned char *) v[i], strlen(v[i]), ZIPLIST_TAIL);
        }

        verify(zl, e);

        assert(e[0].prevrawlensize == 1);
        assert(e[1].prevrawlensize == 5);
        assert(e[2].prevrawlensize == 1);

        /* Deleting entry 1 will increase `prevrawlensize` for entry 2 */
        unsigned char *p = e[1].p;
        zl = ziplistDelete(zl, &p);

        verify(zl, e);

        assert(e[0].prevrawlensize == 1);
        assert(e[1].prevrawlensize == 5);

        printf("SUCCESS\n\n");
        zfree(zl);
    }

    printf("Create long list and check indices:\n");
    {
        unsigned long long start = usec();
        zl = ziplistNew();
        char buf[32];
        int i,len;
        for (i = 0; i < 1000; i++) {
            len = snprintf(buf,sizeof(buf),"%d",i);
            zl = ziplistPush(zl,(unsigned char*)buf,len,ZIPLIST_TAIL);
        }
        for (i = 0; i < 1000; i++) {
            p = ziplistIndex(zl,i);
            assert(ziplistGet(p,NULL,NULL,&value));
            assert(i == value);

            p = ziplistIndex(zl,-i-1);
            assert(ziplistGet(p,NULL,NULL,&value));
            assert(999-i == value);
        }
        printf("SUCCESS. usec=%lld\n\n", usec()-start);
        zfree(zl);
    }

    printf("Compare strings with ziplist entries:\n");
    {
        zl = createList();
        p = ziplistIndex(zl,0);
        if (!ziplistCompare(p,(unsigned char*)"hello",5)) {
            printf("ERROR: not \"hello\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"hella",5)) {
            printf("ERROR: \"hella\"\n");
            return 1;
        }

        p = ziplistIndex(zl,3);
        if (!ziplistCompare(p,(unsigned char*)"1024",4)) {
            printf("ERROR: not \"1024\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"1025",4)) {
            printf("ERROR: \"1025\"\n");
            return 1;
        }
        printf("SUCCESS\n\n");
        zfree(zl);
    }

    printf("Merge test:\n");
    {
        /* create list gives us: [hello, foo, quux, 1024] */
        zl = createList();
        unsigned char *zl2 = createList();

        unsigned char *zl3 = ziplistNew();
        unsigned char *zl4 = ziplistNew();

        if (ziplistMerge(&zl4, &zl4)) {
            printf("ERROR: Allowed merging of one ziplist into itself.\n");
            return 1;
        }

        /* Merge two empty ziplists, get empty result back. */
        zl4 = ziplistMerge(&zl3, &zl4);
        ziplistRepr(zl4);
        if (ziplistLen(zl4)) {
            printf("ERROR: Merging two empty ziplists created entries.\n");
            return 1;
        }
        zfree(zl4);

        zl2 = ziplistMerge(&zl, &zl2);
        /* merge gives us: [hello, foo, quux, 1024, hello, foo, quux, 1024] */
        ziplistRepr(zl2);

        if (ziplistLen(zl2) != 8) {
            printf("ERROR: Merged length not 8, but: %u\n", ziplistLen(zl2));
            return 1;
        }

        p = ziplistIndex(zl2,0);
        if (!ziplistCompare(p,(unsigned char*)"hello",5)) {
            printf("ERROR: not \"hello\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"hella",5)) {
            printf("ERROR: \"hella\"\n");
            return 1;
        }

        p = ziplistIndex(zl2,3);
        if (!ziplistCompare(p,(unsigned char*)"1024",4)) {
            printf("ERROR: not \"1024\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"1025",4)) {
            printf("ERROR: \"1025\"\n");
            return 1;
        }

        p = ziplistIndex(zl2,4);
        if (!ziplistCompare(p,(unsigned char*)"hello",5)) {
            printf("ERROR: not \"hello\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"hella",5)) {
            printf("ERROR: \"hella\"\n");
            return 1;
        }

        p = ziplistIndex(zl2,7);
        if (!ziplistCompare(p,(unsigned char*)"1024",4)) {
            printf("ERROR: not \"1024\"\n");
            return 1;
        }
        if (ziplistCompare(p,(unsigned char*)"1025",4)) {
            printf("ERROR: \"1025\"\n");
            return 1;
        }
        printf("SUCCESS\n\n");
        zfree(zl);
    }

    printf("Stress with random payloads of different encoding:\n");
    {
        unsigned long long start = usec();
        int i,j,len,where;
        unsigned char *p;
        char buf[1024];
        int buflen;
        list *ref;
        listNode *refnode;

        /* Hold temp vars from ziplist */
        unsigned char *sstr;
        unsigned int slen;
        long long sval;

        iteration = accurate ? 20000 : 20;
        for (i = 0; i < iteration; i++) {
            zl = ziplistNew();
            ref = listCreate();
            listSetFreeMethod(ref,(void (*)(void*))sdsfree);
            len = rand() % 256;

            /* Create lists */
            for (j = 0; j < len; j++) {
                where = (rand() & 1) ? ZIPLIST_HEAD : ZIPLIST_TAIL;
                if (rand() % 2) {
                    buflen = randstring(buf,1,sizeof(buf)-1);
                } else {
                    switch(rand() % 3) {
                    case 0:
                        buflen = snprintf(buf,sizeof(buf),"%lld",(0LL + rand()) >> 20);
                        break;
                    case 1:
                        buflen = snprintf(buf,sizeof(buf),"%lld",(0LL + rand()));
                        break;
                    case 2:
                        buflen = snprintf(buf,sizeof(buf),"%lld",(0LL + rand()) << 20);
                        break;
                    default:
                        assert(NULL);
                    }
                }

                /* Add to ziplist */
                zl = ziplistPush(zl, (unsigned char*)buf, buflen, where);

                /* Add to reference list */
                if (where == ZIPLIST_HEAD) {
                    listAddNodeHead(ref,sdsnewlen(buf, buflen));
                } else if (where == ZIPLIST_TAIL) {
                    listAddNodeTail(ref,sdsnewlen(buf, buflen));
                } else {
                    assert(NULL);
                }
            }

            assert(listLength(ref) == ziplistLen(zl));
            for (j = 0; j < len; j++) {
                /* Naive way to get elements, but similar to the stresser
                 * executed from the Tcl test suite. */
                p = ziplistIndex(zl,j);
                refnode = listIndex(ref,j);

                assert(ziplistGet(p,&sstr,&slen,&sval));
                if (sstr == NULL) {
                    buflen = snprintf(buf,sizeof(buf),"%lld",sval);
                } else {
                    buflen = slen;
                    memcpy(buf,sstr,buflen);
                    buf[buflen] = '\0';
                }
                assert(memcmp(buf,listNodeValue(refnode),buflen) == 0);
            }
            zfree(zl);
            listRelease(ref);
        }
        printf("Done. usec=%lld\n\n", usec()-start);
    }

    printf("Stress with variable ziplist size:\n");
    {
        unsigned long long start = usec();
        int maxsize = accurate ? 16384 : 16;
        stress(ZIPLIST_HEAD,100000,maxsize,256);
        stress(ZIPLIST_TAIL,100000,maxsize,256);
        printf("Done. usec=%lld\n\n", usec()-start);
    }

    /* Benchmarks */
    {
        zl = ziplistNew();
        iteration = accurate ? 100000 : 100;
        for (int i=0; i<iteration; i++) {
            char buf[4096] = "asdf";
            zl = ziplistPush(zl, (unsigned char*)buf, 4, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)buf, 40, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)buf, 400, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)buf, 4000, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)"1", 1, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)"10", 2, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)"100", 3, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)"1000", 4, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)"10000", 5, ZIPLIST_TAIL);
            zl = ziplistPush(zl, (unsigned char*)"100000", 6, ZIPLIST_TAIL);
        }

        printf("Benchmark ziplistFind:\n");
        {
            unsigned long long start = usec();
            for (int i = 0; i < 2000; i++) {
                unsigned char *fptr = ziplistIndex(zl, ZIPLIST_HEAD);
                fptr = ziplistFind(zl, fptr, (unsigned char*)"nothing", 7, 1);
            }
            printf("%lld\n", usec()-start);
        }

        printf("Benchmark ziplistIndex:\n");
        {
            unsigned long long start = usec();
            for (int i = 0; i < 2000; i++) {
                ziplistIndex(zl, 99999);
            }
            printf("%lld\n", usec()-start);
        }

        printf("Benchmark ziplistValidateIntegrity:\n");
        {
            unsigned long long start = usec();
            for (int i = 0; i < 2000; i++) {
                ziplistValidateIntegrity(zl, ziplistBlobLen(zl), 1, NULL, NULL);
            }
            printf("%lld\n", usec()-start);
        }

        printf("Benchmark ziplistCompare with string\n");
        {
            unsigned long long start = usec();
            for (int i = 0; i < 2000; i++) {
                unsigned char *eptr = ziplistIndex(zl,0);
                while (eptr != NULL) {
                    ziplistCompare(eptr,(unsigned char*)"nothing",7);
                    eptr = ziplistNext(zl,eptr);
                }
            }
            printf("Done. usec=%lld\n", usec()-start);
        }

        printf("Benchmark ziplistCompare with number\n");
        {
            unsigned long long start = usec();
            for (int i = 0; i < 2000; i++) {
                unsigned char *eptr = ziplistIndex(zl,0);
                while (eptr != NULL) {
                    ziplistCompare(eptr,(unsigned char*)"99999",5);
                    eptr = ziplistNext(zl,eptr);
                }
            }
            printf("Done. usec=%lld\n", usec()-start);
        }

        zfree(zl);
    }

    printf("Stress __ziplistCascadeUpdate:\n");
    {
        char data[ZIP_BIG_PREVLEN];
        zl = ziplistNew();
        iteration = accurate ? 100000 : 100;
        for (int i = 0; i < iteration; i++) {
            zl = ziplistPush(zl, (unsigned char*)data, ZIP_BIG_PREVLEN-4, ZIPLIST_TAIL);
        }
        unsigned long long start = usec();
        zl = ziplistPush(zl, (unsigned char*)data, ZIP_BIG_PREVLEN-3, ZIPLIST_HEAD);
        printf("Done. usec=%lld\n\n", usec()-start);
        zfree(zl);
    }

    printf("Edge cases of __ziplistCascadeUpdate:\n");
    {
        /* Inserting a entry with data length greater than ZIP_BIG_PREVLEN-4 
         * will leads to cascade update. */
        size_t s1 = ZIP_BIG_PREVLEN-4, s2 = ZIP_BIG_PREVLEN-3;
        zl = ziplistNew();

        zlentry e[4] = {{.prevrawlensize = 0, .prevrawlen = 0, .lensize = 0,
                         .len = 0, .headersize = 0, .encoding = 0, .p = NULL}};

        zl = insertHelper(zl, 'a', s1, ZIPLIST_ENTRY_HEAD(zl));
        verify(zl, e);

        assert(e[0].prevrawlensize == 1 && e[0].prevrawlen == 0);
        assert(compareHelper(zl, 'a', s1, 0));
        ziplistRepr(zl);

        /* No expand. */
        zl = insertHelper(zl, 'b', s1, ZIPLIST_ENTRY_HEAD(zl));
        verify(zl, e);

        assert(e[0].prevrawlensize == 1 && e[0].prevrawlen == 0);
        assert(compareHelper(zl, 'b', s1, 0));

        assert(e[1].prevrawlensize == 1 && e[1].prevrawlen == strEntryBytesSmall(s1));
        assert(compareHelper(zl, 'a', s1, 1));

        ziplistRepr(zl);

        /* Expand(tail included). */
        zl = insertHelper(zl, 'c', s2, ZIPLIST_ENTRY_HEAD(zl));
        verify(zl, e);

        assert(e[0].prevrawlensize == 1 && e[0].prevrawlen == 0);
        assert(compareHelper(zl, 'c', s2, 0));

        assert(e[1].prevrawlensize == 5 && e[1].prevrawlen == strEntryBytesSmall(s2));
        assert(compareHelper(zl, 'b', s1, 1));

        assert(e[2].prevrawlensize == 5 && e[2].prevrawlen == strEntryBytesLarge(s1));
        assert(compareHelper(zl, 'a', s1, 2));

        ziplistRepr(zl);

        /* Expand(only previous head entry). */
        zl = insertHelper(zl, 'd', s2, ZIPLIST_ENTRY_HEAD(zl));
        verify(zl, e);

        assert(e[0].prevrawlensize == 1 && e[0].prevrawlen == 0);
        assert(compareHelper(zl, 'd', s2, 0));

        assert(e[1].prevrawlensize == 5 && e[1].prevrawlen == strEntryBytesSmall(s2));
        assert(compareHelper(zl, 'c', s2, 1));

        assert(e[2].prevrawlensize == 5 && e[2].prevrawlen == strEntryBytesLarge(s2));
        assert(compareHelper(zl, 'b', s1, 2));

        assert(e[3].prevrawlensize == 5 && e[3].prevrawlen == strEntryBytesLarge(s1));
        assert(compareHelper(zl, 'a', s1, 3));

        ziplistRepr(zl);

        /* Delete from mid. */
        unsigned char *p = ziplistIndex(zl, 2);
        zl = ziplistDelete(zl, &p);
        verify(zl, e);

        assert(e[0].prevrawlensize == 1 && e[0].prevrawlen == 0);
        assert(compareHelper(zl, 'd', s2, 0));

        assert(e[1].prevrawlensize == 5 && e[1].prevrawlen == strEntryBytesSmall(s2));
        assert(compareHelper(zl, 'c', s2, 1));

        assert(e[2].prevrawlensize == 5 && e[2].prevrawlen == strEntryBytesLarge(s2));
        assert(compareHelper(zl, 'a', s1, 2));

        ziplistRepr(zl);

        zfree(zl);
    }

    printf("__ziplistInsert nextdiff == -4 && reqlen < 4 (issue #7170):\n");
    {
        zl = ziplistNew();

        /* We set some values to almost reach the critical point - 254 */
        char A_252[253] = {0}, A_250[251] = {0};
        memset(A_252, 'A', 252);
        memset(A_250, 'A', 250);

        /* After the rpush, the list look like: [one two A_252 A_250 three 10] */
        zl = ziplistPush(zl, (unsigned char*)"one", 3, ZIPLIST_TAIL);
        zl = ziplistPush(zl, (unsigned char*)"two", 3, ZIPLIST_TAIL);
        zl = ziplistPush(zl, (unsigned char*)A_252, strlen(A_252), ZIPLIST_TAIL);
        zl = ziplistPush(zl, (unsigned char*)A_250, strlen(A_250), ZIPLIST_TAIL);
        zl = ziplistPush(zl, (unsigned char*)"three", 5, ZIPLIST_TAIL);
        zl = ziplistPush(zl, (unsigned char*)"10", 2, ZIPLIST_TAIL);
        ziplistRepr(zl);

        p = ziplistIndex(zl, 2);
        if (!ziplistCompare(p, (unsigned char*)A_252, strlen(A_252))) {
            printf("ERROR: not \"A_252\"\n");
            return 1;
        }

        /* When we remove A_252, the list became: [one two A_250 three 10]
         * A_250's prev node became node two, because node two quite small
         * So A_250's prevlenSize shrink to 1, A_250's total size became 253(1+2+250)
         * The prev node of node three is still node A_250.
         * We will not shrink the node three's prevlenSize, keep it at 5 bytes */
        zl = ziplistDelete(zl, &p);
        ziplistRepr(zl);

        p = ziplistIndex(zl, 3);
        if (!ziplistCompare(p, (unsigned char*)"three", 5)) {
            printf("ERROR: not \"three\"\n");
            return 1;
        }

        /* We want to insert a node after A_250, the list became: [one two A_250 10 three 10]
         * Because the new node is quite small, node three prevlenSize will shrink to 1 */
        zl = ziplistInsert(zl, p, (unsigned char*)"10", 2);
        ziplistRepr(zl);

        /* Last element should equal 10 */
        p = ziplistIndex(zl, -1);
        if (!ziplistCompare(p, (unsigned char*)"10", 2)) {
            printf("ERROR: not \"10\"\n");
            return 1;
        }

        zfree(zl);
    }

    printf("ALL TESTS PASSED!\n");
    return 0;
}
#endif
