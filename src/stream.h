#ifndef STREAM_H
#define STREAM_H

#include "rax.h"
#include "listpack.h"

/* Stream item ID: a 128 bit number composed of a milliseconds time and
 * a sequence counter. IDs generated in the same millisecond (or in a past
 * millisecond if the clock jumped backward) will use the millisecond time
 * of the latest generated ID and an incremented sequence. */
// 流ID
typedef struct streamID {
    // 消息创建的时间（1970年1月1号至今的毫秒数）
    uint64_t ms;        /* Unix time in milliseconds. */
    // 序列号
    uint64_t seq;       /* Sequence number. */
} streamID;

// 消息流
typedef struct stream {
    // rax树用于保存消息生产者生产的具体消息，每个消息有唯一的ID。
    // 以ID为键，消息内容为值存储在rax树中。
    rax *rax;               /* The radix tree holding the stream. */
    // 流中的消息数目
    uint64_t length;        /* Current number of elements inside this stream. */
    // 最后（最新）插入的消息ID，如果没有就是0
    streamID last_id;       /* Zero if there are yet no items. */
    // 第一个可用的消息ID，如果为空就是0
    streamID first_id;      /* The first non-tombstone entry, zero if empty. */
    // 删除了的最大ID
    streamID max_deleted_entry_id;  /* The maximal ID that was deleted. */
    // 总共添加的元素计数
    uint64_t entries_added; /* All time count of elements added. */
    // 消费组字典：名字->消费组
    rax *cgroups;           /* Consumer groups dictionary: name -> streamCG */
} stream;

/* We define an iterator to iterate stream items in an abstract way, without
 * caring about the radix tree + listpack representation. Technically speaking
 * the iterator is only used inside streamReplyWithRange(), so could just
 * be implemented inside the function, but practically there is the AOF
 * rewriting code that also needs to iterate the stream to emit the XADD
 * commands. */
// 流迭代器：以抽象的方式定义一个迭代器来遍历一个流的元素，而不关心radix树和listpack的表示。
// 从技术上讲迭代器只在streamReplyWithRange的函数中使用，因此可以在函数中实现，
// 但实际上存在AOF重写代码，XADD命令也需要遍历流
typedef struct streamIterator {
    // 遍历的消息流
    stream *stream;         /* The stream we are iterating. */
    // 消息内容实际存储在listpack中，每个listpack都有一个master entry（也就是第一个插入的消息）
    // master_id就是第一个插入消息的ID
    streamID master_id;     /* ID of the master entry at listpack head. */
    // master entry中字段（field的）个数
    uint64_t master_fields_count;       /* Master entries # of fields. */
    // master entry field域存储的首地址
    unsigned char *master_fields_start; /* Master entries start in listpack. */
    // 当listpack中消息的field与master entry的field域完全相同时，该消息会复用master entry的
    // field域，在我们遍历该消息时，需要记录当前所在的field域的具体位置，master_fields_ptr就是
    // 记录master entry field的具体位置的
    unsigned char *master_fields_ptr;   /* Master field to emit next. */
    // 当前遍历的消息的标志位
    int entry_flags;                    /* Flags of entry we are emitting. */
    // 是否为逆向遍历，True表示从尾到头遍历
    int rev;                /* True if iterating end to start (reverse). */
    // 是否跳过已经删除的元素，如果为True，跳过已经删除的条目
    int skip_tombstones;    /* True if not emitting tombstone entries. */
    // 迭代器处理的消息开始ID(大端表示的开始键)
    uint64_t start_key[2];  /* Start key as 128 bit big endian. */
    // 迭代器处理的消息结束ID(大端表示的终止键)
    uint64_t end_key[2];    /* End key as 128 bit big endian. */
    // rax树的迭代器，用于遍历rax树中所有的key
    raxIterator ri;         /* Rax iterator. */
    // 当前listpack指针
    unsigned char *lp;      /* Current listpack. */
    // 当前正在遍历的listpack中的元素
    unsigned char *lp_ele;  /* Current listpack cursor. */
    // 指向当前消息的flag域
    unsigned char *lp_flags; /* Current entry flags pointer. */
    /* Buffers used to hold the string of lpGet() when the element is
     * integer encoded, so that there is no string representation of the
     * element inside the listpack itself. */
    // 用于从listpack读取数据时的缓存
    unsigned char field_buf[LP_INTBUF_SIZE];
    unsigned char value_buf[LP_INTBUF_SIZE];
} streamIterator;

/* Consumer group. */
// 消费组：每个stream会有多个消费组，每个消费者通过组名称进行唯一标识，同时关联一个StreamCG结构
typedef struct streamCG {
    // 该消费组已经确认的最后一个消息的ID
    streamID last_id;       /* Last delivered (not acknowledged) ID for this
                               group. Consumers that will just ask for more
                               messages will served with IDs > than this. */
    // 在完美的世界里，CG从0-0开始，没有删除，没有XGROUP SETID等等，这就是组总共读取的数目
    // 真实世界中，这个值背后的详细解释在函数streamEstimateDistanceFromFirstEverEntry顶部的注释里
    long long entries_read; /* In a perfect world (CG starts at 0-0, no dels, no
                               XGROUP SETID, ...), this is the total number of
                               group reads. In the real world, the reasoning behind
                               this value is detailed at the top comment of
                               streamEstimateDistanceFromFirstEverEntry(). */
    // pel为该消费组尚未确认的消息，并以消息ID为键，streamNACK为值(代表一个尚未确认的消息)
    rax *pel;               /* Pending entries list. This is a radix tree that
                               has every message delivered to consumers (without
                               the NOACK option) that was yet not acknowledged
                               as processed. The key of the radix tree is the
                               ID as a 64 bit big endian number, while the
                               associated value is a streamNACK structure.*/
    // consumers为该消费组中所有的消费者，并以消费者的名称为键，消费者(streamConsumer)为值
    rax *consumers;         /* A radix tree representing the consumers by name
                               and their associated representation in the form
                               of streamConsumer structures. */
} streamCG;

/* A specific consumer in a consumer group.  */
// 消费组中的消费者
typedef struct streamConsumer {
    // 最后执行一个动作（比如读或者索要的时间）
    mstime_t seen_time;         /* Last time this consumer tried to perform an action (attempted reading/claiming). */
    // 消费者最后活跃的时间
    mstime_t active_time;       /* Last time this consumer was active (successful reading/claiming). */
    // 消费者的名字
    sds name;                   /* Consumer name. This is how the consumer
                                   will be identified in the consumer group
                                   protocol. Case sensitive. */
    // 未确认的消息，未确认消息（streamNACK）维护了消费组或者消费者尚未确认的消息，值得注意的是，消费组中的pel的元素与每
    // 个消费者的pel中的元素是共享的，即该消费组消费了某个消息，这个消息会同时放到消费组以及该消费者的pel队列中，
    // 并且二者是同一个streamNACK结构。
    rax *pel;                   /* Consumer specific pending entries list: all
                                   the pending messages delivered to this
                                   consumer not yet acknowledged. Keys are
                                   big endian message IDs, while values are
                                   the same streamNACK structure referenced
                                   in the "pel" of the consumer group structure
                                   itself, so the value is shared. */
} streamConsumer;

/* Pending (yet not acknowledged) message in a consumer group. */
// 消费组中没有处理的消息
typedef struct streamNACK {
    // 消息最后投递给消费者的时间
    mstime_t delivery_time;     /* Last time this message was delivered. */
    // 消息已经发送的次数（组内成员可以通过xclaim命令获取某个消息的处理权，
    // 该消息已经分给组内另一个消费者但其并没有确认该消息）
    uint64_t delivery_count;    /* Number of times this message was delivered.*/
    // 当前归属的消费者
    streamConsumer *consumer;   /* The consumer this message was delivered to
                                   in the last delivery. */
} streamNACK;

/* Stream propagation information, passed to functions in order to propagate
 * XCLAIM commands to AOF and slaves. */
// 流传播信息，传递给函数，以便将XCLAIM命令传播到AOF和从机
typedef struct streamPropInfo {
    robj *keyname;
    robj *groupname;
} streamPropInfo;

/* Prototypes of exported APIs. */
struct client;

/* Flags for streamCreateConsumer */
#define SCC_DEFAULT       0
#define SCC_NO_NOTIFY     (1<<0) /* Do not notify key space if consumer created */
#define SCC_NO_DIRTIFY    (1<<1) /* Do not dirty++ if consumer created */

#define SCG_INVALID_ENTRIES_READ -1

stream *streamNew(void);
void freeStream(stream *s);
unsigned long streamLength(const robj *subject);
size_t streamReplyWithRange(client *c, stream *s, streamID *start, streamID *end, size_t count, int rev, streamCG *group, streamConsumer *consumer, int flags, streamPropInfo *spi);
void streamIteratorStart(streamIterator *si, stream *s, streamID *start, streamID *end, int rev);
int streamIteratorGetID(streamIterator *si, streamID *id, int64_t *numfields);
void streamIteratorGetField(streamIterator *si, unsigned char **fieldptr, unsigned char **valueptr, int64_t *fieldlen, int64_t *valuelen);
void streamIteratorRemoveEntry(streamIterator *si, streamID *current);
void streamIteratorStop(streamIterator *si);
streamCG *streamLookupCG(stream *s, sds groupname);
streamConsumer *streamLookupConsumer(streamCG *cg, sds name);
streamConsumer *streamCreateConsumer(streamCG *cg, sds name, robj *key, int dbid, int flags);
streamCG *streamCreateCG(stream *s, char *name, size_t namelen, streamID *id, long long entries_read);
streamNACK *streamCreateNACK(streamConsumer *consumer);
void streamDecodeID(void *buf, streamID *id);
int streamCompareID(streamID *a, streamID *b);
void streamFreeNACK(streamNACK *na);
int streamIncrID(streamID *id);
int streamDecrID(streamID *id);
void streamPropagateConsumerCreation(client *c, robj *key, robj *groupname, sds consumername);
robj *streamDup(robj *o);
int streamValidateListpackIntegrity(unsigned char *lp, size_t size, int deep);
int streamParseID(const robj *o, streamID *id);
robj *createObjectFromStreamID(streamID *id);
int streamAppendItem(stream *s, robj **argv, int64_t numfields, streamID *added_id, streamID *use_id, int seq_given);
int streamDeleteItem(stream *s, streamID *id);
void streamGetEdgeID(stream *s, int first, int skip_tombstones, streamID *edge_id);
long long streamEstimateDistanceFromFirstEverEntry(stream *s, streamID *id);
int64_t streamTrimByLength(stream *s, long long maxlen, int approx);
int64_t streamTrimByID(stream *s, streamID minid, int approx);

#endif
