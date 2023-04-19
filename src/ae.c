/* A simple event-driven programming library. Originally I wrote this code
 * for the Jim's event-loop (Jim is a Tcl interpreter) but later translated
 * it in form of a library for easy reuse.
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

#include "ae.h"
#include "anet.h"
#include "redisassert.h"

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <poll.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include "zmalloc.h"
#include "config.h"

/* Include the best multiplexing layer supported by this system.
 * The following should be ordered by performances, descending. */
#ifdef HAVE_EVPORT
#include "ae_evport.c"
#else
    #ifdef HAVE_EPOLL
    #include "ae_epoll.c"
    #else
        #ifdef HAVE_KQUEUE
        #include "ae_kqueue.c"
        #else
        #include "ae_select.c"
        #endif
    #endif
#endif


aeEventLoop *aeCreateEventLoop(int setsize) {
    aeEventLoop *eventLoop;
    int i;

    monotonicInit();    /* just in case the calling app didn't initialize */

    if ((eventLoop = zmalloc(sizeof(*eventLoop))) == NULL) goto err;
    eventLoop->events = zmalloc(sizeof(aeFileEvent)*setsize);
    eventLoop->fired = zmalloc(sizeof(aeFiredEvent)*setsize);
    if (eventLoop->events == NULL || eventLoop->fired == NULL) goto err;
    eventLoop->setsize = setsize;
    eventLoop->timeEventHead = NULL;
    eventLoop->timeEventNextId = 0;
    eventLoop->stop = 0;
    eventLoop->maxfd = -1;
    eventLoop->beforesleep = NULL;
    eventLoop->aftersleep = NULL;
    eventLoop->flags = 0;
    if (aeApiCreate(eventLoop) == -1) goto err;
    /* Events with mask == AE_NONE are not set. So let's initialize the
     * vector with it. */
    for (i = 0; i < setsize; i++)
        eventLoop->events[i].mask = AE_NONE;
    return eventLoop;

err:
    if (eventLoop) {
        zfree(eventLoop->events);
        zfree(eventLoop->fired);
        zfree(eventLoop);
    }
    return NULL;
}

/* Return the current set size. */
int aeGetSetSize(aeEventLoop *eventLoop) {
    return eventLoop->setsize;
}

/* Tells the next iteration/s of the event processing to set timeout of 0. */
void aeSetDontWait(aeEventLoop *eventLoop, int noWait) {
    if (noWait)
        eventLoop->flags |= AE_DONT_WAIT;
    else
        eventLoop->flags &= ~AE_DONT_WAIT;
}

/* Resize the maximum set size of the event loop.
 * If the requested set size is smaller than the current set size, but
 * there is already a file descriptor in use that is >= the requested
 * set size minus one, AE_ERR is returned and the operation is not
 * performed at all.
 *
 * Otherwise AE_OK is returned and the operation is successful. */
int aeResizeSetSize(aeEventLoop *eventLoop, int setsize) {
    int i;

    if (setsize == eventLoop->setsize) return AE_OK;
    if (eventLoop->maxfd >= setsize) return AE_ERR;
    if (aeApiResize(eventLoop,setsize) == -1) return AE_ERR;

    eventLoop->events = zrealloc(eventLoop->events,sizeof(aeFileEvent)*setsize);
    eventLoop->fired = zrealloc(eventLoop->fired,sizeof(aeFiredEvent)*setsize);
    eventLoop->setsize = setsize;

    /* Make sure that if we created new slots, they are initialized with
     * an AE_NONE mask. */
    for (i = eventLoop->maxfd+1; i < setsize; i++)
        eventLoop->events[i].mask = AE_NONE;
    return AE_OK;
}

void aeDeleteEventLoop(aeEventLoop *eventLoop) {
    aeApiFree(eventLoop);
    zfree(eventLoop->events);
    zfree(eventLoop->fired);

    /* Free the time events list. */
    aeTimeEvent *next_te, *te = eventLoop->timeEventHead;
    while (te) {
        next_te = te->next;
        zfree(te);
        te = next_te;
    }
    zfree(eventLoop);
}

void aeStop(aeEventLoop *eventLoop) {
    eventLoop->stop = 1;
}

// 创建文件事件，并存储在aeEventLoop的events字段中,然后调用aeApiAddEvent加入api的监控中
int aeCreateFileEvent(aeEventLoop *eventLoop, int fd, int mask,
        aeFileProc *proc, void *clientData)
{
    // fd当索引，超过最大值，直接返回错误
    if (fd >= eventLoop->setsize) {
        errno = ERANGE;
        return AE_ERR;
    }
    // 存储在对应的结构中
    aeFileEvent *fe = &eventLoop->events[fd];

    // 加入api事件中
    if (aeApiAddEvent(eventLoop, fd, mask) == -1)
        return AE_ERR;
    // 初始化关心的事件，读取和写入函数已经客户端对象
    fe->mask |= mask;
    if (mask & AE_READABLE) fe->rfileProc = proc;
    if (mask & AE_WRITABLE) fe->wfileProc = proc;
    fe->clientData = clientData;
    // 如果文件句柄大于最大句柄，更新最大句柄
    if (fd > eventLoop->maxfd)
        eventLoop->maxfd = fd;
    return AE_OK;
}

void aeDeleteFileEvent(aeEventLoop *eventLoop, int fd, int mask)
{
    if (fd >= eventLoop->setsize) return;
    aeFileEvent *fe = &eventLoop->events[fd];
    if (fe->mask == AE_NONE) return;

    /* We want to always remove AE_BARRIER if set when AE_WRITABLE
     * is removed. */
    if (mask & AE_WRITABLE) mask |= AE_BARRIER;

    aeApiDelEvent(eventLoop, fd, mask);
    fe->mask = fe->mask & (~mask);
    if (fd == eventLoop->maxfd && fe->mask == AE_NONE) {
        /* Update the max fd */
        int j;

        for (j = eventLoop->maxfd-1; j >= 0; j--)
            if (eventLoop->events[j].mask != AE_NONE) break;
        eventLoop->maxfd = j;
    }
}

void *aeGetFileClientData(aeEventLoop *eventLoop, int fd) {
    if (fd >= eventLoop->setsize) return NULL;
    aeFileEvent *fe = &eventLoop->events[fd];
    if (fe->mask == AE_NONE) return NULL;

    return fe->clientData;
}

int aeGetFileEvents(aeEventLoop *eventLoop, int fd) {
    if (fd >= eventLoop->setsize) return 0;
    aeFileEvent *fe = &eventLoop->events[fd];

    return fe->mask;
}

// 创建时间事件
// eventLoop：事件循环，milliseconds：此时间事件触发时间，单位毫秒，注意这是
// 一个相对时间，即从当前时间算起，milliseconds毫秒后此时间事件会被触发；
// proc：时间事件的处理函数， clientData：对应的客户端数据，
// finalizerProc：同样是函数指针，删除时间事件节点之前会调用此函数。
long long aeCreateTimeEvent(aeEventLoop *eventLoop, long long milliseconds,
        aeTimeProc *proc, void *clientData,
        aeEventFinalizerProc *finalizerProc)
{
    // 生成ID
    long long id = eventLoop->timeEventNextId++;
    aeTimeEvent *te;

    te = zmalloc(sizeof(*te));
    if (te == NULL) return AE_ERR;
    te->id = id;
    te->when = getMonotonicUs() + milliseconds * 1000;
    te->timeProc = proc;
    te->finalizerProc = finalizerProc;
    te->clientData = clientData;
    te->prev = NULL;
    // 插入在队伍头
    te->next = eventLoop->timeEventHead;
    te->refcount = 0;
    // 双向链表连接
    if (te->next)
        te->next->prev = te;
    // 设置队头
    eventLoop->timeEventHead = te;
    return id;
}

// 删除时间事件
int aeDeleteTimeEvent(aeEventLoop *eventLoop, long long id)
{
    aeTimeEvent *te = eventLoop->timeEventHead;
    // 遍历找到对应的时间事件节点
    while(te) {
        if (te->id == id) {
            // 将事件id设置为计划删除的id
            te->id = AE_DELETED_EVENT_ID;
            return AE_OK;
        }
        te = te->next;
    }
    // 没找到就返回错误
    return AE_ERR; /* NO event with the specified ID found */
}

/* How many microseconds until the first timer should fire.
 * If there are no timers, -1 is returned.
 *
 * Note that's O(N) since time events are unsorted.
 * Possible optimizations (not needed by Redis so far, but...):
 * 1) Insert the event in order, so that the nearest is just the head.
 *    Much better but still insertion or deletion of timers is O(N).
 * 2) Use a skiplist to have this operation as O(1) and insertion as O(log(N)).
 */
// 距离第一个计时器启动还有多少微秒。如果没有计时器，则返回-1
static int64_t usUntilEarliestTimer(aeEventLoop *eventLoop) {
    aeTimeEvent *te = eventLoop->timeEventHead;
    if (te == NULL) return -1;

    aeTimeEvent *earliest = NULL;
    while (te) {
        if ((!earliest || te->when < earliest->when) && te->id != AE_DELETED_EVENT_ID)
            earliest = te;
        te = te->next;
    }

    monotime now = getMonotonicUs();
    return (now >= earliest->when) ? 0 : earliest->when - now;
}

/* Process time events */
// 时间事件执行函数
static int processTimeEvents(aeEventLoop *eventLoop) {
    int processed = 0;
    aeTimeEvent *te;
    long long maxId;
    // 取得表头
    te = eventLoop->timeEventHead;
    // 最大的时间事件ID
    maxId = eventLoop->timeEventNextId-1;
    // 现在的时间
    monotime now = getMonotonicUs();
    while(te) {
        long long id;

        /* Remove events scheduled for deletion. */
        // 删除计划删除的事件，id等于AE_DELETED_EVENT_ID表示要删除的事件
        if (te->id == AE_DELETED_EVENT_ID) {
            aeTimeEvent *next = te->next;
            /* If a reference exists for this timer event,
             * don't free it. This is currently incremented
             * for recursive timerProc calls */
            // 如果存在此计时器事件的引用，不要释放它。对于递归timerProc调用，此引用当前会递增
            // 直接跳过，不处理
            if (te->refcount) {
                te = next;
                continue;
            }
            // 从链表中脱离出来，设置下一个
            if (te->prev)
                te->prev->next = te->next;
            else
                eventLoop->timeEventHead = te->next;
            
            // 从链表中脱离出来，设置前一个
            if (te->next)
                te->next->prev = te->prev;

            // 如果有删除前的回调，就调用
            if (te->finalizerProc) {
                te->finalizerProc(eventLoop, te->clientData);
                // 注意需要更新当前时间
                now = getMonotonicUs();
            }
            // 释放节点
            zfree(te);
            // 处理下一个
            te = next;
            continue;
        }

        /* Make sure we don't process time events created by time events in
         * this iteration. Note that this check is currently useless: we always
         * add new timers on the head, however if we change the implementation
         * detail, this check may be useful again: we keep it here for future
         * defense. */
        // 如果时间事件是在这一次遍历中产生的
        if (te->id > maxId) {
            te = te->next;
            continue;
        }

        // 如果时间到了
        if (te->when <= now) {
            int retval;

            id = te->id;
            // 增加引用计数
            te->refcount++;
            // 处理时间事件回调,注意时间事件处理函数timeProc返回值retval，其表示此时间事件
            // 下次应该被触发的时间，单位为毫秒，且是一个相对时间，即从当前时间算起，retval毫秒后此时间事件会被触发。
            retval = te->timeProc(eventLoop, id, te->clientData);
            te->refcount--;
            processed++;
            // 更新当前时间
            now = getMonotonicUs();
            // 如果不是不再触发，就得计算下一次触发时间
            if (retval != AE_NOMORE) {
                te->when = now + retval * 1000;
            } else {
                // 不再触发了，下一次就删除
                te->id = AE_DELETED_EVENT_ID;
            }
        }
        // 处理下一个
        te = te->next;
    }
    // 返回处理时间事件的数目
    return processed;
}

/* Process every pending time event, then every pending file event
 * (that may be registered by time event callbacks just processed).
 * Without special flags the function sleeps until some file event
 * fires, or when the next time event occurs (if any).
 * 处理每一个挂起的时间事件，然后处理每个挂起的文件事件（可能由刚刚处理的时间事件回调注册）。
 * 在没有特殊标志的情况下，函数将休眠，直到某个文件事件触发，或者下一个时间事件发生（如果有）。
 * 
 * If flags is 0, the function does nothing and returns.
 * if flags has AE_ALL_EVENTS set, all the kind of events are processed.
 * if flags has AE_FILE_EVENTS set, file events are processed.
 * if flags has AE_TIME_EVENTS set, time events are processed.
 * if flags has AE_DONT_WAIT set, the function returns ASAP once all
 * the events that can be handled without a wait are processed.
 * if flags has AE_CALL_AFTER_SLEEP set, the aftersleep callback is called.
 * if flags has AE_CALL_BEFORE_SLEEP set, the beforesleep callback is called.
 *
 * The function returns the number of events processed. */
// 该函数返回事件处理的数目
int aeProcessEvents(aeEventLoop *eventLoop, int flags)
{
    int processed = 0, numevents;

    /* Nothing to do? return ASAP */
    // 没事可干，直接返回
    if (!(flags & AE_TIME_EVENTS) && !(flags & AE_FILE_EVENTS)) return 0;

    /* Note that we want to call select() even if there are no
     * file events to process as long as we want to process time
     * events, in order to sleep until the next time event is ready
     * to fire. */
    // 请注意，即使没有要处理的文件事件，只要我们想处理时间事件，我们也要调用select()，以便睡眠，直到下一个事件准备好启动
    if (eventLoop->maxfd != -1 ||
        ((flags & AE_TIME_EVENTS) && !(flags & AE_DONT_WAIT))) {
        int j;
        struct timeval tv, *tvp;
        int64_t usUntilTimer = -1;

        // 处理时间事件，得到最早的时间事件还需要多久
        if (flags & AE_TIME_EVENTS && !(flags & AE_DONT_WAIT))
            usUntilTimer = usUntilEarliestTimer(eventLoop);

        // 将时间转换为timeval
        if (usUntilTimer >= 0) {
            tv.tv_sec = usUntilTimer / 1000000;
            tv.tv_usec = usUntilTimer % 1000000;
            tvp = &tv;
        } else {
            /* If we have to check for events but need to return
             * ASAP because of AE_DONT_WAIT we need to set the timeout
             * to zero */
            // 不等待
            if (flags & AE_DONT_WAIT) {
                tv.tv_sec = tv.tv_usec = 0;
                tvp = &tv;
            } else {
                /* Otherwise we can block */
                // 一直等
                tvp = NULL; /* wait forever */
            }
        }

        // 如果有不等待标志，将时间清零
        if (eventLoop->flags & AE_DONT_WAIT) {
            tv.tv_sec = tv.tv_usec = 0;
            tvp = &tv;
        }
        // 如果有睡眠前处理需求，调用睡眠前处理的回调
        if (eventLoop->beforesleep != NULL && flags & AE_CALL_BEFORE_SLEEP)
            eventLoop->beforesleep(eventLoop);

        /* Call the multiplexing API, will return only on timeout or when
         * some event fires. */
        // 调用多路复用API，将仅在超时或某些事件触发时返回。
        numevents = aeApiPoll(eventLoop, tvp);

        /* Don't process file events if not requested. */
        // 如果不处理文件事件
        if (!(flags & AE_FILE_EVENTS)) {
            numevents = 0;
        }

        /* After sleep callback. */
        // 如果有睡醒后回调需求，睡醒了调用回调函数
        if (eventLoop->aftersleep != NULL && flags & AE_CALL_AFTER_SLEEP)
            eventLoop->aftersleep(eventLoop);

        // 处理文件事件
        for (j = 0; j < numevents; j++) {
            int fd = eventLoop->fired[j].fd;
            aeFileEvent *fe = &eventLoop->events[fd];
            int mask = eventLoop->fired[j].mask;
            int fired = 0; /* Number of events fired for current fd. */

            /* Normally we execute the readable event first, and the writable
             * event later. This is useful as sometimes we may be able
             * to serve the reply of a query immediately after processing the
             * query.
             *
             * However if AE_BARRIER is set in the mask, our application is
             * asking us to do the reverse: never fire the writable event
             * after the readable. In such a case, we invert the calls.
             * This is useful when, for instance, we want to do things
             * in the beforeSleep() hook, like fsyncing a file to disk,
             * before replying to a client. */、
             // 通常，我们先执行可读事件，然后再执行可写事件。这很有用，因为有时我们可以在处理查询后立即提供查询的答复。
             // 但是，如果在掩码中设置了AE_BARRIER，我们的应用程序会要求我们执行相反的操作：永远不要在可读事件之后激发
             // 可写事件。在这种情况下，我们将调用反转。例如，当我们想在beforeSleep（）钩子中做一些事情时，这很有用，
             // 比如在回复客户端之前将文件同步到磁盘。
            int invert = fe->mask & AE_BARRIER;

            /* Note the "fe->mask & mask & ..." code: maybe an already
             * processed event removed an element that fired and we still
             * didn't processed, so we check if the event is still valid.
             *
             * Fire the readable event if the call sequence is not
             * inverted. */
            // 可读事件
            if (!invert && fe->mask & mask & AE_READABLE) {
                fe->rfileProc(eventLoop,fd,fe->clientData,mask);
                fired++;
                fe = &eventLoop->events[fd]; /* Refresh in case of resize. */
            }

            /* Fire the writable event. */
            // 可写事件
            if (fe->mask & mask & AE_WRITABLE) {
                if (!fired || fe->wfileProc != fe->rfileProc) {
                    fe->wfileProc(eventLoop,fd,fe->clientData,mask);
                    fired++;
                }
            }

            /* If we have to invert the call, fire the readable event now
             * after the writable one. */
            // 如果必须反转调用，请在可写事件之后立即启动可读事件
            if (invert) {
                fe = &eventLoop->events[fd]; /* Refresh in case of resize. */
                if ((fe->mask & mask & AE_READABLE) &&
                    (!fired || fe->wfileProc != fe->rfileProc))
                {
                    fe->rfileProc(eventLoop,fd,fe->clientData,mask);
                    fired++;
                }
            }

            processed++;
        }
    }
    /* Check time events */
    // 检查时间事件
    if (flags & AE_TIME_EVENTS)
        processed += processTimeEvents(eventLoop);
    // 返回处理文件和时间事件的总数
    return processed; /* return the number of processed file/time events */
}

/* Wait for milliseconds until the given file descriptor becomes
 * writable/readable/exception */
int aeWait(int fd, int mask, long long milliseconds) {
    struct pollfd pfd;
    int retmask = 0, retval;

    memset(&pfd, 0, sizeof(pfd));
    pfd.fd = fd;
    if (mask & AE_READABLE) pfd.events |= POLLIN;
    if (mask & AE_WRITABLE) pfd.events |= POLLOUT;

    if ((retval = poll(&pfd, 1, milliseconds))== 1) {
        if (pfd.revents & POLLIN) retmask |= AE_READABLE;
        if (pfd.revents & POLLOUT) retmask |= AE_WRITABLE;
        if (pfd.revents & POLLERR) retmask |= AE_WRITABLE;
        if (pfd.revents & POLLHUP) retmask |= AE_WRITABLE;
        return retmask;
    } else {
        return retval;
    }
}

// 处理事件的主函数
void aeMain(aeEventLoop *eventLoop) {
    eventLoop->stop = 0;
    while (!eventLoop->stop) {
        aeProcessEvents(eventLoop, AE_ALL_EVENTS|
                                   AE_CALL_BEFORE_SLEEP|
                                   AE_CALL_AFTER_SLEEP);
    }
}

char *aeGetApiName(void) {
    return aeApiName();
}

void aeSetBeforeSleepProc(aeEventLoop *eventLoop, aeBeforeSleepProc *beforesleep) {
    eventLoop->beforesleep = beforesleep;
}

void aeSetAfterSleepProc(aeEventLoop *eventLoop, aeBeforeSleepProc *aftersleep) {
    eventLoop->aftersleep = aftersleep;
}
