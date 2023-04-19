/* Linux epoll(2) based ae.c module
 *
 * Copyright (c) 2009-2012, Salvatore Sanfilippo <antirez at gmail dot com>
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


#include <sys/epoll.h>

// epoll模型的数据
typedef struct aeApiState {
    // epoll_create返回的epoll文件描述符
    int epfd;
    // events存储epoll_wait函数返回时已触发的事件数组
    struct epoll_event *events;
} aeApiState;

// 创建epoll相关的数据结构
static int aeApiCreate(aeEventLoop *eventLoop) {
    // 分配epoll相关的数据结构
    aeApiState *state = zmalloc(sizeof(aeApiState));

    if (!state) return -1;
    // 分配事件数组
    state->events = zmalloc(sizeof(struct epoll_event)*eventLoop->setsize);
    if (!state->events) {
        zfree(state);
        return -1;
    }
    // 创建epoll句柄
    state->epfd = epoll_create(1024); /* 1024 is just a hint for the kernel */
    // 创建不成功，就将分配的全部释放
    if (state->epfd == -1) {
        zfree(state->events);
        zfree(state);
        return -1;
    }
    // 在调用clone或者exec时自动关闭
    anetCloexec(state->epfd);
    // 设置到对应的时间循环数据中
    eventLoop->apidata = state;
    return 0;
}

// 重新设置event的大小
static int aeApiResize(aeEventLoop *eventLoop, int setsize) {
    aeApiState *state = eventLoop->apidata;

    state->events = zrealloc(state->events, sizeof(struct epoll_event)*setsize);
    return 0;
}

// 释放epoll的数据
static void aeApiFree(aeEventLoop *eventLoop) {
    aeApiState *state = eventLoop->apidata;
    // 关闭句柄
    close(state->epfd);
    // 释放事件数组
    zfree(state->events);
    // 释放epoll整个数据本身
    zfree(state);
}

// mask：添加的事件类型
static int aeApiAddEvent(aeEventLoop *eventLoop, int fd, int mask) {
    // 取得epoll的数据
    aeApiState *state = eventLoop->apidata;
    struct epoll_event ee = {0}; /* avoid valgrind warning */
    /* If the fd was already monitored for some event, we need a MOD
     * operation. Otherwise we need an ADD operation. */
    // 如果fd已经被监控了一些事件，那么我们需要一个MOD操作。否则我们需要ADD操作
    int op = eventLoop->events[fd].mask == AE_NONE ?
            EPOLL_CTL_ADD : EPOLL_CTL_MOD;

    ee.events = 0;
    // 根据mask进行合并监控的事件
    mask |= eventLoop->events[fd].mask; /* Merge old events */
    if (mask & AE_READABLE) ee.events |= EPOLLIN;
    if (mask & AE_WRITABLE) ee.events |= EPOLLOUT;
    ee.data.fd = fd;
    if (epoll_ctl(state->epfd,op,fd,&ee) == -1) return -1;
    return 0;
}

static void aeApiDelEvent(aeEventLoop *eventLoop, int fd, int delmask) {
    // 取得epoll的数据
    aeApiState *state = eventLoop->apidata;
    struct epoll_event ee = {0}; /* avoid valgrind warning */
    int mask = eventLoop->events[fd].mask & (~delmask);

    // 根据mask进行合并监控的事件
    ee.events = 0;
    if (mask & AE_READABLE) ee.events |= EPOLLIN;
    if (mask & AE_WRITABLE) ee.events |= EPOLLOUT;
    ee.data.fd = fd;
    // 根据mask来修改还是删除
    if (mask != AE_NONE) {
        epoll_ctl(state->epfd,EPOLL_CTL_MOD,fd,&ee);
    } else {
        /* Note, Kernel < 2.6.9 requires a non null event pointer even for
         * EPOLL_CTL_DEL. */
        epoll_ctl(state->epfd,EPOLL_CTL_DEL,fd,&ee);
    }
}

// tvp：阻塞等待文件事件的超时时间
static int aeApiPoll(aeEventLoop *eventLoop, struct timeval *tvp) {
    aeApiState *state = eventLoop->apidata;
    int retval, numevents = 0;
    // 取出准备好的事件
    retval = epoll_wait(state->epfd,state->events,eventLoop->setsize,
            tvp ? (tvp->tv_sec*1000 + (tvp->tv_usec + 999)/1000) : -1);
    if (retval > 0) {
        int j;

        numevents = retval;
        // 遍历事件
        for (j = 0; j < numevents; j++) {
            int mask = 0;
            struct epoll_event *e = state->events+j;
            // 对发生的事件进行整理
            if (e->events & EPOLLIN) mask |= AE_READABLE;
            if (e->events & EPOLLOUT) mask |= AE_WRITABLE;
            if (e->events & EPOLLERR) mask |= AE_WRITABLE|AE_READABLE;
            if (e->events & EPOLLHUP) mask |= AE_WRITABLE|AE_READABLE;
            // 放入fired数组中
            eventLoop->fired[j].fd = e->data.fd;
            eventLoop->fired[j].mask = mask;
        }
    } else if (retval == -1 && errno != EINTR) {
        panic("aeApiPoll: epoll_wait, %s", strerror(errno));
    }

    return numevents;
}

static char *aeApiName(void) {
    return "epoll";
}
