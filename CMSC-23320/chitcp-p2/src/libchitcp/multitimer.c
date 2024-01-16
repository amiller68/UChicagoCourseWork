/*
 *  chiTCP - A simple, testable TCP stack
 *
 *  An API for managing multiple timers
 */

/*
 *  Copyright (c) 2013-2019, The University of Chicago
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 *  - Neither the name of The University of Chicago nor the names of its
 *    contributors may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "chitcp/multitimer.h"
#include "chitcp/log.h"

int mt_chilog_single_timer(loglevel_t level, single_timer_t *timer);


int cmp_single_timer(single_timer_t *t1, single_timer_t *t2)
{
    /* if t1 TOs before t2, return a negative number
       if t1 TOs after t2, reutrn a postive number*/
    struct timespec result;
    /*Returns: 1 if the difference is negative, otherwise 0.*/
    return timespec_subtract(&result, &t1->wake_up, &t2->wake_up) ? -1 : 1;
}


/* See multitimer.h */
int timespec_subtract(struct timespec *result, struct timespec *x, struct timespec *y)
{
    struct timespec tmp;
    tmp.tv_sec = y->tv_sec;
    tmp.tv_nsec = y->tv_nsec;

    /* Perform the carry for the later subtraction by updating tmp. */
    if (x->tv_nsec < tmp.tv_nsec) {
        uint64_t sec = (tmp.tv_nsec - x->tv_nsec) / SECOND + 1;
        tmp.tv_nsec -= SECOND * sec;
        tmp.tv_sec += sec;
    }
    if (x->tv_nsec - tmp.tv_nsec > SECOND) {
        uint64_t sec = (x->tv_nsec - tmp.tv_nsec) / SECOND;
        tmp.tv_nsec += SECOND * sec;
        tmp.tv_sec -= sec;
    }

    /* Compute the time remaining to wait.
       tv_nsec is certainly positive. */
    result->tv_sec = x->tv_sec - tmp.tv_sec;
    result->tv_nsec = x->tv_nsec - tmp.tv_nsec;

    /* Return 1 if result is negative. */
    return x->tv_sec < tmp.tv_sec;
}


/* mt_thread - handles the timing and signals of the multitimer thread in order
 * to serve multiple different timers at once.
 *
 * arg - Any arguments passed into the thread when it is created.
 *
 * returns - nothing
*/
void *mt_thread(void *arg)
{
    multi_timer_t *mt = arg;
    single_timer_t *timer;
    int rc;

    pthread_mutex_lock(&mt->timers_lock);
    while (!mt->closing)
    {
        //Wait to be signalled & for us to have a timer to watch
        if(!(timer = mt->active_timers))
            rc = pthread_cond_wait(&mt->cv_active, &mt->timers_lock);

        else
        {
            if (!timer->active)
            {
                chilog(ERROR, "ERR: Priority timer is not active");
                LL_DELETE(mt->active_timers, timer);
            }

            rc = pthread_cond_timedwait(&mt->cv_active, &mt->timers_lock, 
                                            &timer->wake_up);

            //If we TO
            if (rc == ETIMEDOUT)
            {
                chilog(DEBUG, "Timer-%d : TO", timer->id);
                timer->active = false;
                timer->num_timeouts++;
                LL_DELETE(mt->active_timers, timer);
                timer->callback(mt, timer, timer->args);
            }

            else if (rc == EINVAL)
            {
                chilog(ERROR, "ERR: Failed pthread_cond_timedwait", timer->id);
                timer->active = false;
                LL_DELETE(mt->active_timers, timer);
            }
        }
    }
    pthread_mutex_unlock(&mt->timers_lock);

    return CHITCP_OK;
}


/* See multitimer.h */
int mt_init(multi_timer_t *mt, uint16_t num_timers)
{
    chilog(DEBUG, "init...");
    //Init the UTList
    mt->active_timers = NULL;
    mt->num_timers = num_timers;
    mt->closing = false;
    if (mt->timers = calloc(num_timers, sizeof(single_timer_t)))
    {
        for (int i = 0; i < num_timers; i++)
        {
            chilog(DEBUG, "Setting timer at %p to id %d", &mt->timers[i], i);
            mt->timers[i].id = (uint16_t) i;
            mt->timers[i].num_timeouts = 0;
        }
        if (pthread_mutex_init(&mt->timers_lock, NULL) == -1)
        {
            return CHITCP_EINIT;
        }
        if (pthread_cond_init(&mt->cv_active, NULL) == -1)
        {
            return CHITCP_EINIT;
        }
        if (pthread_create(&mt->timer_thread, NULL, mt_thread, mt) == -1)
        {
            return CHITCP_EINIT;
        }

        return CHITCP_OK;
    }

    return CHITCP_ENOMEM;
}


/* See multitimer.h */
int mt_free(multi_timer_t *mt)
{
    pthread_mutex_lock(&mt->timers_lock);
    mt->closing = true;
    pthread_cond_signal(&mt->cv_active);
    pthread_mutex_unlock(&mt->timers_lock);
    pthread_join(mt->timer_thread, NULL);

    free(mt->timers);
    pthread_mutex_destroy(&mt->timers_lock);
    pthread_cond_destroy(&mt->cv_active);

    return CHITCP_OK;
}


/* See multitimer.h */
int mt_get_timer_by_id(multi_timer_t *mt, uint16_t id, single_timer_t **timer)
{
    single_timer_t *timers = mt->timers;
    uint16_t num_timers = mt->num_timers;
    //chilog(DEBUG, "Getting Timer-%d", id);
    if (id >= num_timers)
    {
        chilog(ERROR, "ERR: No such timer");
        *timer = NULL;
        return CHITCP_EINVAL;
    }
    *timer = &timers[id];
    mt_chilog_single_timer(DEBUG, *timer);
    return CHITCP_OK;
}


/* See multitimer.h */
int mt_set_timer(multi_timer_t *mt, uint16_t id, uint64_t timeout, mt_callback_func callback, void* callback_args)
{
    chilog(DEBUG, "Setting timer | id: %d, TO: %d", id, timeout);
    single_timer_t *timer = NULL;
    mt_get_timer_by_id(mt, id, &timer);

    if (!timer)
    {
        chilog(ERROR, "No such timer");
        return CHITCP_EINVAL;
    }

    /*Activate the timer*/
    timer->active = true;

    struct timespec now;
    struct timespec *wake_up = &timer->wake_up;
    /*Get the current timer*/
    clock_gettime(CLOCK_REALTIME, &now);
    /*Increment it according to TO*/
    wake_up->tv_nsec = now.tv_nsec + timeout;
    wake_up->tv_sec  = now.tv_sec + (timeout / SECOND);

    /*Noramlize the timespec*/
    if (wake_up->tv_nsec >= 1000000000)
    {
        wake_up->tv_nsec %= 1000000000;
        ++wake_up->tv_sec;
    }

    /*Assign the callback*/
    timer->callback = callback;
    timer->args = callback_args;

    /*Insert it into the active timer list*/
    pthread_mutex_lock(&mt->timers_lock);
    LL_INSERT_INORDER(mt->active_timers, timer, cmp_single_timer);
    pthread_cond_signal(&mt->cv_active);
    pthread_mutex_unlock(&mt->timers_lock);

    return CHITCP_OK;
}


/* See multitimer.h */
int mt_cancel_timer(multi_timer_t *mt, uint16_t id)
{
    chilog(DEBUG,"Cancelling Timer | id : %d", id);
    single_timer_t *timer;
    mt_get_timer_by_id(mt, id, &timer);
    if (timer)
    {
        if (timer->active)
        {
            pthread_mutex_lock(&mt->timers_lock);
            timer->active = false;
            timer->num_timeouts = 0;
            LL_DELETE(mt->active_timers, timer);
            pthread_cond_signal(&mt->cv_active);
            pthread_mutex_unlock(&mt->timers_lock);

            chilog(DEBUG, "new num_timeout = %d", timer->num_timeouts);
            return CHITCP_OK;
        }
        return CHITCP_EINVAL;
    }
    chilog(ERROR, "ERR: No such timer!");
    return CHITCP_EINVAL;
}


/* See multitimer.h */
int mt_set_timer_name(multi_timer_t *mt, uint16_t id, const char *name)
{
    single_timer_t *timer = NULL;
    mt_get_timer_by_id(mt, id, &timer);
    strcpy(timer->name, name);
    return CHITCP_OK;
}


/* mt_chilog_single_timer - Prints a single timer using chilog
 *
 * level: chilog log level
 *
 * timer: Timer
 *
 * Returns: Always returns CHITCP_OK
 */
int mt_chilog_single_timer(loglevel_t level, single_timer_t *timer)
{
    struct timespec now, diff;
    clock_gettime(CLOCK_REALTIME, &now);

    if(timer->active)
    {
        /* Compute the appropriate value for "diff" here; it should contain
         * the time remaining until the timer times out.
         * Note: The timespec_subtract function can come in handy here*/
        struct timespec *wake_up = &timer->wake_up;
        //timespec_subtract(&diff, &now, wake_up);
        //chilog(level, "%i %s %lis %lins", timer->id, timer->name, diff.tv_sec, diff.tv_nsec);
        chilog(level, "Timer-%d | name: %s ,  num_TOs : %d ,wake_up : %lis", 
                timer->id, timer->name, timer->num_timeouts, wake_up->tv_sec);
    }
    else
        chilog(level, "Timer-%d | name: %s ,  num_TOs : %d", timer->id, 
                    timer->name, timer->num_timeouts);

    return CHITCP_OK;
}


/* See multitimer.h */
int mt_chilog(loglevel_t level, multi_timer_t *mt, bool active_only)
{

    int i;
    for (i = 0; i < mt->num_timers; i++)
    {
        if (active_only)
        {
            if (mt->timers[i].active)
            {
                struct timespec now, diff;
                clock_gettime(CLOCK_REALTIME, &now);
                timespec_subtract(&diff, &now, &mt->timers[i].wake_up);
                chilog(level, "%d: Timer %i: %s %lis %lins", i + 1, 
                        mt->timers[i].id, mt->timers[i].name, diff.tv_sec, 
                        diff.tv_nsec);
            }
            return CHITCP_OK;
        }
        else
        {
            mt_chilog_single_timer(level, &mt->timers[i]);
        }
    }
    return CHITCP_OK;
}
