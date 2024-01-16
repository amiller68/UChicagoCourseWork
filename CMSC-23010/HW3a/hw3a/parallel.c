#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <pthread.h>

#include "lib/lock.h"
#include "lib/stopwatch.h"

#define UNUSED(x) (void)(x)

void *count_worker(void *arg);
long counter(int n, lock_t *L);

/*
 * serial.c - count to a specified integer and print results
 *
 * B : a number to count to
 *
 * n : a number of threads to use (not used in this file)
 *
 * L : a char specifying a Locking algorithm (not used in this file)
 *     t, p, a, and m will direct threads to use our Test and Set Lock,
 *     the pthread Mutex, our Anderson’s Array Lock, and our MCS lock, respectively.
 *
 */
int main(int argc, char *argv[])
{
    //If less than 3 arguments supplied
    if (argc < 4)
    {
        printf("ERR: Inappropriate amount of arguments supplied\n");
        exit(1);
    }

    //printf("Reading input...\n");

    int B = atoi(argv[1]);
    int n = atoi(argv[2]);
    char type = *argv[3];

    //printf("Making lock...\n");

    lock_t *L = new_lock(type, n);
    L->B = B;

    StopWatch_t sw;
    long final_count;

    //printf("Starting test...\n");

    startTimer(&sw);
    final_count = counter(n, L);
    stopTimer(&sw);

    double time = getElapsedTime(&sw);
    printf("%ld,%.*f\n", final_count, DBL_DIG-1, time);

    destroy_lock(L);
    return 0;
}

void *count_worker(void *arg)
{
    lock_t *L = (lock_t *) arg;
    L->init_thread(L->l);
    while(1)
    {
        L->lock(L->l);
        if (L->counter < L->B)
        {
            L->counter++;
            //printf("Counter = %ld\n", L->counter);
            L->unlock(L->l);
            continue;
        }
        //printf("thread-%ld: Last unlock!\n", pthread_self());
        L->unlock(L->l);
        //printf("thread-%ld: Done counting!\n", pthread_self());
        return NULL;
    }
}

long counter(int n, lock_t *L)
{
    int i;
    pthread_t threads[n - 1];
    pthread_attr_t tattr;
    //printf("thread-%ld: top level!\n", pthread_self());


/*
    pthread_attr_init(&tattr);
    pthread_attr_setdetachstate(&tattr,PTHREAD_CREATE_DETACHED);
*/
    for(i = 0; i < n - 1; i++)
    {
        //pthread_create(&threads[i], &tattr, &count_worker, (void *) L);
        pthread_create(&threads[i], NULL, &count_worker, (void *) L);
    }

    L->init_thread(L->l);

    while(1)
    {
        L->lock(L->l);
        if (L->counter < L->B)
        {
            L->counter++;
            L->unlock(L->l);
            continue;
        }
        //printf("thread-%ld: Done counting!\n", pthread_self());
        L->unlock(L->l);
        break;
    }

    for(i = 0; i < n - 1; i++)
    {
        //pthread_create(&threads[i], &tattr, &count_worker, (void *) L);
        pthread_join(threads[i], NULL);
    }

    return L->counter;
}
