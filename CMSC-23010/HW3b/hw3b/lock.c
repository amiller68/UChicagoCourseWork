#include "lib/lock.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define UNUSED(x) (void)(x)

//Create a lock of a specified type
lock_t *new_lock(char type, int n)
{
    lock_t *L = (lock_t *) malloc(sizeof(lock_t));

    if(!L)
    {
        printf("ERR: Could not allocate space for lock_t instance\n");
        return NULL;
    }

    L->type = type;

    switch(type)
    {
        case 't':
            L->l = (void *) new_tas(n);
            L->init_thread = tas_init;
            L->trylock = tas_trylock;
            L->lock = tas_lock;
            L->unlock = tas_unlock;
            return L;
        case 'p':
            L->l = (void *) new_mutex(n);
            L->init_thread = mutex_init;
            L->trylock = mutex_trylock;
            L->lock = mutex_lock;
            L->unlock = mutex_unlock;
            return L;
        case 'a':
            L->l = (void *) new_alock(n);
            L->init_thread = alock_init;
            L->trylock = alock_trylock;
            L->lock = alock_lock;
            L->unlock = alock_unlock;
            return L;
        case 'm':
            L->l = (void *) new_mcs(n);
            L->init_thread = mcs_init;
            L->trylock = mcs_trylock;
            L->lock = mcs_lock;
            L->unlock = mcs_unlock;
            return L;
        default:
            printf("ERR: Invalid lock type\n");
            free(L);
            return NULL;
    }
}

//Destroy a lock of a specified type
int destroy_lock(lock_t *L)
{
    switch(L->type)
    {
        case 't':
            free(L);
            return destroy_tas((tas_t *) L->l);
        case 'p':
            free(L);
            return destroy_mutex((pthread_mutex_t *) L->l);
        case 'a':
            free(L);
            return destroy_alock((alock_t *) L->l);
        case 'm':
            free(L);
            return destroy_mcs((mcs_t *) L->l);
        default:
            printf("ERR: Invalid lock type\n");
            free(L);
            return 1;
    }
}

//Create a lock of a specified type
lock_t *new_lock_pool(int size, char type, int n)
{
    if (type == 'n')
    {
        return NULL;
    }

    lock_t *L_pool = (lock_t *) calloc(size, sizeof(lock_t));
    lock_t *L;

    if(!L_pool)
    {
        printf("ERR: Could not allocate space for lock_t pool!\n");
        return NULL;
    }

    for(int i = 0; i < size; i++)
    {
        L = &L_pool[i];
        L->type = type;

        switch(type)
        {
            case 't':
                L->l = (void *) new_tas(n);
                L->init_thread = tas_init;
                L->trylock = tas_trylock;
                L->lock = tas_lock;
                L->unlock = tas_unlock;
                break;
            case 'p':
                L->l = (void *) new_mutex(n);
                L->init_thread = mutex_init;
                L->trylock = mutex_trylock;
                L->lock = mutex_lock;
                L->unlock = mutex_unlock;
                break;
            case 'a':
                L->l = (void *) new_alock(n);
                L->init_thread = alock_init;
                L->trylock = alock_trylock;
                L->lock = alock_lock;
                L->unlock = alock_unlock;
                break;
            case 'm':
                L->l = (void *) new_mcs(n);
                L->init_thread = mcs_init;
                L->trylock = mcs_trylock;
                L->lock = mcs_lock;
                L->unlock = mcs_unlock;
                break;
            default:
                printf("ERR: Invalid lock type\n");
                free(L);
                return NULL;
        }
    }
    return L_pool;
}
//Destroy a lock of a specified type
int destroy_lock_pool(int size, lock_t *L_pool)
{
    if(!L_pool)
    {
        return 1;
    }

    lock_t *L;

    for(int i = 0; i < size; i++)
    {
        L = &L_pool[i];
        switch(L->type)
        {
            case 't':
                destroy_tas((tas_t *) L->l);
                break;
            case 'p':
                destroy_mutex((pthread_mutex_t *) L->l);
                break;
            case 'a':
                destroy_alock((alock_t *) L->l);
                break;
            case 'm':
                destroy_mcs((mcs_t *) L->l);
                break;
            default:
                printf("ERR: Invalid lock type\n");
                free(L_pool);
                return 1;
        }
    }
    free(L_pool);
    return 0;
}

tas_t *new_tas(int n)
{
    UNUSED(n);
    tas_t *lock = (tas_t *) malloc(sizeof(tas_t));
    lock->state = false;
    return lock;
}

int destroy_tas(tas_t *lock)
{
    free(lock);
    return 0;
}

void tas_init(void *lock)
{
    UNUSED(lock);
    return;
}

int tas_trylock(void *lock)
{
    tas_lock(lock);
    return 1;
}

void tas_lock(void *lock)
{
    while(__atomic_test_and_set(&((tas_t *) lock)->state, __ATOMIC_SEQ_CST)){}
    return;
}

void tas_unlock(void *lock)
{
    __atomic_clear(&((tas_t *) lock)->state, __ATOMIC_SEQ_CST);
    return;
}

pthread_mutex_t *new_mutex(int n)
{
    UNUSED(n);
    pthread_mutex_t *lock = (pthread_mutex_t *) malloc(sizeof(pthread_mutex_t));
    pthread_mutex_init(lock, NULL);
    return lock;
}

int destroy_mutex(pthread_mutex_t *lock)
{
    pthread_mutex_destroy((pthread_mutex_t *) lock);
    return 0;
}

void mutex_init(void *lock)
{
    UNUSED(lock);
    return;
}

int mutex_trylock(void *lock)
{
    //If the lock gets acquired, the function retutns 0
    return (pthread_mutex_trylock((pthread_mutex_t *) lock) == 0);
}

void mutex_lock(void *L)
{
    pthread_mutex_lock((pthread_mutex_t *) L);
    return;
}

void mutex_unlock(void *L)
{
    pthread_mutex_unlock((pthread_mutex_t *) L);
    return;
}

#define PAD 64

alock_t *new_alock(int n)
{
    alock_t *lock = (alock_t *) malloc(sizeof(alock_t));
    volatile bool *flag = (bool *) calloc(n * PAD, sizeof(bool));
    pthread_key_create(&lock->myIndex, NULL);
    lock->flag = flag;
    flag[0] = true;
    lock->tail = 0;
    lock->size = n;
    return lock;
}


int destroy_alock(alock_t *lock)
{
    pthread_key_delete(lock->myIndex);
    free((void*) lock->flag);
    free(lock);
    return 0;
}

void alock_init(void *lock)
{
    UNUSED(lock);
    return;
}

int alock_trylock(void *L)
{
    int last_slot = (int) __atomic_load_n(&((alock_t *) L)->tail,  __ATOMIC_SEQ_CST) % ((alock_t *) L)->size;
    if (((alock_t *) L)->flag[last_slot * PAD])
    {
        alock_lock(L);
        return 1;
    }
    return 0;
}

void alock_lock(void *L)
{
    uintptr_t slot = (uintptr_t) ((int) __sync_fetch_and_add(&((alock_t *) L)->tail, 1)) % ((alock_t *) L)->size;
    pthread_setspecific(((alock_t *) L)->myIndex, (void *) slot);
    while (!((alock_t *) L)->flag[slot * PAD]){}
    return;
}

void alock_unlock(void *L)
{
    int slot = (int) ((uintptr_t) pthread_getspecific(((alock_t *) L)->myIndex));
    ((alock_t *) L)->flag[slot * PAD] = false;
    int index = (slot + 1) % ((alock_t *) L)->size;
    ((alock_t *) L)->flag[index * PAD] = true;
    return;
}

mcs_t *new_mcs(int n)
{
    //printf("Initializing lock\n");
    UNUSED(n);
    mcs_t *lock = (mcs_t *) malloc(sizeof(mcs_t));
    lock->tail = NULL;
    //pthread_key_create(&lock->thread_init, NULL);
    pthread_key_create(&lock->myNode, free);
    //printf("Initialized lock\n");
    return lock;
}

int destroy_mcs(mcs_t *lock)
{
    pthread_key_delete(lock->myNode);
    free(lock);
    return 0;
}

void mcs_init(void *lock)
{
    //printf("thread-%ld: Creating Initial myNode...\n", pthread_self());
    qnode_t *ptr = (qnode_t *) malloc(sizeof(qnode_t));
    ptr->locked = false;
    ptr->next = NULL;
    //printf("thread-%ld: Created myNode = %p\n", pthread_self(), ptr);
    pthread_setspecific(((mcs_t *) lock)->myNode, (void *) ptr);
    //printf("thread-%ld: Initialized myNode = %p\n", pthread_self(), ptr);
    return;
}

int mcs_trylock(void *lock)
{
    mcs_lock(lock);
    return 1;
}

void mcs_lock(void *L)
{
    //printf("thread-%ld: Locking up...\n", pthread_self());
    qnode_t *qnode = (qnode_t *) pthread_getspecific(((mcs_t *) L)->myNode);
    //printf("thread-%ld: Got myNode = %p\n", pthread_self(), qnode);

    qnode_t *pred = (qnode_t *) __atomic_exchange_n(&((mcs_t *) L)->tail, qnode, __ATOMIC_SEQ_CST);
    //printf("thread-%ld: Got pred = %p\n", pthread_self(), pred);
    if (pred)
    {
        __atomic_store_n(&qnode->locked, true, __ATOMIC_SEQ_CST);
        __atomic_store_n(&pred->next, qnode, __ATOMIC_SEQ_CST);

        // wait until predecessor gives up the lock
        while (__atomic_load_n(&qnode->locked,  __ATOMIC_SEQ_CST)) {}
    }

    //__atomic_store_n(&qnode->locked, false, __ATOMIC_SEQ_CST);
    return;
}


void mcs_unlock(void *L)
{
    //printf("thread-%ld: Unlocking...\n", pthread_self());
    qnode_t *qnode = (qnode_t *) pthread_getspecific(((mcs_t *) L)->myNode);
    //printf("thread-%ld: myNode = %p\n", pthread_self(), qnode);

    if (__atomic_load_n(&qnode->next,  __ATOMIC_SEQ_CST) == NULL)
    {
        //printf("thread-%ld: My tail = %p\n", pthread_self(), ((mcs_t *) L)->tail);
        if (__sync_bool_compare_and_swap(&((mcs_t *) L)->tail, qnode, NULL))
        {
            //printf("thread-%ld: I am the tail!\n", pthread_self());

            return;
        }


        // wait until predecessor fills in its next field
        while (__atomic_load_n(&qnode->next,  __ATOMIC_SEQ_CST) == NULL) {}
    }

    __atomic_store_n(&qnode->next->locked, false, __ATOMIC_SEQ_CST);
    __atomic_store_n(&qnode->next, NULL , __ATOMIC_SEQ_CST);
    return;
}
