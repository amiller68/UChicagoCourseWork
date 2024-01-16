#ifndef CTX_H_
#define CTX_H_

#include "uthash.h"
#include <stdbool.h>
#include <pthread.h>

#define EMPTY_FIELD "*"

typedef struct struct_user_t user_t;
typedef struct struct_chan_t chan_t;

#define MAX_BUFF_LEN 512

//source: https://troydhanson.github.io/uthash/userguide.html
struct struct_user_t {
    char nick[MAX_BUFF_LEN];            /* Key */
    char uname[MAX_BUFF_LEN];
    char real_name[MAX_BUFF_LEN];
    char host[MAX_BUFF_LEN];

    bool is_irc_operator;
    bool sent_message;
    bool is_registered;
    bool is_quit;

    pthread_mutex_t chan_table_lock;
    chan_t *chan_table;

    int socket;
    UT_hash_handle hh;
};

struct struct_chan_t {
    char name[512];
    pthread_mutex_t user_table_lock;
    user_t *user_table;
    user_t *oper_table;
    UT_hash_handle hh;
};

enum worker_thread_state {
    STARTING,    /* The thread is starting */
    USER_CREATED,  /* The thread has initialized a user */
    THREAD_ERROR        /* An error has occurred when starting the thread */
};

typedef struct {
    //Find IRC spec
    char server_host[MAX_BUFF_LEN];
    char *passwd;
    unsigned int num_operators;
    unsigned int num_connections;

    enum worker_thread_state latest_thread_state;

    user_t *dumby_user;
    chan_t *dumby_chan;

    pthread_mutex_t user_table_lock;
    pthread_mutex_t chan_table_lock;

    user_t *user_table;
    chan_t *chan_table;
} ctx_t;

/*
 * new_user - Creates a new user in memory. This user must still be associated with the
 * user table inside of our context object.
 *
 * hostname - the name of the host running the client
 *
 * socket - the socket file descriptor being used to communicate between the client
 * and the server
 *
 * dumby-chan - a pointer to the dummy channel we depend on for functionality
 *
 * returns - a pointer to the new user with the given hostname
 */
user_t *new_user(char *hostname, int socket);


/*
 * new_chan - Creates a new channel on the server. This channel must still be
 * associated with the channel table inside of our context object.
 *
 * chan_name - the name this channel will receive during creation
 *
 * returns - a pointer to the new channel with the given channel name
 */
chan_t *new_chan(char *chan_name);

/*
 * new_ctx - Creates a new context object on the server.
 *
 * server_host - a pointer to the name of the host machine running the server process
 *
 * returns - a pointer to the new context object with the given hostname
 */
ctx_t *new_ctx(char *server_host);

/*
 * find_user_ctx - Search for user server-wide via nick.
 * Returns a pointer to the user if they exist.
 *
 * nick - a pointer to the nickname to search the context object for
 *
 * ctx - a pointer to the context object
 *
 * returns - a pointer to the user if found, NULL otherwise
*/
user_t *find_user_ctx(char *nick, ctx_t *ctx);

/*
 * find_chan_ctx - Search for channel server-wide via the channel name.
 * Returns a pointer to the channel if it exists.
 *
 * chan_name - a pointer to the name of the channel to search
 *
 * ctx - a pointer to the context object
 *
 * returns - a pointer to the channel if found, NULL otherwise
*/
chan_t *find_chan_ctx(char *chan_name, ctx_t *ctx);

/*
 * user_in_chan - Search for a user in a given channel.
 *
 * user - a pointer to the user to search in the channel
 * chan - a pointer to the channel to search a user for
 * returns - true if they are members of the channel, false otherwise.
*/
bool user_in_chan(user_t *user, chan_t *chan);

/*
 * add_user_ctx - Associates a user with a given context object. This is required to track the
 * number of users on the server and other global info.
 *
 * user - a pointer to the user to add to the context object
 * ctx - a pointer to the context object
 * returns - nothing
*/
void add_user_ctx(user_t *user, ctx_t *ctx);

/*
 * add_chan_ctx - Associates a channel with a given context object. This is required to track the
 * number of channel on the server and other global info.
 *
 * chan - a pointer to the channel to add to the context object
 * ctx - a pointer to the context object
 * returns - nothing
*/
void add_chan_ctx(chan_t *chan, ctx_t *ctx);

/*
 * del_user_ctx - Removes a user from a given context object.
 * This is the inverse of add_user_ctx.
 *
 * user - a pointer to the user to delete from the context object
 * ctx - a pointer to the context object
 * returns - nothing
*/
void del_user_ctx(user_t *user, ctx_t *ctx);

/*
 * del_chan_ctx - Removes a channel from a given context object.
 * This is the inverse of add_chan_ctx.
 *
 * chan - a pointer to the channel to delete from the context object
 * ctx - a pointer to the context object
 * returns - nothing
*/
void del_chan_ctx(chan_t *chan, ctx_t *ctx);

/*
 * edit_user_ctx - Searches the given context object for a user in their user table.
 * If the user exists, edit the user with the new nick provided.
 *
 * new_nick - a pointer to the nickname to update the user with
 * user - a pointer to the user to update
 * ctx - a pointer to the context object
 * returns - nothing
*/
void edit_user_ctx(char *new_nick, user_t *user, ctx_t *ctx);

/*
 * assoc_user_chan - Makes a user a member of a given channel. This is required to track the
 * number of users on the channel and other global channel info.
 *
 * user - a pointer to the user to associate with the channel
 * chan - a pointer to the channel to associate with the user
 * returns - nothing
*/
void assoc_user_chan(user_t *user, chan_t *chan);

/*
 * assoc_user_oper - Makes a user an operator of a given channel. This is required to track the
 * number of operators on the channel and other global channel info.
 *
 * user - a pointer to the user becoming an operator in the channel
 * chan - a pointer to the channel in which the user is becoming an operator
 * ctx - a pointer to the context object
 * returns - nothing
*/
void assoc_user_oper(user_t *user, chan_t *chan, ctx_t *ctx);

/*
 * remov_user_chan - Removes the user from the given channel.
 *
 * user - a pointer to the user to remove from the channel
 * chan - a pointer to the channel to remove the user from
 * ctx - a pointer to the context object
 * returns - nothing
*/
void remov_user_chan(user_t *user, chan_t *chan, ctx_t *ctx);

/*
 * remov_user_oper - Removes the user from the given channel's list of operators.
 *
 * user - a pointer to the user losing operator status
 * chan - a pointer to the channel losing an operator user
 * ctx - a pointer to the context object
 * returns - nothing
*/
void remov_user_oper(user_t *user, chan_t *chan, ctx_t *ctx);

/*
 * print_users - Prints all users currently associated with the context object with ChiLog.
 *
 * ctx - a pointer to the context object
 * returns - nothing
*/
void print_users(ctx_t *ctx);


/*
 * print_chanss - Prints all users currently associated with the context object with ChiLog.
 *
 * ctx - a pointer to the context object
 * returns - nothing
*/
void print_chans(ctx_t *ctx);

#endif
