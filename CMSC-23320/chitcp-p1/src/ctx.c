#include <stdlib.h>
#include "ctx.h"
#include "log.h"

void print_users_table(user_t **user_table)
{
    user_t *user;
    for(user=*user_table; user != NULL; user=user->hh.next) {
        chilog(TRACE, "user nick %s: uname:  %s\n host: %s", user->nick, user->uname, user->host);
    }
    chilog(TRACE, "No more users");
}


//user helper commands
user_t *find_user(char *nick, user_t **user_table);
void add_user(user_t *user, user_t **user_table);
void del_user(user_t *user, user_t **user_table);
void edit_user(char *new_nick, user_t *user, user_t **user_table);

//chan helper commands
chan_t *find_chan(char *chan_name, chan_t **chan_table);
void add_chan(chan_t *chan, chan_t **chan_table);
void del_chan(chan_t *chan, chan_t **chan_table);

chan_t *copy_chan(chan_t *chan)
{
    chan_t *new_chan = (chan_t *) calloc(1, sizeof(chan_t));
    strcpy(new_chan->name, chan->name);
    return new_chan;
}

user_t *copy_user(user_t *user)
{
    user_t *new_user = (user_t *) calloc(1, sizeof(user_t));

    strcpy(new_user->nick, user->nick);
    strcpy(new_user->uname, user->uname);
    strcpy(new_user->host, user->host);

    new_user->socket = user->socket;

    return new_user;
}

chan_t *new_chan(char *chan_name)
{
    chan_t *new_chan = (chan_t *) calloc(1, sizeof(chan_t));
    strcpy(new_chan->name, chan_name);
    pthread_mutex_init(&(new_chan->user_table_lock), NULL);
    new_chan->user_table = NULL;
    return new_chan;
}

user_t *new_user(char *hostname, int socket)
{
    user_t *new_user = (user_t *) malloc(sizeof(user_t));
    if (new_user == NULL) {
        chilog(ERROR, "Failed Allocation");
        exit(-1);
    }
    strcpy(new_user->nick, EMPTY_FIELD);
    strcpy(new_user->uname, EMPTY_FIELD);
    strcpy(new_user->host, hostname);

    new_user->is_irc_operator = false;
    new_user->is_registered = false;
    new_user->is_quit = false;

    pthread_mutex_init(&(new_user->chan_table_lock), NULL);
    new_user->chan_table = NULL;

    new_user->socket = socket;
    return new_user;
}


ctx_t *new_ctx(char *server_host)
{
    ctx_t *new_ctx = (ctx_t *) calloc(1, sizeof(ctx_t));
    strcpy(new_ctx->server_host, server_host);

    pthread_mutex_init(&(new_ctx->user_table_lock), NULL);
    pthread_mutex_init(&(new_ctx->chan_table_lock), NULL);

    new_ctx->user_table = NULL;
    new_ctx->chan_table = NULL;

    return new_ctx;
}

user_t *find_user_ctx(char *nick, ctx_t *ctx)
{
    pthread_mutex_lock(&(ctx->user_table_lock));
    user_t *result = NULL;
    user_t *user_table = ctx->user_table;
    result = find_user(nick, &user_table);
    pthread_mutex_unlock(&(ctx->user_table_lock));
    return result;
}

chan_t *find_chan_ctx(char *chan_name, ctx_t *ctx)
{

    pthread_mutex_lock(&(ctx->chan_table_lock));
    chan_t *result = NULL;
    chan_t *chan_table = ctx->chan_table;
    result = find_chan(chan_name, &chan_table);

    pthread_mutex_unlock(&(ctx->chan_table_lock));
    return result;
}

bool user_in_chan(user_t *user, chan_t *chan)
{
    user_t *result = NULL;
    pthread_mutex_lock(&(chan->user_table_lock));
    user_t **user_table = &chan->user_table;
    result = find_user(user->nick, user_table);
    pthread_mutex_unlock(&(chan->user_table_lock));
    if (result == NULL) {
        return false;
    }
    return true;
}

void add_user_ctx(user_t *user, ctx_t *ctx)
{
    pthread_mutex_lock(&(ctx->user_table_lock));
    user_t **user_table = &ctx->user_table;
    HASH_ADD_STR(*user_table, nick, user);
    pthread_mutex_unlock(&(ctx->user_table_lock));
}

void add_chan_ctx(chan_t *chan, ctx_t *ctx)
{
    pthread_mutex_lock(&(ctx->chan_table_lock));
    chan_t **chan_table = &ctx->chan_table;
    HASH_ADD_STR(*chan_table, name, chan);
    pthread_mutex_unlock(&(ctx->chan_table_lock));
}

//Delete a user from the context of the server
void del_user_ctx(user_t *user, ctx_t *ctx)
{
    pthread_mutex_lock(&(ctx->chan_table_lock));
    user_t *result;
    chan_t *chan;
    chan_t *tmp;
    chan_t **chan_table = &(ctx->chan_table);
    user_t **user_table = NULL;
    HASH_ITER(hh, *chan_table, chan, tmp) {
        pthread_mutex_lock(&(chan->user_table_lock));
        chan = find_chan(chan->name, &ctx->chan_table);
        user_table = &chan->user_table;
        del_user(user, user_table);
        pthread_mutex_unlock(&(chan->user_table_lock));
    }
    user_table = &ctx->user_table;
    HASH_DEL(*user_table, user);
    pthread_mutex_unlock(&(ctx->user_table_lock));
    //Free the correct lock and object
    pthread_mutex_destroy(&(user->chan_table_lock));
    free(user);
}

//Del a chan from the context of the server
void del_chan_ctx(chan_t *chan, ctx_t *ctx)
{
    pthread_mutex_lock(&(ctx->user_table_lock));
    chan_t *result;
    user_t *user;
    user_t *tmp;
    user_t **user_table = &(ctx->user_table);
    chan_t **chan_table = NULL;
    HASH_ITER(hh, *user_table, user, tmp) {
        pthread_mutex_lock(&(user->chan_table_lock));
        user = find_user(user->nick, &ctx->user_table);
        chan_table = &user->chan_table;
        del_chan(chan, chan_table);
        pthread_mutex_unlock(&(user->chan_table_lock));
    }
    chan_table = &ctx->chan_table;
    HASH_DEL(*chan_table, chan);
    pthread_mutex_unlock(&(ctx->chan_table_lock));
    //Free the correct lock and object
    pthread_mutex_destroy(&(chan->user_table_lock));
    free(chan);
}

void edit_user_ctx(char *new_nick, user_t *user, ctx_t *ctx)
{
    pthread_mutex_lock(&(ctx->user_table_lock));

    user_t *result;
    chan_t *chan;
    chan_t *tmp;
    chan_t **chan_table = &user->chan_table;
    user_t **user_table;
    HASH_ITER(hh, *chan_table, chan, tmp) {
        chan = find_chan(chan->name, &ctx->chan_table);
        pthread_mutex_lock(&(chan->user_table_lock));
        user_table = &chan->user_table;
        edit_user(new_nick, user, user_table);
        print_users_table(user_table);
        pthread_mutex_unlock(&(chan->user_table_lock));
    }

    user_table = &ctx->user_table;
    HASH_DEL(*user_table, user);
    strcpy(user->nick, new_nick);
    HASH_ADD_STR(*user_table, nick, user);
    chilog(TRACE, "Edited user: %s", user->nick);
    pthread_mutex_unlock(&(ctx->user_table_lock));
}

void assoc_user_oper(user_t *user, chan_t *chan, ctx_t *ctx)
{
    user_t **user_table = &chan->oper_table;
    chan_t **chan_table = &user->chan_table;
    pthread_mutex_lock(&(chan->user_table_lock));
    pthread_mutex_lock(&(user->chan_table_lock));
    add_user(user, user_table);
    add_chan(chan, chan_table);
    ctx->num_operators++;
    pthread_mutex_unlock(&(user->chan_table_lock));
    pthread_mutex_unlock(&(chan->user_table_lock));
}

void assoc_user_chan(user_t *user, chan_t *chan)
{
    user_t **user_table = &chan->user_table;
    chan_t **chan_table = &user->chan_table;
    pthread_mutex_lock(&(chan->user_table_lock));
    pthread_mutex_lock(&(user->chan_table_lock));
    add_user(user, user_table);
    add_chan(chan, chan_table);
    pthread_mutex_unlock(&(user->chan_table_lock));
    pthread_mutex_unlock(&(chan->user_table_lock));

    print_users_table(user_table);
}

void remov_user_oper(user_t *user, chan_t *chan, ctx_t *ctx)
{
    int user_count;
    user_t **user_table = &chan->oper_table;
    chan_t **chan_table = &user->chan_table;

    pthread_mutex_lock(&(chan->user_table_lock));
    pthread_mutex_lock(&(user->chan_table_lock));

    del_user(user, user_table);
    del_chan(chan, chan_table);
    ctx->num_operators--;
    user_count = HASH_COUNT(*user_table);

    if (user_count == 0) {
        pthread_mutex_lock(&(ctx->chan_table_lock));
        chan_table = &ctx->chan_table;
        HASH_DEL(*chan_table, chan);
        pthread_mutex_unlock(&(ctx->chan_table_lock));
    }

    pthread_mutex_unlock(&(user->chan_table_lock));
    pthread_mutex_unlock(&(chan->user_table_lock));

    if (user_count == 0) {
        pthread_mutex_destroy(&(chan->user_table_lock));
        free(chan);
    }
}

void remov_user_chan(user_t *user, chan_t *chan, ctx_t *ctx)
{
    int user_count;
    user_t **user_table = &chan->user_table;
    chan_t **chan_table = &user->chan_table;

    pthread_mutex_lock(&(chan->user_table_lock));
    pthread_mutex_lock(&(user->chan_table_lock));

    del_user(user, user_table);
    del_chan(chan, chan_table);

    user_count = HASH_COUNT(*user_table);

    if (user_count == 0) {
        pthread_mutex_lock(&(ctx->chan_table_lock));
        chan_table = &ctx->chan_table;
        HASH_DEL(*chan_table, chan);
        pthread_mutex_unlock(&(ctx->chan_table_lock));
    }

    pthread_mutex_unlock(&(user->chan_table_lock));
    pthread_mutex_unlock(&(chan->user_table_lock));

    print_users_table(user_table);

    if (user_count == 0) {
        pthread_mutex_destroy(&(chan->user_table_lock));
        free(chan);
    }
}

void print_users(ctx_t *ctx)
{
    user_t *user;
    user_t *user_table = ctx->user_table;
    for(user=user_table; user != NULL; user=user->hh.next) {
        chilog(TRACE, "user nick %s: uname:  %s\n host: %s", user->nick, user->uname, user->host);
    }
    chilog(TRACE, "No more users");
}

void print_chans(ctx_t *ctx)
{
    chan_t *chan;
    chan_t *chan_table = ctx->chan_table;
    for(chan=chan_table; chan != NULL; chan=chan->hh.next) {
        chilog(TRACE, "channel title %s: num_users: %s", chan->name, HASH_COUNT(chan->user_table));
        chilog(TRACE, "next...");
    }
    chilog(TRACE, "No more channels");
}

//user_t function defintions
user_t *find_user(char *nick, user_t **user_table)
{
    user_t *result = NULL;
    HASH_FIND_STR(*user_table, nick, result);
    return result;
}

void add_user(user_t *user, user_t **user_table)
{
    user_t *user_copy = copy_user(user);
    HASH_ADD_STR(*user_table, nick, user_copy);
}


void del_user(user_t *user, user_t **user_table)
{
    user_t *del_target;
    del_target = find_user(user->nick, user_table);
    if (del_target) {
        HASH_DEL(*user_table, del_target);
        free(del_target);
    } else {
        chilog(ERROR, "No such user in table: %s", user->nick);
    }
}

//Rehash a user in a user_table with a new nick
void edit_user(char *new_nick, user_t *user, user_t **user_table)
{
    user_t *edit_target;
    edit_target = find_user(user->nick, user_table);
    if (edit_target) {
        HASH_DEL(*user_table, edit_target);
        strcpy(edit_target->nick, new_nick);
        HASH_ADD_STR(*user_table, nick, edit_target);
    } else {
        chilog(ERROR, "No such user in table: %s", user->nick);
    }
}

//chan_t functios
chan_t *find_chan(char *chan_name, chan_t **chan_table)
{
    chan_t *result = NULL;
    HASH_FIND_STR(*chan_table, chan_name, result);
    return result;
}

void add_chan(chan_t *chan, chan_t **chan_table)
{
    chan_t *chan_copy = copy_chan(chan);
    HASH_ADD_STR(*chan_table, name, chan_copy);
}

void del_chan(chan_t  *chan, chan_t **chan_table)
{
    chan_t *del_target;
    del_target = find_chan(chan->name, chan_table);
    if (del_target) {
        HASH_DEL(*chan_table, del_target);
        free(del_target);
    } else {
        chilog(ERROR, "No such chan in table: %s", chan->name);
    }
}
