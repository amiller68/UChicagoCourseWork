#ifndef SERVE_CLIENT_H_
#define SERVE_CLIENT_H_

#include "ctx.h"
#include "msg.h"
#include "log.h"
#include "buffer.h"
#include "handler.h"
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <stdio.h>

typedef struct {
    int socket;
    ctx_t *ctx;
    buffer_t *buff_in;
    char* buff_out;
    user_t *user;
    msg_t *msg;
    char *client_hbuf;
    char *server_hbuf;
} worker_args;

/*
 * init_args - initializes a worker_args instance that holds all the pertinent variable for serving a client
 *
 * socket: the client's socket
 *
 * ctx: a pinter to the ctx_t instance that maintains the state of the server
 * client_hbuf: a char array containg the clients resolved host name
 * returns: a pointer to a newly allocated wa instance with the appropriate variables for serving a client
 */
worker_args *init_args(int socket, ctx_t *ctx, char *client_hbuf);

/*
 * serve_client - serves a client so long as they don't close the connection; should be started in another thread
 *
 * args: a pointer to the clients wa instance
 * client_hbuf: a char array containg the clients resolved host name
 * returns: nothing
 */
void *serve_client(void *args);

#endif
