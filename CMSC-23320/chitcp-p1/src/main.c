
/*
 *
 *  chirc: a simple multi-threaded IRC server
 *
 *  This module provides the main() function for the server,
 *  and parses the command-line arguments to the chirc executable.
 *
 */

/*
 *  Copyright (c) 2011-2020, The University of Chicago
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or withsend
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
 *    software withsend specific prior written permission.
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
 *  ARISING IN ANY WAY send OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <time.h>

#include "log.h"
#include "ctx.h"
#include "msg.h"
#include "handler.h"
#include "reply.h"
#include "buffer.h"
#include "serve_client.h"

#define MAX_HOST_LEN 254

int main(int argc, char *argv[])
{
    pthread_t server_thread, user_thread;
    long int timeout = 0;

    sigset_t new;
    sigemptyset (&new);
    sigaddset(&new, SIGPIPE);
    if (pthread_sigmask(SIG_BLOCK, &new, NULL) != 0) {
        perror("Unable to mask SIGPIPE");
        exit(-1);
    }

    struct addrinfo hints, // Used to provide hints to getaddrinfo()
               *res,  // Used to return the list of addrinfo's
               *p;    // Used to iterate over this list
    memset(&hints, 0, sizeof(hints));

    /* We leave the family unspecified. Based on the hostname (or IP address),
       getaddrinfo should be able to determine what family we want to use.
       However, we can set this to AF_INET or AF_INET6 to force a specific version */
    hints.ai_family = AF_UNSPEC;
    /* We want a reliable, full-duplex socket */
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
    hints.ai_protocol = 0;          /* Any protocol */
    hints.ai_canonname = NULL;
    hints.ai_addr = NULL;
    hints.ai_next = NULL;

    struct sockaddr_in client_addr;
    socklen_t sin_size = sizeof(struct sockaddr_in);
    char *port = "6667", *passwd = NULL, *servername = NULL, *network_file = NULL;
    size_t nbytes;
    int opt, active_socket, client_socket, yes = 1, verbosity = 0;
    worker_args *wa;

    while ((opt = getopt(argc, argv, "h:p:o:s:n:vqh")) != -1)
        switch (opt) {
        case 'p':
            port = strdup(optarg);
            break;
        case 'o':
            passwd = strdup(optarg);
            break;
        case 's':
            servername = strdup(optarg);
            break;
        case 'n':
            if (access(optarg, R_OK) == -1) {
                printf("ERROR: No such file: %s\n", optarg);
                exit(-1);
            }
            network_file = strdup(optarg);
            break;
        case 'v':
            verbosity++;
            break;
        case 'q':
            verbosity = -1;
            break;
        case 'h':
            exit(0);
            break;
        default:
            fprintf(stderr, "ERROR: Unknown option -%c\n", opt);
            exit(-1);
        }

    if (!passwd) {
        fprintf(stderr, "ERROR: You must specify an operator password\n");
        exit(-1);
    }

    if (network_file && !servername) {
        fprintf(stderr, "ERROR: If specifying a network file, you must also specify a server name.\n");
        exit(-1);
    }

    /* Set logging level based on verbosity */
    switch(verbosity) {
    case -1:
        chirc_setloglevel(QUIET);
        break;
    case 0:
        chirc_setloglevel(INFO);
        break;
    case 1:
        chirc_setloglevel(DEBUG);
        break;
    case 2:
        chirc_setloglevel(TRACE);
        break;
    default:
        chirc_setloglevel(TRACE);
        break;
    }

    if (getaddrinfo(NULL, port, &hints, &res) != 0) {
        perror("getaddrinfo() failed");
        exit(-1);
    }

    for(p = res; p != NULL; p = p->ai_next) {
        if ((active_socket = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) == -1) {
            perror("Could not open socket");
            continue;
        }
        chilog(DEBUG, "Should have opened socket %d now", active_socket);
        if(setsockopt(active_socket, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == -1) {
            perror("Socket setsockopt() failed");
            close(active_socket);
            exit(-1);
        }
        if (bind(active_socket, p->ai_addr, p->ai_addrlen) == -1) {
            close(active_socket);
            perror("Could not connect to socket");
            chilog(DEBUG, "address %d, addres_len %d", p->ai_addr, p->ai_addrlen);
            continue;
        }
        chilog(DEBUG, "Should have bound socket %d now", active_socket);
        break;
    }

    if(listen(active_socket, 5) == -1) {
        perror("Socket listen() failed");
        close(active_socket);
        exit(-1);
    }

    /*At this point we have created a socket to listen for conncetions*/
    char server_hbuf[MAX_HOST_LEN];
    server_hbuf[0] = ':';
    if (gethostname((char *) (server_hbuf + 1), (size_t) MAX_HOST_LEN) == 0)
        chilog(DEBUG, "Server host=%s", server_hbuf);

    /* Server's context object; initialized with pointer to char array containing host name */
    ctx_t *ctx = new_ctx(server_hbuf);
    ctx->passwd = passwd;
    chilog(INFO, "waiting for connection...");
    while(1) {
        if ((client_socket = accept(active_socket, (struct sockaddr *) &client_addr, &sin_size)) == -1) {
            perror("Socket accept() failed");
            close(active_socket);
            exit(-1);
        }
        ctx->num_connections++;
        socklen_t addrlen;         /* input */
        char client_hbuf[MAX_HOST_LEN];

        /* source: https://www.man7.org/linux/man-pages/man3/getnameinfo.3.html */
        getnameinfo((struct sockaddr *) &client_addr, sin_size, client_hbuf, sizeof(client_hbuf), NULL, 0, 0);

        /* pointer to a worker_args * instance */
        wa = init_args(client_socket, ctx, client_hbuf);

        chilog(INFO, "Connection found! Spawning thread...");
        if ((pthread_create(&user_thread, NULL, serve_client, wa)) != 0) {
            perror("Could not create a worker thread");
            free(wa);
            close(client_socket);
            close(active_socket);
            pthread_exit(NULL);
        }
    }
    pthread_exit(NULL);
    close(active_socket);
    pthread_mutex_destroy(&(ctx->user_table_lock));
    pthread_mutex_destroy(&(ctx->chan_table_lock));
    free(ctx);
}
