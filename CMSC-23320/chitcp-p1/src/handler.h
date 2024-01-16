#ifndef HANDLER_H_
#define HANDLER_H_

#include "log.h"
#include "ctx.h"
#include "msg.h"

#define NICK     "NICK\0"
#define USER     "USER\0"
#define QUIT     "QUIT\0"
#define JOIN     "JOIN\0"
#define PRIVMSG  "PRIVMSG\0"
#define NOTICE   "NOTICE\0"
#define MOTD     "MOTD\0"
#define LUSERS   "LUSERS\0"
#define WHOIS    "WHOIS\0"
#define PING     "PING\0"
#define PONG     "PONG\0"
#define PART     "PART\0"
#define AWAY     "AWAY\0"
#define NAMES    "NAMES\0"
#define LIST     "LIST\0"
#define MODE     "MODE\0"
#define OPER     "OPER\0"

#define MAX_CMD_SIZE 10

typedef struct {
    char cmd[MAX_CMD_SIZE];
    msg_t *(*handler) (msg_t *, char *, user_t *, ctx_t *); /* pointer to a handler */
} handler_t;

/*
 * handle_msg - updates the server state in response to a message and possibly returns a pointer to a reply
 *
 * msg: a pointer to a msg instance to handle
 * User: a pointer to a user_t instance that describes the clients
 * user_table: a hashtable of connected user
 * buffer: The buffer containing the IRC message to handle
 * server_hbuf: A buffer containing the host name of the server
 * returns: NULL if the msg requires no reply; a pointer to buffer with the correct reply copied inside
 */
msg_t *handle_msg(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);

#endif
