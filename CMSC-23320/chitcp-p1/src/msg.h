//Keeps track of message components
#ifndef MSG_H_
#define MSG_H_

#include <stdbool.h>
#include "ctx.h"

#define MAX_MSG_ARGS 15
#define MAX_MSG_LENGTH 512

//Defintions for correct number of params
#define NICK_VARS 1
#define USER_VARS 3

typedef struct struct_msg_t msg_t;

//Stores pointers to important parts of an IRC message within a buffer
struct struct_msg_t {
    char *pre;                     /* Message prefix, if it exists */
    char *cmd;                     /* The command being executed */
    char *args[MAX_MSG_ARGS];      /* IRC msgs can have at most 15 parameters */
    unsigned int nparams;
    bool long_last;                /* This message is longer than one parameter */
    bool mult_recipients;          /* Is this being sent to one user or several? */
    bool send_to_self;             /* Is this being sent back to the sender? */
    user_t *recipient;
    chan_t *chan_recipient;

    msg_t *next_msg;               /* Basic list linking */

    char err_buff[MAX_MSG_LENGTH]; /* Stores error message data */
    //char misc[MAX_MSG_LENGTH];     /* Stores any still-needed data */
};

/*
 * buffer_to_msg - reads the contents of an IRC message and returns a pointer to
 * a msg_t instance with pointer to the important parameters
 *
 * msg: a pointer to an allocated msg_t
 * buffer: a pointer to a buffer containing an IRC compliant message
 * returns: a pointer to a new msg_t instance
 */
msg_t *buffer_to_msg(msg_t *msg, char *buffer);

/*
 * construct_msg - reads the contents of a message and reads them into a buffer
 *
 * msg: a pointer to a message to read
 * buffer: a pointer to a buffer to write the contructed message into
 * returns: a pointer to the buffer
 */
char *msg_to_buffer(msg_t *msg, char *buffer);

/*
 * Clears a message of all data, allowing it to be overwritten with a new message.
 *
 * msg: a pointer to a message to clear
 * returns: a pointer to the message, empty of all data
*/
msg_t *clear_msg(msg_t *msg);

/*
 * add_param - Adds a parameter to a message being constructed
 *
 * msg: a pointer to a message to read
 * param: a pointer to the parameter to add to the message
 * long_last: a boolean on whether the parameter is longer than one word
 * returns: a pointer to the same message
*/
msg_t *add_param(msg_t *msg, char *param, bool long_last);

/*
 * construct_msg - Constructs the beginning of a message, including its prefix and its
 * command. This is the first step in constructing a message.
 *
 * msg: a pointer to a message to read
 * pre: a pointer to the prefix to add to the message
 * cmd: a pointer to the command to add to the message
*/
msg_t *construct_msg(msg_t *msg, char *pre, char *cmd);

/*
 * add_recipient_user - Adds a user as the recipient of this message, making
 * the server relay the message to that user. Required to route messages.
 *
 * msg: a pointer to a message to read
 * recipient: a pointer to the user receiving this message
*/
msg_t *add_recipient_user(msg_t *msg, user_t *recipient);

/*
 * add_recipient_chan - Adds a channel as the recipient of this message, making
 * the server relay the message to that channel and every member in it.
 * Required to route messages to channels.
 *
 * msg: a pointer to a message to read
 *
 * chan: a pointer to the channel receiving this message
 *
 * send_to_self: a boolean on whether this message should send to oneself if the
 * user is a member of the given channel.
*/
msg_t *add_recipient_chan(msg_t *msg, chan_t *chan, bool send_to_self);

/*
 * add_recipient_server - Sets a server as the recipient of this message, making
 * the server relay the message to the entire server.
 *
 * msg: a pointer to a message to read
 * returns: a pointer to the same message
*/
msg_t *add_recipient_server(msg_t *msg);

/*
 * print_msg - Prints out the message using ChiLog.
 *
 * msg: a pointer to a message to print
 * returns: nothing
*/
void print_msg(msg_t *msg);

#endif
