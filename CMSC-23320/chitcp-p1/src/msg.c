/*Describes how to handle different messages from clients*/
#include "msg.h"
#include "handler.h"
#include "log.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * allocate_msg - allocates an instance of msg_t and populates it with pointers to the correct strings
 *
 * pre: a pointer to the prefix of an IRC message
 *
 * cmd: a pointer to the command of an IRC message
 *
 * args: a pointer to the args of an IRC message
 *
 * msg: a pointer to a msg_t; if NULL, allocate and return a new msg_t; populate msg if otherwise
 * returns: a pointer to the allocated msg_t
 */

msg_t *buffer_to_msg(msg_t *msg, char *buffer)
{
    chilog(TRACE, "Parsing buffer: %p", (void *) buffer);
    char  *pre;
    char *cmd;
    char *arg;
    char *save;
    int i = 0;

    //Are we creating a new message?
    if( msg == NULL) {
        msg_t *new_msg = (msg_t *) calloc((size_t) 1, sizeof(msg_t));
        if (new_msg == NULL) {
            chilog(ERROR, "Allocation Failure");
            exit(-1);
        }
        msg = new_msg;
    }

    if (buffer[0] == ':') {
        msg->pre = strtok_r(buffer, " ", &save );
        msg->cmd = strtok_r(NULL, " ", &save);
    }

    else {
        msg->pre = NULL;
        msg->cmd = strtok_r(buffer, " ", &save);
    }

    chilog(DEBUG, "Created msg { pre : %s, cmd: %s }", msg->pre, msg->cmd);

    //source: https://linux.die.net/man/3/strtok_r
    for(i = 0; ; i++) {
        //Check if the next parameter is the last
        if (save[0] == ':') {
            //buff_in is memset each loop, don't need to worry about placing \0
            arg = save;
            msg->args[i] = arg;
            //chilog(INFO, "arg_%d: %s", i, msg->args[i]);
            i++;
            msg->long_last = true;
            break;
        }

        //Break off the next parameter (if it exists)
        arg = strtok_r(NULL, " ", &save);
        if (arg == NULL) {
            msg->long_last = false;
            break;
        }
        msg->args[i] = arg;
        //chilog(INFO, "arg_%d: %s", i, msg->args[i]);
    }
    msg->nparams = i;
    chilog(DEBUG, "Parsed msg");
    return msg;
}

char *msg_to_buffer(msg_t *msg, char *buffer)
{
    chilog(DEBUG, "Building buffer from msg");
    int nstr = 2 + (msg->nparams);
    char buff_out[MAX_MSG_LENGTH];
    memset(buff_out, '\0', MAX_MSG_LENGTH);
    char *arg = NULL;
    int i;
    int j = 0;

    for(i = 0; i < nstr ; i++ ) {
        chilog(TRACE, "Buff_out: %s", buff_out);
        if (i == 0) {
            if (msg->pre == NULL)
                continue;
            arg = msg->pre;
        } else if (i == 1)
            arg = msg->cmd;
        else
            arg = msg->args[i-2];

        if(i == nstr - 1) {
            sprintf(&buff_out[j], "%s\r\n", arg);
            j += (strlen(&buff_out[j]) + 2);
        }

        else {
            sprintf(&buff_out[j], "%s ", arg);
            j += (strlen(arg) + 1);
        }
    }
    memcpy(buffer, buff_out, j);
    return buffer;
}

void print_msg(msg_t *msg)
{
    chilog(DEBUG, "PRE : %s | cmd: %s", msg->pre, msg->cmd);
    for(int i = 0; i < msg->nparams; i++) {
        chilog(DEBUG, "arg_%d : %s", i, msg->args[i]);
    }
}
//dont need to use buff anymore! lets keep it around if we need to store stuff...need to write it in after the message
//BUt how to figure out where to write it....
msg_t *construct_msg(msg_t *msg, char *pre, char *cmd )
{
    char *arg;

    clear_msg(msg);
    msg->pre = pre;
    msg->cmd = cmd;

    return msg;
}

msg_t *clear_msg(msg_t *msg)
{
    msg->pre = NULL;
    msg->cmd = NULL;
    msg->nparams = 0;
    msg->mult_recipients = false;
    msg->send_to_self = false;
    msg->recipient = NULL;
    msg->chan_recipient = NULL;
    return msg;
}

msg_t *add_pre(msg_t *msg, char *pre)
{
    msg->pre = pre;
    return msg;
}

msg_t *add_cmd(msg_t *msg, char *cmd)
{
    msg->cmd = cmd;
    return msg;
}

msg_t *add_recipient_user(msg_t *msg, user_t *recipient)
{
    msg->recipient = recipient;
    return msg;
}

msg_t *add_recipient_chan(msg_t *msg, chan_t *chan, bool send_to_self)
{
    msg->chan_recipient = chan;
    msg->mult_recipients = true;
    msg->send_to_self = send_to_self;
    return msg;
}

msg_t *add_recipient_server(msg_t *msg)
{
    msg->chan_recipient = NULL;
    msg->mult_recipients = true;
    return msg;
}

msg_t *add_param(msg_t *msg, char *param, bool long_last)
{
    msg->args[msg->nparams] = param;
    msg->nparams++;
    msg->long_last = long_last;
    return msg;
}
