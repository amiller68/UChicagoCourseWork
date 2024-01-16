#include "serve_client.h"

msg_t *send_msg(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);

void *serve_client(void* args)
{
    worker_args *wa;
    ctx_t *ctx;
    int socket, i;
    size_t nbytes;
    buffer_t *buff_in;
    char *server_hbuf, *buff_out;
    msg_t *msg, *reply = NULL;
    user_t *user;
    bool error = false;

    wa = (worker_args *) args;
    socket = wa->socket;
    ctx = wa->ctx;
    buff_in = wa->buff_in;
    buff_out = wa->buff_out;
    msg = wa->msg;
    user = wa->user;
    server_hbuf = wa->server_hbuf;

    pthread_detach(pthread_self());

    while(!user->is_quit) {
        chilog(INFO,"Receiving data from socket %d...", socket);
        nbytes = recv(socket, buff_in->next_msg,
                      BUF_SIZE - buff_in->nbytes, 0);
        if (nbytes == -1) {
            continue;
        }
        if(nbytes == 0) {
            ctx->num_connections--;
            continue;
        }
        chilog(INFO,"Received a message from socket %d: %s",socket, buff_in->next_msg);
        buff_in->next_msg += nbytes;
        buff_in->trail += nbytes;
        buff_in->nbytes += nbytes;
        while(pop_from_buffer(buff_in, "\r\n", buff_out, true) > 0) {
            buffer_to_msg(msg, buff_out);
            reply = handle_msg(msg, buff_out, user, ctx);
            if (reply != NULL) {
                chilog(INFO, "Sending reply...");
                //If failed to send a reply, connection is closed
                if ((msg = send_msg(msg, buff_out, user, ctx)) == NULL) {
                    perror("Message failed to send - disconnecting");
                    exit(-1);
                }
                memset(buff_out, '\0', BUF_SIZE);
            }
        }
    }

    //close the connection
    close(socket);
    del_user_ctx(user, ctx);
    free(msg);
    free(buff_in);
    free(buff_out);
}

msg_t *send_msg(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    chilog(DEBUG, "Sending a msg");
    user_t *recipient;
    user_t *tmp;
    chan_t *chan;
    msg_t *head = msg;
    msg_t *next_msg;
    int count;
    while (msg) {
        int count = 0;
        msg_to_buffer(msg, buffer);
        if (msg->mult_recipients) {
            user_t **recipient_table = NULL;
            //If the msg is going out on a channel
            if (msg->chan_recipient != NULL) {
                chan = msg->chan_recipient;
                recipient_table = &chan->user_table;
                chilog(DEBUG,"Sending msg out on chan: %s...", chan->name);
                pthread_mutex_lock(&(chan->user_table_lock));
            }
            //If the msg is going out on the server
            else {
                recipient_table = &ctx->user_table;
                pthread_mutex_lock(&(ctx->user_table_lock));
            }
            //Iterate through the appropriate Hash Table
            HASH_ITER(hh, *recipient_table, recipient, tmp) {
                //Some messages should only reach the channel - the sender
                if ((strcmp(recipient->nick, user->nick) != 0) || msg->send_to_self) {
                    chilog(INFO, "Sending a reply to %s: %s", recipient->nick, buffer);
                    if (send(recipient->socket, buffer, strlen(buffer), 0) < 0) {
                        perror("Socket send() failed");
                        close(recipient->socket);
                        return NULL;
                    }
                    count++;
                }
            }

            if (msg->chan_recipient != NULL) {
                pthread_mutex_unlock(&(chan->user_table_lock));
            }

            else {
                pthread_mutex_unlock(&(ctx->user_table_lock));
            }
        }

        //If theres another recipient
        if ((recipient = msg->recipient) != NULL) {
            chilog(INFO, "Sending a reply to %s: %s",recipient->nick, buffer);
            if (send(recipient->socket, buffer, strlen(buffer), 0) < 0) {
                perror("Socket send() failed");
                close(recipient->socket);
                return NULL;
            }
            count++;
        }

        memset(buffer, '\0', BUF_SIZE);
        msg = msg->next_msg;
    }

    msg = head;
    while(msg) {
        if(msg->next_msg) {
            next_msg = msg->next_msg;
            free(msg);
            msg = next_msg;
        }

        else {
            return msg;
        }
    }

    return NULL;
}

worker_args *init_args(int socket, ctx_t *ctx, char *client_hbuf)
{
    worker_args *wa = (worker_args *) calloc(1, sizeof(worker_args));
    wa->socket = socket;
    wa->ctx = ctx;
    wa->server_hbuf = ctx->server_host;
    wa->user = new_user(client_hbuf, socket);
    wa->buff_in = init_buffer();
    wa->buff_out = (char *) calloc(BUF_SIZE, sizeof(char));
    wa->msg = (msg_t *) calloc(1, sizeof(msg_t));
    return wa;
}
