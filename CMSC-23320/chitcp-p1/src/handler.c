#include <stdio.h>
#include "handler.h"
#include "reply.h"
#include "buffer.h"
#include "uthash.h"

/* We forward declare necessary handlers */
msg_t *handle_nick(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_user(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_join(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_quit(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_privmsg(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_notice(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_lusers_helper(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx, bool welcome);
msg_t *handle_lusers(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_whois(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_ping(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_part(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_list(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_mode(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);
msg_t *handle_oper(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx);

static handler_t HANDLER_LOOKUP[] = {
    {NICK, &handle_nick},
    {USER, &handle_user},
    {JOIN, &handle_join},
    {QUIT, &handle_quit},
    {PRIVMSG, &handle_privmsg},
    {NOTICE, &handle_notice},
    {LUSERS, &handle_lusers},
    {WHOIS, &handle_whois},
    {PING, &handle_ping},
    {PART, &handle_part},
    {LIST, &handle_list},
    {MODE, &handle_mode},
    {OPER, &handle_oper}
    /*
    ...
    (CMD, &handle_cmd)
    ...
    */
};

#define test_update1b "userfoo\0"

msg_t *make_new_msg(msg_t *msg)
{
    msg_t *next_msg = (msg_t *) calloc(1,sizeof(msg_t));
    msg->next_msg = next_msg;
    return next_msg;
}

msg_t *handle_msg(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    msg_t *reply = NULL;
    char buff_out[MAX_MSG_LENGTH];
    msg_t *(*handler)(msg_t *, char *, user_t *, ctx_t *) = NULL;
    for(int i = 0; i < sizeof(HANDLER_LOOKUP) / sizeof(handler_t); i++) {
        if (strncmp(HANDLER_LOOKUP[i].cmd, msg->cmd, MAX_CMD_SIZE) == 0) {
            handler = HANDLER_LOOKUP[i].handler;
        }
    }
    if (handler) {
        if (user->is_registered || (strcmp(USER, msg->cmd) == 0) || (strcmp(NICK, msg->cmd) == 0))
            reply = handler(msg, buffer, user, ctx);
        else {
            reply = reply_msg(ERR_NOTREGISTERED, msg, user, ctx, msg->cmd);
        }
    } else {
        if (strncmp("PONG\0", msg->cmd, MAX_CMD_SIZE) != 0) {
            if (user->is_registered)
                reply = reply_msg(ERR_UNKNOWNCOMMAND, msg, user, ctx, msg->cmd);
        }
    }

    return reply;
}

msg_t *handle_nick(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    user->sent_message = true;
    chilog(TRACE, "Handling NICK");
    char *nick = msg->args[0];

    msg_t *head;
    bool first_time = true;
    //If there are not the correct number of params
    if (msg->nparams != 1) {
        chilog(TRACE,"no nick given");
        msg = reply_msg(ERR_NONICKNAMEGIVEN, msg, user, ctx);
        return msg;
    }

    //Search ctx for a user with the corresponding nick
    if (find_user_ctx(nick, ctx) != NULL) {
        msg = reply_msg(ERR_NICKNAMEINUSE, msg, user, ctx, nick);
        return msg;
    }

    //Is user->nick unspecified
    if (strcmp(user->nick, EMPTY_FIELD) == 0) {
        //Associate nick with user
        strcpy(user->nick, nick);
        //Associate the user with active users in the server
        add_user_ctx(user, ctx);
    } else if (user->is_registered) {
        chilog(INFO, "User was already registered");
        for (chan_t *c = user->chan_table; c != NULL; c = c->hh.next) {
            print_users(ctx);
            char *old_prefix = msg->err_buff;
            sprintf(old_prefix, PREFIX_FORMAT, user->nick, user->uname, user->host);
            char *new_nick_param = old_prefix + strlen(old_prefix) + 1;
            sprintf(new_nick_param, ":%s", nick);
            msg = construct_msg(msg, old_prefix, NICK);
            if (first_time)  {
                head = msg;
                first_time = false;
            }

            msg = add_param(msg, new_nick_param, true);
            msg = add_recipient_chan(msg, c, true);
            print_msg(msg);
            msg = make_new_msg(msg);
        }
        return head;
    }

    else {
        edit_user_ctx(nick, user, ctx);
    }

    //If user has specified a uname
    if ((strcmp(user->uname, EMPTY_FIELD) != 0) && !user->is_registered) {
        user->is_registered = true;
        msg_t *head = msg = reply_msg(RPL_WELCOME, msg, user, ctx, user->nick, user->uname, user->host);
        msg = reply_msg(RPL_YOURHOST, make_new_msg(msg), user, ctx, user->host);
        msg = reply_msg(RPL_CREATED,  make_new_msg(msg), user, ctx);
        msg = reply_msg(RPL_MYINFO,  make_new_msg(msg), user, ctx);
        msg = handle_lusers_helper(make_new_msg(msg), buffer, user, ctx, true);
        msg = reply_msg(ERR_NOMOTD,  make_new_msg(msg), user, ctx);
        return head;
    }

    return NULL;
}

msg_t *handle_user(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    user->sent_message = true;
    char *uname = msg->args[0];
    char *real_name = msg->args[3];
    chilog(TRACE, "Handling USER: nick = %s, username = %s, real name = %s", user->nick, uname, real_name);
    if (msg->nparams != 4) {
        msg = reply_msg(ERR_NEEDMOREPARAMS, msg, user, ctx, buffer);
        return msg;
    }

    if (user->is_registered) {
        msg = reply_msg(ERR_ALREADYREGISTRED, msg, user, ctx);
        return msg;
    }

    //copy the specified uname into user
    strcpy(user->uname, uname);
    strcpy(user->real_name, real_name + 1);
    //User not associated in ctx therefore user hasnt called a successful NICK
    if(find_user_ctx(user->nick, ctx) == NULL) {
        return NULL;
    }
    //construct and send a rpl welcome
    user->is_registered = true;
    msg_t *head = msg = reply_msg(RPL_WELCOME, msg, user, ctx, user->nick, user->uname, user->host);
    msg = reply_msg(RPL_YOURHOST, make_new_msg(msg), user, ctx, user->host);
    msg = reply_msg(RPL_CREATED,  make_new_msg(msg), user, ctx);
    msg = reply_msg(RPL_MYINFO,  make_new_msg(msg), user, ctx);
    msg = handle_lusers_helper(make_new_msg(msg), buffer, user, ctx, true);
    msg = reply_msg(ERR_NOMOTD,  make_new_msg(msg), user, ctx);
    return head;
}

msg_t *handle_quit(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    user->is_quit = true;
    char *quit_msg;
    if(msg->nparams == 0) {
        //The default quit message
        quit_msg = DEF_QUIT;
    } else {
        quit_msg = msg->args[0];
        quit_msg++;
    }

    chan_t *chan, *tmp;
    chan_t **chan_table = &user->chan_table;

    msg = reply_msg(ERROR, msg, user, ctx, user->host, quit_msg);
    msg_t *head = msg;

    for (chan_t *c = ctx->chan_table; c != NULL; c = c->hh.next) {
        msg = make_new_msg(msg);
        char *prefix = msg->err_buff;
        sprintf(prefix, PREFIX_FORMAT, user->nick, user->uname, user->host);

        char *user_quit_msg = prefix + strlen(prefix) + 1;
        sprintf(user_quit_msg, ":%s", quit_msg);

        msg = construct_msg(msg, prefix, QUIT);
        msg = add_param(msg, user_quit_msg, true);
        print_msg(msg);
        msg = add_recipient_chan(msg, c, false);
    }
    return head;
}

msg_t *handle_join(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    char *chan_name = msg->args[0];
    chan_t *chan;
    if (msg->nparams != 1) {
        msg = reply_msg(ERR_NEEDMOREPARAMS, msg, user, ctx, buffer);
        return msg;
    }

    if((chan = find_chan_ctx(chan_name, ctx)) == NULL) {
        chan = new_chan(chan_name);
        add_chan_ctx(chan, ctx);
        user->is_irc_operator = true;
    }

    else if (user_in_chan(user, chan)) {
        return NULL;
    }

    assoc_user_chan(user, chan);
    sprintf(msg->err_buff, PREFIX_FORMAT, user->nick, user->uname, user->host);
    chilog(TRACE,"msg->err_buff in PRIVMSG is %s", msg->err_buff);
    //Declare a new head
    msg_t *head = construct_msg(msg, msg->err_buff, JOIN);
    msg = add_param(msg, chan_name, false);
    msg = add_recipient_chan(msg, chan, true);
    msg = reply_msg(RPL_NAMEREPLY, make_new_msg(msg), user, ctx, chan_name);
    msg = reply_msg(RPL_ENDOFNAMES, make_new_msg(msg), user, ctx, chan_name);
    return head;
}

msg_t *handle_privmsg(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    char *target = msg->args[0];
    char *text = msg->args[1];
    user_t *recipient = NULL;
    chan_t *recipient_chan = NULL;
    if (msg->nparams != 2) {
        if(msg->nparams  == 1) {
            if (msg->long_last) {
                msg = reply_msg(ERR_NORECIPIENT, msg, user, ctx, msg->cmd);
                return msg;
            } else {
                msg = reply_msg(ERR_NOTEXTTOSEND, msg, user, ctx);
                return msg;
            }
        }

        else {
            msg = reply_msg(ERR_NORECIPIENT, msg, user, ctx, msg->cmd);
            return msg;
        }
    }

    else if ((recipient = find_user_ctx(target, ctx)) == NULL) {
        chilog(DEBUG, "FAILED TO FIND A USER");
        if ((recipient_chan = find_chan_ctx(target, ctx)) == NULL) {
            chilog(DEBUG, "FAILED TO FIND A CHANNEL TOO");
            msg = reply_msg(ERR_NOSUCHNICK, msg, user, ctx, target);
            return msg;
        } else if (!user_in_chan(user, recipient_chan)) {
            msg = reply_msg(ERR_CANNOTSENDTOCHAN, msg, user, ctx, target, recipient_chan->name);
            return msg;
        }
    }

    //construct msg resets param counter; re=add them
    sprintf(msg->err_buff, PREFIX_FORMAT, user->nick, user->uname, user->host);
    chilog(TRACE,"msg->err_buff in PRIVMSG is %s", msg->err_buff);
    msg = construct_msg(msg, msg->err_buff, PRIVMSG);
    if (recipient) {
        msg = add_recipient_user(msg, recipient);
    } else if (recipient_chan) {
        msg = add_recipient_chan(msg, recipient_chan, false);
    }
    msg = add_param(msg, target, false);
    msg = add_param(msg, text, true );

    return msg;
}

msg_t *handle_notice(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    char *target_nick = msg->args[0];
    char *text = msg->args[1];
    user_t *recipient = find_user_ctx(target_nick, ctx);
    if (msg->nparams != 2) {
        return NULL;
    } else if ((recipient = find_user_ctx(target_nick, ctx)) == NULL) {
        return NULL;
    }

    else {
        //construct msg resets param counter
        //re-add params
        sprintf(msg->err_buff, PREFIX_FORMAT, user->nick, user->uname, user->host);
        chilog(TRACE,"msg->err_buff in PRIVMSG is %s", msg->err_buff);
        msg = construct_msg(msg, msg->err_buff, NOTICE);
        msg = add_recipient_user(msg, recipient);
        msg = add_param(msg, target_nick, false);
        msg = add_param(msg, text, true );
    }

    return msg;
}

msg_t *handle_lusers_helper(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx, bool welcome)
{
    int num_users = 0;
    int num_clients = 0;
    for (user_t *u = ctx->user_table; u != NULL; u = u->hh.next) {
        if (u->sent_message) {
            num_users++;
        }
    }
    for (user_t *u = ctx->user_table; u != NULL; u = u->hh.next) {
        if (u->is_registered) {
            num_clients++;
        }
    }
    msg_t *head = reply_msg(RPL_LUSERCLIENT, msg, user, ctx, num_clients, 0, 1);
    msg = reply_msg(RPL_LUSEROP, make_new_msg(msg), user, ctx, ctx->num_operators);
    msg = reply_msg(RPL_LUSERUNKNOWN, make_new_msg(msg), user, ctx, ctx->num_connections - num_users);
    msg = reply_msg(RPL_LUSERCHANNELS, make_new_msg(msg), user, ctx, HASH_COUNT(ctx->chan_table));
    msg = reply_msg(RPL_LUSERME, make_new_msg(msg), user, ctx, num_users, 1);
    return welcome ? msg : head;
}

msg_t *handle_lusers(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    return handle_lusers_helper(msg, buffer, user, ctx, false);
}

msg_t *handle_whois(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    char *target_nick = msg->args[0];
    user_t *target_user = NULL;
    if (msg->nparams != 1) {
        return NULL;
    } else if (find_user_ctx(target_nick, ctx) == NULL) {
        msg = reply_msg(ERR_NOSUCHNICK, msg, user, ctx, target_nick);
        return msg;
    }
    HASH_FIND_STR(ctx->user_table, target_nick, target_user);
    /*whoisuser, whoischannels, whoisserver, rplaway, rplwhoisoperator, endofwhois*/
    /*send whoisoperator to user if operator*/
    /*send whoischannels if in a channel*/
    /*Send rplaway if user is away*/
    msg = reply_msg(RPL_WHOISUSER, msg, user, ctx, user->nick, user->uname, user->host, target_user->real_name);
    msg = reply_msg(RPL_WHOISSERVER, make_new_msg(msg), user, ctx, user->nick, user->host, "the only server");
    msg = reply_msg(RPL_ENDOFWHOIS, make_new_msg(msg), user, ctx, user->uname);
    return msg;
}

msg_t *handle_ping(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    msg = construct_msg(msg, NULL, PONG);
    msg = add_param(msg, ctx->server_host, false);
    msg = add_recipient_user(msg, user);
    return msg;
}

msg_t *handle_part(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    char *chan_name = msg->args[0];
    char *message = NULL;
    chan_t *chan;

    if (msg->nparams == 2) {
        message = msg->args[1];
    }

    else if (msg->nparams != 1) {
        msg = reply_msg(ERR_NEEDMOREPARAMS, msg, user, ctx, PART);
        return msg;
    }

    if ((chan = find_chan_ctx(chan_name, ctx)) == NULL) {
        msg = reply_msg(ERR_NOSUCHCHANNEL, msg, user, ctx, chan_name);
        return msg;
    }

    if (!user_in_chan(user, chan)) {
        msg = reply_msg(ERR_NOTONCHANNEL, msg, user, ctx, chan_name);
        return msg;
    }

    sprintf(msg->err_buff, PREFIX_FORMAT, user->nick, user->uname, user->host);
    chilog(TRACE,"msg->err_buff in PRIVMSG is %s", msg->err_buff);
    msg = construct_msg(msg, msg->err_buff, PART);
    msg = add_param(msg, chan_name, false);
    if (message != NULL) {
        msg = add_param(msg, message, true);
    }
    msg = add_recipient_chan(msg, chan, true);
    remov_user_chan(user, chan, ctx);
    msg = add_recipient_user(msg, user);
    return msg;
}

msg_t *handle_list(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    char *chan_name = NULL;
    if (msg->nparams == 1) {
        chan_name = msg->args[0];
    }
    chan_t *chan = (chan_name == NULL) ? ctx->chan_table : find_chan_ctx(chan_name, ctx);

    msg_t *head = msg;
    bool first_pass = true;
    for (chan_t *c = ctx->chan_table; c != NULL; c = c->hh.next) {
        if (first_pass) {
            msg = reply_msg(RPL_LIST, msg, user, ctx, c->name, HASH_COUNT(chan->user_table));
            first_pass = false;
            continue;
        }
        msg = reply_msg(RPL_LIST, make_new_msg(msg), user, ctx, c->name, HASH_COUNT(chan->user_table));
    }

    if (first_pass) {
        msg = reply_msg(RPL_LISTEND, msg, user, ctx);
    } else {
        msg = reply_msg(RPL_LISTEND, make_new_msg(msg), user, ctx);
    }

    return head;
}

msg_t *handle_mode(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    if (msg->nparams != 3) {
        msg = reply_msg(ERR_NEEDMOREPARAMS, msg, user, ctx, buffer);
        return msg;
    }

    if (!user->is_irc_operator) {
        msg = reply_msg(ERR_CHANOPRIVSNEEDED, msg, user, ctx, msg->args[0]);
        return msg;
    }

    char *chan_name = msg->args[0];
    char *mode = msg->args[1];
    char *nick = msg->args[2];
    user_t *user_target = find_user_ctx(nick, ctx);
    chan_t *chan = find_chan_ctx(chan_name, ctx);

    if (strcmp(mode, "-o") == 0 && user_target->is_irc_operator) {
        chilog(DEBUG, "comparing mode to -o PASSED - taking away operator");
        user_target->is_irc_operator = false;
        remov_user_oper(user, chan, ctx);
    }

    else if (strcmp(mode, "+o") == 0 && !user_target->is_irc_operator) {
        chilog(DEBUG, "comparing mode to +o PASSED - making someone an operator");
        user_target->is_irc_operator = true;
        assoc_user_oper(user_target, chan, ctx);
    }

    sprintf(msg->err_buff, PREFIX_FORMAT, user->nick, user->uname, user->host);
    msg = construct_msg(msg, msg->err_buff, MODE);
    msg = add_param(msg, chan_name, false);
    msg = add_param(msg, mode, false);
    msg = add_param(msg, nick, false);
    msg = add_recipient_chan(msg, chan, true);
    return msg;
}

msg_t * handle_oper(msg_t *msg, char *buffer, user_t *user, ctx_t *ctx)
{
    if (msg->nparams < 2) {
        msg = reply_msg(ERR_NEEDMOREPARAMS, msg, user, ctx, buffer);
        return msg;
    }

    char *nick = msg->args[0];
    char *passwd = msg->args[1];

    if (strcmp(passwd,ctx->passwd) != 0) {
        msg = reply_msg(ERR_PASSWDMISMATCH, msg, user, ctx);
        return msg;
    }
    user_t *user_target = find_user_ctx(nick, ctx);
    user_target->is_irc_operator = true;
    msg = reply_msg(RPL_YOUREOPER, msg, user, ctx);

    return msg;
}
