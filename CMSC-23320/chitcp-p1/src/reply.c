#include "reply.h"
#include <stdio.h>

static reply_t RPL_LOOKUP[] = {
    {RPL_WELCOME, ":Welcome to the Internet Relay Network %s!%s@%s\0"},
    {ERR_NICKNAMEINUSE, "%s :Nickname is already in use\0"},
    {RPL_YOURHOST, ":Your host is %s, running version project.1b.alpha\0"},
    {RPL_CREATED,  ":This server was created January 2021\0"},
    {RPL_MYINFO,   "Alex-and-Neil's-ChiRC-Server project.1b.alpha ao motv\0"},
    {RPL_AWAY,  "%s :%s"},
    {RPL_LUSERCLIENT,  ":There are %d users and %d services on %d servers\0"},
    {RPL_LUSEROP,  "%d :operator(s) online"},
    {RPL_LUSERUNKNOWN,  "%d :unknown connection(s)"},
    {RPL_LUSERCHANNELS,  "%d :channels formed"},
    {RPL_LUSERME,  ":I have %d clients and %d servers"},
    {RPL_MOTDSTART,  ":- %s Message of the day - "},
    {RPL_MOTD,  ":- A message a day keeps the failed tests away"},
    {RPL_ENDOFMOTD,  ":End of MOTD command"},
    {RPL_WHOISUSER,  "%s %s %s * :%s"},
    {RPL_WHOISSERVER,  "%s %s :%s"},
    {RPL_WHOISOPERATOR,  "%s :%s"},
    {RPL_WHOISCHANNELS,  "%s :*( ( '@' / '+' ) %s ' ' )"},
    {RPL_ENDOFWHOIS,  "%s :End of WHOIS list"},
    {RPL_NAMEREPLY,  "= %s :doctor and donna 4ever"},
    {RPL_ENDOFNAMES,  "%s :End of NAMES list"},
    {RPL_LIST,  "%s %d :%s"},
    {RPL_LISTEND,  ":End of LIST"},
    {RPL_WHOREPLY,  "%s %s %s %s %s <H|G>[*][@|+] :%s %s"},
    {RPL_UMODEIS,  "%s %s"},
    {RPL_YOUREOPER,  ":You are now an IRC operator"},

    {ERR_ALREADYREGISTRED, ":Unauthorized command (already registered)"},
    {ERR_NOTREGISTERED, ":You have not registered"},
    {ERR_NEEDMOREPARAMS, "%s :Not enough parameters"},
    {ERR_NONICKNAMEGIVEN, ":No nickname given"},
    {ERR_UNKNOWNCOMMAND, "%s :Unknown command"},
    {ERR_NORECIPIENT, ":No recipient given (%s)"},
    {ERR_NOTEXTTOSEND, ":No text to send"},
    {ERR_NOSUCHNICK, "%s :No such nick/channel"},
    {ERR_CANNOTSENDTOCHAN, "%s :Cannot send to channel"},
    {ERR_NOSUCHCHANNEL, "%s :No such channel"},
    {ERR_CHANOPRIVSNEEDED, "%s :You're not channel operator"},
    {ERR_UNKNOWNMODE, "%s :%s"},
    {ERR_NOTONCHANNEL, "%s :You're not on that channel"},
    {ERR_NOMOTD, ":MOTD File is missing"},
    {ERR_PASSWDMISMATCH, ":Password incorrect"},
    {ERROR, ":Closing Link: %s (%s)"}
};

//Can be simplified by integrating this function with handle_msg() in msg.c:
msg_t *reply_msg(char *rpl, msg_t *msg, user_t *user, ctx_t *ctx, ... )
{
    chilog(TRACE, "Retrieving reply template: %s", rpl);
    int i;
    reply_t *reply = NULL;
    const char *template;
    char *code, *err_buff, *save, *arg;
    for(i = 0; i < sizeof(RPL_LOOKUP) / sizeof(reply_t); i++) {
        if (strcmp(RPL_LOOKUP[i].code, rpl) == 0) {
            chilog(TRACE, "INDEX: %d", i);
            reply = &RPL_LOOKUP[i];
        }
    }

    if (reply == NULL) {
        return NULL;
    }

    template = reply->template;
    err_buff = msg->err_buff;

    chilog(TRACE, "Constructing appropriate reply: %s", rpl);
    //Clear the msg and add the appropriate prefix and code
    //Add recipient
    if (!user->is_quit) {
        msg = construct_msg(msg, ctx->server_host, rpl);
        msg = add_param(msg, user->nick, false);
    }

    else {
        msg = construct_msg(msg, NULL, rpl);
    }

    msg = add_recipient_user(msg, user);
    va_list arg_list;
    va_start(arg_list, ctx);

    vsnprintf(err_buff, MAX_MSG_LENGTH, template, arg_list);
    va_end(arg_list);

    //source: https://linux.die.net/man/3/strtok_r
    for(save = err_buff; ; err_buff = NULL) {
        if (save[0] == ':') {
            add_param(msg, save, true);
            break;
        }

        //Break off the next parameter (if it exists)
        arg = strtok_r(err_buff, " ", &save);
        if (arg == NULL) {
            break;
        }
        add_param(msg, arg, false);
    }
    return msg;
}