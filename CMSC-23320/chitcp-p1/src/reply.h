/*
 *  chirc: a simple multi-threaded IRC server
 *
 *  Reply codes
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

#ifndef REPLY_H_
#define REPLY_H_

#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include "log.h"
#include "msg.h"
#include "ctx.h"

#define DEF_QUIT                "Client Quit\0"

#define ERROR                   "ERROR\0"
#define RPL_WELCOME             "001\0"
#define RPL_YOURHOST            "002\0"
#define RPL_CREATED             "003\0"
#define RPL_MYINFO              "004\0"

#define RPL_UMODEIS             "MODE\0"
#define RPL_LUSERCLIENT         "251\0"
#define RPL_LUSEROP             "252\0"
#define RPL_LUSERUNKNOWN        "253\0"
#define RPL_LUSERCHANNELS       "254\0"
#define RPL_LUSERME             "255\0"

#define RPL_AWAY                "301\0"
#define RPL_UNAWAY              "305\0"
#define RPL_NOWAWAY             "306\0"

#define RPL_WHOISUSER           "311\0"
#define RPL_WHOISSERVER         "312\0"
#define RPL_WHOISOPERATOR       "313\0"
#define RPL_ENDOFWHO            "315\0"
#define RPL_WHOISIDLE           "317\0"
#define RPL_ENDOFWHOIS          "318\0"
#define RPL_WHOISCHANNELS       "319\0"

#define RPL_LIST                "322\0"
#define RPL_LISTEND             "323\0"
#define RPL_CHANNELMODEIS       "324\0"

#define RPL_NOTOPIC             "331\0"
#define RPL_TOPIC               "332\0"

#define RPL_WHOREPLY            "352\0"
#define RPL_NAMEREPLY           "353\0"
#define RPL_ENDOFNAMES          "366\0"

#define RPL_MOTD                "372\0"
#define RPL_MOTDSTART           "375\0"
#define RPL_ENDOFMOTD           "376\0"

#define RPL_YOUREOPER           "381\0"

#define ERR_NOSUCHNICK          "401\0"
#define ERR_NOSUCHSERVER        "402\0"
#define ERR_NOSUCHCHANNEL       "403\0"
#define ERR_CANNOTSENDTOCHAN    "404\0"
#define ERR_NORECIPIENT         "411\0"
#define ERR_NOTEXTTOSEND        "412\0"
#define ERR_UNKNOWNCOMMAND      "421\0"
#define ERR_NOMOTD              "422\0"
#define ERR_NONICKNAMEGIVEN     "431\0"
#define ERR_NICKNAMEINUSE       "433\0"
#define ERR_USERNOTINCHANNEL    "441\0"
#define ERR_NOTONCHANNEL        "442\0"
#define ERR_NOTREGISTERED       "451\0"
#define ERR_NEEDMOREPARAMS      "461\0"
#define ERR_ALREADYREGISTRED    "462\0"
#define ERR_PASSWDMISMATCH      "464\0"
#define ERR_UNKNOWNMODE         "472\0"
#define ERR_NOPRIVILEGES        "481\0"
#define ERR_CHANOPRIVSNEEDED    "482\0"
#define ERR_UMODEUNKNOWNFLAG    "501\0"
#define ERR_USERSDONTMATCH      "502\0"

//Number of chars in code representation
//plus \0
#define CODE_SIZE ((size_t) 6)

#define PREFIX_FORMAT ":%s!%s@%s"

typedef struct {
    char code[CODE_SIZE];
    const char template[512];
} reply_t;

/*
 * retrieve_template - searches a statically defined lookup table for the template for a given RPL code
 * code: a pointer to a char array containing a RPL code
 * returns: a pointer to the desired template
 */
msg_t *reply_msg(char *rpl, msg_t *msg, user_t *user, ctx_t *ctx,  ... );

#endif /* REPLY_H_ */
