/*
 *  chirouter - A simple, testable IP router
 *
 *  This module contains miscellaneous helper functions.
 *
 */

/*
 * This project is based on the Simple Router assignment included in the
 * Mininet project (https://github.com/mininet/mininet/wiki/Simple-Router) which,
 * in turn, is based on a programming assignment developed at Stanford
 * (http://www.scs.stanford.edu/09au-cs144/lab/router.html)
 *
 * While most of the code for chirouter has been written from scratch, some
 * of the original Stanford code is still present in some places and, whenever
 * possible, we have tried to provide the exact attribution for such code.
 * Any omissions are not intentional and will be gladly corrected if
 * you contact us at borja@cs.uchicago.edu
 *
 */

/*
 *  Copyright (c) 2016-2018, The University of Chicago
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
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
 *    software without specific prior written permission.
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
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include "protocols/ethernet.h"
#include "utils.h"
#include "log.h"

//Default TTL of IP datagrams
#define DEFAULT_TTL (64)
//MIN macro
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
//Max number of bytes to read directly from an IP datagram
#define MAX_IP_CPY (sizeof(iphdr_t) + 8)

/* See utils.h */
uint16_t router_ntohs(uint16_t netshort)
{
    return ntohs(netshort);
}

/* See utils.h */
uint16_t router_htons(uint16_t hostshort)
{
    return htons(hostshort);
}

/* See utils.h */
uint32_t router_ntohl(uint32_t netlong)
{
    return ntohl(netlong);
}

/* See utils.h */
uint32_t router_htonl(uint32_t hostlong)
{
    return htonl(hostlong);
}


int send_arp_msg(chirouter_ctx_t *ctx, chirouter_interface_t *interface,
                                    uint8_t *dst_mac, struct in_addr *dst_ip)
{
    chilog(DEBUG, "Construcint an ARP message...");
    //Allocate space for our arp message
    uint32_t msg_len = sizeof(ethhdr_t) + sizeof(arp_packet_t);
    uint8_t msg[msg_len];
    memset(msg, 0, msg_len);

    ethhdr_t *msg_hdr = (ethhdr_t*) msg;
    arp_packet_t *arp_msg = (arp_packet_t*) (msg + sizeof(ethhdr_t));

    chilog(DEBUG, "is_reply = %d | msg_len = %d", (dst_mac ? 1 : 0), msg_len);

    /*These are constant, regardless of wether this is a Reply or Request*/
    memcpy(msg_hdr->src, interface->mac, ETHER_ADDR_LEN);
    msg_hdr->type = router_htons(ETHERTYPE_ARP);
    arp_msg->hrd = router_htons(ARP_HRD_ETHERNET);
    arp_msg->pro = router_htons(ETHERTYPE_IP);
    arp_msg->hln = ETHER_ADDR_LEN;
    arp_msg->pln = IPV4_ADDR_LEN;

    memcpy(arp_msg->sha, interface->mac, ETHER_ADDR_LEN);
    arp_msg->spa = interface->ip.s_addr;

    /*These differ based on whether this is meant to be a reply or request*/
    if (dst_mac) //If we are responding to a request
    {
        memcpy(msg_hdr->dst, dst_mac, ETHER_ADDR_LEN);
        arp_msg->op = router_htons(ARP_OP_REPLY);
        memcpy(arp_msg->tha, dst_mac, ETHER_ADDR_LEN);
        arp_msg->tpa = dst_ip->s_addr;
    }
    else //Otherwise
    {
        //Set all bits to 1, indicates broadcast addr
        memset(msg_hdr->dst, 0xff, ETHER_ADDR_LEN);
        arp_msg->op = router_htons(ARP_OP_REQUEST);
        arp_msg->tpa = dst_ip->s_addr;
        /*tha and tpa already zeroed by calloc*/
    }

    chilog_arp(DEBUG, arp_msg, LOG_OUTBOUND);

    chirouter_send_frame(ctx, interface, msg, msg_len);

    return 0;
}

int send_icmp_reply(chirouter_ctx_t *ctx, ethernet_frame_t *frame,
                                        uint8_t icmp_type, uint8_t icmp_code)
{
    ethhdr_t *hdr = (ethhdr_t*) frame->raw;
    iphdr_t *ip_hdr = (iphdr_t*) (frame->raw + sizeof(ethhdr_t));
    icmp_packet_t *icmp; //A pointer to the frames icmp hdr, if it exists
    int payload_len, reply_len; //The length of our icmp payload and reply
    uint8_t *payload; //Points to where we should read payload data from frame
    struct in_addr dst_ip;
    dst_ip.s_addr = ip_hdr->src;


    /*case ICTMPTYPE_ECHO_REPLY*/
    if (icmp_type == ICMPTYPE_ECHO_REPLY)
    {
        icmp = (icmp_packet_t*) (frame->raw + sizeof(ethhdr_t)
                                            + sizeof(iphdr_t));
        payload_len = router_ntohs(ip_hdr->len) - sizeof(iphdr_t)
                                                - ICMP_HDR_SIZE;
        reply_len = sizeof(ethhdr_t) + sizeof(iphdr_t) + ICMP_HDR_SIZE
                                                       + payload_len;
        payload = (uint8_t *) (frame->raw + sizeof(ethhdr_t) + sizeof(iphdr_t)
                                                             + ICMP_HDR_SIZE);
    }

    /*case ICMPTYPE_DEST_UNREACHABLE:
      case ICMPTYPE_TIME_EXCEEDED:*/
    else
    {
        payload_len = MIN(router_ntohs(ip_hdr->len), MAX_IP_CPY);
        reply_len = sizeof(ethhdr_t) + sizeof(iphdr_t) + ICMP_HDR_SIZE
                                                       + payload_len;
        payload = (uint8_t *) ip_hdr;
    }

    //This holds the raw data of our reply
    uint8_t reply[reply_len];
    memset(reply, 0, reply_len);

    ethhdr_t* reply_hdr = (ethhdr_t*) reply;
    iphdr_t* reply_ip_hdr = (iphdr_t*) (reply + sizeof(ethhdr_t));
    icmp_packet_t* reply_icmp = (icmp_packet_t*) (reply + sizeof(ethhdr_t)
                                                        + sizeof(iphdr_t));
    uint8_t *reply_icmp_payload = (uint8_t *) (reply + sizeof(ethhdr_t)
                                                     + sizeof(iphdr_t)
                                                     + ICMP_HDR_SIZE);

    /* Set values in all the headers */

    //ethernet header fields
    memcpy(reply_hdr->src, frame->in_interface->mac, ETHER_ADDR_LEN);
    memcpy(reply_hdr->dst, hdr->src, ETHER_ADDR_LEN);
    reply_hdr->type = router_ntohs(ETHERTYPE_IP);

    //IP header fields
    /*Chnage these magic vars seomtime*/
    reply_ip_hdr->ihl = 5;
    reply_ip_hdr->version = 4;
    reply_ip_hdr->tos = 0;
    reply_ip_hdr->len = router_htons(reply_len - sizeof(ethhdr_t));
    reply_ip_hdr->id = 0;
    reply_ip_hdr->off = 0;
    reply_ip_hdr->ttl = DEFAULT_TTL;
    reply_ip_hdr->proto = IP_PROTO_ICMP;
    reply_ip_hdr->cksum = 0;
    reply_ip_hdr->src = frame->in_interface->ip.s_addr;
    reply_ip_hdr->dst = dst_ip.s_addr;

    //Generate the cksum on the ip hdr
    reply_ip_hdr->cksum = cksum(reply_ip_hdr, sizeof(iphdr_t));

    switch(icmp_type){
        //This is comming from an icmp echo_request
        case ICMPTYPE_ECHO_REPLY:
            reply_icmp->echo.identifier = icmp->echo.identifier;
            reply_icmp->echo.seq_num = icmp->echo.seq_num;
            break;
        case ICMPTYPE_DEST_UNREACHABLE:
            reply_icmp->dest_unreachable.unused = 0;
            reply_icmp->dest_unreachable.next_mtu = 0;
            break;
        case ICMPTYPE_TIME_EXCEEDED:
            reply_icmp->time_exceeded.unused = 0;
            break;
    }

    //These lines are constant
    reply_icmp->type = icmp_type;
    reply_icmp->code = icmp_code;
    reply_icmp->chksum = 0;
    memcpy(reply_icmp_payload, payload, payload_len);

    //Construct cksum for the ICMP header
    reply_icmp->chksum = cksum(reply_icmp, (ICMP_HDR_SIZE + payload_len));

    chilog_ip(DEBUG, reply_ip_hdr, LOG_OUTBOUND);
    chilog_icmp(DEBUG, reply_icmp, LOG_OUTBOUND);
    chirouter_send_frame(ctx, frame->in_interface, reply, reply_len);
    return 0;
}

void print_mac(uint8_t *mac)
{
    chilog(DEBUG, "MAC: %02X:%02X:%02X:%02X:%02X:%02X",
           mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    return;
}

chirouter_rtable_entry_t *resolve_ip(chirouter_ctx_t *ctx, uint32_t ip_addr)
{
    uint32_t mask, dest, res, best;
    best = 0;
    chirouter_rtable_entry_t *elt, *ret;
    ret = NULL;
    chilog(DEBUG, "Resolving IP, in val = %d", ip_addr);
    for(int i = 0; i < ctx->num_rtable_entries; i++)
    {
        elt  = &ctx->routing_table[i];
        dest = elt->dest.s_addr;
        mask = elt->mask.s_addr;
        chilog(DEBUG, "Considering entry %d", i);
        chilog(DEBUG, "DEST = %d | mask = %d", dest, mask);
        res  = ip_addr & mask;
        chilog(DEBUG, "ip & mask = %d", res);
        if ((res == dest) && (mask >= best))
        {
            chilog(DEBUG, "Found macthing entry, has the longest mask so far");
            best = mask;
            ret = elt;
        }
    }
    return ret;
}


/* See utils.h */
uint16_t cksum (const void *_data, int len)
{
      const uint8_t *data = _data;
      uint32_t sum;

      for (sum = 0;len >= 2; data += 2, len -= 2)
      {
        sum += data[0] << 8 | data[1];
      }

      if (len > 0)
      {
        sum += data[0] << 8;
      }

      while (sum > 0xffff)
      {
        sum = (sum >> 16) + (sum & 0xffff);
      }

      sum = htons (~sum);

      return sum ? sum : 0xffff;
}

/* See utils.h */
bool ethernet_addr_is_equal(uint8_t *addr1, uint8_t *addr2)
{
    for (int i=0; i<ETHER_ADDR_LEN; i++)
    {
        if(addr1[i] != addr2[i])
            return false;
    }
    return true;
}
