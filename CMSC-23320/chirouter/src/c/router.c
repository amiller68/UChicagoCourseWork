/*
 *  chirouter - A simple, testable IP router
 *
 *  This module contains the actual functionality of the router.
 *  When a router receives an Ethernet frame, it is handled by
 *  the chirouter_process_ethernet_frame() function.
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

#include <stdio.h>
#include <assert.h>

#include <string.h>
#include <stdlib.h>

#include "chirouter.h"
#include "arp.h"
#include "utils.h"
#include "utlist.h"


/*
 * handle_arp_packet : handles frame as if it were an ARP msg
 *
 * ctx - a pointer to the router's context object
 *
 * frame - a pointer to the frame that needs to be habdled
 *
 * returns : 0 on success, 1 on error
 */
int handle_arp_packet(chirouter_ctx_t *ctx, ethernet_frame_t *frame);


/*
 * handle_ip_packet : handles frame as if it were an IP datagram
 *
 * ctx - a pointer to the router's context object
 *
 * frame - a pointer to the frame that needs to be habdled
 *
 * returns : 0 on success, 1 on error
 */
int handle_ip_packet(chirouter_ctx_t *ctx, ethernet_frame_t *frame);


/*
 * forward_frame : handles the forwarding of IP datagrams
 *
 * ctx - the pointer to the router's ctx object
 *
 * route - a pointer to the routing_table entry that describes this datagrams
 *         route
 *
 * frame - a pointer to the frame that needs to be forwarded
 *
 * returns : 0 on success, 1 on error
 */
 int forward_frame(chirouter_ctx_t *ctx, chirouter_rtable_entry_t *route,
                                                    ethernet_frame_t *frame);


/*
 * chirouter_process_ethernet_frame - Process a single inbound Ethernet frame
 *
 * This function will get called every time an Ethernet frame is received by
 * a router. This function receives the router context for the router that
 * received the frame, and the inbound frame (the ethernet_frame_t struct
 * contains a pointer to the interface where the frame was received).
 * Take into account that the chirouter code will free the frame after this
 * function returns so, if you need to persist a frame (e.g., because you're
 * adding it to a list of withheld frames in the pending ARP request list)
 * you must make a deep copy of the frame.
 *
 * chirouter can manage multiple routers at once, but does so in a single
 * thread. i.e., it is guaranteed that this function is always called
 * sequentially, and that there will not be concurrent calls to this
 * function. If two routers receive Ethernet frames "at the same time",
 * they will be ordered arbitrarily and processed sequentially, not
 * concurrently (and with each call receiving a different router context)
 *
 * ctx: Router context
 *
 * frame: Inbound Ethernet frame
 *
 * Returns:
 *   0 on success,
 *
 *   1 if a non-critical error happens
 *
 *   -1 if a critical error happens
 *
 *   Note: In the event of a critical error, the entire router will shut down and exit.
 *         You should only return -1 for issues that would prevent the router from
 *         continuing to run normally. Return 1 to indicate that the frame could
 *         not be processed, but that subsequent frames can continue to be processed.
 */
int chirouter_process_ethernet_frame(chirouter_ctx_t *ctx, ethernet_frame_t *frame)
{
    ethhdr_t* hdr = (ethhdr_t*) frame->raw;
    uint16_t type = router_ntohs(hdr->type);
    //chilog(DEBUG, "handling an ethernet frame of type %d...", type);
    /*If this is an ARP packet*/
    if (type == ETHERTYPE_ARP)
    {
        handle_arp_packet(ctx, frame);
    }

    /*Else if it's an IP packet (include ICMP packets)*/
    else if (type == ETHERTYPE_IP)
    {
        handle_ip_packet(ctx, frame);
    }

    return 0;
}


int handle_arp_packet(chirouter_ctx_t *ctx, ethernet_frame_t *frame)
{
    arp_packet_t* arp_packet = (arp_packet_t*) (frame->raw + sizeof(ethhdr_t));
    chirouter_interface_t *in_interface = frame->in_interface;
    uint16_t op = router_ntohs(arp_packet->op);
    struct in_addr src_ip;
    src_ip.s_addr = arp_packet->spa;
    uint8_t *src_mac = arp_packet->sha;
    uint8_t *if_mac = in_interface->mac;
    struct in_addr *if_ip = &in_interface->ip;

    chilog_arp(DEBUG, arp_packet, LOG_INBOUND);

    if (op == ARP_OP_REPLY)
    {
        chilog(DEBUG, "Handling an ARP reply...");

        chirouter_pending_arp_req_t *pending_req;
        withheld_frame_t *withheld_frames, *elt;
        chirouter_interface_t *if_out;

        /*Lock the ARP mutex*/
        pthread_mutex_lock(&ctx->lock_arp);
        if (chirouter_arp_cache_lookup(ctx, &src_ip))
        {
            chilog(DEBUG, "This is a duplicate ARP reply!");
            pthread_mutex_unlock(&ctx->lock_arp);
            return 1;
        }

        chilog(DEBUG, "Adding entry to the arp cache");
        chirouter_arp_cache_add(ctx, &src_ip, src_mac);

        if(!(pending_req = chirouter_arp_pending_req_lookup(ctx, &src_ip)))
        {
            chilog(DEBUG, "No such pending ARP request!");
            pthread_mutex_unlock(&ctx->lock_arp);
            return 1;
        }

        chilog(DEBUG, "Forwarding withheld frames...");

        withheld_frames = pending_req->withheld_frames;
        if_out = pending_req->out_interface;

        DL_FOREACH(withheld_frames, elt)
        {
            uint8_t *msg = elt->frame->raw;
            int msg_len  = elt->frame->length;
            ethhdr_t *hdr = (ethhdr_t*) elt->frame->raw;
            iphdr_t* ip_hdr = (iphdr_t*) (elt->frame->raw + sizeof(ethhdr_t));

            chilog_ip(DEBUG, ip_hdr, LOG_INBOUND);

            /*Send icmp_reply, TIME_EXCEEDED*/
            if(ip_hdr->ttl == 1)
            {
                chilog(DEBUG, "This frame has exceeded it's time limit!");
                send_icmp_reply(ctx, elt->frame, ICMPTYPE_TIME_EXCEEDED, 0);
            }

            else
            {
                memcpy(hdr->src, if_out->mac, ETHER_ADDR_LEN);
                memcpy(hdr->dst, src_mac, ETHER_ADDR_LEN);

                //decrement TTL
                ip_hdr->ttl = ip_hdr->ttl - 1;
                //recompute checksum
                ip_hdr->cksum =  0;
                ip_hdr->cksum = cksum(ip_hdr, sizeof(iphdr_t));

                chirouter_send_frame(ctx, if_out, msg, msg_len);
            }
        }

        /*Free the frames associated with this pending request*/
        chirouter_arp_pending_req_free_frames(pending_req);
        /*Remove this request from the pending list*/
        DL_DELETE(ctx->pending_arp_reqs, pending_req);
        /*Free the request*/
        free(pending_req);
        /*Unlock mutex*/
        pthread_mutex_unlock(&ctx->lock_arp);
    }

    else if (op == ARP_OP_REQUEST)
    {
        chilog(DEBUG, "This is an ARP request!");
        /*if this request is for me*/
        if (arp_packet->tpa == in_interface->ip.s_addr)
        {
            chilog(DEBUG, "This ARP request is for me!");

            /*Respond by sending an ARP reply out on this interface*/
            send_arp_msg(ctx, in_interface, src_mac, &src_ip);
        }

        else
        {
            /*Ignore this request*/
            chilog(DEBUG, "ARP request was for someone else!");
        }
    }

    return 0;
}

int handle_ip_packet(chirouter_ctx_t *ctx, ethernet_frame_t *frame)
{
    ethhdr_t* hdr = (ethhdr_t*) frame->raw;
    iphdr_t* ip_hdr = (iphdr_t*) (frame->raw + sizeof(ethhdr_t));
    icmp_packet_t *icmp_hdr = (icmp_packet_t*) (frame->raw + sizeof(ethhdr_t)
                                                           + sizeof(iphdr_t));

    //Resolve the IP dest
    chirouter_rtable_entry_t *route;
    route = resolve_ip(ctx, ip_hdr->dst);
    chirouter_interface_t *if_in, *if_out;
    if_in  = frame->in_interface; //The interface this frame came on
    chilog(DEBUG,"Handling an IP packet");
    chilog_ip(DEBUG, ip_hdr, LOG_INBOUND);

    //If there is a possible way to forward this datagram
    if (route)
    {
        if_out = route->interface; //The interface this frame is destined for
        //If this frame is addressed to an interface I control

            //If the if_out == if_in
        if (ethernet_addr_is_equal(if_in->mac, if_out->mac))
        {
            switch(ip_hdr->proto)
            {
                case IP_PROTO_TCP:
                case IP_PROTO_UDP:
                    chilog(DEBUG, "This is a UDP/TCP segment!");
                    send_icmp_reply(ctx, frame, ICMPTYPE_DEST_UNREACHABLE,
                                         ICMPCODE_DEST_PORT_UNREACHABLE);
                    break;
                case IP_PROTO_ICMP:
                    chilog(DEBUG, "This is an ICMP segment");
                    if(ip_hdr->ttl == 1)
                    {
                        send_icmp_reply(ctx, frame, ICMPTYPE_TIME_EXCEEDED, 0);
                    }

                    else if(icmp_hdr->type == ICMPTYPE_ECHO_REQUEST)
                    {
                        chilog(DEBUG,"This is an ECHO request!");
                        send_icmp_reply(ctx, frame, ICMPTYPE_ECHO_REPLY,
                                                      ICMPTYPE_ECHO_REPLY);
                    }
                    break;
                default:
                    /*Not sure if this needs to be here*/
                    if(ip_hdr->ttl == 1)
                    {
                        send_icmp_reply(ctx, frame, ICMPTYPE_TIME_EXCEEDED, 0);
                    }
                    break;
            }
        }

            //if_out != in
        else if (ip_hdr->dst == if_out->ip.s_addr)
        {
            chilog(DEBUG, "This segment is for one of my other interfaces");
            send_icmp_reply(ctx, frame, ICMPTYPE_DEST_UNREACHABLE,
                                        ICMPCODE_DEST_HOST_UNREACHABLE);
        }

        //This datagram resolved! lets forward it!
        else
        {
            chilog(DEBUG, "This frame can be forwarded!");
            forward_frame(ctx, route, frame);
        }
    }

    //We can't resolve this ip...
    else
    {
        chilog(DEBUG, "This destination is unreachable");
        send_icmp_reply(ctx, frame, ICMPTYPE_DEST_UNREACHABLE,
                                        ICMPCODE_DEST_NET_UNREACHABLE);
    }

    return 0;
}


int forward_frame(chirouter_ctx_t *ctx, chirouter_rtable_entry_t *route,
                                                        ethernet_frame_t *frame)
{
    chilog(DEBUG, "Forwarding Ethernet frame!");
    uint8_t *msg = frame->raw;
    int msg_len  = frame->length;
    ethhdr_t* hdr = (ethhdr_t*) frame->raw;
    iphdr_t* ip_hdr = (iphdr_t*) (frame->raw + sizeof(ethhdr_t));
    struct in_addr dst_ip;
    chirouter_arpcache_entry_t *cache_match;
    chirouter_interface_t *if_out = route->interface;

    //If the routing table entry specifies a Gateway route...
    if (route->gw.s_addr)
    {
        dst_ip.s_addr = route->gw.s_addr;
    }

    //Otherwise, if no Gateway is specified
    else
    {
        dst_ip.s_addr = ip_hdr->dst;
    }

    chilog(DEBUG, "Searching ARP cache | dst_ip = %d", inet_ntoa(dst_ip));

    pthread_mutex_lock(&ctx->lock_arp);
    if (!(cache_match = chirouter_arp_cache_lookup(ctx, &dst_ip)))
    {
        chilog(DEBUG, "No such cache entry");

        chirouter_pending_arp_req_t *pending_req;

        //If there isn't a pending arp request for dst_ip
        if(!(pending_req = chirouter_arp_pending_req_lookup(ctx, &dst_ip)))
        {
            chilog(DEBUG, "No such pending ARP request! Creating new one.");
            //Initialize a new pending_req
            pending_req = chirouter_arp_pending_req_add(ctx, &dst_ip, if_out);
        }

        chilog(DEBUG, "Adding frame to pending_req");
        chirouter_arp_pending_req_add_frame(ctx, pending_req, frame);
    }

    /*We have a MAC for this packet, therefore we should check the ttl*/
    else if(ip_hdr->ttl > 1)  /*I think this should be here*/
    {
        chilog(DEBUG, "Found a cache match:");
        print_mac(cache_match->mac);

        memcpy(hdr->src, if_out->mac, ETHER_ADDR_LEN);
        memcpy(hdr->dst, cache_match->mac, ETHER_ADDR_LEN);

        //decrement TTL
        ip_hdr->ttl = ip_hdr->ttl - 1;
        //recompute checksum
        ip_hdr->cksum = 0;
        ip_hdr->cksum = cksum(ip_hdr, sizeof(iphdr_t));

        chilog_ip(DEBUG, ip_hdr, LOG_OUTBOUND);

        chirouter_send_frame(ctx, if_out, msg, msg_len);
    }

    /*We have a MAC, but TTL = 1*/
    else
    {
        send_icmp_reply(ctx, frame, ICMPTYPE_TIME_EXCEEDED, 0);
    }

    pthread_mutex_unlock(&ctx->lock_arp);
    return 0;
}
