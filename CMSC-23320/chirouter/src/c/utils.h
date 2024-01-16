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

#ifndef SR_UTILS_H
#define SR_UTILS_H
#include "chirouter.h"

//Macros for discerning IP protocols
#define IP_PROTO_ICMP (1)
#define IP_PROTO_TCP (6)
#define IP_PROTO_UDP (17)

//Free an ethernet_frame_t
#define DROP(frame) {\
    free(frame->raw); \
    free(frame); \
}

/*Use these to translate data to and from frames*/
uint16_t router_ntohs(uint16_t netshort);
uint16_t router_htons(uint16_t hostshort);
uint32_t router_ntohl(uint32_t netlong);
uint32_t router_htonl(uint32_t hostlong);

/*
 * print_mac : print a mac address
 *
 * mac - a pointer to a byte array containing a hardward addr
 *
 * returns : nothing
 */
void print_mac(uint8_t *mac);

/*
 * resolve_ip : resolves the integere value of an IP address to the appropriate
 *              entry in the routing table
 *
 * ctx - a pointer to the router's context object
 *
 * ip_addr - the integer value IP we want to reolve
 *
 * returns : if the IP can be resolved, this returns a pointer to the
 *           appropriate chirouter_rtable_entry_t instance, NULL if no such
 *           entry exists
 */
chirouter_rtable_entry_t *resolve_ip(chirouter_ctx_t *ctx, uint32_t ip_addr);

/*
 * send_icmp_reply : constructs and sends an ICMP reply inresponse to a frame
 *
 * ctx - the router's context object
 *
 * frame - the frame we are responding to
 *
 * icmp_type - the type of ICMP message we want to send
 *
 * icmp_code - the code number of the ICMP message we want to send
 *
 * returns : 0 on success, 1 on failure or error
 */
int send_icmp_reply(chirouter_ctx_t *ctx, ethernet_frame_t *frame,
                                        uint8_t icmp_type, uint8_t icmp_code);

/*
 * send_arp_msg : Sends an ARP message out on interface with the specified
 *                contents
 *
 * ctx - a pointer to the router's context object
 *
 * interface - the interface we are sending this msg on
 *
 * dst_mac - if this is an ARP_REPLY, a ptr to the MAC address that is being
 *            queryed for, otherwise, set this value to NULL
 *
 * dst_ip - The dst_ip of the host that we are asking their MAC for or, if this
 *           is an ARP_REPLY, the host we are responding to.
 *
 * returns : 0 on success and 1 on failure
 */
int send_arp_msg(chirouter_ctx_t *ctx, chirouter_interface_t *interface,
                                    uint8_t *dst_mac, struct in_addr *dst_ip);

/*
 * cksum - Computes a checksum
 *
 * Computes a 16-bit checksum that can be used in an IP or ICMP header.
 *
 * _data: Pointer to data to generate the checksum on
 *
 * len: Number of bytes of data
 *
 * Returns: 16-bit checksum
 *
 */
uint16_t cksum(const void *_data, int len);


/*
 * ethernet_addr_is_equal - Compares two MAC addresses
 *
 * addr1, addr2: Pointers to the MAC addresses. Assumed to be six bytes long.
 *
 * Returns: true if addr1 and addr2 are the same address, false otherwise.
 *
 */
bool ethernet_addr_is_equal(uint8_t *addr1, uint8_t *addr2);

#endif
