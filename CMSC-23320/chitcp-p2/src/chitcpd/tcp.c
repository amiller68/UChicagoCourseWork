/*
 *  chiTCP - A simple, testable TCP stack
 *
 *  Implementation of the TCP protocol.
 *
 *  chiTCP follows a state machine approach to implementing TCP.
 *  This means that there is a handler function for each of
 *  the TCP states (CLOSED, LISTEN, SYN_RCVD, etc.). If an
 *  event (e.g., a packet arrives) while the connection is
 *  in a specific state (e.g., ESTABLISHED), then the handler
 *  function for that state is called, along with information
 *  about the event that just happened.
 *
 *  Each handler function has the following prototype:
 *
 *  int f(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event);
 *
 *  si is a pointer to the chiTCP server info. The functions in
 *       this file will not have to access the data in the server info,
 *       but this pointer is needed to call other functions.
 *
 *  entry is a pointer to the socket entry for the connection that
 *          is being handled. The socket entry contains the actual TCP
 *          data (variables, buffers, etc.), which can be extracted
 *          like this:
 *
 *            tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
 *
 *          Other than that, no other fields in "entry" should be read
 *          or modified.
 *
 *  event is the event that has caused the TCP thread to wake up. The
 *          list of possible events corresponds roughly to the ones
 *          specified in http://tools.ietf.org/html/rfc793#section-3.9.
 *          They are:
 *
 *            APPLICATION_CONNECT: Application has called socket_connect()
 *            and a three-way handshake must be initiated.
 *
 *            APPLICATION_SEND: Application has called socket_send() and
 *            there is unsent data in the send buffer.
 *
 *            APPLICATION_RECEIVE: Application has called socket_recv() and
 *            any received-and-acked data in the recv buffer will be
 *            collected by the application (up to the maximum specified
 *            when calling socket_recv).
 *
 *            APPLICATION_CLOSE: Application has called socket_close() and
 *            a connection tear-down should be initiated.
 *
 *            PACKET_ARRIVAL: A packet has arrived through the network and
 *            needs to be processed (RFC 793 calls this "SEGMENT ARRIVES")
 *
 *            TIMEOUT: A timeout (e.g., a retransmission timeout) has
 *            happened.
 *
 */

/*
 *  Copyright (c) 2013-2014, The University of Chicago
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

#include "chitcp/log.h"
#include "chitcp/utils.h"
#include "chitcp/buffer.h"
#include "chitcp/chitcpd.h"
#include "serverinfo.h"
#include "connection.h"
#include "tcp.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/*TCP Helper macros*/
#define WR_PACKET(x) ((tcp_seq) chitcp_htonl(x))
#define RD_PACKET(x) ((uint32_t) chitcp_ntohl(x))
#define DROP(packet) {\
    chitcp_packet_list_pop_head(&tcp_data->pending_packets); \
    chitcp_tcp_packet_free(packet); \
    free(packet); \
}

/*RTO macros*/
#define K 4 //Constant K
#define G (50 * MILLISECOND) //Clock granularity
#define MIN_RTO (200 * MILLISECOND) //Minimum RTO

/* Used to pass in packet flags all at once, through bitwise comparisons.
*  Eg. passing "ACK | SYN | FIN" turns on all three flags for the packet.
*/
typedef enum
{
    SYN = 1 << 0,
    ACK = 1 << 1,
    FIN = 1 << 2,
    NRT = 1 << 3
} packet_flag_type_t;


/* Used to pass in server data to timer callback threads, as a void*. */
typedef struct {
    serverinfo_t *si;
    chisocketentry_t *entry;
} socket_info_t;

/*Event handler functions*/

/*
 * packet_arrival_handler - Handles the PACKET_ARRIVAL event in all possible TCP
 * states as specified in pages 64-74 of RFC 793. Processes and handles the
 * firstmost packet in the packet arrival queue.
 *
 * si : a pointer to a struct with the chiTCP daemon’s runtime information
 *
 * entry : a pointer to the socket entry for the connection being handled.
 *
 * returns : 0 on success and -1 on an error or unexpected packet
 */
int packet_arrival_handler(serverinfo_t *si, chisocketentry_t *entry);

/*
 * retransmission_handler - Handle the TIMEOUT_RTX even in all possible TCP
 * states
 *
 * si : a pointer to a struct with the chiTCP daemon’s runtime information
 *
 * entry : a pointer to the socket entry for the connection being handled.
 *
 * returns : 0 on success and -1 on an error or unexpected packet
 */
void retransmission_handler(serverinfo_t *si, chisocketentry_t *entry);

/*
 * persist_handler - Handle the TIMEOUT_PST even in all possible TCP
 * states
 *
 * si : a pointer to a struct with the chiTCP daemon’s runtime information
 *
 * entry : a pointer to the socket entry for the connection being handled.
 *
 * returns : 0 on success and -1 on an error or unexpected packet
 */
void persist_handler(serverinfo_t *si, chisocketentry_t *entry);

/*Helper functions */

/*
* write_data : writes the data in a given packet into the receive buffer
*
* si : a pointer to a struct with the chiTCP daemon’s runtime information.
*
* entry : a pointer to the socket entry for the connection being handled.
*
* packet : the packet with data that gets written
*
* returns : nothing but updates the tcp data
*/
void write_data(serverinfo_t *si, chisocketentry_t *entry, tcp_packet_t *packet);

/*
 * send_timeout - Handles timeouts for the RETRANSMISSION and PERSIST timers
 * Signals the chiTCP to trigger an TIMEOUT_RTX or TIMEOUT_PST
 *
 * mt : a pointer to the multi_timer_t that manages the RETRANSMISSION and
 *      PERSIST timers
 *
 * timer : a pointer to the single_timer_t that has just timed out
 *
 * args : A pointer to the args the the timer's callback function requires
 *
 * returns - nothing
 */
void send_timeout(multi_timer_t *mt, single_timer_t *timer, void *args);

/*
* set_timer : Sets the timer specified by id to TO in tcp_data->RTO nanoseconds
*
* si : a pointer to a struct with the chiTCP daemon’s runtime information
*
* entry : a pointer to the socket entry for the connection being handled.
*
* id : the id of the timer being set.
*
*
*
* returns : nothing but sets the appropriate timer
*/
void set_timer(serverinfo_t *si, chisocketentry_t *entry, uint16_t id);

/*
* clear_queue - Clears the retransmisionn queue of all hereby acked packet.
*
* si : a pointer to a struct with the chiTCP daemon’s runtime information.
*
* entry : a pointer to the socket entry for the connection being handled.
*
* returns : nothing but updates the tcp data.
*/
void clear_queue(serverinfo_t *si, chisocketentry_t *entry);


/* send_packet - Sends a packet with the specified values and flags to the
 * socket's peer
 *
 * seq : the host-ordered integer value of the packet's SEQ number
 *
 * ack : the host-ordered integer value of the packet's ack number
 *
 * payload : a pointer to the data to be sent in this packet
 *
 * payload_size : an intege specifying the number of bytes to read from the
 * payload pointer.
 *
 * flags : the possible flags for a packet: ACK, SEQ or FIN.
 *
 * returns : a pointer to the packet which was just sent
 */
tcp_packet_t *send_packet(serverinfo_t *si, chisocketentry_t *entry,
    uint32_t seq, uint32_t ack, void *payload, size_t payload_size, int flags);

/*
 * packetize_data - Sends as much available data as possible in packets to the
 * socket's peer without overflowing the peers RCV_WND. Updates TCP data appropriately
 *
 * si : a pointer to a struct with the chiTCP daemon’s runtime information
 *
 * entry : a pointer to the socket entry for the connection that is being
 * handled.
 *
 * returns : nothing
 */
void packetize_data(serverinfo_t *si, chisocketentry_t *entry);

/*
 * packet_is_acceptable - determines if a packet is acceptable to process as
 * specified by page 69 of RFC 793
 *
 * packet : a pointer to the packet to check
 *
 * tcp_data : a pointer to tcp_data_t struct detailing the socket's TCP data
 *
 * returns - true if the TCP packet is valid for processing and false otherwise.
 */
bool packet_is_acceptable(tcp_packet_t *packet, tcp_data_t *tcp_data);

/*
* add_to_out_of_order : adds s given packet to the out of order list based on
* ascending SEG_SEQ values
*
* out_of_order : a pointer to the out of order list
*
* packet : the packet being added to the list
*
* returns : nothing but updates the out of order list
*/
void add_to_out_of_order(tcp_packet_list_t *out_of_order, tcp_packet_t *packet);


/*
* update_rto : updates the rto in response to a new measurement
*
* si : a pointer to a struct with the chiTCP daemon’s runtime information.
*
* entry : a pointer to the socket entry for the connection being handled.
*
* packet : packet that has been sent and acked ??
*
* returns : nothing
*/
void update_rto(serverinfo_t *si, chisocketentry_t *entry, tcp_packet_t *packet);

/*                                                  */
/*         Functions provided by ChiTCP             */
/*                                                  */

void tcp_data_init(serverinfo_t *si, chisocketentry_t *entry)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    tcp_data->pending_packets = NULL;
    pthread_mutex_init(&tcp_data->lock_pending_packets, NULL);
    pthread_cond_init(&tcp_data->cv_pending_packets, NULL);

    //Initialize the window size
    tcp_data->RCV_WND = TCP_BUFFER_SIZE;

    //Initialize the window tracker vars
    tcp_data->SND_WL1 = 0;
    tcp_data->SND_WL2 = 0;

    /*Initialize retransmission_queue */
    tcp_data->retransmission_queue = NULL;

    /*Initialize out_of_order_list*/
    tcp_data->out_of_order_list = NULL;

    /*Initial RTO*/
    tcp_data->RTO = MIN_RTO;
    tcp_data->first_r = true;

    // Initialize the multitimer
    mt_init(&tcp_data->mt, 2);
}

void tcp_data_free(serverinfo_t *si, chisocketentry_t *entry)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    circular_buffer_free(&tcp_data->send);
    circular_buffer_free(&tcp_data->recv);
    chitcp_packet_list_destroy(&tcp_data->pending_packets);
    pthread_mutex_destroy(&tcp_data->lock_pending_packets);
    pthread_cond_destroy(&tcp_data->cv_pending_packets);

    /* Cleanup of additional tcp_data_t fields goes here */

    //Free the multitimer
    mt_free(&tcp_data->mt);

    //Destroy our linked lists
    chitcp_packet_list_destroy(&tcp_data->out_of_order_list);
    chitcp_packet_list_destroy(&tcp_data->retransmission_queue);
}

int chitcpd_tcp_state_handle_CLOSED(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    if (event == APPLICATION_CONNECT)
    {
        //ISS for the active socket in the TCP connection
        tcp_data->ISS = 12340000;
        //Initialize the send buffer seq
        circular_buffer_t *send = &tcp_data->send;
        circular_buffer_set_seq_initial(send, tcp_data->ISS + 1);
        //Send a SYN packet to a listening peer
        uint32_t seq = tcp_data->ISS, ack_seq = 0;
        send_packet(si, entry, seq, ack_seq, NULL, 0, SYN);

        //Update the tcp_data
        tcp_data->SND_UNA = tcp_data->ISS;
        tcp_data->SND_NXT = tcp_data->ISS + 1;

        //Update the state
        chitcpd_update_tcp_state(si, entry, SYN_SENT);
    }
    else if (event == CLEANUP)
    {
        /* Any additional cleanup goes here */
    }
    else
        chilog(WARNING, "In CLOSED state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_LISTEN(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        packet_arrival_handler(si, entry);
    }
    else
        chilog(WARNING, "In LISTEN state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_SYN_RCVD(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        //Handle the first packet in the arrival queue
        packet_arrival_handler(si, entry);
    }
    else if (event == TIMEOUT_RTX)
    {
       retransmission_handler(si, entry);
    }
    else
        chilog(WARNING, "In SYN_RCVD state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_SYN_SENT(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        packet_arrival_handler(si, entry);
    }
    else if (event == TIMEOUT_RTX)
    {
        retransmission_handler(si, entry);
    }
    else
        chilog(WARNING, "In SYN_SENT state, received unexpected event.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_ESTABLISHED(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    if (event == APPLICATION_SEND)
    {
        //Send any data in the buffer
        packetize_data(si, entry);
    }
    else if (event == PACKET_ARRIVAL)
    {
        //Handle the first packet in the arrival queue
        packet_arrival_handler(si, entry);
    }
    else if (event == APPLICATION_RECEIVE)
    {
        circular_buffer_t *recv = &tcp_data->recv;
        //Update the recieve window following a call to recv()
        tcp_data->RCV_WND = TCP_BUFFER_SIZE - circular_buffer_count(recv);
    }
    else if (event == APPLICATION_CLOSE)
    {
        //Send any remaining data
        packetize_data(si, entry);

        //Send a FIN packet
        uint32_t seq = tcp_data->SND_NXT, ack_seq = tcp_data->RCV_NXT;
        send_packet(si, entry, seq, ack_seq, NULL, 0, FIN);

        //Advance SND_NXT over the FIN segment
        tcp_data->SND_NXT = tcp_data->SND_NXT + 1;

        //Enter the FIN_WAIT_1 state
        chitcpd_update_tcp_state(si, entry, FIN_WAIT_1);

        //Signal that we are closing
        tcp_data->closing = true;
    }
    else if (event == TIMEOUT_RTX)
    {
       retransmission_handler(si, entry);
    }
    else if (event == TIMEOUT_PST)
    {
        chilog(MINIMAL, "Handling event PERSIST on state ESTABLISHED");
        persist_handler(si, entry);
    }
    else
        chilog(WARNING, "In ESTABLISHED state, received unexpected event (%i).",
                         event);

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_FIN_WAIT_1(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        packet_arrival_handler(si, entry);
    }
    else if (event == APPLICATION_RECEIVE)
    {
        /* Your code goes here */
    }
    else if (event == TIMEOUT_RTX)
    {
        retransmission_handler(si, entry);
    }
    else if (event == TIMEOUT_PST)
    {
        persist_handler(si, entry);
    }
    else
        chilog(WARNING, "In FIN_WAIT_1 state, received unexpected event (%i).",
                           event);

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_FIN_WAIT_2(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        packet_arrival_handler(si, entry);
    }
    else if (event == APPLICATION_RECEIVE)
    {
        /* Your code goes here */
    }
    else if (event == TIMEOUT_RTX)
    {
        retransmission_handler(si, entry);
    }
    else
        chilog(WARNING, "In FIN_WAIT_2 state, received unexpected event (%i).",
                         event);

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_CLOSE_WAIT(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    if (event == APPLICATION_CLOSE)
    {
        //Send any data still in the send buffer
        packetize_data(si, entry);

        //Send a FIN packet
        uint32_t seq = tcp_data->SND_NXT, ack_seq = tcp_data->RCV_NXT;
        send_packet(si, entry, seq, ack_seq, NULL, 0, FIN);

        //Advance SND_NXT over the FIN
        tcp_data->SND_NXT = tcp_data->SND_NXT + 1;

        //Enter the LAST_ACK state
        chitcpd_update_tcp_state(si, entry, LAST_ACK);

        //Signal that we are closing
        tcp_data->closing = true;
    }
    else if (event == PACKET_ARRIVAL)
    {
        //Handle the first packet in the arrival queue
        packet_arrival_handler(si, entry);
    }
    else if (event == TIMEOUT_RTX)
    {
        retransmission_handler(si, entry);
    }
    else if (event == TIMEOUT_PST)
    {
        persist_handler(si, entry);
    }
    else
        chilog(WARNING, "In CLOSE_WAIT state, received unexpected event (%i).",
                         event);


    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_CLOSING(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        //Handle the first packet in the arrival queue
        packet_arrival_handler(si, entry);
    }
    else if (event == TIMEOUT_RTX)
    {
        retransmission_handler(si, entry);
    }
    else if (event == TIMEOUT_PST)
    {
        persist_handler(si, entry);
    }
    else
        chilog(WARNING, "In CLOSING state, received unexpected event (%i).",
                         event);

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_TIME_WAIT(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    chilog(WARNING, "Running handler for TIME_WAIT. This should not happen.");

    return CHITCP_OK;
}

int chitcpd_tcp_state_handle_LAST_ACK(serverinfo_t *si, chisocketentry_t *entry, tcp_event_type_t event)
{
    if (event == PACKET_ARRIVAL)
    {
        //Handle the first packet in the arrival queue
        packet_arrival_handler(si, entry);
    }
    else if (event == TIMEOUT_RTX)
    {
        retransmission_handler(si, entry);
    }
    else if (event == TIMEOUT_PST)
    {
        persist_handler(si, entry);
    }
    else
        chilog(WARNING, "In LAST_ACK state, received unexpected event (%i).",
                        event);

    return CHITCP_OK;
}


/*                                                           */
/*                  Additional functions                     */
/*                                                           */

/*Event Handlers*/

int packet_arrival_handler(serverinfo_t *si, chisocketentry_t *entry)
{
    //Implements RFC 793 pg. 65-74: SEGMENT ARRIVES in all TCP states
    tcp_state_t state = entry->tcp_state;
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    tcp_packet_t *packet = tcp_data->pending_packets->packet;
    tcphdr_t *header = TCP_PACKET_HEADER(packet);
    multi_timer_t *mt = &tcp_data->mt;

    /* Observe the advertised window - if it is 0, set the persist timer */
    if(!SEG_WND(packet))
    {
        chilog(DEBUG, "Setting persist timer!");
        mt_cancel_timer(&tcp_data->mt, PERSIST);
        set_timer(si, entry, PERSIST);
    }

    //If we observe a window > 0
    else
    {
        chilog(DEBUG, "Canceling persist timer");
        /*Cancels the timer if active, does nothing if its inactive*/
        mt_cancel_timer(&tcp_data->mt, PERSIST);
        //tcp_data->persist_sent = false;
    }

    switch (entry->tcp_state)
    {
    case CLOSED: //pg. 65
        break;
    /* If the state is LISTEN then */
    case LISTEN: //pg. 65-66
        /* Check for an ACK */
        if (header->ack)
        {
            chilog(ERROR, "ERR: Wasn't expecting an ACK");
            DROP(packet);
            return -1;
        }
        /* Check for an SYN */
        if (header->syn)
        {
            /*Set RCV.NXT to SEG.SEQ+1, IRS is set to SEG.SEQ and any other
            control or text should be queued for processing later. ISS
            should be selected and a SYN segment sent of the form:
            <SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>*/

            tcp_data->RCV_NXT = SEG_SEQ(packet) + 1;
            tcp_data->IRS = SEG_SEQ(packet);
            //Set the ISS of the passive peer
            tcp_data->ISS = 43210000;

            //Initialize the send/recv buffer intial seq number
            circular_buffer_t *recv = &tcp_data->recv;
            circular_buffer_t *send = &tcp_data->send;
            circular_buffer_set_seq_initial(recv, tcp_data->IRS + 1);
            circular_buffer_set_seq_initial(send, tcp_data->ISS + 1);

            //Send a SYN-ACK in response
            uint32_t seq = tcp_data->ISS, ack_seq = tcp_data->RCV_NXT;
            send_packet(si, entry, seq, ack_seq, NULL, 0, ACK | SYN);

            //Update TCP data and state
            tcp_data->SND_NXT = tcp_data->ISS + 1;
            tcp_data->SND_UNA = tcp_data->ISS;
            chitcpd_update_tcp_state(si, entry, SYN_RCVD);

            DROP(packet);
            return 0;
        }

        /* Fourth other text or control */
        chilog(ERROR,"ERR: unexpected packet");
        DROP(packet);
        return -1;
    case SYN_SENT:  //pg. 66-68
        /* Check the ACK bit */
        if (header->ack)
        {
            if (SEG_ACK(packet) <= tcp_data->ISS ||
                SEG_ACK(packet) > tcp_data->SND_NXT)
            {
                //RFC 793 specifies a RST packet to be sent in response
                chilog(ERROR, "ERR: RST sent in this situation");
                DROP(packet);
                return -1;
            }
            if (!(tcp_data->SND_UNA <= SEG_ACK(packet) &&
                     SEG_ACK(packet) <= tcp_data->SND_NXT))
            {
                //Ack is not acceptable
                chilog(ERROR, "ERR: Unnacceptable acknowledgment");
                DROP(packet);
                return -1;
            }
        }
        /* Check the SYN bit */
        if (header->syn)
        {
            //Update the TCP data
            tcp_data->RCV_NXT = SEG_SEQ(packet) + 1;
            tcp_data->IRS = SEG_SEQ(packet);
            circular_buffer_t *recv = &tcp_data->recv;
            circular_buffer_set_seq_initial(recv, tcp_data->IRS + 1);

            if (header->ack)
            {
                tcp_data->SND_UNA = SEG_ACK(packet);
            }

            /* and any segments on the retransmission queue which
            are thereby acknowledged should be removed. */
            update_rto(si, entry, packet);
            clear_queue(si, entry);

            /* Our syn has been ACKed */
            if (tcp_data->SND_UNA > tcp_data->ISS)
            {
                //Send the appropriate ACK packet
                uint32_t seq = tcp_data->SND_NXT, ack_seq = tcp_data->RCV_NXT;
                send_packet(si, entry, seq, ack_seq, NULL, 0, ACK | NRT);

                //Enter ESTABLISHED state
                chitcpd_update_tcp_state(si, entry, ESTABLISHED);

                //As per RFC 1122, The send window should be updated
                tcp_data->SND_WND = SEG_WND(packet);
                tcp_data->RCV_WND = SEG_WND(packet);

                DROP(packet);
                return 0;
            }
            else
            {
                //Enter the SYN_RCVD state
                chitcpd_update_tcp_state(si, entry, SYN_RCVD);

                //Send a SYN-ACK packet
                uint32_t seq = tcp_data->ISS, ack_seq = tcp_data->RCV_NXT;
                send_packet(si, entry, seq, ack_seq, NULL, 0, SYN | ACK);

                DROP(packet);
                return 0;
            }
        }

        /*If neither of the SYN or RST bits is set then drop the
        segment and return. */
        else
        {
            DROP(packet);
            return -1;
        }
        break;

    /* Otherwise */
    case SYN_RCVD:
    case ESTABLISHED:
    case FIN_WAIT_1:
    case FIN_WAIT_2:
    case CLOSE_WAIT:
    case CLOSING:
    case LAST_ACK:
    case TIME_WAIT: //pg 69-70
        if (!packet_is_acceptable(packet, tcp_data))
        {
            //Send an ACK segment
            chilog(DEBUG, "ack is not acceptable!");
            uint32_t seq = tcp_data->SND_NXT, ack_seq = tcp_data->RCV_NXT;
            send_packet(si, entry, seq, ack_seq, NULL, 0, ACK);

            /*After sending, drop the unacceptable segment and return.*/
            DROP(packet);
            return 0;
        }
        chilog(DEBUG, "ack is acceptable");
        break;
    }
    //The following implements pg 71-74

    /*Check the SYN bit */
    if (header->syn) //pg. 71
    {
        //if the SYN is in the window
        if (tcp_data->RCV_NXT <= SEG_SEQ(packet) <
            tcp_data->RCV_NXT + tcp_data->RCV_WND)
        {
            //RFC 793 specifies a RST packet to be sent in response
            chilog(DEBUG, "ERR: RST sent in this situation");

            /*any outstanding RECEIVEs and SEND should receive "reset" responses
            and all segment queues should be flushed, the user should also
            receive an unsolicited general "connection reset" signal,*/

            //Enter the CLOSED state
            chitcpd_update_tcp_state(si, entry, CLOSED);
            return 0;
        }
    }

    /* Check the ACK field*/
    if (!header->ack) //pg. 72
    {
        /*Check if header->fin == True
          and continue processing the packet if so.
          Drops the packet otherwise*/
        chilog(DEBUG, "ack was not set");
        goto HANDLE_FIN;
    }

    /* If the ACK bit is on */
    switch (entry->tcp_state) //pg 72-73
    {
        case SYN_RCVD:
            /*If the segment acknowledgment is acceptable*/
            if (tcp_data->SND_UNA <= SEG_ACK(packet) <= tcp_data->SND_NXT)
            {
                //Update TCP data
                tcp_data->SND_UNA = SEG_ACK(packet);
                tcp_data->SND_WND = SEG_WND(packet);

                if (tcp_data->SND_WND == 0)
                    chilog(CRITICAL,
                        "Zero window received. TCP may become stuck.");

                //Enter the ESTABLISHED state
                chitcpd_update_tcp_state(si, entry, ESTABLISHED);
            }

            /*If the segment acknowledgment is not acceptable*/
            else
            {
                chilog(ERROR, "ERR: Unnacceptable acknowledgment");
                chilog(ERROR, "ERR: RST sent in this situation");
                DROP(packet);
                return -1;
            }
        case FIN_WAIT_1:
            /*In addition to the processing for the ESTABLISHED state ..*/
        case FIN_WAIT_2:
            /* In addition to the processing for the ESTABLISHED state...*/
        case CLOSE_WAIT:
            /*Same as established state*/
        case CLOSING:
            /*In addition to the processing for the ESTABLISHED state...*/
        case ESTABLISHED:
            //The ACK is ok
            if (tcp_data->SND_UNA <  SEG_ACK(packet) &&
                SEG_ACK(packet) <= tcp_data->SND_NXT)
            {

                /*Updates the RTO based on a recieved packet*/
                chilog(DEBUG, "ack was OK - processing data");
                update_rto(si, entry, packet);
                uint32_t len = SEG_ACK(packet) - tcp_data->SND_UNA;

                circular_buffer_t *send = &tcp_data->send;
                int buff_count = circular_buffer_count(send);

                /*If len > 0, packet acks data currently in the send buffer*/
                if(len && buff_count)
                    circular_buffer_read(send, NULL, len, BUFFER_NONBLOCKING);

                //Update the TCP data
                tcp_data->SND_UNA = SEG_ACK(packet);

                //If it is appropriate to udate the SND_WND
                if ((tcp_data->SND_WL1 < SEG_SEQ(packet)) ||
                    ((tcp_data->SND_WL1 == SEG_SEQ(packet)) &&
                    (tcp_data->SND_WL2 <= SEG_ACK(packet))))
                {
                    //Update the SND_WND
                    tcp_data->SND_WND = SEG_WND(packet);
                    tcp_data->SND_WL1 = SEG_SEQ(packet);
                    tcp_data->SND_WL2 = SEG_ACK(packet);
                    if (tcp_data->SND_WND == 0)
                        chilog(CRITICAL,
                                "Zero window received. TCP may become stuck.");
                }
            }

            /*If the ACK is a duplicate it can be ignored*/
            else if (SEG_ACK(packet) <= tcp_data->SND_UNA)
            {
                chilog(DEBUG, "ERR: ACK is a duplicate");
            }

            /*If the ACK acks something not yet sent
            then send an ACK, drop the segment, and return.*/
            else if (tcp_data->SND_NXT < SEG_ACK(packet)){
                chilog(ERROR, "ERR: Packet acks something not yet sent");

                /*Resend the current ACK*/
                uint32_t seq = tcp_data->SND_NXT, ack_seq = tcp_data->RCV_NXT;
                send_packet(si, entry, seq, ack_seq, NULL, 0, ACK);

                DROP(packet);
                return 0;
            }

            /*Any segments on the retransmission queue which are thereby
            entirely acknowledged are removed.*/
            clear_queue(si, entry);
            break;
        default:
            break;
    }

    //Ack bit is set, process these states accordingly
    switch(entry->tcp_state) //pg 73
    {
        case FIN_WAIT_1:
            /*if our FIN is now acknowledged then enter FIN-WAIT-2 and continue
              processing in that state.*/
            if (SEG_ACK(packet) == tcp_data->SND_NXT)
            {
              chitcpd_update_tcp_state(si, entry, FIN_WAIT_2);
            }
            break;
        case FIN_WAIT_2:
             /*If the retransmission queue is empty, the user's CLOSE can be
             acknowledged ("ok") but do not delete the TCB.*/
             break;
        case CLOSING:
            /* If the ACK acknowledges our FIN then enter the TIME-WAIT state,
            otherwise ignore the segment.*/
            if (SEG_ACK(packet) == tcp_data->SND_NXT)
            {
                chitcpd_update_tcp_state(si, entry, TIME_WAIT);
                //Immediately enter the CLOSED state
                chitcpd_update_tcp_state(si, entry, CLOSED);
            }
            else
            {
                DROP(packet);
                return 0;
            }
            break;
        case LAST_ACK:
            //Enter the CLOSED state
            chitcpd_update_tcp_state(si, entry, CLOSED);
            DROP(packet);
            return 0;
        case TIME_WAIT:
              /*The only thing that can arrive in this state is a
              retransmission of the remote FIN.  Acknowledge it, and restart
              the 2 MSL timeout.
              */
              break;
        default:
            break;
    }

    //Process packet data
    switch (entry->tcp_state) //pg. 74-75
    {
        case FIN_WAIT_1:
        case FIN_WAIT_2:
        case ESTABLISHED: ;
            uint32_t seq, ack;

            if (tcp_data->RCV_NXT < SEG_SEQ(packet))
            {
                /*Add to out of order queueu*/
                add_to_out_of_order(tcp_data->out_of_order_list, packet);
                //Should we update RCV_WND here?
                tcp_data->RCV_WND = tcp_data->RCV_WND - TCP_PAYLOAD_LEN(packet);
                //Send repeat ack packet
                seq = tcp_data->SND_NXT, ack = tcp_data->RCV_NXT;
                send_packet(si, entry, seq, ack, NULL, 0, ACK);
            }

            else if (TCP_PAYLOAD_LEN(packet))
            {
                //Writes the data and send an acknowledgment
                write_data(si, entry, packet);

                uint32_t rcv_nxt = tcp_data->RCV_NXT + TCP_PAYLOAD_LEN(packet);
                uint32_t rcv_wnd = tcp_data->RCV_WND - TCP_PAYLOAD_LEN(packet);
                tcp_packet_list_t *elt, *tmp;

                DL_FOREACH_SAFE(tcp_data->out_of_order_list, elt, tmp)
                {
                    if(SEG_SEQ(elt->packet) == rcv_nxt)
                    {
                        //Writes the data and send an acknowledgment
                        write_data(si, entry, elt->packet);

                        rcv_nxt = rcv_nxt + TCP_PAYLOAD_LEN(elt->packet);
                        rcv_wnd = rcv_wnd - TCP_PAYLOAD_LEN(elt->packet);

                        DL_DELETE(tcp_data->out_of_order_list, elt);
                        //Free the associated packet
                        chitcp_tcp_packet_free(elt->packet);
                        free(elt->packet);
                        //Free the packet_list instance
                        free(elt);
                    }

                    else
                    {
                        break;
                    }
                }

                //Update the TCP data
                tcp_data->RCV_NXT = rcv_nxt;
                tcp_data->RCV_WND = rcv_wnd;

                //Send an ACK acknowledging the data as a block
                seq = tcp_data->SND_NXT;
                ack = tcp_data->RCV_NXT;
                send_packet(si, entry, seq, ack, NULL, 0, ACK);
            }

            /*Piggy back this acknowledgment on data in the send buffer*/
            packetize_data(si, entry);
            break;
        case CLOSING:
        case LAST_ACK:
        case TIME_WAIT:
        case CLOSE_WAIT:
            /*This should not occur, since a FIN has been received from the
            remote side.  Ignore the segment text.*/
            chilog(ERROR, "ERR: unexpected packet");
            DROP(packet);
            return 0;
        default:
            break;
    }

HANDLE_FIN:
    //Handle any FIN packets
    if (header->fin) //pg. 75 - 76
    {
        //Advance RCV_NXT over the FIN
        tcp_data->RCV_NXT = SEG_SEQ(packet) + 1;
        //clear_queue(si, entry);

        //Send an acknowledgment for the FIN packet
        uint32_t seq = tcp_data->SND_NXT, ack = tcp_data->RCV_NXT;
        send_packet(si, entry, seq, ack, NULL, 0, ACK);

        /*If the FIN bit is set, signal the user "connection closing" and
        return any pending RECEIVEs with same message*/
        switch (entry->tcp_state)
        {
            case SYN_RCVD:
            case ESTABLISHED:
                //Enter the CLOSE_WAIT state
                chitcpd_update_tcp_state(si, entry, CLOSE_WAIT);
                break;
            case FIN_WAIT_1:
                /*Deviates from RFC 793
                See pg. 75 for specified behavior*/

                //Enter the CLOSING state
                chitcpd_update_tcp_state(si, entry, CLOSING);
                break;
            case FIN_WAIT_2:
                //Enter the TIME_WAIT state
                chitcpd_update_tcp_state(si, entry, TIME_WAIT);
                //Immediately enter the CLOSED state
                chitcpd_update_tcp_state(si, entry, CLOSED);

                //Start the time_wait timer and turn off other timers
                break;
            case CLOSE_WAIT:
            case CLOSING:
            case LAST_ACK:
                /*remain in state*/
                break;
            case TIME_WAIT:
                /*Restart the 2 MSL time-wait timeout.*/
                break;
            //Closed, LISTEN, SYN_SENT
            default:
                /*Do not process FIN if the state is CLOSED, LISTEN or SYN-SENT
                since the SEG.SEQ cannot be validated; drop the segment and
                return.*/
                DROP(packet);
                return 0;
        }
    }
    //Drop the packet and return after proccessing
    DROP(packet);
    return 0;
}

void retransmission_handler(serverinfo_t *si, chisocketentry_t *entry)
{
    /* Handle retransmission queue */
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    tcp_packet_list_t *packet_list = tcp_data->retransmission_queue;
    tcp_packet_list_t *temp;

    DL_FOREACH(packet_list, temp)
    {
        uint32_t seq = SEG_SEQ(temp->packet);
        uint32_t len = TCP_PAYLOAD_LEN(temp->packet);

        chilog(MINIMAL, "Resending SEQ = %d:%d", seq, seq + len);
        chitcpd_send_tcp_packet(si, entry, temp->packet);
        temp->has_retransmitted = true;
    }

    tcp_data->RTO = tcp_data->RTO * 2;

    /* Reset timer - requires canceling and setting again, not just setting */
    set_timer(si, entry, RETRANSMISSION);
}


void persist_handler(serverinfo_t *si, chisocketentry_t *entry)
{
    chilog(MINIMAL, "Handling persist timeout");
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    circular_buffer_t *buff = &tcp_data->send;
    uint8_t *payload;
    uint32_t payload_size = 1, seq = tcp_data->SND_NXT, ack = tcp_data->RCV_NXT;

    if(tcp_data->persist_sent)
    {
        chilog(MINIMAL, "This persist timer timed out twice");
        chitcpd_send_tcp_packet(si, entry, tcp_data->persist_packet);
        return;
    }

    bool nothing_to_send = circular_buffer_count(&tcp_data->send) == 0;
    if(nothing_to_send)
    {
        chilog(MINIMAL, "There was nothing to send in this window");
        set_timer(si, entry, PERSIST);
        chilog(MINIMAL, "Timer reset");
        tcp_data->persist_sent = false;
    }

    else
    {
        chilog(MINIMAL, "There was something to send in this window");
        //Allocate a buffer to read payload data into
        payload = (uint8_t *) calloc(payload_size, sizeof(uint8_t));

        chilog(DEBUG, "Peeking %d bytes from SEND_BUFF", payload_size);
        //Peek data into the buffer
        circular_buffer_peek_at(buff, payload, seq, payload_size);
        tcp_packet_t *packet;
        packet = send_packet(si, entry, seq, ack,
                                        payload, payload_size, ACK | NRT);
        tcp_data->persist_packet = packet;
        set_timer(si, entry, PERSIST);
        tcp_data->persist_sent = true;
    }
}

/*Helper functions*/

void add_to_out_of_order(tcp_packet_list_t *out_of_order, tcp_packet_t *packet)
{
    // if seq less than any packets in out of order list insert in list
    tcp_packet_list_t *temp;
    tcphdr_t *header_pack = TCP_PACKET_HEADER(packet);

    for (temp = out_of_order; temp != NULL; temp = temp->next)
    {
        tcphdr_t *header_ooo = TCP_PACKET_HEADER(temp->packet);

        if (SEG_SEQ(temp->packet) > SEG_SEQ(packet))
        {
            chitcp_packet_list_prepend(&temp, packet);
            return;
        }
    }

    // if seq is greater than all packets in out of order list, append
    chitcp_packet_list_append(&out_of_order, packet);
    return;
}

void clear_queue(serverinfo_t *si, chisocketentry_t *entry)
{
    chilog(MINIMAL, "Clearing RETRANSMISSION QUEUE");
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    tcp_packet_list_t *packet_list = tcp_data->retransmission_queue;
    tcp_packet_list_t *elt, *tmp;

    //If all our segments have been acked
    if (tcp_data->SND_UNA == tcp_data->SND_NXT)
    {
        chitcp_packet_list_destroy(&tcp_data->retransmission_queue);
        tcp_data->retransmission_queue = NULL;
        mt_cancel_timer(&tcp_data->mt, RETRANSMISSION);
        return;
    }


    DL_FOREACH_SAFE(packet_list, elt, tmp)
    {
        //search until we find a packet that was sent after our ack
        if (tcp_data->SND_UNA <= SEG_SEQ(elt->packet))
        {
            break;
        }

        chitcp_packet_list_pop_head(&tcp_data->retransmission_queue);

        //Free the associated packet
        chitcp_tcp_packet_free(elt->packet);
        free(elt->packet);
        //Free the packet_list instance
        free(elt);
    }

    if(!elt)
        mt_cancel_timer(&tcp_data->mt, RETRANSMISSION);

    return;
}

void write_data(serverinfo_t *si, chisocketentry_t *entry, tcp_packet_t *packet)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    circular_buffer_t *recv = &tcp_data->recv;
    int buff_count = circular_buffer_count(recv);
    int buff_max   = circular_buffer_capacity(recv);
    uint8_t *payload = TCP_PAYLOAD_START(packet);
    uint32_t len = TCP_PAYLOAD_LEN(packet);

    chilog_tcp(DEBUG, packet, LOG_INBOUND);
    chilog(DEBUG, "RECV_BUFF: %d / %d Bytes filled",
                   buff_count, buff_max);

    int written = circular_buffer_write(recv, payload,
                                        len, BUFFER_BLOCKING);

    buff_count = circular_buffer_count(recv);
    chilog(DEBUG, "RECV_BUFF: %d / %d Bytes filled | %d Bytes written",
                     buff_count, buff_max, written);

    return;
}

bool packet_is_acceptable(tcp_packet_t *packet, tcp_data_t *tcp_data)
{
    //Extract packet data
    tcphdr_t *header = TCP_PACKET_HEADER(packet);
    size_t packet_len = TCP_PAYLOAD_LEN(packet);
    uint32_t packet_seq = SEG_SEQ(packet);
    uint32_t rcv_wnd = tcp_data->RCV_WND;
    uint32_t rcv_nxt = tcp_data->RCV_NXT;

    /*Perform the appropriate test
      depending on the values of RCV_WND and the
      size of the packet's payload*/
    if (packet_len == 0){
        if (rcv_wnd == 0){
            return (packet_seq == rcv_nxt);
        }
        else{
            return (rcv_nxt <= packet_seq && packet_seq < (rcv_nxt + rcv_wnd));
        }
    }

    else{
        if (rcv_wnd == 0){
            return false;
        }
        else {
            return((rcv_nxt <= packet_seq && packet_seq < (rcv_nxt + rcv_wnd)) ||
              (rcv_nxt <= (packet_seq + packet_len - 1) &&
              (packet_seq + packet_len - 1) < (rcv_nxt + rcv_wnd)));
        }
    }

    chilog(ERROR, "ERR: unaccounted-for quality of packet");
    return false;
}

tcp_packet_t *send_packet(serverinfo_t *si, chisocketentry_t *entry, uint32_t seq,
                 uint32_t ack, void *payload, size_t payload_size, int flags)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;

    //Intialize a new packet
    tcp_packet_t *packet = (tcp_packet_t *) calloc(1, sizeof(tcp_packet_t));
    chitcpd_tcp_packet_create(entry, packet, payload, payload_size);

    //Intiialize and populate the header
    tcphdr_t *header = TCP_PACKET_HEADER(packet);
    header->seq = WR_PACKET(seq);
    header->ack_seq = WR_PACKET(ack);
    header->ack = (flags & ACK) ? true : false;
    header->syn = (flags & SYN) ? true : false;
    header->fin = (flags & FIN) ? true : false;
    header->win = chitcp_ntohs(tcp_data->RCV_WND);

    //Send the packet
    chitcpd_send_tcp_packet(si, entry, packet);

    //Update SND_NXT
    tcp_data->SND_NXT += payload_size;

    //Add the packet to the retransmission queue
    if(!(flags & NRT))
    {
        chilog(DEBUG, "appending to retransmission queue");
        //List implicitly ordered by send order
        tcp_packet_list_t *packet_list = calloc(1, sizeof(tcp_packet_list_t));
        packet_list->packet = packet;


        struct timespec now;
        struct timespec *time_created = &packet_list->time_created;
        clock_gettime(CLOCK_REALTIME, &now);

        time_created->tv_nsec = now.tv_nsec;
        time_created->tv_sec = now.tv_sec;

        packet_list->has_retransmitted = false;

        DL_APPEND(tcp_data->retransmission_queue, packet_list);

        single_timer_t *rt_timer;
        mt_get_timer_by_id(&tcp_data->mt, RETRANSMISSION, &rt_timer);

        if (!rt_timer->active)
        {
            set_timer(si, entry, RETRANSMISSION);
        }
    }

    return packet;
}

void packetize_data(serverinfo_t *si, chisocketentry_t *entry)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    circular_buffer_t *buff = &tcp_data->send;
    uint32_t nbytes = circular_buffer_next(buff) - tcp_data->SND_NXT;
    uint32_t nbytes_sent = 0;
    uint32_t payload_size;
    uint32_t seq;
    uint32_t ack;
    //Keeps track of how much data the peer can accept
    uint32_t effective_window = tcp_data->SND_WND
                              - (tcp_data->SND_NXT - tcp_data->SND_UNA);

    /*If we are closing or advertised window is 0*/
    if(tcp_data->closing || !tcp_data->SND_WND)
        return;

  /*Otherwise if there bytes to send and a place to put them*/
    while ((nbytes) && (effective_window))
    {
        chilog(DEBUG, "%d Bytes in SEND_BUFF", nbytes);
        chilog(DEBUG, "RCV_WND: %d bytes", effective_window);
        chilog(DEBUG, "Peeking at: SEQ = %d", tcp_data->SND_NXT);

        /*Make the payload as large as possible
         Without violating the MSS or
         the peers RCV_WND*/
        if (nbytes <= effective_window)
        {
           payload_size = MIN(nbytes, TCP_MSS);
        }
        else
        {
            payload_size = MIN(effective_window, TCP_MSS);
        }

        //Decrement the effective window
        effective_window = effective_window - payload_size;
        nbytes = nbytes - payload_size;

        //Allocate a buffer to read payload data into
        uint8_t *payload = (uint8_t *) calloc(payload_size, sizeof(uint8_t));

        chilog(DEBUG, "Peeking %d bytes from SEND_BUFF", payload_size);

        //Peek data into the buffer
        circular_buffer_peek_at(buff, payload, tcp_data->SND_NXT, payload_size);

        seq = tcp_data->SND_NXT, ack = tcp_data->RCV_NXT;
        send_packet(si, entry, seq, ack, payload, payload_size, ACK);
        free(payload);
    }
    return;
}

void send_timeout(multi_timer_t *mt, single_timer_t *timer, void *args)
{
    chilog(MINIMAL, "time has expired on timer %d at %p - running callback", timer->id, timer);
    socket_info_t *socket_info = (socket_info_t *) args;
    serverinfo_t *si = socket_info->si;
    chisocketentry_t *entry = socket_info->entry;
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    chitcpd_timeout(si, entry, timer->id);
    free(socket_info);
}


void set_timer(serverinfo_t *si, chisocketentry_t *entry, uint16_t id)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    uint64_t timeout = tcp_data->RTO;
    /* this will be freed in send_timeout */
    socket_info_t *socket_info = calloc(1, sizeof(socket_info_t));
    socket_info->si = si;
    socket_info->entry = entry;
    chilog(DEBUG, "Setting timer %d for %lu ns", id, timeout);
    mt_set_timer(&tcp_data->mt, id, timeout, send_timeout,
                                                (void *) socket_info);
}

void update_rto(serverinfo_t *si, chisocketentry_t *entry, tcp_packet_t *packet)
{
    tcp_data_t *tcp_data = &entry->socket_state.active.tcp_data;
    struct timespec now, r;
    tcp_packet_list_t *packet_list = tcp_data->retransmission_queue;
    tcp_packet_list_t *temp;

    chilog(DEBUG, "Updating the RTO...");
    chilog(DEBUG, "Measuring agsint: ACK = %d", SEG_ACK(packet));

    /*Find the acked_packet in the retransmission_queue s.t.
      SEG_SEQ(acked_packet) + TCP_PAYLOAD_LEN(akced_packet) = SEG_ACK(packet)*/
    DL_FOREACH(packet_list, temp)
    {
        tcp_packet_t *acked_packet = temp->packet;
        uint32_t last_acked_byte = SEG_SEQ(acked_packet) +
                                        TCP_PAYLOAD_LEN(acked_packet);
        if(last_acked_byte == SEG_ACK(packet))
        {
            chilog(DEBUG, "Using packet: SEQ = %d:%d", SEG_SEQ(acked_packet),
                                                         last_acked_byte);
            break;
        }

        else if(TCP_PACKET_HEADER(acked_packet)->syn &&
               (TCP_PACKET_HEADER(packet)->syn &&
                TCP_PACKET_HEADER(packet)->ack))
        {
            chilog(DEBUG, "Using packet: SEQ = %d:%d", SEG_SEQ(acked_packet),
                                                         last_acked_byte);
            break;
        }
    }

    if (!temp)
    {
        chilog(DEBUG, "Nothing to measure this packet against");
        return;
    }

    else if (temp->has_retransmitted)
    {
        //Such packets can't provide proper R values
        chilog(DEBUG, "ERR: Can't measure this packet appropriately");
        return;
    }

    struct timespec *time_created = &temp->time_created;
    clock_gettime(CLOCK_REALTIME, &now);

    if (timespec_subtract(&r, &now, time_created))
    {
        chilog(ERROR, "Calculated negative RTT");
        return;
    }

    //result in nanoseconds
    uint64_t r_val = (uint64_t) (r.tv_nsec * NANOSECOND) +
                                (r.tv_sec * SECOND);

    //Takes the resulting value of r in nanosecond
    //updates the RTO appropriately
    chilog(DEBUG, "Updating RTO: R = %luns", r_val);
    chilog(DEBUG, "Before RTO = %luns", tcp_data->RTO);

    /*If this is our first R measurement*/
    if(tcp_data->first_r) //If this is our first measurement
    {
        tcp_data->SRTT = r_val;
        tcp_data->RTTVAR = r_val / 2;
        tcp_data->RTO = tcp_data->SRTT + MAX(G, K * tcp_data->RTTVAR);
    }

    else
    {
        /*RTTVAR <- (1 - beta) * RTTVAR + beta * |SRTT - R'|
        SRTT <- (1 - alpha) * SRTT + alpha * R'
        The above SHOULD be computed using alpha=1/8 and beta=1/4*/

        tcp_data->RTTVAR = ((3 * r_val) / 4) +
                           (1 * (tcp_data->SRTT > r_val ?
                           (tcp_data->SRTT - r_val) : (r_val - tcp_data->SRTT))
                            / 4);

        tcp_data->SRTT = ((7 * tcp_data->SRTT) / 8) + ((1 * r_val) / 8);

        tcp_data->RTO = tcp_data->SRTT + MAX(G, K * tcp_data->RTTVAR);
    }

    if(tcp_data->RTO < MIN_RTO)
        tcp_data->RTO = MIN_RTO;

    chilog(DEBUG, "After RTO  = %luns", tcp_data->RTO);
    tcp_data->first_r = false;
    return;
}
