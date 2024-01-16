import json
import sys
from tabulate import tabulate

#control output range
start_out = 0
end_out = 0

#might be helpful to process data and then look at packets
#should take the form of '[ [ip_src, ip_dst, port ... ] ]'
packets_dis = []
server_ip = "128.135.24.28"

#keeps track of what ports are currently having conversations
#port appended on SYN and closed on FIN, ACK
#Port # should disgnate dictionary that contains: total byte transmission
#                                               : Start and end packet
active_ports = {}
#Keeps track of finsihed conversations
#sorts them by completion time
finished_convos = []
monitor_time = 0

active_flow = {}
finished_flows = []

#turns a packet into its relevant components
def process_packet(packet, i):
    #Declare the packet Dissection we're gonna build
    packet_dis = {}


    #Break packet into relevant sections
    ip_data  = packet["_source"]["layers"]["ip"]
    tcp_data = packet['_source']['layers']['tcp']
    #populate our dict
    packet_dis['src_port'] = tcp_data["tcp.srcport"]
    packet_dis['dst_port'] = tcp_data["tcp.dstport"]
    packet_dis['seq']      = tcp_data["tcp.seq"]
    packet_dis['ack']      = tcp_data["tcp.ack"]
    packet_dis['src_ip']   = ip_data["ip.src"]
    packet_dis['dst_ip']   = ip_data["ip.dst"]
    packet_dis['i']        = i
    packet_dis['time']     = packet['_source']['layers']['frame']['frame.time_relative']
    return packet_dis


def filter_packet(packet, prev_packet):
    #Filter Duplicate Packets
    
    #only want packets coming from the client
    if( packet['src_ip'] == server_ip ):
        return False

    if( {k: v for k,v in prev_packet.items() if k not in ['time']} =={k: v for k,v in packet.items() if k not in ['time']}):
        return False

    return True
'''
I want to filter packet segments as being related to each other
if a series of packets share th


'''
#keeps track of whether packet is part of ongoing
#conversation; collected finsihed convos in list
def mark_port(packet):
    port = int(packet['src_port'])
    seq = int(packet['seq'])
    ack = int(packet['ack'])
    time = float(packet['time'])
    if not (port in active_ports.keys()):
        active_ports[port] = { 
                                "port"       : port,
                                "start_time" : time,
                                "end_time"   : time,
                                "seq"        : seq,
                                "ack"        : ack,
                                "d_ack"      : 0,
                                "l_ack"      : 0,
                                "u_ack"       : 0,
                                "p_count"    : 1}
        return
    #d_ack set to the ack val before the first time  which seqi == seq i + 1
    #convo also marked as done when d_ack != 0 and seqi != seqi+1
    else:
        convo = active_ports[port]
        count = convo['p_count']
        #Convo hasnt captured page data
        if ( convo['d_ack'] == 0):
            #If packet indicates start of page data
            if ( convo['seq'] == seq ):
                #D_ack = the last ack
                convo['d_ack'] = convo['ack']
        if ( count == 3 ):
            convo['u_ack'] = ack
            convo['l_ack'] = ack
        elif (count == 5 ):
            convo['l_ack'] = ack
        #Packet still collecting; take note and return
        convo['ack'] = ack
        convo['seq'] = seq
        convo['end_time'] = time
        convo['p_count'] = count + 1
        return


    return

def assoc_flow():
    global active_ports
    print("------------------End of Series-----------------")
    u_size = 0
    l_size = 0
    d_size = 0
    ports = 0
    for port in active_ports:
        convo = active_ports[port]
        ports = ports + 1
        d_size = d_size + (convo['ack'] - convo['d_ack'])
        l_size = l_size + (convo['ack'] - convo['l_ack'])
        u_size = u_size + (convo['ack'] - convo['u_ack'])
        print("# Ports: {}".format(port))
        print("Upper Size: {}".format((convo['ack'] - convo['d_ack']) ))
        print("Lower Size: {}".format(convo['ack'] - convo['l_ack']))
        print("D Size: {}".format(convo['ack'] - convo['u_ack']))
        finished_convos.append(convo)

    avg_size = (l_size + u_size) // 2

    print("# Ports: {}".format(ports))
    print("Upper Size: {}".format(u_size))
    print("Lower Size: {}".format(l_size))
    print("Avg Size: {}".format(avg_size))
    print("D Size: {}".format(d_size))

    active_ports = {}

    return

def main(argv):
    monitor_time = 0.0
    start_out = 0
    end_out = 0
    if (len(argv) == 2):
        end_out = int(argv[1])
    elif (len(argv) == 3):
        start_out = int(argv[1])
        end_out = int(argv[2])


    pgs = []

    with open(argv[0], 'r') as packet_data:
        packets = json.load(packet_data)
        if( end_out == 0 ):
            end_out = len(packets)
        elif ( end_out > len(packets) ):
            end_out = len(packets)
        print("{:20} {:20} {:10} {:10} {:10} {:10} {:10}".format('Src_IP', 'Dst_IP', 'Src_prt', 'Dst_prt', 'Seq', 'Ack', 'PkNum'))

        prev_packet = {}

        
        monitor_time = float(process_packet(packets[start_out], 0)['time'])

        for i in range(start_out, end_out):
            packet_dis = process_packet(packets[i], i)
            #Contains all packets formatted into something tabulate can read
            if( filter_packet(packet_dis, prev_packet) ):
                delta_t = float(packet_dis['time']) - monitor_time
                monitor_time = float(packet_dis['time'])
                if( delta_t > 3.0 ):
                    pgs = sorted(pgs, key = lambda k: k['src_port'])
                    for pg in pgs:
                        print("{:15} {:20} {:<20} {:10} {:10} {:10} {:10} {:10}".format(pg['time'], pg['src_ip'], pg['dst_ip'], pg['src_port'], pg['dst_port'], pg['seq'], pg['ack'], pg['i']))
                    assoc_flow()
                    pgs = []
                    print("------------------New Series-----------------")
                prev_packet = packet_dis
                mark_port(packet_dis)
               # print("{:15} {:20} {:<20} {:10} {:10} {:10} {:10} {:10}".format(packet_dis['time'], packet_dis['src_ip'], packet_dis['dst_ip'], packet_dis['src_port'], packet_dis['dst_port'], packet_dis['seq'], packet_dis['ack'], str(i)))
                pgs.append(packet_dis)

    for port in active_ports:
        finished_convos.append(active_ports[port])

    #filter conversation; ones without page data are nixed
    sorted_convos = sorted(finished_convos, key = lambda k: k['start_time'])
        
    print("{:20} {:10} {:10} {:10} {:10} {:10} {:10}".format('Start_time', 'Count', 'Port', 'Seq', 'Ack', 'D_ack', 'Size'))
    for i in range(0, len(sorted_convos)):
        seq  = sorted_convos[i]['seq']
        ack  = sorted_convos[i]['ack']
        port = sorted_convos[i]['port']
        time = sorted_convos[i]['start_time']
        d_ack = sorted_convos[i]['d_ack']
        count = sorted_convos[i]['p_count']
        size = ack - d_ack
        if( d_ack != 0 ):
            print("{:20} {:20} {:10} {:10} {:10} {:10}".format(time, count, port, seq, ack, d_ack, size))

    return

if __name__ == "__main__":
    main(sys.argv[1:])
