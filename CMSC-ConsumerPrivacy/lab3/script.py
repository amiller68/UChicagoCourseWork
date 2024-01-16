import dpkt

path = 'secure.pcap'
f = open(path, 'rb')
pcap = dpkt.pcap.Reader(f)
qnames = []
for ts, buf in pcap:
    #make sure we are dealing with IP traffic
    try:
        eth = dpkt.ethernet.Ethernet(buf)
    except:
        continue
    if eth.type != 2048:
        continue
    #make sure we are dealing with UDP protocol
    try:
        ip = eth.data
    except:
        continue
    try:
        udp = ip.data
    except:
        continue
    # if udp.sport != 53 and udp.dport != 53:
    #     continue
    #make the dns object out of the udp data and
    #check for it being a RR (answer) and for opcode QUERY
    try:
        dns = dpkt.dns.DNS(udp.data)
    except:
        continue
    # if dns.qr != dpkt.dns.DNS_R:
    #     continue
    # if dns.opcode != dpkt.dns.DNS_QUERY:
    #     continue
    # if dns.rcode != dpkt.dns.DNS_RCODE_NOERR:
    #     continue
    if len(dns.an) < 1:
        continue
    #process and print responses based on record type
    for qname in dns.qd:
        print("Query: ", qname.name)
        qnames.append(qname.name)

print("Sorted Queries: ")
data = []
for x in qnames:
    d = x.strip().split('.')
    d.reverse()
    if d not in data:
        data.append(d)
data.sort()
for y in data:
    y.reverse()
    print('.'.join(y))

