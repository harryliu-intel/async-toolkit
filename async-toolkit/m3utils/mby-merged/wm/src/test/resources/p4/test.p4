#include <v1model.p4>

const bit<16> TYPE_IPV4 = 16w0x0800;
typedef bit<48>  MACAddress;
typedef bit<32>  IPv4Address;

header Ethernet_h
{
    MACAddress dstAddr;
    MACAddress srcAddr;
    bit<16> etherType;
}

header Ipv4_h {
    bit<4>       version;
    bit<4>       ihl;
    bit<8>       diffserv;
    bit<16>      totalLen;
    bit<16>      identification;
    bit<3>       flags;
    bit<13>      fragOffset;
    bit<8>       ttl;
    bit<8>       protocol;
    bit<16>      hdrChecksum;
    IPv4Address  srcAddr;
    IPv4Address  dstAddr;
}

header User_Meta {
	bit<9> src_port;
    bit<1> drop;
	bit<22> padding;
}

struct Usr_Meta {
	bit<9> src_port;
    bit<1> drop;
	bit<22> padding;
}

struct Headers_t {
    Ethernet_h  ethernet;
	User_Meta   metadata;
    Ipv4_h      ipv4;
}

parser Prs(packet_in p, 
              out Headers_t headers, 
              inout Usr_Meta usr_meta,
              inout standard_metadata_t standard_metadata) 
{
    state start {
        transition parse_ethernet;
    }

    state parse_ethernet {
        p.extract(headers.ethernet);

        transition select(headers.ethernet.etherType) {
            TYPE_IPV4 : parse_ipv4;
        }
    }

    state parse_ipv4 {
        p.extract(headers.ipv4);
        transition accept;
    }
}

control VerifyChk(inout Headers_t headers,
                       inout Usr_Meta usr_meta)
{
    apply {
    }
}

control Ingr(inout Headers_t headers,
             inout Usr_Meta usr_meta,
             inout standard_metadata_t standard_metadata) 
{
    action Drop_action() {
        standard_metadata.drop = 1;
        standard_metadata.egress_spec = 0;
		usr_meta.drop = 1;
    }

    action Set_nhop(bit<9> port) {
        standard_metadata.egress_spec = port;
		usr_meta.drop = 0;
    }

    table eth_nhop {
        key = { 
            standard_metadata.ingress_port: exact; 
        }
        actions = {
            Drop_action;
            Set_nhop;
        }
        default_action = Drop_action;
    }

    apply {
        eth_nhop.apply(); 
		usr_meta.src_port = standard_metadata.ingress_port;
    }
}

control Egr(inout Headers_t headers,
                  inout Usr_Meta usr_meta,
                  inout standard_metadata_t standard_metadata) 
{
	action Add_meta() {
        headers.metadata.src_port = usr_meta.src_port;
		headers.metadata.drop = usr_meta.drop;
    }

	table add_meta {
			key = { 
				standard_metadata.ingress_port: exact; 
			}
			actions = {
				NoAction;
				Add_meta;
			}
			default_action = NoAction;
		}

    apply {
		add_meta.apply();
    }
}

control ComputeChk(inout Headers_t headers,
                        inout Usr_Meta usr_meta)
{
    apply {
    }
}

control Deprs(packet_out b,
              in Headers_t headers) 
{
    apply {
		b.emit(headers.metadata);
        b.emit(headers.ethernet);
        b.emit(headers.ipv4);
    }
}

V1Switch(Prs(), 
         VerifyChk(), 
         Ingr(), 
         Egr(), 
         ComputeChk(), 
         Deprs()) main;