//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------

package pkt_pkg;

  /////////////////////////////////////////////////////////////////////////
  // Parameters and enums
  localparam W_GLORT       =  16;
  localparam W_SWPRI       =   4;
  localparam W_MAC         =  48;
  localparam W_2MAC        = 2 * W_MAC;
  localparam W_VLAN        =  32; //vlan tag including ethertype
  localparam W_VPRI_VID    =  16;
  localparam W_VID         =  12;
  localparam W_VPRI        =   4;
  localparam W_ETHERTYPE   =  16;
  localparam W_IP6         = 128; //(bits) SIP, DIP
  localparam W_IP4         =  32; //(bits) SIP, DIP
  localparam W_MPLS_LABEL  =  32;
  localparam W_DSCP        =   6;
  localparam W_DS          =   8;

  localparam W_IP_VERSION       = 4;
  localparam W_IPV4_IHL         = 4;
  localparam W_IP_DSCP          = W_DSCP;
  localparam W_IP_ECN           = 2;
  localparam W_IP_LENGTH        = 16;
  localparam W_IPV4_ID          = 16;
  localparam W_IPV4_FLAGS       = 3;
  localparam W_IP_FRAGOFFSET    = 13; // shared with IPv6 frag extension header
  localparam W_IP_TTL           = 8;  // shared with IPv6 Hop Limit
  localparam W_IP_PROTOCOL      = 8;  // shared with IPv6 Next Header
  localparam W_IP_OPTIONS       = 320; // maximum options field (40 bytes)
  localparam W_IPV4_CHECKSUM    = 16;
  localparam W_IPV6_FLOW        = 20;

  localparam W_L4_PORT          = 16;
  localparam W_TCP_SEQ          = 32;
  localparam W_TCP_ACK          = 32;
  localparam W_TCP_HL           = 4;
  localparam W_TCP_RSV          = 3;
  localparam W_TCP_FLAGS        = 9;
  
  localparam N_ICMP_CODE        = 8'd1;
  localparam N_IGMP_CODE        = 8'd2;
  localparam N_TCP_CODE         = 8'd6;
  localparam N_UDP_CODE         = 8'd17;
  localparam N_IPV6_ICMP_CODE   = 8'd58;

  localparam MAC_CONTROL_ADDR   = 48'h0180C2000001;
  localparam MAC_BROADCAST_ADDR = 48'hffffffffffff;
  localparam MAC_B_MULTICAST    = 40;
  localparam PAUSE_ETHERTYPE    = 16'h8808;
  localparam PAUSE_NORMAL_CODE  = 16'h0001;
  localparam PAUSE_CB_CODE      = 16'h0101;

  // proto_t: IP protocol numbers
  typedef enum logic [7:0] {
    PROT_6E_HBH   = 'd0,
    PROT_TCP      = 'd6,
    PROT_UDP      = 'd17,
    PROT_6E_ROUTE = 'd43,
    PROT_6E_FRAG  = 'd44,
    PROT_GRE      = 'd47,
    PROT_6E_AH    = 'd51,
    PROT_6E_DST   = 'd60,
    PROT_ERR      = 'd255 // a code we can use to stop L3 ipv6 ext parsing
// The following are treated as any other L4 protocol.
//      PROT_6E_ESP   = 'd50,
//      PROT_6E_MOB   = 'd135
   } proto_t;

  // ether_type_t: ethertypes
  typedef enum logic [15:0] {
    ET_IPV4 = 'h0800,
    ET_ARP  = 'h0806,
    ET_VLAN = 'h8100,
    ET_IPV6 = 'h86dd
  } ether_type_t;

  typedef enum logic [1:0] {
    ECN_NON   = 'b00, // Non ECN-Capable Transport
    ECN_ECT0  = 'b10, // ECN Capable Transport
    ECN_ECT1  = 'b01, // ECN Capable Transport
    ECN_CE    = 'b11  // Congestion Encountered; value used when marking
  } ecn_t;

  localparam MPLS_ELI_LABEL = 20'h0_0007;


  /////////////////////////////////////////////////////////////////////////
  // Structures

  typedef logic   [ 7:0] octet_t;       // byte; for ease of coding and debugging
  typedef logic   [15:0] csum_t;        // L3 or L4 checksum
  typedef logic   [31:0] fcs_t;         // cyclic redundancy check (CRC) for FCS
  typedef octet_t [0:5]  mac_t;         // lintra s-2028 "big endian" // MAC address, canonical byte ordering
  typedef octet_t [0:3]  ipv4_addr_t;   // lintra s-2028 "big endian" // IPv4 address, canonical byte ordering
  typedef octet_t [0:15] ipv6_addr_t;   // lintra s-2028 "big endian" // IPv6 address, canonical byte ordering
  typedef logic   [15:0] ip_len_t;      // IP length for both v4 and v6
  typedef logic   [ 7:0] ttl_t;         // Time to live
  
  // vpri_t: packed VLAN priority
  typedef struct packed {
    logic  [2:0]  pcp;  // Priority Code Point
    logic         dei;  // Discard Eligible Information
  } vpri_t;

  // vlan_t: packed VLAN tag
  typedef struct packed {
    ether_type_t  tpid; // Tag Protocol Identifier
    vpri_t        vpri; // VLAN Priority
    logic [11:0]  vid;  // VLAN Identifier
  } vlan_t;

  // mpls_t: packed MPLS label
  typedef struct packed {
    logic [19:0]  label;
    logic  [2:0]  exper;  // experimental use; traffic class for QoS and ECN
    logic         s;      // bottom of stack
    ttl_t         ttl;    // time-to-live
  } mpls_t;

  // ds_t: Differentiated Services, used in both IPv4 and IPv6 headers
  typedef struct packed {
    logic  [5:0]  dscp;
    ecn_t         ecn;
  } ds_t;

  // ipv4_hdr_t: packed L3 header (IPv4 case)
  typedef struct packed {
    logic  [3:0]  version; // == 4
    logic  [3:0]  ihl;     // header length in 32-bit words
    ds_t          tos;
    ip_len_t      length;  // header+payload length in bytes
    logic [15:0]  id;
    logic  [2:0]  flags;
    logic [12:0]  frag_offset;
    ttl_t         ttl;
    proto_t       prot;
    csum_t        header_csum; // inverted checksum (0 = not present)
    ipv4_addr_t   sip;
    ipv4_addr_t   dip;
  } ipv4_hdr_t;

  // ipv6_hdr_t: packed L3 header (IPv6 case)
  typedef struct packed {
    logic  [3:0]  version; // == 6
    ds_t          tc;     // traffic class
    logic [19:0]  flow_label;
    ip_len_t      length; // payload length incl. extensions == total-40
    proto_t       prot;   // a.k.a. next_header
    ttl_t         ttl;    // a.k.a. hop_limit
    ipv6_addr_t   sip;
    ipv6_addr_t   dip;
  } ipv6_hdr_t;

  // udp_hdr_t: packed L4 header (UDP case)
  typedef struct packed {
    logic [15:0]  src;
    logic [15:0]  dst;
    logic [15:0]  length;
    csum_t        csumb; // inverted checksum (0 = not present)
  } udp_hdr_t;

  // tcp_hdr_t: packed L4 header (TCP case)
  typedef struct packed {
    logic [15:0]  src;
    logic [15:0]  dst;
    logic [31:0]  seq;
    logic [31:0]  ack;
    logic  [3:0]  hlen; // header length in 32-bit words
    logic [11:0]  flags;
    logic [15:0]  window;
    csum_t        csumb; // inverted checksum (0 = not present)
    logic [15:0]  urgent;
  } tcp_hdr_t;

  // vxlan_hdr_t: packed VXLAN/UDP tunnel header
  typedef struct packed {
    udp_hdr_t     udp;
    logic  [7:0]  flags;
    logic [23:0]  reserved1;
    logic [23:0]  vni;
    logic  [7:0]  reserved2;
  } vxlan_hdr_t;

  // nvgre_hdr_t: packed NVGRE tunnel header (no UDP)
  typedef struct packed {
    logic  [1:0]  reserved1;
    logic         tni_present;
    logic  [9:0]  reserved2;
    logic  [2:0]  version;
    logic [15:0]  encap_prot;
    logic [23:0]  tni;
    logic  [7:0]  reserved3;
  } nvgre_hdr_t;

  // nge_hdr_t: packed NGE/UDP tunnel header
  typedef struct packed {
    udp_hdr_t     udp;
    logic  [1:0]  version;   // == 0 to enable decap
    logic  [5:0]  opt_len;   // number of 32-bit options (0..63)
    logic         oam;       // == 0 to enable decap
    logic         c;         // == 0 to enable decap
    logic  [5:0]  reserved1; 
    logic [15:0]  encap_prot;
    logic [23:0]  vni;
    logic  [7:0]  reserved2;
  } nge_hdr_t;

endpackage
