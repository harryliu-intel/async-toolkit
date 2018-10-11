/*******************************************************************************
* INTEL CORPORATION PROPRIETARY INFORMATION
* Copyright 2017 Intel Corporation.
* All Rights Reserved.
*
*                                 XXXXXXXXXXXXXXXXXXX
*                           XXXXXXXX               XXXXXXX
*                      XXXXXX                           XXXX
*                   XXXX                                  XXXX
*                                                           XXX
*              XXX                                 XXX  XX   XXX
*           X  XXX               XXX               XXX       XXX
*          XX                    XXX               XXX        XX
*         XX   XXX   XXXXXXXX    XXXXX   XXXXXX    XXX        XX
*        XX    XXX   XXX   XXX   XXX   XXX    XXX  XXX       XXX
*       XX     XXX   XXX    XXX  XXX   XXX    XXX  XXX      XXX
*      XXX     XXX   XXX    XXX  XXX   XXXXXXXXXX  XXX     XXXX
*      XX      XXX   XXX    XXX  XXX   XXX         XXX   XXXX
*      XX      XXX   XXX    XXX  XXXXX  XXX  XXX   XXX  XXXX
*      XXX      XX   XXX    XXX   XXXX   XXXXXXX    XX  X
*      XXX
*       XXX                                           X
*        XXXX                                      XXXX
*         XXXXX                               XXXXXXXXX
*           XXXXXXXX                   XXXXXXXXXXXXX
*              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
*                  XXXXXXXXXXXXXXXXXXXXXX
*
*
* The information and source code contained herein is the exclusive
* property of Intel Corporation and may not be disclosed, examined or
* reproduced in whole or in part without explicit written
* authorization from the company.
*
*******************************************************************************/
/*******************************************************************************
  History:
  September 2017              Original, srking
*******************************************************************************/

/*******************************************************************************
  Requirements notes.
  These requirements came from all directions, usually cut/paste from emails.
  This is not a TODO list, but some notes about how the parser solved
  problems are integrated inline.s

-- 9/15/2017 ------------------------------------------------------------------
  Anjali Singhai:
  There is no need to have a separate profile for IPv4 and IPv4 Frag
    -- srking 10/4/2017 - Done -------------------------------

  SCTP profiles may not be required in the Switch (need to confirm with 5G
  guys), but for now I suggest we drop them.
    -- srking 10/4/2017 - Done -------------------------------

  In case of IPv6 over IPv6, extract only Source IPv6 for the inner header,
  for outer extract both Source and Dest

  Add profiles for double VLAN case (quite common)
    -- srking 10/4/2017 - Done -------------------------------

  Add profiles with MPLS headers ( Andrey to give the common cases here)
  [srking: see extensive MPLS notes below]

  NVGRE needs an 8 byte Extraction (VNI + sequence Number)
    -- srking 10/3/2017 --------------------------------------
    NVGRE forbids sequence number, so we don't parse this as
    NVGRE.  However, we do support K+S in regular GRE encap.
    ----------------------------------------------------------

  Drop all IP in IP profiles (No known use case) (can add later if
  something comes up)
    -- srking 10/4/2017 - Done -------------------------------

  Parser Most uses cases on Comms use some ports for Access network and
  some for Core network, which means they have a need for two different
  profiles for a parser. Particularly they would like to use the UDP ports
  differently on the two network.
    -- srking 10/4/2017 --------------------------------------
    The parser supports GTP-U and GTP-C on their two respective
    UDP ports.
    ----------------------------------------------------------

  Prefer to have 4-8 configurable UDP ports per PF for classifying
  different tunnels, the number of tunnel formats created using UDP is
  exploding.
    -- srking 10/3/2017 --------------------------------------
    The parser implements total of 24 ports shared by ALL UDP
    tunnel types.  The parser handles QUIC ports separately.
    ----------------------------------------------------------

  GTP over Vxlan eats up to 3 UDP ports to classify the packet.

  Should reserve some decent number of UDP ports for quic, do not have a
  recommendation.
    -- srking 10/3/2017 --------------------------------------
    The parser implements total of 24 QUIC ports shared by all
    PFs.  In the case of multi-node, a second pass to identify
    the packet's node must occurs before matching a port.
    ----------------------------------------------------------

--Anjali Singhai 9/22/2017 ----------------------------------------------------
  There is no request from customers to support nsh as of now.
    -- srking 9/25/2017 --------------------------------------
    Looks like nsh is in use, see Chilikin note below.
    Parser supports single NSH over VXLAN-GPE.
    ----------------------------------------------------------


-- Andrey Chilikin 9/25/2017 --------------------------------------------------
  Unfortunately MPLS can carry anything with no way to say what exactly. A
  mapping table label -> inner is required if multiple protocols are
  encapsulated in the MPLS, as there is no next header field in the MPS
  label.
    -- srking 10/4/2017 --------------------------------------
    The parser checks label->inner up to a max of 2 MPLS labels.
    ----------------------------------------------------------

  Combinations, we have seen so far for FVL:
  * Native (no tunnels in front of MPLS, only optional VLANs) MPLS with IP
    payload. Eth -> MPLS (max number I've seen so far: 16) -> IPv4/IPv6.
    Version of IP can be determined looking at first 4 bits after the last
    label: this is what FVL is doing.
    -- srking 9/25/2017 --------------------------------------
      Enabled MPLS after VLAN CTAG, max of 2 MPLS labels
      Unlike FVL, we look at the MPLS label, not next 4 bits of
      IP header.
    ----------------------------------------------------------

  * Native MPLS with Eth payload (MPLS pseudo wire, MPLS Martini), might
    contain MPLS Control Word (32bits) between the last label and payload.
    Eth -> MPLS -> Optional CW -> Eth frame * MPLS over gre. Similar to
    NVGRE, but instead of gre Key it uses single MPLS label. This is standard
    protocol for Juniper networks, being gradually replaced by MPLS over UDP.
    Again, it could be IP or Eth after the label: Eth -> IP -> gre -> MPLS ->
    Optional Eth -> IP.
    -- srking 10/4/2017 --------------------------------------
    The parser does not yet support Ethernet over MPLS.
    Finding concise direction in the many specs has been problematic.
    ----------------------------------------------------------

  * MPLS over UDP. Similar to VXLAN, but uses single MPLS label instead of
    VNI. This is standard encapsulation for Juniper networks. MPLS payload -
    could be IP or Eth. Eth -> IP -> UDP -> MPLS -> Optional Eth -> IP
    -- srking 10/4/2017 --------------------------------------
    MPLS over UDP not supported currently.
    ----------------------------------------------------------

  FVL parser does not support MPLS to protocol mapping tables, so it can
  support only single encapsulation type per device - IP or Ethernet. For
  MPLSoGRE/MPLSoUDP we had a discussion with Juniper asking them to reserve
  one bit in the MPLS label for encapsulated protocol type indication. But
  they are not very kin of it, as it requires a lot of changes in their
  routers label assignment mechanism.

  AT&T uses Ethernet after MPLS for MPLSoGRE/MPLSoUDP, but they cannot
  guarantee that they will not switch to IP.
    -- srking 10/4/2017 --------------------------------------
    MPLS over UDP not supported currently.
    ----------------------------------------------------------

  In addition to all packets types above, I've seen MPLS over L2TP
    -- srking 10/4/2017 --------------------------------------
    MPLS over L2TP not supported currently.
    ----------------------------------------------------------

  Yes, nsh is in use, and it looks like nsh over UDP is getting a lot of
  traction now. At least it is what I heard from Open Stack team few month
  ago.
    -- srking 10/4/2017 --------------------------------------
    NSH over UDP supported in the parser.
    ----------------------------------------------------------

  By the way, latest version of nsh supports MPLS payload: ETH - IP -UDP -
  nsh - MPLS - ETH - IP - PROTO - PAY
    -- srking 10/4/2017 --------------------------------------
    MPLS over NSH not supported currently.
    ----------------------------------------------------------

-- Anjali Singhai 9/29/2017 --------------------------------------------------
  Unexpected bits in MPLS should result in MAC_MPLS_Pay ptype
  Unexpected bits in VLAN can revert to vanilla MAC_Pay ptype
      -- srking 10/4/2017 - Done -------------------------------

-- Manasi Deval 10/2/2017 --------------------------------------------------
  Differentiate quic packets types with one PTYPE and flags

-- Alex Duyck 10/5/2017 --------------------------------------------------
  Transparent Ethernet Bridging over GRE uses a protocol value of 0x6558.
  It is called out in the GRE spec rfc1701: https://tools.ietf.org/html/rfc1701
  Transparent Ethernet Bridging over GRE uses a protocol value of 0x6558.
    -- srking 10/4/2017 --------------------------------------
    Ethernet over GRE supported.
    ----------------------------------------------------------

It is called out in the GRE spec rfc1701: https://tools.ietf.org/html/rfc1701

-- Manasi & Anjali 10/6/2017 --------------------------------------------------
  We do not want the presence of VLAN tags to change the PTYPES.  Doing so
  creates too many profiles to deal with.  Just use flags for VLANS.

-- Anjali Singhai 10/6/2017 --------------------------------------------------
  GRE should result in differnet ptypes than the UDP tunnels.
  Manasi and Kevin signed off on this.
    -- srking 10/4/2017 -------------------------------------- This is a
    big change from CPK, affecting both ptypes and flags. Unifying the UDP
    tunnels makes sense since blocks can find the tunnel content at a
    constant offset from the start of the UDP frame. On the other hand, GRE
    tunnels ride over IP, which has variable length. The current
    implementation enumerates all the IP Tunnel types like we do for UDP.
    In addition, we provide a generic "IPTunnel" protocol ID in the stack
    that points to the start of the tunnel header after all IP options.
    Currently, the only IP tunnel supported is GRE, but we have room for 7
    more IP tunnel types.
    ----------------------------------------------------------

-- Partha 10/11/2017 --------------------------------------------------
  Keep IP-in-IP protocols

-- Patrick Fleming 10/12/2017 -----------------------------------------
  For MST, need protocol ID pointing to IP_NEXT_PROTO field regardless of
  of IPv4/6.  When IPv6 has extension headers, the IP_NEXT_PROTO is always
  in the last extension.  This requirement is very analogous to the ETYPE_
  protocol ID that moves around due to VLAN tags.
  This requirement simplifies cases where the packet modifier must update
  the next protocol field.

-- Anjali 11//11/2017 ----------------------------------------------------
  Mentioned wanting 4 MPLS labels.  Sent email asking for rationale.

-- Patrick Fleming 12/1/2017 ---------------------------------------------
  Crypto needs to know the last instance of a protocol header.  Current
  scheme using OFOS and IL designation does not work well for them since
  the "last" protocol has two possible values.  Parsing cannot look ahead,
  so this proposal was formed:
  Use 3 protocol IDs for the affected headers:
    * _OFOS outer protcol header
    * _L last protocol header, maybe equal _OFOS

-- Steve King 12/6/2017 ---------------------------------------------
  Asked software if we must know by ptype of an IP packet is fragment,
  or if we can just leave this case as 'payload' and let the host stack
  determine if the packet is a fragment.

-- Steve King 12/6/2017 ---------------------------------------------
  Asked software if we must know by ptype of an IP packet is fragment, or
  if we can just leave this case as 'payload' and let the host stack
  determine if the packet is a fragment. Software approved after
  consultation with Anjali Singhai, Kevin Scott, Jesse Brendeburg, Manassi
  Deval, Dan Nowlin. We must add fragment flag.
    -- srking 12/6/2017 --------------------------------------
    Done.
    ----------------------------------------------------------

-- Steve King 12/6/2017 ---------------------------------------------
  Make the MPLS flags look like the HAS, which supports MPLSU and MPLSM.
    -- srking 12/6/2017 --------------------------------------
    I'm questioning this now.  FVL could not parse the MPLS
    label, so the current MPLS flag system could be the result
    of unnatural acts.  Need customer inputs.
    ----------------------------------------------------------

-- Steve King 12/12/2017 ---------------------------------------------
  Remove the VLAN protocol IDs.  The STAG is always a constant offset from
  the MAC protocol ID.  The CTAG is not constant, but comes either at the
  end of the MAC or end of the MAC + STAG size. The parser will still
  produce all the VLAN related flag bits so we no ambiguities.


-- Steve King 12/20/2017
  Given that the PTYPE defines version of all IP headers, using version
  specific IP protocol IDs is counter productive.  The new scheme uses
  version neutral IP_FIRST and IP_LAST.

  Reviewed with Manasi Deval.

-- Steve King 1/23/2018
  MNG block places hard-coded requirements on the parser outputs.
  Specifically on the protocol ID values.  As a small part of the
  work-around, I'm replacing the CTAG flag with a protocol ID since it
  would be useful to have regardless of the MNG problems.

-- Steve King 2/1/2018
  AVF requires that IP Fragments be expressed as PTYPES.  We don't need
  a protocol ID for fragments, but we do need a new leaf node.  And
  since we need a dedicated PTYPE, we no longer need the flag.

*******************************************************************************/

#include <icex.p4>
#include "protocols.p4"
#include "proto_ids.p4"
#include "ptypes.p4"

// P4's bizarre way to express a don't care.  See section 8.12.3 in the
// p4-16 1.0 specification (PDF version).
#define DONTCARE 0 &&& 0

// Define the number of platform nodes supported in the device.
// Product  NODE_COUNT
// -------  ----------
//  CPK     1
//  CVL     1
//  MST     5
#define NODE_COUNT 5

// Define the number of physical ports supported in the device.
// Product  NODE_COUNT
// -------  ----------
//  CPK     8
//  CVL     8
//  MST     8
#define PORT_COUNT 8

// P4 does not support operation precedence, nor compile time
// multiplication like in most languages. :/
#define NODEXPF_ONEHOT_SIZE 20

// Define the storage required for features that depend on UDP dport.
// Each PF x Node sets a don't-care in the match per for each supported feature
// on that port.
typedef bit<4> udp_feature_type_t;
const udp_feature_type_t FEATURE_NONE      = 0;
const udp_feature_type_t FEATURE_VXLAN_GPE = 1;
const udp_feature_type_t FEATURE_VXLAN     = 2;
const udp_feature_type_t FEATURE_GENEVE    = 3;
const udp_feature_type_t FEATURE_GTPU      = 4;
const udp_feature_type_t FEATURE_GTPC      = 5;
const udp_feature_type_t FEATURE_QUIC      = 6;
const udp_feature_type_t FEATURE_UDP_NSH   = 7; // NSH over UDP port 6633


const bit<16> UDP_PORT_QUIC_0  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_1  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_2  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_3  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_4  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_5  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_6  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_7  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_8  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_9  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_10 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_11 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_12 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_13 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_14 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_15 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_16 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_17 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_18 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_19 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_20 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_21 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_22 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_QUIC_23 = 0xAAAA; // placeholder

const bit<16> UDP_PORT_TUNNEL_0  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_1  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_2  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_3  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_4  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_5  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_6  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_7  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_8  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_9  = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_10 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_11 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_12 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_13 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_14 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_15 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_16 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_17 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_18 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_19 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_20 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_21 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_22 = 0xAAAA; // placeholder
const bit<16> UDP_PORT_TUNNEL_23 = 0xAAAA; // placeholder

// Define the header stack output of the parser.
// At each stack entry, we record the protocol and the offset into the packet
#define PROTOCOL_STACK_SIZE 16

// Define a node ID value.  10 bits is just a guess,
typedef bit<10> node_id_t;

// Define the number of bits to hold an IP tunnel type enumeration
// This field occupies consecutive flag bits and is visible to
// downstream blocks.
typedef bit<5> tun_type_t;

/*
 Enumerate specific tunnel types.
 Broad categories of tunnel type affect the packet PTYPE.
 We try to keep PTYPE impact to a minimum, preferring to
 differentiate based on this field instead.  See the
 table below for details.

               Tunnel
 Tunnel        Number
 Typess        Range      Tunnel Header Access
 ------------------------------------------------------------------------
 UDP Tunnels   0-7        From start of UDP_FIRST
                          No need for a dedicated protocol ID to point to
                          the start of the tunnel header since all fields
                          can be access from the start of the UDP header.
 GRE Tunnels   8-15       From start of PROTO_IP_TUNNEL
                          IP header is variable length, so the parser
                          provides a dedicated protocol ID pointing to the
                          tunnel header.
 IP-in-IP      16         No tunnel header exists.


 Field extraction from the tunnel header works from the start
 of the fixed size UDP frame.
 The 3 LSb's double as GRE operation header present flags.
*/
const tun_type_t TUN_NONE       = 0x00; // 00000 No tunnel detected
const tun_type_t TUN_VXLAN_GPE  = 0x01; // 00001
const tun_type_t TUN_VXLAN      = 0x02; // 00010
const tun_type_t TUN_GENEVE     = 0x03; // 00011
const tun_type_t TUN_GTPC       = 0x04; // 00100
const tun_type_t TUN_GTPU       = 0x05; // 00101
const tun_type_t TUN_NSH        = 0x06; // 00110 TODO: NSH is not a tunnel.  We should treat this header like an MPLS label.
const tun_type_t TUN_RSVD7      = 0x07; // 00111
const tun_type_t TUN_GRE        = 0x08; // 01000 GRE, no options
const tun_type_t TUN_GREC       = 0x09; // 01001 GRE+CSum
const tun_type_t TUN_GREK       = 0x0A; // 01010 GRE+Key, followed by IP
const tun_type_t TUN_GREKC      = 0x0B; // 01011 GRE+Key+CSum
const tun_type_t TUN_GRES       = 0x0C; // 01100 GRE+SeqNo
const tun_type_t TUN_GRESC      = 0x0D; // 01101 GRE+SeqNo+Csum
const tun_type_t TUN_GRESK      = 0x0E; // 01110 GRE+SeqNo+Key
const tun_type_t TUN_GRESKC     = 0x0F; // 01111 GRE+SeqNo+Key+CSum
const tun_type_t TUN_NVGRE      = 0x10; // 01000 GRE+Key, followed by MAC
const tun_type_t TUN_IP_IP      = 0x11; // 01001 IPv4/6 in IPv4/6
// All other tunnel types are reserved.

// Common CPK data types
// header offset is a signed 9 bit value.
typedef int<9> header_offset_t;

// Define an individual record that HW records in the proto/offset list.
// HW automatically records the header offset at each entry.
// Note: we cannot make arrays of structs, so define this as a header. :/
header protocol_record_t {
    protocol_id_t pid;
    header_offset_t ho;
}
struct standard_metadata_t {
    protocol_record_t[16] proto_stack;
}
struct parser_user_meta_t {
    // Control bits used to steer parsing.  These values are initialized
    // by the selected profile at the start of parsing.
    /*

    !!! KEEP THIS TABLE IN SYNC WITH INDEX.RST !!!

    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |IS|CRYPT|CRYPT|CRYPT|CRYPT|CRYPT|                                      |
    |TX|ENAB |OUTER|IPSEC|NATT |DTLS |Description                           |
    +==+=====+=====+=====+=====+=====+======================================+
    |1 | 0   | n/a | n/a | n/a | n/a |This profile defines packet transmit  |
    |  |     |     |     |     |     |without encryption support.           |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 1   | 1   | 1   | n/a |This profile defines packet transmit  |
    |  |     |     |     |     |     |with outer IPSec NAT-T.               |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 1   | 1   | 0   | n/a |This profile defines packet transmit  |
    |  |     |     |     |     |     |with outer IPSec.                     |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 1   | 0   | n/a | 1   |This profile defines packet transmit  |
    |  |     |     |     |     |     |with outer DTLS.                      |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 0   | 1   | 1   | n/a |This profile defines packet transmit  |
    |  |     |     |     |     |     |with inner IPSec NAT-T.               |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 0   | 1   | 0   | n/a |This profile defines packet transmit  |
    |  |     |     |     |     |     |with inner IPSec.                     |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 0   | 0   | n/a | 1   |This profile defines packet transmit  |
    |  |     |     |     |     |     |with inner DTLS.                      |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |1 | 1   | 0   | 0   | n/a | 1   |This profile defines packet transmit  |
    |  |     |     |     |     |     |with inner DTLS.                      |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |0 | 1   | n/a | n/a | 1   | n/a |This profile defines received packets |
    |  |     |     |     |     | n/a |with inner or outer IPSec with NAT-T. |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |0 | 1   | n/a | n/a | 0   | n/a |This profile defines received packets |
    |  |     |     |     |     |     |with inner or outer IPSec.            |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    |0 | 0   | n/a | n/a | n/a | n/a |This profile defines received packets |
    |  |     |     |     |     |     |without crypto support.               |
    +--+-----+-----+-----+-----+-----+--------------------------------------+
    */

    bit<1> ctrl_is_tx;        // True for transmit, false for receive.
    bit<1> ctrl_crypt_enab;   // 1 = enable crypto processing, 0 = ignore crypto

    // The parser ignores the following controls if ctrl_crypt_enab = 0
    bit<1> ctrl_crypt_outer;  // 1=outer crypto, 0=inner crypto
    bit<1> ctrl_crypt_ipsec;  // 1=IPSec, 0=not IPSec
    bit<1> ctrl_crypt_natt;   // If IPSec=1, 1=ESP after UDP port 4500, 0=ESP after IP
    bit<1> ctrl_crypt_dtls;   // If IPSec=0, 1=DTLS, 0=not DTLS


    // Marker flags, used to track different paths to the same leaf.
    // Markers are NOT visible to downstream blocks.
    // We only create markers for cases that create different PTYPEs.
    bit<1> marker_ipv6_ofos;  // outer IP was v6
    bit<1> marker_ipv6_il;    // inner IP was v6
    bit<1> marker_iptun;      // GRE tunnel family, but not IP-in-IP
    bit<1> marker_udptun;     // UDP tunenl family (VXLAN, Geneve, GTP, etc)
    bit<1> marker_ip_in_ip;   // IP-in-IP tunnel family

    tun_type_t  tun_type;     // See tunnel types explanation above.

    // Defines a unique parser node identifier.  This value is just an enumeration
    // and is not visible outside the parser.
    // Only leaf nodes explicitly set this value.
    node_id_t node_id;

    // Flags visible to downstream blocks
    bit<1> flag_stag_outer;   // pkt has outer (or single) VLAN service tag
    bit<1> flag_stag_inner;   // pkt has inner VLAN service tag
    bit<1> flag_mplsu_outer;  // pkt has at least one outer unicast label
    bit<1> flag_mplsm_outer;  // pkt has at least one outer multicast label
    bit<1> flag_mplsu_inner;  // pkt has at least one inner multicast label
    bit<1> flag_mplsm_inner;  // pkt has at least one inner unicast label

    // The node x PF onehot.  Exactly one bit will be set in this vector.
    // An entity outside the parser must initialize this field
    // fefore entry to the start node.
    bit<NODEXPF_ONEHOT_SIZE>  nodexpf_onehot;

    // The next proto field is internal to the parser only.
    // We extract this field for both IPV4 and IPV6 packets,
    // then process both cases using one logical graph node.
    bit<8> ip_next_proto;

    protocol_record_t[16] proto_stack;

    // Todo: lots more to add, e.g. all the metadata fields.
}

/*
  P4 requires a packet "header extraction" before code can match and
  branch on the content of the header.  This structure defines all
  such extractions using definitions in protocols.p4.

  In cases with both inner and outer headers, inner processing
  extracts the inner header to the same location as the outer,
  overwriting the outer header values.
*/
struct parsed_headers_t {
    eth_t                  mac;        // mac header
    vlan_t                 stag;       // Optional vlan stag
    vlan_t                 ctag;       // Optional vlan ctag
    mpls_t[2]              mpls;       // Optional inner and outer MPLS
    ip_t                   ip;         // v4/v6 union, inner and outer
    ipv6_ext_t             ipv6_ext;   // any extracted v6 extension
    udp_t                  udp;
    vxlan_t                vxlan;
    vxlan_gpe_t            vxlan_gpe;
    gre_t                  gre;
    gre_csum_t             gre_csum;
    gre_key_field_t        gre_key_field; // GRE/NVGRE union
    gre_seqno_t            gre_seqno;
    esp_t                  esp;
    nsh_t                  nsh;
    quic_first_byte_t      quic_first;
    quic_remaining_bytes_t quic_remainder;
    geneve_t               geneve;
    gtp_t                  gtp; // GPTU or GTPC depending on UDP port
    gtp_opt_t              gtp_opt;
}

// Required boilerplate for the V1 model backend
control ingress( inout parsed_headers_t hdrs,
                 inout parser_meta_t meta,
                 inout standard_metadata_t standard_metadata) {
    apply {}
}

// Required boilerplate for the V1 model backend
control egress( inout parsed_headers_t hdrs,
                inout parser_meta_t meta,
                inout standard_metadata_t standard_metadata) {
    apply {}
}

// Required boilerplate for the V1 model backend
control verify_csum( inout parsed_headers_t hdrs,
                     inout parser_meta_t meta) {
    apply {}
}

// Required boilerplate for the V1 model backend
control compute_csum( inout parsed_headers_t hdrs,
                      inout parser_meta_t meta) {
    apply {}
}

// Required boilerplate for the V1 model backend
control deparser( packet_out pkt,
                  in parsed_headers_t hdrs) {
    apply {}
}

// Define node IDs used in the parser.
// Ptype logic uses these ID values to produce a ptype number.
// Only leaf nodes explicitly set this value.
//
// Maybe a more succinct name would be leaf_id_t ?
//
// The scripts that help build this file depend that:
// 1) _FIRST and _LAST are always the suffix.
// 2) The terminal node name matches the PTYPE name as follows:
// 3) Explicitly mentioned flag bits must only be set true.
// 4) Unspecified flag bits are assumed to be false.
// 

const node_id_t NODE_MAC_PAY_FIRST  = 1;
const node_id_t NODE_ARP_FIRST      = 2;
const node_id_t NODE_ARP_LAST       = 3;
const node_id_t NODE_MPLS_PAY_FIRST = 4;
const node_id_t NODE_MPLS_PAY_LAST  = 5;
const node_id_t NODE_IP_PAY_FIRST   = 6;
const node_id_t NODE_FRAG_FIRST     = 7;
const node_id_t NODE_TCP_FIRST      = 8;
const node_id_t NODE_SCTP_FIRST     = 9;
const node_id_t NODE_ESP_FIRST      = 10;
const node_id_t NODE_ESP_LAST       = 11;
const node_id_t NODE_UDP_PAY_FIRST  = 12;
const node_id_t NODE_UDP_PAY_LAST   = 13;
const node_id_t NODE_ICMP_FIRST     = 14;
const node_id_t NODE_ICMP_LAST      = 15;
const node_id_t NODE_ICMPV6_FIRST   = 16;
const node_id_t NODE_ICMPV6_LAST    = 17;
const node_id_t NODE_ROCEV2_FIRST   = 18;
const node_id_t NODE_ROCEV2_LAST    = 19;
const node_id_t NODE_UDPTUN_PAY     = 20;
const node_id_t NODE_IPTUN_PAY      = 21;
const node_id_t NODE_MAC_PAY_LAST   = 22;
const node_id_t NODE_NSH_PAY        = 23;
const node_id_t NODE_TCP_LAST       = 24;
const node_id_t NODE_SCTP_LAST      = 25;
const node_id_t NODE_IP_PAY_LAST    = 26;
const node_id_t NODE_FRAG_LAST      = 27;
const node_id_t NODE_PTP_FIRST      = 28;
const node_id_t NODE_PTP_LAST       = 29;
const node_id_t NODE_LLDP_FIRST     = 30;
const node_id_t NODE_LLDP_LAST      = 31;
const node_id_t NODE_ECP_FIRST      = 32;
const node_id_t NODE_ECP_LAST       = 33;
const node_id_t NODE_EAPOL_FIRST    = 34;
const node_id_t NODE_EAPOL_LAST     = 35;
const node_id_t NODE_QUIC_FIRST     = 36; // no connection ID
const node_id_t NODE_QUIC_LAST      = 37; // no connection ID
const node_id_t NODE_QUICCID_FIRST  = 38; // with connection ID
const node_id_t NODE_QUICCID_LAST   = 39; // with connection ID
const node_id_t NODE_QUICUNK_FIRST  = 40; // unknown QUIC type field
const node_id_t NODE_QUICUNK_LAST   = 41; // unknown QUIC type field


// The v1model P4 backend forces our parser to take these input parameters, but
// this configuration does not intefere with parse graph definition.
//
// Guidelines to keep this sane:
// 1) Parsing must always terminate in a leaf node.
// 2) Each leaf node represents one or more PTYPEs
// 3) A leaf node is named after the last protocol in the stack.
// 4) All state names must follow this convention
//    a) Start with "Parse_"
//    b) Node names must end in _FIRST or _LAST as appropriate.

// Finally, Change whitespace or code structure with care.  Other scripts
//    expects to find certain conventions.
//    Check the script regexes for details.
parser unified_parser( packet_in pkt,
                  out parsed_headers_t hdrs,
                  out parser_meta_t umeta,
                  inout fxp_internal_metadata_t standard_metadata) {
    parser_user_meta_t meta;
    // Required start state.
    // Process the outer first or single MAC.
    // First step is to determine the location of the last ETYPE, which
    // moves in the presence of VLAN tags.
    state start {
        meta.proto_stack.next.pid = MAC_FIRST;
        meta.proto_stack.next.ho = 0;
        meta.proto_stack.next.pid = MAC_LAST;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.mac);
        transition select(hdrs.mac.type) {
            ETYPE_VLAN_CTAG : Parse_CTag_First;   // 0x8100
            ETYPE_VLAN_STAG : Parse_STag_First;   // 0x88A8
            default         : Parse_Last_ETYPE_First;
        }
    }

    // STAG should never be the last VLAN tag
    state Parse_STag_First {
        meta.flag_stag_outer = 1;
        pkt.extract(hdrs.stag);
        transition select(hdrs.stag.type) {
            ETYPE_VLAN_CTAG : Parse_CTag_First;     // 0x8100
                    default : Parse_MAC_Pay_First;  // Weird
        }
    }

    // CTAG should always be the last VLAN tag
    state Parse_CTag_First {
        meta.proto_stack.next.pid = CTAG_FIRST;
        meta.proto_stack.next.ho = 0;
        meta.proto_stack.next.pid = CTAG_LAST;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.ctag);
        transition Parse_Last_ETYPE_First;
    }

    // We enter this state with HO just past the last ETYPE
    state Parse_Last_ETYPE_First {
        pkt.extract(hdrs.mac);
        meta.proto_stack.next.pid = ETYPE_FIRST;
        meta.proto_stack.next.ho = -2;
        meta.proto_stack.next.pid = ETYPE_LAST;
        meta.proto_stack.next.ho = -2;
        transition select(hdrs.mac.type) {
            ETYPE_IPV4      : Parse_IPv4_First;       // 0x0800
            ETYPE_ARP       : Parse_ARP_First;        // 0x0806
            ETYPE_IPV6      : Parse_IPv6_First;       // 0x86DD
            ETYPE_MPLSU     : Parse_MPLSU_0_First;    // 0x8847 - unicast
            ETYPE_MPLSM     : Parse_MPLSM_0_First;    // 0x8848 - multicast
            ETYPE_PTP       : Parse_PTP_First;        // 0x88F7
            ETYPE_LLDP      : Parse_LLDP_First;       // 0x88CC
            ETYPE_ECP       : Parse_ECP_First;        // 0x8940
            ETYPE_EAPOL     : Parse_EAPOL_First;      // 0x888E
            default         : Parse_MAC_Pay_First;
        }
    }

    state Parse_ARP_First {
        meta.proto_stack.next.pid = ARP;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_ARP_FIRST;
        transition accept;
    }

    state Parse_ECP_First {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_ECP_FIRST;
        transition accept;
    }

    state Parse_ECP_Last {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_ECP_LAST;
        transition accept;
    }

    state Parse_EAPOL_First {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_EAPOL_FIRST;
        transition accept;
    }

    state Parse_EAPOL_Last {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_EAPOL_LAST;
        transition accept;
    }

    state Parse_PTP_First {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_PTP_FIRST;
        transition accept;
    }

    state Parse_PTP_Last {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_PTP_LAST;
        transition accept;
    }

    state Parse_LLDP_First {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_LLDP_FIRST;
        transition accept;
    }

    state Parse_LLDP_Last {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_LLDP_LAST;
        transition accept;
    }

    state Parse_MAC_Pay_First {
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        meta.node_id = NODE_MAC_PAY_FIRST;
        transition accept;
    }

    // First MPLS unicast label.
    state Parse_MPLSU_0_First {
        meta.flag_mplsu_outer = 1;
        transition Parse_MPLS_0_First;
    }

    // First MPLS multicast label.
    state Parse_MPLSM_0_First {
        meta.flag_mplsm_outer = 1;
        transition Parse_MPLS_0_First;
    }

    // First MPLS label.
    // RFC4182 allows that next protocol may defined in labels without BOS=1.
    // https://tools.ietf.org/html/rfc4182
    state Parse_MPLS_0_First {
        meta.proto_stack.next.pid = MPLS_TOS_OFoS;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.mpls[0]);
        transition select(hdrs.mpls[0].bos,
                          hdrs.mpls[0].label) {
            (1, MPLS_LABEL_IPV4) : Parse_IPv4_First;     // last label w/ proto
            (1, MPLS_LABEL_IPV6) : Parse_IPv6_First;     // last label w/ proto
            (0, MPLS_LABEL_IPV4) : Parse_MPLS_IPv4_First; // not last label w/ proto
            (0, MPLS_LABEL_IPV6) : Parse_MPLS_IPv6_First; // not last label w/ proto
            (1,        DONTCARE) : Parse_MAC_Pay_First;  // last label, weird proto
                         default : Parse_MPLS_1_First;    // not last label, try next
        }
    }

    // Second MPLS label, next proto will be IPv4.
    // RFC4182 does not appear to specify what happens in a packet
    // with multiple NULL labels.  We ignore all labels after finding
    // a null label.
    state Parse_MPLS_IPv4_First {
        pkt.extract(hdrs.mpls[1]);
        transition select(hdrs.mpls[1].bos,
                          hdrs.mpls[1].label) {
            (1,DONTCARE) : Parse_IPv4_First;    // found last label.
                 default : Parse_MAC_Pay_First; // not BOS, give up
        }
    }

    state Parse_MPLS_IPv6_First {
        pkt.extract(hdrs.mpls[1]);
        transition select(hdrs.mpls[1].bos,
                          hdrs.mpls[1].label) {
            (1,DONTCARE) : Parse_IPv6_First;    // found last label.
                 default : Parse_MAC_Pay_First; // not BOS, give up
        }
    }

    // Second MPLS label, next proto still unknown
    state Parse_MPLS_1_First {
        pkt.extract(hdrs.mpls[1]);
        transition select(hdrs.mpls[1].bos,
                          hdrs.mpls[1].label) {
            (1, MPLS_LABEL_IPV4) : Parse_IPv4_First;    // last label w/ proto
            (1, MPLS_LABEL_IPV6) : Parse_IPv6_First;    // last label w/ proto
                         default : Parse_MAC_Pay_First;  // give up
        }
    }

    state Parse_IPv4_First {
        meta.proto_stack.next.pid = IP_FIRST;
        meta.proto_stack.next.ho = 0;
        meta.proto_stack.next.pid = IP_LAST; // tunnel will override
        meta.proto_stack.next.ho = 0;
        // jump to next state since we hit max of 2 PIDS here
        transition Parse_IPv4_First_Check_Frag;
    }

    state Parse_IPv4_First_Check_Frag {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // tunnel will override
        meta.proto_stack.next.ho = 10;
        pkt.extract(hdrs.ip.v4);
        meta.ip_next_proto = hdrs.ip.v4.protocol;
        transition select(hdrs.ip.v4.mf_flag, hdrs.ip.v4.frag_off) {
              (0, 0) : Parse_IP_Next_Proto_First; // no fragmentation
             default : Parse_IP_Frag_First;
        }
    }

    /*
       In MST, the TX IPSEC flow requires FXP to insert the ESP header and
       encrypt the packet before the mobility and/or dest ops header.
       The CRYPTO_START protocol ID marks the correct location to insert
       the ESP header.

       Transmit Flow

       +------------+
       |   IPV6     | PID=IP_FIRST
       +-----+------+ Offset=0
             |
             |
             v
       +------------+
       | !mobility  |
       |    AND     |<---+
       | !dest ops  |    | Max 4 extensions
       +---------+--+    |
             |   |       |
             |   +-------+
             v
       +------------+
       |  mobility  | PID=CRYPTO_START
       |    OR      | Offset=0
       |  dest ops  |
       +-----+------+
             |
             |
             v
       Parsing Complete


       In MST, the RX IPSEC flow requires FXP to find the ESP header and
       decrypt the packet.  The parser does not expect to find the mobility
       and or dest ops header in clear text.
       The CRYPTO_START protocol ID marks the location of the start of the
       ESP header.

       Receive Flow

       +------------+
       |   IPV6     | PID=IP_FIRST
       +-----+------+ Offset=0
             |
             |
             v
       +------------+
       |            |
       |   !ESP     |<---+
       |            |    | Max 6 extensions
       +---------+--+    |
             |   |       |
             |   +-------+
             v
       +------------+
       |            | PID=CRYPTO_START
       |    ESP     | Offset=0
       |            |
       +------------+
             |
             |
             v
       Parsing Complete

    */

    state Parse_IPv6_First {
        meta.proto_stack.next.pid = IP_FIRST;
        meta.proto_stack.next.ho = 0;
        meta.proto_stack.next.pid = IP_LAST; // tunnel will override
        meta.proto_stack.next.ho = 0;
        meta.marker_ipv6_ofos = 1;
        transition Parse_IPv6_Check_Ext;
    }

    state Parse_IPv6_Check_Ext {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 6;
        pkt.extract(hdrs.ip.v6);
        meta.ip_next_proto = hdrs.ip.v6.next_header;
        transition select(hdrs.ip.v6.next_header) {
          IP_PROTO_EXT_HOP     : Parse_IPv6_Ext1_First;
          IP_PROTO_EXT_ROUTING : Parse_IPv6_Ext1_First;
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
          IP_PROTO_EXT_AH      : Parse_IPv6_Ext1_First;
          IP_PROTO_EXT_DESTOPT : Parse_IPv6_Ext1_First;
          IP_PROTO_EXT_MOBL    : Parse_IPv6_Ext1_First;
          IP_PROTO_EXT_HIPV2   : Parse_IPv6_Ext1_First;
          IP_PROTO_EXT_SHIM6   : Parse_IPv6_Ext1_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }

    // Parse the first IPv6 Extension header.
    state Parse_IPv6_Ext1_First {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 0;

        // Use look-ahead to determine the length before extracting.
        // The ext_size field defines the size of the variable portion
        // of the IPv6 extension.  The fixed size 64-bit extension header
        // is always extracted.

        //bit<8> ext_size = pkt.lookahead<ipv6_ext_hdr_t>().length;
        pkt.extract(hdrs.ipv6_ext, (bit<32>)hdrs.ipv6_ext.length);
        meta.ip_next_proto = hdrs.ipv6_ext.next_header;
        transition select(hdrs.ipv6_ext.next_header) {
          IP_PROTO_EXT_HOP     : Parse_IPv6_Ext2_First;
          IP_PROTO_EXT_ROUTING : Parse_IPv6_Ext2_First;
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
          IP_PROTO_EXT_AH      : Parse_IPv6_Ext2_First;
          IP_PROTO_EXT_DESTOPT : Parse_IPv6_Ext2_First;
          IP_PROTO_EXT_MOBL    : Parse_IPv6_Ext2_First;
          IP_PROTO_EXT_HIPV2   : Parse_IPv6_Ext2_First;
          IP_PROTO_EXT_SHIM6   : Parse_IPv6_Ext2_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }

    // Parse the first IPv6 Extension header.
    state Parse_IPv6_Ext2_First {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 0;

        // Use look-ahead to determine the length before extracting.
        // The ext_size field defines the size of the variable portion
        // of the IPv6 extension.  The fixed size 64-bit extension header
        // is always extracted.

        //bit<8> ext_size = pkt.lookahead<ipv6_ext_hdr_t>().length;
        pkt.extract(hdrs.ipv6_ext, (bit<32>)hdrs.ipv6_ext.length);
        meta.ip_next_proto = hdrs.ipv6_ext.next_header;
        transition select(hdrs.ipv6_ext.next_header) {
          IP_PROTO_EXT_HOP     : Parse_IPv6_Ext3_First;
          IP_PROTO_EXT_ROUTING : Parse_IPv6_Ext3_First;
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
          IP_PROTO_EXT_AH      : Parse_IPv6_Ext3_First;
          IP_PROTO_EXT_DESTOPT : Parse_IPv6_Ext3_First;
          IP_PROTO_EXT_MOBL    : Parse_IPv6_Ext3_First;
          IP_PROTO_EXT_HIPV2   : Parse_IPv6_Ext3_First;
          IP_PROTO_EXT_SHIM6   : Parse_IPv6_Ext3_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }

    // Parse the first IPv6 Extension header.
    state Parse_IPv6_Ext3_First {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 0;

        // Use look-ahead to determine the length before extracting.
        // The ext_size field defines the size of the variable portion
        // of the IPv6 extension.  The fixed size 64-bit extension header
        // is always extracted.

        //bit<8> ext_size = pkt.lookahead<ipv6_ext_hdr_t>().length;
        pkt.extract(hdrs.ipv6_ext, (bit<32>)hdrs.ipv6_ext.length);
        meta.ip_next_proto = hdrs.ipv6_ext.next_header;
        transition select(hdrs.ipv6_ext.next_header) {
          IP_PROTO_EXT_HOP     : Parse_IPv6_Ext4_First;
          IP_PROTO_EXT_ROUTING : Parse_IPv6_Ext4_First;
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
          IP_PROTO_EXT_AH      : Parse_IPv6_Ext4_First;
          IP_PROTO_EXT_DESTOPT : Parse_IPv6_Ext4_First;
          IP_PROTO_EXT_MOBL    : Parse_IPv6_Ext4_First;
          IP_PROTO_EXT_HIPV2   : Parse_IPv6_Ext4_First;
          IP_PROTO_EXT_SHIM6   : Parse_IPv6_Ext4_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }

    // Parse the first IPv6 Extension header.
    state Parse_IPv6_Ext4_First {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 0;

        // Use look-ahead to determine the length before extracting.
        // The ext_size field defines the size of the variable portion
        // of the IPv6 extension.  The fixed size 64-bit extension header
        // is always extracted.

        //bit<8> ext_size = pkt.lookahead<ipv6_ext_hdr_t>().length;
        pkt.extract(hdrs.ipv6_ext, (bit<32>)hdrs.ipv6_ext.length);
        meta.ip_next_proto = hdrs.ipv6_ext.next_header;
        transition select(hdrs.ipv6_ext.next_header) {
          IP_PROTO_EXT_HOP     : Parse_IPv6_Ext5_First;
          IP_PROTO_EXT_ROUTING : Parse_IPv6_Ext5_First;
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
          IP_PROTO_EXT_AH      : Parse_IPv6_Ext5_First;
          IP_PROTO_EXT_DESTOPT : Parse_IPv6_Ext5_First;
          IP_PROTO_EXT_MOBL    : Parse_IPv6_Ext5_First;
          IP_PROTO_EXT_HIPV2   : Parse_IPv6_Ext5_First;
          IP_PROTO_EXT_SHIM6   : Parse_IPv6_Ext5_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }

    // Parse the first IPv6 Extension header.
    state Parse_IPv6_Ext5_First {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 0;

        // Use look-ahead to determine the length before extracting.
        // The ext_size field defines the size of the variable portion
        // of the IPv6 extension.  The fixed size 64-bit extension header
        // is always extracted.

        //bit<8> ext_size = pkt.lookahead<ipv6_ext_hdr_t>().length;
        pkt.extract(hdrs.ipv6_ext, (bit<32>)hdrs.ipv6_ext.length);
        meta.ip_next_proto = hdrs.ipv6_ext.next_header;
        transition select(hdrs.ipv6_ext.next_header) {
          IP_PROTO_EXT_HOP     : Parse_IPv6_Ext6_First;
          IP_PROTO_EXT_ROUTING : Parse_IPv6_Ext6_First;
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
          IP_PROTO_EXT_AH      : Parse_IPv6_Ext6_First;
          IP_PROTO_EXT_DESTOPT : Parse_IPv6_Ext6_First;
          IP_PROTO_EXT_MOBL    : Parse_IPv6_Ext6_First;
          IP_PROTO_EXT_HIPV2   : Parse_IPv6_Ext6_First;
          IP_PROTO_EXT_SHIM6   : Parse_IPv6_Ext6_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }

    // Parse the first IPv6 Extension header.
    // This is the 6th and last header we attempt.
    state Parse_IPv6_Ext6_First {
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST; // many overrides
        meta.proto_stack.next.ho = 0;

        // Use look-ahead to determine the length before extracting.
        // The ext_size field defines the size of the variable portion
        // of the IPv6 extension.  The fixed size 64-bit extension header
        // is always extracted.

        //bit<8> ext_size = pkt.lookahead<ipv6_ext_hdr_t>().length;
        pkt.extract(hdrs.ipv6_ext, (bit<32>)hdrs.ipv6_ext.length); 
        meta.ip_next_proto = hdrs.ipv6_ext.next_header;
        transition select(hdrs.ipv6_ext.next_header) {
          IP_PROTO_EXT_FRAG    : Parse_IP_Frag_First;
                       default : Parse_IP_Next_Proto_First;
        }
    }


    // This state dispatches control for both IP v4 and v6.
    // It's one of the few nodes where we transition based
    // on metadata instead of packet data.
    state Parse_IP_Next_Proto_First {
        transition select(meta.ip_next_proto) {
            IP_PROTO_ICMP   : Parse_ICMP_First;    // 1
            IP_PROTO_IPV4   : Parse_IPv4_in_IP;    // 4
            IP_PROTO_IPV6   : Parse_IPv6_in_IP;    // 41
            IP_PROTO_TCP    : Parse_TCP_First;     // 6
            IP_PROTO_UDP    : Parse_UDP_First;     // 17
            IP_PROTO_GRE    : Parse_GRE;           // 47
            IP_PROTO_ESP    : Parse_ESP_First;     // 50
            IP_PROTO_ICMPV6 : Parse_ICMPv6_First;  // 58
            IP_PROTO_SCTP   : Parse_SCTP_First;    // 132
                    default : Parse_IP_Pay_First;
        }
    }

    state Parse_IPv4_in_IP {
        meta.marker_ip_in_ip = 1;
        meta.tun_type = TUN_IP_IP;
        transition Parse_IPv4_Last;
    }

    state Parse_IPv6_in_IP {
        meta.marker_ip_in_ip = 1;
        meta.tun_type = TUN_IP_IP;
        transition Parse_IPv6_Last;
    }

    state Parse_ICMP_First {
        meta.node_id = NODE_ICMP_FIRST;
        meta.proto_stack.next.pid = ICMP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ICMPv6_First {
        meta.node_id = NODE_ICMPV6_FIRST;
        meta.proto_stack.next.pid = ICMPV6;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    // IPv4/6 come through this state for fragments.
    // For AVF compatibility, fragments require a dedicated PTYPE.
    // Protocol IDs are the same as PAY case.
    state Parse_IP_Frag_First {
        meta.node_id = NODE_FRAG_FIRST;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_IP_Pay_First {
        meta.node_id = NODE_IP_PAY_FIRST;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ESP_First {
        // Everything after the esp header is ciphertext (mostly)
        meta.node_id = NODE_ESP_FIRST;
        meta.proto_stack.next.pid = ESP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_TCP_First {
        meta.node_id = NODE_TCP_FIRST;
        meta.proto_stack.next.pid = TCP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_SCTP_First {
        meta.node_id = NODE_SCTP_FIRST;
        meta.proto_stack.next.pid = SCTP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_UDP_First {
        meta.proto_stack.next.pid = UDP_FIRST;
        meta.proto_stack.next.ho = 0;
        // UDP does not need speculative UDP_LAST.  We insert UDP_LAST
        // definitively depending on tunnel detection.
        pkt.extract(hdrs.udp);

        /*
        Interpretation of UDP dport numbers depends on the specific node
        and PF.  We define a one-hot vector to provide node x PF isolation.
        For this source code, we match a one-hot value of 0 to define
        transitions. A value of 0 cannot appear in the real one-hot value.
        At runtime, the host will modify entries to set don't-care bits in
        the one-hot values.  Alternatively, the host can create custom
        entries e.g. for use of non-standard ports.
        */
        transition select(meta.nodexpf_onehot, hdrs.udp.dport) {
            (0, UDP_PORT_VXLAN     ) : Parse_VXLAN;        // 4789 placeholder
            (0, UDP_PORT_VXLAN_GPE ) : Parse_VXLAN_GPE;    // 4790 placeholder
            (0, UDP_PORT_ROCEV2    ) : Parse_ROCEv2_First;  // 4791 placeholder
            (0, UDP_PORT_GENEVE    ) : Parse_Geneve;  // 6081 placeholder
            (0, UDP_PORT_GTPU      ) : Parse_GTPU;    // 2152 - GTPU
            (0, UDP_PORT_GTPC      ) : Parse_GTPC;    // 2123 - GTPC
            (0, UDP_PORT_NSH       ) : Parse_UDP_NSH_First; // 6633 - NSH
            (0, UDP_PORT_TUNNEL_6  ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_7  ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_8  ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_9  ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_10 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_11 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_12 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_13 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_14 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_15 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_16 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_17 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_18 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_19 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_20 ) : Parse_MAC_Last;       // placeholder
            (0, UDP_PORT_TUNNEL_21 ) : Parse_MAC_Last;       // placeholder
                             default : Parse_Maybe_QUIC_First;
        }
    }

    state Parse_Maybe_QUIC_First {
        transition select(meta.nodexpf_onehot, hdrs.udp.dport) {
            (0, UDP_PORT_QUIC_0  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_1  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_2  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_3  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_4  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_5  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_6  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_7  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_8  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_9  ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_10 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_11 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_12 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_13 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_14 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_15 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_16 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_17 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_18 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_19 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_20 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_21 ) : Parse_QUIC_First;
            (0, UDP_PORT_QUIC_22 ) : Parse_QUIC_First;
                           default : Parse_UDP_Pay_First;
        }
    }

    state Parse_UDP_Pay_First {
        // will need second pass if many UDP port num checks required.
        meta.node_id = NODE_UDP_PAY_FIRST;
        meta.proto_stack.next.pid = UDP_LAST;
        meta.proto_stack.next.ho = -8; // UDP already extracted
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_UDP_Pay_Last {
        // will need second pass if many UDP port num checks required.
        meta.node_id = NODE_UDP_PAY_LAST;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ROCEv2_First {
        meta.node_id = NODE_ROCEV2_FIRST;
        meta.proto_stack.next.pid = ROCEV2;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }
    state Parse_ROCEv2_Last {
        meta.node_id = NODE_ROCEV2_LAST;
        meta.proto_stack.next.pid = ROCEV2;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    // First state of QUIC processing
    state Parse_QUIC_First {
        pkt.extract(hdrs.quic_first.short);

        // Form bit is the same for both long and short forms
        // We combine long and short parsing in one step.
        transition select(hdrs.quic_first.short.form,
                hdrs.quic_first.short.conid_present,
                hdrs.quic_first.short.type) {
            (1, DONTCARE, DONTCARE) : Parse_QUIC_Long_First; // has conid
            (0, 0       , 1       ) : Parse_QUIC_Short_1_First;
            (0, 0       , 2       ) : Parse_QUIC_Short_2_First;
            (0, 0       , 4       ) : Parse_QUIC_Short_4_First;
            (0, 1       , 1       ) : Parse_QUIC_Short_9_First;  // has conid
            (0, 1       , 2       ) : Parse_QUIC_Short_10_First; // has conid
            (0, 1       , 4       ) : Parse_QUIC_Short_12_First; // has conid
                            default : Parse_QUIC_Unknown_First;
        }
    }
    state Parse_QUIC_Last {
        pkt.extract(hdrs.quic_first.short);

        // Form bit is the same for both long and short forms
        // We combine long and short parsing in one step.
        transition select(hdrs.quic_first.short.form,
                hdrs.quic_first.short.conid_present,
                hdrs.quic_first.short.type) {
            (1, DONTCARE, DONTCARE) : Parse_QUIC_Long_Last; // has conid
            (0, 0       , 1       ) : Parse_QUIC_Short_1_Last;
            (0, 0       , 2       ) : Parse_QUIC_Short_2_Last;
            (0, 0       , 4       ) : Parse_QUIC_Short_4_Last;
            (0, 1       , 1       ) : Parse_QUIC_Short_9_Last;  // has conid
            (0, 1       , 2       ) : Parse_QUIC_Short_10_Last; // has conid
            (0, 1       , 4       ) : Parse_QUIC_Short_12_Last; // has conid
                            default : Parse_QUIC_Unknown_Last;
        }
    }

    // unknwon Connection ID
    state Parse_QUIC_Unknown_First {
        meta.node_id = NODE_QUICUNK_FIRST;
        transition accept;
    }
    state Parse_QUIC_Unknown_Last {
        meta.node_id = NODE_QUICUNK_LAST;
        transition accept;
    }

    // has Connection ID
    state Parse_QUIC_Long_First {
        meta.node_id = NODE_QUICCID_FIRST;
        pkt.extract(hdrs.quic_remainder.long);
        transition accept;
    }
    state Parse_QUIC_Long_Last {
        meta.node_id = NODE_QUICCID_LAST;
        pkt.extract(hdrs.quic_remainder.long);
        transition accept;
    }

    // no Connection ID
    state Parse_QUIC_Short_1_First {
        meta.node_id = NODE_QUIC_FIRST;
        pkt.extract(hdrs.quic_remainder.short_1);
        transition accept;
    }
    state Parse_QUIC_Short_1_Last {
        meta.node_id = NODE_QUIC_LAST;
        pkt.extract(hdrs.quic_remainder.short_1);
        transition accept;
    }

    // no Connection ID
    state Parse_QUIC_Short_2_First {
        meta.node_id = NODE_QUIC_FIRST;
        pkt.extract(hdrs.quic_remainder.short_2);
        transition accept;
    }
    state Parse_QUIC_Short_2_Last {
        meta.node_id = NODE_QUIC_LAST;
        pkt.extract(hdrs.quic_remainder.short_2);
        transition accept;
    }

    // no Connection ID
    state Parse_QUIC_Short_4_First {
        meta.node_id = NODE_QUIC_FIRST;
        pkt.extract(hdrs.quic_remainder.short_4);
        transition accept;
    }
    state Parse_QUIC_Short_4_Last {
        meta.node_id = NODE_QUIC_LAST;
        pkt.extract(hdrs.quic_remainder.short_4);
        transition accept;
    }

    // has Connection ID
    state Parse_QUIC_Short_9_First {
        meta.node_id = NODE_QUICCID_FIRST;
        pkt.extract(hdrs.quic_remainder.short_9);
        transition accept;
    }
    state Parse_QUIC_Short_9_Last {
        meta.node_id = NODE_QUICCID_LAST;
        pkt.extract(hdrs.quic_remainder.short_9);
        transition accept;
    }

    // has Connection ID
    state Parse_QUIC_Short_10_First {
        meta.node_id = NODE_QUICCID_FIRST;
        pkt.extract(hdrs.quic_remainder.short_10);
        transition accept;
    }
    state Parse_QUIC_Short_10_Last {
        meta.node_id = NODE_QUICCID_LAST;
        pkt.extract(hdrs.quic_remainder.short_10);
        transition accept;
    }

    // has Connection ID
    state Parse_QUIC_Short_12_First {
        meta.node_id = NODE_QUICCID_FIRST;
        pkt.extract(hdrs.quic_remainder.short_12);
        transition accept;
    }
    state Parse_QUIC_Short_12_Last {
        meta.node_id = NODE_QUICCID_LAST;
        pkt.extract(hdrs.quic_remainder.short_12);
        transition accept;
    }

    state Parse_VXLAN {
        meta.marker_udptun = 1;
        meta.tun_type = TUN_VXLAN;
        pkt.extract(hdrs.vxlan);
        transition Parse_MAC_Last;
    }

    state Parse_VXLAN_GPE {
        meta.marker_udptun = 1;
        meta.tun_type = TUN_VXLAN_GPE;
        pkt.extract(hdrs.vxlan_gpe);
        transition select(hdrs.vxlan_gpe.protocol) {
            VXLAN_GPE_PROTO_IPv4 : Parse_IPv4_Last;
            VXLAN_GPE_PROTO_IPv6 : Parse_IPv6_Last;
            VXLAN_GPE_PROTO_MAC  : Parse_MAC_Last;
            VXLAN_GPE_PROTO_NSH  : Parse_NSH;
            VXLAN_GPE_PROTO_MPLS : Parse_MPLS_0_Last;
                         default : Parse_UDPTun_Pay;
        }
    }

    state Parse_UDPTun_Pay {
        meta.node_id = NODE_UDPTUN_PAY;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }
    state Parse_IPTun_Pay {
        meta.node_id = NODE_IPTUN_PAY;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_GRE {
        // Indicate we came through a GRE tunnel, not the UDP tunnel family.
        meta.marker_iptun = 1;
        meta.proto_stack.next.pid = GRE;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.gre);
        // determine which, if any, optional fields exist.
        transition select(hdrs.gre.csum_present, hdrs.gre.key_present,
                          hdrs.gre.seqno_present) {
            (0,0,0) : Parse_GRE_None;
            (0,0,1) : Parse_GRE_S;
            (0,1,0) : Parse_Maybe_NVGRE; // only this combination might be NVGRE
            (0,1,1) : Parse_GRE_SK;
            (1,0,0) : Parse_GRE_C;
            (1,0,1) : Parse_GRE_SC;
            (1,1,0) : Parse_GRE_KC;
            (1,1,1) : Parse_GRE_SKC;
        }
    }

    state Parse_GRE_None {
        meta.tun_type = TUN_GRE;
        transition Parse_GRE_Done;
    }

    state Parse_GRE_S {
        meta.tun_type = TUN_GRES;
        pkt.extract(hdrs.gre_seqno);
        transition Parse_GRE_Done;
    }

    // NVGRE = GREK followed by an L2 MAC header
    // GREK = GREK followed by an IP header
    state Parse_Maybe_NVGRE {
        pkt.extract(hdrs.gre_key_field.key); // union with NVGRE VSID and Flow ID
        transition select(hdrs.gre.protocol) {
             GRE_PROTO_MAC : Parse_NVGRE;    // 0x6558, it's NVGRE
                   default : Parse_GRE_K;    // not 0x6558, it's GREK
        }
    }

    // Key field was already extracted
    state Parse_GRE_K {
        meta.tun_type = TUN_GREK;
        transition Parse_GRE_Done;
    }

    // Key field was already extracted
    state Parse_NVGRE {
        meta.tun_type = TUN_NVGRE;
        transition Parse_MAC_Last;
    }

    state Parse_GRE_SK {
        meta.tun_type = TUN_GRESK;
        pkt.extract(hdrs.gre_seqno);
        pkt.extract(hdrs.gre_key_field.key);
        transition Parse_GRE_Done;
    }

    state Parse_GRE_C {
        meta.tun_type = TUN_GREC;
        pkt.extract(hdrs.gre_csum);
        transition Parse_GRE_Done;
    }

    state Parse_GRE_SC {
        meta.tun_type = TUN_GRESC;
        pkt.extract(hdrs.gre_seqno);
        pkt.extract(hdrs.gre_csum);
        transition Parse_GRE_Done;
    }

    state Parse_GRE_KC {
        meta.tun_type = TUN_GREKC;
        pkt.extract(hdrs.gre_key_field.key);
        pkt.extract(hdrs.gre_csum);
        transition Parse_GRE_Done;
    }

    state Parse_GRE_SKC {
        meta.tun_type = TUN_GRESKC;
        pkt.extract(hdrs.gre_seqno);
        pkt.extract(hdrs.gre_key_field.key);
        pkt.extract(hdrs.gre_csum);
        transition Parse_GRE_Done;
    }

    // All GRE except NVGRE comes through this node.
    state Parse_GRE_Done {
        transition select(hdrs.gre.protocol) {
            GRE_PROTO_IPV4 : Parse_IPv4_Last;    // 0x0800
            GRE_PROTO_IPV6 : Parse_IPv6_Last;    // 0x86DD
           GRE_PROTO_MPLSU : Parse_MPLS_0_Last; // 0x8847
                   default : Parse_IPTun_Pay;
        }
    }

    state Parse_NSH {
        // todo: nsh is a variable length header.
        // Length field specifies number of 32-bit words, min = 6
        pkt.extract(hdrs.nsh);
        transition select(hdrs.nsh.protocol) {
            NSH_PROTO_IPV4 : Parse_IPv4_Last;     // 1
            NSH_PROTO_IPV6 : Parse_IPv6_Last;     // 2
             NSH_PROTO_MAC : Parse_MAC_Last;      // 3
                   default : Parse_NSH_Pay; // Multiple-NSH & weird stuff
        }
    }

    state Parse_NSH_Pay {
        // terminal node
        meta.node_id = NODE_NSH_PAY;
        transition accept;
    }

    state Parse_Geneve {
        meta.marker_udptun = 1;
        meta.tun_type = TUN_VXLAN_GPE;
        pkt.extract(hdrs.geneve);
        transition select(hdrs.geneve.protocol) {
            ETYPE_MAC  : Parse_MAC_Last;          // 0x6558
            ETYPE_IPV4 : Parse_IPv4_Last;         // 0x0800
            ETYPE_IPV6 : Parse_IPv6_Last;         // 0x86DD
               default : Parse_UDPTun_Pay;
        }
    }

    state Parse_GTPU {
        meta.marker_udptun = 1;
        meta.tun_type = TUN_GTPU;
        pkt.extract(hdrs.gtp);
        transition Parse_IPv4_Last;
    }

    state Parse_UDP_NSH_First {
        meta.marker_udptun = 1;
        meta.tun_type = TUN_NSH;
        transition Parse_NSH;
    }

    state Parse_GTPC {
        meta.marker_udptun = 1;
        meta.tun_type = TUN_GTPC;
        pkt.extract(hdrs.gtp);
        transition Parse_IPv4_Last;
    }

    state Parse_MAC_Last {
        // In this parse graph, we know that the stale MAC_LAST is always
        // at protocol stack slot 1.  In MST, we could directly write
        // the MAC_LAST update to slot 1 and avoid wasting a new slot here.
        // For harmony with CPK, we push a redundant entry on the stack.
        meta.proto_stack.next.pid = MAC_LAST;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.mac);
        transition select(hdrs.mac.type) {
            ETYPE_VLAN_CTAG : Parse_CTag_Last;   // 0x8100
            ETYPE_VLAN_STAG : Parse_STag_Last;   // 0x88A8
            default         : Parse_Last_ETYPE_Last;
        }
    }

    // STAG should never be the last VLAN tag
    state Parse_STag_Last {
        meta.flag_stag_inner = 1;
        pkt.extract(hdrs.stag);
        transition select(hdrs.stag.type) {
            ETYPE_VLAN_CTAG : Parse_CTag_Last;     // 0x8100
                    default : Parse_MAC_Pay_Last;  // Weird
        }
    }

    // CTAG should always be the last VLAN tag
    state Parse_CTag_Last {
        meta.proto_stack.next.pid = CTAG_LAST;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.ctag);
        transition Parse_Last_ETYPE_Last;
    }

    // We enter this state with HO just past the last ETYPE
    state Parse_Last_ETYPE_Last {
        meta.proto_stack.next.pid = ETYPE_LAST;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.mac);
        transition select(hdrs.mac.type) {
            ETYPE_IPV4      : Parse_IPv4_Last;       // 0x0800
            ETYPE_ARP       : Parse_ARP_Last;        // 0x0806
            ETYPE_IPV6      : Parse_IPv6_Last;       // 0x86DD
            ETYPE_MPLSU     : Parse_MPLSU_0_Last;    // 0x8847
            ETYPE_MPLSM     : Parse_MPLSM_0_Last;    // 0x8848
            ETYPE_PTP       : Parse_PTP_Last;        // 0x88F7
            ETYPE_LLDP      : Parse_LLDP_Last;       // 0x88CC
            ETYPE_ECP       : Parse_ECP_Last;        // 0x8940
            ETYPE_EAPOL     : Parse_EAPOL_Last;      // 0x888E
            default         : Parse_MAC_Pay_Last;
        }
    }

    // First MPLS unicast label.
    state Parse_MPLSU_0_Last {
        meta.flag_mplsu_inner = 1;
        transition Parse_MPLS_0_Last;
    }

    // First MPLS multicast label.
    state Parse_MPLSM_0_Last {
        meta.flag_mplsm_inner = 1;
        transition Parse_MPLS_0_Last;
    }

    // First MPLS label.
    // RFC4182 allows that next protocol may defined in labels without BOS=1.
    // https://tools.ietf.org/html/rfc4182
    state Parse_MPLS_0_Last {
        meta.proto_stack.next.pid = MPLS_TOS_IL;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.mpls[0]);
        transition select(hdrs.mpls[0].bos,
                          hdrs.mpls[0].label) {
            (1, MPLS_LABEL_IPV4) : Parse_IPv4_Last;      // last label w/ proto
            (1, MPLS_LABEL_IPV6) : Parse_IPv6_Last;      // last label w/ proto
            (0, MPLS_LABEL_IPV4) : Parse_MPLS_IPv4_Last; // not last label w/ proto
            (0, MPLS_LABEL_IPV6) : Parse_MPLS_IPv6_Last; // not last label w/ proto
            (1,        DONTCARE) : Parse_MAC_Pay_Last;   // last label, weird proto
                         default : Parse_MPLS_1_Last;    // not last label, try next
        }
    }

    state Parse_MPLS_Pay_Last {
        meta.node_id = NODE_MAC_PAY_LAST;
        transition accept;
    }

    // Second MPLS label, next proto still unknown
    state Parse_MPLS_1_Last {
        pkt.extract(hdrs.mpls[1]);
        transition select(hdrs.mpls[1].bos,
                          hdrs.mpls[1].label) {
            (1, MPLS_LABEL_IPV4) : Parse_IPv4_Last;    // last label w/ proto
            (1, MPLS_LABEL_IPV6) : Parse_IPv6_Last;    // last label w/ proto
                         default : Parse_MAC_Pay_Last;  // give up
        }
    }

    // Second MPLS label, next proto will be IPv4.
    // RFC4182 does not appear to specify what happens in a packet
    // with multiple NULL labels.  We ignore all lables after finding
    // a null label.
    state Parse_MPLS_IPv4_Last {
        pkt.extract(hdrs.mpls[1]);
        transition select(hdrs.mpls[1].bos,
                          hdrs.mpls[1].label) {
            (1,DONTCARE) : Parse_IPv4_Last; // found last label.
                 default : Parse_MPLS_Pay_Last; // not BOS, give up
        }
    }

    state Parse_MPLS_IPv6_Last {
        pkt.extract(hdrs.mpls[1]);
        transition select(hdrs.mpls[1].bos,
                          hdrs.mpls[1].label) {
            (1,DONTCARE) : Parse_IPv6_Last; // found last label.
                 default : Parse_MPLS_Pay_Last; // not BOS, give up
        }
    }

    state Parse_MAC_Pay_Last {
        // terminal node
        meta.node_id = NODE_MAC_PAY_LAST;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_IPv4_Last {
        meta.proto_stack.next.pid = IP_LAST;
        meta.proto_stack.next.ho = 0;
        // IP_NEXT_HDR_LAST overrides outer IP
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST;
        meta.proto_stack.next.ho = 10;
        pkt.extract(hdrs.ip.v4);
        meta.ip_next_proto = hdrs.ip.v4.protocol;
        transition select(hdrs.ip.v4.frag_off) {
                   0 : Parse_IP_Next_Proto_Last;
             default : Parse_IP_Frag_Last;
        }
    }

    state Parse_IPv6_Last {
        meta.proto_stack.next.pid = IP_LAST;
        meta.proto_stack.next.ho = 0;
        // IP_NEXT_HDR_LAST overrides outer IP
        meta.proto_stack.next.pid = IP_NEXT_HDR_LAST;
        meta.proto_stack.next.ho = 6;
        meta.marker_ipv6_il = 1;
        pkt.extract(hdrs.ip.v6);
        meta.ip_next_proto = hdrs.ip.v6.next_header;
        transition select(hdrs.ip.v6.next_header) {
          IP_PROTO_EXT_FRAG : Parse_IP_Frag_Last;
                    default : Parse_IP_Next_Proto_Last;
        }
    }

    state Parse_IP_Next_Proto_Last {
        transition select(meta.ip_next_proto) {
            IP_PROTO_ICMP   : Parse_ICMP_Last;    // 1
            IP_PROTO_TCP    : Parse_TCP_Last;     // 6
            IP_PROTO_UDP    : Parse_UDP_Last;     // 17
            IP_PROTO_ESP    : Parse_ESP_Last;     // 50
            IP_PROTO_ICMPV6 : Parse_ICMPv6_Last;  // 58
            IP_PROTO_SCTP   : Parse_SCTP_Last;    // 132
                  default   : Parse_IP_Pay_Last;
        }
    }

    // IPv4/6 come through this state for fragments.
    // For AVF compatibility, fragments require a dedicated PTYPE.
    // Protocol IDs are the same as PAY case.
    state Parse_IP_Frag_Last {
        meta.node_id = NODE_FRAG_LAST;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_IP_Pay_Last {
        meta.node_id = NODE_IP_PAY_LAST;
        meta.proto_stack.next.pid = PAY;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ESP_Last {
        // terminal node, everything after the esp header is ciphertext (mostly)
        meta.node_id = NODE_ESP_LAST;
        meta.proto_stack.next.pid = ESP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ARP_Last {
        meta.node_id = NODE_ARP_LAST;
        meta.proto_stack.next.pid = ARP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ICMP_Last {
        meta.node_id = NODE_ICMP_LAST;
        meta.proto_stack.next.pid = ICMP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_ICMPv6_Last {
        meta.node_id = NODE_ICMPV6_LAST;
        meta.proto_stack.next.pid = ICMPV6;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_TCP_Last {
        meta.node_id = NODE_TCP_LAST;
        meta.proto_stack.next.pid = TCP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_SCTP_Last {
        meta.node_id = NODE_SCTP_LAST;
        meta.proto_stack.next.pid = SCTP;
        meta.proto_stack.next.ho = 0;
        transition accept;
    }

    state Parse_UDP_Last {
        meta.proto_stack.next.pid = UDP_LAST;
        meta.proto_stack.next.ho = 0;
        pkt.extract(hdrs.udp);
        transition select(meta.nodexpf_onehot, hdrs.udp.dport) {
            (0, UDP_PORT_ROCEV2  )  : Parse_ROCEv2_Last; // 4791 placeholder
                            default : Parse_Maybe_QUIC_Last;
        }
    }

    state Parse_Maybe_QUIC_Last {
        transition select(meta.nodexpf_onehot, hdrs.udp.dport) {
            (0, UDP_PORT_QUIC_0  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_1  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_2  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_3  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_4  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_5  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_6  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_7  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_8  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_9  ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_10 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_11 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_12 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_13 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_14 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_15 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_16 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_17 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_18 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_19 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_20 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_21 ) : Parse_QUIC_Last;
            (0, UDP_PORT_QUIC_22 ) : Parse_QUIC_Last;
                           default : Parse_UDP_Pay_Last;
        }
    }
}

/******************************************************************************
 Define PTYPE value production.
 This table produces a PTYPE value based on 16 marker bits and a 10-bit node-id.

 Edit - we should not need to reveal this table explicitly, but just make
 PTYPE and output parameter from the parser
 ******************************************************************************/
control Generate_Ptype(in parser_user_meta_t meta, out ptype_t ptype) {
    action Set_PType(ptype_t p) {
        ptype = p;
    }
    table ptypes_cam {

        // Key bits are used in the entries lookup below, in order.
        key = {
            meta.marker_ipv6_ofos : exact;
            meta.marker_ipv6_il   : exact;
            meta.marker_udptun    : exact;
            meta.marker_iptun     : exact;
            meta.marker_ip_in_ip  : exact;
            meta.node_id          : exact;
        }
        actions = {
            Set_PType;
        }
        size = 1024;
        const entries = {
            // This include file autogenerated by gen_ptype_cam.py
            #include "ptypes_cam_entries.p4"
        }
    }

    apply {
        ptypes_cam.apply();
    }
}

#if 0
// Instantiate a V1Switch, which does not work yet with the
// the eBPF backend :/
// Compiler with implici bmv2 backend, just 'p4c parser.p4'
V1Switch<parsed_headers_t, parser_meta_t>( unified_parser(),
                                           verify_csum(),
                                           ingress(),
                                           egress(),
                                           compute_csum(),
                                           deparser())
                                           main;
#endif
control sem(in parsed_headers_t hdr,
            in parser_meta_t parserMetadata,
            inout fxp_internal_metadata_t meta)
{
    apply {
    }
}

control lem(in parsed_headers_t hdr,
            in parser_meta_t parserMetadata,
            inout fxp_internal_metadata_t meta)
{
    apply {
    }
}

control wcm(in parsed_headers_t hdr,
            in parser_meta_t parserMetadata,
            inout fxp_internal_metadata_t meta)
{
    apply {
    }
}

control  mod(packet_out pkt,
            in parsed_headers_t hdr,
            in parser_meta_t parserMetadata,
            inout fxp_internal_metadata_t meta)
{
    apply {
    }
}

ICEX(unified_parser(), sem(), wcm(), lem(), mod()) main;
