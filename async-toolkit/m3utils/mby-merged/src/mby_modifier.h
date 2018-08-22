// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MODIFIER_H
#define MBY_MODIFIER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

// Enums:

typedef enum mbyTxTagEnum
{
    MBY_NORMAL_TAGGING = 0,
    MBY_INSERT,
    MBY_MODEL_DELETE,
    MBY_UPDATE_ADD

} mbyTxTag;

typedef enum mbyModPerPortCfg1Vid2MapIndexEnum
{
    MBY_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_VID    = 0,
    MBY_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_SGLORT = 1,
    MBY_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_DGLORT = 2

} mbyModPerPortCfg1Vid2MapIndex;

// Structs:

typedef struct mbyModPerPortCfg1Struct
{
    fm_uint16               LOOPBACK_SUPPRESS_GLORT;
    fm_uint16               LOOPBACK_SUPPRESS_MASK;
    mbyModPerPortCfg1Vid2MapIndex
                            VID2_MAP_INDEX;
    fm_bool                 ENABLE_VLAN_UPDATE;

} mbyModPerPortCfg1;

typedef struct mbyModPerPortCfg2Struct
{
    fm_bool                 ENABLE_PCP1_UPDATE;
    fm_bool                 ENABLE_PCP2_UPDATE;
    fm_bool                 ENABLE_DEI1_UPDATE;
    fm_bool                 ENABLE_DEI2_UPDATE;
    fm_byte                 VLAN1_E_TYPE;
    fm_byte                 VLAN2_E_TYPE;
    fm_bool                 ENABLE_DMAC_ROUTING;
    fm_bool                 ENABLE_SMAC_ROUTING;
    fm_bool                 ENABLE_TTL_DECREMENT;
    fm_bool                 ENABLE_ECN_MODIFICATION;
    fm_bool                 VID2_FIRST;
    fm_byte                 VLAN_TAGGING;
    fm_bool                 MIN_FRAME_SIZE;

} mbyModPerPortCfg2;

typedef struct mbyModAqmProfileStruct
{
    fm_byte                 PROFILE_7;
    fm_byte                 PROFILE_6;
    fm_byte                 PROFILE_5;
    fm_byte                 PROFILE_4;
    fm_byte                 PROFILE_3;
    fm_byte                 PROFILE_2;
    fm_byte                 PROFILE_1;
    fm_byte                 PROFILE_0;

} mbyModAqmProfile;

typedef struct mbyModRegDataStruct
{
    mbyModPerPortCfg1       modPerPortCfg1;
    mbyModPerPortCfg2       modPerPortCfg2;
    mbyModAqmProfile        modAqmProfile;

} mbyModRegData;

typedef struct mbyModControlDataStruct
{
    // Misc:
    fm_byte                 dvStatus;
    fm_bool                 isMarkerPkt;
    fm_bool                 isWindowParsing;
    // Mirror Lookup:
    fm_bool                 isMirror;
    fm_bool                 mirrorTrunc;
    // Multicast Lookup:
    fm_uint16               evidA;
    fm_uint16               dglortA;
    fm_uint16               txDglort;
    // Loopback Suppress:
    fm_bool                 vlanSwitched;
    fm_bool                 routeA;
    fm_bool                 loopbackSuppressDrop;
    fm_int                  rx_n_tag;
    fm_byte                 rx_tags[16];
    // MPLS:
    fm_bool                 isInterLSR;
#if 0 
    mbyMplsData             mplsData;  // REVISIT!!!!
#endif
    // L3:
    fm_uint32               l3Idx;
    fm_uint32               l4Idx;
    fm_bool                 otrL3Modified;
    fm_bool                 isRoutable;
    // DS transformation:
    fm_byte                 internalDS;
    fm_byte                 egressDSCP;
    fm_byte                 otrTTL;
    fm_bool                 skipDscp;
    fm_bool                 skipTtl;
    // L4:
    fm_bool                 otrL4Modified;
    // Routing:
    fm_uint16               l3_domain;
    // Priority Profile:
    fm_uint16               operator_id;
    fm_byte                 priority_profile;
    // Mod Descriptor:
    fm_uint32               modIdx;
    fm_bool                 encap;
    fm_bool                 decap;
    // Vlan data:
    fm_byte                 numVlans;
    fm_bool                 rxVlan1;
    fm_bool                 rxVlan2;
    fm_bool                 rxV2first;
    fm_bool                 txVlan1;
    fm_bool                 txVlan2;
    fm_bool                 preserveVlan1;
    fm_bool                 preserveVlan2;
    fm_byte                 txVpri1;
    fm_byte                 txVpri2;
    fm_uint16               txVid1;
    fm_uint16               txVid2;
    fm_uint16               evidB;
    fm_bool                 ecn_tx_drop;
    fm_bool                 timeout_tx_drop;
    fm_bool                 non_cm_tx_drop;
    fm_bool                 cancel_drop_on_marker;
    fm_byte                 cancelled_tx_disp;
    fm_bool                 ecn_mark;
    // Stats data:
    fm_byte                 bytesAdded;
    fm_uint32               egressSeg0Bytes;
    fm_uint32               crc_ingress_diff;
    fm_uint32               crc_egress;
    fm_uint32               refcnt_tx_len;
    fm_uint32               refcntSeg0Bytes;
    fm_uint32               tail_len;
    // intr data:
    fm_bool                 intr_occured;
    fm_uint64               mod_im;
    fm_uint16               igL3TotalLen;

} mbyModControlData;

typedef struct mbyChunkedSegStruct
{
    // Outer L2 + Ethertype:
    fm_byte                 ftag[8];
    fm_byte                 otr_dmac[6];
    fm_byte                 otr_smac[6];
    fm_byte                 otr_tags[16];
    fm_byte                 otr_et[2];
    fm_bool                 ftag_v;
    fm_byte                 n_otr_tag;

    // Outer MPLS:
    fm_byte                 otr_mpls[28];
    fm_byte                 n_otr_mpls;
    fm_byte                 n_otr_mpls_pre;
    
    // Outer IP:
    fm_byte                 otr_ip[56];
    fm_byte                 otr_ip_size;
    fm_bool                 otr_l3_v6;

    // UDP/Tunnel part 1:
    fm_byte                 otr_l4[40];
    fm_bool                 otr_udp_v;
    fm_bool                 otr_tcp_v;
    fm_byte                 tun_size_in_l4_chunk;

    // Tunnel part 2:
    fm_byte                 tun_opt[40];
    fm_byte                 tun_opt_size;

    // Inner L2 (+Ether Type):
    fm_byte                 inr_dmac[6];
    fm_byte                 inr_smac[6];
    fm_byte                 inr_tags[16];
    fm_byte                 inr_et[2];
    fm_bool                 inr_l2_v;
    fm_byte                 n_inr_tag;

    // Inner MPLS:
    fm_byte                 inr_mpls[28];
    fm_byte                 n_inr_mpls;

    // Inner IP:
    fm_byte                 inr_ip[56];
    fm_byte                 inr_ip_size;
    fm_bool                 inr_l3_v6;

    // Inner L4:
    fm_byte                 inr_l4[18];
    fm_bool                 inr_udp_v;
    fm_bool                 inr_tcp_v;

    // Payload:
    fm_byte                 payload_start;
    fm_int                  payload_size;

} mbyChunkedSeg;

typedef struct mbyModifierToTxStatsStruct
{
    fm_byte               * TX_DATA;         // egress packet data
    fm_uint32               TX_LENGTH;       // egress packet data length [bytes]
    fm_uint32               TX_PORT;         // egress port
    fm_uint16               TX_DISP;         // 4-bit egress frame disposition
    fm_uint32               TX_STATS_LENGTH; // egress packet data stats length [bytes]
    fm_bool                 SEG_DROP;        // segment drop

} mbyModifierToTxStats;

#endif
