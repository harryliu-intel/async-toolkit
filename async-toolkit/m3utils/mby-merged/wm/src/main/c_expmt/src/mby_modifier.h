// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MODIFIER_H
#define MBY_MODIFIER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

#define DEFAULT_SEGMENT_BYTES   192
#define VLAN_TAG_BYTES          4
#define MIN_EGRESS_BYTES        18

/******** MOD_BASE *******/
#define MBY_MOD_BASE                                            (0x4000000)
#define MBY_MOD_SIZE                                            (0x0800000)

#define MBY_MOD_PER_PORT_CFG1_WIDTH                             2
#define MBY_MOD_PER_PORT_CFG1_ENTRIES                           24
#define MBY_MOD_PER_PORT_CFG1(index, word)                      ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0118400) + (MBY_MOD_BASE))

#define MBY_MOD_PER_PORT_CFG1_l_LOOPBACK_SUPPRESS_GLORT         19
#define MBY_MOD_PER_PORT_CFG1_h_LOOPBACK_SUPPRESS_GLORT         34
#define MBY_MOD_PER_PORT_CFG1_l_LOOPBACK_SUPPRESS_MASK          3
#define MBY_MOD_PER_PORT_CFG1_h_LOOPBACK_SUPPRESS_MASK          18
#define MBY_MOD_PER_PORT_CFG1_l_VID2_MAP_INDEX                  1
#define MBY_MOD_PER_PORT_CFG1_h_VID2_MAP_INDEX                  2
#define MBY_MOD_PER_PORT_CFG1_b_ENABLE_VLAN_UPDATE              0

#define MBY_MOD_PER_PORT_CFG2_WIDTH                             2
#define MBY_MOD_PER_PORT_CFG2_ENTRIES                           24
#define MBY_MOD_PER_PORT_CFG2(index, word)                      ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0118500) + (MBY_MOD_BASE))

#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_PCP1_UPDATE              16
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_PCP2_UPDATE              15
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_DEI1_UPDATE              14
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_DEI2_UPDATE              13
#define MBY_MOD_PER_PORT_CFG2_l_VLAN1_E_TYPE                    11
#define MBY_MOD_PER_PORT_CFG2_h_VLAN1_E_TYPE                    12
#define MBY_MOD_PER_PORT_CFG2_l_VLAN2_E_TYPE                    9
#define MBY_MOD_PER_PORT_CFG2_h_VLAN2_E_TYPE                    10
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_DMAC_ROUTING             8
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_SMAC_ROUTING             7
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_TTL_DECREMENT            6
#define MBY_MOD_PER_PORT_CFG2_b_ENABLE_ECN_MODIFICATION         5
#define MBY_MOD_PER_PORT_CFG2_b_VID2_FIRST                      4
#define MBY_MOD_PER_PORT_CFG2_l_VLAN_TAGGING                    1
#define MBY_MOD_PER_PORT_CFG2_h_VLAN_TAGGING                    3
#define MBY_MOD_PER_PORT_CFG2_b_MIN_FRAME_SIZE                  0

#define MBY_MOD_ROUTER_SMAC_WIDTH                               2
#define MBY_MOD_ROUTER_SMAC_ENTRIES                             64
#define MBY_MOD_ROUTER_SMAC(index, word)                        ((0x0000008) * ((index) - 0) + ((word)*4)+ (0x0118600) + (MBY_MOD_BASE))

#define MBY_MOD_ROUTER_SMAC_l_SMAC                              0
#define MBY_MOD_ROUTER_SMAC_h_SMAC                              47

#define MBY_MOD_IM_WIDTH                                        2
#define MBY_MOD_IM(word)                                        (((word)*4) + (0x0120210) + (MBY_MOD_BASE))

#define MBY_MOD_IM_b_ECN_DROP                                   36
#define MBY_MOD_IM_b_TTL1_DROP                                  35
#define MBY_MOD_IM_b_LOOPBACK_DROP                              34
#define MBY_MOD_IM_b_TIMEOUT_DROP                               33
#define MBY_MOD_IM_b_L3_LEN_L4_CSUM_ERROR_MARK                  32
#define MBY_MOD_IM_b_L4_CSUM_ERROR_DROP                         31
#define MBY_MOD_IM_b_L3_LEN_ERROR_DROP                          30
#define MBY_MOD_IM_b_MARKER_ERROR_DROP                          29
#define MBY_MOD_IM_b_TX_ERROR_DROP                              28
#define MBY_MOD_IM_b_TX_ECC_DROP                                27
#define MBY_MOD_IM_b_INNER_L4_NONEXIST                          26
#define MBY_MOD_IM_b_OUTER_L4_NONEXIST                          25
#define MBY_MOD_IM_b_INNER_IP_NONEXIST                          24
#define MBY_MOD_IM_b_OUTER_IP_NONEXIST                          23
#define MBY_MOD_IM_b_INNER_MAC_NONEXIST                         22
#define MBY_MOD_IM_b_OUTER_MAC_NONEXIST                         21
#define MBY_MOD_IM_b_SIZE_ERROR_MIN                             20
#define MBY_MOD_IM_b_SIZE_ERROR_MAX                             19
#define MBY_MOD_IM_b_PKT_LEN_UPDATE                             18
#define MBY_MOD_IM_b_TTL_DEC_BELOW0                             17
#define MBY_MOD_IM_b_TTLDS_SRC_NONEXIST                         16
#define MBY_MOD_IM_b_TTLDS_TGT_NONEXIST                         15
#define MBY_MOD_IM_b_INNER_IP_MISMATCH                          14
#define MBY_MOD_IM_b_OUTER_IP_MISMATCH                          13
#define MBY_MOD_IM_b_MPLS_POP_EMPTY                             12
#define MBY_MOD_IM_b_POP_ELI_EMPTY                              11
#define MBY_MOD_IM_b_POP_AL_EMPTY                               10
#define MBY_MOD_IM_b_MPLS_PUSH_FULL                             9
#define MBY_MOD_IM_b_PUSH_ELI_FULL                              8
#define MBY_MOD_IM_b_PUSH_AL_FULL                               7
#define MBY_MOD_IM_b_INNER_TAG_EMPTY                            6
#define MBY_MOD_IM_b_OUTER_TAG_EMPTY                            5
#define MBY_MOD_IM_b_VLAN_TAG_EMPTY                             4
#define MBY_MOD_IM_b_INNER_TAG_FULL                             3
#define MBY_MOD_IM_b_OUTER_TAG_FULL                             2
#define MBY_MOD_IM_b_VLAN_TAG_FULL                              1
#define MBY_MOD_IM_b_MEM_ERROR                                  0

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

typedef enum mbyDvStatusEnum
{
    IS_OK = 0,
    EOP_ERROR,
    NOT_ENOUGH_RX_DATA,
    INVALID_IP_VERSION,
    COPY_ERROR

} mbyDvStatus;

typedef enum mbyDropErrCodeEnum
{
    ERR_NONE           = 0x00,
    ERR_VLAN_TAG_FULL  = 0x01, // done
    ERR_OTR_L2_FULL    = 0x02, // done
    ERR_INR_L2_FULL    = 0x03, // done
    ERR_VLAN_TAG_EMPTY = 0x04, //      the desired deleted vid not present
    ERR_OTR_L2_EMPTY   = 0x05, // done can't remove the number as per mode
    ERR_INR_L2_EMPTY   = 0x06, // done
    ERR_PUSH_AL        = 0x07, // done
    ERR_PUSH_ELI       = 0x08, // done
    ERR_PUSH_G         = 0x09, // done
    ERR_POP_AL         = 0x0a, // done
    ERR_POP_ELI        = 0x0b, // done
    ERR_POP_G          = 0x0c, // done
    ERR_OTR_IPV        = 0x0d, // done
    ERR_INR_IPV        = 0x0e, // done
    ERR_TTLDS_TGT      = 0x0f, // done
    ERR_TTLDS_SRC      = 0x10, //
    ERR_TTL_0          = 0x11, // done
    ERR_PKT_LEN        = 0x12, //
    ERR_SEG_G_320      = 0x13,
    ERR_SEG_S_18       = 0x14,
    ERR_OTR_MAC        = 0x15,
    ERR_INR_MAC        = 0x16,
    ERR_OTR_IP         = 0x17,
    ERR_INR_IP         = 0x18,
    ERR_OTR_L4         = 0x19,
    ERR_INR_L4         = 0x1a

} mbyDropErrCode;

typedef enum mbyMarkerFlagEnum
{
    NOMARKER = 0,
    MARKER = 1

} mbyMarkerFlag;

typedef enum mbyDropFlagEnum
{
    NODROP = 0,
    DROP = 1

} mbyDropFlag;

typedef enum mbyIntrErrCodeEnum // interrupt disp
{
    INTR_DISREGARD_ERR               = -1,
    INTR_MEM_ECC_ERR                 =  0,
    INTR_U_INSERT_VLAN_IPP           =  1,
    INTR_U_INSERT_L2_TAG_OTR         =  2,
    INTR_U_INSERT_L2_TAG_INR         =  3,
    INTR_U_REMOVE_VLAN_IPP           =  4,
    INTR_U_REMOVE_L2_TAG_OTR         =  5,
    INTR_U_REMOVE_L2_TAG_INR         =  6,
    INTR_U_PUSH_AL                   =  7,
    INTR_U_PUSH_ELI                  =  8,
    INTR_U_PUSH_G                    =  9,
    INTR_U_POP_AL                    = 10,
    INTR_U_POP_ELI                   = 11,
    INTR_U_POP_G                     = 12,
    INTR_OTR_IPV_MISMATCH            = 13,
    INTR_INR_IPV_MISMATCH            = 14,
    INTR_TTLDS_NON_TGT               = 15,
    INTR_TTLDS_NON_SRC               = 16,
    INTR_TTL_DEC_ERR                 = 17,
    INTR_PKT_LEN_MD_UPDATE_ERR       = 18,
    INTR_BIGGER_320B                 = 19,
    INTR_SMALLER_18B                 = 20,
    INTR_OTR_MAC_NONEXIST            = 21,
    INTR_INR_MAC_NONEXIST            = 22,
    INTR_OTR_IP_NONEXIST             = 23,
    INTR_INR_IP_NONEXIST             = 24,
    INTR_OTR_L4_NONEXIST             = 25,
    INTR_INR_L4_NONEXIST             = 26,
    INTR_TX_ECC_DROP                 = 27,
    INTR_TX_ERR_DROP                 = 28,
    INTR_MARKER_ERR_DROP             = 29,
    INTR_L3_LEN_ERR_DROP             = 30,
    INTR_L4_CSUM_ERR_DROP            = 31,
    INTR_L3_LEN_L4_CSUM_ERR_MASK     = 32,
    INTR_TIMEOUT_DROP                = 33,
    INTR_LPBK_DROP                   = 34,
    INTR_TTL1_DROP                   = 35,
    INTR_ECN_DROP                    = 36

} mbyIntrErrCode;

typedef enum mbyDispCodeEnum
{
    //                     index   priority  description
    //                     -----   --------  -----------
    DISP_TXECCDROP      =   0,     //  1     pm_read.err
    DISP_TXERRORDROP    =   1,     //  2     mod_ctrl.tx_drop
    DISP_MARKERERRDROP  =   2,     //  3
    DISP_L3ERRDROP      =   3,     //  3
    DISP_L4ERRDROP      =   4,     //  3
    DISP_L3L4ERRMARK    =   5,     //  3
    DISP_TIMEOUTDROP    =   6,     //  4     mod_ctrl.is_timeout
    DISP_LOOPBACKDROP   =   7,     //  5
    DISP_TTL1DROP       =   8,     //  6     fwd_modify.ttl1
    DISP_ECNDROP        =   9,     //  7
    DISP_MODERRORDROP   =  10,     //  8
    DISP_TXERROR        =  11,     //  9
    DISP_OOMTRUNC       =  12,     // 10
    DISP_UCAST          =  13,     // 11
    DISP_BCAST          =  14,     // 12
    DISP_MCAST          =  15      // 13

} mbyDispCode;

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
    mbyDvStatus             dvStatus;
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
    fm_uint32               rx_n_tag;
    fm_byte                 rx_tags[16];
    // MPLS:
    fm_bool                 isInterLSR;
#if 0
    mbyMplsData             mplsData;  // not implemented for now <-- REVISIT!!!!
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
    fm_byte                 otr_dmac[MAC_ADDR_BYTES];
    fm_byte                 otr_smac[MAC_ADDR_BYTES];
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
    fm_byte                 inr_dmac[MAC_ADDR_BYTES];
    fm_byte                 inr_smac[MAC_ADDR_BYTES];
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
    fm_uint32               payload_size;

} mbyChunkedSeg;

typedef struct mbyModifierToTxStatsStruct
{
    fm_bool                 NO_PRI_ENC;      // do not use priority encoding, use default enc.
    fm_byte               * TX_DATA;         // egress packet data
    fm_uint16               TX_DISP;         // egress frame disposition
    fm_uint32               TX_LENGTH;       // egress packet data length [bytes]
    fm_uint32               TX_STATS_LENGTH; // egress packet data stats length [bytes]
    // pass-thru:
    fm_uint32               TX_PORT;         // egress port

} mbyModifierToTxStats;

#endif
