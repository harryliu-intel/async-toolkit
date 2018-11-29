// -*- mode:c -*-

// Copyright (C) 2018 Intel Corporation

#ifndef MBY_MODIFIER_H
#define MBY_MODIFIER_H

// Includes:

#include "mby_common.h"
#include "mby_bitfield.h"

// Defines:

#define DEFAULT_SEGMENT_BYTES                   192
#define VLAN_TAG_BYTES                          4
#define MIN_EGRESS_BYTES                        18

#define MBY_MOD_PROFILE_GROUPS                  8
#define MBY_MOD_FIELD_VECTOR_SIZE               24
#define MBY_MOD_FIELDS_PER_REG_ENTRY            3
#define MBY_MOD_COMMAND_PER_GROUP               4
#define MBY_MOD_PROT_ID_MD_TYPE                 253
#define MBY_MOD_MAX_HDR_REGION                  256
#define MBY_MOD_CNTR_SIZE                       256
#define MBY_MOD_CONTENT_SIZE                    256
#define MBY_MOD_MAP_MAX_SINGLE_LUT              3
#define MBY_MOD_MAP_DUAL_LUT                    4
#define MBY_MOD_MAP_INS_FLD_LUT_LEN             2
#define MBY_MOD_CONTENT_ADDR_BLOCK_SIZE         32
#define MBY_MOD_CONTENT_ENTRY_SIZE              8
#define MBY_MOD_MAP_VALUES_PER_ENTRY            4
#define MBY_MOD_MAP_DUAL_VALUES_PER_ENTRY       8

/* Modifier command encoding field bits definition */
#define MBY_MOD_CMD_l_LEN_MASK                  0
#define MBY_MOD_CMD_h_LEN_MASK                  7
#define MBY_MOD_CMD_l_PROT_ID                   8
#define MBY_MOD_CMD_h_PROT_ID                   15
#define MBY_MOD_CMD_l_OFFSET                    8
#define MBY_MOD_CMD_h_OFFSET                    13
#define MBY_MOD_CMD_b_ALIGNMENT                 14
#define MBY_MOD_CMD_l_LUT                       15
#define MBY_MOD_CMD_h_LUT                       17
#define MBY_MOD_CMD_b_UPDATE                    16
#define MBY_MOD_CMD_b_LUT_MODE                  18
#define MBY_MOD_CMD_b_PROT_DEL                  18
#define MBY_MOD_CMD_l_MODE                      18
#define MBY_MOD_CMD_h_MODE                      20
#define MBY_MOD_CMD_l_TYPE                      21
#define MBY_MOD_CMD_h_TYPE                      23

/* Modifier profile group encoding bits definition */
#define MBY_MOD_PROFILE_GROUP_l_OFFSET          0
#define MBY_MOD_PROFILE_GROUP_h_OFFSET          7
#define MBY_MOD_PROFILE_GROUP_l_PROT_ID         0
#define MBY_MOD_PROFILE_GROUP_h_PROT_ID         7
#define MBY_MOD_PROFILE_GROUP_b_CONFIG          8

#define MBY_MOD_CMD_l_LEN_MASK                  0
#define MBY_MOD_CMD_h_LEN_MASK                  7
#define MBY_MOD_CMD_l_PROT_ID                   8
#define MBY_MOD_CMD_h_PROT_ID                   15
#define MBY_MOD_CMD_l_OFFSET                    8
#define MBY_MOD_CMD_h_OFFSET                    13
#define MBY_MOD_CMD_b_ALIGNMENT                 14
#define MBY_MOD_CMD_l_LUT                       15
#define MBY_MOD_CMD_h_LUT                       17
#define MBY_MOD_CMD_b_UPDATE                    16
#define MBY_MOD_CMD_b_LUT_MODE                  18
#define MBY_MOD_CMD_b_PROT_DEL                  18
#define MBY_MOD_CMD_l_MODE                      18
#define MBY_MOD_CMD_h_MODE                      20
#define MBY_MOD_CMD_l_TYPE                      21
#define MBY_MOD_CMD_h_TYPE                      23
#define MBY_MOD_CMD_DECREMENT_1B_l_POS          0
#define MBY_MOD_CMD_DECREMENT_1B_h_POS          3

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

typedef enum mbyModCmdTypeEnum
{
    MBY_MOD_CMD_TYPE_NOP = 0,
    MBY_MOD_CMD_TYPE_INSERT,
    MBY_MOD_CMD_TYPE_INSERT_FIELD,
    MBY_MOD_CMD_TYPE_INSERT_FIELD_LUT,
    MBY_MOD_CMD_TYPE_DELETE,
    MBY_MOD_CMD_TYPE_REPLACE,
    MBY_MOD_CMD_TYPE_REPLACE_FIELD,
    MBY_MOD_CMD_TYPE_REPLACE_FIELD_LUT,

} mbyModCmdType;

typedef enum mbyModCmdModeEnum
{
    MBY_MOD_CMD_MODE_NOP = 0,
    MBY_MOD_CMD_MODE_BASIC,
    MBY_MOD_CMD_MODE_4B_REPLACE,
    MBY_MOD_CMD_MODE_1B_REPLACE,
    MBY_MOD_CMD_MODE_1B_XOR,
    MBY_MOD_CMD_MODE_1B_DECREMENT,

} mbyModCmdMode;

typedef enum mbyModCmdLutModeEnum
{
    MBY_MOD_CMD_LUT_MODE_DIRECT = 0,
    MBY_MOD_CMD_LUT_MODE_RELATIVE,

} mbyModCmdLutMode;

typedef enum mbyModCmdAlignmentEnum
{
    MBY_MOD_CMD_ALIGN_TOP = 0,
    MBY_MOD_CMD_ALIGN_BOTTOM,

} mbyModCmdAlignment;

typedef enum mbyModCmdSourceEnum
{
    MBY_MOD_CMD_SOURCE_CONTENT_REGION = 0,
    MBY_MOD_CMD_SOURCE_FIELD_CONTAINER,

} mbyModCmdSource;

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

typedef struct mbyModProfileGroupStruct {
    fm_uint16               group[MBY_MOD_PROFILE_GROUPS]; // group[0] will never be populated

} mbyModProfileGroup;

typedef struct mbyModProfileFieldStruct {
    fm_byte                 protocol_id[MBY_MOD_FIELD_VECTOR_SIZE];
    fm_uint16               offset     [MBY_MOD_FIELD_VECTOR_SIZE];

} mbyModProfileField;

typedef struct mbyModProfileCmdStruct {
    fm_uint32               cmd[MBY_MOD_PROFILE_GROUPS][MBY_MOD_COMMAND_PER_GROUP];

} mbyModProfileCmd;

typedef struct mbyModFieldVectorStruct {
    fm_byte                 field[MBY_MOD_FIELD_VECTOR_SIZE];
    fm_uint                 cur_idx;

} mbyModFieldVector;

typedef struct mbyModCmdInsertStruct {
    fm_byte                 len;
    fm_byte                 prot_id;
    fm_bool                 update;
    mbyModCmdMode           mode;

} mbyModCmdInsert;

typedef struct mbyModCmdInsertFldStruct {
    fm_byte                 len_mask;
    mbyModCmdMode           mode;

} mbyModCmdInsertFld;

typedef struct mbyModCmdInsertFldLutStruct {
    fm_byte                 lut;
    mbyModCmdLutMode        lut_mode;

} mbyModCmdInsertFldLut;

typedef struct mbyModCmdDeleteStruct {
    fm_bool                 prot_del;

} mbyModCmdDelete;

typedef struct mbyModCmdReplaceStruct {
    fm_byte                 len_mask;
    fm_byte                 offset;
    mbyModCmdAlignment      align;
    mbyModCmdMode           mode;

} mbyModCmdReplace;

typedef struct mbyModCmdReplaceFldStruct {
    fm_byte                 len_mask;
    fm_byte                 offset;
    mbyModCmdAlignment      align;
    mbyModCmdMode           mode;

} mbyModCmdReplaceFld;

typedef struct mbyModCmdReplaceFldLutStruct {
    fm_byte                 mask;
    fm_byte                 offset;
    mbyModCmdAlignment      align;
    fm_byte                 lut;
    mbyModCmdLutMode        lut_mode;

} mbyModCmdReplaceFldLut;

typedef union mbyModCmdFieldStruct {
    mbyModCmdInsert         insrt;
    mbyModCmdInsertFld      insrt_fld;
    mbyModCmdInsertFldLut   insrt_fld_lut;
    mbyModCmdDelete         del;
    mbyModCmdReplace        replace;
    mbyModCmdReplaceFld     replace_fld;
    mbyModCmdReplaceFldLut  replace_fld_lut;

} mbyModCmdField;

typedef struct mbyModDecCmdStruct {
    mbyModCmdType           type;
    mbyModCmdField          field;

} mbyModDecCmd;

typedef struct mbyModContentContainerStruct {
    fm_byte                 content[MBY_MOD_CONTENT_SIZE];
    fm_uint                 cur_idx;

} mbyModContentContainer;

// profile related metadata
typedef struct mbyModGroupConfigStruct {
    fm_bool                 valid;
    fm_uint                 grp_idx;
    // Offset coming from packet
    fm_uint                 pkt_offset;
    // Size coming from packet
    fm_uint                 pkt_size;
    // Offset coming from container
    fm_uint                 ctnr_offset;
    // Size coming from container
    fm_uint                 ctnr_size;
    /* offset to operate on in current grp, used by insert command */
    fm_uint                 grp_offset;
    // Parser header index with related protocol ID and offset
    fm_int                  pa_hdr_idx;

} mbyModGroupConfig;

typedef struct mbyModProfileActionStruct {
    mbyModProfileGroup      profile_grp;
    mbyModFieldVector       fld_vector;
    mbyModDecCmd            dec_cmd[MBY_MOD_PROFILE_GROUPS][MBY_MOD_COMMAND_PER_GROUP];
    mbyModGroupConfig       grp_list[MBY_MOD_PROFILE_GROUPS];
    fm_uint32               operating_region;
    mbyModContentContainer  content_ctnr;
} mbyModProfileAction;

typedef struct mbyTxInToModifierStruct
{
    fm_uint32               CONTENT_ADDR;  // MOD Content address, expressed in 32B units
    fm_bool                 DROP_TTL;      //
    fm_byte                 ECN;           // ECN value to use in egress packet
    fm_uint16               EDGLORT;       // egress destination glort
    fm_uint32               FNMASK;        // forwarding normal mask
    fm_bool                 IS_TIMEOUT;    //
    fm_macaddr              L2_DMAC;       // L2 destination MAC address
    fm_uint16               L2_EVID1;      // 12-bit egress VLAN ID
    fm_bool                 MARK_ROUTED;   //
    mbyMirrorType           MIRTYP;        // mirror type
    fm_uint32               MOD_IDX;       // index into the MODIFY descriptor tables
    fm_byte                 MOD_PROF_IDX;  // modify profile index
    fm_bool                 NO_MODIFY;     // skip most of modifications in Modifier
    fm_bool                 OOM;           // out of memory
    mbyParserInfo           PARSER_INFO;   //
    mbyParserHdrPtrs        PA_HDR_PTRS;   // parser header pointers
    fm_bool                 PM_ERR;        // ECC error on PM
    fm_bool                 PM_ERR_NONSOP; //
    fm_byte                 QOS_L3_DSCP;   // 6-bit QOS Differentiated Services Code Point (DSCP)
    fm_byte               * RX_DATA;       // ingress (receive) packet data
    fm_uint32               RX_LENGTH;     // ingress packet data length [bytes]
    fm_bool                 SAF_ERROR;     // SAF error
    fm_uint64               TAIL_CSUM_LEN; // L4 CSUM related information
    fm_byte               * TX_DATA;       // egress (transmit) packet data
    fm_bool                 TX_DROP;       // flag indicating packet drop
    fm_byte                 TX_TAG;        //
    fm_byte                 XCAST;         //

} mbyTxInToModifier;

typedef struct mbyModifierToTxStatsStruct
{
    fm_bool                 NO_PRI_ENC;      // do not use priority encoding, use default enc.
    fm_byte               * TX_DATA;         // egress packet data
    fm_uint16               TX_DISP;         // egress frame disposition
    fm_uint32               TX_LENGTH;       // egress packet data length [bytes]
    fm_uint32               TX_PORT;         // egress port
    fm_uint32               TX_STATS_LENGTH; // egress packet data stats length [bytes]

} mbyModifierToTxStats;

#endif
