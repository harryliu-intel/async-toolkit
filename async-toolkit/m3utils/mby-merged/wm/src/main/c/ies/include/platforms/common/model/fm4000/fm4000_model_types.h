/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_model_types.h
 * Creation Date:   June 6, 2012
 * Description:     Data structures, definitions and types for the FM4000 model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 Intel Corporation. All Rights Reserved.
 *
 * The source code contained or described herein and all documents related
 * to the source code ("Material") are owned by Intel Corporation or its
 * suppliers or licensors. Title to the Material remains with Intel
 * Corporation or its suppliers and licensors. The Material contains trade
 * secrets and proprietary and confidential information of Intel or its
 * suppliers and licensors. The Material is protected by worldwide copyright
 * and trade secret laws and treaty provisions. No part of the Material may
 * be used, copied, reproduced, modified, published, uploaded, posted,
 * transmitted, distributed, or disclosed in any way without Intel's prior
 * express written permission.
 *
 * No license under any patent, copyright, trade secret or other intellectual
 * property right is granted to or conferred upon you by disclosure or
 * delivery of the Materials, either expressly, by implication, inducement,
 * estoppel or otherwise. Any license under such intellectual property rights
 * must be express and approved by Intel in writing.
 *****************************************************************************/

#ifndef __FM4000_MODEL_TYPES_H
#define __FM4000_MODEL_TYPES_H

#include <platforms/common/model/fm4000/fm4000_model.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_MODEL_TAKE_LOCK(model)                                              \
    fmCaptureLock(&model->modelLock, FM_WAIT_FOREVER);

#define FM_MODEL_DROP_LOCK(model)   fmReleaseLock(&model->modelLock);

#define FM_MODEL_ADDR_MATCH_1(addr, csr, epl)                                  \
    ( (addr) == (fm_uint32) csr((epl)) )

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_1
 * \ingroup model4k
 *
 * \desc            Retrieves the register index corresponding to the specified
 *                  hardware address of the specified one-dimensional,
 *                  single-word register.
 *
 * \param[in]       addr is the hardware address.
 *
 * \param[in]       csr is the name of the register.
 *
 * \param[out]      index points to user-allocated storage in which this macro
 *                  will store the register index.
 *
 *****************************************************************************/
#define FM_MODEL_GET_OFFSET_1(addr, csr, index)                                \
    fm4000ModelGetOffsetMult1(addr,                                            \
                              csr(0),                                          \
                              csr ## _ENTRIES,                                 \
                              csr(1) - csr(0),                                 \
                              index,                                           \
                              NULL)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_2
 * \ingroup model4k
 *
 * \desc            Retrieves the register index corresponding to the specified
 *                  hardware address of the specified one-dimensional,
 *                  single-word register.
 *
 * \param[in]       addr is the hardware address.
 *
 * \param[in]       csr is the name of the register.
 *
 * \param[out]      index1 points to user-allocated storage in which this macro
 *                  will store the second dimension of the register index.
 *
 * \param[out]      index0 points to user-allocated storage in which this macro
 *                  will store the first dimension of the register index.
 *
 *****************************************************************************/
#define FM_MODEL_GET_OFFSET_2(addr, csr, index1, index0)                       \
    fm4000ModelGetOffsetMult2(addr,                                            \
                              csr(0, 0),                                       \
                              csr ## _ENTRIES_1,                               \
                              csr ## _ENTRIES_0,                               \
                              csr(1, 0) - csr(0, 0),                           \
                              csr(0, 1) - csr(0, 0),                           \
                              index1,                                          \
                              index0,                                          \
                              NULL)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_MULT_1
 * \ingroup model4k
 *
 * \desc            Retrieves the register index and word corresponding to the
 *                  specified hardware address of the specified one-dimensional,
 *                  multi-word register.
 *
 * \param[in]       addr is the hardware address.
 *
 * \param[in]       csr is the name of the register.
 *
 * \param[out]      index points to user-allocated storage in which this macro
 *                  will store the register index.
 *
 * \param[out]      word points to user-allocated storage in which this macro
 *                  will store the word.
 *
 *****************************************************************************/
#define FM_MODEL_GET_OFFSET_MULT_1(addr, csr, index, word)                     \
    fm4000ModelGetOffsetMult1(addr,                                            \
                              csr(0, 0),                                       \
                              csr ## _ENTRIES,                                 \
                              csr(1, 0) - csr(0, 0),                           \
                              index,                                           \
                              word)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_MULT_2
 * \ingroup model4k
 *
 * \desc            Retrieves the register index and word corresponding to the
 *                  specified hardware address of the specified two-dimensional,
 *                  multi-word register.
 *
 * \param[in]       addr is the hardware address.
 *
 * \param[in]       csr is the name of the register.
 *
 * \param[out]      index1 points to user-allocated storage in which this macro
 *                  will store the register index of the second dimension.
 *
 * \param[out]      index0 points to user-allocated storage in which this macro
 *                  will store the register index of the first dimension.
 *
 * \param[out]      word points to user-allocated storage in which this macro
 *                  will store the word.
 *
 *****************************************************************************/
#define FM_MODEL_GET_OFFSET_MULT_2(addr, csr, index1, index0, word)            \
    fm4000ModelGetOffsetMult2(addr,                                            \
                              csr(0, 0, 0),                                    \
                              csr ## _ENTRIES_1,                               \
                              csr ## _ENTRIES_0,                               \
                              csr(1, 0, 0) - csr(0, 0, 0),                     \
                              csr(0, 1, 0) - csr(0, 0, 0),                     \
                              index1,                                          \
                              index0,                                          \
                              word)

#define FM_MODEL_IN_RANGE_1(addr, csr)                                         \
    ( ( (addr) >= csr(0) ) && ( (addr) <= csr(csr ## _ENTRIES - 1) ) )

#define FM_MODEL_IN_RANGE_2(addr, csr)                                         \
    ( ( (addr) >= csr(0, 0) ) &&                                               \
      ( (addr) <= csr(csr ## _ENTRIES_1 - 1, csr ## _ENTRIES_0 - 1) ) )

#define FM_MODEL_IN_RANGE_MULT_1(addr, csr)                                    \
    ( ( (addr) >= csr(0, 0) ) &&                                               \
      ( (addr) <= csr(csr ## _ENTRIES - 1, ( csr(1, 0) - csr(0, 0) ) - 1) ) )

#define FM_MODEL_IN_RANGE_MULT_2(addr, csr)                                    \
    ( ( (addr) >= csr(0, 0, 0) ) &&                                            \
      ( (addr) <= csr(csr ## _ENTRIES_1 - 1,                                   \
                      csr ## _ENTRIES_0 - 1,                                   \
                      ( csr(0, 1, 0) - csr(0, 0, 0) ) - 1) ) )

#define fmModelIsBroadcastMacAddress(addr)                                     \
    ( ( (addr) == FM_LITERAL_U64(0xFFFFFFFFFFFF) ) ? TRUE : FALSE )

#define fmModelIsUnicastMacAddress(addr)                                       \
         ( ((addr) & FM_LITERAL_U64(0x010000000000)) == 0 )

#define fmModelIsMulticastMacAddress(addr)                                     \
         ( ( ((addr) & FM_LITERAL_U64(0x010000000000)) != 0 ) &&               \
           !fmModelIsBroadcastMacAddress(addr) )

#define FM4000_MODEL_MAX_HW_ADDR                0x3fffff
#define FM4000_MODEL_MASK_VERSION_A3            4
#define FM4000_MODEL_TRIGGERS_COUNT             64
#define FM4000_MODEL_ARP_REDIRECT_DIP_WORDS     4
#define FM4000_MODEL_ARP_REDIRECT_SIP_WORDS     4
#define FM4000_MODEL_FFU_MASTER_VALID_WORDS     2
#define FM4000_MODEL_MA_TABLE_WORDS             4
#define FM4000_MODEL_MA_TCN_FIFO_WORDS          4
#define FM4000_MODEL_RX_VPRI_MAP_WORDS          2
#define FM4000_MODEL_FFU_EGRESS_ACTIONS_COUNT   3
#define FM4000_MODEL_AMASK_WIDTH                38
#define FM4000_MODEL_AMASK_TRIGGERS_WIDTH       45
#define FM4000_MODEL_LOG_MASK_WIDTH             6

#define FM4000_MODEL_DEFAULT_DMASK              0x1FFFFFF

#define FM4000_MODEL_DMAC_IEEE_PREFIX           FM_LITERAL_U64(0x0180c2000000)
#define FM4000_MODEL_DMAC_BPDU                                                 \
    ( FM4000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x00) )
#define FM4000_MODEL_DMAC_MAC_CTRL                                             \
    ( FM4000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x01) )
#define FM4000_MODEL_DMAC_GMRP                                                 \
    ( FM4000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x20) )
#define FM4000_MODEL_DMAC_GVRP                                                 \
    ( FM4000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x21) )
#define FM4000_MODEL_DMAC_LACP                                                 \
    ( FM4000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x02) )
#define FM4000_MODEL_DMAC_PORT_AUTH                                            \
    ( FM4000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x03) )

/* Bit number for fields of FFU_SLICE_SRAM.RouteData */
#define FM4000_MODEL_FFU_ROUTE_b_ROUTE_GLORT  21
#define FM4000_MODEL_FFU_ROUTE_l_DGLORT       0
#define FM4000_MODEL_FFU_ROUTE_h_DGLORT       15
#define FM4000_MODEL_FFU_ROUTE_l_ARP_INDEX    0
#define FM4000_MODEL_FFU_ROUTE_h_ARP_INDEX    13
#define FM4000_MODEL_FFU_ROUTE_l_ARP_COUNT    14
#define FM4000_MODEL_FFU_ROUTE_h_ARP_COUNT    17

/* Bit number for FFU Flags */
#define FM4000_MODEL_FFU_FLAGS_b_DROP           0
#define FM4000_MODEL_FFU_FLAGS_b_TRAP           1
#define FM4000_MODEL_FFU_FLAGS_b_LOG            2
#define FM4000_MODEL_FFU_FLAGS_b_NO_ROUTE       3
#define FM4000_MODEL_FFU_FLAGS_b_RX_MIRROR      4

#define FM4000_MODEL_FFU_EGRESS_ACTIONS_CHUNK_SIZE 16

#define FM4000_MODEL_LOG_TYPE_TRIGGERS          (1 << 0)
#define FM4000_MODEL_LOG_TYPE_FFU               (1 << 1)
#define FM4000_MODEL_LOG_TYPE_EGRESS_ACL        (1 << 2)
#define FM4000_MODEL_LOG_TYPE_TTL_IP_MC         (1 << 3)
#define FM4000_MODEL_LOG_TYPE_ICMP              (1 << 4)
#define FM4000_MODEL_LOG_TYPE_IP_UCST_L2_MCST   (1 << 5)

/* Trap codes which forms lower 8-bit of CPU-glort */
#define FM4000_MODEL_CPU_CODE_TRAP              0x80;
#define FM4000_MODEL_CPU_CODE_FFU_LOG           0x81;
#define FM4000_MODEL_CPU_CODE_RSVD              0x82;
#define FM4000_MODEL_CPU_CODE_LACP              0x83;
#define FM4000_MODEL_CPU_CODE_BPDU              0x84;
#define FM4000_MODEL_CPU_CODE_GARP              0x85;
#define FM4000_MODEL_CPU_CODE_IGMP              0x86;
#define FM4000_MODEL_CPU_CODE_802x              0x87;
#define FM4000_MODEL_CPU_CODE_SECURITY          0x88;
#define FM4000_MODEL_CPU_CODE_ICMP              0x90;
#define FM4000_MODEL_CPU_CODE_IP_OPTION         0x91;
#define FM4000_MODEL_CPU_CODE_CPU_ADDRESS       0x92;
#define FM4000_MODEL_CPU_CODE_TYPE              0x93;
#define FM4000_MODEL_CPU_CODE_MTU               0x94;
#define FM4000_MODEL_CPU_CODE_EG_ACL_LOG        0x95;
#define FM4000_MODEL_CPU_CODE_TRAP_TTL          0x96;
#define FM4000_MODEL_CPU_CODE_IP_UCST_L2_MCST   0x97;

#define FM4000_MODEL_TCN_EVENT_MAC_SV           (1 << 0)
#define FM4000_MODEL_TCN_EVENT_PORT_SV          (1 << 1)
#define FM4000_MODEL_TCN_EVENT_SET_FULL         (1 << 2)
#define FM4000_MODEL_TCN_EVENT_LEARNED          (1 << 3)

/* Frame types */
#define FM4000_MODEL_ETYPE_IPv4                 0x0800
#define FM4000_MODEL_ETYPE_IPv6                 0x86DD
#define FM4000_MODEL_ETYPE_MAC_CONTROL          0x8808
#define FM4000_MODEL_IPV6_OPTION_HOP_BY_HOP     0
#define FM4000_MODEL_IPV6_OPTION_ROUTING        43
#define FM4000_MODEL_IPV6_OPTION_FRAG           44
#define FM4000_MODEL_IPV6_OPTION_DEST           60
#define FM4000_MODEL_IPV6_OPTION_AUTH           51
#define FM4000_MODEL_PROT_TCP                   6
#define FM4000_MODEL_PROT_UDP                   17
#define FM4000_MODEL_PROT_ICMPv4                1
#define FM4000_MODEL_PROT_ICMPv6                58
#define FM4000_MODEL_PROT_IGMP                  2

/* Constants for FFU scenario */
#define FM4000_MODEL_FFU_SCENARIO_NOT_IP                 0
#define FM4000_MODEL_FFU_SCENARIO_IP_V4                  1
#define FM4000_MODEL_FFU_SCENARIO_IP_V6                  2
#define FM4000_MODEL_FFU_SCENARIO_IP_V4_IN_V6            3
#define FM4000_MODEL_FFU_SCENARIO_SWITCHED               0
#define FM4000_MODEL_FFU_SCENARIO_SWITCHED_GLORT         4
#define FM4000_MODEL_FFU_SCENARIO_MGMT                   8
#define FM4000_MODEL_FFU_SCENARIO_SPECIAL                12
#define FM4000_MODEL_FFU_SCENARIO_ROUTABLE               16
#define FM4000_MODEL_FFU_SCENARIO_ROUTED_GLORT           20
#define FM4000_MODEL_FFU_SCENARIO_ROUTABLE_MULTICAST     24
#define FM4000_MODEL_FFU_SCENARIO_ROUTED_MULTICAST_GLORT 28

#define MAX(x, y)                   ( ( (x) > (y) ) ? (x) : (y) )
#define MIN(x, y)                   ( ( (x) < (y) ) ? (x) : (y) )

/* Action Type bit numbers */
typedef enum
{
    /* forwarded with fixed destination mask */
    FM4000_MODEL_ACTION_SPECIAL = 0,

    /* forwarded normally*/
    FM4000_MODEL_ACTION_NORMAL,

    /* flooded due to unknown destination */
    FM4000_MODEL_ACTION_FLOOD,

    /* dropped due to security violation */
    FM4000_MODEL_ACTION_DROP_SV,

    /* dropped due to vlan ingress violation */
    FM4000_MODEL_ACTION_DROP_IV,

    /* dropped due to vlan egress violation */
    FM4000_MODEL_ACTION_DROP_EV,

    /* dropped due to a trigger action */
    FM4000_MODEL_ACTION_DROP_TRIG,

    /* trap special multicast addresses */
    FM4000_MODEL_ACTION_TRAP,

    /* dropped due to tagged/untagged config */
    FM4000_MODEL_ACTION_DROP_TAG,

    /* dropped due to spanning tree*/
    FM4000_MODEL_ACTION_DROP_STP_INL,

    /* dropped due to spanning tree */
    FM4000_MODEL_ACTION_DROP_STP_IL,

    /* dropped due to spanning tree */
    FM4000_MODEL_ACTION_DROP_STP_E,

    /* dropped pause frame */
    FM4000_MODEL_ACTION_DROP_PAUSE,

    /* dropped due to parity error */
    FM4000_MODEL_ACTION_DROP_PARITY,

    /* redirected due to trigger match */
    FM4000_MODEL_ACTION_REDIRECT_TRIG,

    /* trapped due to trigger match */
    FM4000_MODEL_ACTION_TRAP_TRIG,

    /* drop due to flood control of DLF frames */
    FM4000_MODEL_ACTION_DROP_DLF,

    /* dropped broadcast frames (SYS_CFG_1[15]) */
    FM4000_MODEL_ACTION_DROP_BCST,

    /* dropped due to FFU flag */
    FM4000_MODEL_ACTION_DROP_FFU,

    /* dropped due to FFU flag */
    FM4000_MODEL_ACTION_TRAP_FFU,

    /* dropped due the policer_drop flag from pre. */
    FM4000_MODEL_ACTION_DROP_POLICER,

    FM4000_MODEL_ACTION_DROP_TTL,

    FM4000_MODEL_ACTION_DROP_CAM,

    /* drop due to rate limiter (CM) */
    FM4000_MODEL_ACTION_DROP_RATE,

    /* drop due to privileged watermark (CM) */
    FM4000_MODEL_ACTION_DROP_PRIV,

    /*  drop due to SMP shared watermark (CM) */
    FM4000_MODEL_ACTION_DROP_SHARED,

    /* drop due to rx hog watermark (CM)*/
    FM4000_MODEL_ACTION_DROP_RX_HOG,

    /*  drop due to tx hog watermark (CM) */
    FM4000_MODEL_ACTION_DROP_TX_HOG,

    /*  drop due to illegal SMP membership (CM) */
    FM4000_MODEL_ACTION_DROP_BAD_SMP,

    /* drop due to parse error */
    FM4000_MODEL_ACTION_DROP_PARSE

} fm4000_modelActionType;

typedef enum
{
    FM4000_MODEL_PACKET_TOO_SHORT = 0x1,

    FM4000_MODEL_PACKET_TOO_LONG = 0x2,

    FM4000_MODEL_PACKET_BAD_FCS = 0x4

} fm4000_modelRxFlags;

typedef enum
{
    FM4000_MODEL_ISL_NONE = 0,

    FM4000_MODEL_ISL_F32,

    FM4000_MODEL_ISL_F64,

    FM4000_MODEL_ISL_F96,

    FM4000_MODEL_ISL_X32,

    FM4000_MODEL_ISL_X64,

    FM4000_MODEL_ISL_X96

} fm4000_modelIslTag;

typedef enum
{
    FM4000_MODEL_FTYPE_NORMAL = 0,

    FM4000_MODEL_FTYPE_ROUTED,

    FM4000_MODEL_FTYPE_SPECIAL,

    FM4000_MODEL_FTYPE_MGMT

} fm4000_modelFType;

typedef enum
{
    FM4000_MODEL_MTYPE_REQUEST = 0,

    FM4000_MODEL_MTYPE_RESPONSE,

    FM4000_MODEL_MTYPE_RESERVED1,

    FM4000_MODEL_MTYPE_RESERVED2

} fm4000_modelMType;

typedef enum
{
    FM4000_MODEL_VTYPE_NONE = 0,

    FM4000_MODEL_VTYPE_C_VLAN,

    FM4000_MODEL_VTYPE_S_VLAN_A,

    FM4000_MODEL_VTYPE_S_VLAN_B

} fm4000_modelVType;

typedef enum
{
    FM4000_MODEL_ARP_TYPE_MAC = 0,

    FM4000_MODEL_ARP_TYPE_GLORT,

    FM4000_MODEL_ARP_TYPE_IPv4InIPv6

} fm4000_modelArpEntryType;

typedef enum
{
    FM4000_MODEL_MA_STATE_INVALID = 0,

    FM4000_MODEL_MA_STATE_OLD,

    FM4000_MODEL_MA_STATE_YOUNG,

    FM4000_MODEL_MA_STATE_LOCKED

} fm4000_modelMAEntryState;

typedef enum
{
    FM4000_MODEL_STP_STATE_DISABLE = 0,

    FM4000_MODEL_STP_STATE_LISTENING,

    FM4000_MODEL_STP_STATE_LEARNING,

    FM4000_MODEL_STP_STATE_FORWARD

} fm4000_modelSTPState;

typedef enum
{
    FM4000_MODEL_TCN_TYPE_LEARNED = 0,

    FM4000_MODEL_TCN_TYPE_AGED,

    FM4000_MODEL_TCN_TYPE_SET_FULL,

    FM4000_MODEL_TCN_TYPE_PARITY_ERROR,

    FM4000_MODEL_TCN_TYPE_MAC_SV,

    FM4000_MODEL_TCN_TYPE_PORT_SV,

    FM4000_MODEL_TCN_TYPE_PURGE_COMPLETED

} fm4000_modelTcnType;

typedef enum
{
    FM4000_MODEL_FFU_SLICE_SRAM_COMMAND_NOP = 0,

    FM4000_MODEL_FFU_SLICE_SRAM_COMMAND_ROUTE,

    FM4000_MODEL_FFU_SLICE_SRAM_COMMAND_BITSET,

    FM4000_MODEL_FFU_SLICE_SRAM_COMMAND_FIELDSET

} fm4000_modelFfuSliceSramCommand;

typedef enum
{
    FM4000_MODEL_FFU_SLICE_SRAM_SUB_COMMAND_CHANGE_FLAG = 0,

    FM4000_MODEL_FFU_SLICE_SRAM_SUB_COMMAND_CHANGE_TRIG,

    FM4000_MODEL_FFU_SLICE_SRAM_SUB_COMMAND_CHANGE_USR,

    FM4000_MODEL_FFU_SLICE_SRAM_SUB_COMMAND_CHANGE_ACTION_A,

    FM4000_MODEL_FFU_SLICE_SRAM_SUB_COMMAND_CHANGE_ACTION_B

} fm4000_modelFfuSliceSramSubCommand;

/**************************************************
 * Register Structures. These structures
 * mimic the equivalent registers as defined
 * in the FM4000 datasheet.
 **************************************************/

typedef struct _fm4000_modelIngressVidTable
{
    fm_bool   parityError;

    fm_bool   reflect;

    fm_byte   CounterIndex;

    fm_uint64 membership;

    fm_uint16 FID;

    fm_bool   TrapIGMP;

} fm4000_modelIngressVidTable;

typedef struct _fm4000_modelEgressVidTable
{
    fm_bool   parityError;

    fm_byte   MTU_Index;

    fm_uint32 membership;

    fm_uint16 FID;

    fm_byte   TrigID;

} fm4000_modelEgressVidTable;

typedef struct _fm4000_modelMaTable
{
    fm_macaddr               MACAddress;

    fm_uint16                FID;

    fm4000_modelMAEntryState state;

    fm_bool                  parityError;

    fm_uint32                destMask;

    fm_uint16                destGlort;

    fm_bool                  destType;

    fm_byte                  trigId;

} fm4000_modelMaTable;

typedef struct _fm4000_modelMaTcnFifo
{
    fm4000_modelMaTable Entry;

    fm_uint32           Index;

    fm_uint32           Set;

    fm_byte             Type;

    fm_bool             ParityError;

} fm4000_modelMaTcnFifo;

typedef struct _fm4000_modelMaTcnPtr
{
    fm_uint16 Head;

    fm_uint16 Tail;

} fm4000_modelMaTcnPtr;

typedef struct _fm4000_modelMaTableCfg2
{
    fm_uint16 FloodGlort;

    fm_uint16 XcastGlort;

} fm4000_modelMaTableCfg2;

typedef struct _fm4000_modelMaTableCfg3
{
    fm_bool softAging;

    fm_bool softLearning;

    fm_byte TrigIdDefault;

} fm4000_modelMaTableCfg3;

typedef struct _fm4000_modelIngressFidTable
{
    fm_bool   parityError;

    fm_uint64 STPState;

} fm4000_modelIngressFidTable;

typedef struct _fm4000_modelEgressFidTable
{
    fm_bool   parityError;

    fm_uint32 Forwarding;

} fm4000_modelEgressFidTable;

typedef struct _fm4000_modelMaTableCfg1
{
    fm_byte hashRotation;

    fm_bool tablePartition;

} fm4000_modelMaTableCfg1;

typedef struct _fm4000_modelSysCfg1
{
    fm_bool trapLACP;

    fm_bool trapBPDU;

    fm_bool trapGARP;

    fm_bool trapSlow;

    fm_bool remapIEEESP15;

    fm_bool remapCPUSP15;

    fm_bool dropPause;

    fm_bool trapMTUViolations;

    fm_bool trap802_1x;

    fm_bool enableTrapPlusLog;

    fm_bool floodControlMulticast;

    fm_bool floodControlUnicast;

} fm4000_modelSysCfg1;

typedef struct _fm4000_modelSysCfg8
{
    fm_bool enableFFU;

    fm_bool allowQTagPause;

} fm4000_modelSysCfg8;

typedef struct _fm4000_modelL234HashCfg
{
    fm_bool UseL2ifIP;

    fm_bool UseL34;

    fm_bool Symmetric;

    fm_bool UseDMAC;

    fm_bool UseSMAC;

    fm_bool UseType;

    fm_bool UseVPRI;

    fm_bool UseVID;

    fm_byte RotationA;

    fm_byte RotationB;

    fm_bool TahoeCompatible;

} fm4000_modelL234HashCfg;

typedef struct _fm4000_modelL34HashCfg
{
    fm_bool Symmetric;

    fm_bool UseSIP;

    fm_bool UseDIP;

    fm_bool UsePROT;

    fm_bool UseTCP;

    fm_bool UseUDP;

    fm_bool UsePROT1;

    fm_bool UsePROT2;

    fm_bool UseL4SRC;

    fm_bool UseL4DST;

    fm_byte ECMP_Rotation;

    fm_byte PROT1;

    fm_byte PROT2;

} fm4000_modelL34HashCfg;

typedef struct _fm4000_modelL34FlowHashCfg1
{
    fm_byte   DiffServMask;

    fm_uint16 UserMask;

} fm4000_modelL34FlowHashCfg1;

typedef struct _fm4000_modelRxMirrorCfg
{
    fm_uint16 rxMirrorGlort;

    fm_int    rxMirrorPort;

} fm4000_modelRxMirrorCfg;

typedef struct _fm4000_modelArpTable
{
    fm_macaddr               DMAC;

    fm_uint16                dstGlort;

    fm_byte                  MTU_Index;

    fm_uint16                VLAN;

    fm4000_modelArpEntryType type;

    fm_bool                  parityError;

    fm_byte                  VRID;

} fm4000_modelArpTable;

typedef struct _fm4000_modelGlortCam
{
    fm_uint16 value;

    fm_uint16 mask;

} fm4000_modelGlortCam;

typedef struct _fm4000_modelGlortRam
{
    fm_bool   ParityError;

    fm_byte   Strict;

    fm_uint16 DestIndex;

    fm_byte   RangeSubIndexA;

    fm_byte   RangeSubIndexB;

    fm_byte   DestCount;

    fm_bool   HashRotation;

} fm4000_modelGlortRam;

typedef struct _fm4000_modelGlortDestTable
{
    fm_bool   ParityError;

    fm_uint32 DestMask;

    fm_uint16 IP_MulticastIndex;

} fm4000_modelGlortDestTable;

typedef struct _fm4000_modelLagCfg
{
    fm_byte LagSize;

    fm_byte Index;

    fm_bool HashRotation;

    fm_bool InLAG;

} fm4000_modelLagCfg;

typedef struct _fm4000_modelCanonicalGlortCam
{
    fm_uint16 LagGlort;

    fm_byte   MaskSize;

    fm_byte   PortFieldSize;

} fm4000_modelCanonicalGlortCam;

typedef struct _fm4000_modelStatsCfg
{
    fm_bool EnableGroup1;

    fm_bool EnableGroup2;

    fm_bool EnableGroup3;

    fm_bool EnableGroup4;

    fm_bool EnableGroup5;

    fm_bool EnableGroup6;

    fm_bool EnableGroup7;

    fm_bool EnableGroup8;

    fm_bool EnableGroup9;

    fm_bool EnableGroup10;

    fm_bool EnableGroup11;

    fm_bool EnableGroup12;

    fm_bool EnableGroup13;

    fm_bool EnableGroup14;

    fm_bool PrioritySelect;

} fm4000_modelStatsCfg;

typedef struct _fm4000_modelFfuMapSrc
{
    fm_byte MAP_SRC;

    fm_bool Routable;

} fm4000_modelFfuMapSrc;

typedef struct _fm4000_modelFfuMapMac
{
    fm_uint64 MAC;

    fm_byte IgnoreLength;

    fm_bool validSMAC;

    fm_bool validDMAC;

    fm_byte MAP_MAC;

    fm_bool Router;

} fm4000_modelFfuMapMac;

typedef struct _fm4000_modelFfuMapVlan
{
    fm_uint16 MAP_VLAN;

    fm_bool Routable;

    fm_bool ParityError;

} fm4000_modelFfuMapVlan;

typedef struct _fm4000_modelFfuMapType
{
    fm_uint16 TYPE;

    fm_byte MAP_ETYPE;

} fm4000_modelFfuMapType;

typedef struct _fm4000_modelFfuMapLength
{
    fm_uint16 LENGTH;

    fm_byte MAP_LENGTH;

} fm4000_modelFfuMapLength;

typedef struct _fm4000_modelFfuMapIpCfg
{
    fm_byte IgnoreLength;

    fm_bool validSIP;

    fm_bool validDIP;

    fm_byte MAP_IP;

} fm4000_modelFfuMapIpCfg;

typedef struct _fm4000_modelFfuMapProt
{
    fm_byte PROT;

    fm_byte MAP_PROT;

} fm4000_modelFfuMapProt;

typedef struct _fm4000_modelFfuMapL4Src
{
    fm_uint16 L4SRC;

    fm_byte MAP_PROT;

    fm_bool VALID;

    fm_uint16 MAP_L4SRC;

} fm4000_modelFfuMapL4Src;

typedef struct _fm4000_modelFfuMapL4Dst
{
    fm_uint16 L4DST;

    fm_byte MAP_PROT;

    fm_bool VALID;

    fm_uint16 MAP_L4DST;

} fm4000_modelFfuMapL4Dst;

typedef struct _fm4000_modelFfuInitSlice
{
    fm_byte ActiveSlices;

    fm_byte Repair1;

    fm_byte Repair2;

} fm4000_modelFfuInitSlice;

typedef struct _fm4000_modelFfuMasterValid
{
    fm_uint32 SliceValid;

    fm_uint32 ChunkValid;

} fm4000_modelFfuMasterValid;

typedef struct _fm4000_modelFfuEgressChunkCfg
{
    fm_uint32 DstPortMask;

    fm_bool StartCascade;

} fm4000_modelFfuEgressChunkCfg;

typedef struct _fm4000_modelFfuEgressActions
{
    fm_bool Drop;

    fm_bool Log;

    fm_bool Count;

} fm4000_modelFfuEgressActions;

typedef struct _fm4000_modelFfuSliceTcam
{
    /* Key contains 36 bit value of KeyLow and KeyTop */
    fm_uint64 Key;

    /* Mask contains 36 bit value of MaskLow and MaskTop */
    fm_uint64 Mask;

    fm_bool Valid;

    fm_bool Case;

} fm4000_modelFfuSliceTcam;

typedef struct _fm4000_modelFfuSliceSram
{
    fm_bool ParityError;

    fm_byte Precedence;

    fm_byte CounterBank;

    fm_uint16 CounterIndex;

    fm4000_modelFfuSliceSramCommand Command;

    fm4000_modelFfuSliceSramSubCommand SubCommand;

    fm_uint32 RouteData;

    fm_uint16 ShortData;

    fm_byte ByteMask;

    fm_byte ByteData;

    fm_bool SetDSCP;

    fm_bool SetVLAN;

    fm_bool SetVPRI;

    fm_bool SetPRI;

    fm_byte PRI;

    fm_uint16 VLAN;

    fm_byte DSCP;

} fm4000_modelFfuSliceSram;

typedef struct _fm4000_modelFfuSliceCaseCfg
{
    fm_byte Select0;

    fm_byte Select1;

    fm_byte Select2;

    fm_byte Select3;

    fm_byte SelectTop;

    fm_bool StartCompare;

    fm_bool StartAction;

} fm4000_modelFfuSliceCaseCfg;

/***************************************************
 * The following structures are helper structures
 * for modelling the FM4000 chip.
 **************************************************/

/* Structure used to store different fields of key used in FFU */
typedef struct _fm4000_modelFfuKey
{
    fm_byte   MAP_SRC;

    fm_byte   MAP_DMAC;

    fm_byte   MAP_SMAC;

    fm_uint16 MAP_VLAN;

    /* The MAP_TYPE symbol name is already taken by <sys/mman.h> */
    fm_byte   MAP_ETYPE;

    fm_byte   MAP_PROT;

    fm_byte   MAP_LENGTH;

    fm_byte   MAP_DIP;

    fm_byte   MAP_SIP;

    fm_uint16 MAP_L4DST;

    fm_uint16 MAP_L4SRC;

    fm_bool   Router;

    fm_bool   Routable;

} fm4000_modelFfuKey;

typedef struct _fm4000_modelPrecVal
{
    fm_byte   prec;

    fm_uint32 val;

} fm4000_modelPrecVal;

typedef struct _fm4000_modelIngressActions
{
    fm_bool             PARITY_ERROR;

    fm4000_modelPrecVal FLAGS[8];

    fm4000_modelPrecVal ROUTE;

    fm4000_modelPrecVal SET_VLAN;

    fm4000_modelPrecVal SET_VPRI;

    fm4000_modelPrecVal SET_PRI;

    fm4000_modelPrecVal SET_DSCP;

    fm4000_modelPrecVal SET_USR[8];

    fm4000_modelPrecVal SET_TRIG[8];

    fm4000_modelPrecVal ACTION_A;

    fm4000_modelPrecVal ACTION_B;

    fm4000_modelPrecVal COUNT[4];

} fm4000_modelIngressActions;

typedef struct _fm4000_modelFfuControl
{
    fm_bool            sramParity;

    fm_byte            scenario;

    fm_bool            rawHit[FM4000_FFU_SLICE_SRAM_ENTRIES_0];

    /* To indicate whether cascading action hit */
    fm_bool            hit;

    fm_uint16          actionIndex;

    fm_int             sliceIndex;

} fm4000_modelFfuControl;

/* Slice result */
typedef struct _fm4000_modelLookupInfo
{
    fm_uint64 key;

    fm_uint64 mask;

    fm_int    index;

} fm4000_modelLookupInfo;

/***************************************************
 * RX_EPL & TX_EPL related types.
 **************************************************/

typedef struct _fm4000_modelMacCfg1
{
    fm_byte   HeaderOffset;

    /* The maximum packet size in units of bytes. */
    fm_uint32 MaxFrameSize;

    /* The minimum packet size in units of bytes. */
    fm_uint32 MinFrameSize;

} fm4000_modelMacCfg1;

typedef struct _fm4000_modelMacCfg2
{
    fm_bool           DisableRX_MAC;

    fm_bool           DisableTX_MAC;

    fm_bool           CheckParseError;

    fm_bool           RuntFrameDiscard;

    fm_bool           RX_CRC_Discard;

    fm_bool           RX_OversizeDiscard;

    fm_bool           PadRuntFrames;

    fm_bool           DrainTX;

    fm_bool           EnableVPriUpdate;

    fm_bool           EnableVCfiUpdate;

    fm4000_modelVType VLAN_EType;

    fm_bool           EnableRouting;

} fm4000_modelMacCfg2;

typedef struct _fm4000_modelMacVlanEType
{
    fm_uint16 CTagType;

    fm_uint16 F32Type;

    fm_uint16 STagTypeA;

    fm_uint16 STagTypeB;

} fm4000_modelMacVlanEType;

typedef struct _fm4000_modelParseCfg
{
    fm_byte   ISLTag;

    fm_bool   SendOtherL3;

    fm_bool   ParseL3;

    fm_bool   ParseL4;

    fm_bool   FlagIPv4Options;

    fm_uint16 CNType;

} fm4000_modelParseCfg;

typedef struct _fm4000_modelTxTrunc
{
    fm_uint32 CpuTruncationLen;

    fm_uint32 MirrorTruncationLen;

} fm4000_modelTxTrunc;

/***************************************************
 * ASSOC related types
 **************************************************/

typedef struct _fm4000_modelHashKeys
{
    fm_byte l234Key[16];

    fm_bool replaceVid;

    fm_bool replaceVpri;

    fm_bool zeroL2;

    fm_bool zeroL34;

    fm_bool useL34;

    fm_byte rotA;

    fm_byte rotB;

    fm_bool tahoeCompatible;

    fm_byte l34Key[42];

    fm_byte arpEcmpCfg;

} fm4000_modelHashKeys;

typedef struct _fm4000_modelPortCfgIsl
{
    fm_uint16 srcGlort;

    fm_byte   USR;

    fm_byte   defaultPriority;

} fm4000_modelPortCfgIsl;

typedef struct _fm4000_modelPortCfg1
{
    fm_uint16 defaultVID;

    fm_byte   defaultVPRI;

    fm_byte   defaultDSCP;

    fm_bool   dropTagged;

    fm_bool   dropUntagged;

    fm_bool   dropMgmtISL;

    fm_bool   useDefaultVLAN;

    fm_bool   useDefaultDSCP;

    fm_bool   SwitchPriorityFromVLAN;

    fm_bool   SwitchPriorityFromDSCP;

    fm_bool   SwitchPriorityFromISL;

    fm_bool   SwitchPriorityPrefersDSCP;

    fm_bool   AssumeUntagged;

} fm4000_modelPortCfg1;

/***************************************************
 * L2_LOOKUP related types
 **************************************************/

typedef enum
{
    FM4000_MODEL_FH_CMD_LOOKUP_DA = 0,

    FM4000_MODEL_FH_CMD_LOOKUP_DA_SA

} fm4000_modelFhCmd;

/***************************************************
 * GEN_MASK1 & GEN_MASK2 related types
 **************************************************/

typedef struct _fm4000_modelPortCfg3
{
    fm_bool filterVLANIngress;

    fm_bool LearningEnable;

    fm_byte SecurityType;

    fm_byte RelaxSourceLookup;

} fm4000_modelPortCfg3;

typedef struct _fm4000_modelSysCfgRouter
{
    fm_byte TTLdisposal;

    fm_bool trapIPOptions;

} fm4000_modelSysCfgRouter;

/***************************************************
 * TRIGGERS related types
 **************************************************/

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_FORWARDING_AS_IS = 0,

    FM4000_MODEL_TRIG_ACTION_FORWARDING_FORWARD,

    FM4000_MODEL_TRIG_ACTION_FORWARDING_REDIRECT,

    FM4000_MODEL_TRIG_ACTION_FORWARDING_DROP

} fm4000_modelTriggerActionForwarding;

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_MIRRORING_AS_IS = 0,

    FM4000_MODEL_TRIG_ACTION_MIRRORING_MIRROR

} fm4000_modelTriggerActionMirroring;

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_SWITCH_PRI_AS_IS = 0,

    FM4000_MODEL_TRIG_ACTION_SWITCH_PRI_REASSIGN

} fm4000_modelTriggerActionSwitchPri;

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_TRAP_AS_IS = 0,

    FM4000_MODEL_TRIG_ACTION_TRAP_TRAP,

    FM4000_MODEL_TRIG_ACTION_TRAP_LOG,

    FM4000_MODEL_TRIG_ACTION_TRAP_REVERT

} fm4000_modelTriggerActionTrap;

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_TYPE_FORWARD = 0,

    FM4000_MODEL_TRIG_ACTION_TYPE_TRAP,

    FM4000_MODEL_TRIG_ACTION_TYPE_MIRROR,

    FM4000_MODEL_TRIG_ACTION_TYPE_PRI,

    FM4000_MODEL_TRIG_ACTION_TYPE_VLAN,

    FM4000_MODEL_TRIG_ACTION_TYPE_LEARN,

    FM4000_MODEL_TRIG_ACTION_TYPE_RATE

} fm4000_modelTriggerActionType;

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_VLAN_AS_IS = 0,

    FM4000_MODEL_TRIG_ACTION_VLAN_REASSIGN

} fm4000_modelTriggerActionVlan;

typedef enum
{
    FM4000_MODEL_TRIG_ACTION_LEARNING_AS_IS = 0,

    FM4000_MODEL_TRIG_ACTION_LEARNING_DONT_LEARN,

    FM4000_MODEL_TRIG_ACTION_LEARNING_FORCE_LEARN

} fm4000_modelTriggerActionLearning;

typedef struct _fm4000_modelTriggerActions
{
    /** TRIGGERS forwarding action. */
    fm4000_modelTriggerActionForwarding forwardingAction;
    fm_uint16                           newDestGlort;
    fm_uint32                           newDestGlortMask;
    fm_uint32                           newDestMask;
    fm_bool                             filterDestMask;
    fm_uint32                           dropMask;

    /** TRIGGERS trap action. */
    fm4000_modelTriggerActionTrap       trapAction;
    fm_bool                             cpuTruncate;
    fm_byte                             cpuCode;

    /** TRIGGERS mirroring action. */
    fm4000_modelTriggerActionMirroring  mirroringAction;
    fm_uint16                           mirrorGlort;
    fm_byte                             mirrorPort;
    fm_bool                             mirrorTruncate;

    /** TRIGGERS ISL switch priority action. */
    fm4000_modelTriggerActionSwitchPri  switchPriAction;
    fm_byte                             newSwitchPri;

    /** TRIGGERS VLAN action. */
    fm4000_modelTriggerActionVlan       vlanAction;
    fm_uint16                           newVlan;

    /** TRIGGERS learning action. */
    fm4000_modelTriggerActionLearning   learningAction;

} fm4000_modelTriggerActions;

typedef struct _fm4000_modelTriggerResults
{
    fm4000_modelActionType              action;

    fm_uint16                           destGlort;
    fm_uint32                           destMask;
    fm_bool                             filterDestMask;

    fm4000_modelTriggerActionTrap       trapAction;
    fm_bool                             cpuTruncate;
    fm_byte                             cpuCode;

    fm_bool                             rxMirror;
    fm_uint16                           mirrorGlort;
    fm_byte                             mirrorPort;
    fm_bool                             mirrorTruncate;

    fm_byte                             switchPri;

    fm_uint16                           vlan;

    fm4000_modelTriggerActionLearning   learningAction;

} fm4000_modelTriggerResults;

typedef struct _fm4000_modelTriggerActionCfg1
{
    fm_byte ForwardingAction;

    fm_byte TrapAction;

    fm_bool MirroringAction;

    fm_bool SwitchPriAction;

    fm_bool VlanAction;

    fm_byte LearningAction;

    fm_bool RateLimitAction;

} fm4000_modelTriggerActionCfg1;

typedef struct _fm4000_modelTriggerActionCfg2
{
    fm_bool   CpuTruncate;

    fm_bool   MirrorTruncate;

    fm_byte   NewSwitchPri;

    fm_uint16 NewVlan;

    fm_byte   RateLimitNum;

} fm4000_modelTriggerActionCfg2;

typedef struct _fm4000_modelTriggerActionDmask
{
    fm_uint32 NewDestMask;

    fm_bool   FilterDestMask;

} fm4000_modelTriggerActionDmask;

typedef struct _fm4000_modelTriggerActionGlort
{
    fm_uint16 NewDestGlort;

    fm_uint16 NewDestGlortMask;

} fm4000_modelTriggerActionGlort;

typedef struct _fm4000_modelTriggerActionMirror
{
    fm_byte   MirrorPort;

    fm_uint16 MirrorGlort;

} fm4000_modelTriggerActionMirror;

typedef struct _fm4000_modelTriggerConditionCfg
{
    fm_byte MatchSA;

    fm_byte MatchDA;

    fm_byte MatchHitSA;

    fm_byte MatchHitDA;

    fm_byte MatchHitSADA;

    fm_byte MatchVlan;

    fm_byte MatchFFU;

    fm_byte MatchSwitchPri;

    fm_byte MatchEtherType;

    fm_byte MatchDestGlort;

    fm_bool MatchByPrecedence;

    fm_bool MatchRandomNumber;

    fm_bool MatchRandomIfLess;

    fm_bool MatchRandomThreshold;

} fm4000_modelTriggerConditionCfg;

typedef struct _fm4000_modelTriggerConditionFfu
{
    fm_byte FFU_ID;

    fm_byte FFU_Mask;

} fm4000_modelTriggerConditionFfu;

typedef struct _fm4000_modelTriggerConditionGlort
{
    fm_uint16 DestGlort;

    fm_uint16 GlortMask;

} fm4000_modelTriggerConditionGlort;

typedef struct _fm4000_modelTriggerConditionParam
{
    fm_byte SA_ID;

    fm_byte DA_ID;

    fm_byte VID_ID;

    fm_byte SwitchPri;

    fm_byte FrameClassMask;

    fm_byte RoutedMask;

    fm_byte FtypeMask;

    fm_bool MatchDroppedFrames;

} fm4000_modelTriggerConditionParam;

typedef struct _fm4000_triggerConditionType
{
    fm_uint16 EtherType;

    fm_uint16 EtherTypeMask;

} fm4000_modelTriggerConditionType;

/***************************************************
 * FSCHED related types
 **************************************************/

typedef struct _fm4000_modelIpMulticastTable
{
    fm_bool   tail;

    fm_bool   novlan;

    fm_bool   skip;

    fm_uint16 vlan;

    fm_uint16 nxtptr;

} fm4000_modelIpMulticastTable;

typedef struct _fm4000_modelLoopbackSuppress
{
    fm_uint16   glort;

    fm_uint16   mask;

} fm4000_modelLoopbackSuppress;

typedef struct _fm4000_modelMirrorGlorts
{
    fm_uint16 logGlort;

    fm_uint16 txMirrorGlort;

} fm4000_modelMirrorGlorts;

typedef enum
{
    FM4000_MODEL_MIRTYPE_FN = 0,

    FM4000_MODEL_MIRTYPE_TX,

    FM4000_MODEL_MIRTYPE_RX,

    FM4000_MODEL_MIRTYPE_CPU

} fm4000_modelMirTyp;

typedef struct _fm4000_modelMsbCfg
{
    fm_bool attachedCpu;

} fm4000_modelMsbCfg;

typedef struct _fm4000_modelTxMirror
{
    fm_byte   dstport;

    fm_byte   srcport;

} fm4000_modelTxMirror;

/***************************************************
 * The following state structure holds the
 * pipeline state while a packet is being processed.
 * This state is completely reset at the start of
 * processing for a packet.  Any persistent state
 * should be defined in fm4000_model data structure.
 **************************************************/

#include <platforms/common/model/fm4000/fm4000_model_state.h>

/**************************************************
 * Overall switch state structure. Contains
 * all registers used by the model and
 * all persistent internal states.
 **************************************************/

typedef struct _fm4000_model
{
    /* The switch that this white model state maps to */
    fm_int                          sw;

    /* Lock to provide exclusive access to the model state & registers */
    fm_lock                         modelLock;

    /* Map from register address to value */
    fm_tree                         registers;

    /* Holds the state of a packet during the pipline processing */
    fm4000_modelState               packetState;

    /* Boolean indicating if the model is allowed to change the register cache.
     */
    fm_bool                         allowStateChange;

    /* The cached register state needed by the RX_EPL, PARSER, MODIFY and TX_EPL
     * stages. */
    fm4000_modelMacCfg1             MAC_CFG_1[FM4000_MAX_PORT + 1];
    fm4000_modelMacCfg2             MAC_CFG_2[FM4000_MAX_PORT + 1];
    fm4000_modelMacVlanEType        MAC_VLAN_ETYPE[FM4000_MAX_PORT + 1];
    fm4000_modelParseCfg            PARSE_CFG[FM4000_MAX_PORT + 1];
    fm4000_modelTxTrunc             TX_TRUNC[FM4000_MAX_PORT + 1];

    /* The cached register state needed by the ASSOC. */
    fm_byte                         DSCP_PRI_MAP[FM4000_DSCP_PRI_MAP_ENTRIES];
    fm4000_modelPortCfg1            PORT_CFG_1[FM4000_MAX_PORT + 1];
    fm4000_modelPortCfgIsl          PORT_CFG_ISL[FM4000_MAX_PORT + 1];
    fm_byte                         RX_VPRI_MAP[FM4000_MAX_PORT + 1][16];
    fm_byte                         VPRI_PRI_MAP[FM4000_VPRI_PRI_MAP_ENTRIES];

    /* The cached register state needed by the HASH stage. */
    fm4000_modelL234HashCfg         L234_HASH_CFG;
    fm4000_modelL34HashCfg          L34_HASH_CFG;
    fm4000_modelL34FlowHashCfg1     L34_FLOW_HASH_CFG_1;
    fm_uint32                       L34_FLOW_HASH_CFG_2;

    /* The cached register state needed by the MAPPER, FFU and NEXT_HOP stages.
     */
    fm4000_modelSysCfg8             SYS_CFG_8;

    /* The cached register state needed by the MAPPER stage */
    fm4000_modelFfuMapIpCfg         FFU_MAP_IP_CFG[FM4000_FFU_MAP_IP_CFG_ENTRIES];
    fm_uint64                       FFU_MAP_IP_LO[FM4000_FFU_MAP_IP_LO_ENTRIES];
    fm_uint64                       FFU_MAP_IP_HI[FM4000_FFU_MAP_IP_HI_ENTRIES];
    fm4000_modelFfuMapLength        FFU_MAP_LENGTH[FM4000_FFU_MAP_LENGTH_ENTRIES];
    fm4000_modelFfuMapL4Dst         FFU_MAP_L4_DST[FM4000_FFU_MAP_L4_DST_ENTRIES];
    fm4000_modelFfuMapL4Src         FFU_MAP_L4_SRC[FM4000_FFU_MAP_L4_SRC_ENTRIES];
    fm4000_modelFfuMapMac           FFU_MAP_MAC[FM4000_FFU_MAP_MAC_ENTRIES];
    fm4000_modelFfuMapProt          FFU_MAP_PROT[FM4000_FFU_MAP_PROT_ENTRIES];
    fm4000_modelFfuMapSrc           FFU_MAP_SRC[FM4000_FFU_MAP_SRC_ENTRIES];
    fm4000_modelFfuMapType          FFU_MAP_TYPE[FM4000_FFU_MAP_TYPE_ENTRIES];
    fm4000_modelFfuMapVlan          FFU_MAP_VLAN[FM4000_FFU_MAP_VLAN_ENTRIES];

    /* The cached register state needed by the FFU stage */
    fm4000_modelFfuInitSlice        FFU_INIT_SLICE;
    fm4000_modelFfuMasterValid      FFU_MASTER_VALID;
    fm4000_modelFfuEgressActions    FFU_EGRESS_ACTIONS[FM4000_FFU_EGRESS_ACTIONS_ENTRIES];
    fm4000_modelFfuEgressChunkCfg   FFU_EGRESS_CHUNK_CFG[FM4000_FFU_EGRESS_CHUNK_CFG_ENTRIES];
    fm_uint32                       FFU_EGRESS_CHUNK_VALID[FM4000_FFU_EGRESS_CHUNK_VALID_ENTRIES];
    fm_uint32                       FFU_SLICE_CASCADE_ACTION[FM4000_FFU_SLICE_CASCADE_ACTION_ENTRIES];
    fm_uint32                       FFU_SLICE_CASE[FM4000_FFU_SLICE_CASE_ENTRIES];
    fm4000_modelFfuSliceCaseCfg     FFU_SLICE_CASE_CFG[FM4000_FFU_SLICE_CASE_CFG_ENTRIES_1][FM4000_FFU_SLICE_CASE_CFG_ENTRIES_0];
    fm4000_modelFfuSliceSram        FFU_SLICE_SRAM[FM4000_FFU_SLICE_SRAM_ENTRIES_1][FM4000_FFU_SLICE_SRAM_ENTRIES_0];
    fm4000_modelFfuSliceTcam        FFU_SLICE_TCAM[FM4000_FFU_SLICE_TCAM_ENTRIES_1][FM4000_FFU_SLICE_TCAM_ENTRIES_0];
    fm_uint32                       FFU_SLICE_VALID[FM4000_FFU_SLICE_VALID_ENTRIES];

    /* The cached register state needed by the NEXT_HOP stage */
    fm_uint32                       ARP_REDIRECT_DIP[FM4000_MODEL_ARP_REDIRECT_DIP_WORDS];
    fm_uint32                       ARP_REDIRECT_SIP[FM4000_MODEL_ARP_REDIRECT_SIP_WORDS];
    fm4000_modelArpTable            ARP_TABLE[FM4000_ARP_TABLE_ENTRIES];
    fm_uint32                       ARP_USED[FM4000_ARP_USED_ENTRIES];

    /* The cached register state needed by the L2_LOOKUP stage. */
    fm4000_modelEgressFidTable      EGRESS_FID_TABLE[FM4000_EGRESS_FID_TABLE_ENTRIES];
    fm4000_modelEgressVidTable      EGRESS_VID_TABLE[FM4000_EGRESS_VID_TABLE_ENTRIES];
    fm4000_modelIngressFidTable     INGRESS_FID_TABLE[FM4000_INGRESS_FID_TABLE_ENTRIES];
    fm4000_modelIngressVidTable     INGRESS_VID_TABLE[FM4000_INGRESS_VID_TABLE_ENTRIES];
    fm4000_modelMaTable             MA_TABLE[FM4000_MA_TABLE_ENTRIES];
    fm4000_modelMaTableCfg1         MA_TABLE_CFG_1;
    fm_uint16                       MTU_TABLE[FM4000_MTU_TABLE_ENTRIES];
    fm_uint32                       PORT_CFG_2[FM4000_MAX_PORT + 1];
    fm4000_modelSysCfg1             SYS_CFG_1;

    /* The cached register state needed by the GLORT stage. */
    fm4000_modelGlortCam            GLORT_CAM[FM4000_GLORT_CAM_ENTRIES];
    fm4000_modelGlortDestTable      GLORT_DEST_TABLE[FM4000_GLORT_DEST_TABLE_ENTRIES];
    fm4000_modelGlortRam            GLORT_RAM[FM4000_GLORT_RAM_ENTRIES];

    /* The cached register state needed by the GEN_MASK1 and GEN_MASK2 stages.
     */
    fm4000_modelCanonicalGlortCam   CANONICAL_GLORT_CAM[FM4000_CANONICAL_GLORT_CAM_ENTRIES];
    fm4000_modelLoopbackSuppress    FH_LOOPBACK_SUPPRESS[FM4000_MAX_PORT + 1];
    fm_uint32                       INTERNAL_PORT_MASK;
    fm4000_modelLagCfg              LAG_CFG[FM4000_MAX_PORT + 1];
    fm4000_modelMaTableCfg2         MA_TABLE_CFG_2;
    fm4000_modelPortCfg3            PORT_CFG_3[FM4000_MAX_PORT + 1];
    fm4000_modelRxMirrorCfg         RX_MIRROR_CFG;
    fm4000_modelSysCfgRouter        SYS_CFG_ROUTER;
    fm4000_modelTxMirror            TX_MIRROR_FH;
    fm_macaddr                      cpuMacAddr;

    /* The cached register state needed by the TRIGGERS stage. */
    fm4000_modelTriggerActionCfg1     TRIGGER_ACTION_CFG_1[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerActionCfg2     TRIGGER_ACTION_CFG_2[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerActionDmask    TRIGGER_ACTION_DMASK[FM4000_MODEL_TRIGGERS_COUNT];
    fm_uint32                         TRIGGER_ACTION_DROP[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerActionGlort    TRIGGER_ACTION_GLORT[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerActionMirror   TRIGGER_ACTION_MIRROR[FM4000_MODEL_TRIGGERS_COUNT];
    fm_uint64                         TRIGGER_CONDITION_AMASK[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerConditionCfg   TRIGGER_CONDITION_CFG[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerConditionFfu   TRIGGER_CONDITION_FFU[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerConditionGlort TRIGGER_CONDITION_GLORT[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerConditionParam TRIGGER_CONDITION_PARAM[FM4000_MODEL_TRIGGERS_COUNT];
    fm_uint32                         TRIGGER_CONDITION_RX[FM4000_MODEL_TRIGGERS_COUNT];
    fm_uint32                         TRIGGER_CONDITION_TX[FM4000_MODEL_TRIGGERS_COUNT];
    fm4000_modelTriggerConditionType  TRIGGER_CONDITION_TYPE[FM4000_MODEL_TRIGGERS_COUNT];
    fm_uint64                         TRIGGER_IM;
    fm_uint64                         TRIGGER_IP;

    /* The cached register state needed by the CM stage. */
    fm_byte                         CM_SMP_MEMBERSHIP[FM4000_MAX_TRAFFIC_CLASS];
    fm_byte                         SWITCH_PRI_TO_CLASS[FM4000_MAX_SWITCH_PRIORITIES];

    /* The cached register state needed by the LEARNING stage. */
    fm4000_modelMaTableCfg3         MA_TABLE_CFG_3;
    fm4000_modelMaTcnPtr            MA_TCN_PTR;

    /* The cached register state needed by the STATS_RX and STATS_TX stages. */
    fm4000_modelStatsCfg            STATS_CFG[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxUcstPktsNonIP[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxMcstPktsNonIP[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxBcstPktsNonIP[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxUcstPktsIPv4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxMcstPktsIPv4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxBcstPktsIPv4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxUcstPktsIPv6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxMcstPktsIPv6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxBcstPktsIPv6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxPausePkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxCBPausePkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxSymbolErrors[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxFCSErrors[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxFrameSizeErrors[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxMinto63[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx64Pkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx65to127[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx128to255[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx256to511[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx512to1023[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx1024to1522[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx1523to2047[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx2048to4095[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx4096to8191[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx8192to10239[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Rx10240toMax[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxFragments[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxUndersized[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOversized[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsNonIP[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsIPv4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsIPv6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsError[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP0[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP1[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP2[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP3[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP5[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxP7[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP0[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP1[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP2[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP3[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP5[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxOctetsP7[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_FIDForwarded[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_FloodForwarded[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_SpeciallyHandled[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_ParseErrDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_ParityError[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Trapped[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_PauseDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_STPDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_SecurityViolations[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_VlanTagDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_VlanIngressDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_VlanEgressDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_GlortMissDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_FFUDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TriggerDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_PolicerDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TTLDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_CMPrivDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_SMP0Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_SMP1Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxHog0Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RxHog1Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxHog0Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxHog1Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RateLimit0Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_RateLimit1Drops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_BadSMPDrops[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TriggerRedirects[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Others[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxUcstPkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxBcstPkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxMcstPkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxTimeOutDrop[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxErrorDrop[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxLoopbackDrop[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxMinto63[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx64Pkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx65to127[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx128to255[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx256to511[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx512to1023[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx1024to1522[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx1523to2047[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx2048to4095[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx4096to8191[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx8192to10239[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_Tx10240toMax[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxErrors[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxOctets[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxErrorOctets[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort0[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort1[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort2[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort3[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort4[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort5[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort6[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort7[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort8[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort9[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort10[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort11[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort12[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort13[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort14[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort15[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort16[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort17[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort18[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort19[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort20[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort21[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort22[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort23[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_TxPort24[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_VlanUcstPkts[64];
    fm_uint64                       STAT_VlanXcstPkts[64];
    fm_uint64                       STAT_VlanUcstOctets[64];
    fm_uint64                       STAT_VlanXcstOctets[64];
    fm_uint64                       STAT_Trigger[FM4000_MODEL_TRIGGERS_COUNT];
    fm_uint64                       STAT_EgressACLPkts[FM4000_MAX_PORT + 1];
    fm_uint64                       STAT_EgressACLOctets[FM4000_MAX_PORT + 1];
    fm_uint32                       STATS_DROP_COUNT_RX[FM4000_MAX_PORT + 1];
    fm_uint32                       STATS_DROP_COUNT_TX[FM4000_MAX_PORT + 1];

    /* The cached register state needed by the FSCHED stage. */
    fm4000_modelIpMulticastTable    IP_MULTICAST_TABLE[FM4000_IP_MULTICAST_TABLE_ENTRIES];
    fm_uint32                       LOG_MASK;
    fm4000_modelLoopbackSuppress    LOOPBACK_SUPPRESS[FM4000_MAX_PORT + 1];
    fm4000_modelMirrorGlorts        MIRROR_GLORTS;
    fm4000_modelMsbCfg              MSB_CFG;
    fm4000_modelTxMirror            TX_MIRROR;

    /* The cached register state needed by the MODIFY stage. */
    fm_macaddr                      SRC_MAC[FM4000_MAX_PORT + 1];
    fm_macaddr                      SRC_MAC_VIRTUAL[FM4000_MAX_PORT + 1];
    fm_byte                         TX_VPRI_MAP[FM4000_MAX_PORT + 1][16];
    fm_bitArray                     VLANTAG_TABLE[FM4000_MAX_PORT + 1];

    /* The set of registers needed to recreate the EPL interrupt chain. */
    fm_uint32                       PCS_IP[FM4000_MAX_PORT + 1];
    fm_uint32                       EPL_INT_DETECT[FM4000_MAX_PORT + 1];
    fm_uint32                       GLOBAL_EPL_INT_DETECT;

    /* The set of registers needed to recreate the NEXT_HOP interrupt chain. */
    fm_uint32                       ARP_IM;
    fm_uint32                       ARP_IP;

    /* The set of registers needed to recreate the TCN interrupt chain. */
    fm_uint32                       MA_TCN_IM;
    fm_uint32                       MA_TCN_IP;

    /* The global interrupt detection register */
    fm_uint32                       INTERRUPT_DETECT;

} fm4000_model;




/*****************************************************************************
 * Public Function Prototypes
 *****************************************************************************/

/***************************************************
 * Register management functions.
 **************************************************/

fm_status fm4000ModelGetOffsetMult1(fm_uint32 addr,
                                    fm_uint32 baseAddr,
                                    fm_int    entries,
                                    fm_int    stride,
                                    fm_int *  index,
                                    fm_int *  word);
fm_status fm4000ModelGetOffsetMult2(fm_uint32 addr,
                                    fm_uint32 baseAddr,
                                    fm_int    entries1,
                                    fm_int    entries0,
                                    fm_int    stride1,
                                    fm_int    stride0,
                                    fm_int *  index1,
                                    fm_int *  index0,
                                    fm_int *  word);
void fm4000ModelGetRegisterAccess(fm_uint32  addr,
                                  fm_uint32 *rwMask,
                                  fm_uint32 *roMask,
                                  fm_uint32 *cwMask,
                                  fm_uint32 *cw1Mask,
                                  fm_uint32 *rvMask);
fm_uint32 fm4000ModelGetRegisterDefault(fm_uint32 addr);
fm_status fm4000ModelWriteCSRAbsolute(fm4000_model *model,
                                      fm_uint32     addr,
                                      fm_uint32     newValue);
fm_status fm4000ModelWriteCSRAbsolute64(fm4000_model *model,
                                        fm_uint32     addr,
                                        fm_uint64     value);
fm_status fm4000ModelWriteCSRMultAbsolute(fm4000_model *model,
                                          fm_uint32     addr,
                                          fm_int        n,
                                          fm_uint32 *   value);
fm_status fm4000ModelEplWriteCSR(fm4000_model *model,
                                 fm_uint32     addr,
                                 fm_uint32     value,
                                 fm_bool       init);
fm_status fm4000ModelFfuWriteCSR(fm4000_model *model,
                                 fm_uint32     addr,
                                 fm_uint32     val);
fm_status fm4000ModelNextHopWriteCSR(fm4000_model *model,
                                     fm_uint32     addr,
                                     fm_uint32     newValue);
fm_status fm4000ModelL2LookupWriteCSR(fm4000_model *model,
                                      fm_uint32     addr,
                                      fm_uint32     val);
fm_status fm4000ModelAssocWriteCSR(fm4000_model *model,
                                   fm_uint32     addr,
                                   fm_uint32     value);
fm_status fm4000ModelGlortWriteCSR(fm4000_model *model,
                                   fm_uint32     addr,
                                   fm_uint32     val);
fm_status fm4000ModelGenMaskWriteCSR(fm4000_model *model,
                                      fm_uint32     addr,
                                      fm_uint32     val);
fm_status fm4000ModelCmWriteCSR(fm4000_model *model,
                                fm_uint32     addr,
                                fm_uint32     val);
fm_status fm4000ModelStatsWriteCSR(fm4000_model *model,
                                   fm_uint32     addr,
                                   fm_uint32     val);
fm_status fm4000ModelFschedWriteCSR(fm4000_model *model,
                                    fm_uint32     addr,
                                    fm_uint32     value);

/***************************************************
 * FM4000 white model initialization functions.
 **************************************************/
fm_status fm4000ModelEplInitialize(fm4000_model *model);

/* Utility functions used in multiple stages */
fm_bool fm4000ModelIsMulticastIPAddress(fm_uint32 *ip,
                                        fm_bool isIPv4,
                                        fm_bool isIPv6);

/***************************************************
 * FM4000 white model stages in pipeline order.
 **************************************************/
fm_int fm4000ModelEplRx(fm4000_model *model);
void fm4000ModelParser(fm4000_model *model);
void fm4000ModelAssoc(fm4000_model *model);
void fm4000ModelMapper(fm4000_model *model);
fm_status fm4000ModelFfu(fm4000_model *model);
void fm4000ModelHash(fm4000_model *model);
fm_status fm4000ModelNextHop(fm4000_model *model);
fm_status fm4000ModelL2Lookup(fm4000_model *model);
void fm4000ModelGlort(fm4000_model *model);
void fm4000ModelGenMask1(fm4000_model *model);
fm_status fm4000ModelTriggers(fm4000_model *model);
void fm4000ModelCm(fm4000_model *model);
fm_status fm4000ModelGenMask2(fm4000_model *model);
fm_status fm4000ModelLearning(fm4000_model *model);
fm_status fm4000ModelStatsRx(fm4000_model *model);
fm_bool fm4000ModelFsched(fm4000_model *model);
fm_status fm4000ModelModify(fm4000_model *model,
                            fm_byte *     packet,
                            fm_uint32     maxPktSize);
fm_status fm4000ModelStatsTx(fm4000_model *model);
fm_int fm4000ModelEplTx(fm4000_model *model,
                        fm_byte *     packet,
                        fm_uint32     maxPktSize);

#endif /* __FM4000_MODEL_TYPES_H */

