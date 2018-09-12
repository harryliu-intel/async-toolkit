/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm10000_model_types.h
 * Creation Date:   June 6, 2012
 * Description:     Data structures, definitions and types for the FM10000 model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2013 Intel Corporation. All Rights Reserved.
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

#ifndef __FM10000_MODEL_TYPES_H
#define __FM10000_MODEL_TYPES_H

#include <platforms/common/model/fm10000/fm10000_model.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_MODEL_TAKE_LOCK(model)                                              \
    fmCaptureLock(&model->modelLock, FM_WAIT_FOREVER);

#define FM_MODEL_DROP_LOCK(model)                                              \
    fmReleaseLock(&model->modelLock);

/*****************************************************************************/
/** FM_MODEL_ADDR_MATCH_1
 * \ingroup model10k
 *
 * \desc            Checks if the specified hardware address matches the address
 *                  of the specified one-dimensional, single-word register
 *                  entry.
 *
 * \param[in]       addr is the hardware address.
 *
 * \param[in]       csr is the name of the register.
 *
 * \param[in]       index is the register index.
 *
 * \return          TRUE if the hardware address matches the address of the
 *                  specified one-dimensional, single-word register entry.
 * \return          FALSE otherwise.
 *
 *****************************************************************************/
#define FM_MODEL_ADDR_MATCH_1(addr, csr, index)                                \
    ( (addr) == (fm_uint32) csr((index)) )

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_1
 * \ingroup model10k
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
    fm10000ModelGetOffsetMult1(addr,                                           \
                               csr(0),                                         \
                               csr ## _ENTRIES,                                \
                               csr(1) - csr(0),                                \
                               index,                                          \
                               NULL)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_2
 * \ingroup model10k
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
    fm10000ModelGetOffsetMult2(addr,                                           \
                               csr(0, 0),                                      \
                               csr ## _ENTRIES_1,                              \
                               csr ## _ENTRIES_0,                              \
                               csr(1, 0) - csr(0, 0),                          \
                               csr(0, 1) - csr(0, 0),                          \
                               index1,                                         \
                               index0,                                         \
                               NULL)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_MULT_1
 * \ingroup model10k
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
    fm10000ModelGetOffsetMult1(addr,                                           \
                               csr(0, 0),                                      \
                               csr ## _ENTRIES,                                \
                               csr(1, 0) - csr(0, 0),                          \
                               index,                                          \
                               word)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_MULT_2
 * \ingroup model10k
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
    fm10000ModelGetOffsetMult2(addr,                                           \
                               csr(0, 0, 0),                                   \
                               csr ## _ENTRIES_1,                              \
                               csr ## _ENTRIES_0,                              \
                               csr(1, 0, 0) - csr(0, 0, 0),                    \
                               csr(0, 1, 0) - csr(0, 0, 0),                    \
                               index1,                                         \
                               index0,                                         \
                               word)

/*****************************************************************************/
/** FM_MODEL_GET_REG_PTR
 * \ingroup model10k
 *
 * \desc            Retrieves a pointer into the model register address space
 *                  for the specified hardware address.
 *                                                                      \lb\lb
 *                  This macro is to be used in conjunction with the
 *                  ''FM_GET_BIT'', ''FM_GET_FIELD'', etc. macros.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the hardware address.
 *
 * \return          A pointer into the model register address space for the
 *                  specified hardware address.
 *
 *****************************************************************************/
#define FM_MODEL_GET_REG_PTR(model, addr)                                      \
    &((model)->registers[(addr)])

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


#define FM10000_MODEL_MAX_HW_ADDR               0xffffff
#define FM10000_MODEL_TRIGGERS_COUNT            64
#define FM10000_MODEL_ARP_REDIRECT_DIP_WORDS    4
#define FM10000_MODEL_ARP_REDIRECT_SIP_WORDS    4
#define FM10000_MODEL_AMASK_WIDTH               38
#define FM10000_MODEL_AMASK_TRIGGERS_WIDTH      45
#define FM10000_MODEL_LOG_MASK_WIDTH            6

#define FM10000_MODEL_DEFAULT_DMASK             FM_LITERAL_U64(0xFFFFFFFFFFFF)


#define FM10000_MODEL_DMAC_IEEE_PREFIX          FM_LITERAL_U64(0x0180c2000000)
#define FM10000_MODEL_SPECIAL_DMASK             FM_LITERAL_U64(0xFFFFFFFFFF00)

#define FM10000_MODEL_DMAC_BPDU                                                \
    ( FM10000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x00) )
#define FM10000_MODEL_DMAC_MAC_CTRL                                            \
    ( FM10000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x01) )
#define FM10000_MODEL_DMAC_GMRP                                                \
    ( FM10000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x20) )
#define FM10000_MODEL_DMAC_GVRP                                                \
    ( FM10000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x21) )
#define FM10000_MODEL_DMAC_LACP                                                \
    ( FM10000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x02) )
#define FM10000_MODEL_DMAC_PORT_AUTH                                           \
    ( FM10000_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x03) )

/* Bit position for RX_TAG */
#define FM10000_MODEL_RXTAG_FTAG                0
#define FM10000_MODEL_RXTAG_VLAN1               1
#define FM10000_MODEL_RXTAG_VLAN2               2
#define FM10000_MODEL_RXTAG_VLAN2FIRST          3
#define FM10000_MODEL_RXTAG_IPV4                4
#define FM10000_MODEL_RXTAG_IPV6                5
#define FM10000_MODEL_RXTAG_MPLS                6
#define FM10000_MODEL_RXTAG_CUSTOM              7

/* Bit position for IPMISC */
#define FM10000_MODEL_IPMISC_TRAPIPOPTION       0
#define FM10000_MODEL_IPMISC_DONOTFRAG          1
#define FM10000_MODEL_IPMISC_HEADFRAG           2
#define FM10000_MODEL_IPMISC_MOREFRAG           3
#define FM10000_MODEL_IPMISC_ROUTABLE           4

#define FM10000_MODEL_PAUSE_TYPE_NOPAUSE        0
#define FM10000_MODEL_PAUSE_TYPE_STDPAUSE       1
#define FM10000_MODEL_PAUSE_TYPE_CBPAUSE        2

/* Bit number for fields of FFU_SLICE_SRAM.RouteData */
#define FM10000_MODEL_FFU_ROUTE_b_ROUTE_GLORT   21
/* Bit numbers when RouteType==GLORT */
#define FM10000_MODEL_FFU_ROUTE_l_DGLORT        0
#define FM10000_MODEL_FFU_ROUTE_h_DGLORT        15
#define FM10000_MODEL_FFU_ROUTE_b_FLOODSET      20
/* Bit numbers when RouteType==ARP */
#define FM10000_MODEL_FFU_ROUTE_l_ARP_INDEX     0
#define FM10000_MODEL_FFU_ROUTE_h_ARP_INDEX     15
#define FM10000_MODEL_FFU_ROUTE_l_ARP_COUNT     16
#define FM10000_MODEL_FFU_ROUTE_h_ARP_COUNT     19
#define FM10000_MODEL_FFU_ROUTE_b_ARP_TYPE      20

/* Bit number for FFU Flags */
#define FM10000_MODEL_FFU_FLAGS_b_DROP          0
#define FM10000_MODEL_FFU_FLAGS_b_TRAP          1
#define FM10000_MODEL_FFU_FLAGS_b_LOG           2
#define FM10000_MODEL_FFU_FLAGS_b_NO_ROUTE      3
#define FM10000_MODEL_FFU_FLAGS_b_RX_MIRROR     4
#define FM10000_MODEL_FFU_FLAGS_b_CAPTURE_TIME  5

#define FM10000_MODEL_FFU_EGRESS_ACTIONS_CHUNK_SIZE 32

#define FM10000_MODEL_LOG_TYPE_TRIG_LOG_ACTION  (1 << 0)
#define FM10000_MODEL_LOG_TYPE_FFU              (1 << 1)
#define FM10000_MODEL_LOG_TYPE_RESERVED_MAC     (1 << 2)
#define FM10000_MODEL_LOG_TYPE_ARP_REDIRECT     (1 << 3)
#define FM10000_MODEL_LOG_TYPE_ICMP             (1 << 4)
#define FM10000_MODEL_LOG_TYPE_TTL_IP_MC        (1 << 5)
#define FM10000_MODEL_LOG_TYPE_IP_UCST_L2_MCST  (1 << 7) /* EAC TBR */

/* Trap codes which forms lower 8-bit of CPU-glort. */
/* See RRC Bug 22835.  Changed twice to match RTL change 271047. */
#define FM10000_MODEL_CPU_CODE_FFU              0x80;
#define FM10000_MODEL_CPU_CODE_RSVD_MAC         0x83;
#define FM10000_MODEL_CPU_CODE_IGMP             0x86;
#define FM10000_MODEL_CPU_CODE_ICMP             0x90;
#define FM10000_MODEL_CPU_CODE_IP_OPTION        0x91;
#define FM10000_MODEL_CPU_CODE_CPU_ADDRESS      0x92;
#define FM10000_MODEL_CPU_CODE_MTU              0x94;
#define FM10000_MODEL_CPU_CODE_TTL              0x96;
#define FM10000_MODEL_CPU_CODE_MAX              0xFF;

/* Frame types */
#define FM10000_MODEL_ETYPE_IPv4                0x0800
#define FM10000_MODEL_ETYPE_IPv6                0x86DD
#define FM10000_MODEL_ETYPE_MAC_CONTROL         0x8808
#define FM10000_MODEL_IPV6_OPTION_HOP_BY_HOP    0
#define FM10000_MODEL_IPV6_OPTION_ROUTING       43
#define FM10000_MODEL_IPV6_OPTION_FRAG          44
#define FM10000_MODEL_IPV6_OPTION_DEST          60
#define FM10000_MODEL_IPV6_OPTION_AUTH          51
#define FM10000_MODEL_PROT_TCP                  6
#define FM10000_MODEL_PROT_UDP                  17
#define FM10000_MODEL_PROT_ICMPv4               1
#define FM10000_MODEL_PROT_ICMPv6               58
#define FM10000_MODEL_PROT_IGMP                 2

/* Constants for FFU scenario */
#define FM10000_MODEL_FFU_SCENARIO_NOT_IP                   0
#define FM10000_MODEL_FFU_SCENARIO_IP_V4                    1
#define FM10000_MODEL_FFU_SCENARIO_IP_V6                    2
#define FM10000_MODEL_FFU_SCENARIO_IP_V4_IN_V6              3
#define FM10000_MODEL_FFU_SCENARIO_UNROUTABLE_FIRST         0
#define FM10000_MODEL_FFU_SCENARIO_UNROUTABLE_INTER         1
#define FM10000_MODEL_FFU_SCENARIO_RESERVED                 2
#define FM10000_MODEL_FFU_SCENARIO_SPECIAL                  3
#define FM10000_MODEL_FFU_SCENARIO_ROUTABLE_IP_UCAST_FIRST  4
#define FM10000_MODEL_FFU_SCENARIO_ROUTABLE_IP_UCAST_INTER  5
#define FM10000_MODEL_FFU_SCENARIO_ROUTABLE_IP_MCAST_FIRST  6
#define FM10000_MODEL_FFU_SCENARIO_ROUTABLE_IP_MCAST_INTER  7

/* FFU mux selects */
#define FM10000_FFU_SELECT_MAP_DIP_MAP_SIP 		0
#define FM10000_FFU_SELECT_MAP_DMAC_MAP_SMAC 	1
#define FM10000_FFU_SELECT_MAP_PROT_MAP_LENGTH 	2
#define FM10000_FFU_SELECT_MAP_SRC_MAP_TYPE 	3
#define FM10000_FFU_SELECT_USER 		        4
#define FM10000_FFU_SELECT_FTYPE_SWPRI 		    5
#define FM10000_FFU_SELECT_IPMISC 		        6
#define FM10000_FFU_SELECT_TOS 		            7
#define FM10000_FFU_SELECT_PROT 		        8
#define FM10000_FFU_SELECT_TTL 		            9
#define FM10000_FFU_SELECT_SRC_PORT 		    10
#define FM10000_FFU_SELECT_VPRI_VID_11_8 		11
#define FM10000_FFU_SELECT_VID_7_0 	            12
#define FM10000_FFU_SELECT_RXTAG 	            13
#define FM10000_FFU_SELECT_L2_DMAC_15_0 		14
#define FM10000_FFU_SELECT_L2_DMAC_31_16 		15
#define FM10000_FFU_SELECT_L2_DMAC_47_32 		16
#define FM10000_FFU_SELECT_L2_SMAC_15_0 		17
#define FM10000_FFU_SELECT_L2_SMAC_31_16 		18
#define FM10000_FFU_SELECT_L2_SMAC_47_32 		19
#define FM10000_FFU_SELECT_DGLORT 	            20
#define FM10000_FFU_SELECT_SGLORT 	            21
#define FM10000_FFU_SELECT_VPRI_VID 	        22
#define FM10000_FFU_SELECT_VPRI2_VID2 	        23
#define FM10000_FFU_SELECT_L2_TYPE 	            24
#define FM10000_FFU_SELECT_L4_DST 	            25
#define FM10000_FFU_SELECT_L4_SRC 	            26
#define FM10000_FFU_SELECT_MAP_L4_DST 	        27
#define FM10000_FFU_SELECT_MAP_L4_SRC 	        28
#define FM10000_FFU_SELECT_L4A 	                29
#define FM10000_FFU_SELECT_L4B 	                30
#define FM10000_FFU_SELECT_L4C 	                31
#define FM10000_FFU_SELECT_L4D 	                32
#define FM10000_FFU_SELECT_MAP_VPRI1_VID1 		33
#define FM10000_FFU_SELECT_L3_DIP_31_0 		    34
#define FM10000_FFU_SELECT_L3_DIP_63_32 		35
#define FM10000_FFU_SELECT_L3_DIP_95_64 		36
#define FM10000_FFU_SELECT_L3_DIP_127_96 		37
#define FM10000_FFU_SELECT_L3_SIP_31_0 		    38
#define FM10000_FFU_SELECT_L3_SIP_63_32 		39
#define FM10000_FFU_SELECT_L3_SIP_95_64 		40
#define FM10000_FFU_SELECT_L3_SIP_127_96 		41

/* The number of banks in the MAC Address Table. */
#define FM10000_MAC_ADDR_BANK_COUNT             4

/* The size of each bank in the MAC Address Table. */
#define FM10000_MAC_ADDR_BANK_SIZE              4096

#define FM10000_MODEL_L2L_HASH_TABLE_SIZE       256

#define MAX(x, y)                   ( ( (x) > (y) ) ? (x) : (y) )
#define MIN(x, y)                   ( ( (x) < (y) ) ? (x) : (y) )

/* Miscellaneous defines */
#define FM10000_CM_APPLY_TC_TO_SMP_ENTRIES           8
#define FM10000_CM_APPLY_SWITCH_PRI_TO_TC_ENTRIES   16

#define FM10000_MIRROR_PORT_NULL 63

/* Action Codes (match RTL, bug 24393) */
#define FM10000_MODEL_ACTION_NORMAL         0   /* forwarded normally */
#define FM10000_MODEL_ACTION_FLOOD          1   /* flooded due to unknown destination */
#define FM10000_MODEL_ACTION_SPECIAL        2   /* forwarded with fixed destination mask */
#define FM10000_MODEL_ACTION_DROP_PARSE     3   /* drop due to parse error */
#define FM10000_MODEL_ACTION_DROP_PARITY    4   /* dropped due to parity error */
#define FM10000_MODEL_ACTION_TRAP           5   /* trap spec. mcast add., MAC CTL & MAC REMAP CTL */
#define FM10000_MODEL_ACTION_DROP_CONTROL   6   /* dropped pause frame */ 
#define FM10000_MODEL_ACTION_DROP_STP       7   /* dropped due to spanning tree*/
#define FM10000_MODEL_ACTION_DROP_SV        8   /* dropped due to security violation */
#define FM10000_MODEL_ACTION_DROP_TAG       9   /* dropped due to tagged/untagged config */
#define FM10000_MODEL_ACTION_DROP_IV       10   /* dropped due to vlan ingress violation */
#define FM10000_MODEL_ACTION_DROP_EV       11   /* dropped due to vlan egress violation */
#define FM10000_MODEL_ACTION_DROP_CAM      12
#define FM10000_MODEL_ACTION_DROP_FFU      13   /* dropped due to FFU flag */
#define FM10000_MODEL_ACTION_DROP_TRIG     14   /* dropped due to a trigger action */
#define FM10000_MODEL_ACTION_DROP_POLICER  16   /* dropped due the policer_drop flag from pre. */
#define FM10000_MODEL_ACTION_DROP_TTL      17  
#define FM10000_MODEL_ACTION_REDIRECT_TRIG 26   /* redirected due to trigger match */
#define FM10000_MODEL_ACTION_DROP_DLF      27   /* drop due to flood control of DLF frames */
#define FM10000_MODEL_ACTION_GLORT_FORWARDED     28  /* Glort forwarded. */
#define FM10000_MODEL_ACTION_DROP_LOOPBACK       29  /* Drop due to port or VLAN reflection disabled. */
#define FM10000_MODEL_ACTION_BANK5_OTHER_DROPS   30  /* Bank 5 Other Drops */

/* uniquely assigned */
#define FM10000_MODEL_ACTION_DROP_BCST           32 /* dropped broadcast frames (SYS_CFG_1[15]) */
#define FM10000_MODEL_ACTION_DROP_RATE           23  /* drop due to rate limiter (CM) */
#define FM10000_MODEL_ACTION_DROP_PRIV           24  /* drop due to privileged watermark (CM) */
#define FM10000_MODEL_ACTION_DROP_SHARED         25  /*  drop due to SMP shared watermark (CM) */
#define FM10000_MODEL_ACTION_DROP_RX_HOG         21  /* drop due to rx hog watermark (CM)*/
#define FM10000_MODEL_ACTION_DROP_TX_HOG         23  /*  drop due to tx hog watermark (CM) */
#define FM10000_MODEL_ACTION_DROP_BAD_SMP        19  /*  drop due to illegal SMP membership (CM) */


typedef enum
{
    FM10000_MODEL_PACKET_TOO_SHORT = 0x1,

    FM10000_MODEL_PACKET_TOO_LONG = 0x2,

    FM10000_MODEL_PACKET_BAD_FCS = 0x4

} fm10000_modelRxFlags;

typedef enum
{
    FM10000_MODEL_ISL_NONE = 0,

    FM10000_MODEL_ISL_F32,

    FM10000_MODEL_ISL_F64,

    FM10000_MODEL_ISL_F96,

    FM10000_MODEL_ISL_X32,

    FM10000_MODEL_ISL_X64,

    FM10000_MODEL_ISL_X96

} fm10000_modelIslTag;

typedef enum
{
    FM10000_MODEL_FTYPE_NORMAL = 0,

    FM10000_MODEL_FTYPE_ROUTED,

    FM10000_MODEL_FTYPE_SPECIAL,

    FM10000_MODEL_FTYPE_MGMT

} fm10000_modelFType;

typedef enum
{
    FM10000_MODEL_MTYPE_REQUEST = 0,

    FM10000_MODEL_MTYPE_RESPONSE,

    FM10000_MODEL_MTYPE_RESERVED1,

    FM10000_MODEL_MTYPE_RESERVED2

} fm10000_modelMType;

typedef enum
{
    FM10000_MODEL_VTYPE_NONE = 0,

    FM10000_MODEL_VTYPE_C_VLAN,

    FM10000_MODEL_VTYPE_S_VLAN_A,

    FM10000_MODEL_VTYPE_S_VLAN_B

} fm10000_modelVType;

typedef enum
{
    FM10000_MODEL_ARP_TYPE_MAC = 0,

    FM10000_MODEL_ARP_TYPE_GLORT,

    FM10000_MODEL_ARP_TYPE_IPv4InIPv6

} fm10000_modelArpEntryType;

typedef enum
{
    FM10000_MODEL_STP_STATE_DISABLE = 0,

    FM10000_MODEL_STP_STATE_LISTENING,

    FM10000_MODEL_STP_STATE_LEARNING,

    FM10000_MODEL_STP_STATE_FORWARD

} fm10000_modelSTPState;

typedef enum
{
    FM10000_MODEL_TCN_ENTRY_NEW_MAC = 0,

    FM10000_MODEL_TCN_ENTRY_MAC_MOVED

} fm10000_modelTcnType;

typedef enum
{
    FM10000_MODEL_FFU_SLICE_SRAM_COMMAND_ROUTE_ARP = 0,

    FM10000_MODEL_FFU_SLICE_SRAM_COMMAND_ROUTE_GLORT,

    FM10000_MODEL_FFU_SLICE_SRAM_COMMAND_BITSET,

    FM10000_MODEL_FFU_SLICE_SRAM_COMMAND_FIELDSET

} fm10000_modelFfuSliceSramCommand;

typedef enum
{
    FM10000_MODEL_FFU_SLICE_BITSET_SUB_COMMAND_CHANGE_FLAG = 0,

    FM10000_MODEL_FFU_SLICE_BITSET_SUB_COMMAND_CHANGE_TRIG,

    FM10000_MODEL_FFU_SLICE_BITSET_SUB_COMMAND_CHANGE_USR

   //FM10000_MODEL_FFU_SLICE_BITSET_SUB_COMMAND_CHANGE_ACTION_A,

   //FM10000_MODEL_FFU_SLICE_BITSET_SUB_COMMAND_CHANGE_ACTION_B

} fm10000_modelFfuSliceBitSetSubCommand;

typedef enum
{
    FM10000_MODEL_FFU_SLICE_FIELDSET_SUB_COMMAND_SET_VLAN = 0,

    FM10000_MODEL_FFU_SLICE_FIELDSET_SUB_COMMAND_SET_DSCP

} fm10000_modelFfuSliceFieldSetSubCommand;

typedef enum
{
    FM10000_MODEL_FTAG = 0x01,

    FM10000_MODEL_VLAN1 = 0x02,

    FM10000_MODEL_VLAN2 = 0x04,

    FM10000_MODEL_VLAN2_BEFORE_VLAN1 = 0x08,

    FM10000_MODEL_IPV4 = 0x10,

    FM10000_MODEL_IPV6 = 0x20,

    FM10000_MODEL_MPLS = 0x40,

    FM10000_MODEL_CUSTOM = 0x80

} fm10000_modelRxTag;

typedef enum
{
    FM10000_MODEL_NORMAL_TAGGING = 0,

    FM10000_MODEL_INSERT,

    FM10000_MODEL_DELETE,

    FM10000_MODEL_UPDATE_ADD

} fm10000_modelTxTag;

/**************************************************
 * Register Structures. These structures
 * mimic the equivalent registers as defined
 * in the FM10000 datasheet.
 **************************************************/

typedef struct _fm10000_modelIngressVidTable
{
    fm_uint64 membership;

    fm_uint16 FID;

    fm_byte   MST_Index;

    fm_byte   CounterIndex;

    fm_bool   reflect;

    fm_bool   TrapIGMP;

} fm10000_modelIngressVidTable;

typedef struct _fm10000_modelEgressVidTable
{
    fm_uint64 membership;

    fm_uint16 FID;

    fm_byte   MST_Index;

    fm_byte   MTU_Index;

    fm_byte   TrigID;

} fm10000_modelEgressVidTable;

typedef struct _fm10000_modelMaTable
{
    fm_macaddr               MACAddress;

    fm_uint16                FID;

    fm_bool                  valid;

    fm_bool                  secure;

    fm_uint16                glort;

    fm_byte                  trigId;

} fm10000_modelMaTable;

typedef struct _fm10000_modelMaTcnFifo
{
    fm_macaddr              MACAddress;

    fm_uint16               VID;

    fm_uint16               srcGlort;

    fm_uint16               Index;

    fm_byte                 Port;

    fm10000_modelTcnType    EntryType;

} fm10000_modelMaTcnFifo;

typedef struct _fm10000_modelMaTcnDequeue
{
  fm10000_modelMaTcnFifo fifoEntry;

  fm_bool                Valid;

} fm10000_modelMaTcnDequeue;

typedef struct _fm10000_modelMaTableCfg2
{
    fm_uint16 FloodUnicastGlort;
    
    fm_uint16 FloodMulticastGlort;

    fm_uint16 BroadcastGlort;

} fm10000_modelMaTableCfg2;

typedef struct _fm10000_modelIngressMstTable
{
    fm_uint64 STPState;

} fm10000_modelIngressMstTable;

typedef struct _fm10000_modelEgressMstTable
{
    fm_uint64 Forwarding;

} fm10000_modelEgressMstTable;

typedef struct _fm10000_modelMaTableCfg1
{
    fm_byte HashRotation0;

    fm_byte HashRotation1;

    fm_bool HashMode;

} fm10000_modelMaTableCfg1;

typedef enum
{
    FM1000_MODEL_IEEE_RSVD_MAC_ACTION_NORMAL = 0,

    FM1000_MODEL_IEEE_RSVD_MAC_ACTION_TRAP,

    FM1000_MODEL_IEEE_RSVD_MAC_ACTION_DROP,

    FM1000_MODEL_IEEE_RSVD_MAC_ACTION_LOG

} fm10000_modelRsvdMacCtrlActionSpecial;

typedef struct _fm10000_modelMcastEpoch
{
    fm_byte Current;

} fm10000_modelMcastEpoch;

typedef struct _fm10000_modelSysCfg1
{
    fm_bool dropPause;

    fm_bool trapMTUViolations;

    fm_bool enableTrapPlusLog;

    fm_bool dropInvalidSMAC;

    fm_bool dropMacCtrlEthertype;

} fm10000_modelSysCfg1;

typedef struct _fm10000_modelSysCfg8
{
    fm_bool enableFFU;

    fm_bool allowQTagPause;

} fm10000_modelSysCfg8;

typedef struct _fm10000_modelL234HashCfg
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

} fm10000_modelL234HashCfg;

typedef struct _fm10000_modelL34HashCfg
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

} fm10000_modelL34HashCfg;

typedef struct _fm10000_modelL34FlowHashCfg1
{
    fm_byte   DiffServMask;

    fm_uint16 UserMask;

} fm10000_modelL34FlowHashCfg1;

typedef struct _fm10000_modelRxMirrorCfg
{
    fm_int    MirrorProfileIdx;

} fm10000_modelRxMirrorCfg;

typedef struct _fm10000_modelLogMirrorProfile
{
    fm_byte FFU;

    fm_byte ReservedMAC;

    fm_byte ARPRedirect;

    fm_byte ICMP;

    fm_byte TTL;

} fm10000_modelLogMirrorProfile;

typedef struct _fm10000_modelArpTable
{
    fm_macaddr               DMAC; // used for DMAC entries

    fm_uint16                DGLORT; // used for GLORT entries

    fm_byte                  MTU_Index; // used for GLORT entries

    fm_bool                  markRouted; // used for GLORT entries

    fm_bool                  IPv6Entry; // used for GLORT entries

    fm_byte                  RouterIdGlort; // used for GLORT entries

    fm_uint16                EVID;

    fm_byte                  RouterId; // TODO - replace fm10000_modelArpEntryType?

} fm10000_modelArpTable;

typedef struct _fm10000_modelGlortCam
{
    fm_uint16 Key;

    fm_uint16 KeyInvert;

    fm_bool Valid;

} fm10000_modelGlortCam;

typedef struct _fm10000_modelGlortRam
{
    fm_byte   Strict;

    fm_uint16 DestIndex;

    fm_byte   RangeSubIndexA;

    fm_byte   RangeSubIndexB;

    fm_byte   DestCount;

    fm_bool   HashRotation;

} fm10000_modelGlortRam;

typedef struct _fm10000_modelGlortDestTable
{
    fm_uint64 DestMask;

    fm_uint16 IP_MulticastIndex;

} fm10000_modelGlortDestTable;

typedef struct _fm10000_modelLagCfg
{
    fm_byte LagSize;

    fm_byte Index;

    fm_bool HashRotation;

    fm_bool InLAG;

} fm10000_modelLagCfg;

typedef struct _fm10000_modelCanonicalGlortCam
{
    fm_uint16 LagGlort;

    fm_byte   MaskSize;

    fm_byte   PortFieldSize;

} fm10000_modelCanonicalGlortCam;

typedef enum
{
    FM10000_MODEL_FCLASS_UNICAST = 0,

    FM10000_MODEL_FCLASS_MULTICAST,

    FM10000_MODEL_FCLASS_BROADCAST

} fm10000_modelFclassType;

typedef struct _fm10000_modelRxStatsCfg
{
    fm_byte PerFrameAdjustment;

    fm_byte EnableMask;

    fm_bool PrioritySelect;

} fm10000_modelRxStatsCfg;

typedef enum 
{
    STAT_RxUcstPktsNonIP = 0,
    STAT_RxMcstPktsNonIP,
    STAT_RxBcstPktsNonIP,
    STAT_RxUcstPktsIPv4,
    STAT_RxMcstPktsIPv4,
    STAT_RxBcstPktsIPv4,
    STAT_RxUcstPktsIPv6,
    STAT_RxMcstPktsIPv6,
    STAT_RxBcstPktsIPv6,
    STAT_RxPausePkts,   // note: IEEE802_3 PAUSE frame
    STAT_RxCBPausePkts,
    STAT_RxFramingErrors,   // note: was SYMBOL_ERRORS,
    STAT_RxFCSErrors 
//  STAT_RxFrameSizeErrors,
} fm10000_modelRxStatsBk1;

typedef enum 
{
    STAT_RxMinto63 = 0,
    STAT_Rx64Pkts,
    STAT_Rx65to127,
    STAT_Rx128to255,
    STAT_Rx256to511,
    STAT_Rx512to1023,
    STAT_Rx1024to1522,
    STAT_Rx1523to2047,
    STAT_Rx2048to4095,
    STAT_Rx4096to8191,
    STAT_Rx8192to10239,
    STAT_Rx10240toMax
} fm10000_modelRxStatsBk2;

typedef enum
{
    STAT_RxP0 = 0,
    STAT_RxP1,
    STAT_RxP2,
    STAT_RxP3,
    STAT_RxP4,
    STAT_RxP5,
    STAT_RxP6,
    STAT_RxP7,
    STAT_RxP8,
    STAT_RxP9,
    STAT_RxP10,
    STAT_RxP11,
    STAT_RxP12,
    STAT_RxP13,
    STAT_RxP14,
    STAT_RxP15
} fm10000_modelRxStatsBk3;

typedef enum
{
    STAT_FIDForwarded = 0,
    STAT_FloodForwarded,
    STAT_SpeciallyHandled,
    STAT_ParseErrDrops,
    STAT_ECCError,       /* Note:  Was parity error. */
    STAT_Trapped,
    STAT_PauseDrops,
    STAT_STPDrops,
    STAT_SecurityViolations,
    STAT_VlanTagDrops,
    STAT_VlanIngressDrops,
    STAT_VlanEgressDrops,
    STAT_GlortMissDrops,
    STAT_FFUDrops,
    STAT_TriggerDrops
} fm10000_modelRxStatsBk4;

typedef enum
{
    STAT_PolicerDrops = 0,
    STAT_TTLDrops,
    STAT_CMPRIVDrops,
    STAT_SMP0Drops,
    STAT_SMP1Drops,
    STAT_RXHog0Drops,
    STAT_RXHog1Drops,
    STAT_TXHog0Drops,
    STAT_TXHog1Drops,
    STAT_BadSMPDrops,
    STAT_TriggerRedirects,
    STAT_FloodControlDrops,
    STAT_GlortForwarded,
    STAT_LoopbackSuppDrops,
    STAT_Others
} fm10000_modelRxStatsBk5;

typedef struct _fm10000_modelRxStatsBanks
{
    fm10000_modelRxStatsBk1     rxStatsBank1;
    fm10000_modelRxStatsBk2     rxStatsBank2;
    fm10000_modelRxStatsBk3     rxStatsBank3;
    fm10000_modelRxStatsBk4     rxStatsBank4;
    fm10000_modelRxStatsBk5     rxStatsBank5;
} fm10000_modelRxStatsBanks;


typedef struct _fm10000_modelFfuMapSrc
{
    fm_byte MAP_SRC;

    fm_bool Routable;

} fm10000_modelFfuMapSrc;

typedef struct _fm10000_modelFfuMapMac
{
    fm_uint64 MAC;

    fm_byte IgnoreLength;

    fm_bool validSMAC;

    fm_bool validDMAC;

    fm_byte MAP_MAC;

    fm_bool Router;

} fm10000_modelFfuMapMac;

typedef struct _fm10000_modelFfuMapVlan
{
    fm_uint16 MAP_VLAN;

    fm_bool Routable;

    fm_bool ParityError;

} fm10000_modelFfuMapVlan;

typedef struct _fm10000_modelFfuMapType
{
    fm_uint16 TYPE;

    fm_byte MAP_ETYPE;

} fm10000_modelFfuMapType;

typedef struct _fm10000_modelFfuMapLength
{
    fm_uint16 LENGTH;

    fm_byte MAP_LENGTH;

} fm10000_modelFfuMapLength;

typedef struct _fm10000_modelFfuMapIpCfg
{
    fm_byte IgnoreLength;

    fm_bool validSIP;

    fm_bool validDIP;

    fm_byte MAP_IP;

} fm10000_modelFfuMapIpCfg;

typedef struct _fm10000_modelFfuMapProt
{
    fm_byte PROT;

    fm_byte MAP_PROT;

} fm10000_modelFfuMapProt;

typedef struct _fm10000_modelFfuMapL4Src
{
    fm_uint16 L4SRC;

    fm_byte MAP_PROT;

    fm_bool VALID;

    fm_uint16 MAP_L4SRC;

} fm10000_modelFfuMapL4Src;

typedef struct _fm10000_modelFfuMapL4Dst
{
    fm_uint16 L4DST;

    fm_byte MAP_PROT;

    fm_bool VALID;

    fm_uint16 MAP_L4DST;

} fm10000_modelFfuMapL4Dst;

typedef struct _fm10000_modelFfuMasterValid
{
    fm_uint32 SliceValid;

    fm_uint32 ChunkValid;

} fm10000_modelFfuMasterValid;

typedef struct _fm10000_modelFfuEgressChunkCfg
{
    //fm_uint32 DstPortMask;

    fm_bool StartCascade;

} fm10000_modelFfuEgressChunkCfg;

typedef struct _fm10000_modelFfuEgressPortCfg
{
    fm_uint32 DropApply;

} fm10000_modelFfuEgressPortCfg;

typedef struct _fm10000_modelFfuEgressChunkActions
{
    fm_uint32 Drop;

   //fm_bool Log;

   //fm_bool Count;

} fm10000_modelFfuEgressChunkActions;

typedef struct _fm10000_modelFfuSliceTcam
{
    /* Key contains 40 bit value of KeyLow and KeyTop */
    fm_uint64 Key;

    /* KeyInvert contains 40 bit value of KeyInvertLow and KeyInvertTop */
    fm_uint64 KeyInvert;

    fm_bool Valid;

   //fm_bool Case;

} fm10000_modelFfuSliceTcam;

typedef struct _fm10000_modelFfuSliceSram
{
    //fm_bool ParityError;

    fm_byte Precedence;

    fm_byte CounterBank;

    fm_uint16 CounterIndex;

    fm10000_modelFfuSliceSramCommand Command;

    fm_uint32 Data;

   //fm10000_modelFfuSliceSramSubCommand SubCommand;

   //fm_uint32 RouteData;

   //fm_uint16 ShortData;

   //fm_byte ByteMask;

   //fm_byte ByteData;

   //fm_bool SetDSCP;

   //fm_bool SetVLAN;

   //fm_bool SetVPRI;

   //fm_bool SetPRI;

   //fm_byte PRI;

   //fm_uint16 VLAN;

   //fm_byte DSCP;

} fm10000_modelFfuSliceSram;

typedef struct _fm10000_modelFfuSliceCfg
{
    fm_byte Select0;

    fm_byte Select1;

    fm_byte Select2;

    fm_byte Select3;

    fm_byte SelectTop;

    fm_bool StartCompare;

    fm_bool StartAction;

    fm_bool ValidLow;

    fm_bool ValidHigh;

    fm_byte Case;

    fm_byte SetCaseLocation;


} fm10000_modelFfuSliceCfg;

/***************************************************
 * The following structures are helper structures
 * for modelling the FM10000 chip.
 **************************************************/

/* Structure used to store different fields of key used in FFU */
typedef struct _fm10000_modelFfuKey
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

} fm10000_modelFfuKey;

typedef struct _fm10000_modelPrecVal
{
    fm_byte   prec;

    fm_uint32 val;

} fm10000_modelPrecVal;

typedef struct _fm10000_modelIngressActions
{
    //fm_bool             PARITY_ERROR;

    fm10000_modelPrecVal ROUTE;

    fm10000_modelPrecVal FLAGS[8];

    fm10000_modelPrecVal SET_TRIG[8];

    fm10000_modelPrecVal SET_USR[8];

    //fm10000_modelPrecVal ACTION_A;

    //fm10000_modelPrecVal ACTION_B;

    fm10000_modelPrecVal SET_VLAN;

    //txtag is part of vlan action
    fm10000_modelPrecVal SET_TXTAG;

    fm10000_modelPrecVal SET_VPRI;

    fm10000_modelPrecVal SET_PRI;

    fm10000_modelPrecVal SET_DSCP;

    fm10000_modelPrecVal COUNT[4];

} fm10000_modelIngressActions;

typedef struct _fm10000_modelFfuControl
{
    fm_bool            sramParity;

    fm_byte            scenario;

    fm_bool            rawHit[FM10000_FFU_SLICE_SRAM_ENTRIES_0];

    /* To indicate whether cascading action hit */
    fm_bool            hit;

    fm_uint16          actionIndex;

    fm_int             sliceIndex;

} fm10000_modelFfuControl;

/* Slice result */
typedef struct _fm10000_modelLookupInfo
{
    fm_uint64 key;

    fm_uint64 keyInvert;

    fm_int    index;

} fm10000_modelLookupInfo;

typedef struct _fm10000_modelBitSet
{
   fm_byte ByteMask;

   fm_byte ByteData;

   fm10000_modelFfuSliceBitSetSubCommand SubCommand;

} fm10000_modelBitSet;

typedef struct _fm10000_modelSetVLAN
{
    fm_uint16 VLAN;

    fm_byte   PRI;

    fm_byte   TxTag;

    fm_bool   SetVPRI;

    fm_bool   SetPRI;

} fm10000_modelSetVLAN;

typedef struct _fm10000_modelSetDSCP
{
    fm_byte   DSCP;

    fm_byte   PRI;

    fm_bool   SetDSCP;

    fm_bool   SetVPRI;

    fm_bool   SetPRI;

} fm10000_modelSetDSCP;

/***************************************************
 * PARSER related types
 **************************************************/

typedef struct _fm10000_modelParserPortCfg1
{
    fm_bool   FTAG;

    fm_byte   Vlan1Tag;

    fm_byte   Vlan2Tag;

    fm_bool   Vlan2First;

    fm_uint16 defaultVID;

    fm_byte   defaultVPRI;

    fm_uint16 defaultVID2;

    fm_byte   defaultVPRI2;

    fm_bool   useDefaultVLAN;

} fm10000_modelParserPortCfg1;

typedef struct _fm10000_modelParserPortCfg2
{
    fm_byte   CustomTag1;

    fm_byte   CustomTag2;

    fm_bool   ParseMPLS;

    fm_byte   StoreMPLS;

    fm_bool   ParseL3;

    fm_bool   ParseL4;

    fm_bool   FlagIPv4Options;

    fm_bool   FlagIPv6HopByHop;

    fm_bool   FlagIPv6Routing;

    fm_bool   FlagIPv6Frag;

    fm_bool   FlagIPv6Dest;

    fm_bool   FlagIPv6Auth;

    fm_byte   defaultDSCP;

    fm_bool   dropTagged;

    fm_bool   dropUntagged;

    fm_bool   dropMgmtISL;

    fm_bool   useDefaultDSCP;

    fm_bool   SwitchPriorityFromVLAN;

    fm_bool   SwitchPriorityFromDSCP;

    fm_bool   SwitchPriorityFromISL;

    fm_bool   SwitchPriorityPrefersDSCP;

} fm10000_modelParserPortCfg2;

typedef struct _fm10000_modelPortCfgIsl
{
    fm_uint16 srcGlort;

    fm_byte   USR;

    fm_byte   defaultPriority;

} fm10000_modelPortCfgIsl;

typedef struct _fm10000_modelParserCustomTag
{
    fm_uint16 Tag;

    fm_byte   Capture;

    fm_bool   CaptureSelect;

    fm_byte   Length;

} fm10000_modelParserCustomTag;

typedef struct _fm10000_modelParserMPLSTag
{
    fm_uint16 Tag1;

    fm_uint16 Tag2;

} fm10000_modelParserMPLSTag;

typedef struct _fm10000_modelParserDICfg
{
    fm_byte   Prot;

    fm_uint16 L4Port;

    fm_byte   WordOffset0;

    fm_byte   WordOffset1;

    fm_byte   WordOffset2;

    fm_byte   WordOffset3;

    fm_byte   WordOffset4;

    fm_byte   WordOffset5;

    fm_byte   WordOffset6;

    fm_byte   WordOffset7;

    fm_bool   CaptureTCPFlags;

    fm_bool   Enable;

    fm_bool   L4Compare;

} fm10000_modelParserDICfg;

/***************************************************
 * HASH related types
 **************************************************/

typedef struct _fm10000_modelHashKeys
{
    fm_uint64 crc34;
    
    fm_uint64 crc234;

    fm_byte l234Key[16];

    fm_bool zeroL2;

    fm_bool zeroL34;

    fm_bool useL34;

    fm_byte rotA;

    fm_byte rotB;

    fm_byte l34Key[42];

    fm_byte arpEcmpCfg;

} fm10000_modelHashKeys;

/***************************************************
 * GEN_MASK1 & GEN_MASK2 related types
 **************************************************/

typedef struct _fm10000_modelPortCfg3
{
    fm_bool filterVLANIngress;

    fm_bool LearningEnable;

} fm10000_modelPortCfg3;

typedef struct _fm10000_modelSysCfgRouter
{
    fm_byte TTLdisposal;

    fm_bool trapIPOptions;

} fm10000_modelSysCfgRouter;

/***************************************************
 * TRIGGERS related types
 **************************************************/

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_FORWARDING_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_FORWARDING_FORWARD,

    FM10000_MODEL_TRIG_ACTION_FORWARDING_REDIRECT,

    FM10000_MODEL_TRIG_ACTION_FORWARDING_DROP

} fm10000_modelTriggerActionForwarding;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_MIRRORING_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_MIRRORING_MIRROR

} fm10000_modelTriggerActionMirroring;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_SWITCH_PRI_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_SWITCH_PRI_REASSIGN

} fm10000_modelTriggerActionSwitchPri;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_TRAP_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_TRAP_TRAP,

    FM10000_MODEL_TRIG_ACTION_TRAP_LOG,

    FM10000_MODEL_TRIG_ACTION_TRAP_REVERT

} fm10000_modelTriggerActionTrap;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_TYPE_FORWARD = 0,

    FM10000_MODEL_TRIG_ACTION_TYPE_TRAP,

    FM10000_MODEL_TRIG_ACTION_TYPE_MIRROR,

    FM10000_MODEL_TRIG_ACTION_TYPE_PRI,

    FM10000_MODEL_TRIG_ACTION_TYPE_VLAN,

    FM10000_MODEL_TRIG_ACTION_TYPE_LEARN,

    FM10000_MODEL_TRIG_ACTION_TYPE_RATE

} fm10000_modelTriggerActionType;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_VLAN_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_VLAN_REASSIGN

} fm10000_modelTriggerActionVlan;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_LEARNING_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_LEARNING_DONT_LEARN,

    FM10000_MODEL_TRIG_ACTION_LEARNING_FORCE_LEARN

} fm10000_modelTriggerActionLearning;

typedef enum
{
    FM10000_MODEL_TRIG_ACTION_RATE_LIMIT_AS_IS = 0,

    FM10000_MODEL_TRIG_ACTION_RATE_LIMIT_APPLY

} fm10000_modelTriggerActionRateLimit;

typedef struct _fm10000_modelTriggerActions
{
    /** TRIGGERS forwarding action. */
    fm10000_modelTriggerActionForwarding forwardingAction;
    fm_uint16                            newDestGlort;
    fm_uint16                            newDestGlortMask;
    fm_uint64                            newDestMask;
    fm_bool                              filterDestMask;
    fm_uint64                            dropMask;

    /** TRIGGERS trap action. */
    fm10000_modelTriggerActionTrap       trapAction;
    fm_byte                              cpuCode;

    /** TRIGGERS mirroring action. */
    fm10000_modelTriggerActionMirroring  mirroringAction;

    fm_bool                              mirrorSelect;
    fm_byte                              mirrorProfileIndex;

    /** TRIGGERS ISL switch priority action. */
    fm10000_modelTriggerActionSwitchPri  switchPriAction;
    fm_byte                              newSwitchPri;

    /** TRIGGERS VLAN action. */
    fm10000_modelTriggerActionVlan       vlanAction;
    fm_uint16                            newVlan;

    /** TRIGGERS learning action. */
    fm10000_modelTriggerActionLearning   learningAction;

    /** TRIGGERS rate limiter action */
    fm10000_modelTriggerActionRateLimit  rateLimitAction;
    fm_byte                              newRateLimitNum; 


} fm10000_modelTriggerActions;

typedef struct _fm10000_modelTriggerResults
{
    fm_uint32                            action;

    fm10000_modelTriggerActionForwarding forwardingAction;
    fm_uint16                            destGlort;
    fm_uint64                            destMask;
    fm_bool                              filterDestMask;

    fm10000_modelTriggerActionTrap       trapAction;
    fm_byte                              cpuCode;

    /* RRC Spec. bug 22835.  Tracks Trig. to log CPU code in apply. */
    fm_bool                              log_action;

    fm10000_modelTriggerActionMirroring  mirroringAction;
    fm_bool                              rxMirror;
    fm_bool                              mirrorSelect;
    fm_byte                              mirrorProfileIndex;
    fm_bool                              mirror0ProfileV;
    fm_bool                              mirror1ProfileV;
    fm_byte                              mirror0ProfileIdx;
    fm_byte                              mirror1ProfileIdx;

    fm10000_modelTriggerActionSwitchPri  switchPriAction;
    fm_byte                              switchPri;

    fm10000_modelTriggerActionVlan       vlanAction;
    fm_uint16                            vlan;

    fm10000_modelTriggerActionLearning   learningAction;
    
    fm_bool                              rateLimitAction;
    fm_byte                              rateLimitNum;

} fm10000_modelTriggerResults;

typedef struct _fm10000_modelTriggerActionCfg1
{
    fm_byte ForwardingAction;

    fm_byte TrapAction;

    fm_bool MirroringAction;

    fm_bool SwitchPriAction;

    fm_bool VlanAction;

    fm_byte LearningAction;

    fm_bool RateLimitAction;

} fm10000_modelTriggerActionCfg1;

typedef struct _fm10000_modelTriggerActionCfg2
{

    fm_byte   NewSwitchPri;

    fm_uint16 NewEVID;

    fm_byte   RateLimitNum;

} fm10000_modelTriggerActionCfg2;

typedef struct _fm10000_modelTriggerActionDmask
{
    fm_uint64 NewDestMask;

    fm_bool   FilterDestMask;

} fm10000_modelTriggerActionDmask;

typedef struct _fm10000_modelTriggerActionGlort
{
    fm_uint16 NewDestGlort;

    fm_uint16 NewDestGlortMask;

} fm10000_modelTriggerActionGlort;

typedef struct _fm10000_modelTriggerActionMirror
{
    fm_bool   MirrorSelect;
    fm_byte   MirrorProfileIndex;
} fm10000_modelTriggerActionMirror;

typedef struct _fm10000_modelTriggerConditionCfg
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

    fm_byte MatchRandomThreshold;

    fm_byte MatchTx;

} fm10000_modelTriggerConditionCfg;

typedef enum
{
    FM10000_MODEL_TRIG_CDTN_CFG_MATCH_TX_MASK_EQ = 0,

    FM10000_MODEL_TRIG_CDTN_CFG_MATCH_TX_MASK_NE,

    FM10000_MODEL_TRIG_CDTN_CFG_MATCH_TX_EXACT_EQ,

    FM10000_MODEL_TRIG_CDTN_CFG_MATCH_TX_EXACT_NE

} fm10000_modelTriggerConditionCfgMatchTx;

typedef struct _fm10000_modelTriggerConditionFfu
{
    fm_byte FFU_ID;

    fm_byte FFU_Mask;

} fm10000_modelTriggerConditionFfu;

typedef struct _fm10000_modelTriggerConditionGlort
{
    fm_uint16 DestGlort;

    fm_uint16 GlortMask;

} fm10000_modelTriggerConditionGlort;

typedef struct _fm10000_modelTriggerConditionParam
{
    fm_byte SA_ID;

    fm_byte DA_ID;

    fm_byte VID_ID;

    fm_byte SwitchPri;

    fm_byte FrameClassMask;

    fm_byte RoutedMask;

    fm_byte FtypeMask;

} fm10000_modelTriggerConditionParam;

typedef struct _fm10000_triggerConditionType
{
    fm_uint16 EtherType;

    fm_uint16 EtherTypeMask;

} fm10000_modelTriggerConditionType;

/***************************************************
 * FSCHED related types
 **************************************************/

typedef struct _fm10000_modelIpMulticastTable
{
    fm_bool   tail;

    fm_bool   novlan;

    fm_bool   skip;

    fm_uint16 vlan;

    fm_uint16 nxtptr;

} fm10000_modelIpMulticastTable;

typedef struct _fm10000_modelLoopbackSuppress
{
    fm_uint16   glort;

    fm_uint16   mask;

} fm10000_modelLoopbackSuppress;

typedef struct _fm10000_modelMirrorGlorts
{
    fm_uint16 logGlort;

    fm_uint16 txMirrorGlort;

} fm10000_modelMirrorGlorts;

typedef enum
{
    FM10000_MODEL_MIRTYPE_NORMAL = 0,

    FM10000_MODEL_MIRTYPE_MIR0,

    FM10000_MODEL_MIRTYPE_MIR1,

/*  FM10000_MODEL_MIRTYPE_CPU */ /* Removed for RRC. */

} fm10000_modelMirTyp;

typedef struct _fm10000_modelTxMirror
{
    fm_byte   dstport;

    fm_byte   srcport;

} fm10000_modelTxMirror;

/***************************************************
 * MODIFY related types
 **************************************************/
typedef struct _fm10000_modelModMcastVlanTable
{
    fm_uint16   VID;

    fm_uint16   DGLORT;

    fm_bool     ReplaceVID;

    fm_bool     ReplaceDGLORT;

} fm10000_modelModMcastVlanTable;

typedef struct _fm10000_modelModVlanTagVid1Map
{
    fm_uint64   Tag;

    fm_uint16   VID;

} fm10000_modelModVlanTagVid1Map;

typedef struct _fm10000_modelModMirrorProfileTable
{
    fm_uint16   GLORT;

    fm_bool     TRUNC;

    fm_uint16   VID;

    fm_byte     VPRI;

} fm10000_modelModMirrorProfileTable;

typedef struct _fm10000_modelModPerPortCfg1
{
    fm_uint16   LoopbackSuppressGlort;

    fm_uint16   LoopbackSuppressMask;

    fm_byte     VID2MapIndex;

    fm_bool     EnableVLANUpdate;

} fm10000_modelModPerPortCfg1;

typedef struct _fm10000_modelModPerPortCfg2
{
    fm_byte     MirrorTruncationLen;

    fm_bool     EnablePcp1Update;

    fm_bool     EnablePcp2Update;

    fm_bool     EnableDei1Update;

    fm_bool     EnableDei2Update;

    fm_byte     VLAN1_EType;

    fm_byte     VLAN2_EType;

    fm_bool     EnableDMACRouting;

    fm_bool     EnableSMACRouting;

    fm_bool     EnableTTLDecrement;

    fm_bool     EnableDSCPModification;

    fm_bool     FTAG;

    fm_bool     VID2First;

    fm_byte     VlanTagging;

    fm_byte     TxPausePriEnVec;

    fm_bool     TxPauseType;

    fm_uint16   TxPauseValue;

    fm_bool     MinFrameSize;

} fm10000_modelModPerPortCfg2;

typedef struct _fm10000_modelModStatsCfg
{
    fm_bool     EnableGroup7;

    fm_bool     EnableGroup8;

} fm10000_modelModStatsCfg;

/***************************************************
 * The following state structure holds the
 * pipeline state while a packet is being processed.
 * This state is completely reset at the start of
 * processing for a packet.  Any persistent state
 * should be defined in fm10000_model data structure.
 **************************************************/

#include <platforms/common/model/fm10000/fm10000_model_state.h>

/**************************************************
 * Overall switch state structure. Contains
 * all registers used by the model and
 * all persistent internal states.
 **************************************************/

typedef struct _fm10000_model
{
    /* The switch that this white model state maps to */
    fm_int                           sw;

    /* Lock to provide exclusive access to the model state & registers */
    fm_lock                          modelLock;

    /* The register address space */
    fm_uint32                        registers[FM10000_MODEL_MAX_HW_ADDR + 1];

    /* Holds the state of a packet during the pipline processing */
    fm10000_modelState               packetState;

    /* Boolean indicating if the model is allowed to change the register cache.
     */
    fm_bool                          allowStateChange;

    /* The cached register state needed by the PARSER. */
    fm10000_modelParserPortCfg1      PARSER_PORT_CFG_1[FM10000_MAX_FABRIC_LOG_PORT + 1];
    fm10000_modelParserPortCfg2      PARSER_PORT_CFG_2[FM10000_MAX_FABRIC_LOG_PORT + 1];
    fm_uint64                        PARSER_PORT_CFG_3[FM10000_MAX_FABRIC_LOG_PORT + 1];
    fm10000_modelPortCfgIsl          PORT_CFG_ISL[FM10000_MAX_FABRIC_LOG_PORT + 1];
    fm_uint16                        PARSER_VLAN_TAG[FM10000_PARSER_VLAN_TAG_ENTRIES];
    fm10000_modelParserCustomTag     PARSER_CUSTOM_TAG[FM10000_PARSER_CUSTOM_TAG_ENTRIES];
    fm10000_modelParserMPLSTag       PARSER_MPLS_TAG;
    fm10000_modelParserDICfg         PARSER_DI_CFG[FM10000_PARSER_DI_CFG_ENTRIES];
    fm_byte                          RX_VPRI_MAP[FM10000_MAX_FABRIC_LOG_PORT + 1][16];
    fm_byte                          DSCP_PRI_MAP[FM10000_DSCP_PRI_MAP_ENTRIES];
    fm_byte                          VPRI_PRI_MAP[FM10000_VPRI_PRI_MAP_ENTRIES];
    fm_byte                          PARSER_CFG;

    /* The cached register state needed by the HASH stage. */
    fm10000_modelL234HashCfg         L234_HASH_CFG;
    fm10000_modelL34HashCfg          L34_HASH_CFG;
    fm10000_modelL34FlowHashCfg1     L34_FLOW_HASH_CFG_1;
    fm_uint32                        L34_FLOW_HASH_CFG_2;

    /* The cached register state needed by the MAPPER, FFU and NEXT_HOP stages.
     */
    fm10000_modelSysCfg8             SYS_CFG_8;

    /* The cached register state needed by the MAPPER stage */
    fm10000_modelFfuMapIpCfg         FFU_MAP_IP_CFG[FM10000_FFU_MAP_IP_CFG_ENTRIES];
    fm_uint64                        FFU_MAP_IP_LO[FM10000_FFU_MAP_IP_LO_ENTRIES];
    fm_uint64                        FFU_MAP_IP_HI[FM10000_FFU_MAP_IP_HI_ENTRIES];
    fm10000_modelFfuMapLength        FFU_MAP_LENGTH[FM10000_FFU_MAP_LENGTH_ENTRIES];
    fm10000_modelFfuMapL4Dst         FFU_MAP_L4_DST[FM10000_FFU_MAP_L4_DST_ENTRIES];
    fm10000_modelFfuMapL4Src         FFU_MAP_L4_SRC[FM10000_FFU_MAP_L4_SRC_ENTRIES];
    fm10000_modelFfuMapMac           FFU_MAP_MAC[FM10000_FFU_MAP_MAC_ENTRIES];
    fm10000_modelFfuMapProt          FFU_MAP_PROT[FM10000_FFU_MAP_PROT_ENTRIES];
    fm10000_modelFfuMapSrc           FFU_MAP_SRC[FM10000_FFU_MAP_SRC_ENTRIES];
    fm10000_modelFfuMapType          FFU_MAP_TYPE[FM10000_FFU_MAP_TYPE_ENTRIES];
    fm10000_modelFfuMapVlan          FFU_MAP_VLAN[FM10000_FFU_MAP_VLAN_ENTRIES];

    /* The cached register state needed by the FFU stage */
    fm10000_modelFfuMasterValid      FFU_MASTER_VALID;
    fm10000_modelFfuEgressChunkActions    FFU_EGRESS_CHUNK_ACTIONS[FM10000_FFU_EGRESS_CHUNK_ACTIONS_ENTRIES];
    fm10000_modelFfuEgressChunkCfg   FFU_EGRESS_CHUNK_CFG[FM10000_FFU_EGRESS_CHUNK_CFG_ENTRIES];
    fm10000_modelFfuEgressPortCfg    FFU_EGRESS_PORT_CFG[FM10000_FFU_EGRESS_PORT_CFG_ENTRIES];
    fm_uint32                        FFU_EGRESS_CHUNK_VALID[FM10000_FFU_EGRESS_CHUNK_VALID_ENTRIES];
    fm_uint32                        FFU_SLICE_CASCADE_ACTION[FM10000_FFU_SLICE_CASCADE_ACTION_ENTRIES];
    //fm_uint32                        FFU_SLICE_CASE[FM4000_FFU_SLICE_CASE_ENTRIES];
    fm10000_modelFfuSliceCfg         FFU_SLICE_CFG[FM10000_FFU_SLICE_CFG_ENTRIES_1][FM10000_FFU_SLICE_CFG_ENTRIES_0];
    fm10000_modelFfuSliceSram        FFU_SLICE_SRAM[FM10000_FFU_SLICE_SRAM_ENTRIES_1][FM10000_FFU_SLICE_SRAM_ENTRIES_0];
    fm10000_modelFfuSliceTcam        FFU_SLICE_TCAM[FM10000_FFU_SLICE_TCAM_ENTRIES_1][FM10000_FFU_SLICE_TCAM_ENTRIES_0];
    fm_uint32                        FFU_SLICE_VALID[FM10000_FFU_SLICE_VALID_ENTRIES];

    /* The cached register state needed by the NEXT_HOP stage */
    fm10000_modelArpTable            ARP_TABLE[FM10000_ARP_TABLE_ENTRIES];
    fm_uint32                        ARP_USED[FM10000_ARP_USED_ENTRIES];

    /* The cached register state needed by the L2_LOOKUP stage. */
    fm10000_modelEgressMstTable      EGRESS_MST_TABLE[FM10000_EGRESS_MST_TABLE_ENTRIES];
    fm10000_modelEgressVidTable      EGRESS_VID_TABLE[FM10000_EGRESS_VID_TABLE_ENTRIES];
    fm10000_modelIngressMstTable     INGRESS_MST_TABLE[FM10000_INGRESS_MST_TABLE_ENTRIES_1][FM10000_INGRESS_MST_TABLE_ENTRIES_0];
    fm10000_modelIngressVidTable     INGRESS_VID_TABLE[FM10000_INGRESS_VID_TABLE_ENTRIES];
    fm10000_modelMaTable             MA_TABLE[FM10000_MA_TABLE_ENTRIES_1][FM10000_MA_TABLE_ENTRIES_0];
    fm_uint32                        MA_USED_TABLE[FM10000_MA_USED_TABLE_ENTRIES_1][FM10000_MA_USED_TABLE_ENTRIES_0];
    fm10000_modelMaTableCfg1         MA_TABLE_CFG_1;
    fm_uint16                        MTU_TABLE[FM10000_MTU_TABLE_ENTRIES];
    fm_uint64                        PORT_CFG_2[FM10000_PORT_CFG_2_ENTRIES];
    fm10000_modelSysCfg1             SYS_CFG_1;
    fm_int                           l2lHashTable[FM10000_MODEL_L2L_HASH_TABLE_SIZE];


    /* The cached register state needed by the GLORT stage. */
    fm10000_modelGlortCam            GLORT_CAM[FM10000_GLORT_CAM_ENTRIES];
    fm10000_modelGlortDestTable      GLORT_DEST_TABLE[FM10000_GLORT_DEST_TABLE_ENTRIES];
    fm10000_modelGlortRam            GLORT_RAM[FM10000_GLORT_RAM_ENTRIES];

    /* The cached register state needed by the GEN_MASK1 and GEN_MASK2 stages.
     */
    fm10000_modelCanonicalGlortCam   CANONICAL_GLORT_CAM[FM10000_CANONICAL_GLORT_CAM_ENTRIES];
    fm10000_modelLoopbackSuppress    FH_LOOPBACK_SUPPRESS[FM10000_FH_LOOPBACK_SUPPRESS_ENTRIES];
    fm_uint64                        INTERNAL_PORT_MASK;
    fm10000_modelLagCfg              LAG_CFG[FM10000_LAG_CFG_ENTRIES];
    fm10000_modelMaTableCfg2         MA_TABLE_CFG_2;
    fm10000_modelPortCfg3            PORT_CFG_3[FM10000_PORT_CFG_3_ENTRIES];
    fm10000_modelRxMirrorCfg         RX_MIRROR_CFG;
    fm10000_modelLogMirrorProfile    LOG_MIRROR_PROFILE;
    fm10000_modelSysCfgRouter        SYS_CFG_ROUTER;
    fm10000_modelTxMirror            TX_MIRROR_FH;
    fm_uint64                        cpuMacAddr;
    fm10000_modelMcastEpoch          MCAST_EPOCH;

    /* The cached register state needed by the TRIGGERS stage. */
    fm10000_modelTriggerActionCfg1     TRIGGER_ACTION_CFG_1[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerActionCfg2     TRIGGER_ACTION_CFG_2[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerActionDmask    TRIGGER_ACTION_DMASK[FM10000_MODEL_TRIGGERS_COUNT];
    fm_uint64                          TRIGGER_ACTION_DROP[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerActionGlort    TRIGGER_ACTION_GLORT[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerActionMirror   TRIGGER_ACTION_MIRROR[FM10000_MODEL_TRIGGERS_COUNT];
    fm_uint64                          TRIGGER_CONDITION_AMASK[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerConditionCfg   TRIGGER_CONDITION_CFG[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerConditionFfu   TRIGGER_CONDITION_FFU[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerConditionGlort TRIGGER_CONDITION_GLORT[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerConditionParam TRIGGER_CONDITION_PARAM[FM10000_MODEL_TRIGGERS_COUNT];
    fm_uint64                          TRIGGER_CONDITION_RX[FM10000_MODEL_TRIGGERS_COUNT];
    fm_uint64                          TRIGGER_CONDITION_TX[FM10000_MODEL_TRIGGERS_COUNT];
    fm10000_modelTriggerConditionType  TRIGGER_CONDITION_TYPE[FM10000_MODEL_TRIGGERS_COUNT];

    /* The cached register state needed by the CM stage. */
    fm_byte                         CM_SMP_MEMBERSHIP[FM10000_CM_APPLY_TC_TO_SMP_ENTRIES];
    fm_byte                         SWITCH_PRI_TO_CLASS[FM10000_CM_APPLY_SWITCH_PRI_TO_TC_ENTRIES];

    /* The cached register state needed by the LEARNING stage. */
    fm_uint16                       MA_TCN_PTR_HEAD;
    fm_uint16                       MA_TCN_PTR_TAIL;
    fm10000_modelMaTcnDequeue       MA_TCN_DEQUEUE;
    fm10000_modelMaTcnFifo          MA_TCN_FIFO[FM10000_MA_TCN_FIFO_ENTRIES];

    /* The cached register state needed by the STATS_RX stages. */
    fm_uint64                       RX_STATS_BANK[FM10000_RX_STATS_BANK_ENTRIES_1][FM10000_RX_STATS_BANK_ENTRIES_0];

    fm10000_modelRxStatsCfg         RX_STATS_CFG[FM10000_RX_STATS_CFG_ENTRIES];


    /* The cached register state needed by the STATS_RX and STATS_TX stages. */
//  fm10000_modelStatsCfg           STATS_CFG[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_FIDForwarded[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_FloodForwarded[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_SpeciallyHandled[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_ParseErrDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_ParityError[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Trapped[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_PauseDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_STPDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_SecurityViolations[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_VlanTagDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_VlanIngressDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_VlanEgressDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_GlortMissDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_FFUDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TriggerDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_PolicerDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TTLDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_CMPrivDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_SMP0Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_SMP1Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_RxHog0Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_RxHog1Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxHog0Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxHog1Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_RateLimit0Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_RateLimit1Drops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_BadSMPDrops[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TriggerRedirects[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Others[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxUcstPkts[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxBcstPkts[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxMcstPkts[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxTimeOutDrop[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxErrorDrop[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxLoopbackDrop[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxMinto63[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx64Pkts[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx65to127[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx128to255[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx256to511[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx512to1023[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx1024to1522[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx1523to2047[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx2048to4095[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx4096to8191[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx8192to10239[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_Tx10240toMax[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxErrors[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxOctets[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxErrorOctets[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort0[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort1[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort2[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort3[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort4[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort5[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort6[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort7[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort8[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort9[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort10[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort11[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort12[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort13[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort14[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort15[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort16[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort17[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort18[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort19[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort20[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort21[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort22[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort23[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_TxPort24[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_VlanUcstPkts[64];
//  fm_uint64                       STAT_VlanXcstPkts[64];
//  fm_uint64                       STAT_VlanUcstOctets[64];
//  fm_uint64                       STAT_VlanXcstOctets[64];
//  fm_uint64                       STAT_Trigger[FM10000_MODEL_TRIGGERS_COUNT];
//  fm_uint64                       STAT_EgressACLPkts[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint64                       STAT_EgressACLOctets[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint32                       STATS_DROP_COUNT_RX[FM10000_MAX_FABRIC_LOG_PORT + 1];
//  fm_uint32                       STATS_DROP_COUNT_TX[FM10000_MAX_FABRIC_LOG_PORT + 1];

    /* The cached register state needed by the FSCHED stage. */
    fm10000_modelIpMulticastTable   IP_MULTICAST_TABLE[FM10000_SCHED_MCAST_DEST_TABLE_ENTRIES];
    fm_uint32                       LOG_MASK;
    fm10000_modelLoopbackSuppress   LOOPBACK_SUPPRESS[FM10000_MAX_FABRIC_LOG_PORT + 1];
    fm10000_modelMirrorGlorts       MIRROR_GLORTS;
    fm10000_modelTxMirror           TX_MIRROR;

    /* The cached register state needed by the MODIFY stage. */
    fm10000_modelModMcastVlanTable      MOD_MCAST_VLAN_TABLE[FM10000_MOD_MCAST_VLAN_TABLE_ENTRIES];
    fm10000_modelModVlanTagVid1Map      MOD_VLAN_TAG_VID1_MAP[FM10000_MOD_VLAN_TAG_VID1_MAP_ENTRIES];
    fm_uint64                           MOD_VID2_MAP[FM10000_MOD_VID2_MAP_ENTRIES];
    fm10000_modelModMirrorProfileTable  MOD_MIRROR_PROFILE_TABLE[FM10000_MOD_MIRROR_PROFILE_TABLE_ENTRIES];
    fm10000_modelModPerPortCfg1         MOD_PER_PORT_CFG_1[FM10000_MOD_PER_PORT_CFG_1_ENTRIES];
    fm10000_modelModPerPortCfg2         MOD_PER_PORT_CFG_2[FM10000_MOD_PER_PORT_CFG_2_ENTRIES];
    fm_uint64                           MOD_VPRI1_MAP[FM10000_MOD_VPRI1_MAP_ENTRIES];
    fm_uint64                           MOD_VPRI2_MAP[FM10000_MOD_VPRI2_MAP_ENTRIES];
    fm_uint16                           MOD_VLAN_ETYPE[FM10000_MOD_VLAN_ETYPE_ENTRIES];
    fm10000_modelModStatsCfg            MOD_STATS_CFG[FM10000_MOD_STATS_CFG_ENTRIES];
    fm_uint64                           MOD_STATS_BANK_FRAME[FM10000_MOD_STATS_BANK_FRAME_ENTRIES_1][FM10000_MOD_STATS_BANK_FRAME_ENTRIES_0];
    fm_uint64                           MOD_STATS_BANK_BYTE[FM10000_MOD_STATS_BANK_BYTE_ENTRIES_1][FM10000_MOD_STATS_BANK_BYTE_ENTRIES_0];
    fm_byte                             MOD_MAX_MGMT_WAIT_CYCLE;
    fm_uint64                           MOD_ROUTER_SMAC[FM10000_MOD_ROUTER_SMAC_ENTRIES];
    fm_uint64                           MOD_PAUSE_SMAC;

    /* The set of registers needed to recreate the EPL interrupt chain. */
    fm_uint32                       PCS_IP[FM10000_MAX_EPL + 1];
    fm_uint32                       EPL_INT_DETECT[FM10000_MAX_EPL + 1];
    fm_uint32                       GLOBAL_EPL_INT_DETECT;

    /* The set of registers needed to recreate the NEXT_HOP interrupt chain. */
    fm_uint32                       ARP_IM;
    fm_uint32                       ARP_IP;

    /* The set of registers needed to recreate the TCN interrupt chain. */
    fm_uint32                       MA_TCN_IM;
    fm_uint32                       MA_TCN_IP;
    fm_uint16                       MA_TCN_WM[FM10000_MA_TCN_WM_ENTRIES];
    fm_uint16                       MA_TCN_USAGE[FM10000_MA_TCN_USAGE_ENTRIES];

    fm_uint32                       FH_TAIL_IP;
    fm_uint32                       FH_TAIL_IM;
    /* The global interrupt detection register */
    fm_uint64                       INTERRUPT_DETECT;

} fm10000_model;




/*****************************************************************************
 * Public Function Prototypes
 *****************************************************************************/

/***************************************************
 * Register management functions.
 **************************************************/

fm10000_resetDomain fm10000ModelFindRegisterResetDomain(fm_uint32 addr,
                                                        fm10000_resetDomain domain);

fm_status fm10000ModelGetOffsetMult1(fm_uint32 addr,
                                     fm_uint32 baseAddr,
                                     fm_int    entries,
                                     fm_int    stride,
                                     fm_int *  index,
                                     fm_int *  word);

fm_status fm10000ModelGetOffsetMult2(fm_uint32 addr,
                                     fm_uint32 baseAddr,
                                     fm_int    entries1,
                                     fm_int    entries0,
                                     fm_int    stride1,
                                     fm_int    stride0,
                                     fm_int *  index1,
                                     fm_int *  index0,
                                     fm_int *  word);

void fm10000ModelGetRegisterAccess(fm_uint32  addr,
                                   fm_uint32 *rwMask,
                                   fm_uint32 *roMask,
                                   fm_uint32 *cwMask,
                                   fm_uint32 *cw1Mask,
                                   fm_uint32 *rvMask);

fm_uint32 fm10000ModelGetRegisterDefault(fm_uint32 addr);

fm_status fm10000ModelReadCSRInternal(fm10000_model *model,
                                       fm_uint32     addr,
                                       fm_uint32     value);

fm_status fm10000ModelWriteCSRAbsolute(fm10000_model *model,
                                       fm_uint32     addr,
                                       fm_uint32     newValue);

fm_status fm10000ModelWriteCSRAbsolute64(fm10000_model *model,
                                         fm_uint32     addr,
                                         fm_uint64     value);

fm_status fm10000ModelWriteCSRInternal(fm10000_model *model,
                                       fm_uint32     addr,
                                       fm_uint32     value,
                                       fm_bool       init);

fm_status fm10000ModelWriteCSRMultAbsolute(fm10000_model *model,
                                           fm_uint32     addr,
                                           fm_int        n,
                                           fm_uint32 *   value);

fm_status fm10000ModelEplWriteCSR(fm10000_model *model,
                                  fm_uint32     addr,
                                  fm_uint32     value,
                                  fm_bool       init);

fm_status fm10000ModelFfuWriteCSR(fm10000_model *model,
                                  fm_uint32     addr,
                                  fm_uint32     val,
                                  fm_bool       init);

fm_status fm10000ModelNextHopWriteCSR(fm10000_model *model,
                                      fm_uint32     addr,
                                      fm_uint32     newValue);

fm_status fm10000ModelL2LookupWriteCSR(fm10000_model *model,
                                       fm_uint32     addr,
                                       fm_uint32     newValue);

fm_status fm10000ModelParserWriteCSR(fm10000_model *model,
                                     fm_uint32     addr,
                                     fm_uint32     value);

fm_status fm10000ModelMapperWriteCSR(fm10000_model *model,
                                     fm_uint32     addr,
                                     fm_uint32     value);

fm_status fm10000ModelGlortWriteCSR(fm10000_model *model,
                                    fm_uint32     addr,
                                    fm_uint32     newValue,
                                    fm_bool       init);

fm_status fm10000ModelGenMaskWriteCSR(fm10000_model *model,
                                      fm_uint32     addr,
                                      fm_uint32     val);

fm_status fm10000ModelTriggersWriteCSR(fm10000_model *model,
                                      fm_uint32     addr,
                                      fm_uint32     value);

fm_status fm10000ModelCmWriteCSR(fm10000_model *model,
                                 fm_uint32     addr,
                                 fm_uint32     val);

fm_status fm10000ModelStatsWriteCSR(fm10000_model *model,
                                    fm_uint32     addr,
                                    fm_uint32     val);

fm_status fm10000ModelFschedWriteCSR(fm10000_model *model,
                                     fm_uint32     addr,
                                     fm_uint32     value);

fm_status fm10000ModelModifyWriteCSR(fm10000_model *model,
                                     fm_uint32     addr,
                                     fm_uint32     value);

fm_status fm10000ModelLearningWriteCSR(fm10000_model *model,
                                       fm_uint32     addr,
                                       fm_uint32     newValue);

fm_status fm10000ModelLearningReadCSR(fm10000_model *model,
                                       fm_uint32     addr,
                                       fm_uint32     value);

/***************************************************
 * FM10000 white model initialization functions.
 **************************************************/
fm_status fm10000ModelEplInitialize(fm10000_model *model);

fm_status fm10000ModelInitializeInternal(fm10000_model *model);

fm_status fm10000ModelInitL2LHash(fm10000_model *model);

/***************************************************
 * FM10000 white model utility functions.
 **************************************************/
fm_bool fm10000ModelIsMulticastIPAddress(fm_uint32 *ip,
                                         fm_bool isIPv4,
                                         fm_bool isIPv6);

fm_bool fmModelIsCpuMacAddress(fm10000_model *model, 
                               fm_macaddr macAddrIn);

/***************************************************
 * FM10000 white model stages in pipeline order.
 **************************************************/
fm_int fm10000ModelEplRx(fm10000_model *model);

void fm10000ModelXbarRx(fm10000_model *model);

void fm10000ModelParser(fm10000_model *model);

void fm10000ModelMapper(fm10000_model *model);

fm_status fm10000ModelFfu(fm10000_model *model);

void fm10000ModelHash(fm10000_model *model);

fm_status fm10000ModelNextHop(fm10000_model *model);

fm_status fm10000ModelPolicer(fm10000_model *model);

fm_status fm10000ModelL2Lookup(fm10000_model *model);

void fm10000ModelGlort(fm10000_model *model);

void fm10000ModelGenMask1(fm10000_model *model);

fm_status fm10000ModelTriggers(fm10000_model *model);

void fm10000ModelCm(fm10000_model *model);

fm_status fm10000ModelGenMask2(fm10000_model *model);

fm_status fm10000ModelLearning(fm10000_model *model);

fm_status fm10000ModelStatsRx(fm10000_model *model);

fm_bool fm10000ModelFsched(fm10000_model *model);

fm_status fm10000ModelModify(fm10000_model *model,
                            fm_byte *     packet,
                            fm_uint32     maxPktSize);

fm_status fm10000ModelStatsTx(fm10000_model *model);

void fm10000ModelXbarTx(fm10000_model *model);

fm_int fm10000ModelEplTx(fm10000_model *model,
                         fm_byte *     packet,
                         fm_uint32     maxPktSize);

#define FM10000_MODEL_AMASK_FORWARD_SPECIAL      (FM_LITERAL_U64(1) << 0)
#define FM10000_MODEL_AMASK_DROP_PARSE_ERROR     (FM_LITERAL_U64(1) << 1)
#define FM10000_MODEL_AMASK_DROP_PARITY_ERROR    (FM_LITERAL_U64(1) << 2)
#define FM10000_MODEL_AMASK_TRAP_RESERVED_MAC    (FM_LITERAL_U64(1) << 3)
#define FM10000_MODEL_AMASK_TRAP_RESERVED_MAC_REMAP  (FM_LITERAL_U64(1) << 4)
#define FM10000_MODEL_AMASK_DROP_CONTROL         (FM_LITERAL_U64(1) << 10)
#define FM10000_MODEL_AMASK_DROP_RESERVED_MAC    (FM_LITERAL_U64(1) << 11)
#define FM10000_MODEL_AMASK_DROP_TAG             (FM_LITERAL_U64(1) << 12)
#define FM10000_MODEL_AMASK_DROP_SMAC            (FM_LITERAL_U64(1) << 13)
#define FM10000_MODEL_AMASK_DROP_PORT_SV         (FM_LITERAL_U64(1) << 14)
#define FM10000_MODEL_AMASK_TRAP_CPU             (FM_LITERAL_U64(1) << 15)
#define FM10000_MODEL_AMASK_DROP_VLAN_IV         (FM_LITERAL_U64(1) << 16)
#define FM10000_MODEL_AMASK_DROP_STP_INL         (FM_LITERAL_U64(1) << 17)
#define FM10000_MODEL_AMASK_DROP_STP_IL          (FM_LITERAL_U64(1) << 18)
#define FM10000_MODEL_AMASK_DROP_FFU             (FM_LITERAL_U64(1) << 19)
#define FM10000_MODEL_AMASK_TRAP_FFU             (FM_LITERAL_U64(1) << 20)
#define FM10000_MODEL_AMASK_TRAP_ICMP_TTL        (FM_LITERAL_U64(1) << 21)
#define FM10000_MODEL_AMASK_TRAP_IP_OPTION       (FM_LITERAL_U64(1) << 22)
#define FM10000_MODEL_AMASK_TRAP_MTU             (FM_LITERAL_U64(1) << 23)
#define FM10000_MODEL_AMASK_TRAP_IGMP            (FM_LITERAL_U64(1) << 24)
#define FM10000_MODEL_AMASK_TRAP_TTL             (FM_LITERAL_U64(1) << 25)
#define FM10000_MODEL_AMASK_DROP_TTL             (FM_LITERAL_U64(1) << 26)
#define FM10000_MODEL_AMASK_DROP_DLF             (FM_LITERAL_U64(1) << 27)
#define FM10000_MODEL_AMASK_DROP_GLORT_CAM_MISS  (FM_LITERAL_U64(1) << 28)
#define FM10000_MODEL_AMASK_DROP_NULL_GLORTDEST  (FM_LITERAL_U64(1) << 29)
#define FM10000_MODEL_AMASK_DROP_VLAN_EV         (FM_LITERAL_U64(1) << 30)
#define FM10000_MODEL_AMASK_DROP_POLICER         (FM_LITERAL_U64(1) << 31)
#define FM10000_MODEL_AMASK_DROP_STP_E           (FM_LITERAL_U64(1) << 32)
#define FM10000_MODEL_AMASK_DROP_LOOPBACK        (FM_LITERAL_U64(1) << 33)
#define FM10000_MODEL_AMASK_FFU_ARP_DGLORT       (FM_LITERAL_U64(1) << 34)
#define FM10000_MODEL_AMASK_FORWARD_FLOOD        (FM_LITERAL_U64(1) << 35)
#define FM10000_MODEL_AMASK_SWITCH_RESERVED_MAC  (FM_LITERAL_U64(1) << 36)
#define FM10000_MODEL_AMASK_FORWARD_FID          (FM_LITERAL_U64(1) << 37)
#define FM10000_MODEL_AMASK_LOG_FFU              (FM_LITERAL_U64(1) << 38)
#define FM10000_MODEL_AMASK_LOG_RESERVED_MAC     (FM_LITERAL_U64(1) << 39)
#define FM10000_MODEL_AMASK_LOG_ARP_REDIRECT     (FM_LITERAL_U64(1) << 40)
#define FM10000_MODEL_AMASK_LOG_MCST_ICMP_TTL    (FM_LITERAL_U64(1) << 41)
#define FM10000_MODEL_AMASK_LOG_IP_MCST_TTL2     (FM_LITERAL_U64(1) << 42)
#define FM10000_MODEL_AMASK_HDR_TIMEOUT          (FM_LITERAL_U64(1) << 43)
#define FM10000_MODEL_AMASK_MIRROR_FFU           (FM_LITERAL_U64(1) << 44)

#endif /* __FM10000_MODEL_TYPES_H */

