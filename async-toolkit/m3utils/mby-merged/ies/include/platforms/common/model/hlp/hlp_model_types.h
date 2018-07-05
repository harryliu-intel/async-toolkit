/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_types.h
 * Creation Date:   June 6, 2012
 * Description:     Data structures, definitions and types for the HLP model.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2012 - 2016 Intel Corporation. All Rights Reserved.
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

#ifndef __HLP_MODEL_TYPES_H
#define __HLP_MODEL_TYPES_H

#include <platforms/common/model/hlp/hlp_model.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/
#define FM_MODEL_TAKE_LOCK(model)                                              \
    fmCaptureLock(&model->modelLock, FM_WAIT_FOREVER);

#define FM_MODEL_DROP_LOCK(model)                                              \
    fmReleaseLock(&model->modelLock);

/*****************************************************************************/
/** DISPLAY
 * \desc            Controls the debug message by verbosity.
 *
 * \param[in]       the debug message.
 *
 *****************************************************************************/
#define WM_DISPLAY(wmDisplayVerbose, cat, ...)                                   \
    if (wmDisplayVerbose >= 1)                            \
    {                                                   \
        FM_LOG_DEBUG(cat, __VA_ARGS__); \
    }

#define WM_DISPLAY2(wmDisplayVerbose, cat, ...)                                   \
    if (wmDisplayVerbose >= 2)                            \
    {                                                   \
        FM_LOG_DEBUG2(cat, __VA_ARGS__); \
    }

#define WM_DISPLAY3(wmDisplayVerbose, cat, ...)                                  \
    if (wmDisplayVerbose == 3)                           \
    {                                                  \
        FM_LOG_DEBUG3(cat, __VA_ARGS__);\
    }

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
#define FM_MODEL_GET_OFFSET_1(addr, csr, index)                            \
    hlpModelGetOffsetMult1(addr,                                           \
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
#define FM_MODEL_GET_OFFSET_2(addr, csr, index1, index0)                   \
    hlpModelGetOffsetMult2(addr,                                           \
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
#define FM_MODEL_GET_OFFSET_MULT_1(addr, csr, index, word)                 \
    hlpModelGetOffsetMult1(addr,                                           \
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
#define FM_MODEL_GET_OFFSET_MULT_2(addr, csr, index1, index0, word)        \
    hlpModelGetOffsetMult2(addr,                                           \
                           csr(0, 0, 0),                                   \
                           csr ## _ENTRIES_1,                              \
                           csr ## _ENTRIES_0,                              \
                           csr(1, 0, 0) - csr(0, 0, 0),                    \
                           csr(0, 1, 0) - csr(0, 0, 0),                    \
                           index1,                                         \
                           index0,                                         \
                           word)

/*****************************************************************************/
/** FM_MODEL_GET_OFFSET_MULT_3
 * \ingroup model10k
 *
 * \desc            Retrieves the register index and word corresponding to the
 *                  specified hardware address of the specified three-dimensional,
 *                  multi-word register.
 *
 * \param[in]       addr is the hardware address.
 *
 * \param[in]       csr is the name of the register.
 *
 * \param[out]      index2 points to user-allocated storage in which this macro
 *                  will store the register index of the third dimension.
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
#define FM_MODEL_GET_OFFSET_MULT_3(addr, csr, index2, index1, index0, word) \
    hlpModelGetOffsetMult3(addr,                                            \
                           csr(0, 0, 0, 0),                                 \
                           csr ## _ENTRIES_2,                               \
                           csr ## _ENTRIES_1,                               \
                           csr ## _ENTRIES_0,                               \
                           csr(1, 0, 0, 0) - csr(0, 0, 0, 0),               \
                           csr(0, 1, 0, 0) - csr(0, 0, 0, 0),               \
                           csr(0, 0, 1, 0) - csr(0, 0, 0, 0),               \
                           index2,                                          \
                           index1,                                          \
                           index0,                                          \
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
#define DWORD_OFF(addr) ((addr)/4)

#define FM_MODEL_GET_REG_PTR(model, addr)                                      \
    &((model)->registers[DWORD_OFF(addr)])

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

#define FM_MODEL_IN_RANGE_MULT_3(addr, csr)                                    \
    ( ( (addr) >= csr(0, 0, 0, 0) ) &&                                         \
      ( (addr) <= csr(csr ## _ENTRIES_2 - 1,                                   \
                      csr ## _ENTRIES_1 - 1,                                   \
                      csr ## _ENTRIES_0 - 1,                                   \
                      ( csr(0, 0, 1, 0) - csr(0, 0, 0, 0) ) - 1) ) )

#define fmModelIsBroadcastMacAddress(addr)                                     \
    ( ( (addr) == FM_LITERAL_U64(0xFFFFFFFFFFFF) ) ? TRUE : FALSE )

#define fmModelIsUnicastMacAddress(addr)                                       \
         ( ((addr) & FM_LITERAL_U64(0x010000000000)) == 0 )

#define fmModelIsMulticastMacAddress(addr)                                     \
         ( ( ((addr) & FM_LITERAL_U64(0x010000000000)) != 0 ) &&               \
           !fmModelIsBroadcastMacAddress(addr) )


#define HLP_MODEL_MAX_HW_ADDR               0x5ffffff
#define HLP_SEGMENT_LEN                     256
#define HLP_MODEL_TRIGGERS_COUNT            96 
#define HLP_MODEL_PORTS_COUNT               24
#define HLP_MODEL_ARP_REDIRECT_DIP_WORDS    4
#define HLP_MODEL_ARP_REDIRECT_SIP_WORDS    4
#define HLP_MODEL_AMASK_WIDTH               38
#define HLP_MODEL_AMASK_WIDTH_IND_UNUSED    44
#define HLP_MODEL_AMASK_TRIGGERS_WIDTH      45
#define HLP_MODEL_LOG_MASK_WIDTH            6

#define HLP_MODEL_DEFAULT_DMASK             0xFFFFFF


#define HLP_MODEL_DMAC_IEEE_PREFIX          FM_LITERAL_U64(0x0180c2000000)
#define HLP_MODEL_SPECIAL_DMASK             0xFFFFFFFFFF00

#define HLP_MODEL_DMAC_BPDU                                                \
    ( HLP_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x00) )
#define HLP_MODEL_DMAC_MAC_CTRL                                            \
    ( HLP_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x01) )
#define HLP_MODEL_DMAC_GMRP                                                \
    ( HLP_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x20) )
#define HLP_MODEL_DMAC_GVRP                                                \
    ( HLP_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x21) )
#define HLP_MODEL_DMAC_LACP                                                \
    ( HLP_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x02) )
#define HLP_MODEL_DMAC_PORT_AUTH                                           \
    ( HLP_MODEL_DMAC_IEEE_PREFIX | FM_LITERAL_U64(0x03) )

/* Bit position for IPMISC */
#define HLP_MODEL_IPMISC_TRAPIPOPTION       0
#define HLP_MODEL_IPMISC_DONOTFRAG          1
#define HLP_MODEL_IPMISC_HEADFRAG           2
#define HLP_MODEL_IPMISC_MOREFRAG           3
#define HLP_MODEL_IPMISC_ROUTABLE           4

/* Parser defines */
#define HLP_MODEL_N_PARSER_KEYS                 80
#define HLP_MODEL_N_PARSER_FLAGS                47
#define HLP_MODEL_N_PARSER_PTRS                 7

/* Mapper defines*/
#define HLP_MODEL_N_REALIGN_KEYS                80
#define HLP_MODEL_N_IS_IP_BITS                  2
#define HLP_MODEL_N_MAC_ROUTABLE_BITS           4
#define HLP_MODEL_N_VLAN_ROUTER_ID              2
#define HLP_MODEL_N_MAP8                        8
#define HLP_MODEL_N_MAP16                       8
#define HLP_MODEL_N_REWRITE_KEY8_BITS           32
#define HLP_MODEL_N_REWRITE_KEY16_BITS          16

#define HLP_MODEL_OTR_L3_LEN_LIMIT              14
#define HLP_MODEL_OTR_TUN_LEN_LIMIT             18
#define HLP_MODEL_INR_L3_LEN_LIMIT              14

#define HLP_MODEL_L2_MIN_SIZE                   14
#define HLP_MODEL_MPLS_MIN_SIZE                 0
#define HLP_MODEL_L3_MIN_SIZE                   20
#define HLP_MODEL_L4_TCP_MIN_SIZE               18
#define HLP_MODEL_L4_MIN_SIZE                   8

/* FFU defines */
#define HLP_MODEL_FFU_N_KEY8                    64 
#define HLP_MODEL_FFU_N_KEY16                   32
#define HLP_MODEL_FFU_N_KEY32                   16 
#define HLP_MODEL_FFU_N_KEYS                    \
    ( HLP_MODEL_FFU_N_KEY8 + HLP_MODEL_FFU_N_KEY16 + HLP_MODEL_FFU_N_KEY32 )

#define HLP_MODEL_FFU_KEY16_BASE                0
#define HLP_MODEL_FFU_KEY8_BASE                 \
    ( HLP_MODEL_FFU_KEY16_BASE + HLP_MODEL_FFU_N_KEY16 )
#define HLP_MODEL_FFU_KEY32_BASE                 \
    ( HLP_MODEL_FFU_KEY8_BASE + HLP_MODEL_FFU_N_KEY8 )

#define HLP_MODEL_FFU_N_HASH_KEYS               \
    ( HLP_MODEL_FFU_N_KEY8 + HLP_MODEL_FFU_N_KEY16*2 + HLP_MODEL_FFU_N_KEY32*4 )

#define HLP_MODEL_FFU_MAX_HASH_ENTRY_SIZE       64
#define HLP_MODEL_FFU_MAX_HASH_ACTIONS          4
#define HLP_MODEL_FFU_HASH_CAM_ETY_7_BITS_31_0  0
#define HLP_MODEL_FFU_HASH_CAM_ETY_7_BITS_63_32 1
#define HLP_MODEL_FFU_HASH_CAM_ETY_6_BITS_31_0  2
#define HLP_MODEL_FFU_HASH_CAM_ETY_6_BITS_63_32 3

/* FFU Action entry encoding */
#define HLP_MODEL_FFU_N_ACT24                   16
#define HLP_MODEL_FFU_N_ACT4                    23
#define HLP_MODEL_FFU_N_ACT1                    24
#define HLP_MODEL_FFU_N_REMAP_ACTIONS           8
#define HLP_MODEL_FFU_ACTIONS_PER_ENTRY         2
#define HLP_MODEL_FFU_ACTION_PREC_WIDTH         3
#define HLP_MODEL_FFU_ACTION_l_PREC             29
#define HLP_MODEL_FFU_ACTION_h_PREC             31
#define HLP_MODEL_FFU_ACTION_l_ENTRYTYPE        24
#define HLP_MODEL_FFU_ACTION_h_ENTRYTYPE        28
#define HLP_MODEL_FFU_ACTION_l_SET1_24B_INDEX   24
#define HLP_MODEL_FFU_ACTION_h_SET1_24B_INDEX   27
#define HLP_MODEL_FFU_ACTION_l_SET1_24B_VALUE   0
#define HLP_MODEL_FFU_ACTION_h_SET1_24B_VALUE   23

#define HLP_MODEL_FFU_ACTION_l_SET3_4B_INDEXC   22
#define HLP_MODEL_FFU_ACTION_h_SET3_4B_INDEXC   26
#define HLP_MODEL_FFU_ACTION_l_SET3_4B_INDEXB   17
#define HLP_MODEL_FFU_ACTION_h_SET3_4B_INDEXB   21
#define HLP_MODEL_FFU_ACTION_l_SET3_4B_INDEXA   12
#define HLP_MODEL_FFU_ACTION_h_SET3_4B_INDEXA   16
#define HLP_MODEL_FFU_ACTION_l_SET3_4B_VALUEC   8
#define HLP_MODEL_FFU_ACTION_h_SET3_4B_VALUEC   11
#define HLP_MODEL_FFU_ACTION_l_SET3_4B_VALUEB   4
#define HLP_MODEL_FFU_ACTION_h_SET3_4B_VALUEB   7
#define HLP_MODEL_FFU_ACTION_l_SET3_4B_VALUEA   0
#define HLP_MODEL_FFU_ACTION_h_SET3_4B_VALUEA   3

#define HLP_MODEL_FFU_ACTION_l_SET3_1B_INDEXC   16
#define HLP_MODEL_FFU_ACTION_h_SET3_1B_INDEXC   21
#define HLP_MODEL_FFU_ACTION_b_SET3_1B_VC       22
#define HLP_MODEL_FFU_ACTION_b_SET3_1B_EC       23
#define HLP_MODEL_FFU_ACTION_l_SET3_1B_INDEXB   8
#define HLP_MODEL_FFU_ACTION_h_SET3_1B_INDEXB   13
#define HLP_MODEL_FFU_ACTION_b_SET3_1B_VB       14
#define HLP_MODEL_FFU_ACTION_b_SET3_1B_EB       15
#define HLP_MODEL_FFU_ACTION_l_SET3_1B_INDEXA   0
#define HLP_MODEL_FFU_ACTION_h_SET3_1B_INDEXA   5
#define HLP_MODEL_FFU_ACTION_b_SET3_1B_VA       6
#define HLP_MODEL_FFU_ACTION_b_SET3_1B_EA       7

#define HLP_MODEL_FFU_ACTION_l_SET8_1B_ENABLE   8 
#define HLP_MODEL_FFU_ACTION_h_SET8_1B_ENABLE   15
#define HLP_MODEL_FFU_ACTION_l_SET8_1B_INDEX    16
#define HLP_MODEL_FFU_ACTION_h_SET8_1B_INDEX    19
#define HLP_MODEL_FFU_ACTION_l_SET8_1B_VALUE    0
#define HLP_MODEL_FFU_ACTION_h_SET8_1B_VALUE    7 

#define HLP_MODEL_FFU_ACTION_l_SET4_4B_ENABLE   20 
#define HLP_MODEL_FFU_ACTION_h_SET4_4B_ENABLE   23
#define HLP_MODEL_FFU_ACTION_l_SET4_4B_INDEX    16
#define HLP_MODEL_FFU_ACTION_h_SET4_4B_INDEX    19
#define HLP_MODEL_FFU_ACTION_l_SET4_4B_VALUE    0
#define HLP_MODEL_FFU_ACTION_h_SET4_4B_VALUE    15

/* FFU Remap Entry Encoding */
#define HLP_MODEL_FFU_REMAP_b_ENTRYTYPE         23
#define HLP_MODEL_FFU_REMAP_l_SET8_1B_INDEX     16
#define HLP_MODEL_FFU_REMAP_h_SET8_1B_INDEX     23
#define HLP_MODEL_FFU_REMAP_l_SET8_1B_VALUE     8 
#define HLP_MODEL_FFU_REMAP_h_SET8_1B_VALUE     15
#define HLP_MODEL_FFU_REMAP_l_SET8_1B_MASK      0 
#define HLP_MODEL_FFU_REMAP_h_SET8_1B_MASK      7

#define HLP_MODEL_FFU_REMAP_l_SET1_16B_INDEX     16
#define HLP_MODEL_FFU_REMAP_h_SET1_16B_INDEX     23
#define HLP_MODEL_FFU_REMAP_l_SET1_16B_VALUE     0 
#define HLP_MODEL_FFU_REMAP_h_SET1_16B_VALUE     15

/* */
#define HLP_MODEL_FFU_HASH_ENTRY_MODE_32B   0
#define HLP_MODEL_FFU_HASH_ENTRY_MODE_64B   1

/* Bit number for fields of FFU_SLICE_SRAM.RouteData */
/* Bit numbers when RouteType==GLORT */
#define HLP_MODEL_FFU_ROUTE_l_DGLORT        0
#define HLP_MODEL_FFU_ROUTE_h_DGLORT        15
#define HLP_MODEL_FFU_ROUTE_b_FLOODSET      16
/* Bit numbers when RouteType==ARP */
#define HLP_MODEL_FFU_ROUTE_l_ARP_INDEX     0
#define HLP_MODEL_FFU_ROUTE_h_ARP_INDEX     15
#define HLP_MODEL_FFU_ROUTE_l_GROUP_SIZE    16
#define HLP_MODEL_FFU_ROUTE_h_GROUP_SIZE    19
#define HLP_MODEL_FFU_ROUTE_b_GROUP_TYPE    20
#define HLP_MODEL_FFU_ROUTE_b_ARP_ROUTE     21

/* Bit number for FFU Flags */
#define HLP_MODEL_FFU_FLAGS_b_DROP          0
#define HLP_MODEL_FFU_FLAGS_b_TRAP          1
#define HLP_MODEL_FFU_FLAGS_b_LOG           2
#define HLP_MODEL_FFU_FLAGS_b_NO_ROUTE      3
#define HLP_MODEL_FFU_FLAGS_b_RX_MIRROR     4
#define HLP_MODEL_FFU_FLAGS_b_CAPTURE_TIME  5

#define HLP_MODEL_N_MA_HASH_KEYS            9
#define HLP_MODEL_MA_TABLE_CAM_BANK         5
#define HLP_MODEL_MA_TABLE_CAM_ENTRIES      1024

#define HLP_MODEL_MA_ENTRY_TYPE_NOT_USED      0
#define HLP_MODEL_MA_ENTRY_TYPE_PROVISIONAL   1
#define HLP_MODEL_MA_ENTRY_TYPE_DYNAMIC       2
#define HLP_MODEL_MA_ENTRY_TYPE_SECURE        3
#define HLP_MODEL_MA_ENTRY_TYPE_STATIC        4
#define HLP_MODEL_MA_ENTRY_TYPE_SECURE_STATIC 5

#define HLP_MODEL_SV_MOVE_DROP_RESERVED     0
#define HLP_MODEL_SV_MOVE_DROP_PORT         1
#define HLP_MODEL_SV_MOVE_DROP_ADDR         2
#define HLP_MODEL_SV_MOVE_DROP_STATIC       3

#define HLP_MODEL_LOG_TYPE_TRIG_LOG_ACTION  (1 << 0)
#define HLP_MODEL_LOG_TYPE_FFU              (1 << 1)
#define HLP_MODEL_LOG_TYPE_RESERVED_MAC     (1 << 2)
#define HLP_MODEL_LOG_TYPE_ARP_REDIRECT     (1 << 3)
#define HLP_MODEL_LOG_TYPE_ICMP             (1 << 4)
#define HLP_MODEL_LOG_TYPE_TTL_IP_MC        (1 << 5)
#define HLP_MODEL_LOG_TYPE_IP_UCST_L2_MCST  (1 << 7) /* EAC TBR */

/* Trap codes which forms lower 8-bit of CPU-glort. */
/* See RRC Bug 22835.  Changed twice to match RTL change 271047. */
/* cpuCode changed to match RTL (src/rtl/hlp/common/hlp_pkg.vh) : */
#define HLP_MODEL_CPU_CODE_FFU              0x0;
#define HLP_MODEL_CPU_CODE_RSVD_MAC         0x1;
#define HLP_MODEL_CPU_CODE_IGMP             0x2;
#define HLP_MODEL_CPU_CODE_ICMP             0x3;
#define HLP_MODEL_CPU_CODE_IP_OPTION        0x4;
#define HLP_MODEL_CPU_CODE_CPU_ADDRESS      0x5;
#define HLP_MODEL_CPU_CODE_MTU              0x6;
#define HLP_MODEL_CPU_CODE_TTL              0x7;
#define HLP_MODEL_CPU_CODE_MAX              0xF;
//#define HLP_MODEL_CPU_CODE_FFU              0x80;
//#define HLP_MODEL_CPU_CODE_RSVD_MAC         0x83;
//#define HLP_MODEL_CPU_CODE_IGMP             0x86;
//#define HLP_MODEL_CPU_CODE_ICMP             0x90;
//#define HLP_MODEL_CPU_CODE_IP_OPTION        0x91;
//#define HLP_MODEL_CPU_CODE_CPU_ADDRESS      0x92;
//#define HLP_MODEL_CPU_CODE_MTU              0x94;
//#define HLP_MODEL_CPU_CODE_TTL              0x96;
//#define HLP_MODEL_CPU_CODE_MAX              0xFF;

/* Frame types */
#define HLP_MODEL_ETYPE_IPv4                0x0800
#define HLP_MODEL_ETYPE_IPv6                0x86DD
#define HLP_MODEL_ETYPE_MAC_CONTROL         0x8808
#define HLP_MODEL_IPV6_OPTION_HOP_BY_HOP    0
#define HLP_MODEL_IPV6_OPTION_ROUTING       43
#define HLP_MODEL_IPV6_OPTION_FRAG          44
#define HLP_MODEL_IPV6_OPTION_DEST          60
#define HLP_MODEL_IPV6_OPTION_AUTH          51
#define HLP_MODEL_PROT_TCP                  6
#define HLP_MODEL_PROT_UDP                  17
#define HLP_MODEL_PROT_ICMPv4               1
#define HLP_MODEL_PROT_ICMPv6               58
#define HLP_MODEL_PROT_IGMP                 2

/* FFU mux selects */
#define HLP_FFU_SELECT_MAP_DIP_MAP_SIP 		0
#define HLP_FFU_SELECT_MAP_DMAC_MAP_SMAC 	1
#define HLP_FFU_SELECT_MAP_PROT_MAP_LENGTH 	2
#define HLP_FFU_SELECT_MAP_SRC_MAP_TYPE 	3
#define HLP_FFU_SELECT_USER 		        4
#define HLP_FFU_SELECT_FTYPE_SWPRI 		    5
#define HLP_FFU_SELECT_IPMISC 		        6
#define HLP_FFU_SELECT_TOS 		            7
#define HLP_FFU_SELECT_PROT 		        8
#define HLP_FFU_SELECT_TTL 		            9
#define HLP_FFU_SELECT_SRC_PORT 		    10
#define HLP_FFU_SELECT_VPRI_VID_11_8 		11
#define HLP_FFU_SELECT_VID_7_0 	            12
#define HLP_FFU_SELECT_RXTAG 	            13
#define HLP_FFU_SELECT_L2_DMAC_15_0 		14
#define HLP_FFU_SELECT_L2_DMAC_31_16 		15
#define HLP_FFU_SELECT_L2_DMAC_47_32 		16
#define HLP_FFU_SELECT_L2_SMAC_15_0 		17
#define HLP_FFU_SELECT_L2_SMAC_31_16 		18
#define HLP_FFU_SELECT_L2_SMAC_47_32 		19
#define HLP_FFU_SELECT_DGLORT 	            20
#define HLP_FFU_SELECT_SGLORT 	            21
#define HLP_FFU_SELECT_VPRI_VID 	        22
#define HLP_FFU_SELECT_VPRI2_VID2 	        23
#define HLP_FFU_SELECT_L2_TYPE 	            24
#define HLP_FFU_SELECT_L4_DST 	            25
#define HLP_FFU_SELECT_L4_SRC 	            26
#define HLP_FFU_SELECT_MAP_L4_DST 	        27
#define HLP_FFU_SELECT_MAP_L4_SRC 	        28
#define HLP_FFU_SELECT_L4A 	                29
#define HLP_FFU_SELECT_L4B 	                30
#define HLP_FFU_SELECT_L4C 	                31
#define HLP_FFU_SELECT_L4D 	                32
#define HLP_FFU_SELECT_MAP_VPRI1_VID1 		33
#define HLP_FFU_SELECT_L3_DIP_31_0 		    34
#define HLP_FFU_SELECT_L3_DIP_63_32 		35
#define HLP_FFU_SELECT_L3_DIP_95_64 		36
#define HLP_FFU_SELECT_L3_DIP_127_96 		37
#define HLP_FFU_SELECT_L3_SIP_31_0 		    38
#define HLP_FFU_SELECT_L3_SIP_63_32 		39
#define HLP_FFU_SELECT_L3_SIP_95_64 		40
#define HLP_FFU_SELECT_L3_SIP_127_96 		41

/* Next Hop defines */
#define HLP_ARP_ENTRY_GLORT_WIDTH            2
#define HLP_ARP_ENTRY_GLORT_l_DGLORT         0
#define HLP_ARP_ENTRY_GLORT_h_DGLORT         15
#define HLP_ARP_ENTRY_GLORT_b_markRouted     16

/* The number of banks in the MAC Address Table. */
#define HLP_MAC_ADDR_BANK_COUNT             6

/* The size of MA_TABLE tcam size*/
#define HLP_MA_TABLE_TCAM_SIZE              1024

/* The size of each bank in the MAC Address Table. */
#define HLP_MAC_ADDR_BANK_SIZE              8192 

#define MAX(x, y)                   ( ( (x) > (y) ) ? (x) : (y) )
#define MIN(x, y)                   ( ( (x) < (y) ) ? (x) : (y) )

/* Miscellaneous defines */
#define HLP_CM_APPLY_TC_TO_SMP_ENTRIES           8
//#define HLP_CM_APPLY_SWITCH_PRI_TO_TC_ENTRIES   16
//#define HLP_CM_APPLY_LOOPBACK_SUPPRESS_ENTRIES  24

#define HLP_MIRROR_PORT_NULL 63

/* Action Codes */
#define HLP_MODEL_ACTION_NORMAL             0   /* forwarded normally */
#define HLP_MODEL_ACTION_FLOOD              1   /* flooded due to unknown destination */
#define HLP_MODEL_ACTION_SPECIAL            2   /* forwarded with fixed destination mask */
#define HLP_MODEL_ACTION_DROP_PARSE         3   /* drop due to parse error */
#define HLP_MODEL_ACTION_DROP_PARITY        4   /* dropped due to parity error */
#define HLP_MODEL_ACTION_TRAP               5   /* trap spec. mcast add., MAC CTL & MAC REMAP CTL */
#define HLP_MODEL_ACTION_DROP_CONTROL       6   /* dropped pause frame */ 
#define HLP_MODEL_ACTION_DROP_STP           7   /* dropped due to spanning tree*/
#define HLP_MODEL_ACTION_DROP_SV            8   /* dropped due to security violation */
#define HLP_MODEL_ACTION_MARKER_ERROR_DROPS 9   /* marker packets dropped due to fatal errors */
#define HLP_MODEL_ACTION_DROP_IV            10   /* dropped due to vlan ingress violation */
#define HLP_MODEL_ACTION_DROP_EV            11   /* dropped due to vlan egress violation */
#define HLP_MODEL_ACTION_DROP_CAM           12
#define HLP_MODEL_ACTION_DROP_FFU           13   /* dropped due to FFU flag */
#define HLP_MODEL_ACTION_DROP_TRIG          14   /* dropped due to a trigger action */
#define HLP_MODEL_ACTION_DROP_L3_PYLD_LEN   15   /* Single segment frames dropped when the L3 payload length does not agree with the actual packet length */
#define HLP_MODEL_ACTION_DROP_POLICER       16   /* dropped due the policer_drop flag from pre. */
#define HLP_MODEL_ACTION_DROP_TTL           17  
#define HLP_MODEL_ACTION_DROP_CM_GLOBAL     18
#define HLP_MODEL_ACTION_DROP_CM_SMP0       19
#define HLP_MODEL_ACTION_DROP_CM_SMP1       20
#define HLP_MODEL_ACTION_DROP_CM_RX_HOG0    21
#define HLP_MODEL_ACTION_DROP_CM_RX_HOG1    22
#define HLP_MODEL_ACTION_DROP_CM_TX_HOG0    23
#define HLP_MODEL_ACTION_DROP_CM_TX_HOG1    24
#define HLP_MODEL_ACTION_DROP_FRAME_ERR     25
#define HLP_MODEL_ACTION_REDIRECT_TRIG      26   /* redirected due to trigger match */
#define HLP_MODEL_ACTION_DROP_DLF           27   /* drop due to flood control of DLF frames */
#define HLP_MODEL_ACTION_GLORT_FORWARDED    28  /* Glort forwarded. */
#define HLP_MODEL_ACTION_BANK5_OTHER_DROPS  29  /* Bank 5 Other Drops */
#define HLP_MODEL_ACTION_DROP_LOOPBACK      30  /* Drop due to port or VLAN reflection disabled. */
#define HLP_MODEL_ACTION_DROP_L4_CSUM       31  /* Single segment packet drops due to L4 checksum (UDP, TCP, SCTP) failures */

#define HLP_MODEL_AMASK_DROP_PERR                  ( FM_LITERAL_U64(1) <<  0  )
#define HLP_MODEL_AMASK_SPECIAL                    ( FM_LITERAL_U64(1) <<  1  )
#define HLP_MODEL_AMASK_DROP_PARSER_ERR            ( FM_LITERAL_U64(1) <<  2  )
#define HLP_MODEL_AMASK_TRAP_RESERVED_MAC          ( FM_LITERAL_U64(1) <<  3  )
#define HLP_MODEL_AMASK_TRAP_RESERVED_MAC_REMAP    ( FM_LITERAL_U64(1) <<  4  )
#define HLP_MODEL_AMASK_DROP_MAC_CTRL              ( FM_LITERAL_U64(1) <<  7  )
#define HLP_MODEL_AMASK_DROP_RESERVED_MAC          ( FM_LITERAL_U64(1) <<  8  )
#define HLP_MODEL_AMASK_DROP_SMAC                  ( FM_LITERAL_U64(1) <<  10 )
#define HLP_MODEL_AMASK_DROP_SEC_ADDR              ( FM_LITERAL_U64(1) <<  11 )
#define HLP_MODEL_AMASK_DROP_SEC_PORT              ( FM_LITERAL_U64(1) <<  12 )
#define HLP_MODEL_AMASK_DROP_STATIC_ADDR           ( FM_LITERAL_U64(1) <<  13 )
#define HLP_MODEL_AMASK_DROP_PROVISIONAL           ( FM_LITERAL_U64(1) <<  14 )
#define HLP_MODEL_AMASK_TRAP_CPU_ADDR              ( FM_LITERAL_U64(1) <<  15 )
#define HLP_MODEL_AMASK_DROP_IV                    ( FM_LITERAL_U64(1) <<  16 )
#define HLP_MODEL_AMASK_DROP_INGRESS_STP_NON_LEARN ( FM_LITERAL_U64(1) <<  17 )
#define HLP_MODEL_AMASK_DROP_INGRESS_STP_LEARN     ( FM_LITERAL_U64(1) <<  18 )
#define HLP_MODEL_AMASK_DROP_FFU                   ( FM_LITERAL_U64(1) <<  19 )
#define HLP_MODEL_AMASK_TRAP_FFU                   ( FM_LITERAL_U64(1) <<  20 )
#define HLP_MODEL_AMASK_TRAP_ICMP_TTL              ( FM_LITERAL_U64(1) <<  21 )
#define HLP_MODEL_AMASK_TRAP_IP_OPTION             ( FM_LITERAL_U64(1) <<  22 )
#define HLP_MODEL_AMASK_TRAP_MTU_VIO               ( FM_LITERAL_U64(1) <<  23 )
#define HLP_MODEL_AMASK_TRAP_IGMP                  ( FM_LITERAL_U64(1) <<  24 )
#define HLP_MODEL_AMASK_TRAP_TTL                   ( FM_LITERAL_U64(1) <<  25 )
#define HLP_MODEL_AMASK_DROP_TTL                   ( FM_LITERAL_U64(1) <<  26 )
#define HLP_MODEL_AMASK_DROP_DLF                   ( FM_LITERAL_U64(1) <<  27 )
#define HLP_MODEL_AMASK_DROP_CAM_MISS              ( FM_LITERAL_U64(1) <<  28 )
#define HLP_MODEL_AMASK_DROP_NULL_GLORTDEST        ( FM_LITERAL_U64(1) <<  29 )
#define HLP_MODEL_AMASK_DROP_EV                    ( FM_LITERAL_U64(1) <<  30 )
#define HLP_MODEL_AMASK_DROP_EGRESS_STP            ( FM_LITERAL_U64(1) <<  32 )
#define HLP_MODEL_AMASK_DROP_LOOPBACK              ( FM_LITERAL_U64(1) <<  33 )
#define HLP_MODEL_AMASK_GLORT                      ( FM_LITERAL_U64(1) <<  34 )
#define HLP_MODEL_AMASK_FLOOD                      ( FM_LITERAL_U64(1) <<  35 )
#define HLP_MODEL_AMASK_SWITCH_RESERVED_MAC        ( FM_LITERAL_U64(1) <<  36 )
#define HLP_MODEL_AMASK_FORWARD_NORMAL             ( FM_LITERAL_U64(1) <<  37 )
#define HLP_MODEL_AMASK_LOG_INGRESS_FFU            ( FM_LITERAL_U64(1) <<  38 )
#define HLP_MODEL_AMASK_LOG_MAC_CTRL               ( FM_LITERAL_U64(1) <<  39 )
#define HLP_MODEL_AMASK_LOG_ARP_REDIRECT           ( FM_LITERAL_U64(1) <<  40 )
#define HLP_MODEL_AMASK_LOG_IP_ICMP                ( FM_LITERAL_U64(1) <<  41 )
#define HLP_MODEL_AMASK_LOG_IP_TTL                 ( FM_LITERAL_U64(1) <<  42 )
#define HLP_MODEL_AMASK_MIRROR_INGRESS_FFU         ( FM_LITERAL_U64(1) <<  43 )

/* Metadata */
#define HLP_MODEL_META_TYPE_OFF       0
#define HLP_MODEL_META_IP_HDR_OFF     11
#define HLP_MODEL_META_ESP_HDR_OFF    15

#define HLP_MODEL_META_TYPE_LAN_RX    0x0
#define HLP_MODEL_META_TYPE_LAN_TX    0x1
#define HLP_MODEL_META_TYPE_MARKER    0x2
#define HLP_MODEL_META_TYPE_DSI_RX    0x14
#define HLP_MODEL_META_TYPE_DSI_TX    0x16
#define HLP_MODEL_META_TYPE_RIMMON_RX 0x18

typedef enum
{
    HLP_MODEL_PACKET_TOO_SHORT = 0x1,
    HLP_MODEL_PACKET_TOO_LONG = 0x2,
    HLP_MODEL_PACKET_BAD_FCS = 0x4

} hlp_modelRxFlags;

typedef enum
{
    HLP_MODEL_ISL_NONE = 0,
    HLP_MODEL_ISL_F32,
    HLP_MODEL_ISL_F64,
    HLP_MODEL_ISL_F96,
    HLP_MODEL_ISL_X32,
    HLP_MODEL_ISL_X64,
    HLP_MODEL_ISL_X96

} hlp_modelIslTag;

typedef enum
{
    HLP_MODEL_FTYPE_NORMAL = 0,
    HLP_MODEL_FTYPE_ROUTED,
    HLP_MODEL_FTYPE_SPECIAL,
    HLP_MODEL_FTYPE_MGMT

} hlp_modelFType;

typedef enum
{
    HLP_MODEL_ARP_TYPE_GLORT = 0,
    HLP_MODEL_ARP_TYPE_MAC = 1
} hlp_modelArpEntryType;

typedef enum
{
    HLP_MODEL_STP_STATE_DISABLE = 0,
    HLP_MODEL_STP_STATE_LISTENING,
    HLP_MODEL_STP_STATE_LEARNING,
    HLP_MODEL_STP_STATE_FORWARD

} hlp_modelSTPState;

typedef enum
{
    HLP_MODEL_TCN_ENTRY_NEW_MAC = 0,
    HLP_MODEL_TCN_ENTRY_MAC_MOVED

} hlp_modelTcnType;

typedef enum
{
    HLP_PA_FLAGS_NOP            = 0,
    HLP_PA_FLAGS_GENERAL_0      = 1,
    HLP_PA_FLAGS_SUP_L4_CSUM_VAL= 3,
    HLP_PA_FLAGS_OTR_L4_UDP_V   = 4,
    HLP_PA_FLAGS_OTR_L4_TCP_V   = 5,
    HLP_PA_FLAGS_OTR_L4_SCTP_V  = 6,
    HLP_PA_FLAGS_GENERAL_7      = 7,
    HLP_PA_FLAGS_OTR_ESP_V      = 8,
    HLP_PA_FLAGS_WINDOW_PARSE_V = 9,
    HLP_PA_FLAGS_OTR_HEAD_FRAG_V= 10,
    HLP_PA_FLAGS_OTR_PAYLOAD_FRAG_V= 11,
    HLP_PA_FLAGS_OTR_ESP_PROT   = 12,
    HLP_PA_FLAGS_OTR_NAT_T      = 13,
    HLP_PA_FLAGS_GENERAL_1      = 14,
    HLP_PA_FLAGS_OTR_L2_VLAN1   = 18,
    HLP_PA_FLAGS_OTR_L2_VLAN2   = 19,
    HLP_PA_FLAGS_OTR_L2_V2FIRST = 20,
    HLP_PA_FLAGS_OTR_MPLS_V     = 21,
    HLP_PA_FLAGS_OTR_L3_V       = 22,
    HLP_PA_FLAGS_OTR_L4_V       = 23,
    HLP_PA_FLAGS_GENERAL_24     = 24,
    HLP_PA_FLAGS_INR_L2_V       = 25,
    HLP_PA_FLAGS_INR_L2_VLAN1   = 26,
    HLP_PA_FLAGS_INR_L2_VLAN2   = 27,
    HLP_PA_FLAGS_INR_L2_V2FIRST = 28,
    HLP_PA_FLAGS_INR_MPLS_V     = 29,
    HLP_PA_FLAGS_INR_L3_V       = 30,
    HLP_PA_FLAGS_INR_L4_V       = 31,
    HLP_PA_FLAGS_OTR_OPT_FLAGS  = 32,
    HLP_PA_FLAGS_INR_OPT_FLAGS  = 38,
    HLP_PA_FLAGS_GENERAL_2      = 44

} hlp_modelParserFlagsIndex;

typedef enum
{
    HLP_PA_PTRS_NOP          = 0,
    HLP_PA_PTRS_OTR_MPLS_PTR = 1,
    HLP_PA_PTRS_OTR_L3_PTR   = 2,
    HLP_PA_PTRS_OTR_L4_PTR   = 3,
    HLP_PA_PTRS_INR_L2_PTR   = 4,
    HLP_PA_PTRS_INR_MPLS_PTR = 5,
    HLP_PA_PTRS_INR_L3_PTR   = 6,
    HLP_PA_PTRS_INR_L4_PTR   = 7,
    HLP_PA_PTRS_ESP_PTR      = 7 /* Same location as INR_L4 */

} hlp_modelParserPtrsIndex;

typedef enum
{
    HLP_MODEL_PA_INFO_OTR_L2      = 0,
    HLP_MODEL_PA_INFO_OTR_MPLS    = 1,
    HLP_MODEL_PA_INFO_OTR_L3      = 2,
    HLP_MODEL_PA_INFO_OTR_L4      = 3,
    HLP_MODEL_PA_INFO_INR_L2      = 4,
    HLP_MODEL_PA_INFO_INR_MPLS    = 5,
    HLP_MODEL_PA_INFO_INR_L3      = 6,
    HLP_MODEL_PA_INFO_INR_L4      = 7

} hlp_modelParserInfoIndex;

typedef enum
{
    HLP_MODEL_HEADER_SIZE_0       = 0,
    HLP_MODEL_HEADER_SIZE_14      = 14,
    HLP_MODEL_HEADER_SIZE_18      = 18,
    HLP_MODEL_HEADER_SIZE_22      = 22,
    HLP_MODEL_HEADER_SIZE_26      = 26,
    HLP_MODEL_HEADER_SIZE_30      = 30

} hlp_modelHeaderSize;

typedef enum
{
    HLP_MODEL_HEADER_LEN_ENCODE_0 = 0,
    HLP_MODEL_HEADER_LEN_ENCODE_1 = 1,
    HLP_MODEL_HEADER_LEN_ENCODE_2 = 2,
    HLP_MODEL_HEADER_LEN_ENCODE_3 = 3,
    HLP_MODEL_HEADER_LEN_ENCODE_4 = 4,
    HLP_MODEL_HEADER_LEN_ENCODE_5 = 5

} hlp_modelHeaderLenEncode;

typedef enum
{
    HLP_PA_KEYS_INNER_DMAC        = 0,
    HLP_PA_KEYS_INNER_SMAC        = 3,
    HLP_PA_KEYS_OUTER_DMAC        = 6,
    HLP_PA_KEYS_OUTER_SMAC        = 9,
    HLP_PA_KEYS_OUTER_ETYPE       = 12,
    HLP_PA_KEYS_OUTER_VLAN1       = 14,
    HLP_PA_KEYS_OUTER_VLAN2       = 15,
    HLP_PA_KEYS_OUTER_L4SRC       = 16,
    HLP_PA_KEYS_OUTER_L4DST       = 17,
    HLP_PA_KEYS_INNER_ETYPE       = 18,
    HLP_PA_KEYS_INNER_VLAN1       = 20,
    HLP_PA_KEYS_INNER_VLAN2       = 21,
    HLP_PA_KEYS_INNER_L4SRC       = 22,
    HLP_PA_KEYS_INNER_L4DST       = 23,
    HLP_PA_KEYS_MPLS              = 24,
    HLP_PA_KEYS_GENERAL           = 32,
    HLP_PA_KEYS_INNER_IP_HEADER   = 36,
    HLP_PA_KEYS_OUTER_IP_HEADER   = 42,
    HLP_PA_KEYS_OUTER_SIPDIP      = 48,
    HLP_PA_KEYS_INNER_SIPDIP      = 64
    //HLP_PA_KEYS_FTAG_0            = 80,
    //HLP_PA_KEYS_FTAG_1            = 81,
    //HLP_PA_KEYS_FTAG_2            = 82,
    //HLP_PA_KEYS_FTAG_3            = 83

} hlp_modelParseKeysIndex;

typedef enum
{
    HLP_MODEL_DEFAULT_TARGET_KEYS_L       = 0,
    HLP_MODEL_DEFAULT_TARGET_KEYS_H       = 79,
    HLP_MODEL_DEFAULT_TARGET_FORCE_KEYS_L = 80,
    HLP_MODEL_DEFAULT_TARGET_FORCE_KEYS_H = 95,
    HLP_MODEL_DEFAULT_TARGET_ACT24_L_L    = 96,
    HLP_MODEL_DEFAULT_TARGET_ACT24_L_H    = 111,
    HLP_MODEL_DEFAULT_TARGET_ACT24_U_L    = 112,
    HLP_MODEL_DEFAULT_TARGET_ACT24_U_H    = 127,
    HLP_MODEL_DEFAULT_TARGET_ACT4_4_L     = 128,
    HLP_MODEL_DEFAULT_TARGET_ACT4_4_H     = 159,
    HLP_MODEL_DEFAULT_TARGET_ACT4_2_L     = 160,
    HLP_MODEL_DEFAULT_TARGET_ACT4_2_H     = 191,
    HLP_MODEL_DEFAULT_TARGET_ACT4_1_L     = 192,
    HLP_MODEL_DEFAULT_TARGET_ACT4_1_H     = 223,
    HLP_MODEL_DEFAULT_TARGET_ACT1_FLAGS   = 224
} hlp_modelPortDefaultTarget;


typedef enum
{
    HLP_RE_KEYS_INNER_DMAC        = 0,
    HLP_RE_KEYS_INNER_SMAC        = 3,
    HLP_RE_KEYS_OUTER_DMAC        = 6,
    HLP_RE_KEYS_OUTER_SMAC        = 9,
    HLP_RE_KEYS_OUTER_ETYPE       = 12,
    HLP_RE_KEYS_OUTER_VLAN1       = 14,
    HLP_RE_KEYS_OUTER_VLAN2       = 15,
    HLP_RE_KEYS_OUTER_L4SRC       = 16,
    HLP_RE_KEYS_OUTER_L4DST       = 17,
    HLP_RE_KEYS_INNER_ETYPE       = 18,
    HLP_RE_KEYS_INNER_VLAN1       = 20,
    HLP_RE_KEYS_INNER_VLAN2       = 21,
    HLP_RE_KEYS_INNER_L4SRC       = 22,
    HLP_RE_KEYS_INNER_L4DST       = 23,
    HLP_RE_KEYS_MPLS              = 24,
    HLP_RE_KEYS_GENERAL_8B        = 32,
    HLP_RE_KEYS_INNER_IP_TTL_PROT = 36,
    HLP_RE_KEYS_INNER_IP_LEN      = 37,
    HLP_RE_KEYS_INNER_IP_DS_FLOW  = 38,
    HLP_RE_KEYS_INNER_IP_FLOW     = 39,
    HLP_RE_KEYS_IP_ISL0_MSB       = 40,
    HLP_RE_KEYS_IP_ISL0_LSB       = 41,
    HLP_RE_KEYS_OUTER_IP_TTL_PROT = 42,
    HLP_RE_KEYS_OUTER_IP_LEN      = 43,
    HLP_RE_KEYS_OUTER_IP_DS_FLOW  = 44,
    HLP_RE_KEYS_OUTER_IP_FLOW     = 45,
    HLP_RE_KEYS_SGLORT            = 46,
    HLP_RE_KEYS_DGLORT            = 47,
    HLP_RE_KEYS_OUTER_SIP         = 48,
    HLP_RE_KEYS_OUTER_DIP         = 56,
    HLP_RE_KEYS_INNER_SIP         = 64,
    HLP_RE_KEYS_INNER_DIP         = 72

} hlp_modelRealignKeysIndex;

typedef enum
{
    HLP_SELECT_FTAG_SWPRI         = 0,
    HLP_SELECT_VPRI               = 1,
    HLP_SELECT_DSCP_PRI           = 2,
    HLP_SELECT_MPLS_EXP           = 3

} hlp_modelSwitchPriIndex;

typedef enum
{
    HLP_MODEL_KEY16_OUTER_VLAN1     = 14,
    HLP_MODEL_KEY16_INNER_VLAN1     = 20,

    HLP_MODEL_KEY16_MPLS_LABEL1_0   = 24,
    HLP_MODEL_KEY16_MPLS_LABEL1_1   = 25,
    HLP_MODEL_KEY16_MPLS_LABEL2_0   = 26,
    HLP_MODEL_KEY16_MPLS_LABEL2_1   = 27,
    HLP_MODEL_KEY16_MPLS_LABEL3_0   = 28,
    HLP_MODEL_KEY16_MPLS_LABEL3_1   = 29,
    HLP_MODEL_KEY16_MPLS_LABEL4_0   = 30,
    HLP_MODEL_KEY16_MPLS_LABEL4_1   = 31
    
} hlp_modelFfuKey16;

typedef enum
{
    HLP_MODEL_KEY8_MPLS_LABEL5_0    = 0,
    HLP_MODEL_KEY8_MPLS_LABEL5_1    = 1,
    HLP_MODEL_KEY8_MPLS_LABEL5_2    = 2,
    HLP_MODEL_KEY8_MPLS_LABEL5_3    = 3,
    HLP_MODEL_KEY8_MPLS_LABEL6_0    = 4,
    HLP_MODEL_KEY8_MPLS_LABEL6_1    = 5,
    HLP_MODEL_KEY8_MPLS_LABEL6_2    = 6,
    HLP_MODEL_KEY8_MPLS_LABEL6_3    = 7,
    HLP_MODEL_KEY8_INNER_TTL        = 8,
    HLP_MODEL_KEY8_INNER_PROT       = 9,
    HLP_MODEL_KEY8_INNER_LEN        = 10,
    HLP_MODEL_KEY8_INNER_DS         = 12, 
    HLP_MODEL_KEY8_OUTER_TTL        = 20,
    HLP_MODEL_KEY8_OUTER_PROT       = 21,
    HLP_MODEL_KEY8_OUTER_LEN        = 22,
    HLP_MODEL_KEY8_OUTER_DS         = 24

} hlp_modelFfuKey8;

typedef enum
{
    HLP_MODEL_KEY32_OUTER_SIP_127_96    = 0,
    HLP_MODEL_KEY32_OUTER_SIP_95_64     = 1,
    HLP_MODEL_KEY32_OUTER_SIP_63_32     = 2,
    HLP_MODEL_KEY32_OUTER_SIP_31_0      = 3,
    HLP_MODEL_KEY32_OUTER_DIP_127_96    = 4,
    HLP_MODEL_KEY32_OUTER_DIP_95_64     = 5,
    HLP_MODEL_KEY32_OUTER_DIP_63_32     = 6,
    HLP_MODEL_KEY32_OUTER_DIP_31_0      = 7,
    HLP_MODEL_KEY32_INNER_SIP_127_96    = 8,
    HLP_MODEL_KEY32_INNER_SIP_95_64     = 9,
    HLP_MODEL_KEY32_INNER_SIP_63_32     = 10,
    HLP_MODEL_KEY32_INNER_SIP_31_0      = 11,
    HLP_MODEL_KEY32_INNER_DIP_127_96    = 12,
    HLP_MODEL_KEY32_INNER_DIP_95_64     = 13,
    HLP_MODEL_KEY32_INNER_DIP_63_32     = 14,
    HLP_MODEL_KEY32_INNER_DIP_31_0      = 15
} hlp_modelFfuKey32;

typedef enum
{
    HLP_MODEL_FFU_ACTION_NOP = 0,
    HLP_MODEL_FFU_ACTION_SET4_4B,
    HLP_MODEL_FFU_ACTION_SET8_1B,
    HLP_MODEL_FFU_ACTION_SET3_1B,
    HLP_MODEL_FFU_ACTION_SET3_4B,
    HLP_MODEL_FFU_ACTION_SET1_24B

} hlp_modelFfuActionEntryType;

typedef enum
{
    HLP_MODEL_FFU_ACTION_DROP = 0,
    HLP_MODEL_FFU_ACTION_TRAP,
    HLP_MODEL_FFU_ACTION_LOG,
    HLP_MODEL_FFU_ACTION_NO_ROUTE,
    HLP_MODEL_FFU_ACTION_RX_MIRROR,
    HLP_MODEL_FFU_ACTION_CAPT_TIME,
    HLP_MODEL_FFU_ACTION_TX_TAG0,
    HLP_MODEL_FFU_ACTION_TX_TAG1,
    HLP_MODEL_FFU_ACTION_TRIGGER0,
    HLP_MODEL_FFU_ACTION_TRIGGER1,
    HLP_MODEL_FFU_ACTION_TRIGGER2,
    HLP_MODEL_FFU_ACTION_TRIGGER3,
    HLP_MODEL_FFU_ACTION_TRIGGER4,
    HLP_MODEL_FFU_ACTION_TRIGGER5,
    HLP_MODEL_FFU_ACTION_TRIGGER6,
    HLP_MODEL_FFU_ACTION_TRIGGER7,
    HLP_MODEL_FFU_ACTION_SCENARIO0,
    HLP_MODEL_FFU_ACTION_SCENARIO1,
    HLP_MODEL_FFU_ACTION_SCENARIO2,
    HLP_MODEL_FFU_ACTION_SCENARIO3,
    HLP_MODEL_FFU_ACTION_SCENARIO4,
    HLP_MODEL_FFU_ACTION_SCENARIO5,
    HLP_MODEL_FFU_ACTION_LEARN,
    HLP_MODEL_FFU_ACTION_COPY_OTR_VPRI

} hlp_modelFfuAct1;

typedef enum
{
    HLP_MODEL_FFU_ACTION_DSCP_CTRL              = 0,
    HLP_MODEL_FFU_ACTION_TTL_CTRL               = 1,
    HLP_MODEL_FFU_ACTION_TC_CTRL                = 2,
    HLP_MODEL_FFU_ACTION_ECN_CTRL               = 3,
    HLP_MODEL_FFU_ACTION_VID_LOW                = 4,
    HLP_MODEL_FFU_ACTION_VID_MID                = 5,
    HLP_MODEL_FFU_ACTION_VID_HIGH               = 6,
    HLP_MODEL_FFU_ACTION_VPRI_LOW               = 7,
    HLP_MODEL_FFU_ACTION_DSCP_LOW               = 8,
    HLP_MODEL_FFU_ACTION_DSCP_HIGH              = 9,
    HLP_MODEL_FFU_ACTION_TC                     = 10,
    /* alternate VPRI address when combining DSCP/SWPRI_CTRL/VPRI actions */
    HLP_MODEL_FFU_ACTION_VPRI_HIGH              = 11,     
    HLP_MODEL_FFU_ACTION_HASH_PROFILE_ECMP_0    = 12,
    HLP_MODEL_FFU_ACTION_HASH_PROFILE_ECMP_1    = 13,
    HLP_MODEL_FFU_ACTION_HASH_PROFILE_MOD_0     = 14,
    HLP_MODEL_FFU_ACTION_HASH_PROFILE_MOD_1     = 15,
    HLP_MODEL_FFU_ACTION_META0_LOW              = 16,
    HLP_MODEL_FFU_ACTION_META0_HIGH             = 17,
    HLP_MODEL_FFU_ACTION_META1_LOW              = 18,
    HLP_MODEL_FFU_ACTION_META1_HIGH             = 19,
    HLP_MODEL_FFU_ACTION_META2_LOW              = 20,
    HLP_MODEL_FFU_ACTION_META2_HIGH             = 21,
    HLP_MODEL_FFU_ACTION_MPLS_POP               = 22
} hlp_modelFfuAct4;

typedef enum
{
    HLP_MODEL_FFU_ACTION_POLICER0 = 0,
    HLP_MODEL_FFU_ACTION_POLICER1,
    HLP_MODEL_FFU_ACTION_POLICER2,
    HLP_MODEL_FFU_ACTION_POLICER3,
    HLP_MODEL_FFU_ACTION_FWD,
    HLP_MODEL_FFU_ACTION_MOD_IDX,
    HLP_MODEL_FFU_ACTION_REMAP0,
    HLP_MODEL_FFU_ACTION_REMAP1,
    HLP_MODEL_FFU_ACTION_REMAP2,
    HLP_MODEL_FFU_ACTION_REMAP3,
    HLP_MODEL_FFU_ACTION_REMAP4,
    HLP_MODEL_FFU_ACTION_REMAP5,
    HLP_MODEL_FFU_ACTION_REMAP6,
    HLP_MODEL_FFU_ACTION_REMAP7
} hlp_modelFfuAct24;

typedef enum
{
    HLP_MODEL_BIT_MASK_MPLS0 = 0,
    HLP_MODEL_BIT_MASK_MPLS1,
    HLP_MODEL_NIBBLE_MASK_MPLS,
    HLP_MODEL_NIBBLE_MASK_IP

} hlp_modelFfuKeySubMode;

typedef enum 
{
    HLP_MODEL_FFU_HASH_CRC32 = 0,
    HLP_MODEL_FFU_HASH_CRC32K,
    HLP_MODEL_FFU_HASH_CRC32C,
    HLP_MODEL_FFU_HASH_CRC32Q

} hlp_modelFfuHashSelect;

typedef enum 
{
    HLP_MODEL_FFU_HASH_ENTRY_SIZE_8B = 0,
    HLP_MODEL_FFU_HASH_ENTRY_SIZE_16B,
    HLP_MODEL_FFU_HASH_ENTRY_SIZE_32B,
    HLP_MODEL_FFU_HASH_ENTRY_SIZE_64B
    
} hlp_modelFfuHashEntrySize;

typedef enum
{
    HLP_MODEL_NORMAL_TAGGING = 0,
    HLP_MODEL_INSERT,
    HLP_MODEL_DELETE,
    HLP_MODEL_UPDATE_ADD

} hlp_modelTxTag;

typedef enum
{
    HLP_MODEL_FCLASS_UNICAST = 0,
    HLP_MODEL_FCLASS_MULTICAST,
    HLP_MODEL_FCLASS_BROADCAST

} hlp_modelFclassType;

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
    STAT_RxBcstPktsIPv6
} hlp_modelRxStatsBk0;

typedef enum
{
    STAT_RxTC0 = 0,
    STAT_RxTC1,
    STAT_RxTC2,
    STAT_RxTC3,
    STAT_RxTC4,
    STAT_RxTC5,
    STAT_RxTC6,
    STAT_RxTC7
} hlp_modelRxStatsBk1;

typedef enum
{
    STAT_FIDForwarded = 0,
    STAT_FloodForwarded,
    STAT_TargetedDeterministicForwarded,
    STAT_ParseErrDrops,
    STAT_ParityErrorDrops,       /* Note:  Was parity error. */
    STAT_Trapped,
    STAT_CtrlDrops,
    STAT_STPDrops,
    STAT_SecurityViolations,
    STAT_MarkerErrorDrops,
    STAT_VlanIngressDrops,
    STAT_VlanEgressDrops,
    STAT_GlortMissDrops,
    STAT_FFUDrops,
    STAT_TriggerDrops,
    STAT_L3PayloadLengthValidationDrops

} hlp_modelRxStatsBk2;

typedef enum
{
    STAT_PolicerDrops = 0,
    STAT_TTLDrops,
    STAT_CMGlobalDrops,
    STAT_SMP0Drops,
    STAT_SMP1Drops,
    STAT_RXHog0Drops,
    STAT_RXHog1Drops,
    STAT_TXHog0Drops,
    STAT_TXHog1Drops,
    STAT_FrameErrorDrops,
    STAT_TriggerRedirects,
    STAT_FloodControlDrops,
    STAT_GlortForwarded,
    STAT_OtherDrops,
    STAT_LoopbackSuppDrops,
    STAT_L4CheckSumValidationDrops

} hlp_modelRxStatsBk3;

typedef enum
{
    TM_NOP = 0,
    TM_DECAP_LEAVE_IN_PLACE,
    TM_DECAP,
    TM_ENCAP_GENEVE,
    TM_ENCAP_GRE,
    TM_ENCAP_VXLAN,
    TM_ENCAP_VXLAN_GPE,
    TM_ENCAP_VXLAN_GPE_NSH,
    TM_GENERIC_DESC,
    TM_GENERIC_ALU_LEN

} hlp_tunnelMode;

/**************************************************
 * Register Structures. These structures
 * mimic the equivalent registers as defined
 * in the HLP datasheet.
 **************************************************/

typedef struct _hlp_modelFfuTcam
{
  fm_uint64         KeyInvert; // 40b field
  fm_uint64         Key; // 40b field
  fm_bool           Valid;
} hlp_modelFfuTcam;


typedef struct _hlp_modelFfuKeyMaskCfg
{
  fm_uint32         Key16Mask;
  fm_uint64         Key8Mask;
  fm_uint16         Key32Mask;
  fm_byte           KeySubmode[2]; // 2b field
  fm_uint32         KeySubmask[2];

} hlp_modelFfuKeyMaskCfg;

typedef struct _hlp_modelFfuHashCfg
{
  fm_bool           mode;
  fm_uint16         base_ptr[2]; // 13b field
  fm_byte           hash_size[2]; // 5b field
  fm_byte           entry_size[2]; // 5b field
} hlp_modelFfuHashCfg;

/*
typedef struct _hlp_modelMaTcnDequeue
{
  hlpMaTcnFifo      fifoEntry;
  fm_bool           Valid;

} hlp_modelMaTcnDequeue;

typedef struct _hlp_modelIngressMstTable
{
    fm_uint64       STPState;

} hlp_modelIngressMstTable;
*/

typedef struct _hlp_modelArpTable
{

    hlp_modelArpEntryType   EntryType; // 1 = MAC entry, 0 - Glort Entry
    fm_macaddr              DMAC; // used for DMAC entries
    fm_uint16               DGLORT; // used for GLORT entries
    fm_byte                 MTU_Index; // used for GLORT entries
    fm_bool                 markRouted; // used for GLORT entries
    fm_bool                 IPv6Entry; // used for GLORT entries
    fm_uint16               EVID;
    fm_byte                 L3Domain;
    fm_uint16               L2Domain;
    fm_bool                 UpdateL3Domain;
    fm_bool                 UpdateL2Domain;
    fm_uint32               ModIdx;

} hlp_modelArpTable;

/***************************************************
 * The following structures are helper structures
 * for modelling the HLP chip.
 **************************************************/

/* Structure used to store parser info */
typedef struct _hlp_modelParserInfo
{
    fm_byte    otr_l2_len; // 3b field 
    fm_bool    otr_l2_vlan1;
    fm_bool    otr_l2_vlan2;
    fm_bool    otr_l2_v2first;
    fm_byte    otr_mpls_len; // 3b field
    fm_byte    otr_l3_len; // 4b field
    fm_bool    otr_l3_v6;
    fm_bool    otr_l4_udp;
    fm_bool    otr_l4_tcp;
    fm_byte    otr_tun_len; // 5b field
    fm_byte    inr_l2_len; // 3b field
    fm_bool    inr_l2_vlan1;
    fm_bool    inr_l2_vlan2;
    fm_bool    inr_l2_v2first;
    fm_byte    inr_mpls_len; // 3b field
    fm_byte    inr_l3_len; // 4b field
    fm_bool    inr_l3_v6;
    fm_bool    inr_l4_udp;
    fm_bool    inr_l4_tcp;  
    fm_bool    window_parse_v;  

} hlp_modelParserInfo;

/* Structure used in hlp_modelFwdKeys */
typedef struct _hlp_modelIppRxTag
{
    fm_bool    custom; 
    fm_bool    mpls;   
    fm_bool    ipv6;   
    fm_bool    ipv4;   
    fm_bool    v2first;
    fm_bool    vlan2;
    fm_bool    vlan1; 

} hlp_modelRxTag;

typedef struct _hlp_modelFfuFlags
{   
    fm_bool     drop;
    fm_bool     trap;
    fm_bool     log;
    fm_bool     no_route;
    fm_bool     rx_mirror;
    fm_bool     capture_time;
    fm_byte     tx_tag;

} hlp_modelFfuFlags;

/* Structure used to store different fields of key used in MAP */
typedef struct _hlp_modelMapKey
{
    fm_byte   MAP_OUTER_PROT;  // 4b field
    fm_byte   MAP_OUTER_ETYPE; // 4b field
    fm_byte   MAP_INNER_PROT;  // 4b field
    fm_byte   MAP_INNER_ETYPE; // 4b field
    fm_byte   MAP_OUTER_DMAC;
    fm_byte   MAP_OUTER_SMAC;
    fm_byte   MAP_INNER_DMAC;
    fm_byte   MAP_INNER_SMAC;
    fm_byte   MAP_OUTER_DIP;  // 4b field
    fm_byte   MAP_OUTER_SIP;  // 4b field
    fm_byte   MAP_INNER_DIP;  // 4b field
    fm_byte   MAP_INNER_SIP;  // 4b field
    fm_byte   MAP_PORT;       // 4b field
    //fm_byte   MAP_OUTER_L3_LENGTH;
    //fm_byte   MAP_INNER_PORT; //duplicated name in HAS
    //fm_byte   MAP_INNER_L3_LENGTH;
    //fm_uint16 MAP_VLAN1;
    //fm_uint16 MAP_VLAN2;
    fm_uint16 MAP_OUTER_L4_DST;
    fm_uint16 MAP_OUTER_L4_SRC;
    fm_uint16 MAP_INNER_L4_DST;
    fm_uint16 MAP_INNER_L4_SRC;
   // fm_bool   Router;
   // fm_bool   Routable;

} hlp_modelMapKey;

typedef struct _hlp_modelFfuKeys
{
    fm_uint32       key32[HLP_MODEL_FFU_N_KEY32];
    fm_uint16       key16[HLP_MODEL_FFU_N_KEY16];
    fm_byte         key8[HLP_MODEL_FFU_N_KEY8];

} hlp_modelFfuKeys;

typedef struct _hlp_modelPrecVal
{
    fm_byte         prec; // 3b field
    // act24.val is 24b, act4.val is 4b, act1.val is 1b
    fm_uint32       val;  

} hlp_modelPrecVal;

typedef struct _hlp_modelFfuActions 
{
    hlp_modelPrecVal act24[HLP_MODEL_FFU_N_ACT24];
    hlp_modelPrecVal act4[HLP_MODEL_FFU_N_ACT4];
    hlp_modelPrecVal act1[HLP_MODEL_FFU_N_ACT1];

} hlp_modelFfuActions;

typedef struct _hlp_modelFghashActions 
{
    fm_bool   actionValid[HLP_MODEL_FFU_MAX_HASH_ACTIONS];
    fm_uint32 action[HLP_MODEL_FFU_MAX_HASH_ACTIONS];
} hlp_modelFghashActions;

/* Slice Result */
typedef struct _hlp_modelLookupInfo
{
    fm_uint64 key; // 40b field
    fm_uint64 keyInvert; // 40b field
    fm_bool   rawHits[HLP_FFU_TCAM_ENTRIES_0];
    fm_int    hitIndex;
    fm_bool   hitIndexValid;

} hlp_modelLookupInfo;

typedef struct _hlp_modelFfuHitInfo
{
    fm_int    hitIndex;
    fm_bool   hitIndexValid;

} hlp_modelFfuHitInfo;

typedef struct _hlp_modelFfuMuxedAction
{    
    fm_byte     ecn;
    fm_bool     aqm_mark_en;
    fm_byte     swpri;
    fm_byte     ttl_ctrl;
    fm_byte     ttl01;
    fm_byte     dscp;
    fm_byte     vpri;
    fm_bool     route;

} hlp_modelFfuMuxedAction;

typedef struct _hlp_modelBaseCmd
{
	fm_byte preload_cmd;
	fm_bool otr1_cmd;
	fm_bool otr2_cmd;
	fm_bool mpls_cmd;
	fm_bool qos_cmd;
	fm_bool inr_cmd;
	fm_bool meta_cmd[3];
	fm_bool otr_dmac;
	fm_bool otr_smac;
	fm_bool dglort;
	fm_bool reserved;

} hlp_modelBaseCmd;

typedef struct _hlp_modelOtr1Cmd
{
	fm_bool otr_sip;
	fm_bool otr_dip;
	fm_bool otr_ipv6;
	fm_bool otr_ipflo;
	fm_bool otr_prot;
	fm_bool otr_l4src;
	fm_bool otr_l4dst;
	fm_bool otr_dip_mac;
	fm_byte tunnel_mode;
	fm_byte tunnel_data;

} hlp_modelOtr1Cmd;

typedef struct _hlp_modelOtr2Cmd
{
	fm_uint16 tunnel_data;
	fm_byte reserved;

} hlp_modelOtr2Cmd;

typedef struct _hlp_modelMPLSCmd
{
	fm_byte mpls_pop;
	fm_byte mpls_push;
	fm_bool pop_al;
	fm_bool pop_eli;
	fm_bool push_al;
	fm_bool push_eli;
	fm_byte otr_tag_mode;
	fm_byte otr_tag_size;

} hlp_modelMPLSCmd;

typedef struct _hlp_modelQOSCmd
{
	fm_byte ds_src;
	fm_byte ttl_src;
	fm_byte ttl_dec;
	fm_byte ttlds_tgt;
	fm_bool ds_wr;
	fm_bool ttl_wr;
    fm_byte aqm_mark_ctrl; 
	fm_bool reserved;

} hlp_modelQOSCmd;

typedef struct _hlp_modelInrCmd
{
	fm_bool vni;
	fm_bool inr_dmac;
	fm_bool inr_smac;
	fm_bool inr_sip;
	fm_bool inr_dip;
	fm_bool inr_ipv6;
	fm_bool inr_dip_mac;
	fm_bool inr_ds;
	fm_bool inr_ttl;
	fm_bool inr_l4src;
	fm_bool inr_l4dst;
	fm_byte inr_tag_mode;
	fm_byte inr_tag_size;

} hlp_modelInrCmd;

typedef struct _hlp_modelMetaCmd
{
	fm_byte source;
	fm_byte count;
	fm_uint16 dest;

} hlp_modelMetaCmd;

typedef struct _hlp_modelModDesc
{
	hlp_modelBaseCmd base_cmd;
	hlp_modelOtr1Cmd otr1_cmd;
	hlp_modelOtr2Cmd otr2_cmd;
	hlp_modelMPLSCmd mpls_cmd;
	hlp_modelQOSCmd qos_cmd;
	hlp_modelInrCmd inr_cmd;
	hlp_modelMetaCmd meta_cmd[3];
    fm_bool otr1_present;
    fm_bool otr1_ipv_present;
    fm_bool inr_ipv_present;
    fm_bool otr2_present;
    fm_bool mpls_present;
    fm_bool qos_present;
    fm_bool inr_present;
    fm_bool meta_present[3];
	fm_byte otr_dmac[6];
	fm_byte otr_smac[6];
	fm_byte dglort[2];
	fm_byte otr_sip[16];
	fm_byte otr_dip[16];
	fm_byte otr_flow[4];
	fm_byte otr_prot[2];
	fm_byte otr_l4src[2];
	fm_byte otr_l4dst[2];
    fm_byte tunnel_data[72];
    fm_byte mpls[16];
    fm_byte al[4];
    fm_byte otr_tags[8];
    fm_byte otr_ds;
    fm_byte otr_ttl;
    fm_byte vni[4];
    fm_byte inr_dmac[6];
    fm_byte inr_smac[6];
    fm_byte inr_sip[16];
    fm_byte inr_dip[16];
    fm_byte inr_ds;
    fm_byte inr_ttl;
    fm_byte inr_l4src[2];
    fm_byte inr_l4dst[2];
    fm_byte inr_tags[8];
} __attribute__((packed)) hlp_modelModDesc;

typedef struct _hlp_modelChunkedSeg
{
	/* Outer L2 + Ethertype */
	fm_byte ftag[8];
	fm_byte otr_dmac[6];
	fm_byte otr_smac[6];
	fm_byte otr_tags[16];
	fm_byte otr_et[2];
	fm_bool ftag_v;
	fm_byte n_otr_tag;

	/* Outer MPLS */
	fm_byte otr_mpls[28];
	fm_byte n_otr_mpls;
	fm_byte n_otr_mpls_pre;

	/* Outer IP */
	fm_byte otr_ip[56];
	fm_byte otr_ip_size;
    fm_bool otr_l3_v6;

	/* UDP/Tunnel part 1 */
	fm_byte otr_l4[40];
	fm_bool otr_udp_v;
    fm_bool otr_tcp_v;
	fm_byte tun_size_in_l4_chunk;

	/* Tunnel part 2 */
	fm_byte tun_opt[40];
	fm_byte tun_opt_size;

	/* Inner L2 (+Ether Type) */
	fm_byte inr_dmac[6];
	fm_byte inr_smac[6];
	fm_byte inr_tags[16];
	fm_byte inr_et[2];
	fm_bool inr_l2_v;
	fm_byte n_inr_tag;

	/* Inner MPLS */
	fm_byte inr_mpls[28];
	fm_byte n_inr_mpls;

	/* Inner IP */
	fm_byte inr_ip[56];
	fm_byte inr_ip_size;
    fm_bool inr_l3_v6;

	/* Inner L4 */
	fm_byte inr_l4[18];
	fm_bool inr_udp_v;
    fm_bool inr_tcp_v;

	/* Payload */
	fm_byte payload_start;
	fm_int  payload_size;

} hlp_modelChunkedSeg;

typedef struct _hlp_modelModRegData
{
    hlpModPerPortCfg1   modPerPortCfg1;
    hlpModPerPortCfg2   modPerPortCfg2;
    hlpModAqmProfile    modAqmProfile;

} hlp_modelModRegData;

typedef struct _hlp_modelMplsData
{
    fm_byte     otr_mpls_type[7];
    fm_byte     n_otr_g;
    fm_bool     otr_eli_v;
    fm_byte     otr_eli_idx;
    fm_bool     otr_al_v;

    fm_byte     inr_mpls_type[7];
    fm_byte     n_inr_g;
    fm_bool     inr_eli_v;
    fm_byte     inr_eli_idx;
    fm_bool     inr_al_v;
    fm_byte     inr_bos_idx;

    fm_byte     mplsPopCount;
    fm_byte     mplsPushCount;
    fm_bool     mplsPushAl;
    fm_bool     mplsPushEli;
    fm_bool     mplsPopEli;
    fm_bool     mplsPopAl;
    fm_int      legalDepthIdx;

    fm_byte     last_label_poped[4];
} hlp_modelMplsData;

typedef struct _hlp_modelModControlData
{
    fm_byte dvStatus;
    fm_bool isMarkerPkt;
    fm_bool isWindowParsing;
    /* Mirror Lookup */
    fm_bool isMirror;
    fm_bool mirrorTrunc;
    /* Multicast Lookup */
    fm_uint16 evidA;
    fm_uint16 dglortA;
    fm_uint16 txDglort;
    /* Loopback Suppress */
    fm_bool vlanSwitched;
    fm_bool routeA;
    fm_bool loopbackSuppressDrop;

    fm_int  rx_n_tag;
    fm_byte rx_tags[16];
    /* MPLS */
    fm_bool isInterLSR;
    hlp_modelMplsData mplsData;
    /* L3 */
    fm_uint32 l3Idx;
    fm_uint32 l4Idx;
    fm_bool otrL3Modified;
    fm_bool isRoutable;
    /* DS transformation */
    fm_byte internalDS;
    fm_byte egressDSCP;
    fm_byte otrTTL;
    fm_bool skipDscp;
    fm_bool skipTtl;
    /* L4 */
    fm_bool otrL4Modified;
    /* Routing */
    fm_uint16 l3_domain;
    /* Priority Profile*/
    fm_uint16 operator_id;
    fm_byte priority_profile;
    /* Mod Descriptor */
    fm_uint32 modIdx;
    fm_bool encap;
    fm_bool decap;
    /* Vlan data */
    fm_byte numVlans;
    fm_bool rxVlan1;
    fm_bool rxVlan2;
    fm_bool rxV2first;
    fm_bool txVlan1;
    fm_bool txVlan2;
    fm_bool preserveVlan1;
    fm_bool preserveVlan2;
    fm_byte txVpri1;
    fm_byte txVpri2;
    fm_uint16 txVid1;
    fm_uint16 txVid2;
    fm_uint16 evidB;
    fm_bool ecn_tx_drop;
    fm_bool timeout_tx_drop;
    fm_bool non_cm_tx_drop;
    fm_bool cancel_drop_on_marker;
    fm_byte cancelled_tx_disp;
    fm_bool ecn_mark;

    /* Stats data */
    fm_byte                 bytesAdded;
    fm_uint32               egressSeg0Bytes;
    fm_uint32               crc_ingress_diff;
    fm_uint32               crc_egress;
    fm_uint32               refcnt_tx_len;
    fm_uint32               refcntSeg0Bytes;
    
    fm_uint32               tail_len;
    /* intr data */
    fm_bool                 intr_occured;
    fm_uint64               mod_im;

    fm_uint16               igL3TotalLen;
} hlp_modelModControlData;

/***************************************************
 * HASH related types
 **************************************************/

typedef struct _hlp_modelHashKeys
{
    fm_uint64 crc34;
    fm_uint64 crc234;
    fm_byte l234Key[17];
    fm_bool zeroL2;
    fm_bool zeroL34;
    fm_bool useL34;
    fm_byte rotA;
    fm_byte rotB;
    fm_bool arpEcmpCfg;

} hlp_modelHashKeys;

/***************************************************
 * TRIGGERS related types
 **************************************************/

typedef enum
{
    HLP_MODEL_TRIG_ACTION_FORWARDING_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_FORWARDING_FORWARD,
    HLP_MODEL_TRIG_ACTION_FORWARDING_REDIRECT,
    HLP_MODEL_TRIG_ACTION_FORWARDING_DROP
} hlp_modelTriggerActionForwarding;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_TRAP_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_TRAP_TRAP,
    HLP_MODEL_TRIG_ACTION_TRAP_LOG,
    HLP_MODEL_TRIG_ACTION_TRAP_REVERT
} hlp_modelTriggerActionTrap;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_MIRRORING_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_MIRRORING_MIRROR,
    HLP_MODEL_TRIG_ACTION_MIRRORING_CANCEL
} hlp_modelTriggerActionMirroring;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_TC_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_TC_REASSIGN
} hlp_modelTriggerActionTC;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_VLAN_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_VLAN_REASSIGN
} hlp_modelTriggerActionVlan;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_LEARNING_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_LEARNING_DONT_LEARN,
    HLP_MODEL_TRIG_ACTION_LEARNING_FORCE_LEARN
} hlp_modelTriggerActionLearning;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_RATE_LIMIT_AS_IS = 0,
    HLP_MODEL_TRIG_ACTION_RATE_LIMIT_APPLY
} hlp_modelTriggerActionRateLimit;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_METADATA_NONE = 0,
	HLP_MODEL_TRIG_ACTION_METADATA_MASKED_16_SET,
	HLP_MODEL_TRIG_ACTION_METADATA_MASKED_16_COPY
} hlp_modelTriggerActionMetadata;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_EGRESS_L2DOMAIN_ALWAYS_UPDATE = 0,
	HLP_MODEL_TRIG_ACTION_EGRESS_L2DOMAIN_SKIP
} hlp_modelTriggerActionEgressL2Domain;

typedef enum
{
	HLP_MODEL_TRIG_ACTION_EGRESS_L3DOMAIN_ALWAYS_UPDATE = 0,
	HLP_MODEL_TRIG_ACTION_EGRESS_L3DOMAIN_SKIP
} hlp_modelTriggerActionEgressL3Domain;

typedef enum
{
	HLP_MODEL_TRIG_ACTION_POLICER_NONE = 0,
	HLP_MODEL_TRIG_ACTION_POLICER_DO_NOT_POLICE
} hlp_modelTriggerActionPolicer;

typedef enum
{
	HLP_MODEL_TRIG_ACTION_NOMODIFY_NONE = 0,
	HLP_MODEL_TRIG_ACTION_NOMODIFY_DO_NOT_MODIFY
} hlp_modelTriggerActionNoModify;

typedef enum
{
    HLP_MODEL_TRIG_ACTION_TYPE_FORWARD = 0,
    HLP_MODEL_TRIG_ACTION_TYPE_TRAP,
    HLP_MODEL_TRIG_ACTION_TYPE_MIRROR,
    HLP_MODEL_TRIG_ACTION_TYPE_PRI,
    HLP_MODEL_TRIG_ACTION_TYPE_VLAN,
    HLP_MODEL_TRIG_ACTION_TYPE_LEARN,
    HLP_MODEL_TRIG_ACTION_TYPE_RATE

} hlp_modelTriggerActionType;

typedef enum
{
    HLP_MODEL_MIRTYPE_NORMAL = 0,
    HLP_MODEL_MIRTYPE_MIR0,
    HLP_MODEL_MIRTYPE_MIR1,

} hlp_modelMirTyp;

typedef enum
{
    HLP_MODEL_AQM_PROFILE_MODE_DISABLED = 0,
    HLP_MODEL_AQM_PROFILE_MODE_RED = 1,
    HLP_MODEL_AQM_PROFILE_MODE_DCTCP = 2,

} hlp_modelAqmProfileMode;

typedef struct _hlp_modelTriggerActions
{
    /** TRIGGERS forwarding action. */
    hlp_modelTriggerActionForwarding    forwardingAction;
    fm_uint16                           newDestGlort;
    fm_uint16                           newDestGlortMask;
    fm_uint64                           newDestMask;
    fm_bool                             filterDestMask;
    fm_uint64                           dropMask;

    /** TRIGGERS trap action. */
    hlp_modelTriggerActionTrap          trapAction;
    fm_byte                             cpuCode;
    fm_byte                             trapCode;

    /** TRIGGERS mirroring action. */
    hlp_modelTriggerActionMirroring     mirroringAction0;
    hlp_modelTriggerActionMirroring     mirroringAction1;

    fm_byte                             mirrorProfileIndex0;
    fm_byte                             mirrorProfileIndex1;

    /** TRIGGERS ISL switch priority action. */
    hlp_modelTriggerActionTC            TCAction;
    fm_byte                             newTC;

    /** TRIGGERS VLAN action. */
    hlp_modelTriggerActionVlan          vlanAction;
    fm_uint16                           newVlan;

    /** TRIGGERS learning action. */
    hlp_modelTriggerActionLearning      learningAction;

    /** TRIGGERS rate limiter action */
    hlp_modelTriggerActionRateLimit     rateLimitAction;
    fm_byte                             newRateLimitNum; 

    /** TRIGGERS metadata action */
    fm_byte                             metadataActionSlot;
    hlp_modelTriggerActionMetadata      metadataAction[4];
    fm_uint16                           metadataMask[4];
    fm_uint16                           metadataValue[4];
    fm_byte                             metadataOffset[4];
    fm_byte                             metadataSource[4];
    fm_int                              metadataTrigNum[4];

    /** TRIGGERS l2 domain action */
	hlp_modelTriggerActionEgressL2Domain egressL2DomainAction;
    hlp_modelTriggerActionEgressL3Domain egressL3DomainAction;

    /** TRIGGERS policer action */
	hlp_modelTriggerActionPolicer       policerAction;

    /** TRIGGERS noModify action */     
    hlp_modelTriggerActionNoModify      noModifyAction;
    fm_byte                             metadataMaskSel;
    fm_uint64                           metadataRevMask[4];

} hlp_modelTriggerActions;

typedef struct _hlp_modelTriggerResults
{
    fm_uint32                           action;

    hlp_modelTriggerActionForwarding    forwardingAction;
    fm_uint16                           destGlort;
    fm_uint64                           destMask;
    fm_bool                             filterDestMask;

    hlp_modelTriggerActionTrap          trapAction;
    fm_byte                             cpuCode;
    fm_byte                             trapCode;

    /* RRC Spec. bug 22835.  Tracks Trig. to log CPU code in apply. */
    fm_bool                             logAction;

    hlp_modelTriggerActionMirroring     mirroringAction0;
    hlp_modelTriggerActionMirroring     mirroringAction1;
    fm_bool                             rxMirror;
    fm_byte                             mirrorProfileIndex0;
    fm_byte                             mirrorProfileIndex1;
    fm_bool                             mirror0ProfileV;
    fm_bool                             mirror1ProfileV;
    fm_byte                             mirror0ProfileIdx;
    fm_byte                             mirror1ProfileIdx;

    hlp_modelTriggerActionTC            TCAction;
    fm_byte                             TC;

    hlp_modelTriggerActionVlan          vlanAction;
    fm_uint16                           vlan;

    hlp_modelTriggerActionLearning      learningAction;
    
    fm_bool                             rateLimitAction;
    fm_byte                             rateLimitNum;

    fm_int                              metadataTrigNum[4];
    fm_byte                             metadataAction[4];

	fm_byte                             egressL2DomainAction;
    fm_byte                             egressL3DomainAction;

	fm_byte                             qcnValid0;
    fm_byte                             qcnValid1;

	fm_byte                             policerAction;

    fm_byte                             noModifyAction;

} hlp_modelTriggerResults;

/***************************************************
 * The following state structure holds the
 * pipeline state while a packet is being processed.
 * This state is completely reset at the start of
 * processing for a packet.  Any persistent state
 * should be defined in hlp_model data structure.
 **************************************************/

#include <platforms/common/model/hlp/hlp_model_state.h>

/**************************************************
 * Overall switch state structure. Contains
 * all registers used by the model and
 * all persistent internal states.
 **************************************************/

typedef struct _hlp_model
{
    /* The switch that this white model state maps to */
    fm_int                          sw;

    /* Lock to provide exclusive access to the model state & registers */
    fm_lock                         modelLock;

    /* The register address space */
    fm_uint32                       registers[HLP_MODEL_MAX_HW_ADDR/4 + 1];

    /* Holds the state of a packet during the pipline processing */
    hlp_modelState                  packetState;

    /* Boolean indicating if the model is allowed to change the register cache.
     */
    fm_bool                         allowStateChange;

    /* The cached register state needed by the FFU stage */
    hlp_modelFfuTcam                FFU_TCAM[HLP_FFU_TCAM_ENTRIES_2][HLP_FFU_TCAM_ENTRIES_1][HLP_FFU_TCAM_ENTRIES_0];
    
    /* The cached register state needed by the L2_Lookup stage */
    //hlpMaTable                      MA_TABLE_CAM[HLP_MODEL_MA_TABLE_CAM_ENTRIES];
    

    /* The cached register state needed by the GEN_MASK1 and GEN_MASK2 stages.
     */
    //hlpCmApplyLoopbackSuppress      LOOPBACK_SUPPRESS[HLP_CM_APPLY_LOOPBACK_SUPPRESS_ENTRIES];
    fm_uint64                       INTERNAL_PORT_MASK;
    //hlpLagCfg                       LAG_CFG[HLP_LAG_CFG_ENTRIES];
    /*hlpMaTableCfg2                  MA_TABLE_CFG_2;*/
    //hlpRxMirrorCfg                  RX_MIRROR_CFG;
    //hlpLogMirrorProfile             LOG_MIRROR_PROFILE;
    fm_uint64                       cpuMacAddr;
    //hlpMcastEpoch                   MCAST_EPOCH;

    /* The cached register state needed by the CM stage. */
    fm_byte                         CM_SMP_MEMBERSHIP[HLP_CM_APPLY_TC_TO_SMP_ENTRIES];
    /*fm_byte                         SWITCH_PRI_TO_CLASS[HLP_CM_APPLY_SWITCH_PRI_TO_TC_ENTRIES];*/

    /* The cached register state needed by the LEARNING stage. */
    /*
    fm_uint16                       MA_TCN_PTR_HEAD;
    fm_uint16                       MA_TCN_PTR_TAIL;
    hlp_modelMaTcnDequeue           MA_TCN_DEQUEUE;
    hlpMaTcnFifo                    MA_TCN_FIFO[HLP_MA_TCN_FIFO_ENTRIES];
    */

    /* The cached register state needed by the STATS_RX stages. */
    //fm_uint64                       RX_STATS_BANK[HLP_RX_STATS_BANK_ENTRIES_1][HLP_RX_STATS_BANK_ENTRIES_0];

    // reg removed (bug 31340)    hlpRxStatsCfg    RX_STATS_CFG[HLP_RX_STATS_CFG_ENTRIES];

    /* The set of registers needed to recreate the EPL interrupt chain. */
    /*
    fm_uint32                       PCS_IP[HLP_MAX_EPL + 1];
    fm_uint32                       EPL_INT_DETECT[HLP_MAX_EPL + 1];
    fm_uint32                       GLOBAL_EPL_INT_DETECT;
    */

    /* The set of registers needed to recreate the TCN interrupt chain. */
    /*
    fm_uint32                       MA_TCN_IM;
    fm_uint32                       MA_TCN_IP;
    fm_uint16                       MA_TCN_WM[HLP_MA_TCN_WM_ENTRIES];
    fm_uint16                       MA_TCN_USAGE[HLP_MA_TCN_USAGE_ENTRIES];
    */

    fm_uint32                       FH_TAIL_IP;
    fm_uint32                       FH_TAIL_IM;
    /* The global interrupt detection register */
    fm_uint64                       INTERRUPT_DETECT;

    /* Local variable for fsched */
    fm_uint32                       FSCHED_L3COPY;

    /* temporary memory to copy MAC stats over for MAC_CFG.stats_request */
    fm_uint32               MAC_TX_STATS[HLP_MAC4_SCRATCH_ENTRIES][36];
    fm_uint32               MAC_RX_STATS[HLP_MAC4_SCRATCH_ENTRIES][46];


} hlp_model;




/*****************************************************************************
 * Public Function Prototypes
 *****************************************************************************/

/***************************************************
 * Register management functions.
 **************************************************/

hlp_resetDomain hlpModelFindRegisterResetDomain(fm_uint32       addr,
                                                hlp_resetDomain domain);

fm_status hlpModelGetOffsetMult1(fm_uint32 addr,
                                 fm_uint32 baseAddr,
                                 fm_int    entries,
                                 fm_int    stride,
                                 fm_int *  index,
                                 fm_int *  word);

fm_status hlpModelGetOffsetMult2(fm_uint32 addr,
                                 fm_uint32 baseAddr,
                                 fm_int    entries1,
                                 fm_int    entries0,
                                 fm_int    stride1,
                                 fm_int    stride0,
                                 fm_int *  index1,
                                 fm_int *  index0,
                                 fm_int *  word);

fm_status hlpModelGetOffsetMult3(fm_uint32 addr,
                                 fm_uint32 baseAddr,
                                 fm_int    entries2,
                                 fm_int    entries1,
                                 fm_int    entries0,
                                 fm_int    stride2,
                                 fm_int    stride1,
                                 fm_int    stride0,
                                 fm_int *  index2,
                                 fm_int *  index1,
                                 fm_int *  index0,
                                 fm_int *  word);

void hlpModelGetRegisterAccess(fm_uint32  addr,
                               fm_uint32 *rwMask,
                               fm_uint32 *roMask,
                               fm_uint32 *cwMask,
                               fm_uint32 *cw1Mask,
                               fm_uint32 *rvMask);

fm_uint32 hlpModelGetRegisterDefault(fm_uint32 addr);

fm_status hlpModelReadUpdateCSRInternal(hlp_model     *model,
                                        fm_uint32     addr,
                                        fm_uint32     value);

fm_status hlpModelReadCSRInternal(hlp_model     *model,
                                  fm_uint32     addr,
                                  fm_uint32     value);

fm_status hlpModelWriteCSRAbsolute(hlp_model    *model,
                                   fm_uint32     addr,
                                   fm_uint32     newValue);

fm_status hlpModelWriteCSRAbsolute64(hlp_model      *model,
                                     fm_uint32      addr,
                                     fm_uint64      value);

fm_status hlpModelWriteCSRInternal(hlp_model     *model,
                                   fm_uint32     addr,
                                   fm_uint32     newValue,
                                   fm_uint32     oldValue,
                                   fm_bool       init);

fm_status hlpModelWriteCSRMultAbsolute(hlp_model    *model,
                                       fm_uint32     addr,
                                       fm_int        n,
                                       fm_uint32 *   value);

fm_status hlpModelMacWriteCSR(hlp_model     *model,
                              fm_uint32     addr,
                              fm_uint32     newValue,
                              fm_uint32     oldValue,
                              fm_bool       init);

fm_status hlpModelMacsecWriteCSR(hlp_model     *model,
                              fm_uint32     addr,
                              fm_uint32     newValue,
                              fm_uint32     oldValue,
                              fm_bool       init);

fm_status hlpModelFfuClassifierWriteCSR(hlp_model     *model,
                              fm_uint32     addr,
                              fm_uint32     val,
                              fm_bool       init);

fm_status hlpModelModifyWriteCSR(hlp_model    *model,
                                 fm_uint32     addr,
                                 fm_uint32     value);

fm_status hlpModelL2LookupWriteCSR(hlp_model   *model,
                                   fm_uint32   addr,
                                   fm_uint32   val,
								   fm_bool     init);

fm_status hlpModelFwdMiscWriteCSR(hlp_model   *model,
                                  fm_uint32    addr,
                                  fm_uint32    newValue,
                                  fm_uint32    oldValue,
                                  fm_bool      init);

fm_status hlpModelL2LookupReadCSR(hlp_model   *model,
                                  fm_uint32   addr,
                                  fm_uint32   value);

fm_status hlpModelL2LookupReadUpdateCSR(hlp_model   *model,
                                        fm_uint32   addr,
                                        fm_uint32   value);

fm_status hlpModelCmWriteCSR(hlp_model          *model,
                                 fm_uint32      addr,
                                 fm_uint32      val);

fm_status hlpModelFschedWriteCSR(hlp_model     *model,
                                 fm_uint32     addr,
                                 fm_uint32     value);

fm_bool hlpModelMacStatsGet(hlp_model *model,
                            fm_uint32 addr,
                            fm_uint32 *value);

/***************************************************
 * HLP white model initialization functions.
 **************************************************/
fm_status hlpModelMacInitialize(hlp_model *model);

fm_status hlpModelInitializeInternal(hlp_model *model);

/***************************************************
 * HLP white model utility functions.
 **************************************************/
fm_status hlpModelApplyKeyMask(hlp_model               *model,
                               hlp_modelFfuKeyMaskCfg   keyMaskCfg,
                               hlp_modelFfuKeys        *hashKeys);

fm_status hlpModelConvertKeysToBytes(hlp_modelFfuKeys    ffuKeys,
                                     fm_byte             *keyBytes);

fm_bool hlpModelIsMulticastIPAddress(fm_uint32  *ip,
                                     fm_bool    isIPv4,
                                     fm_bool    isIPv6);

fm_bool fmModelIsCpuMacAddress(hlp_model *model, 
                               fm_macaddr macAddrIn);

fm_status hlpModelLookupAddress(hlp_model    *model,
		                        fm_byte      lookupKind,
                                fm_macaddr   macAddr,
                                fm_uint16    vid,
                                fm_uint16    l2Domain,
                                fm_uint16    *index,
                                fm_byte      *set,
                                fm_bool      *hit,
                                hlpMaTable   *entry);

void WriteMaTableEntry(hlp_model   *model,
                       fm_uint32    set,
                       fm_uint32    idx,
                       hlpMaTable   *entry);

fm_bool CheckIfEntryTypeValid(fm_byte entryType);

void ClearDirtyBitIfSet(hlp_model    *model,
                        fm_uint16    MATableIndex,
						fm_char      *message);

void UpdateDirtyTableAndCounter(hlp_model *model,
                                fm_uint32 MACTableIndex,
                                fm_bool   bitWritten,
								fm_char   *message);

void UpdateDirtyCount(hlp_model *model, fm_bool IncrOrDecr);

fm_status hlpModelL2MATableDirectWrite(hlp_model *model, fm_uint16 regIndex);

fm_status hlpModelL2MGMTLookup(hlp_model *model, fm_int regIndex);

fm_status hlpModelL2MGMTIndirectMATableRead(hlp_model *model, fm_int regIndex);

fm_status hlpModelL2MATableDirectRead(hlp_model *model, fm_uint16 regIndex);

void UpdateMaTableEntryCount(hlp_model  *model,
                             fm_byte    incrPort,
                             fm_byte    decrPort,
                             fm_byte    entryType);

void UpdateDOSAttackActive(hlp_model  *model,
                           fm_byte    updatePortNumber,
						   fm_byte    callerInfo);

void SetDOSProtectionActiveGlobal(fm_byte updatePortNumber,
                                  fm_bool val);

fm_bool GetDOSProtectionActiveGlobal(fm_byte retrPort);

/***************************************************
 * HLP white model stages in pipeline order.
 **************************************************/
fm_int hlpModelMacsecRx(hlp_model *model);

fm_int hlpModelMacRx(hlp_model *model);

fm_status hlpModelXbarRx(hlp_model *model);

void hlpModelParser(hlp_model *model);

void hlpModelMapper(hlp_model *model);

fm_status hlpModelFfuClassifier(hlp_model *model);

fm_status hlpModelFfuFinalActions(hlp_model *model);

void hlpModelHash(hlp_model *model);

fm_status hlpModelNextHop(hlp_model *model);

fm_status hlpModelPolicer(hlp_model *model);

fm_status hlpModelL2Lookup(hlp_model *model);

void hlpModelGlort(hlp_model *model);

void hlpModelGenMask1(hlp_model *model);

fm_status hlpModelTriggers(hlp_model *model);

void hlpModelCm(hlp_model *model);

fm_status hlpModelGenMask2(hlp_model *model);

fm_status hlpModelLearning(hlp_model *model);

fm_status hlpModelStatsRx(hlp_model *model);

fm_bool hlpModelFsched(hlp_model *model);

fm_status hlpModelModify(hlp_model     *model,
                         fm_byte *     packet,
                         fm_uint32     maxPktSize);

fm_status hlpModelStatsTx(hlp_model *model);

fm_status hlpModelXbarTx(hlp_model *model);

fm_int hlpModelMacTx(hlp_model     *model,
                     fm_byte *     packet,
                     fm_uint32     maxPktSize);

fm_int hlpModelMacsecTx(hlp_model     *model,
                        fm_byte *     packet,
                        fm_uint32     maxPktSize,
                        fm_bool *     encrypted);

#endif /* __HLP_MODEL_TYPES_H */

