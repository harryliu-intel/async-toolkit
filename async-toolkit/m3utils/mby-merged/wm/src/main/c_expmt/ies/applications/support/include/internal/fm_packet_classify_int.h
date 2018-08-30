/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces, text width is 79)  */

/*****************************************************************************
 * File:            fm_packet_classify_int.h
 * Creation Date:   October 26, 2007
 * Description:     
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_PACKET_CLASSIFY_INT_H
#define __FM_FM_PACKET_CLASSIFY_INT_H

#define FM4000_PACKET_TRAP_IGMP     0x86

#define /* int */ __FM_PACKET_P_WORD(/* int  */ p)                            \
    ((p) >> 2)

/**
 * \desc        retrieves a 32-bit word from the packet buffer
 *
 * \param[in]   buffer the packet buffer to retrieve the 32-bit word from
 *
 * \param[in]   p the index of the first byte of the 32-bit word to be
 *              retrieved
 *
 * \return      a 32-bit word
 *
 *****************************************************************************/
#define /* fm_uint32 */ FM_PACKET_GET_WORD(/* fm_buffer *  */ buffer,         \
                                           /* int          */ p)              \
    (__FM_PACKET_P_WORD(p) == __FM_PACKET_P_WORD((p) + 3)                     \
        ? (buffer)->data[__FM_PACKET_P_WORD(p)]                               \
        : (   FM_PACKET_GET_BYTE(buffer, (p) + 0) << 24                       \
            | FM_PACKET_GET_BYTE(buffer, (p) + 1) << 16                       \
            | FM_PACKET_GET_BYTE(buffer, (p) + 2) << 8                        \
            | FM_PACKET_GET_BYTE(buffer, (p) + 3) << 0 ))

/**
 * \desc        modifies a 32-bit word in the packet buffer
 *
 * \param[in]   buffer the packet buffer to be modified
 *
 * \param[in]   p the index of the first byte of the 32-bit word to be modified
 *
 * \param[in]   value the new value
 *
 *****************************************************************************/
#define /* void */ FM_PACKET_SET_WORD(/* fm_buffer *  */ buffer,              \
                                      /* int          */ p,                   \
                                      /* fm_uint32    */ value)               \
    if (__FM_PACKET_P_WORD(p) == __FM_PACKET_P_WORD((p) + 3))                 \
    {                                                                         \
        (buffer)->data[__FM_PACKET_P_WORD(p)] = (value);                      \
    }                                                                         \
    else                                                                      \
    {                                                                         \
        FM_PACKET_SET_BYTE(buffer, (p) + 0, value);                           \
        FM_PACKET_SET_BYTE(buffer, (p) + 1, value);                           \
        FM_PACKET_SET_BYTE(buffer, (p) + 2, value);                           \
        FM_PACKET_SET_BYTE(buffer, (p) + 3, value);                           \
    }

/**
 * \desc        advances the packet buffer position pointer to the next 32-bit
 *              word
 *
 * \param[in]   p the packet buffer position pointer to be advanced
 *
 *****************************************************************************/
#define /* void */ FM_PACKET_ADVANCE_P_WORD(/* int  */ p)                     \
    (p) += 4

#define /* int */ __FM_PACKET_P_HALF_WORD(/* int  */ p)                       \
    ((p) >> 1)

#define /* int */ __FM_PACKET_SHIFT_HALF_WORD(/* int  */ p)                   \
    ((1 - (__FM_PACKET_P_HALF_WORD(p) % 2)) << 4)

/**
 * \desc        retrieves a 16-bit half-word from the packet buffer
 *
 * \param[in]   buffer the packet buffer to retrieve the 16-bit half-word from
 *
 * \param[in]   p the index of the first byte of the 16-bit half-word to be
 *              retrieved
 *
 * \return      a 16-bit half-word
 *
 *****************************************************************************/
#define /* fm_uint32 */ FM_PACKET_GET_HALF_WORD(/* fm_buffer *  */ buffer,    \
                                                /* int          */ p)         \
    (__FM_PACKET_P_HALF_WORD(p) == __FM_PACKET_P_HALF_WORD((p) + 1)           \
        ? (((buffer)->data[__FM_PACKET_P_WORD(p)]                             \
            >> __FM_PACKET_SHIFT_HALF_WORD(p)) & 0xFFFF)                      \
        : (   FM_PACKET_GET_BYTE(buffer, (p) + 0) << 8                        \
            | FM_PACKET_GET_BYTE(buffer, (p) + 1) << 0))

/**
 * \desc        modifies a 16-bit half-word in the packet buffer
 *
 * \param[in]   buffer the packet buffer to be modified
 *
 * \param[in]   p the index of the first byte of the 16-bit half-word to be
 *              modified
 *
 * \param[in]   value the new value
 *
 *****************************************************************************/
#define /* void */ FM_PACKET_SET_HALF_WORD(/* fm_buffer *  */ buffer,         \
                                           /* int          */ p,              \
                                           /* fm_uint32    */ value)          \
    if (__FM_PACKET_P_HALF_WORD(p) == __FM_PACKET_P_HALF_WORD((p) + 1))       \
    {                                                                         \
        (buffer)->data[__FM_PACKET_P_WORD(p)] =                               \
            ((buffer)->data[__FM_PACKET_P_WORD(p)]                            \
                & ~(0xFFFF << __FM_PACKET_SHIFT_HALF_WORD(p)))                \
            | (((value) & 0xFFFF) << __FM_PACKET_SHIFT_HALF_WORD(p));         \
    }                                                                         \
    else                                                                      \
    {                                                                         \
        FM_PACKET_SET_BYTE(buffer, (p) + 0, value);                           \
        FM_PACKET_SET_BYTE(buffer, (p) + 1, value);                           \
    }

/**
 * \desc        advances the packet buffer position pointer to the next 16-bit
 *              half-word
 *
 * \param[in]   p the packet buffer position pointer to be advanced
 *
 *****************************************************************************/
#define /* void */ FM_PACKET_ADVANCE_P_HALF_WORD(/* int  */ p)                \
    (p) += 2

#define /* int */ __FM_PACKET_P_BYTE(/* int  */ p)                            \
    (p)

#define /* int */ __FM_PACKET_SHIFT_BYTE(/* int  */ p)                        \
    ((3 - ((p) % 4)) << 3)

/**
 * \desc        retrieves a byte from the packet buffer
 *
 * \param[in]   buffer the packet buffer to retrieve the byte from
 *
 * \param[in]   p the index of the byte to be retrieved
 *
 * \return      a byte
 *
 *****************************************************************************/
#define FM_PACKET_GET_BYTE(/* fm_buffer *  */ buffer,                         \
                           /* int          */ p)                              \
    (((buffer)->data[__FM_PACKET_P_WORD(p)]                                   \
        >> __FM_PACKET_SHIFT_BYTE(p)) & 0xFF)

/**
 * \desc        modifies a byte in the packet buffer
 *
 * \param[in]   buffer the packet buffer to be modified
 *
 * \param[in]   p the index of the byte to be modified
 *
 * \param[in]   value the new value
 *
 *****************************************************************************/
#define /* void */ FM_PACKET_SET_BYTE(/* fm_buffer *  */ buffer,              \
                                      /* int          */ p,                   \
                                      /* fm_uint32    */ value)               \
    (buffer)->data[__FM_PACKET_P_WORD(p)] =                                   \
        ((buffer)->data[__FM_PACKET_P_WORD(p)]                                \
            & ~(0xFF << __FM_PACKET_SHIFT_BYTE(p)))                           \
        | (((value) & 0xFF) << __FM_PACKET_SHIFT_BYTE(p))


/**
 * \desc        advances the packet buffer position pointer to the next byte
 *
 * \param[in]   p the packet buffer position pointer to be advanced
 *
 *****************************************************************************/
#define /* void */ FM_PACKET_ADVANCE_P_BYTE(/* int  */ p)                     \
    (p) += 1

#endif /* __FM_FM_PACKET_CLASSIFY_INT_H */
