/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_modify.c
 * Creation Date:   June 25, 2012
 * Description:     MODIFY stage of HLP white model
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

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define ABORT_ON_EOP(status, len, p, output_len, outputIdx)     \
    if ( ( (p) >= (len) ) || ( (outputIdx) >= output_len ) )    \
    {                                                           \
        *(status) = FM_FAIL;                                    \
        goto DONE;                                              \
    }

#define GET_BYTE(value, offset)                                 \
    ( (fm_byte) (((value) >> offset) & 0xFF) )

/* conditional print, saves line-length real estate */
#define PRINT(wmDisplayVerbose, key, ...)                        \
    do {                                                        \
        if (wmDisplayVerbose >= 1){                  \
            FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MODIFY, "dxb[%s]: ", key);     \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG, __VA_ARGS__);      \
        }                                                       \
    }while(0);

#define PRINTF(wmDisplayVerbose, ...)                            \
    do {                                          \
        if(wmDisplayVerbose >= 1){                   \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG, __VA_ARGS__);      \
        }                                            \
    }while(0);

#define PRINT2(wmDisplayVerbose, key, ...)                        \
    do {                                         \
        if (wmDisplayVerbose >= 2){                  \
            FM_LOG_DEBUG2(FM_LOG_CAT_MODEL_MODIFY, "dxb[%s]: ", key);     \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG2, __VA_ARGS__);      \
        }                                         \
    }while(0);

#define PRINTF2(wmDisplayVerbose, ...)                            \
    do {                                         \
        if(wmDisplayVerbose >= 2){                   \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG2, __VA_ARGS__);      \
        }                                            \
    }while(0);

#define PRINT3(wmDisplayVerbose, key, ...)                        \
    do {                                         \
        if (wmDisplayVerbose == 3){                  \
            FM_LOG_DEBUG3(FM_LOG_CAT_MODEL_MODIFY, "dxb[%s]: ", key);     \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG3, __VA_ARGS__);      \
        }                                         \
    }while (0);

#define PRINTF3(wmDisplayVerbose, ...)                            \
    do {                                         \
        if(wmDisplayVerbose == 3){                   \
            FM_LOG_PRINTF(FM_LOG_CAT_MODEL_MODIFY, FM_LOG_LEVEL_DEBUG3, __VA_ARGS__);      \
        }                                            \
    }while(0);

#define FTAG_BYTES              8
#define VLAN_TAG_BYTES          4
#define MAC_ADDR_BYTES          6
#define DEFAULT_SEGMENT_BYTES   192
#define MIN_RUNT_BYTES          8
#define PAUSE_FRAME_BYTES       64
#define ETYPE_BYTES             2
#define BYTES_PER_WORD          4
#define MIN_EGRESS_BYTES        18
#define NYBBLE_BITS				4

enum // as per index
{//                         index          priority
    DISP_TXECCDROP      =     0,             //1         pm_read.err
    DISP_TXERRORDROP    =     1,             //2         mod_ctrl.tx_drop
    DISP_MARKERERRDROP  =     2,             //3
    DISP_L3ERRDROP      =     3,             //3
    DISP_L4ERRDROP      =     4,             //3
    DISP_L3L4ERRMARK    =     5,             //3
    DISP_TIMEOUTDROP    =     6,             //4         mod_ctrl.is_timeout
    DISP_LOOPBACKDROP   =     7,             //5
    DISP_TTL1DROP       =     8,             //6         fwd_modify.ttl1
    DISP_ECNDROP        =     9,             //7
    DISP_MODERRORDROP   =     10,            //8
    DISP_TXERROR        =     11,            //9
    DISP_OOMTRUNC       =     12,            //10
    DISP_UCAST          =     13,            //11
    DISP_BCAST          =     14,            //12
    DISP_MCAST          =     15             //13
};

enum
{
    IS_OK = 0,
    EOP_ERROR,
    NOT_ENOUGH_RX_DATA,
    INVALID_IP_VERSION,
    COPY_ERROR
};

enum
{
    NODROP = 0,
    DROP = 1
};

enum
{
    NOSKIP = 0,
    SKIP = 1
};

enum
{
    NOLOG = 0,
    LOG = 1
};

enum // REASON CODE for MOD_UPDATE_DEBUG_RECORD
{
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
};

enum { // interrupt disp
    INTR_MEM_ECC_ERR                 = 0,
    INTR_U_INSERT_VLAN_IPP           = 1, 
    INTR_U_INSERT_L2_TAG_OTR         = 2,
    INTR_U_INSERT_L2_TAG_INR         = 3,
    INTR_U_REMOVE_VLAN_IPP           = 4,
    INTR_U_REMOVE_L2_TAG_OTR         = 5,
    INTR_U_REMOVE_L2_TAG_INR         = 6,
    INTR_U_PUSH_AL                   = 7,
    INTR_U_PUSH_ELI                  = 8,
    INTR_U_PUSH_G                    = 9,
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
};
// TODO
// ds_src > n_otr_g or n_otr_mpls

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_int      modDisplayVerbose;    

#ifdef IPV4_CSUM_RECALC
    fm_bool     ipv4CsumRecalc = 1;
#else
    fm_bool     ipv4CsumRecalc = 0;
#endif

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** CopyDataForDV
 * \ingroup intModel
 *
 * \desc            Copies some WM data for use by Modify unit
 *                  level DV testbench.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[out]      packet points to output buffer (wkey is ready to transmit)
 *
 * \param[in]       dvStatus is 1st byte value
 *
 * \param[in]       egressSeg0Bytes is bytes of first seg
 *
 * \param[in]       refcntSeg0Bytes is bytes of first seg for MD
 *
 * \param[in]       bytesAdded is next byte value
 *
 * \param[in]       isTruncated is next byte value
 *
 * \param[in]       numVlans is next byte value
 *
 * \return          None
 *
 *****************************************************************************/
static void CopyDataForDV(hlp_model *       model,
                          fm_byte *         packet,
                          fm_byte           dvStatus,
                          fm_uint32         egressSeg0Bytes,
                          fm_uint32         refcntSeg0Bytes,
                          fm_byte           bytesAdded,
                          fm_bool           isTruncated,
                          fm_byte           numVlans,
                          fm_byte           reasonCode,
                          fm_bool           ecn_tx_drop,
                          fm_bool           timeout_tx_drop,
                          fm_bool           non_cm_tx_drop,
                          fm_bool           cancel_drop_on_marker,
                          fm_byte           cancelled_tx_disp,
                          fm_byte           ecn_mark,
                          fm_bool           intr_occured)
{
    hlp_modelState *    state;
    int                     bufferIndex;
    int                     pktIndex;
    int                     i;
    int                     j;

    state = &model->packetState;
    bufferIndex = 0;
    pktIndex = 0;

    /* copy informational data from params */
    state->MODIFY_DV_DATA[bufferIndex++] = dvStatus;
    state->MODIFY_DV_DATA[bufferIndex++] = egressSeg0Bytes;
    state->MODIFY_DV_DATA[bufferIndex++] = refcntSeg0Bytes;
    state->MODIFY_DV_DATA[bufferIndex++] = bytesAdded;
    state->MODIFY_DV_DATA[bufferIndex++] = isTruncated;
    //state->MODIFY_DV_DATA[bufferIndex++] = portIsFtagged;
    state->MODIFY_DV_DATA[bufferIndex++] = 0;
    state->MODIFY_DV_DATA[bufferIndex++] = numVlans;
    state->MODIFY_DV_DATA[bufferIndex++] = reasonCode;
    state->MODIFY_DV_DATA[bufferIndex++] = ecn_tx_drop;
    state->MODIFY_DV_DATA[bufferIndex++] = timeout_tx_drop;
    state->MODIFY_DV_DATA[bufferIndex++] = non_cm_tx_drop;
    state->MODIFY_DV_DATA[bufferIndex++] = cancel_drop_on_marker;
    state->MODIFY_DV_DATA[bufferIndex++] = cancelled_tx_disp;
    state->MODIFY_DV_DATA[bufferIndex++] = ecn_mark;
    state->MODIFY_DV_DATA[bufferIndex++] = intr_occured;

    /* copy egress ftag data (if present) */
    //if (portIsFtagged)
    //{
    //    for (pktIndex = 0 ; pktIndex < FTAG_BYTES ; pktIndex++, bufferIndex++)
    //    {
    //        state->MODIFY_DV_DATA[bufferIndex] = packet[pktIndex];
    //    }
    //}
    //else
    //{
        bufferIndex += FTAG_BYTES;
    //}

    /* copy egress vlan tag data (if present) */
    if (numVlans == 0)
    {
        return;
    }

    pktIndex += MAC_ADDR_BYTES * 2;        /* skip DMAC and SMAC */
    for (i = 0 ; i < numVlans ; i++)
    {
        for (j = 0 ; j < VLAN_TAG_BYTES ; j++, bufferIndex++, pktIndex++)
        {
            state->MODIFY_DV_DATA[bufferIndex] = packet[pktIndex];
        }
    }
}

/*****************************************************************************/
/** CopyVlanToOutput
 * \ingroup intModel
 *
 * \desc            Copies VLAN tag data to output buffer
 *
 * \param[in]       etype is the TPID value (first 16 bits of tag)
 *
 * \param[in]       vpri is the PCP/DEI value (next 4 bits of tag)
 *
 * \param[in]       vid is the VID value (next 12 bits of tag)
 *
 * \param[out]      tags is pointing to the structure that is holding the
 *                  output vlan tags.
 *
 * \param[in]       vlan2 adjusts the index of the otr_tags
 *
 * \return          None
 *
 *****************************************************************************/
static void CopyVlanToOutput(fm_uint16           etype,
                             fm_byte             vpri,
                             fm_uint16           vid,
                             fm_byte             *tags,
                             fm_bool             vlan2)
{
	fm_uint32 idx;
	idx = 0;
	if (vlan2)
	{
		idx = 4;
	}
    *(tags+idx)   = (etype >> 8) & 0xFF;
    *(tags+idx+1) = (etype & 0xFF);
    *(tags+idx+2) = (vpri << 4) | (vid >> 8);
    *(tags+idx+3) = (vid & 0xFF);
}

/*****************************************************************************/
/** GetIPv4Checksum
 * \ingroup intModel
 *
 * \desc            Calculates and returns csum value for IPv4 header
 *
 * \param[in]       startOffset points to beginning of IPv4 header; assumes
 *                  that csum bytes in the buffer are set to 00, else csum
 *                  returned will be incorrect
 *
 * \param[in]       numBytes is the length in bytes of the IPv4 header
 *
 * \return          IPv4 checksum value
 *                  (to be put into header buffer by caller)
 *
 *****************************************************************************/
static fm_uint16 GetIPv4Checksum(fm_byte *  startOffset,
                                 fm_int     numBytes)
{
    fm_uint32 csum;
    fm_uint16 temp16;
    fm_int    i;

    csum = 0;

    for (i = 0 ; i < numBytes ; i += 2)
    {
        temp16 = *(startOffset + i);
        temp16 <<= 8;
        temp16 |= (fm_uint16) *(startOffset + i + 1);
        csum += temp16;
        if (csum & 0x80000000)
        {
            csum = (csum & 0xFFFF) + (csum >> 16);
        }
    }

    while (csum >> 16)
    {
        csum = (csum & 0xFFFF) + (csum >> 16);
    }

    csum = ~csum;
    return ((fm_uint16)(csum & 0xFFFF));
}

/*****************************************************************************/
/** UpdateChecksum
 * \ingroup intModel
 *
 * \desc            Calculates and returns incrememental csum value for
 *                  header
 *
 * \param[in]       startOffset points to beginning of header;
 *
 * \param[in]       numBytes is the length in bytes of the header
 *
 * \param[in]       oldCsum is the old checksum value
 *
 * \return          checksum value
 *                  (to be put into header buffer by caller)
 *
 *****************************************************************************/
static fm_uint16 UpdateChecksum(fm_byte *  oldStartOffset,
                                fm_byte *  newStartOffset,
                                fm_int     numBytes,
                                fm_uint16  oldCsum,
                                fm_text    key)
{
    fm_uint32 newCsum;
    fm_uint16 oldVal;
    fm_uint16 newVal;

    FM_NOT_USED(key);

    newCsum = (oldCsum & 0x0FFFF);

    for (int i = 0; i < numBytes; i += 2)
    {
        oldVal = *(oldStartOffset + i);
        oldVal <<= 8;
        oldVal |= (fm_uint16) *(oldStartOffset + i + 1);
        newVal = *(newStartOffset + i);
        newVal <<= 8;
        newVal |= (fm_uint16) *(newStartOffset + i + 1);

        PRINT3(modDisplayVerbose, key, "1.newCsum=0x%08x\n", newCsum); 
        newCsum = (~newCsum&0x0FFFF) + (~oldVal&0x0FFFF) + (newVal&0x0FFFF);
        PRINT3(modDisplayVerbose, key, "2.newCsum=0x%08x\n", newCsum);
        while (newCsum >> 16)
            newCsum = (newCsum & 0x0FFFF) + (newCsum >> 16);
        newCsum = ~newCsum;
        PRINT3(modDisplayVerbose, key, "3.newCsum=0x%08x i=%0d oldVal=0x%08x newVal=0x%08x\n", newCsum, i, oldVal, newVal);
    }

    return ((fm_uint16)(newCsum & 0xFFFF));
}

/*****************************************************************************/
/** GetVPRI
 * \ingroup intModel
 *
 * \desc            Reads a VPRI map and returns the correct 4 bits
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       whichVlan is 1 for vlan1, 2 for vlan2
 *
 * \param[in]       vpri is vpri from frame processor, used as index
 *
 * \return          4-bit VPRI value for which_vlan, indexed by vpri
 *
 *****************************************************************************/
static fm_byte  GetVPRI(hlp_model        *model,
                        fm_byte          whichVlan,
                        fm_byte          vpri,
                        fm_byte          p_profile,
                        fm_text          key)
{
    fm_byte                 retVal;
    fm_uint64               temp64;
    hlp_modelState          *state;
    fm_uint32               *vpri_map;

    state = &model->packetState;

    if (whichVlan == 1) {
        vpri_map = FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_VPRI1_MAP(p_profile, 0));
            //HLP_MOD_VPRI1_MAP(state->TX_PORT, 0));
    	temp64 = FM_ARRAY_GET_UNNAMED_FIELD64(vpri_map, 0, 64);
    }
    else
    {
        vpri_map = FM_MODEL_GET_REG_PTR(model,
        	HLP_MOD_VPRI2_MAP(p_profile, 0));
        	//HLP_MOD_VPRI2_MAP(state->TX_PORT, 0));
    	temp64 = FM_ARRAY_GET_UNNAMED_FIELD64(vpri_map, 0, 64);
    }

    PRINT2(modDisplayVerbose, key, "MOD_VPRI%0d_MAP[%0d]=0x%016llx\n", whichVlan, p_profile, temp64);

    /* this seems easier than storing reg nibble fields in byte array,
     * or than constructing a name for FM_GET_FIELD */
    temp64 >>= (vpri * 4);
    temp64 &= 0xF;
    retVal = (fm_byte)temp64;
    return retVal;
}

/*****************************************************************************/
/** UpdateTxStatsBankFrame
 * \ingroup intModel
 *
 * \desc            Updates MOD_STATS_BANK_FRAME and MOD_STATS_BANK_BYTE for
 *                  given bank, index, byte count
 *
 * \param[in]       model points to the switch model state.
 * \param[in]       bank indicates bank of registers
 *                      (0 -> group 7, 1 -> group 8)
 * \param[in]       index indicates offset within the bank (0-767)
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status UpdateTxStatsBank(fm_text          key,
                            hlp_model        *model,
                            fm_uint32        bank,
                            fm_uint16        index)
{
    hlp_modelState                  *state;
    fm_status                       status;
    fm_uint64                       frameCnt;
    fm_uint64                       byteCnt;
    fm_uint64                       delta1;
    fm_uint64                       delta2;
    const fm_uint64                 one = FM_LITERAL_U64(1);
    fm_uint64                       rdval;
    fm_uint64                       *modStatsBankFrame;
    fm_uint64                       *modStatsBankByte;
    fm_uint32                       length;

    state = &model->packetState;
    status = FM_OK;

    /* Obtain existing values */

    PRINT2(modDisplayVerbose, key, "bank = 0x%0x index = %0d txport = %0d, txstatslen = 0x%0x, tx_len = 0x%0x\n", bank, index, state->TX_PORT, state->TX_STATS_LENGTH, state->TX_LENGTH);
    modStatsBankFrame = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_STATS_BANK_FRAME(index, 0));
    frameCnt = FM_GET_FIELD64(*modStatsBankFrame,
        HLP_MOD_STATS_BANK_FRAME, FRAME_COUNTER);

    modStatsBankByte = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_STATS_BANK_BYTE(index, 0));
    byteCnt = FM_GET_FIELD64(*modStatsBankByte, HLP_MOD_STATS_BANK_BYTE, BYTE_COUNTER);

    length = state->TX_STATS_LENGTH;

    PRINT2(modDisplayVerbose, key, "final length been updated is 0x%0x, tx_disp is %0d\n", length, state->TX_DISP);

    /* Increment frame count; Wrap around to zero portably. */
    delta1 = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) - frameCnt;
    if (one > delta1)
    {
        /* Ensure that 0xFFFFFFFFFFFFFFFF + 1 wraps around to zero. */
        frameCnt = one - ( FM_LITERAL_U64(1) + delta1 );
    }
    else
    {
        frameCnt += one;
    }

    /* Update register */
    FM_SET_FIELD64(*modStatsBankFrame,
        HLP_MOD_STATS_BANK_FRAME, FRAME_COUNTER, frameCnt);

    /* Update byte count; Wrap around to zero portably. */
    delta2 = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) - byteCnt;
    if (length > delta2)
    {
        /* Ensure that 0xFFFFFFFFFFFFFFFF + 1 wraps around to zero. */
        byteCnt = length - ( FM_LITERAL_U64(1) + delta2 );
    }
    else
    {
        byteCnt += length;
    }

    /* Update register */
    FM_SET_FIELD64(*modStatsBankByte,
        HLP_MOD_STATS_BANK_BYTE, BYTE_COUNTER, byteCnt);

    return status;
}

/*****************************************************************************/
/** HandleGroup7
 * \ingroup intModel
 *
 * \desc            Handles Group 7 statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleGroup7(fm_text key, hlp_model *model)
{
    hlp_modelState                  *state;
    fm_status                       status;
    fm_uint32                       bank;
    fm_uint16                       index;

    state = &model->packetState;
    status = FM_OK;
    bank = 0;
    index = (state->TX_PORT * 16);

    status = UpdateTxStatsBank(key, model, bank, index + state->TX_DISP);
    return status;
}

/*****************************************************************************/
/** HandleGroup8
 * \ingroup intModel
 *
 * \desc            Handles Group 8 statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleGroup8(fm_text key, hlp_model *model)
{
    hlp_modelState      *state;
    fm_status           status;
    fm_uint32           bank;
    fm_uint16           index;
    fm_bool             portIsFtagged;
    fm_uint64           length;

    state = &model->packetState;
    status = FM_OK;
    bank = 1;
    index = (state->TX_PORT * 16);

    //portIsFtagged = FM_GET_BIT64(*rdval,
    //    HLP_MOD_PER_PORT_CFG2, FTAG);
    length = state->TX_LENGTH;// - (8 * portIsFtagged);

    if (length < 64)
    {
        index += 0;
    }
    else if (length == 64)
    {
        index += 1;
    }
    else if (length < 128)
    {
        index += 2;
    }
    else if (length < 256)
    {
        index += 3;
    }
    else if (length < 512)
    {
        index += 4;
    }
    else if (length < 1024)
    {
        index += 5;
    }
    else if (length < 1523)
    {
        index += 6;
    }
    else if (length < 2048)
    {
        index += 7;
    }
    else if (length < 4096)
    {
        index += 8;
    }
    else if (length < 8192)
    {
        index += 9;
    }
    else if (length < 10240)
    {
        index += 10;
    }
    else
    {
        index += 11;
    }

    status = UpdateTxStatsBank(key, model, bank, index);
    return status;
}

/*****************************************************************************/
/** PrintChunkedSeg
 * \ingroup intModel
 *
 * \desc            Prints out the valid/length bits of the ChunkedSeg.
 *
 * \param[in]		chunkedSeg points to the broken down packet.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static void PrintChunkedSeg(fm_text key, hlp_modelChunkedSeg *chunkedSeg)
{
    PRINT(modDisplayVerbose, key, "ChunkedSeg: frag_v=%0d; n_otr_tag=%0d; n_otr_mpls=%0d; otr_ip_size=%0d; "
        "otr_udp_v=%0d; otp_tcp_v=%0d; tun_size_in_l4_chunk=%0d;\n",
        chunkedSeg->ftag_v, chunkedSeg->n_otr_tag, chunkedSeg->n_otr_mpls,
        chunkedSeg->otr_ip_size, chunkedSeg->otr_udp_v, chunkedSeg->otr_tcp_v,
        chunkedSeg->tun_size_in_l4_chunk);

    PRINT(modDisplayVerbose, key, "ChunkedSeg Inner: tun_opt_size=%0d; inr_l2_v=%0d; "
        "n_inr_tag=%0d; inr_mpls_len=%d; inr_ip_size=%0d; inr_udp_v=%0d; inr_tcp_v=%0d;\n",
        chunkedSeg->tun_opt_size, chunkedSeg->inr_l2_v, chunkedSeg->n_inr_tag,
        chunkedSeg->n_inr_mpls, chunkedSeg->inr_ip_size, chunkedSeg->inr_udp_v, chunkedSeg->inr_tcp_v);

    PRINT(modDisplayVerbose, key, "ChunkedSeg Payload: start=%0d; size=%0d;\n",
        chunkedSeg->payload_start, chunkedSeg->payload_size);
}

fm_int GetL4hdrIdx(fm_text key,
                   hlp_modelChunkedSeg *chunkedSeg,
			       hlp_modelParserInfo *parserInfo)
{
    fm_int idx = 0;
    fm_int otr_l2_len;
    fm_int otr_ip_size;
    fm_int n_otr_mpls;
	/* OTR ETH */
    if (parserInfo->otr_l2_len > 5) {
        otr_l2_len = 5;
    }
    else {
        otr_l2_len = parserInfo->otr_l2_len;
    }
    if (otr_l2_len > 0) {
        for (int i = 0; i < 6; i++)
            idx++;
        for (int i = 0; i < 6; i++)
            idx++;
        for (int i = 0; i < (4*chunkedSeg->n_otr_tag); i++)
            idx++;
        /* Etype */
        for (int i = 0; i < 2; i++)
            idx++;
    }
	/* OTR MPLS */
    if (parserInfo->otr_mpls_len > 7) {
        n_otr_mpls = 7;
    }
    else {
        n_otr_mpls = parserInfo->otr_mpls_len;
    }
	for (int i = 0; i < (4*n_otr_mpls); i++)
        idx++;
	/* OTR L3 */
    if (parserInfo->otr_l3_len > 14) {
        otr_ip_size = 14;
    }
    else {
        otr_ip_size = parserInfo->otr_l3_len;
    }
    for (int i = 0; i < (4*otr_ip_size); i++)
        idx++;
    /* Layer 4 */
	PRINT2(modDisplayVerbose, key, "l4hdr start idx = %0d;\n", idx);
    return idx;
}

void SelectL4CsumSrc(fm_text key,
                     hlp_model *model,
				 	 fm_byte   *rx_packet,
                     hlp_modelChunkedSeg *chunkedSeg)
{
    fm_int l4hdrIdx = 0;
    hlp_modelState *state = &model->packetState;
    l4hdrIdx = GetL4hdrIdx(key, chunkedSeg, &state->PARSER_INFO);
	PRINT2(modDisplayVerbose, key, "tail_csum_len: 0x%0llx; udp_v(%0d); tcp_v(%0d);\n", state->TAIL_CSUM_LEN, chunkedSeg->otr_udp_v, chunkedSeg->otr_tcp_v);
    if((state->TAIL_CSUM_LEN>>30) == 2) { // COMPUTEL4CHECKSUM
	    PRINT2(modDisplayVerbose, key, "tail_csum_len: COMPUTEL4CSUM CASE\n");
        if(chunkedSeg->otr_udp_v) {
            chunkedSeg->otr_l4[6] = (state->TAIL_CSUM_LEN >> 8)&0xFF;
            rx_packet[l4hdrIdx+6] = (state->TAIL_CSUM_LEN >> 8)&0xFF;
            chunkedSeg->otr_l4[7] = state->TAIL_CSUM_LEN & 0xFF;
            rx_packet[l4hdrIdx+7] = state->TAIL_CSUM_LEN & 0xFF;
        }
        if(chunkedSeg->otr_tcp_v) {
            chunkedSeg->otr_l4[16] = (state->TAIL_CSUM_LEN >> 8)&0xFF; 
            rx_packet[l4hdrIdx+16] = (state->TAIL_CSUM_LEN >> 8)&0xFF;
            chunkedSeg->otr_l4[17] = state->TAIL_CSUM_LEN & 0xFF; 
            rx_packet[l4hdrIdx+17] = state->TAIL_CSUM_LEN & 0xFF;
        }
    }
}

/*****************************************************************************/
/** UnpackPacket
 * \ingroup intModel
 *
 * \desc            Unpacks the packet into the ChunkedSeg data structure.
 *
 * \param[in]		packet points to the actual packet data.
 *
 * \param[in]       chuckedSeg is a pointer to the unpacked packet.
 *
 * \param[in]		parserInfo is the information from the parser about what
 * 					header information is in the packet.
 *
 * \param[in]		rx_length is the length of the incoming packet.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status UnpackPacket(fm_byte             *packet,
				 	          hlp_modelChunkedSeg *chunkedSeg,
				 	          hlp_modelParserInfo *parserInfo,
				 	          fm_uint32			   rx_length,
                              fm_text              key)
{
	fm_status status;
	status = FM_OK;

	fm_uint32 idx;
	fm_uint32 tmp_idx;
	fm_int otr_l2_len;
	fm_int otr_tun_len;
	fm_int inr_l2_len;

	idx = 0;

	/* OTR ETH */
    if (parserInfo->otr_l2_len > 5) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_l2_len=%0d, overrun 30B otr_l2 chunk segment\n", parserInfo->otr_l2_len);
        otr_l2_len = 5;
    }
    else {
        otr_l2_len = parserInfo->otr_l2_len;
    }
    if (otr_l2_len > 0) {
        for (int i = 0; i < 6; i++, idx++)
            chunkedSeg->otr_dmac[i] = packet[idx];
        for (int i = 0; i < 6; i++, idx++)
            chunkedSeg->otr_smac[i] = packet[idx];
        chunkedSeg->n_otr_tag = (otr_l2_len - 1);
        for (int i = 0; i < (4*chunkedSeg->n_otr_tag); i++, idx++)
            chunkedSeg->otr_tags[i] = packet[idx];
        /* Etype */
        for (int i = 0; i < 2; i++, idx++)
            chunkedSeg->otr_et[i] = packet[idx];
    }
	/* OTR MPLS */
    if (parserInfo->otr_mpls_len > 7) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_mpls_len=%0d, overrun 28B otr_mpls chunk segment\n", parserInfo->otr_mpls_len);
        chunkedSeg->n_otr_mpls = 7;
    }
    else {
        chunkedSeg->n_otr_mpls = parserInfo->otr_mpls_len;
    }
	chunkedSeg->n_otr_mpls_pre = parserInfo->otr_mpls_len;
	for (int i = 0; i < (4*chunkedSeg->n_otr_mpls); i++, idx++) {
		chunkedSeg->otr_mpls[i] = packet[idx];
    }
    // try to mimic rtl
    tmp_idx = idx;
	for (int i = 4*chunkedSeg->n_otr_mpls; i < 28; i++, tmp_idx++) {
		chunkedSeg->otr_mpls[i] = packet[tmp_idx];
    }
	/* OTR L3 */
    if (parserInfo->otr_l3_len > 14) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_l3_len=%0d, overrun 56B otr_l3 chunk segment\n", parserInfo->otr_l3_len);
        chunkedSeg->otr_ip_size = 14;
    }
    else {
        chunkedSeg->otr_ip_size = parserInfo->otr_l3_len;
    }
    chunkedSeg->otr_l3_v6 = parserInfo->otr_l3_v6;
    for (int i = 0; i < (4*chunkedSeg->otr_ip_size); i++, idx++)
		chunkedSeg->otr_ip[i] = packet[idx];
	/* OTR L4 */
    if (parserInfo->otr_l4_udp && parserInfo->otr_l4_tcp) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: In no way parserInfo->otr_l4_udp and parserInfo->otr_l4_tcp should be both asserted\n");
    }
    chunkedSeg->otr_udp_v = parserInfo->otr_l4_udp;
    chunkedSeg->otr_tcp_v = parserInfo->otr_l4_tcp;
    if (chunkedSeg->otr_udp_v)
    {
        if (parserInfo->otr_tun_len > 18) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_tun_len=%0d, overrun 80B combined otr_l4+tunnel_opt chunk segments\n", parserInfo->otr_tun_len);
            otr_tun_len = 18;
        }
        else {
            otr_tun_len = parserInfo->otr_tun_len;
        }
        for (int i = 0; i < (8 + otr_tun_len*4); i++, idx++)
        {
            if (i < 40)
                chunkedSeg->otr_l4[i] = packet[idx];
            else
                chunkedSeg->tun_opt[i-40] = packet[idx];
        }
        // it is not l4 hdr size, you can think of it is the tunnel hdr len in
        // l4 chunk and tunnel chunk
        chunkedSeg->tun_size_in_l4_chunk = (otr_tun_len < 8) ? otr_tun_len : 8;
        chunkedSeg->tun_opt_size = (otr_tun_len < 8) ? 0 : otr_tun_len - 8;
    }
    else if (chunkedSeg->otr_tcp_v)
    {
        for (int i = 0; i < 18; i++, idx++)
            chunkedSeg->otr_l4[i] = packet[idx];
        // TODO if tcp could followed by a tunnel hdr our codes in
        // UpdateL3LenCsum will fail since no tun_opt_size set for tcp
        // TODO comment out should be ok
        //chunkedSeg->tun_size_in_l4_chunk = 3;
    }
    else if (parserInfo->otr_tun_len) { // any tunnel protocol following L3 immediately
        if (parserInfo->otr_tun_len > 18) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->otr_tun_len=%0d, overrun 80B combined otr_l4+tunnel_opt chunk segments\n", parserInfo->otr_tun_len);
            otr_tun_len = 18;
        }
        else {
            otr_tun_len = parserInfo->otr_tun_len;
        }
        for (int i = 0; i < otr_tun_len*4; i++, idx++)
        {
            if (i < 40)
                chunkedSeg->otr_l4[i] = packet[idx];
            else
                chunkedSeg->tun_opt[i-40] = packet[idx];
        }
        chunkedSeg->tun_size_in_l4_chunk = (otr_tun_len < 10) ? otr_tun_len : 10;
        chunkedSeg->tun_opt_size = (otr_tun_len < 10) ? 0 : otr_tun_len - 10;
    }

    /* Inner */
    if (parserInfo->inr_l2_len > 5) {
        //D
        PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->inr_l2_len=%0d, overrun 30B inr_l2 chunk segment\n", parserInfo->inr_l2_len);
        inr_l2_len = 5;
    }
    else {
        inr_l2_len = parserInfo->inr_l2_len;
    }
    if (inr_l2_len > 0)
	{
		/* ETH */
        chunkedSeg->inr_l2_v = 1;
        for (int i = 0; i < 6; i++, idx++)
			chunkedSeg->inr_dmac[i] = packet[idx];
		for (int i = 0; i < 6; i++, idx++)
			chunkedSeg->inr_smac[i] = packet[idx];
		/* VLAN & Custom Tags */
		chunkedSeg->n_inr_tag = parserInfo->inr_l2_len - 1;
		for (int i = 0; i < (4*chunkedSeg->n_inr_tag); i++, idx++)
			chunkedSeg->inr_tags[i] = packet[idx];
	    /* Etype */
        for (int i = 0; i < 2; i++, idx++)
			chunkedSeg->inr_et[i] = packet[idx];
        /* INR MPLS */
        if (parserInfo->inr_mpls_len > 7) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->inr_mpls_len=%0d, overrun 28B inr_mpls chunk segment\n", parserInfo->inr_mpls_len);
            chunkedSeg->n_inr_mpls = 7;
        }
        else {
            chunkedSeg->n_inr_mpls = parserInfo->inr_mpls_len;
        }
        for (int i = 0; i < (4*chunkedSeg->n_inr_mpls); i++, idx++)
            chunkedSeg->inr_mpls[i] = packet[idx];
    }
    {
        /* INR L3 */
        if (parserInfo->inr_l3_len > 14) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: parserInfo->inr_l3_len=%0d, overrun 56B inr_l3 chunk segment\n", parserInfo->inr_l3_len);
            chunkedSeg->inr_ip_size = 14;
        }
        else {
            chunkedSeg->inr_ip_size = parserInfo->inr_l3_len;
        }
        chunkedSeg->inr_l3_v6 = parserInfo->inr_l3_v6;
		for (int i = 0; i < (chunkedSeg->inr_ip_size*4); i++, idx++)
			chunkedSeg->inr_ip[i] = packet[idx];
        /* INR L4 */
        if (parserInfo->inr_l4_udp && parserInfo->inr_l4_tcp) {
            //D
            PRINT(modDisplayVerbose, key, "MOD_WM_WARNING: In no way parserInfo->inr_l4_udp and parserInfo->inr_l4_tcp should be both asserted\n");
        }
        chunkedSeg->inr_udp_v = parserInfo->inr_l4_udp;
        chunkedSeg->inr_tcp_v = parserInfo->inr_l4_tcp;
        if (parserInfo->inr_l4_udp)
        {
			for (int i = 0; i < 8; i++, idx++)
				chunkedSeg->inr_l4[i] = packet[idx];
		}
        if (parserInfo->inr_l4_tcp)
        {
			for (int i = 0; i < 18; i++, idx++)
				chunkedSeg->inr_l4[i] = packet[idx];
		}
	}
	chunkedSeg->payload_start = idx;
	chunkedSeg->payload_size = rx_length - idx;

    PrintChunkedSeg(key, chunkedSeg);

    return status;
}

void GetModRegData(fm_text key,
                   hlp_model *model,
                   hlp_modelModRegData *r)
{
    hlp_modelState                  *state;
    fm_uint64               *modPerPortCfg1;
    fm_uint64               *modPerPortCfg2;
    fm_uint64               *modAqmProfile;
    fm_uint64               *modWredLevel;

    FM_NOT_USED(key);

    state = &model->packetState;

    modPerPortCfg1 = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_PER_PORT_CFG1(state->TX_PORT, 0));
    r->modPerPortCfg1.ENABLE_VLAN_UPDATE = FM_GET_BIT64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, ENABLE_VLAN_UPDATE);
    r->modPerPortCfg1.VID2_MAP_INDEX = FM_GET_FIELD64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, VID2_MAP_INDEX);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT = FM_GET_FIELD64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_GLORT);
    r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK = FM_GET_FIELD64(
        *modPerPortCfg1, HLP_MOD_PER_PORT_CFG1, LOOPBACK_SUPPRESS_MASK);

    modPerPortCfg2 = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_PER_PORT_CFG2(state->TX_PORT, 0));
    r->modPerPortCfg2.ENABLE_DMAC_ROUTING = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_DMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_SMAC_ROUTING = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_SMAC_ROUTING);
    r->modPerPortCfg2.ENABLE_TTL_DECREMENT = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_TTL_DECREMENT);
    r->modPerPortCfg2.ENABLE_ECN_MODIFICATION = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_ECN_MODIFICATION);
    r->modPerPortCfg2.VLAN2_E_TYPE = FM_GET_FIELD64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VLAN2_E_TYPE);
    r->modPerPortCfg2.VLAN1_E_TYPE = FM_GET_FIELD64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VLAN1_E_TYPE);
    r->modPerPortCfg2.ENABLE_DEI2_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_DEI2_UPDATE);
    r->modPerPortCfg2.ENABLE_DEI1_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_DEI1_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP2_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_PCP2_UPDATE);
    r->modPerPortCfg2.ENABLE_PCP1_UPDATE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, ENABLE_PCP1_UPDATE);
    r->modPerPortCfg2.VID2_FIRST = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VID2_FIRST);
    r->modPerPortCfg2.VLAN_TAGGING = FM_GET_FIELD64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, VLAN_TAGGING);
    r->modPerPortCfg2.MIN_FRAME_SIZE = FM_GET_BIT64(
        *modPerPortCfg2, HLP_MOD_PER_PORT_CFG2, MIN_FRAME_SIZE);

}

/*****************************************************************************/
/** DecrementDescIdx
 * \ingroup intModel
 *
 * \desc            Decrements the descriptor index and increment the data
 * 					index, if necessary. Both are used in UnpackModDesc.
 *
 * \param[in]       *data_idx is a pointer to the data index
 *
 * \param[in]		*desc_idx is a pointer to the descriptor index
 *
 * \param[in]		size is the number of bits the descriptor index is being
 * 					decremented.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status DecrementDescIdx(fm_int *data_idx,
						          fm_int *desc_idx,
						          fm_int size)
{
	fm_status status;
	status = FM_OK;

    if (size < 1/*FIELD_WIDTH_MIN*/ || size > 14/*FILED_WIDTH_MAX*/)
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "size (%d) is out of valid range (%d ... %d)", size, 1, 9);

	if ( (*desc_idx - size) >= 0 )
		*desc_idx -= size;
	else
	{
		*desc_idx = (64 - size);
        if (*data_idx + 1 > 3) {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "data_idx out of bound\n");
		    status = FM_FAIL;
        }
        else
		    *data_idx += 1;

	}
	return status;
}

/*****************************************************************************/
/** UnpackModDesc
 * \ingroup intModel
 *
 * \desc            Grabs desc from registers and unpacks it into structure
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]		desc points to the unpacked descriptor.
 *
 * \param[in]		bank is the bank # based on the MOD_MASTER table.
 *
 * \param[in]		A is the pointer into the MOD_DESC table from the
 * 					MOD_MASTER table.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
/*
typedef struct _mod_desc_unpack_meta {
    fm_uint64 data[4];
    fm_int data_idx;
    fm_int desc_idx;
} mod_desc_unpack_meta

static void initialize(mod_desc_unpack_meta *s, hlp_model *model, fm_byte bank, fm_uint16 A) {
	fm_uint64               *modDesc;

    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4), 0));
	s->data[0] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4+1), 0));
	s->data[1] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4+2), 0));
	s->data[2] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4+3), 0));
	s->data[3] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);

	s->data_idx = 0;
	s->desc_idx = (64 - ((A%4)*16)); // A%4 is start ptr (0,1,2,3)
}

static void DecrementDescIdx(mod_desc_unpack_meta *s, fm_uint16 fw) {
    if (size < 1 || size > 14)
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "size (%d) is out of valid range (%d ... %d)", size, 1, 9);

	if ((s->desc_idx-fw) >= 0)
		s->desc_idx -= fw;
	else {
		s->desc_idx = (64-fw);
        if (s->data_idx + 1 > 3)
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM, "data_idx out of bound\n");
        else
		    s->data_idx += 1;
	}
}

static fm_uin16 getField(mod_desc_unpack_meta *s, fm_uint16 fieldWidth) {
    DecrementDescIdx(s, fieldWidth);
    return ((data[s->data_idx] >> s->desc_idx) & (1U<<fieldWidth - 1));
}
*/
static fm_status UnpackModDesc(hlp_model        *model,
				               hlp_modelModDesc *desc,
		                       fm_byte          bank,
		                       fm_uint16        A,
                               fm_text          key)
{
    fm_status               status;

	fm_uint64               *modDesc;
    fm_uint64 				*modPreload;

	fm_uint64				data[4];
    fm_uint16				preload_otr1;
    fm_uint16				preload_otr2;
    fm_uint16				preload_mpls;
    fm_uint16				preload_qos;
    fm_uint16				preload_inr;
    fm_uint16				preload_meta[3];

	int                     data_idx;
    int     				desc_idx;
    // a struct desc_pre_unpack with data[4]/data_idx/desc_idx

    status = FM_OK;
    if(A == 0) {
        PRINT2(modDisplayVerbose, key, "pointer is 0, skip MOD_DESC\n");
        return status;
    }

    // desc_pre_unpack.initialize()
    /* Get modDesc data from Registers */
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4), 0));
	data[0] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4+1), 0));
	data[1] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4+2), 0));
	data[2] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);
    modDesc = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_DESC(bank, (A/4+3), 0));
	data[3] = FM_GET_FIELD64(*modDesc, HLP_MOD_DESC, DATA);

    /* Define starting indexes */
	data_idx = 0;
	desc_idx = (64 - ((A%4)*16)); /* A%4 is start ptr (0,1,2,3) */

    PRINT2(modDisplayVerbose, key, "data_idx = %0d, desc_idx = %0d\n", data_idx, desc_idx);
    PRINT2(modDisplayVerbose, key, "MOD_DESC[%0d][%0d] = 0x%016llx\n", bank, (A/4), data[0]);
    PRINT2(modDisplayVerbose, key, "MOD_DESC[%0d][%0d] = 0x%016llx\n", bank, (A/4+1), data[1]);
    PRINT2(modDisplayVerbose, key, "MOD_DESC[%0d][%0d] = 0x%016llx\n", bank, (A/4+2), data[2]);
    PRINT2(modDisplayVerbose, key, "MOD_DESC[%0d][%0d] = 0x%016llx\n", bank, (A/4+3), data[3]);

    /* Grab Preload Cmd from start of Descriptor */
	status = DecrementDescIdx(&data_idx, &desc_idx, 4);
	desc->base_cmd.preload_cmd = (data[data_idx] >> desc_idx) & 0x0F;

    /* Get modPreload data from Registers */
	modPreload = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_PRELOAD0(bank, desc->base_cmd.preload_cmd, 0));
	preload_otr1 = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD0, OTR1);
	preload_otr2 = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD0, OTR2);
	preload_mpls = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD0, MPLS);
	preload_qos = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD0, QOS);

	modPreload = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_PRELOAD1(bank, desc->base_cmd.preload_cmd, 0));
	preload_inr = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD1, INR);
	preload_meta[0] = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD1, META0);
	preload_meta[1] = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD1, META1);
	preload_meta[2] = FM_GET_FIELD(*modPreload, HLP_MOD_PRELOAD1, META2);

    if (desc->base_cmd.preload_cmd != 0)
    {
        PRINT2(modDisplayVerbose, key, "preload_cmd ::  otr1_cmd:0x%04x otr2_cmd:0x%04x "
            "mpls_cmd:0x%04x qos_cmd:0x%04x inr_cmd:0x%04x "
            "meta0_cmd:0x%04x meta1_cmd:0x%04x meta2_cmd:0x%04x\n",
            preload_otr1, preload_otr2, preload_mpls,
            preload_qos, preload_inr, preload_meta[0], preload_meta[1],
            preload_meta[2]);
    }

    /* Base CMD */
    // desc->base_cmd.otr1_cmd = desc_pre_unpack.getField(fieldWidth);
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);/*X the width of the field could be the return val of DecrementDescIdx, repetitive operations script generated? Or a more clever way?*/
	desc->base_cmd.otr1_cmd    = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.otr2_cmd    = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.mpls_cmd   = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.qos_cmd   = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.inr_cmd     = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.meta_cmd[0] = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.meta_cmd[1] = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.meta_cmd[2] = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.otr_dmac    = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.otr_smac    = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.dglort      = (data[data_idx] >> desc_idx) & 0x01;
	status = DecrementDescIdx(&data_idx, &desc_idx, 1);
	desc->base_cmd.reserved    = (data[data_idx] >> desc_idx) & 0x01;

    /* OTR1 CMD */
	if ( desc->base_cmd.otr1_cmd )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_sip     = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_dip     = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_ipv6    = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_ipflo   = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_prot    = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_l4src   = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_l4dst   = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->otr1_cmd.otr_dip_mac = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 4);
		desc->otr1_cmd.tunnel_mode = ((data[data_idx] >> desc_idx) & 0x0F);
		status = DecrementDescIdx(&data_idx, &desc_idx, 4);
		desc->otr1_cmd.tunnel_data = ((data[data_idx] >> desc_idx) & 0x0F);
        if(desc->otr1_cmd.otr_sip | desc->otr1_cmd.otr_dip |
           desc->otr1_cmd.otr_ipflo | desc->otr1_cmd.otr_prot)
            desc->otr1_ipv_present = 1;
        desc->otr1_present = 1;
	}
    if ( preload_otr1 != 0 )
    {
		desc->otr1_cmd.otr_sip     |= (preload_otr1 >> 15 & 0x01);
		desc->otr1_cmd.otr_dip     |= (preload_otr1 >> 14 & 0x01);
		desc->otr1_cmd.otr_ipv6    |= (preload_otr1 >> 13 & 0x01);
		desc->otr1_cmd.otr_ipflo   |= (preload_otr1 >> 12 & 0x01);
		desc->otr1_cmd.otr_prot    |= (preload_otr1 >> 11 & 0x01);
		desc->otr1_cmd.otr_l4src   |= (preload_otr1 >> 10 & 0x01);
		desc->otr1_cmd.otr_l4dst   |= (preload_otr1 >> 9 & 0x01);
		desc->otr1_cmd.otr_dip_mac |= (preload_otr1 >> 8 & 0x01);
		desc->otr1_cmd.tunnel_mode |= (preload_otr1 >> 4 & 0x0F);
		desc->otr1_cmd.tunnel_data |= (preload_otr1 >> 0 & 0x0F);
        if(desc->otr1_cmd.otr_sip | desc->otr1_cmd.otr_dip |
           desc->otr1_cmd.otr_ipflo | desc->otr1_cmd.otr_prot)
            desc->otr1_ipv_present = 1;
        desc->otr1_present = 1; // TODO, maycalled, otr1_l3mod_present
    }
    /* OTR2 CMD */
	if ( desc->base_cmd.otr2_cmd )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 14);
		desc->otr2_cmd.tunnel_data = ((data[data_idx] >> desc_idx) & 0x07FFF);
		status = DecrementDescIdx(&data_idx, &desc_idx, 2);
		desc->otr2_cmd.reserved    = ((data[data_idx] >> desc_idx) & 0x03);
        desc->otr2_present = 1;
	}
    if ( preload_otr2 != 0 )
    {
		desc->otr2_cmd.tunnel_data |= (preload_otr2 >> 2 & 0x07FFF);
		desc->otr2_cmd.reserved    |= (preload_otr2 >> 0 & 0x03);
        desc->otr2_present = 1;
    }
    /* MPLS CMD */
	if ( desc->base_cmd.mpls_cmd )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 3);
		desc->mpls_cmd.mpls_pop     = ((data[data_idx] >> desc_idx) & 0x07);
		status = DecrementDescIdx(&data_idx, &desc_idx, 4);
		desc->mpls_cmd.mpls_push    = ((data[data_idx] >> desc_idx) & 0x0F);
        status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->mpls_cmd.pop_al    = ((data[data_idx] >> desc_idx) & 0x01);
        status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->mpls_cmd.pop_eli    = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->mpls_cmd.push_al      = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->mpls_cmd.push_eli     = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 3);
	    desc->mpls_cmd.otr_tag_mode = ((data[data_idx] >> desc_idx) & 0x07);
	    status = DecrementDescIdx(&data_idx, &desc_idx, 2);
	    desc->mpls_cmd.otr_tag_size = ((data[data_idx] >> desc_idx) & 0x03);
	}
    if ( preload_mpls != 0 )
    {
		desc->mpls_cmd.mpls_pop     |= (preload_mpls >> 13 & 0x07);
		desc->mpls_cmd.mpls_push    |= (preload_mpls >> 9 & 0x0F);
		desc->mpls_cmd.pop_al       |= (preload_mpls >> 8 & 0x01);
		desc->mpls_cmd.pop_eli      |= (preload_mpls >> 7 & 0x01);
		desc->mpls_cmd.push_al      |= (preload_mpls >> 6 & 0x01);
		desc->mpls_cmd.push_eli     |= (preload_mpls >> 5 & 0x01);
	    desc->mpls_cmd.otr_tag_mode |= (preload_mpls >> 2 & 0x07);
	    desc->mpls_cmd.otr_tag_size |= (preload_mpls >> 0 & 0x03);
    }
    /* QOS CMD */
	if ( desc->base_cmd.qos_cmd )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 3);
		desc->qos_cmd.ds_src      = ((data[data_idx] >> desc_idx) & 0x07);
		status = DecrementDescIdx(&data_idx, &desc_idx, 3);
		desc->qos_cmd.ttl_src     = ((data[data_idx] >> desc_idx) & 0x07);
		status = DecrementDescIdx(&data_idx, &desc_idx, 2);
		desc->qos_cmd.ttl_dec     = ((data[data_idx] >> desc_idx) & 0x03);
		status = DecrementDescIdx(&data_idx, &desc_idx, 3);
		desc->qos_cmd.ttlds_tgt   = ((data[data_idx] >> desc_idx) & 0x07);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->qos_cmd.ds_wr       = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->qos_cmd.ttl_wr      = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 2);
		desc->qos_cmd.aqm_mark_ctrl      = ((data[data_idx] >> desc_idx) & 0x03);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->qos_cmd.reserved    = ((data[data_idx] >> desc_idx) & 0x01);
        desc->qos_present = 1;
	}
    if ( preload_qos != 0 )
    {
		desc->qos_cmd.ds_src      |= (preload_qos >> 13 & 0x07);
		desc->qos_cmd.ttl_src     |= (preload_qos >> 10 & 0x07);
		desc->qos_cmd.ttl_dec     |= (preload_qos >> 8 & 0x03);
		desc->qos_cmd.ttlds_tgt   |= (preload_qos >> 5 & 0x07);
		desc->qos_cmd.ds_wr       |= (preload_qos >> 4 & 0x01);
		desc->qos_cmd.ttl_wr      |= (preload_qos >> 3 & 0x01);
		desc->qos_cmd.aqm_mark_ctrl  |= (preload_qos >> 1 & 0x03);
		desc->qos_cmd.reserved    |= (preload_qos >> 0 & 0x01);
        desc->qos_present = 1;
    }
    /* INR CMD */
	if ( desc->base_cmd.inr_cmd )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.vni          = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_dmac     = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_smac     = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_sip      = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_dip      = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_ipv6     = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_dip_mac  = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_ds       = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_ttl      = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_l4src    = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 1);
		desc->inr_cmd.inr_l4dst    = ((data[data_idx] >> desc_idx) & 0x01);
		status = DecrementDescIdx(&data_idx, &desc_idx, 3);
		desc->inr_cmd.inr_tag_mode = ((data[data_idx] >> desc_idx) & 0x07);
		status = DecrementDescIdx(&data_idx, &desc_idx, 2);
		desc->inr_cmd.inr_tag_size = ((data[data_idx] >> desc_idx) & 0x03);
        desc->inr_present = 1;
        if(desc->inr_cmd.inr_sip | desc->inr_cmd.inr_dip)
            desc->inr_ipv_present = 1;
	}
    if ( preload_inr != 0 )
    {
		desc->inr_cmd.vni          |= (preload_inr >> 15 & 0x01);
		desc->inr_cmd.inr_dmac     |= (preload_inr >> 14 & 0x01);
		desc->inr_cmd.inr_smac     |= (preload_inr >> 13 & 0x01);
		desc->inr_cmd.inr_sip      |= (preload_inr >> 12 & 0x01);
		desc->inr_cmd.inr_dip      |= (preload_inr >> 11 & 0x01);
		desc->inr_cmd.inr_ipv6     |= (preload_inr >> 10 & 0x01);
		desc->inr_cmd.inr_dip_mac  |= (preload_inr >> 9 & 0x01);
		desc->inr_cmd.inr_ds       |= (preload_inr >> 8 & 0x01);
		desc->inr_cmd.inr_ttl      |= (preload_inr >> 7 & 0x01);
		desc->inr_cmd.inr_l4src    |= (preload_inr >> 6 & 0x01);
		desc->inr_cmd.inr_l4dst    |= (preload_inr >> 5 & 0x01);
		desc->inr_cmd.inr_tag_mode |= (preload_inr >> 2 & 0x07);
		desc->inr_cmd.inr_tag_size |= (preload_inr >> 0 & 0x03);
        desc->inr_present = 1;
    }
    /* META CMDs */
	for ( int i = 0; i < 3; i++ )
	{
		if ( desc->base_cmd.meta_cmd[i] )
        {
			status = DecrementDescIdx(&data_idx, &desc_idx, 4);
			desc->meta_cmd[i].source = ((data[data_idx] >> desc_idx) & 0x0F);
			status = DecrementDescIdx(&data_idx, &desc_idx, 3);
			desc->meta_cmd[i].count  = ((data[data_idx] >> desc_idx) & 0x07);
			status = DecrementDescIdx(&data_idx, &desc_idx, 9);
			desc->meta_cmd[i].dest   = ((data[data_idx] >> desc_idx) & 0x01FF);
            desc->meta_present[i] = 1;
        }
        if ( preload_meta[i] != 0 )
        {
			desc->meta_cmd[i].source |= (preload_meta[i] >> 12 & 0x0F);
			desc->meta_cmd[i].count  |= (preload_meta[i] >> 9 & 0x07);
			desc->meta_cmd[i].dest   |= (preload_meta[i] >> 0 & 0x01FF);
            desc->meta_present[i] = 1;
        }
	}

    /* DATA that is avaliable based on CMD booleans */
    if ( desc->base_cmd.otr_dmac )
    {
    	for ( int i = 0; i <= 5; i++ )
    	{
            status = DecrementDescIdx(&data_idx, &desc_idx, 8);
    		desc->otr_dmac[i] = (data[data_idx] >> desc_idx) & 0x0FF;
    	}
    }
    if ( desc->base_cmd.otr_smac )
    {
    	for ( int i = 0; i <= 5; i++ )
    	{
            status = DecrementDescIdx(&data_idx, &desc_idx, 8);
    		desc->otr_smac[i] = (data[data_idx] >> desc_idx) & 0x0FF;
    	}
    }
	if ( desc->base_cmd.dglort )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 8);
		desc->dglort[0] = (data[data_idx] >> desc_idx) & 0x0FF;
		status = DecrementDescIdx(&data_idx, &desc_idx, 8);
		desc->dglort[1] = (data[data_idx] >> desc_idx) & 0x0FF;
	}
	if ( desc->otr1_cmd.otr_sip )
	{
		for ( int i = 0; i <= (desc->otr1_cmd.otr_ipv6 ? 15 : 3); i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_sip[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->otr1_cmd.otr_dip )
	{
		for ( int i = 0; i <= (desc->otr1_cmd.otr_ipv6 ? 15 : 3); i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_dip[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->otr1_cmd.otr_ipflo )
	{
		for ( int i = 0; i <= 3; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_flow[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->otr1_cmd.otr_prot )
	{
		for ( int i = 0; i <= 1; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_prot[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->otr1_cmd.otr_l4src )
	{
		for ( int i = 0; i <= 1; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_l4src[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->otr1_cmd.otr_l4dst )
	{
        for ( int i = 0; i <= 1; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_l4dst[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->otr1_cmd.tunnel_data != 0 )
	{
		for ( int i = 0; i < 4; i++ )
		{
			if ( (desc->otr1_cmd.tunnel_data >> (3-i)) & 0x01 )
			{
				for ( int j = 0; j < 4; j++ )
				{
					status = DecrementDescIdx(&data_idx, &desc_idx, 8);
					desc->tunnel_data[(i*4) + j] = (data[data_idx] >> desc_idx) & 0x0FF;
				}
			}
		}
	}
	if ( desc->otr2_cmd.tunnel_data != 0 )
	{
		for ( int i = 4; i < 18; i++ )
		{
			if ( (desc->otr2_cmd.tunnel_data >> (17-i)) & 0x01 )
			{
				for ( int j = 0; j < 4; j++ )
				{
					status = DecrementDescIdx(&data_idx, &desc_idx, 8);
					desc->tunnel_data[(i*4) + j] = (data[data_idx] >> desc_idx) & 0x0FF;
				}
			}
		}
	}
	if ( desc->mpls_cmd.mpls_push != 0 )
	{
		for ( int i = 0; i < 4; i++ )
        {
            if ( (desc->mpls_cmd.mpls_push >> (3-i)) & 0x01 )
		    {
                for ( int j = 0; j < 4; j++ )
                {
                    status = DecrementDescIdx(&data_idx, &desc_idx, 8);
                    desc->mpls[(i*4) + j] = (data[data_idx] >> desc_idx) & 0x0FF;
                }
            }
        }
	}
	if ( desc->mpls_cmd.push_al != 0 )
	{
        for ( int j = 0; j < 4; j++ )
        {
            status = DecrementDescIdx(&data_idx, &desc_idx, 8);
            desc->al[j] = (data[data_idx] >> desc_idx) & 0x0FF;
        }
	}
	if ( desc->mpls_cmd.otr_tag_size > 0 && desc->mpls_cmd.otr_tag_size < 3)
	{
		/* otr_tag_size = # of 4B tags present for insertion */
		for ( int i = 0; i <= ((desc->mpls_cmd.otr_tag_size * 4) - 1); i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->otr_tags[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( (desc->qos_cmd.ds_src == 5) | (desc->qos_cmd.ttl_src == 5) )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 8);
		desc->otr_ds = (data[data_idx] >> desc_idx) & 0x0FF;
		status = DecrementDescIdx(&data_idx, &desc_idx, 8);
		desc->otr_ttl = (data[data_idx] >> desc_idx) & 0x0FF;
	}
	if ( desc->inr_cmd.vni )
	{
		for ( int i = 0; i <= 3; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->vni[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_dmac )
	{
		for ( int i = 0; i <= 5; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_dmac[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_smac )
	{
		for ( int i = 0; i <= 5; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_smac[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_sip )
	{
		for ( int i = 0; i <= (desc->inr_cmd.inr_ipv6 ? 15 : 3); i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_sip[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_dip )
	{
		for ( int i = 0; i <= (desc->inr_cmd.inr_ipv6 ? 15 : 3); i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_dip[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_ds  | desc->inr_cmd.inr_ttl )
	{
		status = DecrementDescIdx(&data_idx, &desc_idx, 8);
		desc->inr_ds = (data[data_idx] >> desc_idx) & 0x0FF;
		status = DecrementDescIdx(&data_idx, &desc_idx, 8);
		desc->inr_ttl = (data[data_idx] >> desc_idx) & 0x0FF;
	}
	if ( desc->inr_cmd.inr_l4src )
	{
		for ( int i = 0; i <= 1; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_l4src[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_l4dst )
	{
		for ( int i = 0; i <= 1; i++ )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_l4dst[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}
	if ( desc->inr_cmd.inr_tag_size > 0 )
	{
		/* otr_tag_size = # of 4B tags present for insertion */
		for ( int i = (desc->inr_cmd.inr_tag_size * 4) - 1; i >= 0; i-- )
		{
			status = DecrementDescIdx(&data_idx, &desc_idx, 8);
			desc->inr_tags[i] = (data[data_idx] >> desc_idx) & 0x0FF;
		}
	}

	return status;
}

/*****************************************************************************/
/** UpdateDescMaster
 * \ingroup intModel
 *
 * \desc            Merges two descriptors together
 *
 * \param[in]		descMaster points to the descriptor which will end with
 * 					all of the data.
 *
 * \param[in]		desc points to the descriptor that comes before descMaster.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status UpdateDescMaster(hlp_modelModDesc *descMaster,
				                  hlp_modelModDesc *desc)
{
	fm_status status;
	status = FM_OK; /*X the structure and magic numbers might be script generated*/

	/* Start by replacing the data */
    if ( ~descMaster->base_cmd.dglort & desc->base_cmd.dglort )
	{
		for ( int i = 1; i >= 0; i-- )
			descMaster->dglort[i] = desc->dglort[i];
	}
	if ( ~descMaster->base_cmd.otr_dmac & desc->base_cmd.otr_dmac )
	{
		for ( int i = 5; i >= 0; i-- )
			descMaster->otr_dmac[i] = desc->otr_dmac[i];
	}
	if ( ~descMaster->base_cmd.otr_smac & desc->base_cmd.otr_smac )
	{
		for ( int i = 5; i >= 0; i-- )
			descMaster->otr_smac[i] = desc->otr_smac[i];
	}
	if ( ~descMaster->otr1_cmd.otr_sip & desc->otr1_cmd.otr_sip )
	{
		for ( int i = 15; i >= 0; i-- )
			descMaster->otr_sip[i] = desc->otr_sip[i];
	}
	if ( ~descMaster->otr1_cmd.otr_dip & desc->otr1_cmd.otr_dip )
	{
		for ( int i = 15; i >= 0; i-- )
			descMaster->otr_dip[i] = desc->otr_dip[i];
	}
	if ( ~descMaster->otr1_cmd.otr_ipflo & desc->otr1_cmd.otr_ipflo )
	{
		for ( int i = 3; i >= 0; i-- )
			descMaster->otr_flow[i] = desc->otr_flow[i];
	}
	if ( ~descMaster->otr1_cmd.otr_prot & desc->otr1_cmd.otr_prot )
	{
		for ( int i = 1; i >= 0; i-- )
			descMaster->otr_prot[i] = desc->otr_prot[i];
	}
	if ( ~descMaster->otr1_cmd.otr_l4src & desc->otr1_cmd.otr_l4src )
	{
		for ( int i = 1; i >= 0; i-- )
			descMaster->otr_l4src[i] = desc->otr_l4src[i];
	}
	if ( ~descMaster->otr1_cmd.otr_l4dst & desc->otr1_cmd.otr_l4dst )
	{
		for ( int i = 1; i >= 0; i-- )
			descMaster->otr_l4dst[i] = desc->otr_l4dst[i];
	}
	if ( desc->otr1_cmd.tunnel_data != 0 )
	{
        for ( int i = 0; i < 4; i++ )
        {
            if ( (((descMaster->otr1_cmd.tunnel_data >> (3-i)) & 0x01) == 0) &
                  ((desc->otr1_cmd.tunnel_data >> (3-i)) & 0x01))
            {
                for ( int j = 0; j < 4; j++ )
                    descMaster->tunnel_data[(i*4) + j] = desc->tunnel_data[(i*4) + j];
			}
	    }
	}
	if ( desc->otr2_cmd.tunnel_data != 0 )
	{
        for ( int i = 4; i < 18; i++ )
        {
            if ( (((descMaster->otr2_cmd.tunnel_data >> (17-i)) & 0x01) == 0) &
                  ((desc->otr2_cmd.tunnel_data >> (17-i)) & 0x01))
            {
                for ( int j = 0; j < 4; j++ )
                    descMaster->tunnel_data[(i*4) + j] = desc->tunnel_data[(i*4) + j];
            }
        }
	}
	if ( (descMaster->mpls_cmd.mpls_push != 0) &
		 (desc->mpls_cmd.mpls_push != 0) )
	{
		for ( int i = 0; i < 4; i++ )
        {
            if ( (((descMaster->mpls_cmd.mpls_push >> (3-i)) & 0x01) == 0) &
			  ((desc->mpls_cmd.mpls_push >> (3-i)) & 0x01))
		    {
                for ( int j = 0; j < 4; j++ )
                    descMaster->mpls[(i*4) + j] = desc->mpls[(i*4) + j];
            }
        }
	}
	else if ( (descMaster->mpls_cmd.mpls_push == 0) &
			  (desc->mpls_cmd.mpls_push != 0) )
	{
		for ( int i = 15; i >= 0; i-- )
			descMaster->mpls[i] = desc->mpls[i];
	}
	if ( ~descMaster->mpls_cmd.push_al & desc->mpls_cmd.push_al )
	{
		descMaster->mpls_cmd.push_al = desc->mpls_cmd.push_al;
        for (int i=0; i<4; i++)
            descMaster->al[i] = desc->al[i];
	}
	if ( ~descMaster->mpls_cmd.push_eli & desc->mpls_cmd.push_eli )
	{
		descMaster->mpls_cmd.push_eli = desc->mpls_cmd.push_eli;
	}
	if ( (descMaster->mpls_cmd.otr_tag_size == 0) &
		 (desc->mpls_cmd.otr_tag_size > 0) )
	{
		for ( int i = 7; i >= 0; i-- )
			descMaster->otr_tags[i] = desc->otr_tags[i];
	}
	if ( (descMaster->qos_cmd.ds_src == 0) & (desc->qos_cmd.ds_src != 0) )
	{
		descMaster->otr_ds = desc->otr_ds;
	}
	if ( (descMaster->qos_cmd.ttl_src == 0) & (desc->qos_cmd.ttl_src != 0) )
	{
		descMaster->otr_ttl = desc->otr_ttl;
	}
	if ( ~descMaster->inr_cmd.vni & desc->inr_cmd.vni )
	{
		for ( int i = 3; i >= 0; i-- )
			descMaster->vni[i] = desc->vni[i];
	}
	if ( ~descMaster->inr_cmd.inr_dmac & desc->inr_cmd.inr_dmac )
	{
		for ( int i = 5; i >= 0; i-- )
			descMaster->inr_dmac[i] = desc->inr_dmac[i];
	}
	if ( ~descMaster->inr_cmd.inr_smac & desc->inr_cmd.inr_smac)
	{
		for ( int i = 5; i >= 0; i-- )
			descMaster->inr_smac[i] = desc->inr_smac[i];
	}
	if ( ~descMaster->inr_cmd.inr_sip & desc->inr_cmd.inr_sip )
	{
		for ( int i = 15; i >= 0; i-- )
			descMaster->inr_sip[i] = desc->inr_sip[i];
	}
	if ( ~descMaster->inr_cmd.inr_dip & desc->inr_cmd.inr_dip )
	{
		for ( int i = 15; i >= 0; i-- )
			descMaster->inr_dip[i] = desc->inr_dip[i];
	}
	if ( ~descMaster->inr_cmd.inr_ds & desc->inr_cmd.inr_ds )
	{
		descMaster->inr_ds = desc->inr_ds;
	}
	if ( ~descMaster->inr_cmd.inr_ttl & desc->inr_cmd.inr_ttl )
	{
		descMaster->inr_ttl = desc->inr_ttl;
	}
	if ( ~descMaster->inr_cmd.inr_l4src & desc->inr_cmd.inr_l4src )
	{
		for ( int i = 1; i >= 0; i-- )
			descMaster->inr_l4src[i] = desc->inr_l4src[i];
	}
	if ( ~descMaster->inr_cmd.inr_l4dst & desc->inr_cmd.inr_l4dst )
	{
		for ( int i = 1; i >= 0; i-- )
			descMaster->inr_l4dst[i] = desc->inr_l4dst[i];
	}
	if ( (descMaster->inr_cmd.inr_tag_size == 0) &
		 (desc->inr_cmd.inr_tag_size > 0) )
	{
		for ( int i = 7; i >= 0; i-- )
			descMaster->inr_tags[i] = desc->inr_tags[i];
	}

    /* Replace the commands so we know what data is avaliable */
    /* OTR1 CMD */
    // otr_dip_mac implies otr_ipv6

    if ( ( desc->otr1_cmd.otr_sip | desc->otr1_cmd.otr_dip |
desc->otr1_cmd.otr_ipflo | desc->otr1_cmd.otr_prot) &
        ~( descMaster->otr1_cmd.otr_sip | descMaster->otr1_cmd.otr_dip |
descMaster->otr1_cmd.otr_ipflo | descMaster->otr1_cmd.otr_prot |
descMaster->otr1_cmd.otr_dip_mac) ) {
        descMaster->otr1_cmd.otr_ipv6 = desc->otr1_cmd.otr_ipv6;
        descMaster->otr1_ipv_present |= desc->otr1_ipv_present;
    }
    descMaster->otr1_cmd.otr_sip     |= desc->otr1_cmd.otr_sip;
    descMaster->otr1_cmd.otr_dip     |= desc->otr1_cmd.otr_dip;
    descMaster->otr1_cmd.otr_ipflo   |= desc->otr1_cmd.otr_ipflo;
    descMaster->otr1_cmd.otr_prot    |= desc->otr1_cmd.otr_prot;
    descMaster->otr1_cmd.otr_l4src   |= desc->otr1_cmd.otr_l4src;
    descMaster->otr1_cmd.otr_l4dst   |= desc->otr1_cmd.otr_l4dst;
    //if(descMaster->otr1_cmd.otr_dip_mac)
    //      descMaster->otr1_cmd.otr_ipv6 = 1;
    descMaster->otr1_cmd.otr_dip_mac |= desc->otr1_cmd.otr_dip_mac;
    descMaster->otr1_present |= desc->otr1_present;

    if ( descMaster->otr1_cmd.tunnel_mode == 0 )
        descMaster->otr1_cmd.tunnel_mode = desc->otr1_cmd.tunnel_mode;
    descMaster->otr1_cmd.tunnel_data |= desc->otr1_cmd.tunnel_data;
    /* OTR2 CMD */
    descMaster->otr2_cmd.tunnel_data |= desc->otr2_cmd.tunnel_data;
    /*MPLS CMD */
    /* mpls_pop is taken from descMaster */
    if ( descMaster->mpls_cmd.mpls_pop == 0 )
        descMaster->mpls_cmd.mpls_pop = desc->mpls_cmd.mpls_pop;
    descMaster->mpls_cmd.mpls_push    |= desc->mpls_cmd.mpls_push;
    descMaster->mpls_cmd.push_al      |= desc->mpls_cmd.push_al;
    descMaster->mpls_cmd.push_eli     |= desc->mpls_cmd.push_eli;
    descMaster->mpls_cmd.pop_al       |= desc->mpls_cmd.pop_al;
    descMaster->mpls_cmd.pop_eli      |= desc->mpls_cmd.pop_eli;
    if ( descMaster->mpls_cmd.otr_tag_mode == 0 )
        descMaster->mpls_cmd.otr_tag_mode = desc->mpls_cmd.otr_tag_mode;
    if ( descMaster->mpls_cmd.otr_tag_size == 0 )
        descMaster->mpls_cmd.otr_tag_size = desc->mpls_cmd.otr_tag_size;
	/* QOS CMD */
    if ( descMaster->qos_cmd.ds_src == 0)
        descMaster->qos_cmd.ds_src = desc->qos_cmd.ds_src;
    if ( descMaster->qos_cmd.ttl_src == 0)
        descMaster->qos_cmd.ttl_src = desc->qos_cmd.ttl_src;
    if ( descMaster->qos_cmd.ttl_dec == 0)
        descMaster->qos_cmd.ttl_dec = desc->qos_cmd.ttl_dec;
    if ( descMaster->qos_cmd.ttlds_tgt == 0)
        descMaster->qos_cmd.ttlds_tgt = desc->qos_cmd.ttlds_tgt;
    descMaster->qos_cmd.ds_wr       |= desc->qos_cmd.ds_wr;
    descMaster->qos_cmd.ttl_wr      |= desc->qos_cmd.ttl_wr;
    // FIXME
    if (descMaster->qos_cmd.aqm_mark_ctrl == 0)
        descMaster->qos_cmd.aqm_mark_ctrl    |= desc->qos_cmd.aqm_mark_ctrl;
    descMaster->qos_cmd.reserved    |= desc->qos_cmd.reserved;
    if ( ( desc->inr_cmd.inr_sip | desc->inr_cmd.inr_dip ) &
        ~( descMaster->inr_cmd.inr_sip | descMaster->inr_cmd.inr_dip ) )
        descMaster->inr_cmd.inr_ipv6 = desc->inr_cmd.inr_ipv6;
    /* INR CMD */
    descMaster->inr_cmd.vni          |= desc->inr_cmd.vni;
    descMaster->inr_cmd.inr_dmac     |= desc->inr_cmd.inr_dmac;
    descMaster->inr_cmd.inr_smac     |= desc->inr_cmd.inr_smac;
    descMaster->inr_cmd.inr_sip      |= desc->inr_cmd.inr_sip;
    descMaster->inr_cmd.inr_dip      |= desc->inr_cmd.inr_dip;
    descMaster->inr_cmd.inr_dip_mac  |= desc->inr_cmd.inr_dip_mac;
    descMaster->inr_cmd.inr_ds       |= desc->inr_cmd.inr_ds;
    descMaster->inr_cmd.inr_ttl      |= desc->inr_cmd.inr_ttl;
    descMaster->inr_cmd.inr_l4src    |= desc->inr_cmd.inr_l4src;
    descMaster->inr_cmd.inr_l4dst    |= desc->inr_cmd.inr_l4dst;
    if ( descMaster->inr_cmd.inr_tag_mode == 0 )
        descMaster->inr_cmd.inr_tag_mode = desc->inr_cmd.inr_tag_mode;
    if ( descMaster->inr_cmd.inr_tag_size == 0 )
        descMaster->inr_cmd.inr_tag_size = desc->inr_cmd.inr_tag_size;
	/* META CMDs */
    for ( int i = 0; i < 3; i++ )
	{
		if ( descMaster->meta_cmd[i].count == 0 ) {
			descMaster->meta_cmd[i] = desc->meta_cmd[i];
		}
	}
    /* Base CMD is last */
	descMaster->base_cmd.preload_cmd |= desc->base_cmd.preload_cmd;
	descMaster->base_cmd.otr1_cmd    |= desc->base_cmd.otr1_cmd;
	descMaster->base_cmd.otr2_cmd    |= desc->base_cmd.otr2_cmd;
	descMaster->base_cmd.mpls_cmd   |= desc->base_cmd.mpls_cmd;
	descMaster->base_cmd.qos_cmd   |= desc->base_cmd.qos_cmd;
	descMaster->base_cmd.inr_cmd     |= desc->base_cmd.inr_cmd;
	descMaster->base_cmd.meta_cmd[0] |= desc->base_cmd.meta_cmd[0];
	descMaster->base_cmd.meta_cmd[1] |= desc->base_cmd.meta_cmd[1];
	descMaster->base_cmd.meta_cmd[2] |= desc->base_cmd.meta_cmd[2];
	descMaster->base_cmd.dglort      |= desc->base_cmd.dglort;
	descMaster->base_cmd.otr_dmac    |= desc->base_cmd.otr_dmac;
	descMaster->base_cmd.otr_smac    |= desc->base_cmd.otr_smac;

	return status;
}

static fm_int DecMetaIdx(fm_int meta_idx, fm_int dec) {
    if(meta_idx - dec < 0)
        return 0;
    else
        return (meta_idx - dec);
}

/*****************************************************************************/
/** UpdateDescMeta
 * \ingroup intModel
 *
 * \desc            Updates Desc with Meta Data based on Meta Command.
 *
 * \param[in]		desc points to the descriptor which will end with
 * 					all of the data.
 *
 * \param[in]		modMeta is the meta data from Forward Modify
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status UpdateDescMeta(hlp_modelModDesc *desc,
				                fm_uint64         modMeta)
{
	fm_status               status;
	fm_byte					tmp_desc[220];
	fm_int 					meta_idx;
	int						desc_idx;
	fm_bool					even;

	status = FM_OK;

	memcpy(&tmp_desc, (void *)((unsigned long int)desc+sizeof(hlp_modelModDesc)-220), 220);

    modMeta = modMeta << 16;

    for (int i = 0; i < 3; i++)
	{
		if (desc->meta_cmd[i].count != 0)
		{
            /* Meta data is 48 bits of Big-Endian data */
			/* Nybble size is 4 bits */
			desc_idx = desc->meta_cmd[i].dest/2;
			even = (desc->meta_cmd[i].dest%2 == 0) ? 1 : 0;
			meta_idx = 64 - NYBBLE_BITS*(desc->meta_cmd[i].source+1);
            if (even) meta_idx = DecMetaIdx(meta_idx, 4);
			if (desc->meta_cmd[i].count < 6)
			{
				for (int j = 0; j < desc->meta_cmd[i].count; j++)
				{
					/* This if statement accounts for the fact that we are
					 * indexing by bytes but trying to keep track of nybbles
					 */
					if (even) /* dest is even */
					{
                        tmp_desc[desc_idx] = (tmp_desc[desc_idx] & 0x0F) |
											 ((modMeta >> meta_idx) &  0xF0);
						even = 0;
					}
					else /* dest is odd */
					{
                        tmp_desc[desc_idx] = (tmp_desc[desc_idx] & 0xF0) |
											 ((modMeta >> meta_idx) &  0x0F);

					    meta_idx = DecMetaIdx(meta_idx, 8);
                        desc_idx++;
						even = 1;
					}
				}
			}
			else if (desc->meta_cmd[i].count == 6)
			{
				for (int j = 0; j < 4; j++)
				{
					/* This if statement accounts for the fact that we are
					 * indexing by bytes but trying to keep track of nybbles
					 */
                    if (even) /* dest is even */
					{
                        tmp_desc[desc_idx] = (tmp_desc[desc_idx]) |
											 ((modMeta >> meta_idx) &  0xF0);
						even = 0;

					}
					else /* dest is odd */
					{
                        tmp_desc[desc_idx] = (tmp_desc[desc_idx]) |
											 ((modMeta >> meta_idx) &  0x0F);

						meta_idx = DecMetaIdx(meta_idx, 8);
                        desc_idx++;
						even = 1;
					}
				}
			}
			else if (desc->meta_cmd[i].count == 7)
			{
				for (int j = 0; j < 4; j++)
				{
					/* This if statement accounts for the fact that we are
					 * indexing by bytes but trying to keep track of nybbles
					 */
                    if (even) /* dest is even */
					{
                        tmp_desc[desc_idx] = (tmp_desc[desc_idx]) &
											 (((modMeta >> meta_idx) &  0xF0) |
                                             0x0F);
						even = 0;

					}
					else /* dest is odd */
					{
                        tmp_desc[desc_idx] = (tmp_desc[desc_idx]) &
											 (((modMeta >> meta_idx) &  0x0F) |
                                             0xF0);

                        meta_idx = DecMetaIdx(meta_idx, 8);
						desc_idx++;
						even = 1;
					}

				}
			}
		}
	}

	memcpy((void *)((unsigned long int)desc+sizeof(hlp_modelModDesc)-220), &tmp_desc, 220);

	return status;
}

/*****************************************************************************/
/** PrintDesc
 * \ingroup intModel
 *
 * \desc            Prints out the unpacked descriptor
 *
 * \param[in]		desc points to the descriptor will be printed.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static void PrintDesc(fm_text key, hlp_modelModDesc *desc)
{
	PRINT2(modDisplayVerbose, key, "      %10s ::  preload_cmd:0x%0x otr1_cmd:%0d otr2_cmd:%0d "
        "mpls_cmd:%0d qos_cmd:%0d inr_cmd:%0d meta_cmd0:%0d meta_cmd1:%0d "
        "meta_cmd2:%0d otr_dmac:%0d otr_smac:%0d dglort:%0d reserved:%0d\n",
        "base_cmd",
        desc->base_cmd.preload_cmd, desc->base_cmd.otr1_cmd,
		desc->base_cmd.otr2_cmd, desc->base_cmd.mpls_cmd,
		desc->base_cmd.qos_cmd, desc->base_cmd.inr_cmd,
		desc->base_cmd.meta_cmd[0], desc->base_cmd.meta_cmd[1],
		desc->base_cmd.meta_cmd[2], desc->base_cmd.otr_dmac,
		desc->base_cmd.otr_smac, desc->base_cmd.dglort,
		desc->base_cmd.reserved);

	if ( (desc->otr1_cmd.otr_sip   != 0) | (desc->otr1_cmd.otr_dip     != 0) |
         (desc->otr1_cmd.otr_ipv6  != 0) | (desc->otr1_cmd.otr_ipflo   != 0) |
         (desc->otr1_cmd.otr_prot  != 0) | (desc->otr1_cmd.otr_l4src   != 0) |
         (desc->otr1_cmd.otr_l4dst != 0) | (desc->otr1_cmd.otr_dip_mac != 0) |
         (desc->otr1_cmd.tunnel_mode != 0) |
         (desc->otr1_cmd.tunnel_data != 0) )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  otr_sip:%0d otr_dip:%0d otr_ipv6:%0d "
            "otr_ipflo:%0d otr_prot:%0d otr_l4src:%0d otr_l4dst:%0d "
            "odip_mac:%0d tunnel_mode:0x%0x tunnel_data[0:3]:0x%0x\n",
            "otr1_cmd",
			desc->otr1_cmd.otr_sip, desc->otr1_cmd.otr_dip,
			desc->otr1_cmd.otr_ipv6, desc->otr1_cmd.otr_ipflo,
			desc->otr1_cmd.otr_prot, desc->otr1_cmd.otr_l4src,
			desc->otr1_cmd.otr_l4dst, desc->otr1_cmd.otr_dip_mac,
			desc->otr1_cmd.tunnel_mode, desc->otr1_cmd.tunnel_data);
	}
	if ( desc->otr2_cmd.tunnel_data != 0 )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  tunnel_data[4:17]:0x%0x\n",
            "otr2_cmd", desc->otr2_cmd.tunnel_data);
	}
	if ( (desc->mpls_cmd.mpls_pop != 0) | (desc->mpls_cmd.mpls_push != 0) |
        (desc->mpls_cmd.pop_al  != 0) | (desc->mpls_cmd.pop_eli  != 0) |
         (desc->mpls_cmd.push_al  != 0) | (desc->mpls_cmd.push_eli  != 0) |
         (desc->mpls_cmd.otr_tag_mode != 0) |
         (desc->mpls_cmd.otr_tag_size != 0) )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  mpls_pop:0x%0x mpls_push:0x%0x push_al:%0d "
            "push_eli:%0d pop_al:%0d pop_eli:%0d otr_tag_mode:0x%0x "
            "otr_tag_size:0x%0x\n", "mpls_cmd",
            desc->mpls_cmd.mpls_pop, desc->mpls_cmd.mpls_push,
			desc->mpls_cmd.push_al, desc->mpls_cmd.push_eli,
            desc->mpls_cmd.pop_al, desc->mpls_cmd.pop_eli,
			desc->mpls_cmd.otr_tag_mode, desc->mpls_cmd.otr_tag_size);
	}
	if ( (desc->qos_cmd.ds_src  != 0) | (desc->qos_cmd.ttl_src   != 0) |
         (desc->qos_cmd.ttl_dec != 0) | (desc->qos_cmd.ttlds_tgt != 0) |
         (desc->qos_cmd.ds_wr   != 0) | (desc->qos_cmd.ttl_wr    != 0))
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  ds_src:0x%0x ttl_src:0x%0x ttl_dec:0x%0x "
            "ttlds_tgt:0x%0x ds_wr:%0d ttl_wr:%0d aqm_mark_ctrl:%0d\n",
            "qos_cmd",
			desc->qos_cmd.ds_src, desc->qos_cmd.ttl_src,
			desc->qos_cmd.ttl_dec, desc->qos_cmd.ttlds_tgt,
			desc->qos_cmd.ds_wr, desc->qos_cmd.ttl_wr,
            desc->qos_cmd.aqm_mark_ctrl);
	}
	if ( (desc->inr_cmd.vni         != 0) | (desc->inr_cmd.inr_dmac  != 0) |
         (desc->inr_cmd.inr_smac    != 0) | (desc->inr_cmd.inr_sip   != 0) |
         (desc->inr_cmd.inr_dip     != 0) | (desc->inr_cmd.inr_ipv6  != 0) |
         (desc->inr_cmd.inr_dip_mac != 0) | (desc->inr_cmd.inr_ds    != 0) |
         (desc->inr_cmd.inr_ttl     != 0) | (desc->inr_cmd.inr_l4src != 0) |
         (desc->inr_cmd.inr_l4dst   != 0) |
         (desc->inr_cmd.inr_tag_mode != 0) |
         (desc->inr_cmd.inr_tag_size != 0) )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  vni:%0d inr_dmac:%0d inr_smac:%0d inr_sip:%0d "
            "inr_dip:%0d inr_ipv6:%0d idip_mac:%0d inr_ds:%0d inr_ttl:%0d "
            "inr_l4src:%0d inr_l4dst:%0d inr_tag_mode:0x%0x "
            "inr_tag_size:0x%0x\n", "inr_cmd",
			desc->inr_cmd.vni, desc->inr_cmd.inr_dmac,
			desc->inr_cmd.inr_smac, desc->inr_cmd.inr_sip,
			desc->inr_cmd.inr_dip, desc->inr_cmd.inr_ipv6,
			desc->inr_cmd.inr_dip_mac, desc->inr_cmd.inr_ds,
			desc->inr_cmd.inr_ttl, desc->inr_cmd.inr_l4src,
			desc->inr_cmd.inr_l4dst, desc->inr_cmd.inr_tag_mode,
			desc->inr_cmd.inr_tag_size);
	}
	for ( int i = 0; i < 3; i++ )
	{
		if ( (desc->meta_cmd[i].source != 0) | (desc->meta_cmd[i].count != 0) |
             (desc->meta_cmd[i].dest != 0))
		{
			PRINT2(modDisplayVerbose, key, "   %10s[%d] ::  source:%0d count:%0d dest:%0d\n",
                "meta_cmd",
				i, desc->meta_cmd[i].source, desc->meta_cmd[i].count,
				desc->meta_cmd[i].dest);
		}
	}
    if ( desc->base_cmd.otr_dmac )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  ", "otr_dmac");
        for (int i = 0; i < 6; i++)
        {
            if (i != 5)
            {
                PRINTF2(modDisplayVerbose, "%02x:", desc->otr_dmac[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->otr_dmac[i]);
            }
        }
    }
    if ( desc->base_cmd.otr_smac )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  ", "otr_smac");
        for (int i = 0; i < 6; i++)
        {
            if (i != 5)
            {
                PRINTF2(modDisplayVerbose, "%02x:", desc->otr_smac[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->otr_smac[i]);
            }
        }
    }
    if ( desc->base_cmd.dglort )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  ", "dglort");
        for (int i = 0; i < 2; i++)
        {
            if (i != 1)
            {
                PRINTF2(modDisplayVerbose, "%02x:", desc->dglort[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->dglort[i]);
            }
        }
    }

    /* OTR1_CMD */
	if ( desc->otr1_cmd.otr_sip & desc->otr1_cmd.otr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "otr_sip(v6)");
        for (int i = 0; i < 16; i++)
        {
            if (i != 15)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->otr_sip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->otr_sip[i]);
            }
        }
	}
    if ( desc->otr1_cmd.otr_sip & !desc->otr1_cmd.otr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "otr_sip(v4)");
        for (int i = 0; i < 4; i++)
        {
            if (i != 3)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->otr_sip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->otr_sip[i]);
	        }
        }
    }
    if ( desc->otr1_cmd.otr_dip & desc->otr1_cmd.otr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "otr_dip(v6)");
        for (int i = 0; i < 16; i++)
        {
            if (i != 15)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->otr_dip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->otr_dip[i]);
            }
        }
	}
    if ( desc->otr1_cmd.otr_dip & !desc->otr1_cmd.otr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "otr_dip(v4)");
        for (int i = 0; i < 4; i++)
        {
            if (i != 3)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->otr_dip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->otr_dip[i]);
	        }
        }
    }
    if ( desc->otr1_cmd.otr_ipflo )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  ", "otr_ipflo");
        PRINTF2(modDisplayVerbose, "id:0x%02x%02x flags:0x%01x frag_offseg:0x%02x%02x\n",
            desc->otr_flow[0], desc->otr_flow[1],
            (desc->otr_flow[2] >> 5) & 0x07, desc->otr_flow[2] & 0x1F,
            desc->otr_flow[3]);
    }
    if ( desc->otr1_cmd.otr_prot )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  0x%02x%02x\n", "otr_prot",
            desc->otr_prot[0], desc->otr_prot[1]);
    }
    if ( desc->otr1_cmd.otr_l4src )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  0x%02x%02x\n", "otr_l4src",
            desc->otr_l4src[0], desc->otr_l4src[1]);
    }
    if ( desc->otr1_cmd.otr_l4dst )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  0x%02x%02x\n", "otr_l4dst",
            desc->otr_l4dst[0], desc->otr_l4dst[1]);
    }
    if ( desc->otr1_cmd.tunnel_data )
    {
        for ( int i = 0; i < 4; i++ )
        {
            if ((desc->otr1_cmd.tunnel_data >> (3-i)) & 0x01)
            {
                PRINT2(modDisplayVerbose, key, "   %10s[%d] ::  ", "tunnel_data", i);
                for ( int j = 0; j < 4; j++ )
                {
                    if (j != 3)
                    {
                        PRINTF2(modDisplayVerbose, "%02x.", desc->tunnel_data[(i*4) + j]);
                    }
                    else
                    {
                        PRINTF2(modDisplayVerbose, "%02x\n", desc->tunnel_data[(i*4) + j]);
                    }
                }
            }
	    }
    }
    /* OTR2_CMD */
    if ( desc->otr2_cmd.tunnel_data )
    {
        for ( int i = 4; i < 18; i++ )
		{
			if ( (desc->otr2_cmd.tunnel_data >> (17-i)) & 0x01 )
			{
				PRINT2(modDisplayVerbose, key, "   %10s[%d] ::  ", "tunnel_data", i);
                for ( int j = 0; j < 4; j++ )
                {
					if (j != 3)
                    {
                        PRINTF2(modDisplayVerbose, "%02x.", desc->tunnel_data[(i*4) + j]);
		            }
                    else
                    {
                        PRINTF2(modDisplayVerbose, "%02x\n", desc->tunnel_data[(i*4) + j]);
                    }
                }
			}
		}
    }
    /* MPLS_CMD */
    if ( desc->mpls_cmd.mpls_push != 0 )
	{
		for (int i = 0; i < 4; i++)
        {
            if ( (desc->mpls_cmd.mpls_push >> (3-i)) & 0x01 )
		    {
			    PRINT2(modDisplayVerbose, key, "   %10s[%d] ::  ", "otr_mpls", i);
                for ( int j = 0; j < 4; j++ )
                {
                    if (j != 3)
                    {
                        PRINTF2(modDisplayVerbose, "%02x.", desc->mpls[(i*4) + j]);
                    }
                    else
                    {
                        PRINTF2(modDisplayVerbose, "%02x\n", desc->mpls[(i*4) + j]);
                    }
                }
            }
        }
	}
    if (desc->mpls_cmd.push_al) {
        PRINT2(modDisplayVerbose, key, "   %10s ::  ", "al_label");
        for ( int j = 0; j < 4; j++ )
        {
            if (j != 3)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->al[j]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->al[j]);
            }
        }
    }
	if ( desc->mpls_cmd.otr_tag_size > 0 && desc->mpls_cmd.otr_tag_size < 3)
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  ", "otr_tags");
        for ( int i = 0; i <= (desc->mpls_cmd.otr_tag_size - 1); i++ )
		{
		    PRINTF2(modDisplayVerbose, "tag_%0d:0x", i);
            for (int j = 0; j < 4; j++)
                PRINTF2(modDisplayVerbose, "%02x", desc->otr_tags[(i*4) + j]);
            PRINTF2(modDisplayVerbose, " ");
        }
        PRINTF2(modDisplayVerbose, "\n");
	}
	/* QOS_CMD */
    if ( (desc->qos_cmd.ds_src == 5) || (desc->qos_cmd.ttl_src == 5) )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  otr_ds:0x%x otr_ttl:0x%x\n", "otr_ds_ttl",
            desc->otr_ds, desc->otr_ttl);
	}
	/* INR_CMD */
    if ( desc->inr_cmd.vni )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  ", "inr_vni");
        for (int i = 0; i < 4; i++)
            PRINTF2(modDisplayVerbose, "%02x.", desc->vni[i]);
        PRINTF2(modDisplayVerbose, "\n");
	}
	if ( desc->inr_cmd.inr_dmac )
	{
	    PRINT2(modDisplayVerbose, key, "      %10s ::  ", "inr_dmac");
        for (int i = 0; i < 6; i++)
        {
            if (i != 5)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->inr_dmac[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->inr_dmac[i]);
            }
        }
	}
	if ( desc->inr_cmd.inr_smac )
	{
    	PRINT2(modDisplayVerbose, key, "      %10s ::  ", "inr_smac");
        for (int i = 0; i < 6; i++)
        {
            if (i != 5)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->inr_smac[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->inr_smac[i]);
            }
        }

	}
	if ( desc->inr_cmd.inr_sip & desc->inr_cmd.inr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "inr_sip(v6)");
        for (int i = 0; i < 16; i++)
        {
            if (i != 15)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->inr_sip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->inr_sip[i]);
            }
        }
	}
    if ( desc->inr_cmd.inr_sip & !desc->inr_cmd.inr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "inr_sip(v4)");
        for (int i = 0; i < 4; i++)
        {
            if (i != 3)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->inr_sip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->inr_sip[i]);
	        }
        }
    }
    if ( desc->inr_cmd.inr_dip & desc->inr_cmd.inr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "inr_dip(v6)");
        for (int i = 0; i < 16; i++)
        {
            if (i != 15)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->inr_dip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->inr_dip[i]);
            }
        }
	}
    if ( desc->inr_cmd.inr_dip & !desc->inr_cmd.inr_ipv6)
	{
        PRINT2(modDisplayVerbose, key, "     %10s ::  ", "inr_dip(v4)");
        for (int i = 0; i < 4; i++)
        {
            if (i != 3)
            {
                PRINTF2(modDisplayVerbose, "%02x.", desc->inr_dip[i]);
            }
            else
            {
                PRINTF2(modDisplayVerbose, "%02x\n", desc->inr_dip[i]);
	        }
        }
    }
    if ( desc->inr_cmd.inr_ds || desc->inr_cmd.inr_ttl )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  inr_ds:0x%x inr_ttl:0x%x\n", "inr_ds_ttl",
            desc->inr_ds, desc->inr_ttl);
	}
    if ( desc->inr_cmd.inr_l4src )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  0x%02x%02x\n", "inr_l4src",
            desc->inr_l4src[0], desc->inr_l4src[1]);
    }
    if ( desc->inr_cmd.inr_l4dst )
    {
        PRINT2(modDisplayVerbose, key, "      %10s ::  0x%02x%02x\n", "inr_l4dst",
            desc->inr_l4dst[0], desc->inr_l4dst[1]);
    }
	if ( desc->inr_cmd.inr_tag_size > 0 )
	{
		PRINT2(modDisplayVerbose, key, "      %10s ::  ", "inr_tags");
        for ( int i = 0; i <= (desc->inr_cmd.inr_tag_size - 1); i++ )
		{
		    PRINTF2(modDisplayVerbose, "tag_%0d:0x", i);
            for (int j = 0; j < 4; j++)
                PRINTF2(modDisplayVerbose, "%02x", desc->inr_tags[(i*4) + j]);
            PRINTF2(modDisplayVerbose, " ");
        }
        PRINTF2(modDisplayVerbose, "\n");
	}
                //D1
}

void CalcIngressCRC(fm_text key,
                         hlp_model *model,
                         hlp_modelModControlData *c)
{
    hlp_modelState *state = &model->packetState;
    fm_uint32 ingress_crc = 0;
    fm_uint32 good_ingress_crc;

    for (int i = 0; i < 4; i++)
        ingress_crc |= state->RX_DATA[state->RX_LENGTH-4+i] << (i*8);

    // calculate good ingress crc
    good_ingress_crc = fmCrc32(state->RX_DATA, state->RX_LENGTH-4);
    // calculate ingress crc error differential
    c->crc_ingress_diff = good_ingress_crc ^ ingress_crc;

    if (c->crc_ingress_diff != 0) {
        PRINT(modDisplayVerbose, key, "Ingress packet has bad CRC. ingress crc=0x%08x, good crc=0x%08x, crc_diff=0x%08x\n", ingress_crc, good_ingress_crc, c->crc_ingress_diff);
    }
    else {
        PRINT(modDisplayVerbose, key, "Ingress packet has good CRC. ingress crc=0x%08x, good crc=0x%08x\n", ingress_crc, good_ingress_crc);
    }
}

void InitChunkedSeg(fm_text key,
                   hlp_model *model,
                   fm_byte *packet,
                   hlp_modelChunkedSeg *chunkedSeg)
{
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(packet);

    UnpackPacket(state->RX_DATA, chunkedSeg, &state->PARSER_INFO, state->RX_LENGTH, key);

    SelectL4CsumSrc(key, model, state->RX_DATA, chunkedSeg);
    state->TX_REASONCODE = 0x00;

    state->NO_PRI_ENC = !((state->PARSER_INFO.otr_l2_vlan1>0) || (state->PARSER_INFO.otr_mpls_len>0) || (state->PARSER_INFO.otr_l3_len>0));

    PRINT2(modDisplayVerbose, key, "state->RX_LENGTH = %0d, state->TX_PORT = 0x%02x\n",
        state->RX_LENGTH, state->TX_PORT);
    PRINT2(modDisplayVerbose, key, "sglort = 0x%04x (from modelState)\n", state->SGLORT);
    PRINT(modDisplayVerbose, key, "parserInfo = OTR_L2_len=%d, "
    "V1=%0d, V2=%0d, V2FIRST=%0d MPLS_LEN=%0d, OTR_L3_LEN=%0d, "
    "OTR_L3_V6=%0d, OTR_TUN_LEN=%0d, OTR_UDP=%0d, OTR_TCP=%0d,\n",
        state->PARSER_INFO.otr_l2_len,
        state->PARSER_INFO.otr_l2_vlan1,
        state->PARSER_INFO.otr_l2_vlan2,
        state->PARSER_INFO.otr_l2_v2first,
        state->PARSER_INFO.otr_mpls_len,
        state->PARSER_INFO.otr_l3_len,
        chunkedSeg->otr_l3_v6,
        state->PARSER_INFO.otr_tun_len,
        state->PARSER_INFO.otr_l4_udp,
        state->PARSER_INFO.otr_l4_tcp);
    PRINT2(modDisplayVerbose, key, "INR_L2_LEN=%0d, INR_L3_LEN=%0d, INR_L3_V6=%0d, "
        "INR_UDP=%0d, INR_TCP=%0d\n",
        state->PARSER_INFO.inr_l2_len,
        state->PARSER_INFO.inr_l3_len,
        chunkedSeg->inr_l3_v6,
        state->PARSER_INFO.inr_l4_udp,
        state->PARSER_INFO.inr_l4_tcp);
}

static fm_bool isWindowParsing(hlp_modelParserInfo *paInfo)
{
    if (paInfo->otr_l2_len     == 0 &&
        paInfo->otr_l2_vlan1   == 0 &&
        paInfo->otr_l2_vlan2   == 0 &&
        paInfo->otr_l2_v2first == 0 &&
        paInfo->otr_mpls_len   == 0 &&
        paInfo->otr_l3_len     == 0 &&
        paInfo->otr_l3_v6      == 0 &&
        paInfo->otr_l4_udp     == 0 &&
        paInfo->otr_l4_tcp     == 0 &&
        paInfo->otr_tun_len    == 0 &&
        paInfo->inr_l2_len     == 0 &&
        paInfo->inr_l2_vlan1   == 0 &&
        paInfo->inr_l2_vlan2   == 0 &&
        paInfo->inr_l2_v2first == 0 &&
        paInfo->inr_mpls_len   == 0 &&
        paInfo->inr_l3_len     == 0 &&
        paInfo->inr_l3_v6      == 0 &&
        paInfo->inr_l4_udp     == 0 &&
        paInfo->inr_l4_tcp     == 0)
        return 1;
    else
        return 0;
}

void InitControl(fm_text key,
                 hlp_model *model,
                 hlp_modelModRegData *r,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg)
{
	hlp_modelState *state;
    fm_uint64 *modIm;

    FM_NOT_USED(key);
    FM_NOT_USED(r);

	state = &model->packetState;

    c->dvStatus = IS_OK;

    if (isWindowParsing(&(state->PARSER_INFO)) && !state->NO_MODIFY) {
        c->isWindowParsing = 1;
        PRINT(modDisplayVerbose, key, "Ingress flow 2nd pass window parsing packet: passing through\n");
    }
    else {
        c->isWindowParsing = 0;
    }

    c->isMarkerPkt = 0;
	c->evidA = state->L2_EVID1;
	c->dglortA = state->EDGLORT;
	c->isMirror = ((state->MIRTYP == HLP_MODEL_MIRTYPE_MIR0) || (state->MIRTYP == HLP_MODEL_MIRTYPE_MIR1));
    c->mirrorTrunc = 0;
    c->rx_n_tag = 0;
    c->otrL3Modified = 0;
    c->otrL4Modified = 0;
    c->internalDS = state->QOS_L3_DSCP << 2 | (state->ECN & 0x03);
    c->isInterLSR = 0;
    c->skipDscp = 0;
    c->skipTtl = 0;
    c->ecn_tx_drop = 0;
    c->timeout_tx_drop = 0;
    c->non_cm_tx_drop = 0;
    c->cancelled_tx_disp = 0;
    c->cancel_drop_on_marker = 0;
    c->ecn_mark = 0;
    FM_CLEAR(c->mplsData);

    /* Grabbing l3Idx for checksum updates */
    if (chunkedSeg->otr_ip_size > 0)
    {
        c->l3Idx = 8*(chunkedSeg->ftag_v) + 14 +
            4*(chunkedSeg->n_otr_tag) + 4*(chunkedSeg->n_otr_mpls);
    }
	if (chunkedSeg->otr_udp_v | chunkedSeg->otr_tcp_v)
    {
        c->l4Idx = 8*chunkedSeg->ftag_v + 14 +
            4*chunkedSeg->n_otr_tag + 4*chunkedSeg->n_otr_mpls +
            4*chunkedSeg->otr_ip_size;
    }

    c->routeA = state->MARK_ROUTED;
	c->vlanSwitched = 0;
    c->loopbackSuppressDrop = 0;

	c->numVlans = 0;
	if (state->PARSER_INFO.otr_l2_vlan1) ++(c->numVlans);
	if (state->PARSER_INFO.otr_l2_vlan2) ++(c->numVlans);
    c->rxVlan1 = 0;
    c->rxVlan2 = 0;
    c->rxV2first = 0;
    c->txVlan1 = 0;
	c->txVlan2 = 0;
	c->preserveVlan1 = 0;
	c->preserveVlan2 = 0;
	c->txVpri1 = 0;
	c->txVpri2 = 0;
	c->txVid2 = 0;
	c->txVid2 = 0;
	c->evidB = 0;

	c->modIdx = state->MOD_IDX;
	c->encap = 0;
	c->decap = 0;

    c->bytesAdded = 0;
    c->egressSeg0Bytes = (state->RX_LENGTH < DEFAULT_SEGMENT_BYTES)
        ? state->RX_LENGTH : DEFAULT_SEGMENT_BYTES;
    c->refcnt_tx_len = state->TX_LENGTH;
    
    c->intr_occured = 0;
    c->tail_len = (((state->TAIL_CSUM_LEN >> 16) & 0x3FFF) < DEFAULT_SEGMENT_BYTES) ? 0 : (((state->TAIL_CSUM_LEN >> 16) & 0x3FFF) - DEFAULT_SEGMENT_BYTES);

    modIm = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_IM(0));
    c->mod_im = FM_GET_UNNAMED_FIELD64(*modIm, 0, 63);

    // calculate the correct ingress L3 total length for incremental len update
    c->igL3TotalLen = chunkedSeg->otr_ip_size*4 + chunkedSeg->otr_tcp_v*18 +
        chunkedSeg->otr_udp_v*8 + chunkedSeg->tun_size_in_l4_chunk*4 +
        chunkedSeg->tun_opt_size*4 + chunkedSeg->inr_l2_v*14 +
        chunkedSeg->n_inr_tag*4 + chunkedSeg->n_inr_mpls*4 +
        chunkedSeg->inr_ip_size*4 + chunkedSeg->inr_udp_v*8 +
        chunkedSeg->inr_tcp_v*18 + chunkedSeg->payload_size - 4;
    if (chunkedSeg->otr_l3_v6)
        c->igL3TotalLen -= 40;
    PRINT2(modDisplayVerbose, key, "ingress L3 total length = %0d\n", c->igL3TotalLen);
}

void ExtractPktMeta(fm_text key,
                    hlp_model *model,
                    hlp_modelModControlData *c)
{
    hlp_modelState *state;
    state = &model->packetState;

    c->l3_domain = (state->PKT_META[21] >> 5) & 0x07;
    c->l3_domain |= ((state->PKT_META[22] & 0x07) << 3);
    PRINT2(modDisplayVerbose, key, "extracted L3_Domain from pkt_meta = 0x%0x\n", c->l3_domain);

    c->operator_id = state->PKT_META[20] & 0x0F;
    PRINT2(modDisplayVerbose, key, "extracted Operator_ID from pkt_meta = 0x%0x\n", c->operator_id);

    c->isMarkerPkt = (state->PKT_META[0] == 0x02);
    PRINT2(modDisplayVerbose, key, "pkt type = 0x%0x, isMarkerPkt = %0d\n", state->PKT_META[0], c->isMarkerPkt);

    for (int i=0; i<32; i++)
        state->TX_PKT_META[i] = state->PKT_META[i];
}

void MulticastLookup(fm_text key,
                     hlp_model *model,
                     hlp_modelModControlData *c,
                     hlpModMirrorProfileTable1 *modMirrorProfileTable1)
{
    hlp_modelState *state;
    fm_uint64      *mcastTable;
    fm_uint32      mcastVlanData;
    fm_bool        replaceVid;
    fm_bool        replaceDglort;

    FM_NOT_USED(modMirrorProfileTable1);

    state = &model->packetState;

    PRINT(modDisplayVerbose, key, "------- MCAST LOOKUP -------\n");
    PRINT2(modDisplayVerbose, key, "evid = 0x%03x (from modelState)\n", state->L2_EVID1);
    PRINT2(modDisplayVerbose, key, "ivid = 0x%03x (from modelState)\n", state->L2_IVID1);

    mcastTable = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_MCAST_VLAN_TABLE(state->MOD_IP_MCAST_IDX, 0));
    PRINT2(modDisplayVerbose, key, "MCAST ptr = %0d\n", state->MOD_IP_MCAST_IDX);
    if (c->isMirror)
    {
        state->MOD_IP_MCAST_IDX = 0;
        PRINT2(modDisplayVerbose, key, "MCAST ptr reset to 0 because this is a mirrored frame\n");
    }

    replaceVid = 0;
    replaceDglort = 0;

    if (state->MOD_IP_MCAST_IDX != 0)
    {
        replaceVid = FM_GET_BIT64(*mcastTable, HLP_MOD_MCAST_VLAN_TABLE, REPLACE_VID);
        replaceDglort = FM_GET_BIT64(*mcastTable, HLP_MOD_MCAST_VLAN_TABLE, REPLACE_DGLORT);

        PRINT2(modDisplayVerbose, key, "replaceVid = %0d\n", replaceVid);
        PRINT2(modDisplayVerbose, key, "replaceDglort = %0d\n", replaceDglort);

        if (replaceVid)
        {
            mcastVlanData = FM_GET_FIELD64(*mcastTable, HLP_MOD_MCAST_VLAN_TABLE, DATA);
        	c->evidA = (mcastVlanData & 0x0FFF);
        }
        if (replaceDglort)
        {
            mcastVlanData = FM_GET_FIELD64(*mcastTable, HLP_MOD_MCAST_VLAN_TABLE, DATA);
            c->dglortA = (mcastVlanData >> 12) & 0x0FFFF;
        }
        if (!replaceVid && !replaceDglort)
        {
            mcastVlanData = FM_GET_FIELD64(*mcastTable, HLP_MOD_MCAST_VLAN_TABLE, DATA);
            if ((mcastVlanData >> 27) & 0x01)
            {
            	c->modIdx = (mcastVlanData >> 2) & 0x0FFFF;
                c->decap = (mcastVlanData >> 1) & 0x01;
                c->encap = mcastVlanData & 0x01;
            }
        }

        c->l3_domain = 0;

        PRINT2(modDisplayVerbose, key, "evidA = 0x%03x (replaceVid ? mcast table value " ": evid)\n", c->evidA);
        PRINT2(modDisplayVerbose, key, "dglortA = 0x%04x (replaceDglort ? mcast table " "value : dglort)\n", c->dglortA);
        PRINT2(modDisplayVerbose, key, "modIdx = 0x%05x\n", c->modIdx);
    }
    else
    {
        PRINT2(modDisplayVerbose, key, "evidA = 0x%03x (equals evid)\n", c->evidA);
        PRINT2(modDisplayVerbose, key, "dglortA = 0x%04x (equals dglort)\n", c->dglortA);
        PRINT2(modDisplayVerbose, key, "modIdx = 0x%05x\n", c->modIdx);
    }
}

void MirrorLookup(fm_text key,
                  hlp_model *model,
                  hlp_modelModControlData *c,
                  hlpModMirrorProfileTable1 *modMirrorProfileTable1,
                  hlpModMirrorProfileTable2 *modMirrorProfileTable2)
{
    hlp_modelState *state;
    fm_uint64 *mirrorProfile1;
    fm_uint64 *mirrorProfile2;

    state = &model->packetState;

    PRINT(modDisplayVerbose, key, "------- MIRROR LOOKUP -------\n");

    switch (state->MIRTYP)
    {
        case HLP_MODEL_MIRTYPE_NORMAL:
            PRINT2(modDisplayVerbose, key, "mir_type = %0d (do not mirror)\n", state->MIRTYP);
            mirrorProfile1 = NULL;
            mirrorProfile2 = NULL;
            break;
        case HLP_MODEL_MIRTYPE_MIR0:
            PRINT2(modDisplayVerbose, key, "mir_type = %0d (mirror 0)\n", state->MIRTYP);
            mirrorProfile1 = (fm_uint64 *)FM_MODEL_GET_REG_PTR(model,
                HLP_MOD_MIRROR_PROFILE_TABLE1(state->MIRROR0_PROFILE_IDX, 0));
            mirrorProfile2 = (fm_uint64 *)FM_MODEL_GET_REG_PTR(model,
                HLP_MOD_MIRROR_PROFILE_TABLE2(state->MIRROR0_PROFILE_IDX, 0));
            break;
        case HLP_MODEL_MIRTYPE_MIR1:
            PRINT2(modDisplayVerbose, key, "mir_type = %0d (mirror 1)\n", state->MIRTYP);
            mirrorProfile1 = (fm_uint64 *)FM_MODEL_GET_REG_PTR(model,
                HLP_MOD_MIRROR_PROFILE_TABLE1(state->MIRROR1_PROFILE_IDX, 0));
            mirrorProfile2 = (fm_uint64 *)FM_MODEL_GET_REG_PTR(model,
                HLP_MOD_MIRROR_PROFILE_TABLE2(state->MIRROR1_PROFILE_IDX, 0));
            break;
        default:
            PRINT2(modDisplayVerbose, key, "mir_type = %0d (UNDEFINED!! deprecated)\n", state->MIRTYP);
            mirrorProfile1 = NULL;
            mirrorProfile2 = NULL;
            break;
    }

    PRINT2(modDisplayVerbose, key, "isMirror = %0d\n", c->isMirror);
    // if (c->isMirror)
    // redundant logic to please klocwork
    if ((state->MIRTYP == HLP_MODEL_MIRTYPE_MIR0) || (state->MIRTYP == HLP_MODEL_MIRTYPE_MIR1))
    {
        modMirrorProfileTable2->METADATA_BYTE_OFFSET = FM_GET_FIELD64(
            *mirrorProfile2, HLP_MOD_MIRROR_PROFILE_TABLE2, METADATA_BYTE_OFFSET);
        modMirrorProfileTable2->REPLACE_TYPE = FM_GET_BIT64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, REPLACE_TYPE);
        modMirrorProfileTable2->TYPE_VALUE = FM_GET_FIELD64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, TYPE_VALUE);
        modMirrorProfileTable2->METADATA_SET_MODE = FM_GET_BIT64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, METADATA_SET_MODE);
        modMirrorProfileTable2->METADATA_VALUE = FM_GET_FIELD64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, METADATA_VALUE);
        modMirrorProfileTable2->METADATA_MASK = FM_GET_FIELD64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, METADATA_MASK);
        modMirrorProfileTable2->MIRROR_VPRI = FM_GET_FIELD64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, MIRROR_VPRI);
        modMirrorProfileTable2->MIRROR_VID = FM_GET_FIELD64(*mirrorProfile2,
            HLP_MOD_MIRROR_PROFILE_TABLE2, MIRROR_VID);
        modMirrorProfileTable1->MIRROR_DGLORT = FM_GET_FIELD64(*mirrorProfile1,
            HLP_MOD_MIRROR_PROFILE_TABLE1, MIRROR_DGLORT);
        c->mirrorTrunc = FM_GET_BIT64(*mirrorProfile1,
            HLP_MOD_MIRROR_PROFILE_TABLE1, TRUNC);
        c->modIdx = FM_GET_FIELD64(*mirrorProfile1,
            HLP_MOD_MIRROR_PROFILE_TABLE1, MOD_IDX);
        if(c->isWindowParsing) {
            c->modIdx = 0;
            PRINT2(modDisplayVerbose, key, "Mirroring when window_parsing set modIdx to 0\n");
        }
        c->decap = (c->modIdx >> 1) & 0x01;
        c->encap = c->modIdx & 0x01;
        if(((c->modIdx >> 2) & 0x0FFFF) != 0)
            c->modIdx = (c->modIdx >> 2) & 0x0FFFF;

        PRINT2(modDisplayVerbose, key, "mirrorVid = 0x%03x\n", modMirrorProfileTable2->MIRROR_VID);
        PRINT2(modDisplayVerbose, key, "mirrorVpri = 0x%01x\n", modMirrorProfileTable2->MIRROR_VPRI);
        PRINT2(modDisplayVerbose, key, "dglortM = 0x%04x\n", modMirrorProfileTable1->MIRROR_DGLORT);
        PRINT2(modDisplayVerbose, key, "mirrorTrunc = %0d\n", c->mirrorTrunc);
        PRINT2(modDisplayVerbose, key, "modIdx = 0x%05x\n", c->modIdx);
        PRINT2(modDisplayVerbose, key, "decap = %0d\n", c->decap);
        PRINT2(modDisplayVerbose, key, "encap = %0d\n", c->encap);
    }

    c->txDglort = c->isMirror ? modMirrorProfileTable1->MIRROR_DGLORT : c->dglortA;
    PRINT2(modDisplayVerbose, key, "txDglort = 0x%04x\n", c->txDglort);
}

void LoopbackSuppress(fm_text key,
                      hlp_model *model,
                      hlp_modelModRegData *r,
                      hlp_modelModControlData *c)
{
    hlp_modelState *state;
    fm_uint16 csGlortF;

    state = &model->packetState;

    csGlortF = state->SGLORT & r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK;
    if (state->MOD_IP_MCAST_IDX != 0)
    {
        c->vlanSwitched = (c->evidA == state->L2_IVID1);
        if (c->vlanSwitched)
        {
            c->routeA = 0;
        }
        c->loopbackSuppressDrop = (r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT == csGlortF)
            & c->vlanSwitched;
    }
    else
    {
        c->loopbackSuppressDrop = 0;
    }
    PRINT2(modDisplayVerbose, key, "route = %0d\n", state->MARK_ROUTED);
    PRINT2(modDisplayVerbose, key, "routeA = %0d\n", c->routeA);
    PRINT2(modDisplayVerbose, key, "glortMask = 0x%04x\n", r->modPerPortCfg1.LOOPBACK_SUPPRESS_MASK);
    PRINT2(modDisplayVerbose, key, "csGlortF = 0x%04x (sglort & glortMask)\n", csGlortF);
    PRINT2(modDisplayVerbose, key, "csGlortX = 0x%04x\n", r->modPerPortCfg1.LOOPBACK_SUPPRESS_GLORT);
    PRINT2(modDisplayVerbose, key, "vlanSwitched = %0d\n", c->vlanSwitched);
    PRINT2(modDisplayVerbose, key, "drop (because of loopback suppress) = %0d\n", c->loopbackSuppressDrop);
}

void ModDescLookup(fm_text key,
                   hlp_model *model,
                   hlp_modelModControlData *c,
                   hlp_modelModDesc **descMaster,
                   hlp_modelModDesc *desc0,
                   hlp_modelModDesc *desc1,
                   hlp_modelModDesc *desc2,
                   hlp_modelModDesc *desc3)
{
    hlp_modelState          *state;
    fm_byte                 mode;
    fm_uint16               A;
    fm_uint16               B;
    fm_uint64               *modMaster;

    FM_CLEAR(*desc0);
    FM_CLEAR(*desc1);
    FM_CLEAR(*desc2);
    FM_CLEAR(*desc3);

    state = &model->packetState;
    *descMaster = desc0;

    if(c->modIdx == 0) {
        PRINT2(modDisplayVerbose, key, "mod_idx is 0, skip MOD_MASTER!\n");
        return;
    }
    modMaster = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
    		HLP_MOD_MASTER(c->modIdx, 0));

    mode = FM_GET_FIELD(*modMaster,
            HLP_MOD_MASTER, MODE);
    A = FM_GET_FIELD(*modMaster,
            HLP_MOD_MASTER, A);
    B = FM_GET_FIELD(*modMaster,
            HLP_MOD_MASTER, B);

    PRINT2(modDisplayVerbose, key, "MOD_MASTER[%0d]: mode=%0d A=0x%0x B=0x%0x\n",
        c->modIdx, mode, A, B);
    /* Unpack Descriptors based on Mode from modMaster */
    switch (mode)
    {
		case 1:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    *descMaster = desc1;
		    UpdateDescMaster(*descMaster, desc0);
			break;
		case 2:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(*descMaster, desc1);
			break;
		case 3:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    *descMaster = desc1;
		    UpdateDescMaster(*descMaster, desc0);
			break;
		case 4:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
		case 5:
			UnpackModDesc(model, desc0, 1, B, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    *descMaster = desc1;
		    UpdateDescMaster(*descMaster, desc0);
			break;
		case 6:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc0, desc1);
		    UpdateDescMaster(*descMaster, desc0);
			break;
		case 7:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
		case 8:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(*descMaster, desc1);
			break;
		case 9:
			UnpackModDesc(model, desc0, 1, A, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(*descMaster, desc1);
			break;
		case 10:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc0, desc2);
		    UpdateDescMaster(*descMaster, desc0);
			break;
		case 11:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(*descMaster, desc2);
			break;
		case 12:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
			break;
		case 13:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 3, A, key);
		    *descMaster = desc1;
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 14:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 15:
			UnpackModDesc(model, desc0, 1, B, key);
		    UnpackModDesc(model, desc1, 3, A, key);
		    *descMaster = desc1;
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 16:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc0, desc1);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 17:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 18:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 19:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 20:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 21:
			UnpackModDesc(model, desc0, 1, B, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 22:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc0, desc1);
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 23:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 24:
			UnpackModDesc(model, desc0, 2, B, key);
		    UnpackModDesc(model, desc1, 3, A, key);
		    *descMaster = desc1;
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 25:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc0, desc1);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 26:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 27:
			UnpackModDesc(model, desc0, 1, A, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc0, desc1);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 28:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc0, desc2);
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 29:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(desc1, desc2);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 30:
			UnpackModDesc(model, desc0, 1, B, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    UnpackModDesc(model, desc2, 3, A, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 31:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(desc0, desc2);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 32:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, A, key);
		    *descMaster = desc3;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 33:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 34:
			UnpackModDesc(model, desc0, 1, A, key);
		    UnpackModDesc(model, desc1, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 35:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc0, desc2);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 36:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 37:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 38:
			UnpackModDesc(model, desc0, 2, A, key);
		    UnpackModDesc(model, desc1, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(*descMaster, desc1);
			break;
        case 39:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc0, desc2);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 40:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(*descMaster, desc2);
			break;
        case 41:
			UnpackModDesc(model, desc0, 1, A, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc0, desc2);
		    UpdateDescMaster(*descMaster, desc0);
			break;
        case 42:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc0, desc3);
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(*descMaster, desc1);
		    break;
        case 43:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc3, desc0);
		    UpdateDescMaster(desc1, desc3);
		    UpdateDescMaster(*descMaster, desc1);
		    break;
        case 44:
			UnpackModDesc(model, desc0, 1, B, key);
		    UnpackModDesc(model, desc1, 2, A, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(*descMaster, desc2);
		    break;
        case 45:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc3, desc1);
		    UpdateDescMaster(desc0, desc3);
		    UpdateDescMaster(*descMaster, desc0);
		    break;
        case 46:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, A, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc2;
		    UpdateDescMaster(desc1, desc0);
		    UpdateDescMaster(desc3, desc1);
		    UpdateDescMaster(*descMaster, desc3);
		    break;
        case 47:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
		    break;
        case 48:
			UnpackModDesc(model, desc0, 1, A, key);
		    UnpackModDesc(model, desc1, 2, B, key);
		    UnpackModDesc(model, desc2, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(*descMaster, desc2);
		    break;
        case 49:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc3, desc2);
		    UpdateDescMaster(desc0, desc3);
		    UpdateDescMaster(*descMaster, desc0);
		    break;
        case 50:
			UnpackModDesc(model, desc0, 0, B, key);
		    UnpackModDesc(model, desc1, 1, A, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc1;
		    UpdateDescMaster(desc2, desc0);
		    UpdateDescMaster(desc3, desc2);
		    UpdateDescMaster(*descMaster, desc3);
		    break;
        case 51:
			UnpackModDesc(model, desc0, 0, A, key);
		    UnpackModDesc(model, desc1, 1, B, key);
		    UnpackModDesc(model, desc2, 2, B, key);
		    UnpackModDesc(model, desc3, 3, B, key);
		    *descMaster = desc0;
		    UpdateDescMaster(desc2, desc1);
		    UpdateDescMaster(desc3, desc2);
		    UpdateDescMaster(*descMaster, desc3);
		    break;
		default:
			break;
    }

    UpdateDescMeta(*descMaster, state->MOD_META);

    PrintDesc(key, *descMaster);
}

void GetRxL2Tags(fm_text key,
                 hlp_model *model,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg,
                 hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;
    /* Use inner VLANs: DECAP_LEAVE_IN_PLACE */
    if (descMaster->otr1_cmd.tunnel_mode == TM_DECAP_LEAVE_IN_PLACE) {
        PRINT2(modDisplayVerbose, key, "TM_DECAP_LEAVE_IN_PLACE: VLAN modifications "
            "using inner VLANs\n");
        for (int i = 0; i < (chunkedSeg->n_inr_tag*4); i++)
    		c->rx_tags[i] = chunkedSeg->inr_tags[i];
    	c->rx_n_tag = chunkedSeg->n_inr_tag;
        c->rxVlan1 = state->PARSER_INFO.inr_l2_vlan1;
        c->rxVlan2 = state->PARSER_INFO.inr_l2_vlan2;
        c->rxV2first = 0;
    }
    else
    {
        for (int i = 0; i < VLAN_TAG_BYTES*4; i++)
            c->rx_tags[i] = chunkedSeg->otr_tags[i];
        c->rx_n_tag = chunkedSeg->n_otr_tag;
        c->rxVlan1 = state->PARSER_INFO.otr_l2_vlan1;
        c->rxVlan2 = state->PARSER_INFO.otr_l2_vlan2;
        c->rxV2first = state->PARSER_INFO.otr_l2_v2first;
    }

    PRINT2(modDisplayVerbose, key, "rx_n_tag=%0d rx_tags=0x", c->rx_n_tag);
    for (int i = 0; i < c->rx_n_tag; i++)
    {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ", c->rx_tags[i*4],c->rx_tags[(i*4)+1],
            c->rx_tags[(i*4)+2],c->rx_tags[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");
}

void LogError(fm_text key,
              hlp_model *model,
              hlp_modelModControlData *c)
{
    hlp_modelState *state = &model->packetState;
    fm_uint64 *modUpdateErrorRecord;
    fm_byte mir_type = 0;
    fm_byte mir_prof = 0;
    fm_byte tx_tag = 0;
    fm_byte mpls_pop = 0;
    fm_uint mod_idx = 0;

    FM_NOT_USED(key);

    modUpdateErrorRecord = (fm_uint64*) FM_MODEL_GET_REG_PTR(model, HLP_MOD_UPDATE_ERROR_RECORD(0));
    FM_SET_BIT64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, WINDOW_PARSING, isWindowParsing(&(state->PARSER_INFO)));
    if(state->TX_REASONCODE == ERR_VLAN_TAG_FULL ||
       state->TX_REASONCODE == ERR_VLAN_TAG_EMPTY) {// VLAN tag errors
        switch(state->MIRTYP) {
            case HLP_MODEL_MIRTYPE_NORMAL:
                mir_prof = 0;
                break;
            case HLP_MODEL_MIRTYPE_MIR0:
                mir_prof = state->MIRROR0_PROFILE_IDX;
                break;
            case HLP_MODEL_MIRTYPE_MIR1:
                mir_prof = state->MIRROR1_PROFILE_IDX;
                break;
            default:
                break;
        }
        mir_type = state->MIRTYP;
        tx_tag = state->TX_TAG;
    }
    if(state->TX_REASONCODE == ERR_PUSH_AL ||
       state->TX_REASONCODE == ERR_PUSH_ELI ||
       state->TX_REASONCODE == ERR_PUSH_G ||
       state->TX_REASONCODE == ERR_POP_AL ||
       state->TX_REASONCODE == ERR_POP_ELI ||
       state->TX_REASONCODE == ERR_POP_G) { // mpls desc errors
        mpls_pop = state->MPLS_POP;
    }
    if (state->TX_REASONCODE != ERR_VLAN_TAG_FULL && state->TX_REASONCODE != ERR_VLAN_TAG_EMPTY && state->TX_REASONCODE != ERR_PKT_LEN) { // desc errors
        mod_idx = (c->modIdx << 2);
    }
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MIR_TYPE, mir_type);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, TX_TAG, tx_tag);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MIRROR_PROFILE, mir_prof);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MPLS_POP, mpls_pop);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, MOD_IDX, mod_idx);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, TX_PORT, state->TX_PORT);
    FM_SET_FIELD64(*modUpdateErrorRecord, HLP_MOD_UPDATE_ERROR_RECORD, REASON_CODE, state->TX_REASONCODE);
}

void DropAndLog(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c,
                fm_byte dropDisp,
                fm_bool isMarker,
                fm_bool isDrop,
                fm_bool isErrLog,
                fm_bool isSkip,
                fm_byte reasonCode,
                fm_int  intrDisp)
{
    hlp_modelState *state = &model->packetState;

    if (isSkip) return;

    if (dropDisp < state->TX_DISP && !isMarker) {
        if (dropDisp == DISP_MODERRORDROP) {
            if (isDrop) {
                state->TX_DISP = dropDisp;
            }
        }
        else {
            state->TX_DISP = dropDisp;
        }
    }

    if(state->TX_DROP == 0)
        state->TX_DROP = ((!isMarker) & isDrop);

    // this block serves a few purposes:
    // 1. for state->ECN_TX_DROP prediction, which should only be set if a packet
    //    is dropped due to ecn but not dropped for any other reason
    // 2. c->cancelled_tx_disp memorizes the drop that would have happend were it
    //    not cancelled by isMarker
    if (isDrop) {
        if (!isMarker) {
            if (dropDisp == DISP_ECNDROP)
                c->ecn_tx_drop = 1;
            else if (dropDisp == DISP_TIMEOUTDROP)
                c->timeout_tx_drop = 1;
            else
                c->non_cm_tx_drop = 1;
        }
        else {
            c->cancel_drop_on_marker = 1;
            if (dropDisp < c->cancelled_tx_disp)
                c->cancelled_tx_disp = dropDisp;
        }
    }

    PRINT2(modDisplayVerbose, key, "state->TX_DROP = %0d\n", state->TX_DROP);
    if(isErrLog && (!((c->mod_im)>>intrDisp & 0x1))) {
        if(reasonCode < state->TX_REASONCODE || state->TX_REASONCODE == 0) {
            state->TX_REASONCODE = reasonCode;
            LogError(key, model, c);
        }
    }
 
    // interrupt prediction. Only valid for single-step tests
    if(intrDisp >= 0){
        fm_uint64 *modIp;
        fm_uint64 ip = 0;

        modIp = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_IP(0));
        ip = FM_GET_UNNAMED_FIELD64(*modIp, 0 ,63);
        ip |= FM_LITERAL_U64(1) << intrDisp;
        FM_SET_UNNAMED_FIELD64(*modIp, 0, 63, ip);

        if(!((c->mod_im)>>intrDisp & 0x1))
            c->intr_occured = 1;
        PRINT2(modDisplayVerbose, key, "INTR: set intr type %0d\n", intrDisp);
    }
}

void ExtractPriorityProfile(fm_text key,
                            hlp_model *model,
                            hlp_modelModRegData *r,
                            hlp_modelModControlData *c)
{
     fm_uint32 *modDomainTcamKey;
     fm_uint32 *modDomainTcamMask;
     fm_uint32 *modDomainAction;
     fm_uint32 hitIdx = 100;
     hlpModDomainTcamKey key_data;
     hlpModDomainTcamMask mask_data;
     hlpModDomainAction   modDomainTcamAction_data;
     fm_uint64 keyReg, keyIngress, keyMask;
     hlp_modelState *state;

    FM_NOT_USED(r);

     state = &model->packetState;

     PRINT(modDisplayVerbose, key, "------- Extract Priority Profile -------:\n");
     for (int i=63; i >= 0; i--) {
         modDomainTcamKey = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DOMAIN_TCAM_KEY(i, 0));
         modDomainTcamMask = FM_MODEL_GET_REG_PTR(model,HLP_MOD_DOMAIN_TCAM_MASK(i, 0));
         key_data.PORT = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, PORT);
         key_data.OPERATOR_ID = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, OPERATOR_ID);
         key_data.VID1 = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, VID1);
         key_data.VID2 = FM_GET_FIELD64(
             *modDomainTcamKey, HLP_MOD_DOMAIN_TCAM_KEY, VID2);

         mask_data.PORT = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, PORT);
         mask_data.OPERATOR_ID = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, OPERATOR_ID);
         mask_data.VID1 = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, VID1);
         mask_data.VID2 = FM_GET_FIELD64(
             *modDomainTcamMask, HLP_MOD_DOMAIN_TCAM_KEY, VID2);

         keyReg = FM_ARRAY_GET_UNNAMED_FIELD64(modDomainTcamKey, 0, 34);
         keyIngress = (((fm_uint64)(state->TX_PORT&0x3F) << 28) | ((fm_uint64)(c->operator_id&0x000F) << 24)
                      | ((fm_uint64)(c->txVid1&0x0FFF) << 12) |
                      (c->txVid2&0x0FFF))&0x3FFFFFFFF;
         keyMask = FM_ARRAY_GET_UNNAMED_FIELD64(modDomainTcamMask, 0, 34);

         PRINT3(modDisplayVerbose, key, "index at %0d\n", i);
         PRINT3(modDisplayVerbose, key, "keyReg=0x%0llx\n", keyReg);
         PRINT3(modDisplayVerbose, key, "keyIngress=0x%0llx\n", keyIngress);
         PRINT3(modDisplayVerbose, key, "keyMask=0x%0llx\n", keyMask);
         PRINT3(modDisplayVerbose, key, "0x%0llx : 0x%0llx\n",(keyReg & ~keyMask) ,(keyIngress & ~keyMask));
         if (keyReg == (keyIngress & keyMask)) {
             hitIdx = i;
             PRINT2(modDisplayVerbose, key, "key.Port=0x%0x, key.Operator_id=0x%0x, key.vid1=0x%0x, key.Vid2=0x%0x\n",
                   key_data.PORT, key_data.OPERATOR_ID, key_data.VID1, key_data.VID2);
             PRINT2(modDisplayVerbose, key, "Mask.Port=0x%0x, Mask.Operator_id=0x%0x, Mask.vid1=0x%0x,Mask.Vid2=0x%0x\n",
                   mask_data.PORT, mask_data.OPERATOR_ID, mask_data.VID1,mask_data.VID2);
             break;
         }
     }

     if (hitIdx == 100) {
        PRINT2(modDisplayVerbose, key, "no hit, set hitIdx to 0\n");
        hitIdx = 0;
     }
     else
        PRINT2(modDisplayVerbose, key, "hitIdx in DomainTCAMKey %0d\n", hitIdx);
     modDomainAction = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DOMAIN_ACTION(hitIdx, 0));
     c->priority_profile = FM_ARRAY_GET_UNNAMED_FIELD64(modDomainAction, 0, 5);
     PRINT2(modDisplayVerbose, key, "Prioriy Profile is 0x%0x\n", c->priority_profile);
}

void VlanLookup(fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlpModMirrorProfileTable2 *modMirrorProfileTable2,
                hlp_modelModDesc *descMaster)
{
    hlp_modelState *state;

    fm_uint16 rxVlan1Type;
    fm_uint16 rxVlan2Type;
    fm_byte rxVpri1;
    fm_byte rxVpri2;
    fm_uint16 rxVid1;
    fm_uint16 rxVid2;

    fm_uint16 evidB;
    fm_uint16 modVid1MapAddr;
    fm_uint16 modVid2MapAddr;
    fm_uint16 modVlanTagAddr;
    fm_uint64 *rdval;
    fm_uint64 temp64;

    fm_byte newVpri1;
    fm_byte newVpri2;
    fm_uint16 newVid1;
    fm_uint16 newVid2;
    fm_bool vtag;

    FM_NOT_USED(chunkedSeg);

    state = &model->packetState;
    rxVpri1 = 0;
    rxVpri2 = 0;
    rxVid1 = 0;
    rxVid2 = 0;
    evidB = 0;
    newVpri1 = 0;
    newVpri2 = 0;
    newVid1 = 0;
    newVid2 = 0;
    vtag = 0;

    PRINT2(modDisplayVerbose, key, "------- VLAN LOOKUP -------\n");

    PRINT2(modDisplayVerbose, key, "initial numVlans = %0d\n", c->numVlans);
    PRINT2(modDisplayVerbose, key, "vpri = 0x%01x (from modelState)\n", state->QOS_L2_VPRI1);
    PRINT2(modDisplayVerbose, key, "enableVlanUpdate = %0d\n", r->modPerPortCfg1.ENABLE_VLAN_UPDATE);
    PRINT2(modDisplayVerbose, key, "vid2First = %0d (in cfg2)\n", r->modPerPortCfg2.VID2_FIRST);

    evidB = r->modPerPortCfg1.ENABLE_VLAN_UPDATE ? c->evidA : state->L2_IVID1;
    PRINT2(modDisplayVerbose, key, "evidB = 0x%03x (enableVlanUpdate ? evidA : ivid)\n",
        evidB);

    /* grab rx vlan data */
    //FIXME:
    //if (state->PARSER_INFO.ftag_v > 0)
    //{
    //    rxFtagVpri = state->RX_DATA[2] >> 4;
    //}

    if (c->numVlans == 1)
    {
        /* assume that we don't need to check vlan 2 first */
        if (c->rxVlan1)
        {
            rxVlan1Type = c->rx_tags[0]; /*X repetitive operation*/
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) c->rx_tags[1];
            rxVpri1 = c->rx_tags[2] >> 4;
            rxVid1 = (c->rx_tags[2] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) c->rx_tags[3];
        }
        else
        {
            rxVlan2Type = c->rx_tags[0];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) c->rx_tags[1];
            rxVpri2 = c->rx_tags[2] >> 4;
            rxVid2 = (c->rx_tags[2] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) c->rx_tags[3];
        }
    }
    else if (c->numVlans == 2)
    {
        if (state->PARSER_INFO.otr_l2_v2first)
        {
            rxVlan2Type = c->rx_tags[0];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) c->rx_tags[1];
            rxVpri2 = c->rx_tags[2] >> 4;
            rxVid2 = (c->rx_tags[2] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) c->rx_tags[3];
            rxVlan1Type = c->rx_tags[4];
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) c->rx_tags[5];
            rxVpri1 = c->rx_tags[6] >> 4;
            rxVid1 = (c->rx_tags[6] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) c->rx_tags[7];

        }
        else
        {
            rxVlan1Type = c->rx_tags[0];
            rxVlan1Type <<= 8;
            rxVlan1Type |= (fm_uint16) c->rx_tags[1];
            rxVpri1 = c->rx_tags[2] >> 4;
            rxVid1 = (c->rx_tags[2] & 0x0F);
            rxVid1 <<= 8;
            rxVid1 |= (fm_uint16) c->rx_tags[3];
            rxVlan2Type = c->rx_tags[4];
            rxVlan2Type <<= 8;
            rxVlan2Type |= (fm_uint16) c->rx_tags[5];
            rxVpri2 = c->rx_tags[6] >> 4;
            rxVid2 = (c->rx_tags[6] & 0x0F);
            rxVid2 <<= 8;
            rxVid2 |= (fm_uint16) c->rx_tags[7];
        }
    }

    /* VLAN tag lookups: */
    modVid1MapAddr = evidB;
    modVid2MapAddr = evidB;
    modVlanTagAddr = evidB;
    c->evidB = evidB;
    switch (r->modPerPortCfg1.VID2_MAP_INDEX) {
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_VID:
            break;
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_SGLORT:
            modVid2MapAddr = state->SGLORT & 0xFFF;
            break;
        case HLP_MOD_PER_PORT_CFG1_VID2_MAP_INDEX_DGLORT:
            modVid2MapAddr = c->dglortA & 0xFFF;
            break;
        defualt:
            PRINT2(modDisplayVerbose, key, "vid2MapIndex = %0d (UNDEFINED!!)\n",
                r->modPerPortCfg1.VID2_MAP_INDEX);
            break;
    }

    if (descMaster->mpls_cmd.otr_tag_mode == 7 && descMaster->mpls_cmd.otr_tag_size > 0 && descMaster->mpls_cmd.otr_tag_size < 3) {
        PRINT2(modDisplayVerbose, key, "Desc OTR Vlans fed into vid{1,2}_map tables\n");
        modVlanTagAddr = ((descMaster->otr_tags[2] & 0x0F) << 8) |
                          descMaster->otr_tags[3];
        modVid1MapAddr = ((descMaster->otr_tags[2] & 0x0F) << 8) |
                          descMaster->otr_tags[3];
        if (descMaster->mpls_cmd.otr_tag_size > 1) {
            modVid2MapAddr = ((descMaster->otr_tags[6] & 0x0F) << 8) |
                              descMaster->otr_tags[7];
        }
    } // TODO what is the priority of these 2
    if (descMaster->inr_cmd.inr_tag_mode == 7 && descMaster->inr_cmd.inr_tag_size > 0){
        PRINT2(modDisplayVerbose, key, "Desc INR Vlans fed into vid{1,2}_map tables\n");
        modVlanTagAddr = ((descMaster->inr_tags[2] & 0x0F) << 8) |
                          descMaster->inr_tags[3];
        modVid1MapAddr = ((descMaster->inr_tags[2] & 0x0F) << 8)|
                          descMaster->inr_tags[3];
        if (descMaster->inr_cmd.inr_tag_size > 1) {
            modVid2MapAddr = ((descMaster->inr_tags[6] & 0x0F) << 8) |
                              descMaster->inr_tags[7];
        }
    }

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_VID1_MAP(modVid1MapAddr, 0));

    newVid1 = FM_GET_FIELD64(*rdval, HLP_MOD_VID1_MAP, VID);

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_VLAN_TAG(modVlanTagAddr, 0));
    temp64 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_TAG, TAG);
    vtag = ((temp64 >> state->TX_PORT) & 0x01);

    PRINT2(modDisplayVerbose, key, "MOD_VID1_MAP[%0d] = %0x\n", modVid1MapAddr, newVid1);
    PRINT2(modDisplayVerbose, key, "MOD_VLAN_TAG[%0d] = %016llx\n", modVlanTagAddr, temp64);

    rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
        HLP_MOD_VID2_MAP(modVid2MapAddr, 0));
    newVid2 = FM_GET_FIELD64(*rdval, HLP_MOD_VID2_MAP, VID);
    PRINT2(modDisplayVerbose, key, "MOD_VID2_MAP[%0d] = %0x\n", modVid2MapAddr, newVid2);

    c->txVid1  = newVid1;
    c->txVid2  = newVid2;

    if (c->isMirror)
    {   /* Step 2b in EAS: Update VLAN Fields, Mirrored Frames */
        c->preserveVlan1 = (c->rxVlan1 != 0);
        c->preserveVlan2 = (c->rxVlan2 != 0);
        if (r->modPerPortCfg2.VID2_FIRST) {
            c->txVlan2 = ((modMirrorProfileTable2->MIRROR_VID) != 0);
            if(c->txVlan2)
                c->numVlans++;
        }
        else {
            c->txVlan1 = ((modMirrorProfileTable2->MIRROR_VID) != 0);
            if(c->txVlan1)
                c->numVlans++;
        }
        c->txVid1 = (modMirrorProfileTable2->MIRROR_VID);
        c->txVpri1 = (modMirrorProfileTable2->MIRROR_VPRI);
        c->txVid2 = (modMirrorProfileTable2->MIRROR_VID);
        c->txVpri2 = (modMirrorProfileTable2->MIRROR_VPRI);

        PRINT2(modDisplayVerbose, key, "Frame is mirrored and port is not ftagged; "
            "VLAN %0d added with mirrorVid and "
            "mirrorVpri; any RX'd tags will be transmitted\n",
            (r->modPerPortCfg2.VID2_FIRST+1));
    }
    // TODO, if leave-as-rx should we use rxvid also as final vid?
    ExtractPriorityProfile(key, model, r, c);

    newVpri1 = GetVPRI(model, 1, state->QOS_L2_VPRI1, c->priority_profile, key);
    newVpri2 = GetVPRI(model, 2, state->QOS_L2_VPRI1, c->priority_profile, key);

    PRINT2(modDisplayVerbose, key, "enablePcp1Update = %0d\n", r->modPerPortCfg2.ENABLE_PCP1_UPDATE);
    PRINT2(modDisplayVerbose, key, "enableDei1Update = %0d\n", r->modPerPortCfg2.ENABLE_DEI1_UPDATE);
    PRINT2(modDisplayVerbose, key, "enablePcp2Update = %0d\n", r->modPerPortCfg2.ENABLE_PCP2_UPDATE);
    PRINT2(modDisplayVerbose, key, "enableDei2Update = %0d\n", r->modPerPortCfg2.ENABLE_DEI2_UPDATE);
    PRINT2(modDisplayVerbose, key, "rxVpri1 = 0x%01x\n", rxVpri1);
    PRINT2(modDisplayVerbose, key, "rxVid1  = 0x%03x\n", rxVid1);
    PRINT2(modDisplayVerbose, key, "rxVpri2 = 0x%01x\n", rxVpri2);
    PRINT2(modDisplayVerbose, key, "rxVid2  = 0x%03x\n", rxVid2);
    PRINT2(modDisplayVerbose, key, "newVpri1 = 0x%01x (from MOD_VPRI1_MAP)\n", newVpri1);
    PRINT2(modDisplayVerbose, key, "newVid1  = 0x%03x (from MOD_VID1_MAP)\n", newVid1);
    PRINT2(modDisplayVerbose, key, "newVpri2 = 0x%01x (from MOD_VPRI2_MAP)\n", newVpri2);
    PRINT2(modDisplayVerbose, key, "newVid2  = 0x%03x (from MOD_VID2_MAP)\n", newVid2);

    /* EAS Step 2a : Update VLAN Fields, Non-mirrored and non-special frames */
    if (!c->isMirror)
    {
        /* figure out what action will be taken on each vlan */
        if (state->TX_TAG == HLP_MODEL_INSERT) {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
            c->txVlan1 = (c->txVid1 != 0);
            c->txVlan2 = (c->txVid2 != 0);
            c->preserveVlan1 = (c->rxVlan1) != 0;
            c->preserveVlan2 = (c->rxVlan2) != 0;

            c->numVlans += 2;
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = INSERT, if "
                "txVid1/txVid2 != 0); any RX'd tags will also be transmitted\n",
                state->TX_TAG);
        }
        else if (state->TX_TAG == HLP_MODEL_DELETE)
        {
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = DELETE); no "
                "new or RX'd tags will be transmitted\n",
                state->TX_TAG);
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
            if(!(c->rxVlan1 && c->rxVlan2)) {
                PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
            }
            c->numVlans = 0;
        }
        else if (state->TX_TAG == HLP_MODEL_UPDATE_ADD)
        {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
            c->txVlan1 = (c->txVid1 != 0);
            c->txVlan2 = (c->txVid2 != 0);
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (vid1/vid2 = UPDATE or "
                "ADD, if txVid1/txVid2 != 0); if txVlan1 or txVlan2, new "
                "tag data will be used\n",
                state->TX_TAG);
            if (!c->rxVlan1 && c->txVlan1)
                c->numVlans++;
            if (!c->rxVlan2 && c->txVlan2)
                c->numVlans++;
        }
        else
        {   /* TX_TAG == HLP_MODEL_NORMAL_TAGGING */
            PRINT2(modDisplayVerbose, key, "state->TX_TAG = %0d (NORMAL TAGGING)\n", state->TX_TAG);
            PRINT2(modDisplayVerbose, key, "PTAG = %0d\n", r->modPerPortCfg2.VLAN_TAGGING);
            PRINT2(modDisplayVerbose, key, "VTAG = %0d\n", vtag);

            /* note that some ptag/vtag combo's are (intentionally) duplicates;
             *  this is coded per the table in EAS, in the effort to make
             *  maintenance easier if one combo changes and another does not */
            switch (r->modPerPortCfg2.VLAN_TAGGING)
            {
                case 5:
                    c->txVid1  = rxVid1;
                    c->txVid2  = rxVid2;
                    c->txVlan1 = (c->rxVlan1 != 0);
                    c->txVlan2 = (c->rxVlan2 != 0);
                    PRINT2(modDisplayVerbose, key, "vid1 = leave-as-rx, vid2 = leave-as-rx\n");
                    break;
                case 0:
                    if (vtag == 0) { /*X similar operation*/
                        /* leave-as-rx allows tag to transmit even if txVid2 == 0 */
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

                        c->txVid2  = rxVid2;
                        c->txVlan2 = (c->rxVlan2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = leave-as-rx\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVid2  = rxVid2;
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->rxVlan2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1 != 0), vid2 = leave-as-rx\n");
                    }
                    break;
                case 1:
                    if (vtag == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);

                        c->txVid2  = rxVid2;
                        c->txVlan2 = (c->rxVlan2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = leave-as-rx\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                    }
                    else
                    {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan2 = (c->txVid2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = update-or-add (if txVid2 != 0)\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                    }
                    break;
                case 2:
                    if (vtag == 0) {
                        // TODO is possible 4 cstum tags comes in?
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVid2  = rxVid2;
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->rxVlan2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1" "!= 0), vid2 = leave-as-rx\n");
                    }
                    else {

                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->txVid2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1" "!= 0), vid2 = update-or-add (if txVid2 != 0)\n");
                    }
                    break;
                case 3:
                    if (vtag == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan2 = (c->txVid2 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = " "update-or-add (if txVid2 != 0)\n");
                        if(!c->rxVlan1) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan1 = (c->txVid1 != 0);
                        c->txVlan2 = (c->txVid2 != 0);
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                        if(!c->rxVlan2 && c->txVlan2)
                            c->numVlans++;
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if txVid1"
                            "!= 0), vid2 = update-or-add (if txVid2 != 0)\n");
                    }
                    break;
                case 4:
                    if (vtag == 0)
                    {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        PRINT2(modDisplayVerbose, key, "vid1 = delete, vid2 = delete\n");
                        if(!(c->rxVlan1 && c->rxVlan2)) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        c->numVlans = 0;
                    }
                    else {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !c->isWindowParsing, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
                        c->txVlan1 = (c->txVid1 != 0);
                        PRINT2(modDisplayVerbose, key, "vid1 = update-or-add (if "
                            "txVid1 != 0), vid2 = delete\n");
                        if(!c->rxVlan2) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: L2 tag can't delete, ipp route\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_EMPTY, INTR_U_REMOVE_VLAN_IPP);
                        }
                        else {
                            c->numVlans--;
                        }
                        if(!c->rxVlan1 && c->txVlan1)
                            c->numVlans++;
                    }
                    break;
                default:
                    PRINT2(modDisplayVerbose, key, "ptag = %0d (UNDEFINED!!)\n",
                        r->modPerPortCfg2.VLAN_TAGGING);
                    break;
            }
        }
    }

    if (!c->isMirror)
    {
        /* VPRI Updates (step 2a and 2c) */
        c->txVpri1 = 0;
        c->txVpri2 = 0;
        if ((c->rxVlan1) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_PCP1_UPDATE == 0))
        {
            c->txVpri1 |= (rxVpri1 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri1 PCP (bits 3:1) set to rxVpri1 PCP "
                "because ingress VLAN1 present and enablePcp1Update = 0\n");
        }
        //FIXME:
        /*else if ((state->PARSER_INFO.ftag_v > 0) &&
                 (r.modPerPortCfg2.ENABLE_PCP1_UPDATE == 0))
        {
            txVpri1 |= (rxFtagVpri & 0x0E);
            PRINT(key, "txVpri1 PCP (bits 3:1) set to rxFtagVpri "
                "PCP because ingress FTAG present and enablePcp1Update = 0\n");
        }*/
        else
        {
            c->txVpri1 |= (newVpri1 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri1 PCP (bits 3:1) set to newVpri1 "
                "PCP (from map)\n");
        }

        if ((c->rxVlan1) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_DEI1_UPDATE == 0))
        {
            c->txVpri1 = ((c->txVpri1) & 0xFE) | (rxVpri1 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to rxVpri1 DEI "
                "because ingress VLAN1 present and enableDei1Update = 0\n");
        }
        //FIXME:
        /*else if ((state->PARSER_INFO.ftag_v > 0) &&
                 (r.modPerPortCfg2.ENABLE_DEI1_UPDATE == 0))
        {
            txVpri1 = (txVpri1 & 0xFE) | (rxFtagVpri & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to rxFtagVpri DEI "
                "because ingress FTAG present and enableDei1Update = 0\n");
        }*/
        else
        {
            c->txVpri1 = (c->txVpri1 & 0xFE) | (newVpri1 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri1 DEI (bit 0) set to newVpri1 DEI "
                "(from map)\n");
        }

        if ((c->rxVlan2) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_PCP2_UPDATE == 0))
        {
            c->txVpri2 |= (rxVpri2 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri2 PCP (bits 3:1) set to rxVpri2 PCP "
                "because ingress VLAN2 present and enablePcp2Update = 0\n");
        }
        else
        {
            c->txVpri2 |= (newVpri2 & 0x0E);
            PRINT2(modDisplayVerbose, key, "txVpri2 PCP (bits 3:1) set to newVpri2 "
                "PCP (from map)\n");
        }

        if ((c->rxVlan2) && (state->TX_TAG != HLP_MODEL_INSERT) &&
            (r->modPerPortCfg2.ENABLE_DEI2_UPDATE == 0))
        {
            c->txVpri2 = (c->txVpri2 & 0xFE) | (rxVpri2 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri2 DEI (bit 0) set to rxVpri2 DEI "
                "because ingress VLAN2 present and enableDei2Update = 0\n");
        }
        else
        {
            c->txVpri2 = (c->txVpri2 & 0xFE) | (newVpri2 & 0x01);
            PRINT2(modDisplayVerbose, key, "txVpri2 DEI (bit 0) set to newVpri2 DEI "
                "(from map)\n");
        }
    }

    PRINT2(modDisplayVerbose, key, "txVlan1 = %0d\n", c->txVlan1);
    PRINT2(modDisplayVerbose, key, "txVlan2 = %0d\n", c->txVlan2);
    PRINT2(modDisplayVerbose, key, "preserveVlan1 = %0d\n", c->preserveVlan1);
    PRINT2(modDisplayVerbose, key, "preserveVlan2 = %0d\n", c->preserveVlan2);
    PRINT2(modDisplayVerbose, key, "txVpri1 = 0x%01x\n", c->txVpri1);
    PRINT2(modDisplayVerbose, key, "txVid1  = 0x%03x\n", c->txVid1);
    PRINT2(modDisplayVerbose, key, "txVpri2 = 0x%01x\n", c->txVpri2);
    PRINT2(modDisplayVerbose, key, "txVid2  = 0x%03x\n", c->txVid2);
}

void ParseMPLS(fm_text key,
               hlp_model *model,
               hlp_modelModRegData *r,
               hlp_modelModControlData *c,
               hlp_modelChunkedSeg *chunkedSeg,
               hlp_modelModDesc *descMaster)
{
    hlp_modelState *state  = &model->packetState;

    FM_NOT_USED(r);

    for (int i = 0; i < chunkedSeg->n_otr_mpls; i++)
    {
        // MPLS ELI label == 20'h0_0007
        if ((chunkedSeg->otr_mpls[(i*4)+0] == 0) &&
            (chunkedSeg->otr_mpls[(i*4)+1] == 0) &&
           ((chunkedSeg->otr_mpls[(i*4)+2] >> 4) == 0x7))
        {
            c->mplsData.otr_eli_idx = i;
            c->mplsData.otr_eli_v = 1;
            if ((c->mplsData.otr_eli_idx+1) < (chunkedSeg->n_otr_mpls-1))
            {
                c->mplsData.otr_al_v = 1;
            }
            break;
        }

        // Only the first 4 labels can be Generic
        if (i < 4)
            c->mplsData.n_otr_g++;
        // ELI should only be looked for in the first 5 labels
        else
            break;
    }
    // If ELI is not found within the first 5 labels and the stack is
    // greater than 4, the last label is the application label
    if ((c->mplsData.otr_eli_v == 0) && (chunkedSeg->n_otr_mpls > 4))
    {
        c->mplsData.otr_al_v = 1;
    }

    // Does packet have mpls on ingress or egress?
	// mpls_pop = 0..2 -> add 0..2 to pop count from IPP
    // if mirror pkt, ignore mpls_pop 0..2
	if (descMaster->mpls_cmd.mpls_pop <= 2) {
        if (!c->isMirror)
		    c->mplsData.mplsPopCount = descMaster->mpls_cmd.mpls_pop + state->MPLS_POP;
        else {
            c->mplsData.mplsPopCount = 0;
            PRINT2(modDisplayVerbose, key, "MISCONFIG: Mir pkt, mpls_pop <= 2\n");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_POP_G, INTR_U_POP_G);
        }
    }
	// mpls_pop = 3..7 -> set pop count to 0..4
	else
		c->mplsData.mplsPopCount = descMaster->mpls_cmd.mpls_pop - 3;

    if (c->mplsData.mplsPopCount > c->mplsData.n_otr_g) {
        c->mplsData.mplsPopCount = c->mplsData.n_otr_g;
        PRINT2(modDisplayVerbose, key, "MISCONFIG: mplsPopCount > g label count\n");
        // TODO should ERR_POP_G be only mpls_pop from desc?:w
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_POP_G, INTR_U_POP_G);
    }
    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing && c->mplsData.mplsPopCount > 0), ERR_POP_G, INTR_U_POP_G);

    if (descMaster->mpls_cmd.pop_eli) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !c->isWindowParsing, ERR_POP_ELI, INTR_U_POP_ELI);
        if ((c->mplsData.otr_eli_v) && (c->mplsData.otr_eli_idx == c->mplsData.mplsPopCount)) {
            c->mplsData.mplsPopEli = 2;
        }
        else {
            PRINT2(modDisplayVerbose, key, "MISCONFIG: can't perform pop eli\n");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_POP_ELI, INTR_U_POP_ELI);
        }
    }
    if (descMaster->mpls_cmd.pop_al) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_POP_AL, INTR_U_POP_AL);
        if ((c->mplsData.otr_al_v) && ((chunkedSeg->n_otr_mpls - c->mplsData.mplsPopCount - c->mplsData.mplsPopEli) == 1)) {
            c->mplsData.mplsPopAl = 1;
        }
        else {
            PRINT2(modDisplayVerbose, key, "MISCONFIG: can't perform pop al\n");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_POP_AL, INTR_U_POP_AL);
        }
    }

    if (descMaster->mpls_cmd.push_al) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_PUSH_AL, INTR_U_PUSH_AL);
// add 2 more argument, isWP, skipDnL
        if (((chunkedSeg->n_otr_mpls - c->mplsData.mplsPopCount - c->mplsData.mplsPopEli - c->mplsData.mplsPopAl) == 0)) {
            c->mplsData.mplsPushAl = 1;
        }
        else {
            PRINT2(modDisplayVerbose, key, "MISCONFIG: can't perform push al\n");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_PUSH_AL, INTR_U_PUSH_AL);
        }
    }
	if (descMaster->mpls_cmd.push_eli) {
       DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_PUSH_ELI, INTR_U_PUSH_ELI);
       if (chunkedSeg->n_otr_mpls - c->mplsData.mplsPopCount - c->mplsData.mplsPopEli - c->mplsData.mplsPopAl + c->mplsData.mplsPushAl <= 5) {
           c->mplsData.mplsPushEli = 2;
       }
       else {
           PRINT2(modDisplayVerbose, key, "MISCONFIG: can't perform push eli\n");
           DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_PUSH_ELI, INTR_U_PUSH_ELI);
       }
    }
    c->mplsData.mplsPushCount = ((descMaster->mpls_cmd.mpls_push >> 0) & 0x01) + ((descMaster->mpls_cmd.mpls_push >> 1) & 0x01) + ((descMaster->mpls_cmd.mpls_push >> 2) & 0x01) + ((descMaster->mpls_cmd.mpls_push >> 3) & 0x01);
    if (c->mplsData.mplsPushCount > 0) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_PUSH_G, INTR_U_PUSH_G);
        if ((chunkedSeg->n_otr_mpls - c->mplsData.mplsPopCount - c->mplsData.mplsPopEli - c->mplsData.mplsPopAl + c->mplsData.mplsPushAl + c->mplsData.mplsPushEli + c->mplsData.mplsPushCount > 7)
             ||
            ((c->mplsData.n_otr_g - c->mplsData.mplsPopCount)*(!c->mplsData.mplsPushEli) + c->mplsData.mplsPushCount > 4)) {
            PRINT2(modDisplayVerbose, key, "MISCONFIG: can't perform push G\n");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_PUSH_G, INTR_U_PUSH_G);
        }
    }
}

void UpdateMacAddrIPP(fm_text key,
                      hlp_model *model,
                      hlp_modelModRegData *r,
                      hlp_modelModControlData *c,
                      hlp_modelChunkedSeg *chunkedSeg)
{
    fm_macaddr  origDmac = 0;
    fm_macaddr  origSmac = 0;
    fm_uint64	*modRouterSmac;
    fm_uint64   temp64;
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(r);

    PRINT(modDisplayVerbose, key, "------- Update MAC IPP -------\n");
    if (!c->isMirror) {
        for (int i = 0 ; i < MAC_ADDR_BYTES ; i++) {
	    	origDmac |= (fm_macaddr) chunkedSeg->otr_dmac[i];
	    	origSmac |= (fm_macaddr) chunkedSeg->otr_smac[i];
	    	if (i < 5) {
	    		origDmac <<= 8;
	    		origSmac <<= 8;
	    	}
	    }
        if (c->isRoutable && r->modPerPortCfg2.ENABLE_DMAC_ROUTING) {
        	/* modify DMAC */
            if(state->PARSER_INFO.otr_l2_len == 0 && !state->NO_MODIFY) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_MAC, INTR_OTR_MAC_NONEXIST);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: otr mac non-exist\n");
            }
        	for (int i = 0, bitOffset = 40 ;
                 i < MAC_ADDR_BYTES;
                 i++, bitOffset -= 8) {
        		chunkedSeg->otr_dmac[i] = GET_BYTE(state->L2_DMAC, bitOffset);
            }

            PRINT2(modDisplayVerbose, key, "Changed DMAC from 0x%012lx to 0x%012lx\n",
                (unsigned long int)origDmac, (unsigned long int)state->L2_DMAC);
        }
        else {
        	PRINT2(modDisplayVerbose, key, "No DMAC modification (0x%012lx)\n",
                (unsigned long int)origDmac);
        }

        if (c->isRoutable && r->modPerPortCfg2.ENABLE_SMAC_ROUTING) {
            /* modify SMAC */
            if(state->PARSER_INFO.otr_l2_len == 0 && !state->NO_MODIFY) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_MAC, INTR_OTR_MAC_NONEXIST);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: otr mac non-exist\n");
            }
            PRINT2(modDisplayVerbose, key, "L3_Domain = 0x%02x\n", c->l3_domain);
            modRouterSmac = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            		HLP_MOD_ROUTER_SMAC(c->l3_domain, 0));
            temp64 = FM_GET_FIELD64(*modRouterSmac, HLP_MOD_ROUTER_SMAC, SMAC);
            for (int i = 0, bitOffset = 40 ;
                 i < MAC_ADDR_BYTES ;
                 i++, bitOffset -= 8) {
            	chunkedSeg->otr_smac[i] = GET_BYTE(temp64, bitOffset);
            }

            PRINT2(modDisplayVerbose, key, "Changed SMAC from 0x%012lx to 0x%012lx "
                "(MOD_ROUTER_SMAC[L3_Domain])\n",
                (unsigned long int)origSmac, (unsigned long int)temp64);
        }
        else {
            PRINT2(modDisplayVerbose, key, "No SMAC modification (0x%012lx)\n",
                (unsigned long int)origSmac);
        }
    }
}

void UpdateVlanIPP(fm_text key,
                   hlp_model *model,
                   hlp_modelModRegData *r,
                   hlp_modelModControlData *c,
                   hlp_modelChunkedSeg *chunkedSeg,
                   hlp_modelModDesc *descMaster)
{ // TODO can be further splited
    fm_byte	n_tag = 0;
    fm_uint64 *rdval;
    fm_uint16 txEtype1 = 0;
    fm_uint16 txEtype2 = 0;
    fm_byte tags[16];

    hlp_modelState *state  = &model->packetState;
    for (int i = 0; i < VLAN_TAG_BYTES*4; i++)
        tags[i] = 0;

    /* read etype values */
    if (c->txVlan1) {
        rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_VLAN_ETYPE(r->modPerPortCfg2.VLAN1_E_TYPE, 0));
        txEtype1 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_ETYPE, TAG_TYPE);
    }

    if (c->txVlan2) {
        rdval = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_VLAN_ETYPE(r->modPerPortCfg2.VLAN2_E_TYPE, 0));
        txEtype2 = FM_GET_FIELD64(*rdval, HLP_MOD_VLAN_ETYPE, TAG_TYPE);
    }

    /* Create New VLANS */
    if (c->txVlan1 && c->txVlan2) {
        if (r->modPerPortCfg2.VID2_FIRST) {
            CopyVlanToOutput(txEtype2, c->txVpri2, c->txVid2, &tags[0], 0);
            PRINT2(modDisplayVerbose, key, "VLAN2 data = 0x%02x%02x%02x%02x\n",
                tags[0], tags[1], tags[2], tags[3]);

            CopyVlanToOutput(txEtype1, c->txVpri1, c->txVid1, &tags[0], 1); /* X */
            PRINT2(modDisplayVerbose, key, "VLAN1 data = 0x%02x%02x%02x%02x\n",
				tags[4], tags[5], tags[6], tags[7]);
            n_tag += 2; /* Y */
        }
        else {
            CopyVlanToOutput(txEtype1, c->txVpri1, c->txVid1, &tags[0], 0);
            PRINT2(modDisplayVerbose, key, "VLAN1 data = 0x%02x%02x%02x%02x\n",
				tags[0], tags[1], tags[2], tags[3]);

            CopyVlanToOutput(txEtype2, c->txVpri2, c->txVid2, &tags[0], 1);
            PRINT2(modDisplayVerbose, key, "VLAN2 data = 0x%02x%02x%02x%02x\n",
            	tags[4], tags[5], tags[6], tags[7]);
            n_tag += 2;
        }
    }
    else if (c->txVlan1) {
        CopyVlanToOutput(txEtype1, c->txVpri1, c->txVid1, &tags[0], 0);
        PRINT2(modDisplayVerbose, key, "VLAN1 data = 0x%02x%02x%02x%02x\n",
			tags[0], tags[1], tags[2], tags[3]);
        n_tag++;
    }
    else if (c->txVlan2) {
        CopyVlanToOutput(txEtype2, c->txVpri2, c->txVid2, &tags[0], 0);
        PRINT2(modDisplayVerbose, key, "VLAN2 data = 0x%02x%02x%02x%02x\n",
			tags[0], tags[1], tags[2], tags[3]);
        n_tag++;
    }

    if (c->rxVlan1 && c->rxVlan2 &&
		!c->preserveVlan1 && !c->preserveVlan2) {
		    PRINT2(modDisplayVerbose, key, "Delete RX Vlan1 & RX Vlan2 and shift otr_tags bytes\n");
		    c->rx_n_tag -= 2;
            if (c->rx_n_tag < 0)
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                        "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                        c->rx_n_tag);
            for (int i = 0; i < c->rx_n_tag*VLAN_TAG_BYTES; i++)
		    	c->rx_tags[i] = c->rx_tags[i+2*VLAN_TAG_BYTES];
  	}
	/* Delete first tag in rx_tags */
	else if ((c->rxVlan1 && !c->preserveVlan1 && !c->rxV2first) ||
	  	 (c->rxVlan2 && !c->preserveVlan2 && c->rxV2first)  ||
           (c->rxVlan2 && !c->preserveVlan2 && !c->rxVlan1)) {
		PRINT2(modDisplayVerbose, key, "Delete first RX Vlan (by order) and shift otr_tags "
            "bytes\n");
		c->rx_n_tag--;
        if (c->rx_n_tag < 0)
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                    "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                    c->rx_n_tag);
		for (int i = 0; i < c->rx_n_tag*VLAN_TAG_BYTES; i++)
			c->rx_tags[i] = c->rx_tags[i+VLAN_TAG_BYTES];
	}
	/* Delete second tag in rx_tags */
	else if ((c->rxVlan1 && !c->preserveVlan1 && c->rxV2first) ||
			 (c->rxVlan2 && !c->preserveVlan2 && !c->rxV2first)) {
		PRINT2(modDisplayVerbose, key, "Delete second RX Vlan (by order) and shift otr_tags "
            "bytes\n");
		c->rx_n_tag--;
        if (c->rx_n_tag < 0)
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                    "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                    c->rx_n_tag);
		for (int i = 0; i < (c->rx_n_tag*VLAN_TAG_BYTES); i++)
			c->rx_tags[i+VLAN_TAG_BYTES] = c->rx_tags[i+2*VLAN_TAG_BYTES];
	}

    /* Copy over Preseved and Cust Tags */
    if (descMaster->otr1_cmd.tunnel_mode == 1) {
        for (int i = 0; i < (n_tag*VLAN_TAG_BYTES); i++)
            chunkedSeg->inr_tags[i] = tags[i];
        for (int i = 0; i < (c->rx_n_tag*VLAN_TAG_BYTES); i++)
            chunkedSeg->inr_tags[(n_tag*VLAN_TAG_BYTES)+i] =
                c->rx_tags[i];
        chunkedSeg->n_inr_tag = n_tag + c->rx_n_tag;

        PRINT2(modDisplayVerbose, key, "n_inr_tags=%0d, tx_tags=0x", chunkedSeg->n_inr_tag);
        for (int i = 0; i < chunkedSeg->n_inr_tag; i++) {
            PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
                chunkedSeg->inr_tags[i*4], chunkedSeg->inr_tags[(i*4)+1],
                chunkedSeg->inr_tags[(i*4)+2], chunkedSeg->inr_tags[(i*4)+3]);
        }
        PRINTF2(modDisplayVerbose, "\n");
    }
    else {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && n_tag > 0 && !c->isMirror), ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);
        for (int i = 0; i < (n_tag*VLAN_TAG_BYTES); i++)
            chunkedSeg->otr_tags[i] = tags[i];
        if ((n_tag+c->rx_n_tag) > 4) { // chop excessive tags since chunk seg can house up to 4 tags
            c->rx_n_tag = 4-n_tag;
            if (c->rx_n_tag < 0)
                FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                        "rx_n_tag (%d) should never be negative, possibly causing segmentation fault\n",
                        c->rx_n_tag);
            PRINT2(modDisplayVerbose, key, "MISCONFIG: can't insert vlan ipp");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_VLAN_TAG_FULL, INTR_U_INSERT_VLAN_IPP);// insert vlan during route
        }
        for (int i = 0; i < (c->rx_n_tag*VLAN_TAG_BYTES); i++)
            chunkedSeg->otr_tags[(n_tag*VLAN_TAG_BYTES)+i] = c->rx_tags[i];
        chunkedSeg->n_otr_tag = n_tag + c->rx_n_tag;

        PRINT2(modDisplayVerbose, key, "n_otr_tags=%0d, tx_tags=0x", chunkedSeg->n_otr_tag);
        for (int i = 0; i < chunkedSeg->n_otr_tag; i++) {
            PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
                chunkedSeg->otr_tags[i*4], chunkedSeg->otr_tags[(i*4)+1],
                chunkedSeg->otr_tags[(i*4)+2], chunkedSeg->otr_tags[(i*4)+3]);
        }
        PRINTF2(modDisplayVerbose, "\n");
    }
}

/*
    populate c->internalDS and c->egressDSCP
    before pop and push
*/
void DSCPTransformation(fm_text key,
                        hlp_model *model,
                        hlp_modelModRegData *r,
                        hlp_modelModControlData *c,
                        hlp_modelChunkedSeg *chunkedSeg,
                        hlp_modelModDesc *descMaster)
{
    // TODO, if it is interLSR can we update Ip hdr's dscp?
    fm_byte modDscpMapIdx;
    fm_uint32 *modDscpMap;
    fm_uint64 temp64;
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(r);

    PRINT2(modDisplayVerbose, key, "------- DSCP Transformation -------\n");
    if(descMaster->qos_cmd.ds_wr == 0) {
        PRINT2(modDisplayVerbose, key, "DS_WR(0) internal DS(0x%0x) from IPP\n", c->internalDS);
    }
    else {
        // TODO do we check ttl_ctrl?
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);

        switch (descMaster->qos_cmd.ds_src) {
            fm_byte   exp;
            fm_uint32 *modExpDs;
            fm_uint64 otrDS64;
        case 0: // Use DS value from IPP, FIXEME, should be fwd_modify.dscp
            PRINT2(modDisplayVerbose, key, "internal DS from IPP\n");
            break;
        case 5: // Value present in descriptor
            PRINT2(modDisplayVerbose, key, "internal DS from Desc\n");
            c->internalDS = descMaster->otr_ds;
            break;
        case 1: case 2: case 3: case 4:
            exp = 0;
            PRINT2(modDisplayVerbose, key, "internal DS from MPLS\n");
            if (c->mplsData.n_otr_g < descMaster->qos_cmd.ds_src) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: MPLS label used for DSCP source not present, mpls.n_otr_g(%0d) ds_src(%0d)\n", c->mplsData.n_otr_g, descMaster->qos_cmd.ds_src);
                c->skipDscp = 1;
                PRINT2(modDisplayVerbose, key, "Skip DSCP Update\n");
            }
            else {
                exp = ((chunkedSeg->otr_mpls[2+(descMaster->qos_cmd.ds_src-1)*4]) >> 1) & 0x7;
                modExpDs = FM_MODEL_GET_REG_PTR(model, HLP_MOD_EXP_DS(c->operator_id, 0));
                otrDS64 = FM_ARRAY_GET_UNNAMED_FIELD64(modExpDs, 0, 64);
                c->internalDS = (otrDS64 >> (exp * 8)) & 0xFF;
            }
            break;
        }
    }

    modDscpMapIdx = (c->priority_profile << 3) | ((c->internalDS >> 5) & 0x07);
    PRINT2(modDisplayVerbose, key, "ModDscpMapIdx {prio_prof, internalDS[7:5]} = {0x%0x, 0x%0x} = 0x%0x\n", c->priority_profile, (c->internalDS>>5), modDscpMapIdx);
    modDscpMap = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DSCP_MAP(modDscpMapIdx, 0));
    temp64 = FM_ARRAY_GET_UNNAMED_FIELD64(modDscpMap, 0, 48);
    c->egressDSCP = (temp64>>(((c->internalDS >> 2) & 0x07)*6)) & 0x3F;

    PRINT2(modDisplayVerbose, key, "internalDS = 0x%0x egressDSCP (for IPP ip hdr) = 0x%0x\n",c->internalDS, c->egressDSCP);
}

void ParseTtlCtrl(fm_text key,
                  hlp_model *model,
                  hlp_modelModControlData *c,
                  hlp_modelChunkedSeg *chunkedSeg,
                  hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    PRINT2(modDisplayVerbose, key, "TTL_CTRL = 0x%0x\n", state->TTL_CTRL);
    if ((descMaster->qos_cmd.ttl_wr == 0) || (descMaster->qos_cmd.ttl_wr == 1 && descMaster->qos_cmd.ttl_src == 0)) {
        switch (state->TTL_CTRL) {
         case 0: /* Ingress otr TTL */
            if (chunkedSeg->otr_ip_size != 0) {
                if (!chunkedSeg->otr_l3_v6)
                    c->otrTTL = chunkedSeg->otr_ip[8];
                else
                    c->otrTTL = chunkedSeg->otr_ip[7];
                PRINT2(modDisplayVerbose, key, "TTL_CTRL: Otr IP, 0x%0x\n", c->otrTTL);
            }
            else {
                if(descMaster->qos_cmd.ttl_wr) {
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: TTL src non-existing\n");
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                }
                c->skipTtl = 1;
                PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
            }
            break;
         case 1: /* Ingress inr TTL */
            if (chunkedSeg->inr_ip_size != 0) {
                if (!chunkedSeg->inr_l3_v6)
                    c->otrTTL = chunkedSeg->inr_ip[8];
                else
                    c->otrTTL = chunkedSeg->inr_ip[7];
                PRINT2(modDisplayVerbose, key, "TTL_CTRL: Inr IP, 0x%0x\n", c->otrTTL);
            }
            else {
                if(descMaster->qos_cmd.ttl_wr) {
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: TTL src non-existing\n");
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                }
                c->skipTtl = 1;
                PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
            }
            break;
         case 2: /* TTL value pointed to by MPLS_POP */
            if (state->MPLS_POP < c->mplsData.n_otr_g) {
                c->otrTTL = chunkedSeg->otr_mpls[state->MPLS_POP*4+3];
                PRINT2(modDisplayVerbose, key, "TTL_CTRL: Fwd_mod mpls_pop, 0x%0x\n", c->otrTTL);
            }
            else {
                if(descMaster->qos_cmd.ttl_wr) {
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: TTL src non-existing\n");
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                }
                c->skipTtl = 1;
                PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
            }
            break;
         case 3: /* Reserved */
            break;
         case 4: case 5: case 6: case 7: /* MPLS TTL from label 1..4 */
            if ((state->TTL_CTRL-4) < c->mplsData.n_otr_g) {
                c->otrTTL = chunkedSeg->otr_mpls[(state->TTL_CTRL-4)*4+3];
                PRINT2(modDisplayVerbose, key, "TTL_CTRL: MPLS_%0d,  0x%0x\n", (state->TTL_CTRL-3), c->otrTTL);
            }
            else {
                if(descMaster->qos_cmd.ttl_wr) {
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: TTL src non-existing\n");
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                }
                c->skipTtl = 1;
                PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
            }
            break;
        }
    }
}

/*
* only update ds.dscp field and ttl field in ip hdr
* leave ds.ecn field to be updated by TODO
* rval: -1 goto DONE, 0 good
*/
int UpdateL3IPP(fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    hlp_modelState *state  = &model->packetState;

    //if (state->PARSER_INFO.otr_l3_len > 0) {
        fm_bool     updateIphdrDSCP;

        PRINT2(modDisplayVerbose, key, "enableTtlDecr = %0d\n",
            r->modPerPortCfg2.ENABLE_TTL_DECREMENT);
        PRINT2(modDisplayVerbose, key, "EgressDSCP = 0x%02x\n",
            c->egressDSCP);

        /* just a dummy-check here: the next nibble should be 4 or 6 */
        if (((chunkedSeg->otr_ip[0] & 0xF0) != 0x40) &&
            ((chunkedSeg->otr_ip[0] & 0xF0) != 0x60) && state->PARSER_INFO.otr_l3_len > 0) {
            PRINT(modDisplayVerbose, key, "ERROR: First nibble of IPv4 or IPv6 is "
                "0x%01x! (should be 4 or 6)\n",
                chunkedSeg->otr_ip[0] >> 4);
            c->dvStatus = INVALID_IP_VERSION;
            //return -1;
        }

        /* update or copy DSCP */
        //TODO, unable to disable dscp updates?
        updateIphdrDSCP = ((state->TX_TAG == HLP_MODEL_NORMAL_TAGGING) && !c->isInterLSR && !c->isMirror);
        if (updateIphdrDSCP && (descMaster->qos_cmd.ds_wr == 0) && !c->skipDscp) {
            if((c->isWindowParsing || state->PARSER_INFO.otr_l3_len ==0) && !state->NO_MODIFY) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_IP, INTR_OTR_IP_NONEXIST);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: otr ip non-exist\n");
                return 0;
            }
            // FIXME: here we only touch the dscp field of otr_ip
            if (!chunkedSeg->otr_l3_v6) {
                PRINT2(modDisplayVerbose, key, "Updating IP otrDSCP from 0x%02x to 0x%02x \n",
                    (chunkedSeg->otr_ip[1]>>2), c->egressDSCP);
                chunkedSeg->otr_ip[1] = (c->egressDSCP << 2) | (chunkedSeg->otr_ip[1] & 0x03);
            }
            else {
                PRINT2(modDisplayVerbose, key, "Updating IP otrDS(clear out ecn) from 0x%01x%01x to 0x%02x \n",
                    (chunkedSeg->otr_ip[0] & 0x0F),
                    (chunkedSeg->otr_ip[1] >> 4) & 0x0C, (c->egressDSCP<<2));
                chunkedSeg->otr_ip[0] = (chunkedSeg->otr_ip[0] & 0xF0) |
                    ((c->egressDSCP >> 2) & 0x0F);
                chunkedSeg->otr_ip[1] = (chunkedSeg->otr_ip[1] & 0x3F) |
                    ((c->egressDSCP << 6) & 0xC0);
            }
            c->otrL3Modified = 1;
        }

        /* dummy check that state->DROP_TTL was set properly based on pkt TTL byte */
        if (!chunkedSeg->otr_l3_v6) {
			      if (state->DROP_TTL && (chunkedSeg->otr_ip[8] > 1)) {
			      	PRINT(modDisplayVerbose, key, "WARNING: state->DROP_TTL doesn't match "
                          "TTL in frame (state->DROP_TTL = 1 and IP TTL = 0x%02x\n",
			      		chunkedSeg->otr_ip[8]);
			      }
			      else if (!state->DROP_TTL && (chunkedSeg->otr_ip[8] <= 1)) {
			      	PRINT(modDisplayVerbose, key, "WARNING: state->DROP_TTL doesn't match "
                          "TTL in frame (state->DROP_TTL = 0 and IP TTL = 0x%02x\n",
			      		chunkedSeg->otr_ip[8]);
			      }
        }
        else {
			      if (state->DROP_TTL && (chunkedSeg->otr_ip[7] > 1)) {
			      	PRINT(modDisplayVerbose, key, "WARNING: state->DROP_TTL doesn't match "
                          "TTL in frame (state->DROP_TTL = 1 and IP TTL = 0x%02x\n",
			      		chunkedSeg->otr_ip[7]);
			      }
			      else if (!state->DROP_TTL && (chunkedSeg->otr_ip[7] <= 1)) {
			      	PRINT(modDisplayVerbose, key, "WARNING: state->DROP_TTL doesn't match "
                          "TTL in frame (state->DROP_TTL = 0 and IP TTL = 0x%02x\n",
			      		chunkedSeg->otr_ip[7]);
			      }
        }

        /* decrement if enabled, even if we're flagging to drop it
         *  (for unit level bench);
         *  RTL doesn't use state->DROP_TTL for this,
         *  and has code to prevent decrementing from 0->0xFF */
        if(!c->isMirror && (descMaster->qos_cmd.ttl_wr == 0) && !c->skipTtl && c->routeA && r->modPerPortCfg2.ENABLE_TTL_DECREMENT && (state->TX_TAG == HLP_MODEL_NORMAL_TAGGING)) {
            if((c->isWindowParsing || state->PARSER_INFO.otr_l3_len ==0) && !state->NO_MODIFY) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_IP, INTR_OTR_IP_NONEXIST);
                PRINT2(modDisplayVerbose, key, "MISCONFIG: otr ip non-exist\n");
                return 0;
            }
            if (!chunkedSeg->otr_l3_v6) {
			    	//TODO chunkedSeg->otr_ip[8]--;
                    if(c->otrTTL == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_TTL_0, INTR_TTL_DEC_ERR);
			    	    PRINT2(modDisplayVerbose, key, "TX_DROP TTL decrement to below 0\n");
                    }
                    else {
			    	    chunkedSeg->otr_ip[8] = c->otrTTL - 1;
                    }
			    	c->otrL3Modified = 1;
			    	PRINT2(modDisplayVerbose, key, "Decrementing TTL: new value = %0d\n",
                        chunkedSeg->otr_ip[8]);
            }
            else {
			    	// TODO, chunkedSeg->otr_ip[7]--;
                    if(c->otrTTL == 0) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_TTL_0, INTR_TTL_DEC_ERR);
			    	    PRINT2(modDisplayVerbose, key, "TX_DROP TTL decrement to below 0\n");
                    }
                    else {
			    	    chunkedSeg->otr_ip[7] = c->otrTTL - 1;
                    }
			    	c->otrL3Modified = 1;
			    	PRINT2(modDisplayVerbose, key, "Decrementing TTL: new value = %0d\n",
                        chunkedSeg->otr_ip[7]);
            }
        }
    //}
    //else if (c->isWindowParsing && !state->NO_MODIFY) {
    //    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_IP);
    //    PRINT2(modDisplayVerbose, key, "MISCONFIG: otr ip non-exist\n");
    //}
    return 0;
}


void UpdateL3LenCsum (fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;
    fm_uint16 oldHdrTotalLen;
    fm_uint16 newHdrTotalLen;
    fm_uint16 newL3TotalLen;

    FM_NOT_USED(r);
    FM_NOT_USED(descMaster);

    PRINT(modDisplayVerbose, key, "------- Update L3 Len and Csum -------\n");
    if (chunkedSeg->otr_ip_size > 0)
    {
        // -4 at the end because we don't include the CRC
        newL3TotalLen = chunkedSeg->otr_ip_size*4 + chunkedSeg->otr_tcp_v*18 +
            chunkedSeg->otr_udp_v*8 + chunkedSeg->tun_size_in_l4_chunk*4 +
            chunkedSeg->tun_opt_size*4 + chunkedSeg->inr_l2_v*14 +
            chunkedSeg->n_inr_tag*4 + chunkedSeg->n_inr_mpls*4 +
            chunkedSeg->inr_ip_size*4 + chunkedSeg->inr_udp_v*8 +
            chunkedSeg->inr_tcp_v*18 + chunkedSeg->payload_size - 4;
        if (!chunkedSeg->otr_l3_v6)
        {
            oldHdrTotalLen = (chunkedSeg->otr_ip[2] << 8) | (chunkedSeg->otr_ip[3]);
        }
        else
        {
            oldHdrTotalLen = (chunkedSeg->otr_ip[4] << 8) | (chunkedSeg->otr_ip[5]);
            // -40 because we do not include the length of the IPv6 header, but we do include the options
            newL3TotalLen -= 40;
        }
        newHdrTotalLen = oldHdrTotalLen + (newL3TotalLen - c->igL3TotalLen);

        if (!chunkedSeg->otr_l3_v6)
        {
            chunkedSeg->otr_ip[2] = (newHdrTotalLen & 0xFF00) >> 8;
            chunkedSeg->otr_ip[3] = (newHdrTotalLen & 0xFF);
        }
        else
        {
            chunkedSeg->otr_ip[4] = (newHdrTotalLen & 0xFF00) >> 8;
            chunkedSeg->otr_ip[5] = (newHdrTotalLen & 0xFF);
        }
        PRINT2(modDisplayVerbose, key, "Updating IP total_length from 0x%04x to 0x%04x\n", oldHdrTotalLen, newHdrTotalLen);
    }

    if ((chunkedSeg->otr_ip_size > 0) &&
        (!chunkedSeg->otr_l3_v6) && c->otrL3Modified)
    {
        fm_byte   ipv4IHL = chunkedSeg->otr_ip[0] & 0x0F;
        fm_uint16 newCsum;
        fm_uint16 oldCsum;
        PRINT(modDisplayVerbose, key, "------- IPv4 Checksum -------\n");

    	/* update checksum if IPv4 */
		oldCsum = (chunkedSeg->otr_ip[10] << 8) | chunkedSeg->otr_ip[11];

		/* calculate and update header checksum in packet */
        newCsum = UpdateChecksum(&(state->RX_DATA[c->l3Idx]),
            &(chunkedSeg->otr_ip[0]), 4*(chunkedSeg->otr_ip_size), oldCsum, key);
		PRINT2(modDisplayVerbose, key, "Updating IPv4 csum from 0x%04x to 0x%04x, l3Idx = 0x%0x\n",
            oldCsum, newCsum, c->l3Idx);
		chunkedSeg->otr_ip[10] = (newCsum & 0xFF00) >> 8;
		chunkedSeg->otr_ip[11] = (newCsum & 0xFF);
    }
}

void UpdateDglortDesc(fm_text key,
                      hlp_modelModControlData *c,
                      hlp_modelChunkedSeg *chunkedSeg,
                      hlp_modelModDesc *descMaster)
{
    FM_NOT_USED(chunkedSeg);

    if (!c->isMirror) {
        if (descMaster->base_cmd.dglort) {
            PRINT2(modDisplayVerbose, key, "update DGLORT to 0x%02x%02x from descriptor\n", descMaster->dglort[0], descMaster->dglort[1]);
            c->txDglort = (descMaster->dglort[0] << 8) | descMaster->dglort[1];
        }
        else
            PRINT2(modDisplayVerbose, key, "base_cmd.dglort is 0\n");
    }
}

void UpdateMacAddrDesc(fm_text key,
                       hlp_model *model,
                       hlp_modelModControlData *c,
                       hlp_modelChunkedSeg *chunkedSeg,
                       hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;
    FM_NOT_USED(c);

    // TODO, does mirror support this
    if(state->PARSER_INFO.otr_l2_len == 0 && !state->NO_MODIFY && (descMaster->base_cmd.otr_dmac || descMaster->base_cmd.otr_smac)) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_MAC, INTR_OTR_MAC_NONEXIST);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: otr mac non-exist, desc base cmd request mac updates\n");
    }

    if (descMaster->base_cmd.otr_dmac) {
        fm_macaddr orig = 0;
        fm_macaddr new_val = 0;
        for (int i = 0; i < MAC_ADDR_BYTES; i++) {
    		orig |= (fm_macaddr) chunkedSeg->otr_dmac[i];
            new_val |= (fm_macaddr) descMaster->otr_dmac[i];
            if ( i < (MAC_ADDR_BYTES-1)) {
                orig <<= 8;
                new_val <<= 8;
            }
            chunkedSeg->otr_dmac[i] = descMaster->otr_dmac[i];
        }
        PRINT2(modDisplayVerbose, key, "Changed DMAC from 0x%012lx to 0x%012lx\n",
            (unsigned long int)orig, (unsigned long int)new_val);
    }
    if (descMaster->base_cmd.otr_smac) {
        fm_macaddr orig = 0;
        fm_macaddr new_val = 0;
        for (int i = 0; i < MAC_ADDR_BYTES; i++) {
            orig |= (fm_macaddr) chunkedSeg->otr_smac[i];
            new_val |= (fm_macaddr) descMaster->otr_smac[i];
            if ( i < (MAC_ADDR_BYTES-1)) {
                orig <<= 8;
                new_val <<= 8;
            }
    		chunkedSeg->otr_smac[i] = descMaster->otr_smac[i];
        }
        PRINT2(modDisplayVerbose, key, "Changed SMAC from 0x%012lx to 0x%012lx\n",
            (unsigned long int)orig, (unsigned long int)new_val);
    }
}

void UpdateL3Desc(fm_text key,
                  hlp_model *model,
                  hlp_modelModControlData *c,
                  hlp_modelChunkedSeg *chunkedSeg,
                  hlp_modelModDesc *descMaster)
{ //TODO, could be further split up
    hlp_modelState *state = &model->packetState;


    if (descMaster->otr1_cmd.otr_ipv6 != chunkedSeg->otr_l3_v6 &&
        descMaster->otr1_ipv_present) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_OTR_IPV, INTR_OTR_IPV_MISMATCH);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: desc.otr1_cmd.otr_ipv6 == %0d,chunkedSeg->otr_l3_v6 == %0d, \n", descMaster->otr1_cmd.otr_ipv6, chunkedSeg->otr_l3_v6);
    }
    //else {
        if (descMaster->otr1_cmd.otr_sip) {
        	fm_char orig[33] = "";
            fm_char new_val[33] = "";
            fm_char tmp[3] = "";
            //for (int i = 0; i < (chunkedSeg->otr_l3_v6 ? 16 : 4); i++) {
            for (int i = 0; i < (descMaster->otr1_cmd.otr_ipv6 ? 16 : 4); i++) {
        		//if (!chunkedSeg->otr_l3_v6) {
        		if (!descMaster->otr1_cmd.otr_ipv6) {
                    sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+12]);
                    chunkedSeg->otr_ip[i+12] = descMaster->otr_sip[i];
        		}
                else {
        			sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+8]);
                    if (!chunkedSeg->otr_l3_v6) {
                        if (i != 2 && i != 3) { 
                            PRINT2(modDisplayVerbose, key, "ipv misconfig happens!\n");
                            chunkedSeg->otr_ip[i+8] = descMaster->otr_sip[i];
                        }
                    }
                    else {
                        chunkedSeg->otr_ip[i+8] = descMaster->otr_sip[i];
                    }
                    // in this case, if misconfigured, say ipv6 sip writes to a
                    // ipv4 hdr, the ipv4 csum part in chunkedSeg will be
                    // override which will cause ipv4 csum mismatch (since our
                    // UpdateCheckSum algorithm assumes 2 csum parts are the same), so we should skip
                    // writing to csum part which is
                }
                strcat(orig, tmp);
                sprintf(tmp, "%02x", descMaster->otr_sip[i]);
                strcat(new_val, tmp);
            }
            c->otrL3Modified = 1;
            c->otrL4Modified = 1;
            PRINT2(modDisplayVerbose, key, "Changed SIP from 0x%s to 0x%s\n", orig, new_val);
        }

        if (descMaster->otr1_cmd.otr_dip) {
            fm_char orig[33] = "";
            fm_char new_val[33] = "";
            fm_char tmp[3] = "";
            //for (int i = 0; i < (chunkedSeg->otr_l3_v6 ? 16 : 4); i++) {
            for (int i = 0; i < (descMaster->otr1_cmd.otr_ipv6 ? 16 : 4); i++) {
        		//if (!chunkedSeg->otr_l3_v6) {
        		if (!descMaster->otr1_cmd.otr_ipv6) {
        			sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+16]);
                    chunkedSeg->otr_ip[i+16] = descMaster->otr_dip[i];
        		}
                else {
        			sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+24]);
                    chunkedSeg->otr_ip[i+24] = descMaster->otr_dip[i];
                }
                strcat(orig, tmp);
                sprintf(tmp, "%02x", descMaster->otr_dip[i]);
                strcat(new_val, tmp);
            }
            c->otrL3Modified = 1;
            c->otrL4Modified = 1;
            PRINT2(modDisplayVerbose, key, "Changed DIP from 0x%s to 0x%s\n", orig, new_val);
        }

        if (descMaster->otr1_cmd.otr_ipflo) {
            fm_uint32 orig = 0;
            fm_uint32 new_val = 0;
            if (!chunkedSeg->otr_l3_v6) {
	    		for (int i = 0; i < 4; i++) {
	    			orig |= (fm_uint32) chunkedSeg->otr_ip[i+4];
                    new_val |= (fm_uint32) descMaster->otr_flow[i];
                    if ( i < 3)
                    {
                        orig <<= 8;
                        new_val <<= 8;
                    }
                    chunkedSeg->otr_ip[i+4] = descMaster->otr_flow[i];
	    		}
        	}
        	else {
	            orig |= (fm_uint32) chunkedSeg->otr_ip[1];
                orig <<= 8;
	    		chunkedSeg->otr_ip[1] = (chunkedSeg->otr_ip[1] & 0xF0) |
                    ((descMaster->otr_flow[1]) & 0x0F);
                new_val |= (fm_uint32) chunkedSeg->otr_ip[1];
                new_val <<= 8;
	            orig |= (fm_uint32) chunkedSeg->otr_ip[2];
                orig <<= 8;
                chunkedSeg->otr_ip[2] = descMaster->otr_flow[2];
                new_val |= (fm_uint32) chunkedSeg->otr_ip[2];
                new_val <<= 8;
	            orig |= (fm_uint32) chunkedSeg->otr_ip[3];
	    		chunkedSeg->otr_ip[3] = descMaster->otr_flow[3];
                new_val |= (fm_uint32) chunkedSeg->otr_ip[3];
            }
            c->otrL3Modified = 1;
            PRINT2(modDisplayVerbose, key, "Changed FLOW from 0x%08x to 0x%08x\n",
                orig, new_val);
        }
        if (descMaster->otr1_cmd.otr_prot) {
            /* lsb-aligned, padded up to 2B (otr_prot[15:8]=0 */
        	if (!chunkedSeg->otr_l3_v6) {
                PRINT2(modDisplayVerbose, key, "Changed PROT from 0x%02x to 0x%02x\n",
                    chunkedSeg->otr_ip[9], descMaster->otr_prot[1]);
        		chunkedSeg->otr_ip[9] = descMaster->otr_prot[1];
        	}
        	else {
                PRINT2(modDisplayVerbose, key, "Changed PROT from 0x%02x to 0x%02x\n",
                    chunkedSeg->otr_ip[6], descMaster->otr_prot[1]);
        		chunkedSeg->otr_ip[6] = descMaster->otr_prot[1];
        	}
            c->otrL3Modified = 1;
            c->otrL4Modified = 1;
        }
        if (descMaster->otr1_cmd.otr_dip_mac && chunkedSeg->otr_l3_v6) {
            fm_char orig[33] = "";
            fm_char new_val[33] = "";
            fm_char tmp[3] = "";
            /* Keep routing prefix (48 bits) & subnet id (16 bits) the same */
            for (int i = 0; i < 8; i++) {
                sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+24]);
                strcat(orig, tmp);
                strcat(new_val, tmp);
            }
            /* Update Interface Identifier (64 bits) */
            for (int i = 0; i < 8; i++) {
                sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+32]);
                strcat(orig, tmp);
                if (i < 3)
                    chunkedSeg->otr_ip[i+32] = chunkedSeg->otr_dmac[i];
                else if (i == 3)
                    chunkedSeg->otr_ip[i+32] = 0xFF;
                else if (i == 4)
                    chunkedSeg->otr_ip[i+32] = 0xFE;
                else if (i > 4)
                    chunkedSeg->otr_ip[i+32] = chunkedSeg->otr_dmac[i-2];
                /* invert the 7th bit */
                if (i == 0)
                    chunkedSeg->otr_ip[i+32] ^= 0x02;
                sprintf(tmp, "%02x", chunkedSeg->otr_ip[i+32]);
                strcat(new_val, tmp);
	    	}
            c->otrL3Modified = 1;
	    	PRINT2(modDisplayVerbose, key, "Changed OTR_DIP_MAC from 0x%s to 0x%s\n", orig, new_val);
        }
        if((c->isWindowParsing || state->PARSER_INFO.otr_l3_len ==0) && !state->NO_MODIFY && (descMaster->otr1_cmd.otr_sip || descMaster->otr1_cmd.otr_dip || descMaster->otr1_cmd.otr_ipflo || descMaster->otr1_cmd.otr_prot || descMaster->otr1_cmd.otr_dip_mac)) {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_IP, INTR_OTR_IP_NONEXIST);
            PRINT2(modDisplayVerbose, key, "MISCONFIG: otr ip non-exist, desc otr1 cmd requires l3 updates\n");
        }
}

void UpdateL4Desc(fm_text key,
                  hlp_model *model,
                  hlp_modelModControlData *c,
                  hlp_modelChunkedSeg *chunkedSeg,
                  hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    if((c->isWindowParsing || (state->PARSER_INFO.otr_l4_tcp ==0 && state->PARSER_INFO.otr_l4_udp == 0)) && !state->NO_MODIFY && (descMaster->otr1_cmd.otr_l4src || descMaster->otr1_cmd.otr_l4dst)) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_OTR_L4, INTR_OTR_L4_NONEXIST);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: otr l4 non-exist, desc otr1 cmd requires l4 updates\n");
    }
    else {
        if (descMaster->otr1_cmd.otr_l4src) {
        	PRINT2(modDisplayVerbose, key, "Changed l4SRC from 0x%02x%02x to 0x%02x%02x\n",
                chunkedSeg->otr_l4[0], chunkedSeg->otr_l4[1],
                descMaster->otr_l4src[0], descMaster->otr_l4src[1]);
            for (int i = 0; i < 2; i++) {
        		chunkedSeg->otr_l4[i] = descMaster->otr_l4src[i];
        	}
            c->otrL4Modified = 1;
        }
        if (descMaster->otr1_cmd.otr_l4dst) {
        	PRINT2(modDisplayVerbose, key, "Changed l4DST from 0x%02x%02x to 0x%02x%02x\n",
                chunkedSeg->otr_l4[2], chunkedSeg->otr_l4[3],
                descMaster->otr_l4dst[0], descMaster->otr_l4dst[1]);
            for (int i = 0; i < 2; i++) {
        		chunkedSeg->otr_l4[i+2] = descMaster->otr_l4dst[i];
        	}
        	c->otrL4Modified = 1;
        }
    }
}

/* return num of vlan tags in desc*/
fm_int NumL2VlanTag(fm_text key,
                    fm_byte *l2tag,
                    fm_int l2_tag_size) {
    fm_int vlan_num;
    vlan_num = 0;

    FM_NOT_USED(key);

    if(l2_tag_size == 1) {
        if((l2tag[0] == 0x81 && l2tag[1] == 0x00) || (l2tag[0] == 0x88 && l2tag[1] == 0xA8))
            vlan_num++;
    }
    else if(l2_tag_size > 1) {
        if((l2tag[0] == 0x81 && l2tag[1] == 0x00) || (l2tag[0] == 0x88 && l2tag[1] == 0xA8))
            vlan_num++;
        if((l2tag[4] == 0x81 && l2tag[5] == 0x00) || (l2tag[4] == 0x88 && l2tag[5] == 0xA8))
            vlan_num++;
    }
    return vlan_num;
}

void UpdateVlanDesc(fm_text key,
                    hlp_model *model,
                    hlp_modelModControlData *c,
                    hlp_modelChunkedSeg *chunkedSeg,
                    hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;
    fm_bool                 tx_drop_otr_tag = 0;
    fm_int                  index = 0;

    if (c->isMirror) {
        descMaster->mpls_cmd.otr_tag_mode = 0;
        descMaster->mpls_cmd.otr_tag_size = 0;
    }
    //else {// TODO for no_modify error log
        for (int i = 0; i < chunkedSeg->n_otr_tag*4 && i<16; i++)
            c->rx_tags[i] = chunkedSeg->otr_tags[i];
        c->rx_n_tag = chunkedSeg->n_otr_tag;
        if(descMaster->mpls_cmd.otr_tag_mode < 6) {
            if((c->rx_n_tag + descMaster->mpls_cmd.otr_tag_size - (descMaster->mpls_cmd.otr_tag_mode)%3) > 4) {
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);// Error insert outer L2 tags desc
                tx_drop_otr_tag = 1;
                PRINT2(modDisplayVerbose, key, "MISCONFIG: desc.mpls_cmd.otr_tag_mode == %d, try to push more than 16B otr_tags totally\n", descMaster->mpls_cmd.otr_tag_mode);
            }
            else {
                c->numVlans += NumL2VlanTag(key, &(descMaster->otr_tags[0]), descMaster->mpls_cmd.otr_tag_size);
            }
            if(((descMaster->mpls_cmd.otr_tag_mode%3) > c->rx_n_tag) && ((descMaster->mpls_cmd.otr_tag_mode % 3) > 0)) {
                PRINT2(modDisplayVerbose, key, "MISCONFIG: desc can't remove vlan\n");
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_OTR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_OTR);// Error delete outer L2 tags desc
            }
            else if ((descMaster->mpls_cmd.otr_tag_mode % 3) > 0) {
                if(descMaster->mpls_cmd.otr_tag_mode < 3) {
                    c->numVlans -= NumL2VlanTag(key, &(c->rx_tags[0]), (descMaster->mpls_cmd.otr_tag_mode % 3));
                }
                else {
                    index = chunkedSeg->n_otr_tag - (descMaster->mpls_cmd.otr_tag_mode % 3);
                    if(index < 4)
                        c->numVlans -= NumL2VlanTag(key, &(c->rx_tags[index*4]), (descMaster->mpls_cmd.otr_tag_mode % 3));
                }
            }
        }
        switch (descMaster->mpls_cmd.otr_tag_mode) {
        case 0: /* Delete 0 tag from the beginning, the insert new tags (if any) */
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);
            chunkedSeg->n_otr_tag = descMaster->mpls_cmd.otr_tag_size;
            for (int i = 0; i < 4*chunkedSeg->n_otr_tag; i++)
                chunkedSeg->otr_tags[i] = descMaster->otr_tags[i];
            for (int i = 0; ((i < 4*c->rx_n_tag) && (chunkedSeg->n_otr_tag*4+i)<16); i++) {
                chunkedSeg->otr_tags[chunkedSeg->n_otr_tag*4+i] =
                    c->rx_tags[i];
            }
            chunkedSeg->n_otr_tag += c->rx_n_tag;
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_DELETE_0_BEG\n");
            break;
        case 1: /* Delete 1 tag from the beginning, the insert new tags (if any) */
             DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_OTR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_OTR);
             DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);
            chunkedSeg->n_otr_tag = descMaster->mpls_cmd.otr_tag_size;
            for (int i = 0; i < 4*chunkedSeg->n_otr_tag; i++)
                chunkedSeg->otr_tags[i] = descMaster->otr_tags[i];
            if ((c->rx_n_tag-1) > 0) {
                for (int i = 0; (i < 4*(c->rx_n_tag-1) && (chunkedSeg->n_otr_tag*4+i<16)); i++) {
                    chunkedSeg->otr_tags[chunkedSeg->n_otr_tag*4+i] =
                        c->rx_tags[i+4];
                }
                chunkedSeg->n_otr_tag += (c->rx_n_tag-1);
            }
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_DELETE_1_BEG\n");
            break;
        case 2: /* Delete 2 tags from the beginning, the insert new tags (if any) */
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_OTR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_OTR);
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);

            chunkedSeg->n_otr_tag = descMaster->mpls_cmd.otr_tag_size;
            for (int i = 0; i < 4*chunkedSeg->n_otr_tag; i++)
                chunkedSeg->otr_tags[i] = descMaster->otr_tags[i];
            if ((c->rx_n_tag-2) > 0) {
                for (int i = 0; (i < 4*(c->rx_n_tag-2) && (chunkedSeg->n_otr_tag*4+i<16)); i++) {
                        chunkedSeg->otr_tags[chunkedSeg->n_otr_tag*4+i] =
                        c->rx_tags[i+8];
                }
                chunkedSeg->n_otr_tag += (c->rx_n_tag-2);
            }
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_DELETE_2_BEG\n");
            break;
        case 3: /* Delete 0 tag from the end, the insert new tags (if any) */
            //for (int i = 0; (i < (4*descMaster->mpls_cmd.otr_tag_size) && (chunkedSeg->n_otr_tag*4+i<16)); i++) {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);

            for (int i = 0; i < (4*descMaster->mpls_cmd.otr_tag_size); i++) {
                index = (chunkedSeg->n_otr_tag*4+i) & 0x0F;
                chunkedSeg->otr_tags[index]
                    = descMaster->otr_tags[i];
            }
            chunkedSeg->n_otr_tag += descMaster->mpls_cmd.otr_tag_size;
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_DELETE_0_END\n");
            break;
        case 4: /* Delete 1 tag from the end, the insert new tags (if any) */
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_OTR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_OTR);
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);

            if (chunkedSeg->n_otr_tag > 0)
                chunkedSeg->n_otr_tag--;
            //for (int i = 0; (i < (4*descMaster->mpls_cmd.otr_tag_size) && (chunkedSeg->n_otr_tag*4+i<16)); i++) {
            for (int i = 0; i < (4*descMaster->mpls_cmd.otr_tag_size); i++) {
                index = (chunkedSeg->n_otr_tag*4+i) & 0x0F;
                chunkedSeg->otr_tags[index]
                    = descMaster->otr_tags[i];
            }
            chunkedSeg->n_otr_tag += descMaster->mpls_cmd.otr_tag_size;
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_DELETE_1_END\n");
            break;
        case 5: /* Delete 2 tags from the end, the insert new tags (if any) */
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_OTR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_OTR);
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);

            if ((chunkedSeg->n_otr_tag-2) > 0)
                chunkedSeg->n_otr_tag -= 2;
            else
                chunkedSeg->n_otr_tag = 0;
            for (int i = 0; i < 4*descMaster->mpls_cmd.otr_tag_size; i++) {
                if ((chunkedSeg->n_otr_tag*4+i<16) && descMaster->mpls_cmd.otr_tag_size <= 2)
                    chunkedSeg->otr_tags[chunkedSeg->n_otr_tag*4+i]
                        = descMaster->otr_tags[i];
            }
            chunkedSeg->n_otr_tag += descMaster->mpls_cmd.otr_tag_size;
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_DELETE_2_END\n");
            break;
        case 6: /* Replace All */
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->mpls_cmd.otr_tag_size > 0), ERR_OTR_L2_FULL, INTR_U_INSERT_L2_TAG_OTR);

            for (int i = 0; i < (4*descMaster->mpls_cmd.otr_tag_size); i++) {
                chunkedSeg->otr_tags[i] = descMaster->otr_tags[i];
            }
            if(descMaster->mpls_cmd.otr_tag_size < 3)
                chunkedSeg->n_otr_tag = descMaster->mpls_cmd.otr_tag_size;
            PRINT2(modDisplayVerbose, key, "OTR_TAG_MODE_REPLACE_ALL\n");
            break;
        case 7: /* Supply 1-2 tags to mod_ctrl_s2 */
            break;
        default:
            PRINT2(modDisplayVerbose, key, "No OTR Tag Mode\n");
            break;
        }
        if(tx_drop_otr_tag) {
            if(descMaster->mpls_cmd.otr_tag_mode<6) {
                //chunkedSeg->n_otr_tag = rx_n_tag - ((descMaster->mpls_cmd.otr_tag_mode)%3);
                chunkedSeg->n_otr_tag = 4;
            }
        }
    //}

    PRINT2(modDisplayVerbose, key, "n_otr_tags=%0d, tx_tags=0x", chunkedSeg->n_otr_tag);
    for (int i = 0; i < chunkedSeg->n_otr_tag; i++) {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
            chunkedSeg->otr_tags[i*4], chunkedSeg->otr_tags[(i*4)+1],
            chunkedSeg->otr_tags[(i*4)+2], chunkedSeg->otr_tags[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");
}

void UpdateInrVni(fm_text key,
                  hlp_modelChunkedSeg *chunkedSeg,
                  hlp_modelModDesc *descMaster)
{
    if (descMaster->inr_cmd.vni) {
        /* For VxLAN, NVGRE, GENEVE the vni is bytes 4,5,6 */
        fm_uint32 orig = 0;
        fm_uint32 new_val = 0;
        for (int i = 0; i < 3; i++) {
            orig |= (fm_uint32) chunkedSeg->tun_opt[i+4];
            new_val |= (fm_uint32) descMaster->vni[i];
            if (i < 2) {
                orig <<= 8;
                new_val <<= 8;
            }
            chunkedSeg->tun_opt[i+4] = descMaster->vni[i];
        }
        PRINT2(modDisplayVerbose, key, "Changed Tun VNI from 0x%08x to 0x%08x\n",
            orig, new_val);
    }
}

void UpdateInrMacAddr(fm_text key,
                      hlp_model *model,
                      hlp_modelModControlData *c,
                      hlp_modelChunkedSeg *chunkedSeg,
                      hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    if((state->PARSER_INFO.inr_l2_len == 0 || c->isWindowParsing) && !state->NO_MODIFY && (descMaster->inr_cmd.inr_dmac || descMaster->inr_cmd.inr_smac)) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_INR_MAC, INTR_INR_MAC_NONEXIST);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: inr mac non-exist, desc inr cmd request mac updates\n");
    }

    if (descMaster->inr_cmd.inr_dmac) {
        fm_macaddr orig = 0;
        fm_macaddr new_val = 0;
        for (int i = 0; i < MAC_ADDR_BYTES; i++) {
    		orig |= (fm_macaddr) chunkedSeg->inr_dmac[i];
            new_val |= (fm_macaddr) descMaster->inr_dmac[i];
            if ( i < (MAC_ADDR_BYTES-1)) {
                orig <<= 8;
                new_val <<= 8;
            }
            chunkedSeg->inr_dmac[i] = descMaster->inr_dmac[i];
        }
        PRINT2(modDisplayVerbose, key, "Changed INR DMAC from 0x%012lx to 0x%012lx\n",
            (unsigned long int)orig, (unsigned long int)new_val);
    }
    if (descMaster->inr_cmd.inr_smac) {
        fm_macaddr orig = 0;
        fm_macaddr new_val = 0;
        for (int i = 0; i < MAC_ADDR_BYTES; i++) {
            orig |= (fm_macaddr) chunkedSeg->inr_smac[i];
            new_val |= (fm_macaddr) descMaster->inr_smac[i];
            if ( i < (MAC_ADDR_BYTES-1)) {
                orig <<= 8;
                new_val <<= 8;
            }
    		chunkedSeg->inr_smac[i] = descMaster->inr_smac[i];
        }
        PRINT2(modDisplayVerbose, key, "Changed INR SMAC from 0x%012lx to 0x%012lx\n",
            (unsigned long int)orig, (unsigned long int)new_val);
    }
}

void UpdateInrL3(fm_text key,
                 hlp_model *model,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg,
                 hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    if((state->PARSER_INFO.inr_l3_len == 0 || c->isWindowParsing) && !state->NO_MODIFY && (descMaster->inr_cmd.inr_sip || descMaster->inr_cmd.inr_dip || descMaster->inr_cmd.inr_dip_mac)) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_INR_IP, INTR_INR_IP_NONEXIST);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: inr ip non-exist, desc inr cmd request l3 updates\n");
    }

    if (descMaster->inr_cmd.inr_ipv6 != chunkedSeg->inr_l3_v6 &&
        descMaster->inr_ipv_present) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_INR_IPV, INTR_INR_IPV_MISMATCH);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: desc.inr_cmd.inr_ipv6 == %0d,chunkedSeg->inr_l3_v6 == %0d, misconfiguration\n", descMaster->inr_cmd.inr_ipv6, chunkedSeg->inr_l3_v6);
    }

    if (descMaster->inr_cmd.inr_sip) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_INR_IPV, INTR_INR_IPV_MISMATCH);
    	fm_char orig[33] = "";
        fm_char new_val[33] = "";
        fm_char tmp[3] = "";
        for (int i = 0; i < (chunkedSeg->inr_l3_v6 ? 16 : 4); i++) {
    		if (!chunkedSeg->inr_l3_v6) {
                sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+12]);
                chunkedSeg->inr_ip[i+12] = descMaster->inr_sip[i];
    		}
            else {
    			sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+8]);
                chunkedSeg->inr_ip[i+8] = descMaster->inr_sip[i];
            }
            strcat(orig, tmp);
            sprintf(tmp, "%02x", descMaster->inr_sip[i]);
            strcat(new_val, tmp);
        }
        PRINT2(modDisplayVerbose, key, "Changed INR SIP from 0x%s to 0x%s\n", orig, new_val);
    }
    if (descMaster->inr_cmd.inr_dip) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_INR_IPV, INTR_INR_IPV_MISMATCH);
        fm_char orig[33] = "";
        fm_char new_val[33] = "";
        fm_char tmp[3] = "";
        for (int i = 0; i < (chunkedSeg->inr_l3_v6 ? 16 : 4); i++) {
    		if (!chunkedSeg->otr_l3_v6) {
    			sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+16]);
                chunkedSeg->inr_ip[i+16] = descMaster->inr_dip[i];
    		}
            else {
    			sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+24]);
                chunkedSeg->inr_ip[i+24] = descMaster->inr_dip[i];
            }
            strcat(orig, tmp);
            sprintf(tmp, "%02x", descMaster->inr_dip[i]);
            strcat(new_val, tmp);
        }
        PRINT2(modDisplayVerbose, key, "Changed INR DIP from 0x%s to 0x%s\n", orig, new_val);
    }
    if (descMaster->inr_cmd.inr_dip_mac) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_INR_IPV, INTR_INR_IPV_MISMATCH);
		fm_char orig[33] = "";
        fm_char new_val[33] = "";
        fm_char tmp[3] = "";
        /* Keep routing prefix (48 bits) & subnet id (16 bits) the same */
        for (int i = 0; i < 8; i++) {
            sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+24]);
            strcat(orig, tmp);
            strcat(new_val, tmp);
        }
        /* Update Interface Identifier (64 bits) */
        for (int i = 0; i < 8; i++) {
            sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+32]);
            strcat(orig, tmp);
            if (i < 3)
                chunkedSeg->inr_ip[i+32] = chunkedSeg->inr_dmac[i];
            else if (i == 3)
                chunkedSeg->inr_ip[i+32] = 0xFF;
            else if (i == 4)
                chunkedSeg->inr_ip[i+32] = 0xFE;
            else if (i > 4)
                chunkedSeg->inr_ip[i+32] = chunkedSeg->inr_dmac[i-2];
            /* invert the 7th bit */
            if (i == 0)
                chunkedSeg->inr_ip[i+32] ^= 0x02;
            sprintf(tmp, "%02x", chunkedSeg->inr_ip[i+32]);
            strcat(new_val, tmp);
		}
        //TODO, c.otrL3Modified = 1;
		PRINT2(modDisplayVerbose, key, "Changed INR_DIP_MAC from 0x%s to 0x%s\n", orig, new_val);
    }
}

void UpdateInrDs(){ ;}//if (descMaster->inr_cmd.inr_ds);}
void UpdateInrTtl(){;}//if (descMaster->inr_cmd.inr_ttl);}

void UpdateInrL4(fm_text key,
                 hlp_model *model,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg,
                 hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    if(((state->PARSER_INFO.inr_l4_udp == 0 && state->PARSER_INFO.inr_l4_tcp) || c->isWindowParsing) && !state->NO_MODIFY && (descMaster->inr_cmd.inr_l4src || descMaster->inr_cmd.inr_l4dst)) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, 0, ERR_INR_L4, INTR_INR_L4_NONEXIST);
        PRINT2(modDisplayVerbose, key, "MISCONFIG: inr l4 non-exist, desc inr cmd request l4 updates\n");
    }

    if (descMaster->inr_cmd.inr_l4src) {
    	PRINT2(modDisplayVerbose, key, "Changed INR l4SRC from 0x%02x%02x to 0x%02x%02x\n",
            chunkedSeg->inr_l4[0], chunkedSeg->inr_l4[1],
            descMaster->inr_l4src[0], descMaster->inr_l4src[1]);
        for (int i = 0; i < 2; i++) {
    		chunkedSeg->inr_l4[i] = descMaster->inr_l4src[i];
    	}
    }
    if (descMaster->inr_cmd.inr_l4dst) {
        PRINT2(modDisplayVerbose, key, "Changed INR l4DST from 0x%02x%02x to 0x%02x%02x\n",
            chunkedSeg->inr_l4[2], chunkedSeg->inr_l4[3],
            descMaster->inr_l4dst[0], descMaster->inr_l4dst[1]);
        for (int i = 0; i < 2; i++) {
    		chunkedSeg->inr_l4[i+2] = descMaster->inr_l4dst[i];
    	}
    }
}

void UpdateInrL2Tags(fm_text key,
                     hlp_model *model,
                     hlp_modelModControlData *c,
                     hlp_modelChunkedSeg *chunkedSeg,
                     hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    for (int i = 0; i < chunkedSeg->n_inr_tag*4; i++)
    // TODO
        c->rx_tags[i] = chunkedSeg->inr_tags[i];
    c->rx_n_tag = chunkedSeg->n_inr_tag;
    if(descMaster->inr_cmd.inr_tag_mode < 6) {
        if((c->rx_n_tag + descMaster->inr_cmd.inr_tag_size - (descMaster->inr_cmd.inr_tag_mode)%3) > 4) {
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);// Error insert inner L2 tags desc
            PRINT2(modDisplayVerbose, key, "MISCONFIG: desc.inr_cmd.inr_tag_mode == %d, try to push more than 16B inr_tags totally\n", descMaster->inr_cmd.inr_tag_mode);
        }
        if(((descMaster->inr_cmd.inr_tag_mode % 3) > c->rx_n_tag) && ((descMaster->inr_cmd.inr_tag_mode % 3) > 0)) {
            PRINT2(modDisplayVerbose, key, "MISCONFIG: can't remove inr vlan desc\n");
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_INR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_INR);// Error insert outer L2 tags desc
        }
    }

    switch (descMaster->inr_cmd.inr_tag_mode) {
    case 0: /* Delete 0 tag from the beginning, the insert new tags (if any) */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->inr_cmd.inr_tag_size > 0), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);
        chunkedSeg->n_inr_tag = descMaster->inr_cmd.inr_tag_size;
        for (int i = 0; i < 4*chunkedSeg->n_inr_tag; i++)
            chunkedSeg->inr_tags[i] = descMaster->inr_tags[i];
        for (int i = 0; i < 4*c->rx_n_tag; i++) {
            chunkedSeg->inr_tags[chunkedSeg->n_inr_tag*4+i] =
                c->rx_tags[i];
        }
        chunkedSeg->n_inr_tag += c->rx_n_tag;
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_DELETE_0_BEG\n");
        break;
    case 1: /* Delete 1 tag from the beginning, the insert new tags (if any) */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_INR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_INR);
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->inr_cmd.inr_tag_size > 0), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);

        chunkedSeg->n_inr_tag = descMaster->inr_cmd.inr_tag_size;
        for (int i = 0; i < 4*chunkedSeg->n_inr_tag; i++)
            chunkedSeg->inr_tags[i] = descMaster->inr_tags[i];
        if ((c->rx_n_tag-1) > 0) {
            for (int i = 0; i < 4*(c->rx_n_tag-1); i++) {
                chunkedSeg->inr_tags[chunkedSeg->n_inr_tag*4+i] =
                    c->rx_tags[i+4];
            }
            chunkedSeg->n_inr_tag += (c->rx_n_tag-1);
        }
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_DELETE_1_BEG\n");
        break;
    case 2: /* Delete 2 tags from the beginning, the insert new tags (if any) */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_INR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_INR);
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->inr_cmd.inr_tag_size > 0), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);

        chunkedSeg->n_inr_tag = descMaster->inr_cmd.inr_tag_size;
        for (int i = 0; i < 4*chunkedSeg->n_inr_tag; i++)
            chunkedSeg->inr_tags[i] = descMaster->inr_tags[i];
        if ((c->rx_n_tag-2) > 0) {
            for (int i = 0; i < 4*(c->rx_n_tag-2); i++) {
                chunkedSeg->inr_tags[chunkedSeg->n_inr_tag*4+i] =
                    c->rx_tags[i+8];
            }
            chunkedSeg->n_inr_tag += (c->rx_n_tag-2);
        }
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_DELETE_2_BEG\n");
        break;
    case 3: /* Delete 0 tag from the end, the insert new tags (if any) */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->inr_cmd.inr_tag_size > 0), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);

        for (int i = 0; i < (4*descMaster->inr_cmd.inr_tag_size); i++) {
            chunkedSeg->inr_tags[chunkedSeg->n_inr_tag*4+i]
                = descMaster->inr_tags[i];
        }
        chunkedSeg->n_inr_tag += descMaster->inr_cmd.inr_tag_size;
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_DELETE_0_END\n");
        break;
    case 4: /* Delete 1 tag from the end, the insert new tags (if any) */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_INR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_INR);
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->inr_cmd.inr_tag_size > 0), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);

        if (chunkedSeg->n_inr_tag > 0)
            chunkedSeg->n_inr_tag--;
        for (int i = 0; i < (4*descMaster->inr_cmd.inr_tag_size); i++) {
            chunkedSeg->inr_tags[chunkedSeg->n_inr_tag*4+i]
                = descMaster->inr_tags[i];
        }
        chunkedSeg->n_inr_tag += descMaster->inr_cmd.inr_tag_size;
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_DELETE_1_END\n");
        break;
    case 5: /* Delete 2 tags from the end, the insert new tags (if any) */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_INR_L2_EMPTY, INTR_U_REMOVE_L2_TAG_INR);
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing && descMaster->inr_cmd.inr_tag_size > 0), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);

        if ((chunkedSeg->n_inr_tag-2) > 0)
            chunkedSeg->n_inr_tag -= 2;
        else
            chunkedSeg->n_inr_tag = 0;
        for (int i = 0; i < (4*descMaster->inr_cmd.inr_tag_size); i++) {
            chunkedSeg->inr_tags[chunkedSeg->n_inr_tag*4+i]
                = descMaster->inr_tags[i];
        }
        chunkedSeg->n_inr_tag += descMaster->inr_cmd.inr_tag_size;
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_DELETE_2_END\n");
        break;
    case 6: /* Replace All */
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, !(c->isWindowParsing), ERR_INR_L2_FULL, INTR_U_INSERT_L2_TAG_INR);

        for (int i = 0; i < (4*descMaster->inr_cmd.inr_tag_size); i++) {
            chunkedSeg->inr_tags[i] = descMaster->inr_tags[i];
        }
        chunkedSeg->n_inr_tag = descMaster->inr_cmd.inr_tag_size;
        PRINT2(modDisplayVerbose, key, "INR_TAG_MODE_REPLACE_ALL\n");
        break;
    case 7: /* Supply 1-2 tags to mod_ctrl_s2 */
        break;
    default:
        PRINT2(modDisplayVerbose, key, "No INR Tag Mode\n");
        break;
    }
    PRINT2(modDisplayVerbose, key, "n_inr_tags=%0d, tx_tags=0x", chunkedSeg->n_inr_tag);
    for (int i = 0; i < chunkedSeg->n_inr_tag; i++) {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
            chunkedSeg->inr_tags[i*4], chunkedSeg->inr_tags[(i*4)+1],
            chunkedSeg->inr_tags[(i*4)+2], chunkedSeg->inr_tags[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");
}

void TtlSrcDesc(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    if (!c->isMirror) {
        if (descMaster->qos_cmd.ttl_wr) {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);

            switch (descMaster->qos_cmd.ttl_src) {
            case 0: /* From IPP TTL_CTRL */
                // should be performed in ParseTtlCtrl
                break;
            case 1: case 2: case 3: case 4: /* top..top-3 MPLS label before PUSH/POP */
                PRINT2(modDisplayVerbose, key, "TTL from MPLS label %0d\n", descMaster->qos_cmd.ttl_src);
                if (c->mplsData.n_otr_g > (descMaster->qos_cmd.ttl_src-1))
                //if (chunkedSeg->n_otr_mpls > (descMaster->qos_cmd.ttl_src-1))
                    c->otrTTL = chunkedSeg->otr_mpls[(descMaster->qos_cmd.ttl_src-1)*4+3];
                else {
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: MPLS label used for TTL source not present, n_otr_mpls(%0d) ttl_src(%0d)\n", chunkedSeg->n_otr_mpls, descMaster->qos_cmd.ttl_src);
                    c->skipTtl = 1;
                    PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
                }
                break;
            case 5: /* Present in descriptor */
                PRINT2(modDisplayVerbose, key, "TTL from Desc\n");
                c->otrTTL = descMaster->otr_ttl;
                break;
            case 6: /* Use label pointed to by total MPLS pop count */
                PRINT2(modDisplayVerbose, key, "TTL from top mpls label after pop\n");
                c->otrTTL = chunkedSeg->otr_mpls[c->mplsData.mplsPopCount*4+3];
                //if (c->mplsData.n_otr_g > c->mplsData.mplsPopCount)
                //if (chunkedSeg->n_otr_mpls > c->mplsData.mplsPopCount + c->mplsData.mplsPopEli + c->mplsData.mplsPopAl)
                if(c->mplsData.n_otr_g <= c->mplsData.mplsPopCount) {
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: MPLS label used for TTL source not present, n_otr_mpls(%0d) total_pop_count(%0d)\n", chunkedSeg->n_otr_mpls, c->mplsData.mplsPopCount);
                    c->skipTtl = 1;
                    PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
                }
                break;
            case 7: /* IP header */
                PRINT2(modDisplayVerbose, key, "TTL from Ip hdr\n");
                if (chunkedSeg->otr_ip_size != 0) {
                    if (!chunkedSeg->otr_l3_v6)
                        c->otrTTL = chunkedSeg->otr_ip[8];
                    else
                        c->otrTTL = chunkedSeg->otr_ip[7];
                    PRINT2(modDisplayVerbose, key, "TTL_CTRL: Otr IP, 0x%0x\n", c->otrTTL);
                }
                else {
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: TTL src non-existing\n");
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_SRC, INTR_TTLDS_NON_SRC);
                    c->skipTtl = 1;
                    PRINT2(modDisplayVerbose, key, "Skip TTL Update\n");
                }
                break;
            }
            if ((descMaster->qos_cmd.ttl_dec > 0) && (c->evidA != state->L2_IVID1) && (c->skipTtl == 0)) {
                if(c->otrTTL < descMaster->qos_cmd.ttl_dec) {
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, state->NO_MODIFY, ERR_TTL_0, INTR_TTL_DEC_ERR);
                    PRINT2(modDisplayVerbose, key, "TX_DROP, decrementing TTL below 0\n");
                }
                else {
                    PRINT2(modDisplayVerbose, key, "Decrementing TTL from %0d to %0d\n", c->otrTTL, c->otrTTL - descMaster->qos_cmd.ttl_dec);
                    c->otrTTL = c->otrTTL - descMaster->qos_cmd.ttl_dec;
                }
            }
        }
    }
}

void PopMPLSG(fm_text key,
              hlp_model *model,
              hlp_modelModControlData *c,
              hlp_modelChunkedSeg *chunkedSeg)
{
    FM_NOT_USED(model);

    if (c->mplsData.mplsPopCount > 0) {
        PRINT2(modDisplayVerbose, key, "pop %0d GLs\n", c->mplsData.mplsPopCount);
        chunkedSeg->n_otr_mpls -= c->mplsData.mplsPopCount;
        if (chunkedSeg->n_otr_mpls == 0) {
            for (int i=0; i <4; i++)
                c->mplsData.last_label_poped[i] = chunkedSeg->otr_mpls[i+((c->mplsData.mplsPopCount-1)*4)];
        }

        for (int i = 0; i < chunkedSeg->n_otr_mpls*4; i++)
            chunkedSeg->otr_mpls[i] = chunkedSeg->otr_mpls[i+(c->mplsData.mplsPopCount*4)];

        if (c->mplsData.otr_eli_v)
            c->mplsData.otr_eli_idx -= c->mplsData.mplsPopCount;
        c->mplsData.n_otr_g -= c->mplsData.mplsPopCount;

        //if (c->mplsData.n_otr_g > 0)
        if (chunkedSeg->n_otr_mpls > 0)
            c->mplsData.legalDepthIdx = 0;
        else
            c->mplsData.legalDepthIdx = -1;
    }
}

void PopMPLSEli(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    FM_NOT_USED(model);
    FM_NOT_USED(descMaster);

	if (c->mplsData.mplsPopEli) {
        PRINT2(modDisplayVerbose, key, "pop eli/el pair\n");
		chunkedSeg->n_otr_mpls -= c->mplsData.mplsPopEli;
        if (chunkedSeg->n_otr_mpls == 0) {
            for (int i=0; i <4; i++)
                c->mplsData.last_label_poped[i] = chunkedSeg->otr_mpls[i+((c->mplsData.mplsPopEli-1)*4)];
        }

        for (int i = 0; i < chunkedSeg->n_otr_mpls*4; i++)
            chunkedSeg->otr_mpls[i] = chunkedSeg->otr_mpls[i+(c->mplsData.mplsPopEli*4)];

        c->mplsData.otr_eli_v = 0;
    }
}

void PopMPLSAl(fm_text key,
               hlp_model *model,
               hlp_modelModControlData *c,
               hlp_modelChunkedSeg *chunkedSeg,
               hlp_modelModDesc *descMaster)
{
    FM_NOT_USED(descMaster);
    FM_NOT_USED(model);

	if (c->mplsData.mplsPopAl) {
        PRINT2(modDisplayVerbose, key, "pop al\n");
		chunkedSeg->n_otr_mpls = 0;
        if (chunkedSeg->n_otr_mpls == 0) {
            for (int i=0; i <4; i++)
                c->mplsData.last_label_poped[i] = chunkedSeg->otr_mpls[i+((c->mplsData.mplsPopAl-1)*4)];
        }

        for (int i=0; i <4; i++)
            c->mplsData.last_label_poped[i] = chunkedSeg->otr_mpls[i];

        c->mplsData.otr_al_v = 0;
    }
}

void StackET_MPLS(fm_text key,
                  hlp_model *model,
                  hlp_modelChunkedSeg *chunkedSeg,
                  hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

	if ((chunkedSeg->n_otr_mpls == 0) && (descMaster->mpls_cmd.push_al |
        descMaster->mpls_cmd.push_eli | descMaster->mpls_cmd.mpls_push)) {
		fm_bool use_dmac_mcast = 0;
		fm_bool use_ipp_mcast = 0;
		fm_uint16 etype;
		fm_uint64 *modMplsStackEt;

		modMplsStackEt = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
	    		HLP_MOD_MPLS_STACK_ET(0));

		etype = FM_GET_FIELD(*modMplsStackEt,
		        HLP_MOD_MPLS_STACK_ET, UCAST_ET);
		use_dmac_mcast = FM_GET_BIT64(*modMplsStackEt,
		        HLP_MOD_MPLS_STACK_ET, USE_DMAC_MCAST);
		use_ipp_mcast = FM_GET_BIT64(*modMplsStackEt,
		        HLP_MOD_MPLS_STACK_ET, USE_IPP_MCAST);
        PRINT2(modDisplayVerbose, key, "use_dmac_mcast=%0d use_ipp_mcast=%0d\n", use_dmac_mcast, use_ipp_mcast);

		if (use_dmac_mcast && ((chunkedSeg->otr_dmac[0] & 0x01) != 0))
		{
            PRINT3(modDisplayVerbose, key, "in dmac mcast\n");
			etype = FM_GET_FIELD(*modMplsStackEt,
					HLP_MOD_MPLS_STACK_ET, MCAST_ET);
		}
		if (use_ipp_mcast && (state->MOD_IP_MCAST_IDX != 0))
		{
            PRINT3(modDisplayVerbose, key, "in ipp mcast\n");
			etype = FM_GET_FIELD(*modMplsStackEt,
					HLP_MOD_MPLS_STACK_ET, MCAST_ET);
		}
        PRINT2(modDisplayVerbose, key, "new_stack_etype=0x%0x\n", etype);
        chunkedSeg->otr_et[0] = (etype >> 8) & 0x0FF;
		chunkedSeg->otr_et[1] = etype & 0x0FF;
	}
}

void PushMPLSAl(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    FM_NOT_USED(key);

    hlp_modelState *state = &model->packetState;

    if (c->mplsData.mplsPushAl) {
        chunkedSeg->n_otr_mpls = 1;
        for (int i = 0; i < 4; i++)
            chunkedSeg->otr_mpls[i] = descMaster->al[i];
        c->mplsData.otr_al_v = 1;
    }
}

void PushMPLSEli(fm_text key,
                 hlp_model *model,
                 hlp_modelModControlData *c,
                 hlp_modelChunkedSeg *chunkedSeg,
                 hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(key);
    FM_NOT_USED(descMaster);

    if (c->mplsData.mplsPushEli) {
        /* Shift AL label if needed */
        for (int i = chunkedSeg->n_otr_mpls*4-1; i >= 0; i--)
            chunkedSeg->otr_mpls[i + 8] = chunkedSeg->otr_mpls[i];

        /* ELI label = 20'h0_0007, BOS = 0,
         * EXP/TTL = same as label preceding ELI */
        chunkedSeg->otr_mpls[0] = 0;
        chunkedSeg->otr_mpls[1] = 0;
        chunkedSeg->otr_mpls[2] = 0x70;
        chunkedSeg->otr_mpls[3] = 0;
        /* EL label = mod_meta[19:0], EXP = anything
         * BOS = 0 or 1 (if AL is present), TTL = 0 */
        chunkedSeg->otr_mpls[4] = (state->MOD_META >> 12) & 0x0FF;
        chunkedSeg->otr_mpls[5] = (state->MOD_META >> 4) & 0x0FF;
        chunkedSeg->otr_mpls[6] = (state->MOD_META << 4) & 0x0F0;
        if (chunkedSeg->n_otr_mpls == 0) // set BOS if EL is pushed at bottom
            chunkedSeg->otr_mpls[6] |= 0x01;
        chunkedSeg->otr_mpls[7] = 0;

        chunkedSeg->n_otr_mpls += c->mplsData.mplsPushEli;

        if (c->mplsData.legalDepthIdx == 0)
            c->mplsData.legalDepthIdx += 2;

        c->mplsData.otr_eli_v = 1;
        c->mplsData.otr_eli_idx = 0;

        c->mplsData.n_otr_g = 0; // if push Eli, there is no g label
    }
}

void PushMPLSG(fm_text key,
               hlp_model *model,
               hlp_modelModControlData *c,
               hlp_modelChunkedSeg *chunkedSeg,
               hlp_modelModDesc *descMaster)
{
    fm_int act_push_count = c->mplsData.mplsPushCount;
    fm_uint stop_counter = 10;
    hlp_modelState *state = &model->packetState;
    int mpls_idx = 0;

	if (descMaster->mpls_cmd.mpls_push != 0) {
        if ((c->mplsData.mplsPushCount + c->mplsData.n_otr_g > 4) || ((c->mplsData.mplsPushCount+chunkedSeg->n_otr_mpls) > 7)) {
            act_push_count = MIN(MIN(4 - c->mplsData.n_otr_g, c->mplsData.mplsPushCount), MIN(7 - chunkedSeg->n_otr_mpls, c->mplsData.mplsPushCount));
            //act_push_count = c->mplsData.mplsPushCount - 1;
            stop_counter = act_push_count; // seems redundant
        }
        // TODO
        //if ((c->mplsData.mplsPushCount + c->mplsData.n_otr_g <= 4) &&
        //   ((c->mplsData.mplsPushCount+chunkedSeg->n_otr_mpls) <= 7)) {

        PRINT2(modDisplayVerbose, key, "actual mplsPushCount = %0d\n", act_push_count);
        /* Make space for new mpls labels */
        for (int j = 0; 27-j-act_push_count*4 >= 0; j++)
            chunkedSeg->otr_mpls[27 - j] = chunkedSeg->otr_mpls[27 - j - act_push_count*4];

        mpls_idx = act_push_count;
        if (((descMaster->mpls_cmd.mpls_push >> 0) & 0x01) && mpls_idx > 0) {
            if(stop_counter != 0){
                mpls_idx--;
                for ( int i = 0; i < 4; i++ )
                    chunkedSeg->otr_mpls[i + mpls_idx*4] = descMaster->mpls[i+12];
            }
            stop_counter--;
        }
        if (((descMaster->mpls_cmd.mpls_push >> 1) & 0x01) && mpls_idx > 0) {
            if(stop_counter != 0){
                mpls_idx--;
                for ( int i = 0; i < 4; i++ )
                    chunkedSeg->otr_mpls[i + mpls_idx*4] = descMaster->mpls[i+8];
            }
            stop_counter--;
        }
        if (((descMaster->mpls_cmd.mpls_push >> 2) & 0x01) && mpls_idx > 0) {
            if(stop_counter != 0){
                mpls_idx--;
                for ( int i = 0; i < 4; i++ )
                    chunkedSeg->otr_mpls[i + mpls_idx*4] = descMaster->mpls[i+4];
            }
            stop_counter--;
        }
        if (((descMaster->mpls_cmd.mpls_push >> 3) & 0x01) && mpls_idx > 0) {
            if(stop_counter != 0){
                mpls_idx--;
                for ( int i = 0; i < 4; i++ )
                    chunkedSeg->otr_mpls[i + mpls_idx*4] = descMaster->mpls[i];
            }
            stop_counter--;
        }
        mpls_idx = act_push_count;

        chunkedSeg->n_otr_mpls += act_push_count;
        chunkedSeg->n_otr_mpls &= 0x07;// TODO mimic rtl behavior when G is more than 4

        c->mplsData.n_otr_g += mpls_idx;
        if (c->mplsData.otr_eli_v)
            c->mplsData.otr_eli_idx += mpls_idx;

        if (c->mplsData.legalDepthIdx >= 0)
            c->mplsData.legalDepthIdx += act_push_count;

        /* Update EXP & TTL for ELI label if necessary */
        if (c->mplsData.mplsPushEli && (c->mplsData.otr_eli_idx > 0)) {
            chunkedSeg->otr_mpls[c->mplsData.otr_eli_idx*4+2] =
                (chunkedSeg->otr_mpls[c->mplsData.otr_eli_idx*4+2] & 0xF1) |
                (chunkedSeg->otr_mpls[(c->mplsData.otr_eli_idx-1)*4+2] & 0x0E);
            chunkedSeg->otr_mpls[c->mplsData.otr_eli_idx*4+3] = chunkedSeg->otr_mpls[(c->mplsData.otr_eli_idx-1)*4+3];
            PRINT2(modDisplayVerbose, key, "In populating eli label, eli label = 0x%x.%x\n", chunkedSeg->otr_mpls[c->mplsData.otr_eli_idx*4+2], chunkedSeg->otr_mpls[c->mplsData.otr_eli_idx*4+3]);
        }
    }
}

void RestoreET_MPLS(fm_text key,
                    hlp_model *model,
                    hlp_modelModControlData *c,
                    hlp_modelChunkedSeg *chunkedSeg,
                    hlp_modelModDesc *descMaster)
{
    FM_NOT_USED(descMaster);

//	if ((chunkedSeg->n_otr_mpls == 0) && ((c->mplsData.mplsPopCount > 0) |
//	    c->mplsData.mplsPopEli | c->mplsData.mplsPopAl |
	if ((chunkedSeg->n_otr_mpls == 0) && (chunkedSeg->n_otr_mpls_pre > 0)) {
		fm_uint16 etype;
		fm_uint32 split;
		fm_uint32 label;
		fm_uint32 *modMplsRestoreEt;

		/* IP headers after MPLS */
		if (chunkedSeg->otr_ip_size != 0) {
			if (!chunkedSeg->otr_l3_v6)
				etype = HLP_MODEL_ETYPE_IPv4;
			else
				etype = HLP_MODEL_ETYPE_IPv6;
		}
		else {
		    label = (c->mplsData.last_label_poped[0] << 24) | (c->mplsData.last_label_poped[1] << 16) |
		    		(c->mplsData.last_label_poped[2] << 8)  | c->mplsData.last_label_poped[3];

			modMplsRestoreEt = FM_MODEL_GET_REG_PTR(model,
		    		HLP_MOD_MPLS_RESTORE_ET(0));
		    split = FM_ARRAY_GET_FIELD(modMplsRestoreEt,
			    HLP_MOD_MPLS_RESTORE_ET, SPLIT);
		    /* EtherType to use if label < split. */
		    /* label is top 20 bits of mpls label */
		    if ((label >> 12) < split)
                etype = FM_GET_FIELD(*modMplsRestoreEt, HLP_MOD_MPLS_RESTORE_ET, ET1);
		    else
			    etype = FM_GET_FIELD(*modMplsRestoreEt, HLP_MOD_MPLS_RESTORE_ET, ET2);
		}
        chunkedSeg->otr_et[0] = (etype >> 8) & 0x0FF;
		chunkedSeg->otr_et[1] = etype & 0x0FF;
	    PRINT2(modDisplayVerbose, key, "Popped all MPLS, update etype to 0x%0x\n", etype);
    }
}

void UpdateL4Csum(fm_text key,
                  hlp_model *model,
                  hlp_modelModControlData *c,
                  hlp_modelChunkedSeg *chunkedSeg)
{
    hlp_modelState *state = &model->packetState;

    if ((chunkedSeg->otr_udp_v | chunkedSeg->otr_tcp_v | ((state->TAIL_CSUM_LEN>>30) == 2)) && c->otrL4Modified)
    {
        fm_uint16 oldCsum = 0;
        fm_uint16 newCsum = 0;
        fm_uint64 length = 0;
        fm_byte oldPseudoHeader[32];
        fm_byte newPseudoHeader[32];
        fm_byte ip_prot_old[2];
        fm_byte ip_prot_new[2];

        // old checksum from either hdr or csum_len
        // TODO will csum from tail_len  equal to the hdr one?
        // if this is the case, doesn't need to code anything here ...
        if (chunkedSeg->otr_udp_v)
            oldCsum = (chunkedSeg->otr_l4[6] << 8) | chunkedSeg->otr_l4[7];
        else if (chunkedSeg->otr_tcp_v) {
            oldCsum = (chunkedSeg->otr_l4[16] << 8) | chunkedSeg->otr_l4[17];
            if (oldCsum == 0xFFFF)
                oldCsum = 0;
        }

        /* Checksum includes PROT from IP header which is only 1 byte     *
         * Need to pad because UpdateChecksum only does 2 byte increments */
        ip_prot_old[0] = 0x00;
        ip_prot_new[0] = 0x00;
        if (chunkedSeg->otr_l3_v6) {
            ip_prot_old[1] = state->RX_DATA[c->l3Idx+6];
            ip_prot_new[1] = chunkedSeg->otr_ip[6];
        }
        else {
            ip_prot_old[1] = state->RX_DATA[c->l3Idx+9];
            ip_prot_new[1] = chunkedSeg->otr_ip[9];
        }
        newCsum = UpdateChecksum(&(ip_prot_old[0]),
            &(ip_prot_new[0]), 2, oldCsum, key);

        /* Checksum includes SIP/DIP from IP header */
        if (chunkedSeg->otr_l3_v6) {
            newCsum = UpdateChecksum(&(state->RX_DATA[c->l3Idx+8]),
                &(chunkedSeg->otr_ip[8]), 32, newCsum, key);
            newCsum = UpdateChecksum(&(state->RX_DATA[c->l4Idx]),
                &(chunkedSeg->otr_l4[0]), 4, newCsum, key);
        }
        else {
            newCsum = UpdateChecksum(&(state->RX_DATA[c->l3Idx+12]),
                &(chunkedSeg->otr_ip[12]), 8, newCsum, key);
            newCsum = UpdateChecksum(&(state->RX_DATA[c->l4Idx]),
                &(chunkedSeg->otr_l4[0]), 4, newCsum, key);
        }

		if (chunkedSeg->otr_udp_v) {
            /* for UDP, 0x0000 means no checksum, 0xFFFF represents 0 checksum */
            if (newCsum == 0x00000)
                newCsum = 0x0FFFF;
            chunkedSeg->otr_l4[6] = (newCsum & 0xFF00) >> 8;
		        chunkedSeg->otr_l4[7] = (newCsum & 0xFF);
        }
        else if (chunkedSeg->otr_tcp_v) {
            /* TCPZeroTransmit selects whether to transmit TCP zero-checksum as
             *  0x0000 or 0xFFFF */
            if (newCsum == 0x0000) {
                fm_bool   TCPZeroTransmit;
                fm_uint64 *modCsumCfg;
                modCsumCfg = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
                        HLP_MOD_CSUM_CFG(0));
                TCPZeroTransmit = FM_GET_BIT64(*modCsumCfg, HLP_MOD_CSUM_CFG,
                        TCP_ZERO_TRANSMIT);
                if (TCPZeroTransmit)
                    newCsum = 0xFFFF;
            }
            chunkedSeg->otr_l4[16] = (newCsum & 0xFF00) >> 8;
		    chunkedSeg->otr_l4[17] = (newCsum & 0xFF);
        }
        PRINT2(modDisplayVerbose, key, "Updating L4 csum from 0x%04x to 0x%04x\n", oldCsum, newCsum);
    }
}

/*
 * Update ECN on internalDS, which might translate to EXP through DS_EXP_MAP
 *         or on ip hdr
 */
void UpdateECN (fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{

    fm_byte   ecn;
    fm_bool   ecn_mark;
    fm_bool   mark_action;
    fm_bool   ecn_disable = 0;
    fm_uint32 *modAqmProfile;
    fm_byte   modAqmProfileIdx;
    fm_uint64 *modWredLevel;
    fm_uint16 aqm_profile_mode;
    fm_bool   aqm_profile_srcidx;
    fm_bool   aqm_profile_ecneligiblemark;
    fm_bool   aqm_profile_dropnonecn;
    fm_uint16 wredLevel;
    fm_uint64   temp64;
    fm_byte   profile;


    hlp_modelState *state = &model->packetState;

    PRINT(modDisplayVerbose, key, "------- Update ECN -------\n");

    if (!c->isMirror && !c->isWindowParsing &&!state->NO_MODIFY) { // ECN drop is ignored for both?
        modAqmProfileIdx = ((c->priority_profile & 0x1F) << 3) |
        ((c->internalDS & 0xE0) >> 5);
        modAqmProfile = FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_AQM_PROFILE(modAqmProfileIdx, 0));
        temp64 = FM_ARRAY_GET_UNNAMED_FIELD64(modAqmProfile, 0, 40);
        profile = (temp64 >> (((c->internalDS & 0x1C) >> 2)*5)) & 0x1F;
        aqm_profile_mode = (profile & 0x18) >> 3;
        aqm_profile_srcidx = (profile & 0x04) >> 2;
        aqm_profile_ecneligiblemark = (profile & 0x02) >> 1;
        aqm_profile_dropnonecn = profile & 0x01;
        modWredLevel = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model,
            HLP_MOD_WRED_LEVEL(state->QEWMA[aqm_profile_srcidx], 0));
        PRINT2(modDisplayVerbose, key, "state->QEWMA[0]=0x%0x QEWMA[1]=0x%0x\n",
              state->QEWMA[0], state->QEWMA[1]);
        wredLevel = FM_GET_FIELD64(*modWredLevel, HLP_MOD_WRED_LEVEL,
            WRED_LEVEL);
        PRINT2(modDisplayVerbose, key, "MOD_AQM_PROFILE[%0d].%0d:DropNonEcn=0x%0x;"
            "EcnEligibleMark=0x%0x;SrcIdx=0x%0x;Mode=0x%0x\n",
            modAqmProfileIdx, ((c->internalDS & 0x1C)>>2), aqm_profile_dropnonecn,
            aqm_profile_ecneligiblemark, aqm_profile_srcidx,
            aqm_profile_mode);

        ecn = c->internalDS & 0x03;
        /*******************************************************
        // ecn_mark modeling based on AQM, internally generated by Modify
        *******************************************************/
        mark_action = 0;
        ecn_mark = 0;
        // ECN updates on IP hdr ds_wr == 0
        if (descMaster->qos_cmd.ds_wr == 0) {
            if ((state->TX_TAG == HLP_MODEL_NORMAL_TAGGING) && !c->isInterLSR && !c->isMirror) {
                if(!r->modPerPortCfg2.ENABLE_ECN_MODIFICATION) {
                    PRINT2(modDisplayVerbose, key, "modPerPortCfg2 ECN disable\n");
                    ecn_disable = 1;
                }
            }
            else {
                ecn_disable = 1;
            }
        }
        else {
            if(descMaster->qos_cmd.aqm_mark_ctrl == 0) {
                PRINT2(modDisplayVerbose, key, "AQM_MARK_MOP(0)\n");
            }
            else if (descMaster->qos_cmd.aqm_mark_ctrl == 1) {
                PRINT2(modDisplayVerbose, key, "AQM_MARK_ENABLE(1)\n");
                state->AQM_MARK_EN = 1;
                ecn_disable = 0;
            }
            else if (descMaster->qos_cmd.aqm_mark_ctrl == 2) {
                PRINT2(modDisplayVerbose, key, "AQM_MARK_DISABLE(2)\n");
                state->AQM_MARK_EN = 0;
                ecn_disable = 0;
            }
        }

        // ECN update or drop is timing sensitive and can't be predicted correctly
        // for chained model, only do it for ULV
        if (state->CALLED_FRM_UNIT) {
            // HLP_MODEL_AQM_PROFILE_MODE_DISABLED = 0,
            // HLP_MODEL_AQM_PROFILE_MODE_RED = 1,
            // HLP_MODEL_AQM_PROFILE_MODE_DCTCP = 2,
            if (aqm_profile_mode == HLP_MODEL_AQM_PROFILE_MODE_DISABLED){
                ;
            }
            else if (aqm_profile_mode == HLP_MODEL_AQM_PROFILE_MODE_RED) {
                if (wredLevel >= state->LFSR) {
                    mark_action = 1;
                }
                PRINT2(modDisplayVerbose, key, "aqm_profile_mode = RED, wredLevel:0x%0x  "
                        "state->LFSR:0x%0x, mark_action = %0d\n", wredLevel,
                        state->LFSR, mark_action);
            }
            else if (aqm_profile_mode == HLP_MODEL_AQM_PROFILE_MODE_DCTCP) {
                if ((state->DCTCP_MARK >> aqm_profile_srcidx) & 0x01) {
                    mark_action = 1;
                }
                PRINT2(modDisplayVerbose, key, "aqm_profile_mode = DCTCP, DCTCP_MARK = "
                        "%0d, mark_action = %0d\n", state->DCTCP_MARK, mark_action);
            }

            if ((!aqm_profile_ecneligiblemark || (ecn != 0)) && mark_action) {
                ecn_mark = 1;
            }
            if (mark_action && aqm_profile_dropnonecn && (ecn == 0)){
                DropAndLog(key, model, c, DISP_ECNDROP, c->isMarkerPkt, DROP, NOLOG, state->NO_MODIFY, state->TX_REASONCODE, INTR_ECN_DROP);
                PRINT2(modDisplayVerbose, key, "ECNDROP: aqm_profile_dropnonecn & ECN == 0\n");
            }
            if (state->AQM_MARK_EN && ecn_mark) {
                ecn = 3; // Congestion Encountered;
                PRINT2(modDisplayVerbose, key, "Congestion Encountered: ECN = 0x3\n");
            }
        }

        if(!ecn_disable) {
            if (descMaster->qos_cmd.ds_wr == 0) {
                PRINT2(modDisplayVerbose, key, "Updating otr_ip ECN to 0x%02x\n", ecn);
                if (!chunkedSeg->otr_l3_v6) {
                    chunkedSeg->otr_ip[1] = (ecn & 0x03) | (chunkedSeg->otr_ip[1] & 0xFC);
                }
                else {
                    chunkedSeg->otr_ip[1] = ((ecn & 0x03) << 4) | (chunkedSeg->otr_ip[1] & 0xCF);
                }
                c->otrL3Modified = 1;
            }
            else {
                PRINT2(modDisplayVerbose, key, "Updating internalDS ECN to 0x%02x\n", ecn);
                c->internalDS = (c->internalDS & 0xFC) | (ecn & 0x03);
            }
            if (ecn == 3) {
                c->ecn_mark = 1;
            }
        }
    }
}

// after pop and push
void UpdateDSCPDesc(fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    fm_uint32 modDsExpIdx = 0;
    fm_uint32 *modDsExp;
    fm_uint64 exp64;
    fm_byte   DS2mplsEXP;
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(r);

    PRINT(modDisplayVerbose, key, "------- Update DSCP Desc -------\n");
    //if (c->skipDscp) {
    //    return;
    //}
    if (!c->isMirror) {
        if(descMaster->qos_cmd.ds_wr == 0) {
            PRINT2(modDisplayVerbose, key, "DS_WR(0), TTLDS_TGT_IP should be updated in L3 Modifications and UpdateECN\n");
        }
        else {
            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, !(c->isWindowParsing), ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);

            switch (descMaster->qos_cmd.ttlds_tgt) {
            case 0: //TTLDS_TGT_IP
                if (state->PARSER_INFO.otr_l3_len == 0) {
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                    PRINT2(modDisplayVerbose, key, "ttldg_tgt(%0d) TTLDS_TGT_IP not exist\n", descMaster->qos_cmd.ttlds_tgt);
                }
                else if (!c->skipDscp) {
                    PRINT2(modDisplayVerbose, key, "TTLDS_TGT_IP: \n");
                    if(!chunkedSeg->otr_l3_v6) {
                        PRINT2(modDisplayVerbose, key, "Updating IP otrDS from 0x%02x to 0x%02x \n", chunkedSeg->otr_ip[1], ((c->egressDSCP << 2) | (c->internalDS & 0x03)));
                        chunkedSeg->otr_ip[1] = (c->egressDSCP << 2) | (c->internalDS & 0x03);
                    }
                    else {
                        PRINT2(modDisplayVerbose, key, "Updating IP otrDS from 0x%01x%01x to 0x%02x \n", (chunkedSeg->otr_ip[0] & 0x0F), (chunkedSeg->otr_ip[1] >> 4) & 0x0F, ((c->egressDSCP << 2) | (c->internalDS & 0x03)));
                        chunkedSeg->otr_ip[0] = (chunkedSeg->otr_ip[0] & 0xF0) | ((c->egressDSCP >> 2) & 0x0F);
                        chunkedSeg->otr_ip[1] = (chunkedSeg->otr_ip[1] & 0x0F) | ((c->egressDSCP << 6) & 0xC0) | ((c->internalDS & 0x03) << 4);
                    }
                    c->otrL3Modified = 1;
                    PRINTF2(modDisplayVerbose, "\n");
                }
                break;
            case 1: case 2: case 3: case 4: // TTLDS_TGT_MPLS_LABEL_*
                PRINT2(modDisplayVerbose, key, "TTLDS_TGT_MPLS_LABEL_%0d \n", descMaster->qos_cmd.ttlds_tgt);
                if ((c->mplsData.legalDepthIdx >= 0) && (descMaster->qos_cmd.ttlds_tgt-1 > c->mplsData.legalDepthIdx)) {
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: Updating EXP ignored since ttldg_tgt tries to touch the label deeper than remaining ingress top\n");
                    // TODO, not drop here but need to error log
                }
                else {
                    if (c->mplsData.n_otr_g < descMaster->qos_cmd.ttlds_tgt) {
                    //if (chunkedSeg->n_otr_mpls < descMaster->qos_cmd.ttlds_tgt) {
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                        PRINT2(modDisplayVerbose, key, "MISCONFIG: Updating EXP err_or since ttldg_tgt tries to touch beyond mpls stack\n");
                    }
                    else if (!c->skipDscp) {
                        modDsExpIdx = ((c->priority_profile << 4) & 0x1F0)|((c->internalDS >> 4) & 0x0F);
                        modDsExp = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DS_EXP(modDsExpIdx, 0));
                        exp64 = FM_ARRAY_GET_UNNAMED_FIELD64(modDsExp, 0, 64);
                        DS2mplsEXP = (exp64 >> ((c->internalDS & 0x0F)*3)&0x07);
                        PRINT2(modDisplayVerbose, key, "Updating EXP of mpls label%0d from 0x%02x to 0x%02x\n",
                            descMaster->qos_cmd.ttlds_tgt-1,
                            chunkedSeg->otr_mpls[(descMaster->qos_cmd.ttlds_tgt-1)*4+2]&0x00E,
                            DS2mplsEXP<<1);
                        chunkedSeg->otr_mpls[(descMaster->qos_cmd.ttlds_tgt-1)*4+2] =
                            (chunkedSeg->otr_mpls[(descMaster->qos_cmd.ttlds_tgt-1)*4+2]&0x0F1)
                            | (DS2mplsEXP<<1);
                    }
                }
                PRINTF2(modDisplayVerbose, "\n");
                break;
            case 5: case 6: case 7: // TTLDS_TGT_MPLS_TOPN_*
                PRINT2(modDisplayVerbose, key, "TTLDS_TGT_MPLS_TOPN_%0d\n", descMaster->qos_cmd.ttlds_tgt-3);
                //if (c->mplsData.n_otr_g >= (descMaster->qos_cmd.ttlds_tgt-3)) {
                //if (chunkedSeg->n_otr_mpls >= (descMaster->qos_cmd.ttlds_tgt-3)) {
                    modDsExpIdx = ((c->priority_profile << 4) & 0x1F0)|((c->internalDS >> 4) & 0x0F);
                    modDsExp = FM_MODEL_GET_REG_PTR(model, HLP_MOD_DS_EXP(modDsExpIdx, 0));
                    exp64 = FM_ARRAY_GET_UNNAMED_FIELD64(modDsExp, 0, 64);
                    DS2mplsEXP = (exp64 >> ((c->internalDS & 0x0F)*3)&0x07);
                    PRINT2(modDisplayVerbose, key, "Updating EXP of top %0d labels to 0x%0x\n", descMaster->qos_cmd.ttlds_tgt-3,DS2mplsEXP);
                    for (int i=0; i<descMaster->qos_cmd.ttlds_tgt-3;i++) {
                        if(c->mplsData.legalDepthIdx >=0 && i > c->mplsData.legalDepthIdx) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: Updating EXP ignored on label[%0d] since deeper than remaining ingress label top\n", i);
                            // Error Log and unmodified
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                            break;
                        }
                        if(i >= c->mplsData.n_otr_g) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: ttlds_tgt points to a non-MPLS label location\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                            break;
                        }

                        if (!c->skipDscp) {
                            chunkedSeg->otr_mpls[i*4+2] = (chunkedSeg->otr_mpls[i*4+2]&0x0F1) | (DS2mplsEXP<<1);
                        }
                    }
                //}
                //else { // configuration error
                //    // Not drop only error log
                //    PRINT2(modDisplayVerbose, key, "MISCONFIG: ttlds_tgt points to a non-MPLS label location\n");
                //    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                //}
                break;
            }
        }
    }
}

void UpdateTTLDesc(fm_text key,
                hlp_model *model,
                hlp_modelModRegData *r,
                hlp_modelModControlData *c,
                hlp_modelChunkedSeg *chunkedSeg,
                hlp_modelModDesc *descMaster)
{
    hlp_modelState *state = &model->packetState;

    FM_NOT_USED(r);

    PRINT(modDisplayVerbose, key, "------- Update TTL Desc -------\n");
    //if (c->skipTtl) {
    //    return;
    //}
    if (!c->isMirror) {
        if(descMaster->qos_cmd.ttl_wr == 0) {
            PRINT2(modDisplayVerbose, key, "DS_TTL(0), TTLDS_TGT_IP should be applied in L3 Modification\n");
        }
        else {
            switch (descMaster->qos_cmd.ttlds_tgt) {
            case 0: //TTLDS_TGT_IP
                if(state->PARSER_INFO.otr_l3_len == 0) {
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: ttlds_tgt(%0d) TTLDS_TGT_IP not exist\n", descMaster->qos_cmd.ttlds_tgt);
                        // Error Log and unmodified
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                }
                else if(!c->skipTtl) {
                        PRINT2(modDisplayVerbose, key, "TTLDS_TGT_IP: \n");
                        if (!chunkedSeg->otr_l3_v6) {
                            PRINT2(modDisplayVerbose, key, "Updating otrTTL from 0x%02x to 0x%02x \n",
                                chunkedSeg->otr_ip[8], c->otrTTL);
                            chunkedSeg->otr_ip[8] = c->otrTTL;
                        }
                        else {
                            PRINT2(modDisplayVerbose, key, "Updating otrTTL from 0x%02x to 0x%02x \n",
                                chunkedSeg->otr_ip[7], c->otrTTL);
                            chunkedSeg->otr_ip[7] = c->otrTTL;
                        }
                        c->otrL3Modified = 1;
                        PRINTF2(modDisplayVerbose, "\n");
                }
                break;
            case 1: case 2: case 3: case 4: // TTLDS_TGT_MPLS_LABEL_*
                PRINT2(modDisplayVerbose, key, "TTLDS_TGT_MPLS_LABEL_%0d\n", descMaster->qos_cmd.ttlds_tgt);
                if (c->mplsData.n_otr_g >= descMaster->qos_cmd.ttlds_tgt) {
                    if(c->mplsData.legalDepthIdx >=0 &&
                       descMaster->qos_cmd.ttlds_tgt-1 > c->mplsData.legalDepthIdx) {
                        PRINT2(modDisplayVerbose, key, "MISCONFIG: Updating TTL ignored on label[%0d] since deeper than remaining ingress label top\n", descMaster->qos_cmd.ttlds_tgt-1);
                        // Error Log and unmodified
                        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                    }
                    else if (!c->skipTtl){
                        PRINT2(modDisplayVerbose, key, "Updating otrTTL from 0x%02x to 0x%02x \n",
                            chunkedSeg->otr_mpls[(descMaster->qos_cmd.ttlds_tgt-1)*4+3],
                            c->otrTTL);
                        chunkedSeg->otr_mpls[(descMaster->qos_cmd.ttlds_tgt-1)*4+3] = c->otrTTL;
                    }
                }
                else {// configuration error
                    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                    PRINT2(modDisplayVerbose, key, "MISCONFIG: ttlds_tgt points to a non-MPLS label location\n");
                }
                PRINTF2(modDisplayVerbose, "\n");
                break;
            case 5: case 6: case 7: // TTLDS_TGT_MPLS_TOPN_*
                PRINT2(modDisplayVerbose, key, "TTLDS_TGT_MPLS_TOPN_%0d\n", descMaster->qos_cmd.ttlds_tgt-3);
                //if (c->mplsData.n_otr_g >= (descMaster->qos_cmd.ttlds_tgt-3)) {
                //if (chunkedSeg->n_otr_mpls >= (descMaster->qos_cmd.ttlds_tgt-3)) {
                    for (int i=0; i<descMaster->qos_cmd.ttlds_tgt-3;i++) {
                        if(c->mplsData.legalDepthIdx >=0 && i >
                           c->mplsData.legalDepthIdx) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: Updating TTL ignored on label[%0d] since deeper than remaining ingress label top\n", i);
                            // Error Log and unmodified
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                            break;
                        }
                        PRINT2(modDisplayVerbose, key, "Updating otrTTL from 0x%02x to 0x%02x \n", chunkedSeg->otr_mpls[i*4+3], c->otrTTL);
                        if(i >= c->mplsData.n_otr_g) {
                            PRINT2(modDisplayVerbose, key, "MISCONFIG: ttlds_tgt points to a non-MPLS label location\n");
                            DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, state->NO_MODIFY, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                            break;
                        }
                        if(!c->skipTtl)
                            chunkedSeg->otr_mpls[i*4+3] = c->otrTTL;
                    }
                //}
                //else { // configuration error
                //    // Not drop only error log
                //    DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, NODROP, LOG, ERR_TTLDS_TGT, INTR_TTLDS_NON_TGT);
                //    PRINT2(modDisplayVerbose, key, "MISCONFIG: ttlds_tgt points to a non-MPLS label location\n");
                //}
                break;
            }
        }
    }
}

void DropPacket(fm_text key,
                hlp_model *model,
                hlp_modelModControlData *c)
{
    hlp_modelState *state;
    state = &model->packetState;

    /* set TX_DISP initial value :
     *  this is the value that would egress with first segment's CmTxInfo;
     *  values seem to apply to the whole frame.
     *  Caveats : - does not predict anything based on ModCtrl.ps.err
     *               (RTL generates DISP_TXERRORDROP if 1st seg
     *               or DISP_TXERROR on other seg's)
     *            - does not predict anything based on ModCtrl.ps.is_timeout
    */
    state->TX_DISP = DISP_UCAST;
    state->TX_DROP = 0;
    c->cancelled_tx_disp = DISP_UCAST;

    /* kellysca: changed if to be based on xcast instead of egress DMAC
     * for stats registers because we use TX_DISP to pick with stats bin
     * to increment */
    /*if (fmModelIsBroadcastMacAddress(state->L2_DMAC)) */
    if (state->XCAST == 2)
    {
        state->TX_DISP = DISP_BCAST;
        PRINT2(modDisplayVerbose, key, "state->TX_DISP = DISP_BCAST (%0d) "
            "based on state->L2_DMAC\n",
            DISP_BCAST);
    }
    /*else if (fmModelIsMulticastMacAddress(state->L2_DMAC))*/
    else if (state->XCAST == 1)
    {
        state->TX_DISP = DISP_MCAST;
        PRINT2(modDisplayVerbose, key, "state->TX_DISP = DISP_MCAST (%0d) "
            "based on state->L2_DMAC\n",
            DISP_MCAST);
    }
    else
    {
        PRINT2(modDisplayVerbose, key, "state->TX_DISP = DISP_UCAST (%0d) "
            "based on state->L2_DMAC\n",
            DISP_UCAST);
    }

    // TODO marker pkt err only disp as marker error
//    if(!c->isMarkerPkt) {
        if (c->routeA && !(c->isMirror) && state->DROP_TTL && !state->TX_DROP) {
            //TODO same error code?
            DropAndLog(key, model, c, DISP_TTL1DROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, ERR_TTL_0, INTR_TTL1_DROP);
            PRINT2(modDisplayVerbose, key, "TTL1: setting state->TX_DISP = DISP_TTL1DROP (%0d) "
                "because because TTL <= 1\n",
                DISP_TTL1DROP);
        }

        if (c->loopbackSuppressDrop && !state->TX_DROP) {
            DropAndLog(key, model, c, DISP_LOOPBACKDROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, 0, INTR_LPBK_DROP);
            PRINT2(modDisplayVerbose, key, "LPBK: setting state->TX_DISP = DISP_LOOPBACKDROP (%0d)"
                " and state->TX_DROP = 1\n",
                DISP_LOOPBACKDROP);
        }
        /* prior to checking IS_TIMEOUT, the RTL checks for uncorrectable
         * SRAM errors here (and in a later stage as well);
         * TX_DISP set to DISP_TXECCDROP not supported by Modify WM stage */
        //istimeout pkt data sits in memory for too long that we internally
        //drop it
        if (state->IS_TIMEOUT) {
            DropAndLog(key, model, c, DISP_TIMEOUTDROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, 0, INTR_TIMEOUT_DROP);
            PRINT2(modDisplayVerbose, key, "TIMEOUT: setting state->TX_DISP = DISP_TIMEOUTDROP (%0d) "
                "and state->TX_DROP = 1 because state->IS_TIMEOUT = 1\n",
                DISP_TIMEOUTDROP);
        }
//    }
    // if OOM and pm_read.err both set at EOP, we count it as OOM in stats
    if ((state->OOM > 0 && state->OOM <= state->PM_ERR_NONSOP) || (state->OOM > 0 && state->PM_ERR_NONSOP == 0)) {
        if (!c->mirrorTrunc) { // if mirTrunc we only receive sop, shouldn't set oom at sop
            DropAndLog(key, model, c, DISP_OOMTRUNC, 0, NODROP, NOLOG, NOSKIP, 0, -1);
            PRINT2(modDisplayVerbose, key, "OOM: setting state->TX_DISP = DISP_OOMTRUNC (%0d)\n", DISP_OOMTRUNC);
        }
    }
//    else if ((state->SEG_META_ERR>0 && state->SEG_META_ERR!=1) ||
//             (state->SEG_META_ERR==1 && !c->mirrorTrunc)) { 
    else if (state->PM_ERR_NONSOP && !c->mirrorTrunc) { //
        state->SEG_META_ERR = 3;
        DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
        PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR casue pm_read.err on nonsop (%0d)\n", 3);
    }
    // Mirror trunc will always maintain seg_meta.err even recalc good crc
    else {// mirror trunc | no pm_err_nonsop
        if (state->RX_LENGTH > DEFAULT_SEGMENT_BYTES) {
            if (c->mirrorTrunc) {
                state->SEG_META_ERR = 0;
                PRINT2(modDisplayVerbose, key, "MirTrunc on Multi-seg: state->SEG_META_ERR to 0\n");
            }
            else {
                if(state->SEG_META_ERR) {
                    DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
                    PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR cause seg_meta.err (%0d)\n", state->SEG_META_ERR);
                }
            }
        }
        else { // single seg
            if(state->SEG_META_ERR) {
                DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
                PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR cause seg_meta.err (%0d)\n", state->SEG_META_ERR);
            }
        } 
    }
//    else if (state->SEG_META_ERR) { 
//    //else if (state->SEG_META_ERR && !c->mirrorTrunc) { 
//        DropAndLog(key, model, c, DISP_TXERROR, 0, NODROP, NOLOG, NOSKIP, 0, -1);
//        PRINT2(modDisplayVerbose, key, "TXERR: setting state->TX_DISP = DISP_TXERROR cause seg_meta.err (%0d)\n", state->SEG_META_ERR);
//    }


    if (state->SAF_ERROR) {
        if (!c->isMarkerPkt) {
            DropAndLog(key, model, c, DISP_TXERRORDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_TX_ERR_DROP);
            PRINT2(modDisplayVerbose, key, "SAF: setting state->TX_DISP = DISP_TXERR_ORDROP (%0d) "
                "and state->TX_DROP = 1 because state->SAF_ERR_OR = 1\n",
                DISP_TXERRORDROP);
        }
        else {
            DropAndLog(key, model, c, DISP_MARKERERRDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_MARKER_ERR_DROP);
            PRINT2(modDisplayVerbose, key, "MARKER: setting state->TX_DISP = DISP_MARKERERRDROP (%0d) "
                "and state->TX_DROP = 1 because state->SAF_ERR_OR = 1\n",
                DISP_TXERRORDROP);
        }
    }
    if (state->PM_ERR) {
        if (!c->isMarkerPkt) {
            DropAndLog(key, model, c, DISP_TXECCDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_TX_ECC_DROP);
            PRINT2(modDisplayVerbose, key, "PM_ERR: setting state->TX_DISP = DISP_TXECCDROP (%0d) "
                "and state->TX_DROP = 1 because state->PM_ERR = 1\n",
                DISP_MARKERERRDROP);
        }
        else {
            DropAndLog(key, model, c, DISP_MARKERERRDROP, 0, DROP, NOLOG, NOSKIP, 0, INTR_MARKER_ERR_DROP);
            PRINT2(modDisplayVerbose, key, "PM_ERR: setting state->TX_DISP = DISP_MARKERERRDROP (%0d) "
                "and state->TX_DROP = 1 because state->PM_ERR = 1\n",
                DISP_MARKERERRDROP);
        }
    }
}

void ReCalcIPcsum(fm_text key,
                   hlp_model *model,
                   hlp_modelModControlData *c,
                   hlp_modelChunkedSeg *chunkedSeg,
                   fm_byte * packet)
{
    hlp_modelState *state  = &model->packetState;

    if (ipv4CsumRecalc & ((state->PARSER_INFO.otr_l3_len > 0) &&
        (!chunkedSeg->otr_l3_v6) && c->otrL3Modified)) {
        fm_uint16   oldCsum = (chunkedSeg->otr_ip[10] << 8) | chunkedSeg->otr_ip[11];
        fm_uint16   newCsum;
        fm_byte     ipv4IHL = chunkedSeg->otr_ip[0] & 0x0F;
        fm_uint32   ipv4TxStart = 8*chunkedSeg->ftag_v + 14 +
            4*chunkedSeg->n_otr_tag + 4*chunkedSeg->n_otr_mpls;

        /* Zero out initial checksum bytes */
        packet[ipv4TxStart + 10] = 0;
        packet[ipv4TxStart + 11] = 0;

        /* calculate and update header checksum in packet */
        newCsum = GetIPv4Checksum(&packet[ipv4TxStart], 4*ipv4IHL);
        chunkedSeg->otr_ip[10] = (newCsum & 0xFF00) >> 8;
        chunkedSeg->otr_ip[11] = (newCsum & 0xFF);
        // packet[ipv4TxStart + 10] = (newCsum & 0xFF00) >> 8;
        // packet[ipv4TxStart + 11] = (newCsum & 0xFF);
        if (newCsum != oldCsum) {
            PRINT2(modDisplayVerbose, key, "ERROR Recalculated IPv4 csum from 0x%04x to 0x%04x\n",
                oldCsum, newCsum);
        }
    }
}

/*****************************************************************************/
/** PackPacket
 * \ingroup intModel
 *
 * \desc            Packs the chunkedSeg data structure into a packet.
 *
 * \param[out]		packet points to the output packet data.
 *
 * \param[in]		rx_packet points to the original packet data.
 *
 * \param[in]       chuckedSeg is a pointer to the unpacked packet.
 *
 * \param[in]		parserInfo is the information from the parser about what
 * 					header information is in the packet.
 *
 * \param[out]		tx_length is the length of the outgoing packet.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status PackPacket(fm_byte             *packet,
				 	        fm_byte             no_modify,
				 	        fm_uint32           rx_length,
				 	        fm_byte             *rx_packet,
                            hlp_modelChunkedSeg *chunkedSeg,
                            hlp_modelModControlData *c,
				 	        fm_uint32			*tx_length,
                            fm_text             key)
{
	fm_status status;
	status = FM_OK;

	fm_uint32 idx;
	fm_uint32 inr_l2_ptr;

	idx = 0;
	inr_l2_ptr =0;

    // isWindowParsing parser_info is 0, however no_modify not neccessary
    if (!c->isWindowParsing && !no_modify) {
        /* FTAG */
        if (chunkedSeg->ftag_v)
        {
            for (int i = 0; i < 8; i++, idx++)
                packet[idx] = chunkedSeg->ftag[i];
        }
        /* OTR ETH */
        for (int i = 0; i < 6; i++, idx++)
            packet[idx] = chunkedSeg->otr_dmac[i];
        for (int i = 0; i < 6; i++, idx++)
            packet[idx] = chunkedSeg->otr_smac[i];
        /* VLAN & Custom Tags */
        for (int i = 0; (i < (4*chunkedSeg->n_otr_tag) && i<16); i++, idx++)
            packet[idx] = chunkedSeg->otr_tags[i];
        /* Etype */
        for (int i = 0; i < 2; i++, idx++)
            packet[idx] = chunkedSeg->otr_et[i];
        /* OTR MPLS */
        for (int i = 0; i < (4*chunkedSeg->n_otr_mpls); i++, idx++)
            packet[idx] = chunkedSeg->otr_mpls[i];
        /* OTR IP */
        for (int i = 0; i < (4*chunkedSeg->otr_ip_size); i++, idx++)
            packet[idx] = chunkedSeg->otr_ip[i];
        /* Layer 4 */
        if (chunkedSeg->otr_udp_v)
        {
            for (int i = 0; i < (8 + chunkedSeg->tun_size_in_l4_chunk*4); i++, idx++)
                packet[idx] = chunkedSeg->otr_l4[i];
            for (int i = 0; i < chunkedSeg->tun_opt_size*4; i++, idx++)
                packet[idx] = chunkedSeg->tun_opt[i];
        }
        else if (chunkedSeg->otr_tcp_v)
        {
            for (int i = 0; i < 18; i++, idx++)
                packet[idx] = chunkedSeg->otr_l4[i];
        }
        else if (chunkedSeg->tun_size_in_l4_chunk || chunkedSeg->tun_opt_size) {
            for (int i = 0; i < chunkedSeg->tun_size_in_l4_chunk*4; i++, idx++)
                packet[idx] = chunkedSeg->otr_l4[i];
            for (int i = 0; i < chunkedSeg->tun_opt_size*4; i++, idx++)
                packet[idx] = chunkedSeg->tun_opt[i];
        }
        /* Inner */
        if (chunkedSeg->inr_l2_v)
        {
            /* ETH */
            for (int i = 0; i < 6; i++, idx++)
                packet[idx] = chunkedSeg->inr_dmac[i];
            for (int i = 0; i < 6; i++, idx++)
                packet[idx] = chunkedSeg->inr_smac[i];
            /* VLAN & Custom Tags */
            for (int i = 0; i < (4*chunkedSeg->n_inr_tag); i++, idx++)
                packet[idx] = chunkedSeg->inr_tags[i];
            /* Etype */
            for (int i = 0; i < 2; i++, idx++)
                packet[idx] = chunkedSeg->inr_et[i];
            /* INR MPLS */
            for (int i = 0; i < (4*chunkedSeg->n_inr_mpls); i++, idx++)
                packet[idx] = chunkedSeg->inr_mpls[i];
        }
        {
            /* IP */
            /* inr_ip_size is expressed as a multiple of 4B */
            for (int i = 0; i < (chunkedSeg->inr_ip_size*4); i++, idx++)
                packet[idx] = chunkedSeg->inr_ip[i];
            if (chunkedSeg->inr_udp_v)
            {
                for (int i = 0; i < 8; i++, idx++)
                    packet[idx] = chunkedSeg->inr_l4[i];
            }
            if (chunkedSeg->inr_tcp_v)
            {
                for (int i = 0; i < 18; i++, idx++)
                    packet[idx] = chunkedSeg->inr_l4[i];
            }
        }
    }
    //else if (c->isWindowParsing) {
    //    if (c->isMirror) {
    //        for (int i = 0; i < 6; i++, idx++)
    //            packet[idx] = rx_packet[idx];
    //        for (int i = 0; i < 6; i++, idx++)
    //            packet[idx] = rx_packet[idx];
    //        chunkedSeg->payload_start += idx;
    //        chunkedSeg->payload_size -= idx;
    //        for (int i = 0; (i < (4*chunkedSeg->n_otr_tag) && i<16); i++, idx++)
    //            packet[idx] = chunkedSeg->otr_tags[i];

    //    }
    //}

    if(!no_modify) {
	    for (int i = 0; i < chunkedSeg->payload_size; i++, idx++)
	    	packet[idx] = rx_packet[i + chunkedSeg->payload_start];

        PRINT2(modDisplayVerbose, key, "tx_length=%0d; chunkedSeg->payload_start=%0d; "
                "chunkedSeg->payload_size=%0d;\n",
                idx, chunkedSeg->payload_start, chunkedSeg->payload_size);
	    *tx_length = idx;
    }
    else {
        for (unsigned int i = 0; i < rx_length; i++, idx++)
      	    packet[idx] = rx_packet[i];
        *tx_length = rx_length;
    }

	return status;
}

void MiscOps(fm_text key,
             hlp_model *model,
             hlp_modelModRegData *r,
             hlp_modelModControlData *c,
             hlp_modelChunkedSeg *chunkedSeg,
             fm_byte *packet)
{
    hlp_modelState *state  = &model->packetState;
    fm_byte crc_idx = 0;

    FM_NOT_USED(chunkedSeg);

    if (state->TX_LENGTH >= state->RX_LENGTH) {
        c->bytesAdded = (state->TX_LENGTH - state->RX_LENGTH);
        c->egressSeg0Bytes = (state->RX_LENGTH >= 192) ?
                           192 + (uint) c->bytesAdded : state->TX_LENGTH;
    }
    else {
        c->bytesAdded = (state->RX_LENGTH - state->TX_LENGTH);
        c->egressSeg0Bytes = (state->RX_LENGTH >= 192) ?
                           192 - (uint) c->bytesAdded: state->TX_LENGTH;
        c->bytesAdded |= 0x80; /* MS bit set -> negative value */
    }
    if (c->mirrorTrunc)
    {
        if ((int)(state->RX_LENGTH) >= (int)(DEFAULT_SEGMENT_BYTES+4))
            crc_idx = 4;
        else if ((int)(state->RX_LENGTH) == (int)(DEFAULT_SEGMENT_BYTES+3))
            crc_idx = 3;
        else if ((int)(state->RX_LENGTH) == (int)(DEFAULT_SEGMENT_BYTES+2))
            crc_idx = 2;
        else if ((int)(state->RX_LENGTH) == (int)(DEFAULT_SEGMENT_BYTES+1))
            crc_idx = 1;
        else
            crc_idx = 0;

        if (crc_idx != 0) {
            if (c->bytesAdded != 0) {
                if (c->bytesAdded & 0x80) {
                    fm_byte bytesDeleted;
                    bytesDeleted = c->bytesAdded & 0x7F;
                    state->TX_LENGTH =(fm_uint32)
                        DEFAULT_SEGMENT_BYTES-bytesDeleted+crc_idx;
                }
                else {
                        state->TX_LENGTH =(fm_uint32)
                            DEFAULT_SEGMENT_BYTES+c->bytesAdded+crc_idx;

                }
            }
            else {
                state->TX_LENGTH = (fm_uint32)DEFAULT_SEGMENT_BYTES+crc_idx;
            }
            // when pkt is larger than 192, we add crc to sop seg when
            // trunc, so stats needs adjusted

            c->egressSeg0Bytes = state->TX_LENGTH;
            PRINT2(modDisplayVerbose, key, "Truncating frame to %0d bytes\n", state->TX_LENGTH);
        }
    }

    // calculate good egress crc
    c->crc_egress = fmCrc32(packet, state->TX_LENGTH-4);

    // apply ingress crc error differential onto egress crc
    // use recalculated egress CRC instead of incremental update for mirror
    // trunc frame, that is how RTL behaves now
    if (!c->mirrorTrunc) {
        c->crc_egress ^= c->crc_ingress_diff;
    }

    for (int i = 0; i < 4; i++)
        packet[state->TX_LENGTH-4+i] = (c->crc_egress >> (i*8)) & 0x0FF;

    PRINT2(modDisplayVerbose, key, "CRC updated to: 0x%2x%2x%2x%2x\n",
        packet[state->TX_LENGTH-4],
        packet[state->TX_LENGTH-3],
        packet[state->TX_LENGTH-2],
        packet[state->TX_LENGTH-1]);

    PRINT2(modDisplayVerbose, key, "bytesAdded by modify = %0d (high bit set -> bytes "
        "subtracted)\n", c->bytesAdded);


    /* mkhan3: egressSeg0Bytes needs to be set properly as well if TX_LENGTH <= MIN_EGRESS_BYTES */
	if (state->TX_LENGTH < MIN_EGRESS_BYTES) {
		PRINT2(modDisplayVerbose, key, "MISCONFIG: segment size < 18B after modification\n");
        DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, NOSKIP, ERR_SEG_S_18, INTR_SMALLER_18B);
	}

    c->refcntSeg0Bytes = c->egressSeg0Bytes;
    c->refcnt_tx_len = state->TX_LENGTH;
    if (r->modPerPortCfg2.MIN_FRAME_SIZE && state->TX_LENGTH < 64) {
		PRINT2(modDisplayVerbose, key, "MinFrameSize asserted, state->TX_LENGTH was %0d; setting refcnt tx_len to 64 bytes\n", state->TX_LENGTH);
        c->refcnt_tx_len = 64;
        c->refcntSeg0Bytes = 64;
    }

}

void ErrorLog(fm_text key, hlp_model *model)
{
    FM_NOT_USED(key);
    FM_NOT_USED(model);
}

void PrintPktMeta(fm_text key,
                  fm_byte pkt_meta[32])
{
    PRINT2(modDisplayVerbose, key, "pkt_meta: ");
    for (int i = 0; i < 32; i++)
    {
        PRINTF2(modDisplayVerbose, "%02x", pkt_meta[i]);
        if (((i+1)%4) == 0) PRINTF2(modDisplayVerbose, " ");
    }
    PRINTF2(modDisplayVerbose, "\n");
}

void UpdatePktMeta(fm_text key,
                   hlp_model *model,
                   hlp_modelModControlData *c,
                   hlpModMirrorProfileTable2 *modMirrorProfileTable2)
{
    hlp_modelState *state;
    state = &model->packetState;
    fm_uint16 txLen;

    /* Fields in pkt_meta we are replacing */
    fm_byte                 cd;
    fm_bool                 pf;
    fm_uint16               q;
    fm_byte                 fn;

    /* Register Pointers and fields */
    fm_uint64				*modDglortMap;
    hlpModDglortMap         dm;
    fm_uint64               *modDglortDec;
    hlpModDglortDec         dd;
    fm_uint32               *modCdFn;
    fm_uint32               *modCPMFn;
    fm_uint64               modCdFnData;
    fm_uint64               modCPMFnData;
    fm_byte                 cdMode;
    fm_uint32               *modCdMap;
    fm_byte                 cdMapIndex;
    fm_uint64               modCdMapData;
    fm_uint64               *modRimmonFn;
    hlpModRimmonFn          rf;

    fm_bool                 isRimmonPort = 0;

    PrintPktMeta(key, state->TX_PKT_META);

    modRimmonFn = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_RIMMON_FN(0));
    rf.PORT_MASK = FM_GET_FIELD64(*modRimmonFn, HLP_MOD_RIMMON_FN, PORT_MASK);
    rf.NEW_TYPE = FM_GET_FIELD64(*modRimmonFn, HLP_MOD_RIMMON_FN, NEW_TYPE);
    isRimmonPort = (rf.PORT_MASK >> state->TX_PORT) & 0x01;

    if (c->isMirror && !isRimmonPort) { // bug 30904, mirror action ignored when rimmon
        fm_uint16 old;
        fm_uint16 new_val;
        fm_byte offset;

        offset = modMirrorProfileTable2->METADATA_BYTE_OFFSET;
        if(offset < 31)
            //old = (state->PKT_META[offset+1] << 8) | state->PKT_META[offset];
            old = (state->TX_PKT_META[offset+1] << 8) | state->TX_PKT_META[offset];
        else
            //old = state->PKT_META[offset] & 0x00FF;
            old = state->TX_PKT_META[offset] & 0x00FF;
        new_val = 0;
        if (modMirrorProfileTable2->METADATA_SET_MODE) {
            for (int i = 0; i < 16; i++) {
                if ((modMirrorProfileTable2->METADATA_MASK >> i) & 0x1)
                    //new_val |= (((fm_uint16)state->PKT_META[9] & 0x00FF) & (1 << i));
                    new_val |= (((fm_uint16)state->TX_PKT_META[9] & 0x00FF) & (1 << i));
                else
                    new_val |= (old & (1 << i));
            }
        }
        else {
            for (int i = 0; i < 16; i++) {
                if ((modMirrorProfileTable2->METADATA_MASK >> i) & 0x1)
                    new_val |= (modMirrorProfileTable2->METADATA_VALUE & (1 << i));
                else
                    new_val |= (old & (1 << i));
            }
        }

        //PRINT2(modDisplayVerbose, key, "Mirror Updating at offset %0d, from 0x%0x to 0x%0x\n",
        //      offset, state->PKT_META[offset], new_val);
        PRINT2(modDisplayVerbose, key, "Mirror Updating at offset %0d, from 0x%0x to 0x%0x\n",
              offset, state->TX_PKT_META[offset], new_val);

        //state->PKT_META[offset] = (new_val & 0x0FF);
        state->TX_PKT_META[offset] = (new_val & 0x0FF);
        //state->PKT_META[offset+1] = (new_val >> 8) & 0x0FF;
        state->TX_PKT_META[offset+1] = (new_val >> 8) & 0x0FF;

        if (modMirrorProfileTable2->REPLACE_TYPE)
            //state->PKT_META[0] = modMirrorProfileTable2->TYPE_VALUE;
            state->TX_PKT_META[0] = modMirrorProfileTable2->TYPE_VALUE;
    }

    if (!isRimmonPort) {
        /* Defines CD */
        modCdFn = FM_MODEL_GET_REG_PTR(model, HLP_MOD_CD_FN(0));
        modCdFnData = FM_ARRAY_GET_UNNAMED_FIELD64(modCdFn, 0, 48);
        cdMode = (modCdFnData >> (state->TX_PORT*2)) & 0x03;

        switch (cdMode)
        {
        case 0: /* NOP */
            break;
        case 1: /* TC */
            cd = state->TC;
            //state->PKT_META[3] &= 0xc7;
            state->TX_PKT_META[3] &= 0xc7;
            //state->PKT_META[3] |= (cd << 3) & 0x38;
            state->TX_PKT_META[3] |= (cd << 3) & 0x38;
            PRINT2(modDisplayVerbose, key, "Updating CD to (TC) 0x%0x\n", cd);
            break;
        case 2: /* MOD_CD_MAP */
            cdMapIndex = (state->TX_PKT_META[3] >> 4) & 0x03;
            cd = state->TX_PKT_META[3] & 0x0F;
            modCdMap = FM_MODEL_GET_REG_PTR(model, HLP_MOD_CD_MAP(cdMapIndex, 0));
            modCdMapData = FM_ARRAY_GET_UNNAMED_FIELD64(modCdMap, 0, 48);
            PRINT2(modDisplayVerbose, key, "MOD_CD_MAP idx = %0d, orig 5b cd = 0x%0x\n", cdMapIndex, cd);
            cd = (modCdMapData >> cd*3) & 0x07;
            //state->PKT_META[3] &= 0xc7;
            state->TX_PKT_META[3] &= 0xc7;
            //state->PKT_META[3] |= (cd << 3) & 0x38;
            state->TX_PKT_META[3] |= (cd << 3) & 0x38;
            PRINT2(modDisplayVerbose, key, "Updating CD to (MDO_CD_MAP) 0x%0x\n", cd);
            break;
        default:
            PRINT2(modDisplayVerbose, key, "cdMode=%0d from MOD_CD_FN is not valid\n", cdMode);
            break;
        }

        /* Defines queue, function and function type */
        if (!state->SKIP_DGLORT_DEC && !state->NO_MODIFY) {
            fm_bool hit = 0;
            fm_byte hitIdx = 0;
            for (int i = 63; i >= 0; i--) {// highest-numbered matching
                modDglortMap = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_DGLORT_MAP(i , 0));
                dm.VALUE = FM_GET_FIELD64(*modDglortMap, HLP_MOD_DGLORT_MAP, VALUE);
                dm.MASK = FM_GET_FIELD64(*modDglortMap, HLP_MOD_DGLORT_MAP, MASK);

                if (dm.VALUE == (c->txDglort & dm.MASK)) {
                    hit = 1;
                    hitIdx = i;
                    break;
                }
            }
            if (hit) {
                modDglortDec = (fm_uint64 *) FM_MODEL_GET_REG_PTR(model, HLP_MOD_DGLORT_DEC(hitIdx, 0));
                dd.QUEUE_BASE = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, QUEUE_BASE);
                dd.QUEUE_OFFSET_START = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, QUEUE_OFFSET_START);
                dd.QUEUE_OFFSET_LEN = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, QUEUE_OFFSET_LEN);
                dd.NUM_PRIORITY_BITS = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, NUM_PRIORITY_BITS);
                dd.FUNCTION_BASE = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, FUNCTION_BASE);
                dd.FUNCTION_OFFSET_START = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, FUNCTION_OFFSET_START);
                dd.FUNCTION_OFFSET_LEN = FM_GET_FIELD64(*modDglortDec, HLP_MOD_DGLORT_DEC, FUNCTION_OFFSET_LEN);
                dd.FUNCTION_TYPE = FM_GET_BIT64(*modDglortDec, HLP_MOD_DGLORT_DEC, FUNCTION_TYPE);

                PRINT2(modDisplayVerbose, key, "hitIdx=%0d\n", hitIdx);
                PRINT2(modDisplayVerbose, key, "Qbase=0x%0x QoffStart=0x%0x QoffLen=0x%0x\n",
                      dd.QUEUE_BASE, dd.QUEUE_OFFSET_START, dd.QUEUE_OFFSET_LEN);
                PRINT2(modDisplayVerbose, key, "numPbits=0x%0x txDglort=0x%0x\n",
                      dd.NUM_PRIORITY_BITS, c->txDglort);
                PRINT2(modDisplayVerbose, key, "FunBase=0x%0x FunOffStart=0x%0x FunOffLen=0x%0x FunType=0x%0x\n",
                      dd.FUNCTION_BASE, dd.FUNCTION_OFFSET_START, dd.FUNCTION_OFFSET_LEN,
                      dd.FUNCTION_TYPE);
                q = dd.QUEUE_BASE + (((c->txDglort >> dd.QUEUE_OFFSET_START)
                    & (((1<<(dd.QUEUE_OFFSET_START+dd.QUEUE_OFFSET_LEN))-1)>>dd.QUEUE_OFFSET_START))
                    << dd.NUM_PRIORITY_BITS) +
                    (state->TC & ((1 << dd.NUM_PRIORITY_BITS) -1));

                fn = dd.FUNCTION_BASE + ((c->txDglort >> dd.FUNCTION_OFFSET_START)
                        & (((1<<(dd.FUNCTION_OFFSET_START+dd.FUNCTION_OFFSET_LEN))-1)>>dd.FUNCTION_OFFSET_START));

                pf = dd.FUNCTION_TYPE;

                //state->PKT_META[2] = q & 0x0FF;
                state->TX_PKT_META[2] = q & 0x0FF;
                //state->PKT_META[3] &= 0x78;
                state->TX_PKT_META[3] &= 0x78;
                //state->PKT_META[3] |= (q >> 8) & 0x07;
                state->TX_PKT_META[3] |= (q >> 8) & 0x07;

                //state->PKT_META[1] = fn;
                state->TX_PKT_META[1] = fn;

                //state->PKT_META[3] |= pf << 7;
                state->TX_PKT_META[3] |= pf << 7;

                PRINT2(modDisplayVerbose, key, "Updating q to 0x%0x; fn to 0x%0x; pf to 0x%0x\n", q, fn, pf);
            }
        }
        /*Defines pkt_len*/
        modCPMFn = FM_MODEL_GET_REG_PTR(model, HLP_MOD_CPM_FN(0));
        modCPMFnData = FM_ARRAY_GET_UNNAMED_FIELD64(modCPMFn, 0, 24);
        if (((modCPMFnData >> state->TX_PORT) & 0x1) == 1) {
            if ((state->TAIL_CSUM_LEN>>30) >= 4) {
                PRINT2(modDisplayVerbose, key, "MISCONFIG: can't update pkt_len, since tail_csum_len.type is sctp\n");
                DropAndLog(key, model, c, DISP_MODERRORDROP, c->isMarkerPkt, DROP, LOG, NOSKIP, ERR_PKT_LEN, INTR_PKT_LEN_MD_UPDATE_ERR);
            }

            if(c->mirrorTrunc)
                txLen = c->refcntSeg0Bytes;
            else
                txLen = c->tail_len + c->refcntSeg0Bytes;
            state->TX_PKT_META[13] = txLen & 0x0FF;
            state->TX_PKT_META[14] = (txLen >> 8) & 0xFF;
            PRINT2(modDisplayVerbose, key, "Updating pktLen to 0x%0x\n", txLen);
        }
    }
    /* Define new type */
    else {//if (isRimmonPort)
        //if ((rf.NEW_TYPE == 0x01) | (rf.NewType == 0x16) |
        //    (rf.NEW_TYPE == 0x17))
//        if ((state->PKT_META[0] == 0x01) | (state->PKT_META[0] == 0x16))
//            state->PKT_META[1] = (state->PKT_META[8] & 0x7F) | (state->PKT_META[1] & 0x80);
//        else
//            state->PKT_META[1] &= 0x80;
        if ((state->TX_PKT_META[0] == 0x01) | (state->TX_PKT_META[0] == 0x16))
            state->TX_PKT_META[1] = (state->TX_PKT_META[8] & 0x7F) | (state->TX_PKT_META[1] & 0x80);
        else
            state->TX_PKT_META[1] &= 0x80;


//        state->PKT_META[1] &= 0x7F;
//        state->PKT_META[2] &= 0x01;
//        state->PKT_META[3] = 0;
//
//        state->PKT_META[0] = rf.NEW_TYPE;
//        state->PKT_META[2] = state->PKT_META[22] & 0x01;
        state->TX_PKT_META[1] &= 0x7F;
        state->TX_PKT_META[2] &= 0x01;
        state->TX_PKT_META[3] = 0;

        state->TX_PKT_META[0] = rf.NEW_TYPE;
        state->TX_PKT_META[2] = state->PKT_META[22] & 0x01;

        for (int i = 4; i < 32; i++)
            //state->PKT_META[i] = 0;
            state->TX_PKT_META[i] = 0;

        PRINT2(modDisplayVerbose, key, "Port is Rimmon: reformating and updating Type to 0x%0x and "
            //"Timestamp to 0x%0x\n", state->PKT_META[0], state->PKT_META[1]);
            "Timestamp to 0x%0x\n", state->TX_PKT_META[0], state->TX_PKT_META[1]);
    }

    /* checksum DCN */

    if (!isRimmonPort) {
        PRINT2(modDisplayVerbose, key, "case_code = %d\n", (int)(state->TAIL_CSUM_LEN>>30));
        switch (state->TAIL_CSUM_LEN>>30) {// case_code 
	    	case 0: // DEFAULT
                break;
            case 1: {// VALIDATEL4CHECKSUM
                fm_bool drop;
                fm_bool l3_err;
                fm_bool l4_err;

                drop   = (state->TAIL_CSUM_LEN >> 2) & 0x1;
                l3_err = (state->TAIL_CSUM_LEN >> 1) & 0x1;
                l4_err = state->TAIL_CSUM_LEN & 0x1;
                if (drop) {
                    if (l3_err) {
                        DropAndLog(key, model, c, DISP_L3ERRDROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, 0, INTR_L3_LEN_ERR_DROP);
                        PRINT2(modDisplayVerbose, key, "VALIDATE: L3 length err drop\n");
                    }
                    if (l4_err) {
                        DropAndLog(key, model, c, DISP_L4ERRDROP, c->isMarkerPkt, DROP, NOLOG, NOSKIP, 0, INTR_L4_CSUM_ERR_DROP);
                        PRINT2(modDisplayVerbose, key, "VALIDATE: L4 csum err drop\n");
                    }
                }
                else {
                    if (l3_err || l4_err) {
                        DropAndLog(key, model, c, DISP_L3L4ERRMARK, c->isMarkerPkt, NODROP, NOLOG, NOSKIP, 0, INTR_L3_LEN_L4_CSUM_ERR_MASK);
                        PRINT2(modDisplayVerbose, key, "VALIDATE: l3 or l4 err mark in MD\n");
                        state->TX_PKT_META[18] = (state->TX_PKT_META[18] & 0xBF) | (0x1 << 6);
                    }
                }
                break;
            }
            case 2: // COMPUTEL4CHECKSUM
                break;
            case 3: // STOREL4PARTIALCHECKSUM_TCP_UDP
//                state->PKT_META[28] = state->TAIL_CSUM_LEN & 0xFF;
//                state->PKT_META[29] = (state->TAIL_CSUM_LEN & 0xFF00) >> 8;
                state->TX_PKT_META[28] = state->TAIL_CSUM_LEN & 0xFF;
                state->TX_PKT_META[29] = (state->TAIL_CSUM_LEN & 0xFF00) >> 8;
                break;
            case 4: // STOREL4PARTIALCHECKSUM_SCTP
            case 5:
            case 6:
            case 7:
//                state->PKT_META[28] = state->TAIL_CSUM_LEN & 0xFF;
//                state->PKT_META[29] = (state->TAIL_CSUM_LEN & 0xFF00) >> 8;
//                state->PKT_META[15] = (state->TAIL_CSUM_LEN & 0xFF0000) >> 16;
//                state->PKT_META[11] = (state->TAIL_CSUM_LEN & 0xFF000000) >> 24;
                state->TX_PKT_META[28] = state->TAIL_CSUM_LEN & 0xFF;
                state->TX_PKT_META[29] = (state->TAIL_CSUM_LEN & 0xFF00) >> 8;
                state->TX_PKT_META[15] = (state->TAIL_CSUM_LEN & 0xFF0000) >> 16;
                state->TX_PKT_META[11] = (state->TAIL_CSUM_LEN & 0xFF000000) >> 24;
                break;
            default:
                break;
        }
    }
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelModify
 * \ingroup intModel
 *
 * \desc            Modifies incoming packet based on information from prior
 *                  stages, and stores new packet into parameter
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       packet points to output packet buffer
 *
 * \param[in]       maxPktSize is number of caller-allocated bytes in packet
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 * \see             MODIFY section of EAS for HLP
 *
 *****************************************************************************/
fm_status hlpModelModify(hlp_model *         model,
                         fm_byte *           packet,
                         fm_uint32           maxPktSize)
{
    hlp_modelState *        state = &model->packetState;
    fm_text                 key = fmAlloc(20);
    fm_status               status;
    int rval = 0;

    /* data from configuration registers and state->RX_TAG */
    hlp_modelModRegData     r;
    fm_bool                 portIsFtagged = 0;
    hlp_modelModControlData	c;

    hlp_modelModDesc		*descMaster;
    hlp_modelModDesc		desc0;
    hlp_modelModDesc		desc1;
    hlp_modelModDesc		desc2;
    hlp_modelModDesc		desc3;
    //hlp_modelModDesc		desc;

    /* Chunked Packet */
    hlp_modelChunkedSeg		chunkedSeg;

    FM_NOT_USED(maxPktSize);

    /* register read storage */
    hlpModMirrorProfileTable1 modMirrorProfileTable1;
    hlpModMirrorProfileTable2 modMirrorProfileTable2;

    sprintf(key, "%0x_%0d_%0d", state->ADDR, state->TX_PORT,
        state->MOD_IP_MCAST_IDX);

    descMaster = &desc0;

    FM_CLEAR(desc0);
    FM_CLEAR(desc1);
    FM_CLEAR(desc2);
    FM_CLEAR(desc3);
    FM_CLEAR(chunkedSeg);
    FM_CLEAR(c);
    FM_CLEAR(r);
    FM_CLEAR(modMirrorProfileTable1);
    FM_CLEAR(modMirrorProfileTable2);

    if (testPlusArgs("HLP_MOD_WM_PRINT_VERBOSE") >= 0)
        modDisplayVerbose = testPlusArgs("HLP_MOD_WM_PRINT_VERBOSE");
    else
        modDisplayVerbose = 1;

    status = FM_OK;

    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  MODIFY WM \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    GetModRegData(key, model, &r);

    CalcIngressCRC(key, model, &c);

    InitChunkedSeg(key, model, packet, &chunkedSeg);

    InitControl(key, model, &r, &c, &chunkedSeg);

    ExtractPktMeta(key, model, &c);

    /*******************************************************
     * L2 Modifications
     *******************************************************/
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  L2 MODIFICATIONS \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    MulticastLookup(key, model, &c, &modMirrorProfileTable1);

    MirrorLookup(key, model, &c, &modMirrorProfileTable1, &modMirrorProfileTable2);

    LoopbackSuppress(key, model, &r, &c);

    ModDescLookup(key, model, &c, &descMaster, &desc0, &desc1, &desc2, &desc3);

    GetRxL2Tags(key, model, &c, &chunkedSeg, descMaster);

    DropPacket(key, model, &c);

    VlanLookup(key, model, &r, &c, &chunkedSeg, &modMirrorProfileTable2, descMaster);

    ParseMPLS(key, model, &r, &c, &chunkedSeg, descMaster);

    // intermediate LSR : if the size of the MPLS stack immediately preceding
    // the L3 header under modification is greater than 0 on both ingress and egress.
    // DSCP update should be cancelled on intermediate LSR. This rule applies to IPP update only
    c.isInterLSR = (state->PARSER_INFO.otr_mpls_len != 0) &
        ((state->PARSER_INFO.otr_mpls_len+
        c.mplsData.mplsPushCount+c.mplsData.mplsPushEli+c.mplsData.mplsPushAl
        -c.mplsData.mplsPopCount-c.mplsData.mplsPopEli-c.mplsData.mplsPopAl) > 0);

    PRINT2(modDisplayVerbose, key, "isInterLSR = %0d\n", c.isInterLSR);

    /* DMAC/SMAC update */
    PRINT2(modDisplayVerbose, key, "enableDmacRouting = %0d\n", r.modPerPortCfg2.ENABLE_DMAC_ROUTING);
    PRINT2(modDisplayVerbose, key, "enableSmacRouting = %0d\n", r.modPerPortCfg2.ENABLE_SMAC_ROUTING);
    c.isRoutable = c.routeA &&
        (state->TX_TAG == HLP_MODEL_NORMAL_TAGGING) &&
        !(c.isMirror);

    PRINT2(modDisplayVerbose, key, "isRoutable:%0d, (%0d && %0d && %0d) && %0d\n",
        c.isRoutable, c.routeA, (state->TX_TAG == HLP_MODEL_NORMAL_TAGGING),
        (state->PARSER_INFO.otr_l3_len > 0), !(c.isMirror));

    UpdateMacAddrIPP(key, model, &r, &c, &chunkedSeg);

    /* Step 3 in EAS: Construct VLAN Tags (and copy to output) */
    UpdateVlanIPP(key, model, &r, &c, &chunkedSeg, descMaster);

    /*******************************************************
     * L3 Modifications
     *******************************************************/
    // DSCP transformation needs priority profile, which is extracted in
    // vlanLookup
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  L3 MODIFICATIONS \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");
    if (!c.isMirror) {
        DSCPTransformation(key, model, &r, &c, &chunkedSeg, descMaster);
        ParseTtlCtrl(key, model, &c, &chunkedSeg, descMaster);

        rval = UpdateL3IPP(key, model, &r, &c, &chunkedSeg, descMaster);
        if (rval == -1) goto DONE;
    }

    /*******************************************************
     * Tunnel decap operations
     *******************************************************/
    if (state->DECAP | c.decap | (descMaster->otr1_cmd.tunnel_mode == TM_DECAP))
    {
        PRINT2(modDisplayVerbose, key, "-- Tunnel decap: Tunnel Mode = TM_DECAP\n");
        for (int i = 0; i < 6; i++)
            chunkedSeg.otr_dmac[i] = chunkedSeg.inr_dmac[i];
        for (int i = 0; i < 6; i++)
            chunkedSeg.otr_smac[i] = chunkedSeg.inr_smac[i];

        chunkedSeg.n_otr_tag = chunkedSeg.n_inr_tag;
        chunkedSeg.n_inr_tag = 0;
        for (int i = 0; i < (4*chunkedSeg.n_otr_tag); i++)
            chunkedSeg.otr_tags[i] = chunkedSeg.inr_tags[i];

        for (int i = 0; i < 2; i++)
            chunkedSeg.otr_et[i] = chunkedSeg.inr_et[i];

        chunkedSeg.otr_ip_size = chunkedSeg.inr_ip_size;
        chunkedSeg.otr_l3_v6 = chunkedSeg.inr_l3_v6;
        chunkedSeg.inr_ip_size = 0;
        for (int i = 0; i < (4*chunkedSeg.otr_ip_size); i++)
            chunkedSeg.otr_ip[i] = chunkedSeg.inr_ip[i];

        chunkedSeg.otr_udp_v = chunkedSeg.inr_udp_v;
        chunkedSeg.inr_udp_v = 0;
        chunkedSeg.otr_tcp_v = chunkedSeg.inr_tcp_v;
        chunkedSeg.inr_tcp_v = 0;
        chunkedSeg.tun_size_in_l4_chunk = 0;
        // TODO comment out should be ok
        //if (chunkedSeg.otr_tcp_v) chunkedSeg.otr_l4_size = 3;
        for (int i = 0; i < 18; i++)
            chunkedSeg.otr_l4[i] = chunkedSeg.inr_l4[i];

        chunkedSeg.tun_opt_size = 0;
    }

    /*******************************************************
     * Tunnel encap operations
     *******************************************************/
    if (state->ENCAP | c.encap |
       (descMaster->otr1_cmd.tunnel_mode == TM_ENCAP_GENEVE        )|
       (descMaster->otr1_cmd.tunnel_mode == TM_ENCAP_GRE           )|
       (descMaster->otr1_cmd.tunnel_mode == TM_ENCAP_VXLAN         )|
       (descMaster->otr1_cmd.tunnel_mode == TM_ENCAP_VXLAN_GPE     )|
       (descMaster->otr1_cmd.tunnel_mode == TM_ENCAP_VXLAN_GPE_NSH ))
    {
        PRINT2(modDisplayVerbose, key, "-- Tunnel encap: Tunnel Mode = TM_ENCAP\n");
        for (int i = 0; i < 6; i++)
            chunkedSeg.inr_dmac[i] = chunkedSeg.otr_dmac[i];
        for (int i = 0; i < 6; i++)
            chunkedSeg.inr_smac[i] = chunkedSeg.otr_smac[i];

        chunkedSeg.n_inr_tag = chunkedSeg.n_otr_tag;
        chunkedSeg.n_otr_tag = 0;
        for (int i = 0; i < (4*chunkedSeg.n_inr_tag); i++)
            chunkedSeg.inr_tags[i] = chunkedSeg.otr_tags[i];

        for (int i = 0; i < 2; i++)
            chunkedSeg.inr_et[i] = chunkedSeg.otr_et[i];

        chunkedSeg.inr_ip_size = chunkedSeg.otr_ip_size;
        chunkedSeg.inr_l3_v6 = chunkedSeg.otr_l3_v6;
        chunkedSeg.otr_ip_size = 0;
        for (int i = 0; i < (4*chunkedSeg.inr_ip_size); i++)
            chunkedSeg.inr_ip[i] = chunkedSeg.otr_ip[i];

        chunkedSeg.inr_udp_v = chunkedSeg.otr_udp_v;
        chunkedSeg.otr_udp_v = 0;
        chunkedSeg.inr_tcp_v = chunkedSeg.otr_tcp_v;
        chunkedSeg.otr_tcp_v = 0;
        chunkedSeg.tun_size_in_l4_chunk = 0;
        for (int i = 0; i < 18; i++)
            chunkedSeg.inr_l4[i] = chunkedSeg.otr_l4[i];

        chunkedSeg.tun_opt_size = 0;

        /* Need a new payload start and payload size to move old
         * tunnel options + inner into payload. */
        chunkedSeg.payload_start = (chunkedSeg.payload_start -
            (chunkedSeg.tun_opt_size*4) -
            (chunkedSeg.inr_l2_v ? 14 : 0) -
            (chunkedSeg.n_inr_tag*4) -
            (chunkedSeg.n_inr_mpls*4) -
            (chunkedSeg.inr_ip_size*4) -
            (chunkedSeg.inr_udp_v ? 8 : 0) -
            (chunkedSeg.inr_tcp_v ? 18 : 0));
        chunkedSeg.payload_size = (chunkedSeg.payload_size +
            (chunkedSeg.tun_opt_size*4) +
            (chunkedSeg.inr_l2_v ? 14 : 0) +
            (chunkedSeg.n_inr_tag*4) +
            (chunkedSeg.n_inr_mpls*4) +
            (chunkedSeg.inr_ip_size*4) +
            (chunkedSeg.inr_udp_v ? 8 : 0) +
            (chunkedSeg.inr_tcp_v ? 18 : 0));
        PRINT2(modDisplayVerbose, key, "Encap: ChunkedSeg New Payload: start=%0d, "
            "size=%0d\n", chunkedSeg.payload_start, chunkedSeg.payload_size);
    }

    /*******************************************************
     * Set fields by Descriptor requests
     *******************************************************/
    /* BASE CMD */
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  DESC MODIFICATIONS \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    UpdateDglortDesc(key, &c, &chunkedSeg, descMaster);
    UpdateMacAddrDesc(key, model, &c, &chunkedSeg, descMaster);

    /* OTR1 CMD */
    UpdateL3Desc(key, model, &c, &chunkedSeg, descMaster);
    UpdateL4Desc(key, model, &c, &chunkedSeg, descMaster);

    /* MPLS CMD */
    /* OTR_TAG_MODE & OTR_TAG_SIZE */
    UpdateVlanDesc(key, model, &c, &chunkedSeg, descMaster);

    /* INR CMD */
    UpdateInrVni(key, &chunkedSeg, descMaster);
    UpdateInrMacAddr(key, model, &c, &chunkedSeg, descMaster);
    UpdateInrL3(key, model, &c, &chunkedSeg, descMaster);
    UpdateInrDs();
    UpdateInrTtl();
    UpdateInrL4(key, model, &c, &chunkedSeg, descMaster);
    /* INR_TAG_MODE & INR_TAG_SIZE */
    UpdateInrL2Tags(key, model, &c, &chunkedSeg, descMaster);

    /*******************************************************
     * L4 Checksum
     *******************************************************/
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  L4 Checksum \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");
    UpdateL4Csum(key, model, &c, &chunkedSeg);

    /*******************************************************
     * MPLS pop/push and custom L2 tag handling
     *******************************************************/
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*  MPLS MODIFICATIONS \n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    // TODO, ttl dec override ipp dec
    // ttl src here cause we need to use mpls before pop/push what about dscp?
    TtlSrcDesc(key, model, &c, &chunkedSeg, descMaster);

    PRINT2(modDisplayVerbose, key, "n_otr_mpls=%0d, rx_mpls=0x", chunkedSeg.n_otr_mpls);
    for (int i = 0; i < chunkedSeg.n_otr_mpls; i++)
    {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
            chunkedSeg.otr_mpls[i*4], chunkedSeg.otr_mpls[(i*4)+1],
            chunkedSeg.otr_mpls[(i*4)+2], chunkedSeg.otr_mpls[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");

    /* Pop old MPLS labels (MPLS CMD) */
    PRINT2(modDisplayVerbose, key, "mpls_pop count = %0d mplsData.n_otr_g = %0d\n", c.mplsData.mplsPopCount, c.mplsData.n_otr_g);
    PopMPLSG(key, model, &c, &chunkedSeg);
    PopMPLSEli(key, model, &c, &chunkedSeg, descMaster);
    PopMPLSAl(key, model, &c, &chunkedSeg, descMaster);

    /* Push new MPLS labels from DESC (MPLS CMD) */
    /* Check if pushing onto an empty stack for etype changes */
    StackET_MPLS(key, model, &chunkedSeg, descMaster);
    PushMPLSAl(key, model, &c, &chunkedSeg, descMaster);
    PushMPLSEli(key, model, &c, &chunkedSeg, descMaster);
    PushMPLSG(key, model, &c, &chunkedSeg, descMaster);
    /* Popped all MPLS -> IPv4/6 EtherType */
    RestoreET_MPLS(key, model, &c, &chunkedSeg, descMaster);


    PRINT2(modDisplayVerbose, key, "n_otr_mpls=%0d, tx_mpls=0x", chunkedSeg.n_otr_mpls);
    for (int i = 0; i < chunkedSeg.n_otr_mpls; i++)
    {
        PRINTF2(modDisplayVerbose, "%02x%02x%02x%02x ",
            chunkedSeg.otr_mpls[i*4], chunkedSeg.otr_mpls[(i*4)+1],
            chunkedSeg.otr_mpls[(i*4)+2], chunkedSeg.otr_mpls[(i*4)+3]);
    }
    PRINTF2(modDisplayVerbose, "\n");

    /* DS/TTL update and write (QOS CMD) */
    // TODO, ECN should happen here cause it might change the internalDS and
    // then translated into exp
    UpdateECN(key, model, &r, &c, &chunkedSeg, descMaster);
    UpdateDSCPDesc(key, model, &r, &c, &chunkedSeg, descMaster);
    UpdateTTLDesc(key, model, &r, &c, &chunkedSeg, descMaster);

    UpdateL3LenCsum(key, model, &r, &c, &chunkedSeg, descMaster);

COPY_REST_OF_FRAME:
    PRINT(modDisplayVerbose, key, "/*********************\n");
    PRINT(modDisplayVerbose, key, "*   COPY REST OF FRAME\n");
    PRINT(modDisplayVerbose, key, "**********************/ \n");

    ReCalcIPcsum(key, model, &c, &chunkedSeg, packet);
DONE:
    PackPacket(packet, state->NO_MODIFY, state->RX_LENGTH, state->RX_DATA, &chunkedSeg, &c, &state->TX_LENGTH, key);

    // if minFrameSize, updatePktmeta use min size
    MiscOps(key, model, &r, &c, &chunkedSeg, packet);
    // FIXME
    // MD.len uses tail_csum_len, which is the len before trunc
    // however if trunc happens, stats collection collects the len after trunc
    //UpdatePktMeta(key, model, &c, c.refcnt_tx_len, &modMirrorProfileTable2);
    UpdatePktMeta(key, model, &c, &modMirrorProfileTable2);
    // TODO
    if (state->PM_ERR_NONSOP > 1 && !c.mirrorTrunc && !state->TX_DROP) {
        state->TX_STATS_LENGTH = c.egressSeg0Bytes + ((state->PM_ERR_NONSOP-1)*256) + state->TX_STATS_LAST_LEN;
    }
    else if (state->PM_ERR_NONSOP > 0 && !c.mirrorTrunc && !state->TX_DROP) {
        state->TX_STATS_LENGTH = c.egressSeg0Bytes + state->TX_STATS_LAST_LEN;
    }
    else {
        state->TX_STATS_LENGTH = c.refcnt_tx_len;
    }

    state->SEG_DROP = state->TX_DROP;

	PRINT2(modDisplayVerbose, key, "egressSeg0Bytes = %0d (includes egress FTAG)\n", c.egressSeg0Bytes);
	CopyDataForDV(model, packet, c.dvStatus, c.egressSeg0Bytes, c.refcntSeg0Bytes, c.bytesAdded, c.mirrorTrunc, c.numVlans, state->TX_REASONCODE, c.ecn_tx_drop, c.timeout_tx_drop, c.non_cm_tx_drop, c.cancel_drop_on_marker, c.cancelled_tx_disp, c.ecn_mark, c.intr_occured);

    //TODO if(!c.skipStats)
        hlpModelStatsTx(model);
    if(!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpModify);

    fmFree(key);

    return status;
}   /* end hlpModelModify */


 /*****************************************************************************/
 /** hlpModelStatsTx
  * \ingroup intModel
  *
  * \desc            Increments the TX statistics counters (Groups 7-8).
  *
  * \param[in]       model points to the switch model state.
  *
  * \return          FM_OK if successful.
  * \return          Other ''Status Codes'' as appropriate in case of
  *                  failure.
  *
  *****************************************************************************/
fm_status hlpModelStatsTx(hlp_model *model)
{

    hlp_modelState *state  = &model->packetState;
    fm_text                 key = fmAlloc(20);
    fm_status           status = FM_OK;

    sprintf(key, "%0x_%0d_%0d", state->ADDR, state->TX_PORT,
        state->MOD_IP_MCAST_IDX);
    if (!model->allowStateChange)
    {
        /*  The HLP white model not allowed to change the register cache */
        fmFree(key);
        return FM_OK;
    }

    status = HandleGroup7(key, model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

    //status = HandleGroup8(key, model);
    //FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);
	PRINT2(modDisplayVerbose, key, "reasonCode = 0x%0x\n", state->TX_REASONCODE);// TODO might be 0 or 0x15 both means no error

ABORT:
    fmFree(key);
    return status;
}


