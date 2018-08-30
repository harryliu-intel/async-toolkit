/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_stats.c
 * Creation Date:   July 11, 2012
 * Description:     RX_STATS stage of HLP white model
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

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define GET_BANK_INDEX(state, status, index)                                   \
    if ( ( (state)->RX_PORT < 0 ) || ( (state)->RX_PORT > HLP_MAX_FABRIC_LOG_PORT ) ) \
    {                                                                          \
        *(status) = FM_FAIL;                                                   \
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, *(status));                   \
    }                                                                          \
    *(index) = (fm_uint16) ( ((state)->RX_PORT << 4) & 0x03F0 );

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static inline fm_status IncrementRxStatsBank(hlp_model *model,
                                             fm_uint32     addr,
                                             fm_uint64 *   cnt,
                                             fm_uint64     increment);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

fm_status UpdateRxStatsBank(hlp_model   *model,
                            fm_uint32   bank,
                            fm_uint16   index,                  
                            fm_uint64   len)
{
    fm_status               status = FM_OK;
    fm_uint64               delta1;
    fm_uint64               delta2;
    const fm_uint64         one = FM_LITERAL_U64(1);
    fm_uint64               frameCnt;
    fm_uint64               byteCnt;
    fm_uint32               *regPtr;

    /* Update Frame count */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_RX_STATS_BANK_FRAME(bank, index, 0)); 
    frameCnt = FM_ARRAY_GET_FIELD64(regPtr, HLP_RX_STATS_BANK_FRAME, FRAME_COUNTER);
    
    /* Wrap around to zero portably. */
    delta1 = FM_LITERAL_U64(0xFFFFFFFFFFFF) - frameCnt;;
    if (one > delta1)
    {
        /* Ensure that 0xFFFFFFFFFFFF + 1 wraps around to zero. */
        frameCnt = one - ( FM_LITERAL_U64(1) + delta1 );
    }
    else
    {
        frameCnt += one;
    }
    FM_ARRAY_SET_FIELD64(regPtr, HLP_RX_STATS_BANK_FRAME, FRAME_COUNTER, frameCnt);

    /* Update Byte count */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_RX_STATS_BANK_BYTE(bank, index, 0)); 
    byteCnt = FM_ARRAY_GET_FIELD64(regPtr, HLP_RX_STATS_BANK_BYTE, BYTE_COUNTER);
    

    /* Wrap around to zero portably. */
    delta2 = FM_LITERAL_U64(0xFFFFFFFFFFFFFF) - byteCnt;
    if (len > delta2)
    {
        /* Ensure that 0xFFFFFFFFFFFFFF + 1 wraps around to zero. */
        byteCnt = len - ( FM_LITERAL_U64(1) + delta2 );
    }
    else
    {
        byteCnt += len;
    }
    FM_ARRAY_SET_FIELD64(regPtr, HLP_RX_STATS_BANK_BYTE, BYTE_COUNTER, byteCnt);

ABORT:
   FM_LOG_EXIT(FM_LOG_CAT_MODEL_STATS, status);

}

fm_status UpdateVlanStats(hlp_model   *model,
                          fm_uint16   index,                  
                          fm_uint64   len)
{
    fm_status               status = FM_OK;
    fm_uint64               delta1;
    fm_uint64               delta2;
    const fm_uint64         one = FM_LITERAL_U64(1);
    fm_uint64               frameCnt;
    fm_uint64               byteCnt;
    fm_uint32               *regPtr;

    /* Update Frame count */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_RX_STATS_VLAN_FRAME(index, 0)); 
    frameCnt = FM_ARRAY_GET_FIELD64(regPtr, HLP_RX_STATS_VLAN_FRAME, FRAME_COUNTER);
    
    /* Wrap around to zero portably. */
    delta1 = FM_LITERAL_U64(0xFFFFFFFFF) - frameCnt;;
    if (one > delta1)
    {
        /* Ensure that 0xFFFFFFFFF + 1 wraps around to zero. */
        frameCnt = one - ( FM_LITERAL_U64(1) + delta1 );
    }
    else
    {
        frameCnt += one;
    }
    FM_ARRAY_SET_FIELD64(regPtr, HLP_RX_STATS_VLAN_FRAME, FRAME_COUNTER, frameCnt);

    /* Update Byte count */
    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_RX_STATS_VLAN_BYTE(index, 0)); 
    byteCnt = FM_ARRAY_GET_FIELD64(regPtr, HLP_RX_STATS_VLAN_BYTE, BYTE_COUNTER);
    

    /* Wrap around to zero portably. */
    delta2 = FM_LITERAL_U64(0xFFFFFFFFFFF) - byteCnt;
    if (len > delta2)
    {
        /* Ensure that 0xFFFFFFFFFFF + 1 wraps around to zero. */
        byteCnt = len - ( FM_LITERAL_U64(1) + delta2 );
    }
    else
    {
        byteCnt += len;
    }
    FM_ARRAY_SET_FIELD64(regPtr, HLP_RX_STATS_VLAN_BYTE, BYTE_COUNTER, byteCnt);

ABORT:
   FM_LOG_EXIT(FM_LOG_CAT_MODEL_STATS, status);

}

/*****************************************************************************/
/** HandleRxBank1
 * \ingroup intModel
 *
 * \desc            Handles RxBank 0 statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleRxBank0(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status                       status = FM_FAIL;
    fm_uint64                       len;
    fm_uint32                       bank = 0;
    fm_uint16                       index;
    fm_bool                         isIPv4;
    fm_bool                         isIPv6;
    fm_bool                         isL2Bcast;
    fm_bool                         isL2Mcast;
    fm_bool                         isL2Ucast;

    GET_BANK_INDEX(state, &status, &index);

    len = ((fm_uint64) state->RX_LENGTH);

    isIPv4 = state->IS_IPV4;
    isIPv6 = state->IS_IPV6;
    isL2Bcast = fmModelIsBroadcastMacAddress(state->L2_DMAC);
    isL2Mcast = fmModelIsMulticastMacAddress(state->L2_DMAC);
    isL2Ucast = fmModelIsUnicastMacAddress(state->L2_DMAC);

    /* IPv4 packets */
    if ( isL2Bcast && isIPv4 )
    {
        index += STAT_RxBcstPktsIPv4;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    else if ( isL2Mcast && isIPv4 )
    {
        index += STAT_RxMcstPktsIPv4;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    else if ( isL2Ucast && isIPv4 )
    {
        index += STAT_RxUcstPktsIPv4;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    /* IPv6 packets */
    else if ( isL2Bcast && isIPv6 )
    {
        index += STAT_RxBcstPktsIPv6;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    else if ( isL2Mcast && isIPv6 )
    {
        index += STAT_RxMcstPktsIPv6;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    else if ( isL2Ucast && isIPv6 )
    {
        index += STAT_RxUcstPktsIPv6;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    /* Non-IP packets */
    else if (isL2Bcast)
    {
        index += STAT_RxBcstPktsNonIP;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    else if (isL2Mcast)
    {
        index += STAT_RxMcstPktsNonIP;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
    else if (isL2Ucast)
    {
        index += STAT_RxUcstPktsNonIP;
        status = UpdateRxStatsBank(model, bank, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }

ABORT:
    return status;

}   /* end HandleRxBank1 */


/*****************************************************************************/
/** HandleRxBank1
 * \ingroup intModel
 *
 * \desc            Handles RxBank 1 statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleRxBank1(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status                   status = FM_FAIL;
    fm_uint64                   len;
    fm_uint32                   bank = 1;
    fm_uint16                   index;
    fm_byte                     priority;

    GET_BANK_INDEX(state, &status, &index);

    /* Bank parameters */
    len = ((fm_uint64) state->RX_LENGTH);

    /* Group parameters */
    index += ( state->TC  & 0x7);
    
    status = UpdateRxStatsBank(model, bank, index, len);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);

ABORT:
    return status;

}   /* end HandleRxBank1 */



/*****************************************************************************/
/** HandleRxBank2
 * \ingroup intModel
 *
 * \desc            Handles RxBank 4 statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleRxBank2(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status                   status = FM_FAIL;
    fm_uint64                   len;
    fm_uint32                   bank = 2;
    fm_uint16                   index;
    fm_bool                     hit = FALSE;

    GET_BANK_INDEX(state, &status, &index);

    /* Bank parameters */
    len = ((fm_uint64) state->RX_LENGTH);


    switch (state->ACTION)
    {
        case HLP_MODEL_ACTION_NORMAL:
            hit = TRUE;
            index += STAT_FIDForwarded;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_FLOOD:
            hit = TRUE;
            index += STAT_FloodForwarded;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_SPECIAL:
            hit = TRUE;
            index += STAT_TargetedDeterministicForwarded;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_PARSE:
            hit = TRUE;
            index += STAT_ParseErrDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_PARITY:
            hit = TRUE;
            index += STAT_ParityErrorDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_TRAP:
            hit = TRUE;
            index += STAT_Trapped;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;    
        case HLP_MODEL_ACTION_DROP_CONTROL:
            hit = TRUE;
            index += STAT_CtrlDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_STP:
            hit = TRUE;
            index += STAT_STPDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;    
        case HLP_MODEL_ACTION_DROP_SV:
            hit = TRUE;
            index += STAT_SecurityViolations;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_MARKER_ERROR_DROPS:
            hit = TRUE;
            index += STAT_MarkerErrorDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_IV:
            hit = TRUE;
            index += STAT_VlanIngressDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_EV:
            hit = TRUE;
            index += STAT_VlanEgressDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CAM:
            hit = TRUE;
            index += STAT_GlortMissDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_FFU:
            hit = TRUE;
            index += STAT_FFUDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_TRIG:
            hit = TRUE;
            index += STAT_TriggerDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_L3_PYLD_LEN:
            hit = TRUE;
            index += STAT_L3PayloadLengthValidationDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;    
        default:
            /* count in unused top bank */
            hit = TRUE;
            //index = 0x03FF;
            //status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
    }
    
ABORT:
    return status;

}   /* end HandleRxBank2 */


/*****************************************************************************/
/** HandleRxBank3
 * \ingroup intModel
 *
 * \desc            Handles RxBank 5 statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleRxBank3(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status                   status = FM_FAIL;
    fm_uint64                   len;
    fm_uint32                   bank = 3;
    fm_uint16                   index;
    fm_bool                     hit = FALSE;

    GET_BANK_INDEX(state, &status, &index);
    
    /* Bank parameters */
    len = ((fm_uint64) state->RX_LENGTH);

    switch (state->ACTION)
    {
        case HLP_MODEL_ACTION_DROP_POLICER:
            hit = TRUE;
            index += STAT_PolicerDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_TTL:
            hit = TRUE;
            index += STAT_TTLDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CM_GLOBAL:
            hit = TRUE;
            index += STAT_CMGlobalDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CM_SMP0:
            hit = TRUE;
            index += STAT_SMP0Drops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CM_SMP1:
            hit = TRUE;
            index += STAT_SMP1Drops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CM_RX_HOG0:
            hit = TRUE;
            index += STAT_RXHog0Drops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
         case HLP_MODEL_ACTION_DROP_CM_RX_HOG1:
            hit = TRUE;
            index += STAT_RXHog1Drops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CM_TX_HOG0:
            hit = TRUE;
            index += STAT_TXHog0Drops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_CM_TX_HOG1:
            hit = TRUE;
            index += STAT_TXHog1Drops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_FRAME_ERR:
            hit = TRUE;
            index += STAT_FrameErrorDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_REDIRECT_TRIG:
            hit = TRUE;
            index += STAT_TriggerRedirects;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_DLF:
            hit = TRUE;
            index += STAT_FloodControlDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_GLORT_FORWARDED:
            hit = TRUE;
            index += STAT_GlortForwarded;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break; 
        case HLP_MODEL_ACTION_DROP_LOOPBACK:
            hit = TRUE;
            index += STAT_LoopbackSuppDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_BANK5_OTHER_DROPS:
            hit = TRUE;
            index += STAT_OtherDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
        case HLP_MODEL_ACTION_DROP_L4_CSUM:
            hit = TRUE;
            index += STAT_L4CheckSumValidationDrops;
            status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;     
        default:
            hit = TRUE;
            //index = 0x03FF;
            //status = UpdateRxStatsBank(model, bank, index, len);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
            break;
    }

ABORT:
    return FM_OK;

}   /* end HandleRxBank3 */


/*****************************************************************************/
/** HandleRxBankVlan
 * \ingroup intModel
 *
 * \desc            Handles VLAN statistics counters.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleRxBankVlan(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status                   status = FM_OK;
    fm_uint64                   len;
    fm_uint16                   index;
    fm_uint16                   Type;
    fm_bool                     hit = FALSE;
    fm_bool                     isDrop = TRUE;

    /* Bank parameters */
    len = ((fm_uint64) state->RX_LENGTH);

    Type = 0x0;
    
    if (state->ACTION == HLP_MODEL_ACTION_NORMAL ||
        state->ACTION == HLP_MODEL_ACTION_FLOOD ||
        state->ACTION == HLP_MODEL_ACTION_SPECIAL ||
        state->ACTION == HLP_MODEL_ACTION_TRAP ||
        state->ACTION == HLP_MODEL_ACTION_REDIRECT_TRIG ||
        state->ACTION == HLP_MODEL_ACTION_GLORT_FORWARDED)
    {
        isDrop = FALSE;
    }

    if(isDrop)
    {
        Type = 0x3;
    }    
    else if ( fmModelIsUnicastMacAddress(state->L2_DMAC) )
    {
        Type = 0x0;
    }
    else if (fmModelIsMulticastMacAddress(state->L2_DMAC ))
    {
        Type = 0x1;
    }
    else if (fmModelIsBroadcastMacAddress(state->L2_DMAC) )
    {
        Type = 0x2;
    }
    
    hit = TRUE;
    /* WM only fix for bug #33258, no increment should happen for idx == 0 */
    if (state->L2_IVLAN1_CNT_INDEX != 0)
    {
        index = ((state->L2_IVLAN1_CNT_INDEX << 2) | (Type & 0x3)) & 0x3FFF ; 
        status = UpdateVlanStats(model, index, len);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);
    }
ABORT:
    return status;

}   /* end HandleRxBankVlan */


/*****************************************************************************
 * NOTE:  Egress ACL counting is in Action Res for RCL.  
 * It is removed from here.  It was Group14 in Bali.
 *****************************************************************************/

 
/*****************************************************************************/
/** IncrementRxStatsBank
 * \ingroup intModel
 *
 * \desc            Increments the specified statistics counter by the specified
 *                  amount wrapping around to zero if necessary.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the statistics counter hardware address.
 *
 * \param[in,out]   cnt points to the current value of the statistics counter.
 *                  Upon return this function will replace it with the new
 *                  value.
 *
 * \param[in]       increment is the amount by which the counter is to be
 *                  incremented.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static inline fm_status IncrementRxStatsBank(hlp_model *model,
                                             fm_uint32     addr,
                                             fm_uint64 *   cnt,
                                             fm_uint64     increment)
{
    fm_uint64 delta;

    /* Wrap around to zero portably. */
    delta = FM_LITERAL_U64(0xFFFFFFFFFFFFFFFF) - *cnt;
    if (increment > delta)
    {
        /* Ensure that 0xFFFFFFFFFFFFFFFF + 1 wraps around to zero. */
        *cnt = increment - ( FM_LITERAL_U64(1) + delta );
    }
    else
    {
        *cnt += increment;
    }


    return hlpModelWriteCSRAbsolute64(model, addr, *cnt);

}   /* end IncrementRxStatsBank */


/*****************************************************************************/
/** HandleTail
 * \ingroup intModel
 *
 * \desc            Handles tail functions, including SAF
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
static fm_status HandleTail(hlp_model *model)
{
    hlp_modelState *state           = &model->packetState;
    fm_uint32           value[2];
    
    /* from SAF register */
    fm_uint64           enableSNF;
    fm_uint32           cutThruMode;
    fm_uint32           ignoreFrameError;
    
    /* compute local state */
    fm_uint64           dstMask;
    fm_int              isSaf;
    fm_int              numSegs;

    fm_status           status         = FM_OK;
    fm_uint32           pktLen;

    /* remove FCS bytes from array storage */
    pktLen = state->RX_LENGTH < 4 ? 0 : state->RX_LENGTH - 4;
    //First segment is 192 bytes
    if (pktLen <= 192)
    {
        numSegs = 1;
        pktLen = 0;
    }    
    else
    {    
        numSegs = 1;
        pktLen = pktLen - 192;
        numSegs += (pktLen + HLP_SEGMENT_LEN-1) / HLP_SEGMENT_LEN;
    }
    
    status = hlpModelReadCSRMult(model->sw, 
            HLP_SAF_MATRIX(state->RX_PORT, 0), 
            HLP_SAF_MATRIX_WIDTH,
            value);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, status);

    enableSNF   = FM_ARRAY_GET_FIELD64(value, HLP_SAF_MATRIX, ENABLE_SNF);
    cutThruMode = FM_ARRAY_GET_FIELD(value, HLP_SAF_MATRIX, CUT_THRU_MODE);
    ignoreFrameError = FM_ARRAY_GET_BIT(value, HLP_SAF_MATRIX, IGNORE_FRAME_ERROR);
    
    dstMask = state->FNMASK;
    if(state->MIRROR1_PROFILE_V)
        dstMask |= (FM_LITERAL_U64(1) << state->MIRROR1_PORT);
    if(state->MIRROR0_PROFILE_V)
        dstMask |= (FM_LITERAL_U64(1) << state->MIRROR0_PORT);
    dstMask &= ( 0xFFFFFFFFFFFF ); /* 48 bit port mask */
    FM_LOG_DEBUG( FM_LOG_CAT_MODEL_STATS,
            "dstMask with mirrors=%012llx\n", dstMask);

    /* enableSNF needs to be ANDed with DMASK including mirroring */
    enableSNF = (enableSNF & dstMask) & 0xFFFFFF;

    FM_LOG_DEBUG( FM_LOG_CAT_MODEL_STATS,
            "EnableSNF=0x%012llx CutThruMode=%d IgnoreError=%d\n",
            enableSNF, cutThruMode, ignoreFrameError);
    
    /* compute whether frame is saf */
    isSaf = 0;
    if (enableSNF != 0)
    {
        FM_LOG_DEBUG( FM_LOG_CAT_MODEL_STATS,
                "Detected SNF destination, mask=%012llx\n", (enableSNF & dstMask));
        isSaf = 1;
    }
    /* handle cutThruMode */
    else if (cutThruMode==0)
    {
        isSaf = (numSegs==1);
    }
    else if (cutThruMode==1)
    {
        /* if number of segments == 1, then effectively snf */
        isSaf = (numSegs==1);
    }
    else if (cutThruMode==2)
    {
        /* if number of segments <= 2, then effectively snf */
        isSaf = (numSegs<=2);
    }
    else /* end-of-frame snf */
    {
        isSaf = 1;
    }

    /* 
     * SAF_ERROR from Tail.
     *
     * This implementation uses the RX_FLAGS parsed by the MacRx.
     * Actual HLP Tail uses the TailInfo.ERR input which will have
     * possibly different error conditions. The current implementation
     * models the crc error handling.
     */
    state->SAF_ERROR = 0;
    if (state->SEG_META_ERR == 1 || state->SEG_META_ERR == 2)
    {
        FM_LOG_DEBUG( FM_LOG_CAT_MODEL_STATS,
                "SafMatrix TAIL handling, bad FCS, "
                "RX_PORT=%d segs=%d len(+fcs)=%d\n",
                state->RX_PORT, numSegs, state->RX_LENGTH);
        if(isSaf || (dstMask == 0))
        {
            state->SAF_ERROR = ~ignoreFrameError & 0x1;
            if(!enableSNF & (cutThruMode == 0) & (dstMask != 0))
                state->SAF_ERROR = 0;
        }    
        
        FM_LOG_DEBUG( FM_LOG_CAT_MODEL_STATS,
                "state->SAF_ERROR=%d\n", state->SAF_ERROR );

    } /* end if bad fcs detected */
    
    if(state->SAF_ERROR)
    {
        if(numSegs == 1 || dstMask == 0)
        {    
            state->ACTION = HLP_MODEL_ACTION_DROP_FRAME_ERR;
            
            state->FNMASK = 0;
            state->MIRROR1_PROFILE_V = 0;
            state->MIRROR0_PROFILE_V = 0;
        }
        else //multi-segment
        {
            state->TX_DROP = 1;
        }
    }
    /* Update Action code for CSUM and L3 Length errors */
    /* Applies to only single segment packets. Multi-segment packets are handled
     * by Modify */
    //actions that have lower precedence than l3_len_val and l4_csum_val
    if(state->RX_LENGTH <= 192)
    { 
        if (state->ACTION == HLP_MODEL_ACTION_NORMAL ||
            state->ACTION == HLP_MODEL_ACTION_FLOOD ||
            state->ACTION == HLP_MODEL_ACTION_GLORT_FORWARDED ||
            state->ACTION == HLP_MODEL_ACTION_TRAP ||
            state->ACTION == HLP_MODEL_ACTION_SPECIAL ||
            state->ACTION == HLP_MODEL_ACTION_REDIRECT_TRIG ||
            state->ACTION == HLP_MODEL_ACTION_DROP_CONTROL ||
            state->ACTION == HLP_MODEL_ACTION_DROP_IV ||
            state->ACTION == HLP_MODEL_ACTION_DROP_EV ||
            state->ACTION == HLP_MODEL_ACTION_DROP_STP ||
            state->ACTION == HLP_MODEL_ACTION_DROP_CAM ||
            state->ACTION == HLP_MODEL_ACTION_DROP_FFU ||
            state->ACTION == HLP_MODEL_ACTION_DROP_TRIG ||
            state->ACTION == HLP_MODEL_ACTION_DROP_TTL ||
            state->ACTION == HLP_MODEL_ACTION_DROP_DLF ||
            state->ACTION == HLP_MODEL_ACTION_BANK5_OTHER_DROPS ||
            state->ACTION == HLP_MODEL_ACTION_DROP_SV)
        {    
            if (state->PA_DROP && state->PA_L3LEN_ERR)
                state->ACTION = HLP_MODEL_ACTION_DROP_L3_PYLD_LEN;
            else if (state->PA_DROP && state->PA_CSUM_ERR)
                state->ACTION = HLP_MODEL_ACTION_DROP_L4_CSUM;
            FM_LOG_DEBUG( FM_LOG_CAT_MODEL_STATS,
                    "state->ACTION=%d\n", state->ACTION );
        }
        /* Drop single-segment packets with l4csum error /l3 length error */  
        if (state->ACTION == HLP_MODEL_ACTION_DROP_L3_PYLD_LEN ||
            state->ACTION == HLP_MODEL_ACTION_DROP_L4_CSUM)
        {
            state->FNMASK = 0;
            state->MIRROR1_PROFILE_V = 0;
            state->MIRROR0_PROFILE_V = 0;
        }
    } 
    else { //multi-segment packets
        if (state->PA_DROP && (state->PA_L3LEN_ERR || state->PA_CSUM_ERR))
        {
            state->SEG_META_ERR = 2;//framing error
        }
    }

    //clear parser_info for window parsing
    if(state->PARSER_INFO.window_parse_v)
        FM_CLEAR(state->PARSER_INFO);
ABORT:
    return status;

}   /* end HandleTail */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelStatsRx
 * \ingroup intModel
 *
 * \desc            Increments the RX statistics counters (Group 1 through 7).
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status hlpModelStatsRx(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status           err   = FM_OK;

    if (!model->allowStateChange)
    {
        /* The HLP white model is not allowed to change the register cache.
         */
        return FM_OK;
    }

    /* Tail handling */
    err = HandleTail(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, err);

    /* RX frame classification */
    err = HandleRxBank0(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, err);

    /* RX per-port TC counters */
    err = HandleRxBank1(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, err);
    
    /* RX forwarding action */
    if (state->ACTION & 0x10)
    {
        /* RX drop actions */
        err = HandleRxBank3(model);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, err);
    }
    else
    {
        err = HandleRxBank2(model);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, err);
    }

    /* RX VCNT */
    err = HandleRxBankVlan(model);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_MODEL_STATS, err);

ABORT:
    return err;

} /* end hlpModelStatsRx */

/*****************************************************************************
 * NOTE: hlpModelStatsTx is now in Modify, for Groups7-8. 
 * It is removed from here.  
 *****************************************************************************/


