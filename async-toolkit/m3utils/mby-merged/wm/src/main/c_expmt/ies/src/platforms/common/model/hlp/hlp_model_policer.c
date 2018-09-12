/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_policer.c
 * Creation Date:   November 7, 2013
 * Description:     POLICER stage of HLP white model
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

typedef enum
{
    HLP_POL_CMD_NOP,
    HLP_POL_CMD_CNT,
    HLP_POL_CMD_POL,
} hlpPolActionCmd; 

typedef struct _hlpPolicerAction
{
    hlpPolActionCmd     cmd;
    fm_uint16           index;
    fm_byte             bank;
    fm_bool             cntRed; 
    fm_bool             cntYellow; 
    fm_bool             cntGreen; 
} hlpPolicerAction;

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelPolicerActionExtract
 * \ingroup intModel
 *
 * \desc                Takes and maps the FFU_ACTION into a structure
 *
 * \param[in]           action, the FFU_ACTION value
 *
 * \param[in,out]       actionStruct, user allocated structure that will be
 *                      filled in by this function
 *
 * \return              FM_OK
 * \return              FM_ERR_INVALID_ARGUMENT if anything in FFU_ACTION is
 *                       unexpected
 *
 *****************************************************************************/
fm_status hlpModelPolicerActionExtract(fm_uint32         action, 
                                       hlpPolicerAction *actionStruct)
{
    FM_CLEAR(*actionStruct);


    if (action == 0)
    {
        actionStruct->cmd = HLP_POL_CMD_NOP;
        return FM_OK;
    } 

    actionStruct->index = action & 0x7FF;
    actionStruct->bank  = (action >> 16) & 0xF;

    if (((action >> 23) & 0x1) == 0)
    {
        /* Count action */

        actionStruct->cmd = HLP_POL_CMD_CNT;

        if (((action >> 20) & 0xF) != 0x4)
        {
            FM_LOG_PRINT("hlpModelPolicerActionExtract bits 23:20 are not 0x4"
                        " in count mode, saw 0x%x\n", ((action >> 20) & 0xF));
            return FM_ERR_INVALID_ARGUMENT;
        }

    }
    else
    {   
        /* Police action */

        actionStruct->cmd = HLP_POL_CMD_POL;
        actionStruct->cntRed    = (((action >> 20) & 0x1) == 1);
        actionStruct->cntYellow = (((action >> 21) & 0x1) == 1);
        actionStruct->cntGreen  = (((action >> 22) & 0x1) == 1);

        if (!((actionStruct->bank == 0) || (actionStruct->bank == 5)))
        {
            FM_LOG_PRINT("hlpModelPolicerActionExtract bank is not 0 or 5"
                        " in policer mode, saw 0x%x\n", actionStruct->bank);
            return FM_ERR_INVALID_ARGUMENT;
        }
    }

    if (((action >> 11) & 0x1F) != 0)
    {
        FM_LOG_PRINT("hlpModelPolicerActionExtract Reserve field is not 0\n");
        return FM_ERR_INVALID_ARGUMENT;
    }

    return FM_OK;

}   /* end hlpModelPolicerActionExtract */


/*****************************************************************************/
/** hlpModelPolicerIncrementCounter
 * \ingroup intModel
 *
 * \desc                Increments the specified byte/packet counters in
 *                      POL_STATE
 *
 * \param[in,out]       model, function will write the model's POL_STATE
 *                      counters
 *
 * \param[in]           policerAction, action from FFU indicating bank/index
 *                      of POL_STATE to count in
 *
 *****************************************************************************/
void hlpModelPolicerIncrementCounter(hlp_model *model,
                                     hlpPolicerAction *policerAction)
{
    hlp_modelState  *state = &model->packetState;
    fm_uint32       *regPtr;
    fm_uint64        packetCnt;
    fm_uint64        byteCnt;

    FM_LOG_PRINT("hlpModelPolicerIncrementCounter: BANK = %d, INDEX = %d\n\n",
                policerAction->bank, policerAction->index);

    regPtr    = FM_MODEL_GET_REG_PTR(model, 
                                     HLP_POL_CNTR_STATE(policerAction->bank,
                                                        policerAction->index,
                                                        0));
        
    byteCnt   = FM_ARRAY_GET_FIELD64(regPtr, HLP_POL_CNTR_STATE, DATA_CNTB);
    packetCnt = FM_ARRAY_GET_FIELD64(regPtr, HLP_POL_CNTR_STATE, DATA_CNTP);

    packetCnt++;
    byteCnt += state->RX_LENGTH;

    FM_ARRAY_SET_FIELD64(regPtr, HLP_POL_CNTR_STATE, DATA_CNTB, byteCnt);
    FM_ARRAY_SET_FIELD64(regPtr, HLP_POL_CNTR_STATE, DATA_CNTP, packetCnt);

}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

fm_status hlpModelPolicer(hlp_model *model)
{
    hlp_modelState *state = &model->packetState;
    fm_status           status = FM_OK;
    fm_uint64           policerCfg;
    fm_uint16           bankIdx;
    fm_uint16           lastPolIdx;
    fm_int              k;
    hlpPolicerAction    policerAction;

    for (k = 0; k < 4; k++)
    {
        status = hlpModelPolicerActionExtract(state->POLICER_ACTION[k], 
                                              &policerAction);

        if (status != FM_OK)
        {
            return status;
        }

        switch (policerAction.cmd)
        {
            case HLP_POL_CMD_NOP:
                break;
            case HLP_POL_CMD_POL:
                FM_LOG_PRINT("hlpModelPolicer command not implemented\n");
                break;

            case HLP_POL_CMD_CNT:
                hlpModelPolicerIncrementCounter(model, &policerAction);
                break;

            default:
                FM_LOG_PRINT("hlpModelPolicer unknown command\n");
                return FM_ERR_INVALID_ARGUMENT;
        }
    }

    return status;

}   /* end hlpModelPolicer */

