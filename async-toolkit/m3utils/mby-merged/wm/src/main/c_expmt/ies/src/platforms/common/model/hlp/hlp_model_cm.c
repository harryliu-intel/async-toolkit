/* vim:et:sw=4:ts=4:sw=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_cm.c
 * Creation Date:   June 22, 2012
 * Description:     CM stage of HLP white model
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

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_int cmDisplayVerbose = 0;

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

fm_status hlpModelCmWriteCSR(hlp_model *model,
                             fm_uint32 addr,
                             fm_uint32 value)
{

	fm_status status = FM_OK;
    fm_uint64 value64;
    fm_int    sw = model->sw;

    HLP_MODEL_LOG_ENTRY_CSR(FM_LOG_CAT_PLATFORM,
                                "model=%p addr=0x%x value=0x%x\n",
                                (void *) model,
                                addr,
                                value);
//	FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM,
//                 "hlpModelCmWriteCSR: addr=0x%x value=0x%x\n",
//                 addr, value);

    if (addr == HLP_CM_APPLY_TC_TO_SMP(1))
    {
        model->CM_SMP_MEMBERSHIP[0] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_0);
        model->CM_SMP_MEMBERSHIP[1] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_1);
        model->CM_SMP_MEMBERSHIP[2] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_2);
        model->CM_SMP_MEMBERSHIP[3] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_3);
        model->CM_SMP_MEMBERSHIP[4] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_4);
        model->CM_SMP_MEMBERSHIP[5] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_5);
        model->CM_SMP_MEMBERSHIP[6] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_6);
        model->CM_SMP_MEMBERSHIP[7] =
            FM_GET_BIT(value, HLP_CM_APPLY_TC_TO_SMP, SMP_7);
    }
/* removed: HLP_CM_APPLY_SWITCH_PRI_TO_TC register is no more:
    else if (addr == HLP_CM_APPLY_SWITCH_PRI_TO_TC(1))
    {
        status = hlpModelReadCSR64(sw, HLP_CM_APPLY_SWITCH_PRI_TO_TC(0), &value64);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status);

        model->SWITCH_PRI_TO_CLASS[0] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_0);
        model->SWITCH_PRI_TO_CLASS[1] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_1);
        model->SWITCH_PRI_TO_CLASS[2] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_2);
        model->SWITCH_PRI_TO_CLASS[3] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_3);
        model->SWITCH_PRI_TO_CLASS[4] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_4);
        model->SWITCH_PRI_TO_CLASS[5] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_5);
        model->SWITCH_PRI_TO_CLASS[6] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_6);
        model->SWITCH_PRI_TO_CLASS[7] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_7);
        model->SWITCH_PRI_TO_CLASS[8] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_8);
        model->SWITCH_PRI_TO_CLASS[9] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_9);
        model->SWITCH_PRI_TO_CLASS[10] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_10);
        model->SWITCH_PRI_TO_CLASS[11] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_11);
        model->SWITCH_PRI_TO_CLASS[12] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_12);
        model->SWITCH_PRI_TO_CLASS[13] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_13);
        model->SWITCH_PRI_TO_CLASS[14] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_14);
        model->SWITCH_PRI_TO_CLASS[15] =
            FM_GET_FIELD64(value64, HLP_CM_APPLY_SWITCH_PRI_TO_TC, tc_15);
    }
*/

ABORT:
    HLP_MODEL_LOG_EXIT_CSR(FM_LOG_CAT_PLATFORM, status);

} /* end hlpModelCmWriteCSR */



void hlpModelCm(hlp_model *model)
{

/*
	hlp_modelState *state = &model->packetState;

    state->TC = model->SWITCH_PRI_TO_CLASS[state->QOS_SWPRI];
    state->SMP_MEMBERSHIP = model->CM_SMP_MEMBERSHIP[state->TC];

    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpCm);
*/

    fm_uint32      *regPtr;
    hlp_modelState *state = &model->packetState;

    //if(testPlusArgs("HLP_CM_WM_PRINT_VERBOSE") >= 0)
    //    cmDisplayVerbose = testPlusArgs("HLP_CM_WM_PRINT_VERBOSE");
    //WM_DISPLAY(cmDisplayVerbose, FM_CAT_MODEL_CM, "cmDisplayVerbose=%0d\n", cmDisplayVerbose)

    state->TC = state->QOS_SWPRI;

    regPtr = FM_MODEL_GET_REG_PTR(model, HLP_CM_APPLY_TC_TO_SMP(0));
    model->CM_SMP_MEMBERSHIP[0] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_0);
    model->CM_SMP_MEMBERSHIP[1] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_1);
    model->CM_SMP_MEMBERSHIP[2] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_2);
    model->CM_SMP_MEMBERSHIP[3] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_3);
    model->CM_SMP_MEMBERSHIP[4] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_4);
    model->CM_SMP_MEMBERSHIP[5] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_5);
    model->CM_SMP_MEMBERSHIP[6] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_6);
    model->CM_SMP_MEMBERSHIP[7] = FM_ARRAY_GET_BIT(regPtr, HLP_CM_APPLY_TC_TO_SMP, SMP_7);
    state->SMP_MEMBERSHIP = model->CM_SMP_MEMBERSHIP[state->TC];

    if (!state->DISABLE_DBG_DUMP)
        HLP_MODEL_DBG_DUMP(model, hlpModelDbgDumpCm);

} /* end hlpModelCm */
