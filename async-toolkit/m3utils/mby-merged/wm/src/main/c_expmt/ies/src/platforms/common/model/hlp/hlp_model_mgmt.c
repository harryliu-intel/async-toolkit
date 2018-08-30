/* vim:et:sw=4:ts=4:tw=80:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_model_mgmt.c
 * Creation Date:   December 3, 2012
 * Description:     HLP white model (management).
 *
 * INTEL CONFIDENTIAL
 * Copyright 2009 - 2013 Intel Corporation. All Rights Reserved. 
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

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define WITHIN_UNIT_RANGE(addr, unit)                                         \
    ( ( (addr) >= (HLP_ ## unit ## _BASE) ) &&                             \
      ( (addr) < ( (HLP_ ## unit ## _BASE) + (HLP_ ## unit ## _SIZE)) ) )

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** hlpModelInitializeInternal
 * \ingroup model10k
 *
 * \desc            Handles initialization of all white model stages that need
 *                  initializing.
 *
 * \param[in]       model points to the switch model state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelInitializeInternal(hlp_model *model)
{

    int i;

    /* Workaround to allow the API to change MSEC_ENABLE bit */
    for (i = 0; i < 32; ++i) {
        FM_ARRAY_SET_BIT(
                FM_MODEL_GET_REG_PTR(model, HLP_MAC_STATUS(i, 0)),
                HLP_MAC_STATUS, MSEC_EMPTY, 1);
    }

    return FM_OK;

} /* end hlpModelInitializeInternal */

/*****************************************************************************/
/** hlpModelWriteCSRInternal
 * \ingroup model10k
 *
 * \desc            Handles parsing of register write operations to trap
 *                  white model cached state.
 *
 * \note            The model lock must be held by the caller upon entry.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the register address.
 *
 * \param[in]       value is the value being written.
 *
 * \param[in]       oldValue is the value before the new value was written.
 *
 * \param[in]       init is a boolean indicating whether this write operation
 *                  is trying to initialize the register related white model
 *                  cached state.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelWriteCSRInternal(hlp_model *model,
                                   fm_uint32 addr,
                                   fm_uint32 value,
                                   fm_uint32 oldValue,
                                   fm_bool   init)
{
    if (WITHIN_UNIT_RANGE(addr, MAC))
    {
        return hlpModelMacWriteCSR(model, addr, value, oldValue, init);
    }
    else if (WITHIN_UNIT_RANGE(addr, MSEC))
    {
        return hlpModelMacsecWriteCSR(model, addr, value, oldValue, init);
    }
    else if (WITHIN_UNIT_RANGE(addr, FFU_GROUP))
    {
        return hlpModelFfuClassifierWriteCSR(model, addr, value, init);
    }
    else if (WITHIN_UNIT_RANGE(addr, L2LOOKUP))
    {
//if (addr == HLP_ENTRY_COUNT_IP(0)) FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "aaverchedebug: hlpModelWriteCSRInternal for HLP_ENTRY_COUNT_IP: addr=0x%x oldValue=0x%x value=0x%x.\n", addr, oldValue, value);
        return hlpModelL2LookupWriteCSR(model, addr, value, init);
    }
    else if (WITHIN_UNIT_RANGE(addr, FWD_MISC))
    {
        return hlpModelFwdMiscWriteCSR(model, addr, value, oldValue, init);
    }
    else if ( WITHIN_UNIT_RANGE(addr, CM_APPLY) ||
              WITHIN_UNIT_RANGE(addr, CM_USAGE) )
    {
        return hlpModelCmWriteCSR(model, addr, value);
    }
    return FM_OK;
} /* end hlpModelWriteCSRInternal */

/*****************************************************************************/
/** hlpModelReadCSRInternal
 * \ingroup model10k
 *
 * \desc            Handles parsing of register read operations to trap
 *                  white model cached state.
 *
 * \note            The model lock must be held by the caller upon entry.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the register address.
 *
 * \param[out]      value is the value being read.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status hlpModelReadCSRInternal(hlp_model *model,
                                  fm_uint32 addr,
                                  fm_uint32 value)
{
    if (WITHIN_UNIT_RANGE(addr, L2LOOKUP))
    {
        return hlpModelL2LookupReadCSR(model, addr, value);
    }
/*    FM_NOT_USED(model); */
/*    FM_NOT_USED(addr); */
/*    FM_NOT_USED(value); */
    return FM_OK;
} /* end hlpModelReadCSRInternal */

/*****************************************************************************/
/** hlpModelReadUpdateCSRInternal
 * \ingroup model10k
 *
 * \desc            Handle register read and update operations
 *
 * \note            The model lock must be held by the caller upon entry.
 *
 * \param[in]       model points to the switch model state.
 *
 * \param[in]       addr is the register address.
 *
 * \param[out]      value is the value being read.
 *
 * \return          FM_OK if successful.
  *
 *****************************************************************************/
fm_status hlpModelReadUpdateCSRInternal(hlp_model *model,
                                        fm_uint32 addr,
                                        fm_uint32 value)
{
    if (WITHIN_UNIT_RANGE(addr, L2LOOKUP))
    {
        return hlpModelL2LookupReadUpdateCSR(model, addr, value);
    }
    return FM_OK;
} /* end hlpModelReadUpdateCSRInternal */



