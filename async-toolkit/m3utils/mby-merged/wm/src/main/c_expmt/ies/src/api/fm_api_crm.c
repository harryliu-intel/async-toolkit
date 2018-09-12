/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_crm.c 
 * Creation Date:   June 3, 2010 
 * Description:     Application exposed functions for managing low-level
 *                  CRM resources.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

#include <fm_sdk_int.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmAllocateCrm
 * \ingroup lowlevCrm
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Allocate a counter rate monitor for exclusive use by the
 *                  application. It is up to the application to ensure that 
 *                  such CRMs do not modify any core API functionality.  
 *                  See the datasheet for information on programming
 *                  the CRM hardware registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       name is the name of the CRM, which is used for 
 *                  diagnostic purposes only.
 *
 * \param[out]      crm points to caller-allocated storage where this
 *                  function should place the hardware CRM number used as
 *                  an index into the CRM hardware registers.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if crm is NULL.
 * \return          FM_ERR_CRM_UNAVAILABLE if no CRMs are available.
 *****************************************************************************/
fm_status fmAllocateCrm(fm_int sw, 
                        fm_text name, 
                        fm_int *crm) 
{
    fm_status   err = FM_OK;
    fm_switch * switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_CRM, "sw=%d, crm=%p\n", sw, (void *) crm);

    if (!crm || !name)
    {
        FM_LOG_EXIT(FM_LOG_CAT_CRM, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, 
                       switchPtr->AllocateCrm, 
                       sw, 
                       name, 
                       crm);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_CRM, err);

} /* end fmAllocateCrm */




/*****************************************************************************/
/** fmFreeCrm
 * \ingroup lowlevCrm
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Frees a counter rate monitor allocated by the application
 *                  with a previous call to ''fmAllocateCrm''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       crm is the CRM number returned by a previous call 
 *                  to ''fmAllocateCrm''. 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_CRM if crm is invalid.
 *
 *****************************************************************************/
fm_status fmFreeCrm(fm_int sw, fm_int crm)
{
    fm_status  err = FM_OK;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_CRM, "sw=%d, crm=%d\n", sw, crm);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, switchPtr->FreeCrm, sw, crm);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_CRM, err);

} /* end fmFreeCrm */

