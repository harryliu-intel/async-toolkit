/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_trigger.c 
 * Creation Date:   October 24, 2008 
 * Description:     Application exposed functions for managing low-level
 *                  trigger resources.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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
/** fmAllocateTrigger
 * \ingroup lowlevTrig4k
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Allocate a trigger for exclusive use by the
 *                  application. It is up to the application to ensure that 
 *                  such triggers do not modify any core API functionality.  
 *                  See the datasheet for information on programming
 *                  the trigger hardware registers.
 *                                                                      \lb\lb
 *                  Instead of this function, you may use 
 *                  ''fmAllocateTriggerExt'', which allows you to specify a 
 *                  name for the trigger for diagnostic purposes.
 *                  
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      trigger points to caller-allocated storage where this
 *                  function should place the hardware trigger number used as
 *                  an index into the trigger hardware registers
 *
 * \param[out]      info points to a structure that gives hints to the 
 *                  allocator. 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_RATELIMITER_UNAVAILABLE if info specifies use of
 *                  a rate limiter, but there are no rate limiters available.
 *****************************************************************************/
fm_status fmAllocateTrigger(fm_int sw, 
                            fm_int *trigger, 
                            fm_triggerRequestInfo *info)
{
    fm_status   err = FM_OK;
    fm_switch * switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_TRIGGER,
                     "sw=%d, trigger=%p, info=%p\n",
                     sw, (void *) trigger, (void *) info);

    if (!trigger || !info)
    {
        FM_LOG_EXIT(FM_LOG_CAT_TRIGGER, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, 
                       switchPtr->AllocateTrigger, 
                       sw, 
                       "NA", 
                       trigger, 
                       info);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_TRIGGER, err);

} /* end fmAllocateTrigger */

/*****************************************************************************/
/** fmAllocateTriggerExt
 * \ingroup lowlevTrig4k
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Allocate a trigger for exclusive use by the
 *                  application. It is up to the application to ensure that 
 *                  such triggers do not modify any core API functionality.  
 *                  See the datasheet for information on programming
 *                  the trigger hardware registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       name is the name of the trigger, which is used for 
 *                  diagnostic purposes only and is displayed by
 *                  ''fmDbgDumpTriggers''.
 *
 * \param[out]      trigger points to caller-allocated storage where this
 *                  function should place the hardware trigger number used as
 *                  an index into the trigger hardware registers
 *
 * \param[out]      info points to a structure that gives hints to the 
 *                  allocator. 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_RATELIMITER_UNAVAILABLE if info specifies use of
 *                  a rate limiter, but there are no rate limiters available.
 *****************************************************************************/
fm_status fmAllocateTriggerExt(fm_int sw, 
                               fm_text name, 
                               fm_int *trigger, 
                               fm_triggerRequestInfo *info)
{
    fm_status   err = FM_OK;
    fm_switch * switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_TRIGGER,
                     "sw=%d, trigger=%p, info=%p\n",
                     sw, (void *) trigger, (void *) info);

    if (!trigger || !info || !name)
    {
        FM_LOG_EXIT(FM_LOG_CAT_TRIGGER, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, 
                       switchPtr->AllocateTrigger, 
                       sw, 
                       name, 
                       trigger, 
                       info);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_TRIGGER, err);

} /* end fmAllocateTriggerExt */

/*****************************************************************************/
/** fmFreeTrigger
 * \ingroup lowlevTrig4k
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Frees a trigger previously allocated by the application
 *                  with a call to ''fmAllocateTrigger''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       trigger is the trigger number returned by a previous call 
 *                  to ''fmAllocateTrigger''. 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_TRIG if trigger is invalid.
 *
 *****************************************************************************/
fm_status fmFreeTrigger(fm_int sw, fm_int trigger)
{
    fm_status   err = FM_OK;
    fm_switch * switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_TRIGGER,
                     "sw=%d, trigger=%d\n",
                     sw, trigger);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_API_CALL_FAMILY(err, switchPtr->FreeTrigger, sw, trigger);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_TRIGGER, err);

} /* end fmFreeTrigger */

