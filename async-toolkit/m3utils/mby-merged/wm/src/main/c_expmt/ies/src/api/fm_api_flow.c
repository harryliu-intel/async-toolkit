/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_flow.c
 * Creation Date:   July 20, 2010 
 * Description:     OpenFlow API interface.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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
/** fmCreateFlowTCAMTable
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Creates a flow table in the FFU TCAM. Calling this function
 *                  also automatically add a low priority catch-all flow that
 *                  redirects traffic to the CPU port if the tableIndex does
 *                  not support priority (see ''FM_FLOW_TABLE_WITH_PRIORITY'') 
 *                  and the ''FM_FLOW_TABLE_WITH_DEFAULT_ACTION'' flow
 *                  attribute is enabled.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is used to order this table with respect to other
 *                  TCAM tables. The lower the tableIndex, the higher the
 *                  precedence. tableIndex must be no larger than
 *                  ''FM_FLOW_MAX_TABLE_TYPE''.
 * 
 * \param[in]       condition is a bit mask of matching conditions used to
 *                  identify flows that may be added to this table. See 
 *                  ''Flow Condition Masks'' for the definitions of each bit 
 *                  in the mask. Currently this argument is ignored and all
 *                  available conditions are enabled by default.
 * 
 * \param[in]       maxEntries is the size of the flow table and must be
 *                  no larger than ''FM4000_MAX_RULE_PER_FLOW_TABLE''
 *                  (FM3000/FM4000 devices) or ''FM6000_MAX_RULE_PER_FLOW_TABLE''
 *                  (FM6000 devices).
 * 
 * \param[in]       maxAction is the maximum number of actions supported by
 *                  rules in this table (must be at least 1).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if the specified condition is not
 *                  supported for this table.
 * \return          FM_ERR_INVALID_ARGUMENT if an argument is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex already used.
 *
 *****************************************************************************/
fm_status fmCreateFlowTCAMTable(fm_int           sw, 
                                fm_int           tableIndex, 
                                fm_flowCondition condition,
                                fm_uint32        maxEntries,
                                fm_uint32        maxAction)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw=%d, tableIndex=%d, condition=%llx, maxEntries=%d, maxAction= %d\n", 
                     sw, 
                     tableIndex, 
                     (fm_uint64) condition,
                     maxEntries,
                     maxAction);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->CreateFlowTCAMTable,
                       sw,
                       tableIndex,
                       condition,
                       maxEntries,
                       maxAction);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmCreateFlowTCAMTable */



/*****************************************************************************/
/** fmDeleteFlowTCAMTable
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Deletes a flow table in the FFU TCAM.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the index to remove.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex already used.
 *
 *****************************************************************************/
fm_status fmDeleteFlowTCAMTable(fm_int sw, 
                                fm_int tableIndex)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw=%d, tableIndex=%d\n", 
                     sw, 
                     tableIndex);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->DeleteFlowTCAMTable,
                       sw,
                       tableIndex);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmDeleteFlowTCAMTable */



/*****************************************************************************/
/** fmAddFlow
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Add a flow entry to the specified table.
 * 
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which this flow should
 *                  be added.
 * 
 * \param[in]       priority is the priority for this flow within the table.
 *                  Flow Attribute ''FM_FLOW_TABLE_WITH_PRIORITY'' must be
 *                  set to activate this functionality on a tableIndex
 *                  basis.
 * 
 * \param[in]       precedence is the inter-table flow precedence. This is
 *                  currently not supported.
 * 
 * \param[in]       condition is a bit mask of matching conditions used to
 *                  identify the flow. See ''Flow Condition Masks'' for the
 *                  definitions of each bit in the mask.
 * 
 * \param[in]       condVal points to a ''fm_flowValue'' structure containing
 *                  the values to match against for the specified condition.
 * 
 * \param[in]       action is a bit mask of actions that will occur when a
 *                  packet is identified that matches the flow condition.
 *                  See ''Flow Action Masks'' for definitions of each bit in 
 *                  the mask.
 * 
 * \param[in]       param points to a ''fm_flowParam'' structure containing
 *                  values used by some actions.
 * 
 * \param[in]       flowState is the initial state of the flow (enabled or
 *                  not).
 * 
 * \param[out]      flowId points to caller-allocated storage where this
 *                  function will store the handle identifying this flow entry.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if tableIndex or action is not
 *                  supported.
 * \return          FM_ERR_INVALID_ARGUMENT if an argument is invalid.
 * \return          FM_ERR_TABLE_FULL if there is no more room in the table to
 *                  add this flow.
 * \return          FM_ERR_INVALID_ACL if tableIndex is not valid.
 * \return          FM_ERR_INVALID_ACL_PARAM if param is not valid.
 *
 *****************************************************************************/
fm_status fmAddFlow(fm_int           sw, 
                    fm_int           tableIndex,
                    fm_uint16        priority,
                    fm_uint32        precedence, 
                    fm_flowCondition condition,
                    fm_flowValue *   condVal,
                    fm_flowAction    action,
                    fm_flowParam *   param,
                    fm_flowState     flowState,
                    fm_int *         flowId)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, priority = %d, precedence = %d,"
                     "condition = %llx, condVal = %p, "
                     "action = %x, param = %p, flowState=%d\n",
                     sw,
                     tableIndex,
                     priority,
                     precedence,
                     condition,
                     (void *) condVal,
                     action,
                     (void *) param,
                     flowState);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->AddFlow,
                       sw,
                       tableIndex,
                       priority,
                       precedence,
                       condition,
                       condVal,
                       action,
                       param,
                       flowState,
                       flowId);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmAddFlow */



/*****************************************************************************/
/** fmModifyFlow
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Modify a flow entry in the specified flow table. The flow
 *                  entry is updated atomically without affecting traffic.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the flow belongs.
 * 
 * \param[in]       flowId is the flow instance to be modified.
 * 
 * \param[in]       priority is the priority for this flow within the table.
 *                  This argument is only effective if the 
 *                  ''FM_FLOW_TABLE_WITH_PRIORITY'' flow attribute is 
 *                  enabled on the specified tableIndex.
 * 
 * \param[in]       precedence is the inter-table flow precedence. This is
 *                  currently not supported.
 * 
 * \param[in]       condition is a bit mask of matching conditions used to
 *                  identify the flow. See ''Flow Condition Masks'' for the
 *                  definitions of each bit in the mask.
 * 
 * \param[in]       condVal points to a ''fm_flowValue'' structure containing
 *                  the values to match against for the specified condition.
 * 
 * \param[in]       action is a bit mask of actions that will occur when a
 *                  packet is identified that matches the flow condition.
 *                  See ''Flow Action Masks'' for definitions of each bit in 
 *                  the mask.
 * 
 * \param[in]       param points to a ''fm_flowParam'' structure containing
 *                  values used by some actions.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if flowId is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex is invalid.
 * \return          FM_ERR_INVALID_ACL_RULE if flowId is not valid.
 * \return          FM_ERR_INVALID_ACL_PARAM if param is not valid.
 *
 *****************************************************************************/
fm_status fmModifyFlow(fm_int           sw, 
                       fm_int           tableIndex,
                       fm_int           flowId,
                       fm_uint16        priority,
                       fm_uint32        precedence, 
                       fm_flowCondition condition,
                       fm_flowValue *   condVal,
                       fm_flowAction    action,
                       fm_flowParam *   param)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, flowId = %d, priority = %d,"
                     "precedence = %d, action = %d, param = %p\n",
                     sw,
                     tableIndex,
                     flowId,
                     priority,
                     precedence,
                     action,
                     (void *) param);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->ModifyFlow,
                       sw,
                       tableIndex,
                       flowId,
                       priority,
                       precedence,
                       condition,
                       condVal,
                       action,
                       param);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmModifyFlow */



/*****************************************************************************/
/** fmDeleteFlow
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Delete a flow entry from the specified table.
 * 
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the flow table instance from which to delete
 *                  the flow.
 * 
 * \param[in]       flowId is the flow instance to be deleted.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if an argument is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex is not valid.
 *
 *****************************************************************************/
fm_status fmDeleteFlow(fm_int sw, fm_int tableIndex, fm_int flowId)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, flowId = %d\n",
                     sw,
                     tableIndex,
                     flowId);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->DeleteFlow,
                       sw,
                       tableIndex,
                       flowId);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmDeleteFlow */



/*****************************************************************************/
/** fmSetFlowState
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Change the state of a flow (Enable or Standby).
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the flow belongs.
 * 
 * \param[in]       flowId is the flow instance.
 * 
 * \param[in]       flowState is the flow's new state (see ''fm_flowState'').
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if tableIndex identifies a table that
 *                  does not support enabling/disabling individual flows.
 * \return          FM_ERR_INVALID_ACL if tableIndex is not valid.
 * \return          FM_ERR_INVALID_ACL_RULE if flowId is not valid.
 *
 *****************************************************************************/
fm_status fmSetFlowState(fm_int       sw, 
                         fm_int       tableIndex, 
                         fm_int       flowId, 
                         fm_flowState flowState)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, flowId = %d, flowState = %d\n",
                     sw,
                     tableIndex,
                     flowId,
                     flowState);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->SetFlowState,
                       sw,
                       tableIndex,
                       flowId,
                       flowState);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmSetFlowState */



/*****************************************************************************/
/** fmGetFlowCount
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Retrieve counters for a flow.
 * 
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the flow belongs.
 * 
 * \param[in]       flowId is the flow instance.
 * 
 * \param[out]      counters points to caller-allocated storage where this
 *                  function will store the counter values.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if tableIndex does not identify a
 *                  a flow table that supports counters.
 * \return          FM_ERR_INVALID_ACL if tableIndex is invalid.
 * \return          FM_ERR_INVALID_ACL_RULE if flowId is invalid.
 *                  
 *****************************************************************************/
fm_status fmGetFlowCount(fm_int           sw, 
                         fm_int           tableIndex, 
                         fm_int           flowId,
                         fm_flowCounters *counters)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, flowId = %d, counters = %p\n",
                     sw,
                     tableIndex,
                     flowId,
                     (void *) counters);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetFlowCount,
                       sw,
                       tableIndex,
                       flowId,
                       counters);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmGetFlowCount */



/*****************************************************************************/
/** fmResetFlowCount
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Resets a flow's counters to zero.
 * 
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the flow belongs.
 * 
 * \param[in]       flowId is the flow instance.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if tableIndex identifies a flow table
 *                  that does not support counters.
 * \return          FM_ERR_INVALID_ACL if tableIndex is invalid.
 * \return          FM_ERR_INVALID_ACL_RULE if flowId is invalid.
 *
 *****************************************************************************/
fm_status fmResetFlowCount(fm_int sw, 
                           fm_int tableIndex, 
                           fm_int flowId)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, flowId = %d\n",
                     sw,
                     tableIndex,
                     flowId);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->ResetFlowCount,
                       sw,
                       tableIndex,
                       flowId);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmResetFlowCount */



/*****************************************************************************/
/** fmGetFlowUsed
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Determine if a packet has been received that matches
 *                  the specified flow. This function can be used to age out 
 *                  unused flow entries.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the flow belongs.
 * 
 * \param[in]       flowId is the flow instance.
 * 
 * \param[in]       clear should be set to TRUE to reset the flow's "used"
 *                  flag after it has been read.
 * 
 * \param[out]      used points to caller-allocated storage where the flow's
 *                  "used" flag will be stored. If clear is set to TRUE,
 *                  used will reflect the value of the "used" flag prior to
 *                  it being cleared.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex is invalid.
 * \return          FM_ERR_INVALID_ACL_RULE if flowId is invalid.
 *
 *****************************************************************************/
fm_status fmGetFlowUsed(fm_int   sw, 
                        fm_int   tableIndex, 
                        fm_int   flowId,
                        fm_bool  clear,
                        fm_bool *used)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, flowId = %d, clear = %d, used = %p\n",
                     sw,
                     tableIndex,
                     flowId,
                     clear,
                     (void *) used);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetFlowUsed,
                       sw,
                       tableIndex,
                       flowId,
                       clear,
                       used);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmGetFlowUsed */



/*****************************************************************************/
/** fmSetFlowAttribute
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Set a flow attribute value.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the
 *                  attribute belongs.
 * 
 * \param[in]       attr is the flow attribute to set (see 'Flow Attributes').
 * 
 * \param[in]       value points to the attribute value to set.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex is invalid.
 * \return          FM_ERR_UNSUPPORTED if attr is invalid for the specified
 *                  switch device.
 *
 *****************************************************************************/
fm_status fmSetFlowAttribute(fm_int sw, 
                             fm_int tableIndex, 
                             fm_int attr,
                             void * value)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, attr = %d, value = %p\n",
                     sw,
                     tableIndex,
                     attr,
                     value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->SetFlowAttribute,
                       sw,
                       tableIndex,
                       attr,
                       value);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmSetFlowAttribute */



/*****************************************************************************/
/** fmGetFlowAttribute
 * \ingroup flow
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Get a flow attribute value.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       tableIndex is the table instance to which the
 *                  attribute belongs.
 * 
 * \param[in]       attr is the flow attribute to get (see 'Flow Attributes').
 * 
 * \param[in]       value points to caller-allocated storage where this
 *                  function should place the retrieved attribute value.
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ACL if tableIndex is invalid.
 * \return          FM_ERR_UNSUPPORTED if attr is invalid for the specified
 *                  switch device.
 *
 *****************************************************************************/
fm_status fmGetFlowAttribute(fm_int sw, 
                             fm_int tableIndex, 
                             fm_int attr,
                             void * value)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_FLOW, 
                     "sw = %d, tableIndex = %d, attr = %d, value = %p\n",
                     sw,
                     tableIndex,
                     attr,
                     value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetFlowAttribute,
                       sw,
                       tableIndex,
                       attr,
                       value);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_FLOW, err);

}   /* end fmGetFlowAttribute */
