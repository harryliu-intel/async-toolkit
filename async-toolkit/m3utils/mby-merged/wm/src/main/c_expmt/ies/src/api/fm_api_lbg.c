/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lbg.c
 * Creation Date:   April 3, 2008
 * Description:     Functions for managing load balancing groups.
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

#define REQUIRED_LOCALS              \
    fm_switch * switchPtr;           \
    fm_LBGInfo *info;                \
    fm_status   err    = FM_OK;      \
    fm_status   preErr = FM_OK;

#define PREAMBLE_FRAGMENT                                           \
    if ( ( preErr = LBGPreamble(sw, &switchPtr, &info) ) != FM_OK ) \
    {                                                               \
        err = LBGPostamble(sw, preErr, FM_OK);                      \
                                                                    \
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, err);                       \
    }

#define POSTAMBLE_FRAGMENT               \
    err = LBGPostamble(sw, preErr, err); \
    FM_LOG_EXIT_API(FM_LOG_CAT_LBG, err);

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

/*****************************************************************************/
/** LBGPreamble
 *
 * \desc            Used at the beginning of every LBG exposed API function.
 *                  This functions handles validating the switch, the
 *                  family, and grabbing the access lock.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      switchPtr points to caller provided storage where the
 *                  pointer to the switch is written.
 *
 * \param[out]      info points to caller provided storage where the pointer
 *                  to the LBG info structure is written.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status LBGPreamble(fm_int sw, fm_switch **switchPtr, fm_LBGInfo **info)
{
    fm_status    err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG, "sw=%d, info=%p\n", sw, (void *) info);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    *switchPtr = GET_SWITCH_PTR(sw);

    if (!(*switchPtr)->CreateLBG)
    {
        FM_LOG_EXIT(FM_LOG_CAT_LBG, FM_ERR_UNSUPPORTED);
    }

    *info = &(*switchPtr)->lbgInfo;

    err = fmCaptureLock(&( (*info)->lbgLock ), NULL);

    if (err != FM_OK)
    {
        FM_LOG_FATAL( FM_LOG_CAT_LBG, "Unable to capture lock: %s\n",
                     fmErrorMsg(err) );
    }

    FM_LOG_EXIT(FM_LOG_CAT_LBG, FM_OK);

}   /* end LBGPreamble */




/*****************************************************************************/
/** LBGPostamble
 *
 * \desc            Handles the standard end of every exposed LBG API
 *                  function.  The return error precedence is as follows:
 *
 *                  preambleError > internal error > retError
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       preambleError is the error return from the preamble
 *                  function.
 *
 * \param[in]       retError is the error value to return
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status LBGPostamble(fm_int sw, fm_status preambleError, fm_status retError)
{
    fm_status   err = FM_OK;
    fm_switch * switchPtr;
    fm_LBGInfo *info;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG, "sw=%d\n", sw);

    switchPtr = GET_SWITCH_PTR(sw);

    info = &switchPtr->lbgInfo;

    if (preambleError == FM_OK)
    {
        err = fmReleaseLock(&info->lbgLock);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT( FM_LOG_CAT_LBG,
                (preambleError != FM_OK) ? preambleError :
                ( (err != FM_OK) ? err : retError ) );

}   /* end LBGPostamble */




/*****************************************************************************/
/** FreeLBGGroup
 * \ingroup intLbg
 *
 * \desc            This method frees the linked list structures of the
 *                  member list.
 *
 * \param[in]       ptr points to the object being freed.
 *
 *
 * \return          None
 *
 *****************************************************************************/
void FreeLBGGroup(void *ptr)
{
    fm_LBGGroup * group = (fm_LBGGroup *) ptr;
    fm_LBGMember *m, *p;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG, "ptr=%p\n", (void *) ptr);

    for (m = group->firstMember ; m ; )
    {
        p = m;
        m = m->nextMember;
        fmFree(p);
    }

    fmFree(group);

    FM_LOG_EXIT_VOID(FM_LOG_CAT_LBG);

}   /* end FreeLBGGroup */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/



/*****************************************************************************/
/** fmAllocateLBGsInt
 * \ingroup intLbg 
 *
 * \desc            Allocate LBGs given a glort range. The function returns
 *                  the base LBG number and the number of handles created.
 *                  The caller can then emumerate these handles by 'step' up to 
 *                  the number of handles allocated. These handles will have 
 *                  the same CAM resources across multiple switches, given the
 *                  input glort information is the same.
 *
 * \note            The return base handle might not be the same on different
 *                  switches. However the cam resources for 
 *                  (baseLbgNumber + n*step) will be consistent on 
 *                  different switches when using ''fmCreateStackLBG''..
 *                  In addition, this API can also be used to reserve specific
 *                  LBG resources on standalone switches. In this case,
 *                  calling ''fmCreateLBG'' will just provide a free entry
 *                  in this reserved pool first.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       startGlort is the starting glort to use to reserve 
 *                  for LBGs
 *
 * \param[in]       glortSize is the glort size to use. This value must be a
 *                  power of two.
 *
 * \param[out]      baseLbgNumber points to caller-allocated storage 
 *                  where this function should place the base LBG number
 *                  (handle) of the newly allocated LBGs pool.
 *
 * \param[out]      numLbgs points to caller-allocated storage 
 *                  where this function should place the number of LBGs
 *                  allocated given the specified glort space.
 *
 * \param[out]      step points to caller-allocated storage 
 *                  where this function should place the step value, where
 *                  the caller can increment from base by to get
 *                  subsequent LBG numbers
 * 
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if baseLbgNumber, numLbgs, or 
 *                  step is NULL or input parameters fail checking.
 * \return          FM_ERR_NO_MEM if not enough memory is available for the
 *                  LBG structure.
 * \return          FM_ERR_LOG_PORT_UNAVAILABLE if any glort or port resources
 *                  required in in the given glort range is being used.
 * \return          FM_ERR_NO_LBG_RESOURCES if no more resoures are available
 *
 *****************************************************************************/
fm_status fmAllocateLBGsInt(fm_int     sw,
                            fm_uint    startGlort,
                            fm_uint    glortSize,
                            fm_int    *baseLbgNumber,
                            fm_int    *numLbgs,
                            fm_int    *step)
{
    fm_status    err;
    fm_switch *  switchPtr;
    fm_int       baseHandle;
    fm_int       numHandles;
    fm_int       off;
 
    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "sw = %d, startGlort = %u, glortSize = %u, "
                 "baseLbgNumber = %p, numLbgs = %p, step = %p\n",
                 sw,
                 startGlort,
                 glortSize,
                 (void *) baseLbgNumber,
                 (void *) numLbgs,
                 (void *) step);

    *numLbgs = 0;

    switchPtr = GET_SWITCH_PTR(sw);
 
    FM_API_CALL_FAMILY(err, 
                       switchPtr->AllocateLBGs,
                       sw,
                       startGlort,
                       glortSize,
                       &baseHandle,
                       &numHandles,
                       &off);

    if (err == FM_OK)
    {
        /* Don't need to remap, use this directly */
        *numLbgs       = numHandles;
        *baseLbgNumber = baseHandle;
        *step          = off;
    }

    FM_LOG_EXIT(FM_LOG_CAT_LBG, err);

}   /* end fmAllocateLBGsInt */




/*****************************************************************************/
/** fmFreeLBGsInt
 * \ingroup intLbg 
 *
 * \desc            Free LBGs previously created with ''fmAllocateLBGsInt''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       baseLbgNumber is the base number previously created with
 *                  ''fmAllocateLBGsInt''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LBG if baseLbgNumber is not found
 * \return          FM_ERR_PORT_IN_USE if any port resources are still being
 *                  used.
 *
 *****************************************************************************/
fm_status fmFreeLBGsInt(fm_int    sw,
                        fm_int    baseLbgNumber)
{
    fm_status   err;
    fm_switch * switchPtr;
 
    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "sw = %d, baseLbgNumber = %d\n",
                 sw,
                 baseLbgNumber);

    switchPtr = GET_SWITCH_PTR(sw);
 
    FM_API_CALL_FAMILY(err,
                       switchPtr->FreeLBGs,
                       sw,
                       baseLbgNumber);


    FM_LOG_EXIT(FM_LOG_CAT_LBG, err);

}   /* end fmFreeLBGsInt */




/*****************************************************************************/
/** fmCreateLBGInt
 * \ingroup intLbg
 *
 * \desc            Create a new load balancing group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in,out]   lbgNumber points to caller allocated storage where this
 *                  function should place the logical port number of the newly
 *                  created LBG for non-stacking. For stacking mode, the caller
 *                  should place the desired lbgNumber to be used.
 *
 * \param[in]       params points to the parameter structure for LBGs.
 *
 * \param[in]       stacking is TRUE if this is a stacking LBG, FALSE if not.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if lbgNumber is NULL.
 * \return          FM_ERR_NO_MEM if no memory for LBG structures.
 *
 *****************************************************************************/
fm_status fmCreateLBGInt(fm_int sw, 
                         fm_int *lbgNumber, 
                         fm_LBGParams *params, 
                         fm_bool stacking)
{
    fm_int       err = FM_OK;
    fm_switch *  switchPtr;
    fm_LBGGroup *group = NULL;
    fm_LBGParams globalParams;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "sw=%d, lbgNumber=%p\n",
                 sw, (void *) lbgNumber);
    
    switchPtr = GET_SWITCH_PTR(sw);

    if (!stacking)
    {
        *lbgNumber = FM_LOGICAL_PORT_ANY;
    }

    if (params == NULL)
    {
        FM_CLEAR(globalParams);
        globalParams.mode = switchPtr->lbgInfo.mode;
        params = &globalParams;
    }

    /***************************************************
     * Allocate the group.
     **************************************************/

    group = (fm_LBGGroup *) fmAlloc( sizeof(fm_LBGGroup) );
    if (group == NULL)
    {
        err = FM_ERR_NO_MEM;
        goto ABORT;
    }

    FM_CLEAR(*group);

    /* The group has a list of all members */
    FM_DLL_INIT_LIST(group, firstMember, lastMember);

    FM_API_CALL_FAMILY(err, switchPtr->CreateLBG, sw, group, lbgNumber, params);

    if (err != FM_OK)
    {
        fmFree(group);
    }


ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_LBG, err);

}   /* end fmCreateLBGInt */




/*****************************************************************************/
/** fmAllocateLBGDataStructures
 * \ingroup intLbg
 *
 * \desc            This method is called upon switch insertion.
 *
 * \param[in]       switchPtr points to the switch structure for the switch
 *                  on which to operate.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmAllocateLBGDataStructures(fm_switch *switchPtr)
{
    fm_status   err  = FM_OK;
    fm_LBGInfo *info = &switchPtr->lbgInfo;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "switchPtr=%p\n",
                 (void *) switchPtr);

    err = fmCreateLockV2("LBGLock", 
                         switchPtr->switchNumber,
                         FM_LOCK_PREC_LBGS,
                         &info->lbgLock);

    FM_DLL_INIT_LIST(info, firstMember, lastMember);

    FM_LOG_EXIT(FM_LOG_CAT_LBG, err);

}   /* end fmAllocateLBGDataStructures */




/*****************************************************************************/
/** fmFreeLBGDataStructures
 * \ingroup intLbg
 *
 * \desc            This method is called upon switch removal.
 *
 * \param[in]       switchPtr points to the switch state table of the switch.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmFreeLBGDataStructures(fm_switch *switchPtr)
{
    fm_status   err  = FM_OK;
    fm_LBGInfo *info = &switchPtr->lbgInfo;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "switchPtr=%p\n",
                 (void *) switchPtr);

    err = fmDeleteLock(&info->lbgLock);

    FM_LOG_EXIT(FM_LOG_CAT_LBG, err);

}   /* end fmFreeLBGDataStructures */




/*****************************************************************************/
/** fmInitLBG
 * \ingroup intLbg
 *
 * \desc            This method is called when fmSetSwitchState brings a
 *                  switch back up.  It re-initializes the soft state related
 *                  to load balancing groups.
 *
 * \param[in]       switchPtr points to the switch state table of the switch.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitLBG(fm_switch *switchPtr)
{
    fm_LBGInfo *info = &switchPtr->lbgInfo;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "switchPtr=%p\n",
                 (void *) switchPtr);

    FM_NOT_USED(switchPtr);

    fmTreeInit(&info->groups);

    FM_LOG_EXIT(FM_LOG_CAT_LBG, FM_OK);

}   /* end fmInitLBG */




/*****************************************************************************/
/** fmGetLBGRouteData
 * \ingroup intLbg
 *
 * \desc            This helper method is used by the ACL compiler to retrieve
 *                  information about the LBG in order to appropriately set up
 *                  the route to logical port action.  Calling this method has
 *                  the side-effect of locking the load balancing group from
 *                  further administrative action.
 *
 *                  The typical sequence of events is that the stack software
 *                  configures the load balancing group, then creates an ACL
 *                  rule with a load balancing action.  Applying the ACL state
 *                  results in a call to this function which then locks the
 *                  configuration of the load balancing group.  Later, the
 *                  ACL rule is removed, and the subsequent apply will trigger
 *                  a call to ''fmGetLBGUnlock'' which allows administrative
 *                  action on the group again.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[out]      routeType points to caller allocated storage where the
 *                  routing method used by the load balancing group is
 *                  written.
 *
 * \param[out]      routeData points to caller allocated storage where either
 *                  the ARP base index or glort is written to.
 *
 * \param[out]      dataCount points to caller allocated storage where the
 *                  number of used ARP entries is written to.  This argument
 *                  is only written to when routeType is FM_LBG_ROUTE_ARP.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetLBGRouteData(fm_int sw, fm_int lbgNumber,
                            fm_LBGRouteType *routeType, fm_int *routeData,
                            fm_int *dataCount)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "sw=%d, lbgNumber=%d, routeType=%p, "
                 "routeData=%p, dataCount=%p\n",
                 sw, lbgNumber, (void *) routeType,
                 (void *) routeData, (void *) dataCount);

    if (!routeType || !routeData || !dataCount)
    {
        /* Need to exit if the code is before PREAMBLE_FRAGMENT */
        FM_LOG_EXIT(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    FM_API_CALL_FAMILY(err, switchPtr->GetLBGRouteData,
                       sw, lbgNumber, routeType, routeData,
                       dataCount);

    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGRouteData */




/*****************************************************************************/
/** fmGetLBGUnlock
 * \ingroup intLbg
 *
 * \desc            Allows administrative action to occur on this load
 *                  balancing group.  This function is the counterpart to
 *                  ''fmGetLBGRouteData''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmGetLBGUnlock(fm_int sw, fm_int lbgNumber)
{
    FM_LOG_ENTRY(FM_LOG_CAT_LBG,
                 "sw=%d, lbgNumber=%d\n",
                 sw, lbgNumber);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    FM_NOT_USED(sw);
    FM_NOT_USED(lbgNumber);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_LBG, FM_OK);

}   /* end fmGetLBGUnlock */




/*****************************************************************************/
/** fmCreateLBG
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Create a load balancing group.
 *
 * \note            See also, ''fmCreateLBGExt''.
 *
 * \note            This function should not be used after calling
 *                  ''fmAllocateStackLBGs'' (see ''Stacking and GloRT 
 *                  Management''). Instead, use ''fmCreateStackLBG''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      lbgNumber points to caller allocated storage where this
 *                  function should place the logical port number of the newly
 *                  created LBG.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if lbgNumber is NULL.
 * \return          FM_ERR_NO_MEM if no memory for LBG structures.
 *
 *****************************************************************************/
fm_status fmCreateLBG(fm_int sw, fm_int *lbgNumber)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%p\n",
                     sw, (void *) lbgNumber);

    if (!lbgNumber)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    err = fmCreateLBGInt(sw, lbgNumber, NULL, FALSE);

    POSTAMBLE_FRAGMENT;

}   /* end fmCreateLBG */




/*****************************************************************************/
/** fmCreateLBGExt
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Create a load balancing group. This function is similar
 *                  to ''fmCreateLBG'', but provides additional parameters
 *                  for the configuration of the resulting LBG.
 *
 * \note            This function should not be used after calling
 *                  ''fmAllocateStackLBGs'' (see ''Stacking and GloRT 
 *                  Management''). Instead, use ''fmCreateStackLBGExt''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      lbgNumber points to caller allocated storage where this
 *                  function should place the logical port number of the newly
 *                  created LBG.
 *
 * \param[in]       params points to the LBG parameter structure, used for
 *                  certain LBG modes.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if lbgNumber is NULL.
 * \return          FM_ERR_NO_MEM if no memory for LBG structures.
 *
 *****************************************************************************/
fm_status fmCreateLBGExt(fm_int sw, fm_int *lbgNumber, fm_LBGParams *params)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%p, params=%p\n",
                     sw, (void *) lbgNumber, (void *) params);

    if (!lbgNumber || !params)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    err = fmCreateLBGInt(sw, lbgNumber, params, FALSE);

    POSTAMBLE_FRAGMENT;

}   /* end fmCreateLBGExt */




/*****************************************************************************/
/** fmDeleteLBG
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Delete a load balancing group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG to delete,
 *                  as returned by ''fmCreateLBG'' or ''fmCreateLBGExt'' or
 *                  specified in the call to ''fmCreateStackLBG''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 *
 *****************************************************************************/
fm_status fmDeleteLBG(fm_int sw, fm_int lbgNumber)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d\n",
                     sw, lbgNumber);

    PREAMBLE_FRAGMENT;

    FM_API_CALL_FAMILY(err, switchPtr->DeleteLBG, sw, lbgNumber);

    if (err == FM_OK)
    {
        err = fmTreeRemove(&info->groups, lbgNumber, FreeLBGGroup);
    }
    else if (err == FM_ERR_NOT_FOUND)
    {
        err = FM_ERR_INVALID_LBG;
    }

    POSTAMBLE_FRAGMENT;

}   /* end fmDeleteLBG */




/*****************************************************************************/
/** fmAddLBGPort
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Add a new member port to a load balancing group. The
 *                  LBG must not be in the active state when adding ports.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       port is the logical port number of the member port to add.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_LBG_STATE if the LBG is currently in the
 *                  active state.
 * \return          FM_ERR_NO_MEM if no memory available for data structures.
 *
 *****************************************************************************/
fm_status fmAddLBGPort(fm_int sw, fm_int lbgNumber, fm_int port)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, port=%d\n",
                     sw, lbgNumber, port);

    PREAMBLE_FRAGMENT;

    /* Support for (non-cpu) cardinal and remote ports */
    if ( !fmIsValidPort(sw, port, ALLOW_REMOTE) )
    {
        err = FM_ERR_INVALID_PORT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LBG, err);
    }

    FM_API_CALL_FAMILY(err, switchPtr->AddLBGPort, sw, lbgNumber, port);

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmAddLBGPort */




/*****************************************************************************/
/** fmDeleteLBGPort
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Delete a load balancing group member port. The
 *                  LBG must not be in the active state when deleting ports.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       port is the logical port number of the member port to
 *                  delete.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_LBG_STATE if the LBG is currently in the
 *                  active state.
 * \return          FM_ERR_PORT_NOT_LBG_MEMBER if port is not a member of the
 *                  LBG.
 *
 *****************************************************************************/
fm_status fmDeleteLBGPort(fm_int sw, fm_int lbgNumber, fm_int port)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, port=%d\n",
                     sw, lbgNumber, port);

    PREAMBLE_FRAGMENT;

    /* Support for (non-cpu) cardinal and remote ports */
    if ( !fmIsValidPort(sw, port, ALLOW_REMOTE) )
    {
        err = FM_ERR_INVALID_PORT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LBG, err);
    }

    FM_API_CALL_FAMILY(err, switchPtr->DeleteLBGPort, sw, lbgNumber, port);

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmDeleteLBGPort */




/*****************************************************************************/
/** fmGetLBGList
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            List all existing load balancing groups.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      numLBGs points to caller-allocated storage where this
 *                  function should place the number of LBGs listed.
 *
 * \param[out]      lbgList points to a caller-allocated array of size max 
 *                  where this function should place the list of existing 
 *                  LBG logical port numbers.
 *
 * \param[in]       max is the size of lbgList, being the maximum number of
 *                  LBG logical port numbers that may be accommodated.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if numLBGs or lbgList is NULL.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the list of LBGs.
 *
 *****************************************************************************/
fm_status fmGetLBGList(fm_int sw, fm_int *numLBGs, fm_int *lbgList, fm_int max)
{
    fm_treeIterator iter;
    fm_uint64       nextKey;
    void *          nextData;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, numLBGs=%p, lbgList=%p, max=%d\n",
                     sw, (void *) numLBGs, (void *) lbgList, max);

    if (!lbgList || !numLBGs)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    *numLBGs = 0;

    fmTreeIterInit(&iter, &info->groups);

    err = fmTreeIterNext(&iter, &nextKey, &nextData);

    while (err == FM_OK)
    {
        if (*numLBGs < max)
        {
            lbgList[(*numLBGs)++] = (fm_uint32) nextKey;
    
            err = fmTreeIterNext(&iter, &nextKey, &nextData);
        }
        else
        {
            err = FM_ERR_BUFFER_FULL;
            break;
        }
    }

    /* This is the only valid error */
    if (err == FM_ERR_NO_MORE)
    {
        err = FM_OK;
    }
    
    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGList */




/*****************************************************************************/
/** fmGetLBGFirst
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the first existing load balancing group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      firstLbgNumber points to caller-allocated storage where
 *                  this function should place the first existing LBG 
 *                  logical port number. If no load balancing groups exist, 
 *                  firstLbgNumber will be set to -1.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if firstLbgNumber is NULL.
 * \return          FM_ERR_NO_MORE if there are no LBGs.
 * \return          FM_ERR_MODIFIED_WHILE_ITERATING if an LBG was
 *                  created or deleted while searching for the first LBG.
 *
 *****************************************************************************/
fm_status fmGetLBGFirst(fm_int sw, fm_int *firstLbgNumber)
{
    fm_treeIterator iter;
    fm_uint64       nextKey;
    void *          nextData;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, firstLbgNumber=%p\n",
                     sw, (void *) firstLbgNumber);

    if (!firstLbgNumber)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    fmTreeIterInit(&iter, &info->groups);

    err = fmTreeIterNext(&iter, &nextKey, &nextData);

    *firstLbgNumber = (err != FM_ERR_NO_MORE) ?
                      ( (fm_int32) nextKey ) :
                      -1;

    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGFirst */




/*****************************************************************************/
/** fmGetLBGNext
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the next existing load balancing group, following 
 *                  a prior call to this function or to ''fmGetLBGFirst''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       currentLbgNumber is the last LBG found by a previous call 
 *                  to this function or to ''fmGetLBGFirst''.
 *
 * \param[out]      nextLbgNumber points to caller-allocated storage where this 
 *                  function should place the next existing LBG. Will be set 
 *                  to -1 if no more LBGs found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if nextLbgNumber is NULL.
 * \return          FM_ERR_NO_MORE if there are no more LBGs.
 * \return          FM_ERR_MODIFIED_WHILE_ITERATING if an LBG was
 *                  created or deleted while searching for the next LBG.
 *****************************************************************************/
fm_status fmGetLBGNext(fm_int sw, fm_int currentLbgNumber,
                       fm_int *nextLbgNumber)
{
    fm_treeIterator iter;
    fm_uint64       nextKey;
    void *          nextData;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, currentLbgNumber=%d, nextLbgNumber=%p\n",
                     sw, currentLbgNumber, (void *) nextLbgNumber);

    if (!nextLbgNumber)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    err = fmTreeIterInitFromKey(&iter, &info->groups, currentLbgNumber);

    if (err != FM_OK)
    {
        goto ABORT;
    }

    err = fmTreeIterNext(&iter, &nextKey, &nextData);

    if (err == FM_OK)
    {
        err = fmTreeIterNext(&iter, &nextKey, &nextData);

        *nextLbgNumber = (err != FM_ERR_NO_MORE) ?
                         ( (fm_int32) nextKey ) :
                         -1;
    }

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGNext */




/*****************************************************************************/
/** fmGetLBGPortList
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            List all member ports in a load balancing group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[out]      numPorts points to caller-allocated storage where this
 *                  function should place the number of LBG member ports listed.
 *
 * \param[out]      portList points to a caller-allcoated array of size max 
 *                  where this function should place the list of LBG member
 *                  ports.
 *
 * \param[in]       max is the size of portList, being the maximum number of
 *                  LBG member ports that may be accommodated.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if numPorts or portList is NULL.
 * \return          FM_ERR_BUFFER_FULL if max was too small to accommodate
 *                  the list of LBG member ports.
 *****************************************************************************/
fm_status fmGetLBGPortList(fm_int sw, fm_int lbgNumber, fm_int *numPorts,
                           fm_int *portList, fm_int max)
{
    fm_LBGGroup * group;
    fm_LBGMember *member;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, numPorts=%p, portList=%p, max=%d\n",
                     sw, lbgNumber, (void *) numPorts, (void *) portList, max);

    if (!numPorts || !portList)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    err = fmTreeFind(&info->groups, lbgNumber, (void **) &group);

    if (err != FM_OK)
    {
        err = FM_ERR_INVALID_LBG;
        goto ABORT;
    }

    for (*numPorts = 0, member = group->firstMember ;
         member ;
         member = member->nextMember)
    {
        if (*numPorts < max)
        {
            portList[(*numPorts)++] = member->lbgMemberPort;
        }
        else
        {
            err = FM_ERR_BUFFER_FULL;
            break;
        }
    }

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGPortList */




/*****************************************************************************/
/** fmGetLBGPortFirst
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the first member port of a load balancing group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[out]      firstLbgPort points to caller-allocated storage where
 *                  this function should place the first member port of the
 *                  specified LBG. If LBG has no member ports, firstLbgPort 
 *                  will be set to -1.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if firstLbgPort is NULL.
 * \return          FM_ERR_NO_MORE if there are no member ports in the LBG.
 * \return          FM_ERR_MODIFIED_WHILE_ITERATING if an LBG was
 *                  created or deleted while searching for the first member
 *                  port.
 *****************************************************************************/
fm_status fmGetLBGPortFirst(fm_int sw, fm_int lbgNumber, fm_int *firstLbgPort)
{
    fm_LBGGroup *group;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, firstLbgPort=%p\n",
                     sw, lbgNumber, (void *) firstLbgPort);

    if (!firstLbgPort)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    err = fmTreeFind(&info->groups, lbgNumber, (void **) &group);

    if (err != FM_OK)
    {
        err = FM_ERR_INVALID_LBG;
        goto ABORT;
    }

    if (group->firstMember)
    {
        *firstLbgPort = group->firstMember->lbgMemberPort;
    }
    else
    {
        err = FM_ERR_NO_MORE;
        *firstLbgPort = -1;
    }

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGPortFirst */




/*****************************************************************************/
/** fmGetLBGPortNext
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Retrieve the next member port of a load balancing group, 
 *                  following a prior call to this function or to 
 *                  ''fmGetLBGPortFirst''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       currentLbgPort is the last LBG member port found by a 
 *                  previous call to this function or to ''fmGetLBGPortFirst''.
 *
 * \param[out]      nextLbgPort points to caller-allocated storage where this 
 *                  function should place the next LBG member port. Will be set 
 *                  to -1 if no more member ports found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if nextLbgPort is NULL.
 * \return          FM_ERR_NO_MORE if there are no more member ports in the LBG.
 * \return          FM_ERR_MODIFIED_WHILE_ITERATING if an LBG member port was 
 *                  added or deleted while searching for the next member port.
 *****************************************************************************/
fm_status fmGetLBGPortNext(fm_int sw, fm_int lbgNumber, fm_int currentLbgPort,
                           fm_int *nextLbgPort)
{
    fm_LBGGroup * group;
    fm_LBGMember *member;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, currentLbgPort=%d, nextLbgPort=%p\n",
                     sw, lbgNumber, currentLbgPort, (void *) nextLbgPort);

    if (!nextLbgPort)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    err = fmTreeFind(&info->groups, lbgNumber, (void **) &group);

    if (err != FM_OK)
    {
        err = FM_ERR_INVALID_LBG;
        goto ABORT;
    }

    member = group->firstMember;

    while ( member && (member->lbgMemberPort != currentLbgPort) )
    {
        member = member->nextMember;
    }

    if (!member ||
        (member->lbgMemberPort != currentLbgPort ) ||
        !member->nextMember)
    {
        /* FIXME: proper error */
        err = FM_ERR_NO_MORE;

        goto ABORT;
    }

    *nextLbgPort = member->nextMember->lbgMemberPort;

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGPortNext */




/*****************************************************************************/
/** fmSetLBGAttribute
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Set a load balancing group attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       attr is the load balancing group attribute (see 
 *                  ''Load Balancing Group Attributes'') to set.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if value is NULL.
 * \return          FM_ERR_UNSUPPORTED if attr is not recognized.
 *
 *****************************************************************************/
fm_status fmSetLBGAttribute(fm_int sw, 
                            fm_int lbgNumber, 
                            fm_int attr,
                            void *value)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, attr=%d, value=%p\n",
                     sw, lbgNumber, attr, (void *) value);

    if (!value)
    {
        /* Need to exit if the code is before PREAMBLE_FRAGMENT */
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    FM_API_CALL_FAMILY(err, switchPtr->SetLBGAttribute,
                       sw, lbgNumber, attr, value);

    POSTAMBLE_FRAGMENT;

}   /* end fmSetLBGAttribute */




/*****************************************************************************/
/** fmGetLBGAttribute
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Get a load balancing group attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       attr is the load balancing group attribute (see 
 *                  ''Load Balancing Group Attributes'') to get.
 *
 * \param[in]       value points to call-allocated storage where this function
 *                  should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if value is NULL.
 * \return          FM_ERR_UNSUPPORTED if attr is not recognized.
 *
 *****************************************************************************/
fm_status fmGetLBGAttribute(fm_int sw, fm_int lbgNumber, fm_int attr,
                            void *value)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, attr=%d, value=%p\n",
                     sw, lbgNumber, attr, (void *) value);

    if (!value)
    {
        /* Need to exit if the code is before PREAMBLE_FRAGMENT */
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    FM_API_CALL_FAMILY(err, switchPtr->GetLBGAttribute,
                       sw, lbgNumber, attr, value);

    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGAttribute */




/*****************************************************************************/
/** fmSetLBGPortAttribute
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Set a load balancing group member port attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       port is the logical port number of the member port.
 *
 * \param[in]       attr is the load balancing group port attribute (see 
 *                  ''Load Balancing Group Port Attributes'') to set.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if value is NULL.
 * \return          FM_ERR_UNSUPPORTED if attr is not recognized.
 *
 *****************************************************************************/
fm_status fmSetLBGPortAttribute(fm_int sw,
                                fm_int lbgNumber,
                                fm_int port,
                                fm_int attr,
                                void * value)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, port=%d, attr=%d, value=%p\n",
                     sw, lbgNumber, port, attr, (void *) value);

    if (!value)
    {
        /* Need to exit if the code is before PREAMBLE_FRAGMENT */
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    if ( !fmIsValidPort(sw, port, ALLOW_REMOTE) )
    {
        /* This accesses the switchPtr so do it after PREAMBLE_FRAGMENT */
        err = FM_ERR_INVALID_PORT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LBG, err);
    }

    FM_API_CALL_FAMILY(err, switchPtr->SetLBGPortAttribute,
                       sw, lbgNumber, port, attr, value);

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmSetLBGPortAttribute */




/*****************************************************************************/
/** fmGetLBGPortAttribute
 * \ingroup lbg
 *
 * \chips           FM3000, FM4000, FM6000
 *
 * \desc            Get a load balancing group member port attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the logical port number of the LBG.
 *
 * \param[in]       port is the logical port number of the member port.
 *
 * \param[in]       attr is the load balancing group port attribute (see 
 *                  ''Load Balancing Group Port Attributes'') to get.
 *
 * \param[in]       value points to caller-allocated storage where this
 *                  function should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_LBG if lbgNumber is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if value is NULL.
 * \return          FM_ERR_UNSUPPORTED if attr is not recognized.
 *
 *****************************************************************************/
fm_status fmGetLBGPortAttribute(fm_int sw,
                                fm_int lbgNumber,
                                fm_int port,
                                fm_int attr,
                                void * value)
{
    REQUIRED_LOCALS;

    FM_LOG_ENTRY_API(FM_LOG_CAT_LBG,
                     "sw=%d, lbgNumber=%d, port=%d, attr=%d, value=%p\n",
                     sw, lbgNumber, port, attr, (void *) value);

    if (!value)
    {
        /* Need to exit if the code is before PREAMBLE_FRAGMENT */
        FM_LOG_EXIT_API(FM_LOG_CAT_LBG, FM_ERR_INVALID_ARGUMENT);
    }

    PREAMBLE_FRAGMENT;

    if ( !fmIsValidPort(sw, port, ALLOW_REMOTE) )
    {
        /* This accesses the switchPtr so do it after PREAMBLE_FRAGMENT */
        err = FM_ERR_INVALID_PORT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_LBG, err);
    }

    FM_API_CALL_FAMILY(err, switchPtr->GetLBGPortAttribute,
                       sw, lbgNumber, port, attr, value);

ABORT:
    POSTAMBLE_FRAGMENT;

}   /* end fmGetLBGPortAttribute */




/*****************************************************************************/
/** fmDbgDumpLBG
 * \ingroup debug
 *
 * \desc            Prints out debugging information related to the LBG group.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       lbgNumber is the group number.  Use -1 to dump information
 *                  about all LBGs.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmDbgDumpLBG(fm_int sw, fm_int lbgNumber)
{
    fm_int lbgList[256];
    fm_int numLbgs;

    REQUIRED_LOCALS;

    FM_LOG_ENTRY(FM_LOG_CAT_LBG, "sw=%d, lbg=%d\n", sw, lbgNumber);

    PREAMBLE_FRAGMENT;

    if (lbgNumber != -1)
    {
        FM_API_CALL_FAMILY(err, switchPtr->DbgDumpLBG, sw, lbgNumber);
    }
    else
    {
        err = fmGetLBGList(sw, &numLbgs, lbgList, 256);

        if (err == FM_OK)
        {
            for (lbgNumber = 0 ; lbgNumber < numLbgs ; lbgNumber++)
            {
                /* Ignore errors here */
                FM_API_CALL_FAMILY(err,
                                   switchPtr->DbgDumpLBG,
                                   sw,
                                   lbgList[lbgNumber]);
            }
        }
    }

    POSTAMBLE_FRAGMENT;

}   /* end fmDbgDumpLBG */
