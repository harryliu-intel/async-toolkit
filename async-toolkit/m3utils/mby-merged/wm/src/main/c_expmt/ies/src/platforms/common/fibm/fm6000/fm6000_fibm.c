/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm6000_fibm.c
 * Creation Date:   Aug 22, 2008
 * Description:     FM6000 FIBM implementation
 *
 * INTEL CONFIDENTIAL
 * Copyright 2011-2012  Intel Corporation. All Rights Reserved. 
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


#include <fm_sdk_fm6000_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fm6000FibmEnableSlaveSwitch
 * \ingroup intFibm
 *
 * \desc            Perform any hardware initialization on the master switch
 *                  to support FIBM to control the slave/remote switch. If the
 *                  remote switch is not directly connected to the master
 *                  switch, it is the responsibility of the platform to add
 *                  additional setup on the intermediate switches enable
 *                  the connection between the master and remote switch.
 *
 * \param[in]       slaveSw is the slave switch number.
 *
 *
 * \param[in]       enable is to enable or disable fibm
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmEnableSlaveSwitch(fm_int slaveSw, fm_bool enable)
{
    fm_status           status = FM_OK;
    fm_switch *         switchPtr;
    fm_fibmSwitchConfig cfg[1];
    fm_int              masterSw;
    fm_int              physPort;
    fm_islTagFormat     islFormat = FM_ISL_TAG_F64;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fm6000FibmEnableSlaveSwitch: slaveSw %d enable %d\n",
                 slaveSw, 
                 enable);

    /* Slave/remote switch has to be bootstrapped to enable FIBM
     * Here we only enable local switch to process FIBM
     * message comming from the remote switch on the local port. */ 
    if ( !FIBM_INFO_VALID(slaveSw) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_UNINITIALIZED);
    }

    if (FIBM_INFO(slaveSw)->flags & FM_FIBM_FLAGS_INITED)
    {
        /* This function can be called twice */
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
    }
    
    switchPtr = fmRootApi->fmSwitchStateTable[slaveSw];

    if (!switchPtr->GetFibmSwitchConfig)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_UNSUPPORTED);
    }

    /* Get the platform fibm switch config */
    status = switchPtr->GetFibmSwitchConfig(slaveSw, cfg);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    }

    TAKE_FIBM_TXRX_LOCK(slaveSw);

    if (enable)
    {
        /* Copy the configuration over if enabled */
        memcpy( &FIBM_INFO(slaveSw)->cfg, cfg, sizeof(fm_fibmSwitchConfig) );
    }

    DROP_FIBM_TXRX_LOCK(slaveSw);

    masterSw = cfg->masterSwitch;

    /* The slave switch is controlled via another switch */
    if (masterSw >= 0)
    {
        VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(status, masterSw);
        
        if (status != FM_OK)
        {
            goto EXIT;
        }
        
        switchPtr = fmRootApi->fmSwitchStateTable[masterSw];

        status = fmMapLogicalPortToPhysical(switchPtr, 
                                            cfg->masterMgmtPort, 
                                            &physPort);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);

#if 0
        /* Don't drop mgmt ISL */
        status = switchPtr->ReadUINT32(masterSw,
                                       FM4000_PORT_CFG_1(physPort),
                                       &val32);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);

        FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, enable ? 0 : 1);

        status = switchPtr->WriteUINT32(masterSw,
                                        FM4000_PORT_CFG_1(physPort),
                                        val32);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
#endif

        /* Use F64 Tag */
        status = fmSetPortAttribute(masterSw,
                                    cfg->masterMgmtPort,
                                    FM_PORT_ISL_TAG_FORMAT,
                                    &islFormat);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
        
ABORT:
        UNPROTECT_SWITCH(masterSw);
        
    }   /* end if (masterSw >= 0) */
    
EXIT:
    FIBM_INFO(slaveSw)->flags |= FM_FIBM_FLAGS_INITED;
    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);
    
} /* end fm6000FibmEnableSlaveSwitch */




/*****************************************************************************/
/** fm6000FibmEnableMasterSwitch
 * \ingroup intSwitch
 *
 * \desc            Perform any hardware initialization to support FIBM on
 *                  master switch
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       enable is to enable or disable fibm
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmEnableMasterSwitch(fm_int sw, fm_bool enable)
{
    fm_status status;
    fm_uint32 val32;
    fm_switch *switchPtr = fmRootApi->fmSwitchStateTable[sw];

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fm6000FibmEnableMasterSwitch: sw %d enable %d\n", sw, enable);

    if ( FIBM_INFO_VALID(sw) )
    {
        /* Master switch should not have fibm configuration */
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    /* Enable FIBM on local switch */
    status = switchPtr->ReadUINT32(sw, FM6000_FIBM_CFG, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    if (enable)
    {
        FM_SET_BIT(val32, FM6000_FIBM_CFG, disableIbm, 0);
        FM_SET_BIT(val32, FM6000_FIBM_CFG, ibmGlortEn, 0);
        FM_SET_BIT(val32, FM6000_FIBM_CFG, interruptGlortEn, 0);
    }
    else
    {
        FM_SET_BIT(val32, FM6000_FIBM_CFG, disableIbm, 1);
        FM_SET_BIT(val32, FM6000_FIBM_CFG, ibmGlortEn, 1);
        FM_SET_BIT(val32, FM6000_FIBM_CFG, interruptGlortEn, 1);
    }

    status = switchPtr->WriteUINT32(sw, FM6000_FIBM_CFG, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Source glort port for response, only needed if we
     * are doing FIBM on local switch
     */
    status = switchPtr->ReadUINT32(sw, FM6000_FIBM_SGLORT, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    val32  = FM6000_GLORT_FIBM(0);
    status = switchPtr->WriteUINT32(sw, FM6000_FIBM_SGLORT, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Since we are not enable FIBM interrupt on local switch
     * we don't need to configure interrupt glort and interval
     */

    /* Ethertype for response frames */
    status = switchPtr->ReadUINT32(sw, FM6000_FIBM_INT_FRAME, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    FM_SET_FIELD(val32, FM6000_FIBM_INT_FRAME, etherType, FM_FIBM_ETHERTYPE);
    FM_SET_FIELD(val32, FM6000_FIBM_INT_FRAME, islSysPri, FM_FIBM_SYS_PRI);
    status = switchPtr->WriteUINT32(sw, FM6000_FIBM_INT_FRAME, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

#if 0
    /* Don't drop mgmt ISL on CPU port */
    status = switchPtr->ReadUINT32(sw, FM4000_PORT_CFG_1(0), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, enable ? 0 : 1);

    status = switchPtr->WriteUINT32(sw, FM4000_PORT_CFG_1(0), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Use F64 Tag on CPU port */
    status = switchPtr->ReadUINT32(sw, FM4000_PARSE_CFG(0), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    FM_SET_FIELD(val32, FM4000_PARSE_CFG, ISLTag, FM4000_ENABLE_F64_TAG);
    status = switchPtr->WriteUINT32(sw, FM4000_PARSE_CFG(0), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
#endif

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_OK);
} /* end fm6000FibmEnableMasterSwitch */




/*****************************************************************************/
/** fm6000FibmAddSlaveSwitch
 * \ingroup intSwitch
 *
 * \desc            Function to simulate a slave/remote switch insertion. This can
 *                  be called on the local switch after the remote switch
 *                  is brought up.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmAddSlaveSwitch(fm_int sw)
{
    fm_status               err = FM_OK;
    fm_event *              insertEvent;
    fm_eventSwitchInserted *insert;
    fm_int                  i;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fm6000FibmAddSlaveSwitch: sw %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        err = FM_ERR_INVALID_ARGUMENT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, err);
    }

    if (!IS_PLAT_STATE_INITED)
    {
        err = FM_ERR_UNINITIALIZED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, err);
    }

#ifdef FM_FIBM_NIC_SELECT_TIMEOUT

    err = fmNicIsDeviceReady(sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, err);

#endif

    /* assign switch number */
    GET_PLAT_STATE(sw)->sw         = sw;
    GET_PLAT_STATE(sw)->family     = FM_SWITCH_FAMILY_REMOTE_FM6000;
    GET_PLAT_STATE(sw)->intrSource = FM_INTERRUPT_SOURCE_NONE;

    /* initialize locks */
    for (i = 0 ; i < FM_MAX_PLAT_LOCKS ; i++)
    {
        if (i == FM_MEM_TYPE_CSR)
        {
            err = fmCreateLockV2("Platform Register Access", 
                                 sw,
                                 FM_LOCK_PREC_PLATFORM,
                                 &GET_PLAT_STATE(sw)->accessLocks[i]);
        }
        else
        {
            err = fmCreateLock("Platform Access", &GET_PLAT_STATE(sw)->accessLocks[i]);
        }

        if (err != FM_OK)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, err);
        }
    }

    /***************************************************
     * Allocate and generate the switch inserted event
     * for this switch.
     **************************************************/

    /* This is priority high because we don't want to be throttled */
    insertEvent = fmAllocateEvent(sw,
                                  FM_EVID_SYSTEM,
                                  FM_EVENT_SWITCH_INSERTED,
                                  FM_EVENT_PRIORITY_HIGH);

    if (!insertEvent)
    {
        FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                     "Unable to allocate event for switch insertion\n");

        /**************************************************
         * We have to return here.  If we keep going, we'll
         * dereference a NULL pointer at "insert->model = -1".
         **************************************************/
        err = FM_FAIL;
    }
    else
    {
        insert = &insertEvent->info.fpSwitchInsertedEvent;   
        insert->model = -1;
        insert->slot  = sw;
        err = fmSendThreadEvent(&fmRootApi->eventThread, insertEvent);
    }
ABORT: 
    return err;

} /* end fm6000FibmAddSlaveSwitch */




/*****************************************************************************/
/** fm6000FibmRemoveSlaveSwitch
 * \ingroup intSwitch
 *
 * \desc            Function to simulate a slave/remote switch removal.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmRemoveSlaveSwitch(fm_int sw)
{
    fm_event *removeEvent;


    FM_LOG_ENTRY(FM_LOG_CAT_FIBM,
                 "fm6000FibmRemoveSlaveSwitch: sw %d\n", sw);

    if ( (sw > FM_MAX_NUM_FOCALPOINTS) || (sw < 0) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_FIBM, FM_ERR_INVALID_ARGUMENT);
    }

    /***************************************************
     * Allocate and generate the switch inserted event
     * for this switch.
     **************************************************/

    /* This is priority high because we don't want to be throttled */
    removeEvent = fmAllocateEvent(sw,
                                  FM_EVID_SYSTEM,
                                  FM_EVENT_SWITCH_REMOVED,
                                  FM_EVENT_PRIORITY_HIGH);

    if (!removeEvent)
    {
        FM_LOG_FATAL(FM_LOG_CAT_FIBM,
                     "Unable to allocate event for switch removal\n");

        /**************************************************
         * We have to return here.  If we keep going, we'll
         * dereference a NULL pointer at "insert->model = -1".
         **************************************************/
        return FM_FAIL;
    }

    removeEvent->info.fpSwitchRemovedEvent.slot = sw;

    return fmSendThreadEvent(&fmRootApi->eventThread, removeEvent);

} /* end fm6000FibmRemoveSlaveSwitch */




/*****************************************************************************/
/** fm6000FibmCreateFwdRuleMasterSwitch
 * \ingroup intSwitch
 *
 * \desc            Function to create all neccessary forwarding rules on the
 *                  master switch to forward traffic to/from slave switch. This
 *                  only setups the master switch. If there are intermediate
 *                  switch between the master and the slave, then additional
 *                  forwarding entries must be created on the intermediate
 *                  switches.
 *
 * \param[in]       masterSw is the master switch number.
 *
 * \param[in]       masterFibmPort is the fibm mgmt port on the master switch.
 *
 * \param[in]       slaveSw is the slave switch number.
 *
 * \param[in]       slaveGlortBase is the glort base of slave switch
 *
 * \param[in]       slaveGlortMask is the glort mask of slave switch.
 *
 * \param[out]      fwdRuleId is pointer to forwarding rule ids created.
 *
 * \param[out]      fwdRuleCnt is the number of forwarding rule ids created.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmCreateFwdRuleMasterSwitch(fm_int  masterSw,
                                              fm_int  masterFibmPort,
                                              fm_int  slaveSw,
                                              fm_int  slaveGlortBase,
                                              fm_int  slaveGlortMask,
                                              fm_int *fwdRuleId,
                                              fm_int *fwdRuleCnt)
{
    fm_forwardRule rule;
    fm_status      status;

    FM_NOT_USED(slaveSw);

    /* Create an entry on the master switch to direct
     * whole glort space of slave switch to fibm port.
     * This allows PacketSendDirect to send packets
     * to a specific port on the slave
     */
    rule.glort       = slaveGlortBase;
    rule.mask        = slaveGlortMask;
    rule.logicalPort = masterFibmPort;
    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                 "Switch #%u: Creating a forwarding rule (0x%04x/%04x->%u) "
                 "to slave traffic to slave\n", masterSw,
                 rule.glort, rule.mask, rule.logicalPort);
    *fwdRuleCnt = 0;
    status      = fmCreateStackForwardingRule(masterSw, 
                                              &fwdRuleId[*fwdRuleCnt], 
                                              &rule);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    (*fwdRuleCnt)++;

    /* Create an entry on the master switch to direct
     * slave CPU traffic to master CPU port
     */
#if 0
    /* This is not needed since CPU port is the same on all switches
     * And the master already has an entry for this
     */
    rule.glort       = FM4000_GLORT_CPU_BASE(slaveGlortBase);
    rule.mask        = 0x00FF;
    rule.logicalPort = 0;
    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                 "Switch #%u: Creating a forwarding rule (0x%04x/%04x->%u) "
                 "to direct slave slowpath traffic to master CPU\n", masterSw,
                 rule.glort, rule.mask, rule.logicalPort);
    status = fmCreateStackForwardingRule(masterSw,
                                         &fwdRuleId[*fwdRuleCnt],
                                         &rule);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    (*fwdRuleCnt)++;

    /* Create an entry on the master switch to direct
     * FIBM request message to slave switch
     */
    /* Not needed since this is a subset of switch glort space */
    rule.glort       = FM4000_GLORT_FIBM(slaveGlortBase);
    rule.mask        = 0x0000;
    rule.logicalPort = masterFibmPort;
    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                 "Switch #%u: Creating a forwarding rule (0x%04x/%04x->%u) "
                 "to send FIBM request to slave\n", masterSw,
                 rule.glort, rule.mask, rule.logicalPort);
    status = fmCreateStackForwardingRule(masterSw,
                                         &fwdRuleId[*fwdRuleCnt],
                                         &rule);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    (*fwdRuleCnt)++;
#endif

    return FM_OK;

} /* end fm6000FibmCreateFwdRuleMasterSwitch */




/*****************************************************************************/
/** fm6000FibmCreateFwdRuleSlaveSwitch
 * \ingroup intSwitch
 *
 * \desc            Function to create all neccessary forwarding rules on the
 *                  slave switch to forward traffic to/from slave switch.
 *
 * \param[in]       slaveSw is the slave switch number.
 *
 * \param[in]       slaveFibmPort is the fibm mgmt port on the slave switch.
 *
 * \param[in]       masterSw is the master switch number.
 *
 * \param[in]       masterGlortBase is the glort base of master switch
 *
 * \param[in]       masterGlortMask is the glort mask of master switch.
 *
 * \param[out]      fwdRuleId is pointer to forwarding rule ids created.
 *
 * \param[out]      fwdRuleCnt is the number of forwarding rule ids created.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmCreateFwdRuleSlaveSwitch(fm_int  slaveSw,
                                             fm_int  slaveFibmPort,
                                             fm_int  masterSw,
                                             fm_int  masterGlortBase,
                                             fm_int  masterGlortMask,
                                             fm_int *fwdRuleId,
                                             fm_int *fwdRuleCnt)
{
    fm_forwardRule rule;
    fm_status      status;

    FM_NOT_USED(masterSw);

    *fwdRuleCnt = 0;
    /* Add an entry for slave CPU to send all packets to
     * master cpu glort space to port connecting to master
     * switch
     */
    rule.glort       = FM6000_GLORT_FIBM(masterGlortBase);
    rule.mask        = masterGlortMask;
    rule.logicalPort = slaveFibmPort;
    FM_LOG_DEBUG(FM_LOG_CAT_FIBM,
                 "Switch #%u: Creating a forwarding rule (0x%04x/%04x->%u) "
                 "to send FIBM response to master switch\n", slaveSw,
                 rule.glort, rule.mask, rule.logicalPort);
    status = fmCreateStackForwardingRule(slaveSw, 
                                         &fwdRuleId[*fwdRuleCnt], 
                                         &rule);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    (*fwdRuleCnt)++;

    /* An entry to send FIBM request up to mgmt CPU is already created
     * in PortInitialize. A logical CpuMgmtPort is created for this.
     * Hence it is not needed to create one here.
     */

#if 0
    /* Invalidate two bootstrap entries during switch bring up */
    status = fmRegCacheWriteSingle1D ( slaveSw,
                                       &fm4000CacheGlortCam,
                                       &zero,
                                       FM6000_FIBM_MASTER_CAM_INDEX,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    status = fmRegCacheWriteSingle1D ( slaveSw,
                                       &fm4000CacheGlortCam,
                                       &zero,
                                       FM6000_FIBM_CPU_CAM_INDEX,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
#endif

    return FM_OK;
} /* end fm6000FibmCreateFwdRuleSlaveSwitch */




fm_int fm6000GetMaxFibmCmdLength(fm_int addr)
{
    fm_int cmdMaxLen = FM_FIBM_HW_CMD_MAX_LEN;

    return cmdMaxLen;
} /* end fm6000GetMaxFibmCmdLength */




#if 0
/*****************************************************************************/
/** fm6000SetFibmSlaveEepromConfig
 * \ingroup intFibm
 *
 * \desc            Function to set up an FIBM configuration that will be
 *                  eventually used for an eeprom image generation.
 *
 * \param[in]       localSw is the switch number.
 *
 * \param[in]       fibmPort is port which is used for fibm communication.
 *
 * \param[in]       fibmGlort is the fibm glort for the switch. It is the glort
 *                  that allows the switch to accept and process the fibm request
 *                  messages.
 *
 * \param[in]       fibmDstGlort is the fibm destination glort for the interrupt
 *                  messages. This would be the master fibm glort, in master slave
 *                  configuration. For standalone NIC, it would the the glort of the
 *                  port that is directed connected to the NIC.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000SetFibmSlaveEepromConfig(fm_int localSw,
                                         fm_int fibmPort,
                                         fm_uint32 fibmGlort,
                                         fm_uint32 fibmDstGlort)
{
    fm_status status;
    fm_uint32 val32;
    fm_int    physPort;
    fm_uint64 rv;
    fm_uint32 cv;
    fm_uint32 physMask;
    fm_uint   camIndex;
    fm_uint   glortIndex;
    fm_status err = FM_OK;
    fm_int    sw;
    fm_switch     *switchPtr = fmRootApi->fmSwitchStateTable[localSw];

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw=%d\n", localSw);

    FM_LOG_INFO(FM_LOG_CAT_FIBM,
        "Set FIBM eeprom config: port %d, fibmGlort 0x%04x, dstGlort 0x%04x\n",
           fibmPort, fibmGlort, fibmDstGlort);

    /* Diable the parity sweeper */
    switchPtr->paritySweeperCfg.enabled = FALSE;

    /* This is where the connection to the master switch */
    err = fmPlatformMapLogicalPortToPhysical(localSw, fibmPort, &sw, &physPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, err);

    /***************************************************
     * Enable FIBM.
     **************************************************/
    status = fmReadUINT32(localSw, FM4000_MSB_CFG, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_BIT(val32, FM4000_MSB_CFG, attachedCpu, 0);
    FM_SET_BIT(val32, FM4000_MSB_CFG, disableIbm, 0);
    FM_SET_BIT(val32, FM4000_MSB_CFG, ibmGlortEn, 0);
    FM_SET_BIT(val32, FM4000_MSB_CFG, interruptGlortEn, 1);
    status = fmWriteUINT32(localSw, FM4000_MSB_CFG, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* FIBM Source Glort port for response and interrupt frames */
    status = fmReadUINT32(localSw, FM4000_MSB_IBM_GLORT, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    /* Note this should match in the GetFibmSwitchConfig */
    val32  = fibmGlort;
    status = fmWriteUINT32(localSw, FM4000_MSB_IBM_GLORT, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* FIBM Dest Glort and Interrupt Interval */
    status = fmReadUINT32(localSw, FM4000_MSB_IBM_INT, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    val32 = (0xFFF << FM4000_MSB_IBM_INT_l_interruptInterval) |
            fibmDstGlort;
    status = fmWriteUINT32(localSw, FM4000_MSB_IBM_INT, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* FIBM Ethertype */
    status = fmReadUINT32(localSw, FM4000_MSB_INT_FRAME, &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_FIELD(val32, FM4000_MSB_INT_FRAME, etherType, FM_FIBM_ETHERTYPE);
    FM_SET_FIELD(val32, FM4000_MSB_INT_FRAME, islSysPri, FM_FIBM_SYS_PRI);
    status = fmWriteUINT32(localSw, FM4000_MSB_INT_FRAME, val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Don't drop mgmt ISL on CPU port */
    status = fmReadUINT32(localSw, FM4000_PORT_CFG_1(0), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, 0);
    status = fmWriteUINT32(localSw, FM4000_PORT_CFG_1(0), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Use F64 Tag on CPU port */
    status = fmReadUINT32(localSw, FM4000_PARSE_CFG(0), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_FIELD(val32, FM4000_PARSE_CFG, ISLTag, FM4000_ENABLE_F64_TAG);
    status = fmWriteUINT32(localSw, FM4000_PARSE_CFG(0), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Don't drop mgmt ISL on FIBM mgmt port */
    status = fmReadUINT32(localSw, FM4000_PORT_CFG_1(physPort), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, 0);
    status = fmWriteUINT32(localSw, FM4000_PORT_CFG_1(physPort), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Use F64 Tag on FIBM mgmt port */
    status = fmReadUINT32(localSw, FM4000_PARSE_CFG(physPort), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_FIELD(val32, FM4000_PARSE_CFG, ISLTag, FM4000_ENABLE_F64_TAG);
    status = fmWriteUINT32(localSw, FM4000_PARSE_CFG(physPort), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /***************************************************
     * Add a CPU entry for slave switch to
     * receive fibm messages.
     **************************************************/
    glortIndex = FM4000_FIBM_CPU_GLORT_INDEX;
    camIndex   = FM4000_FIBM_CPU_CAM_INDEX;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
        "Installing a CAM index %d glort index %d to direct "
        "FIBM request messages (glort 0x%04x) to CPU port\n",
        camIndex, glortIndex, fibmGlort);

    /* Write into the RAM first. */
    rv = 0;

    /* strict */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       Strict,
                       FM_GLORT_ENTRY_TYPE_ISL );

    /* destIndex */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestIndex,
                       glortIndex );

    /* rangeSubIndexA */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexA,
                       0 );

    /* rangeSubIndexB */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexB,
                       0 );

    /* destCount */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestCount,
                       1 );

    /* hashRotation */
    FM_ARRAY_SET_BIT((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       HashRotation,
                       FM_GLORT_ENTRY_HASH_A );

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortRam,
                                       (fm_uint32 *)&rv,
                                       camIndex,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Now write into the CAM. */
    cv = (fibmGlort & 0xffff) |   /* key */
         ( (0xFFFF & 0xffff) << 16 );                 /* mask */

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortCam,
                                       &cv,
                                       camIndex,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Now write destination glort table. */
    physMask = (1 << 0); /* CPU port */
    rv = 0;
    FM_ARRAY_SET_FIELD ((fm_uint32 *)&rv,
                        FM4000_GLORT_DEST_TABLE,
                        DestMask,
                        physMask );

    status   = fmRegCacheWriteSingle1D ( localSw,
                                         &fm4000CacheGlortDestTable,
                                         (fm_uint32 *)&rv,
                                         glortIndex,
                                         FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /***************************************************
     *  Add an entry for sending back to
     *  the master switch. This entry will be
     *  removed after the master switch
     *  has initialize the CPU entry in its
     *  normal processing path
     **************************************************/
    glortIndex = FM4000_FIBM_MASTER_GLORT_INDEX;
    camIndex   = FM4000_FIBM_MASTER_CAM_INDEX;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
        "Installing a CAM index %d glort index %d to direct FIBM "
        "response and interrupt messages (glort 0x%04x) to hw port %d\n",
        camIndex, glortIndex, fibmDstGlort, physPort);

    /* Write into the RAM first. */
    rv = 0;

    /* strict */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       Strict,
                       FM_GLORT_ENTRY_TYPE_ISL );

    /* destIndex */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestIndex,
                       glortIndex );

    /* rangeSubIndexA */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexA,
                       0 );

    /* rangeSubIndexB */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexB,
                       0 );

    /* destCount */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestCount,
                       1 );

    /* hashRotation */
    FM_ARRAY_SET_BIT((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       HashRotation,
                       FM_GLORT_ENTRY_HASH_A );

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortRam,
                                       (fm_uint32 *)&rv,
                                       camIndex,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Now write into the CAM. */
    cv = (fibmDstGlort & 0xFFFF) |  /* key */
         ( (0xFFFF) << 16 );     /* mask - exactly match */

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortCam,
                                       &cv,
                                       camIndex,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Now Write destination glort table. */
    physMask = (1 << physPort); /* Which port is connected to the control switch */
    rv = 0;
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_DEST_TABLE,
                       DestMask,
                       physMask );

    status   = fmRegCacheWriteSingle1D ( localSw,
                                         &fm4000CacheGlortDestTable,
                                         (fm_uint32 *)&rv,
                                         glortIndex,
                                         FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, err);
} /* fm6000SetFibmSlaveEepromConfig */




/*****************************************************************************/
/** fm6000SetFibmSlaveEepromPath
 * \ingroup intFibm
 *
 * \desc            Function to set up an FIBM configuration that will be
 *                  eventually used for an eeprom image generation.
 *
 * \param[in]       localSw is the switch number.
 *
 * \param[in]       offset
 *
 * \param[in]       fibmGlort
 *
 * \param[in]       forwardingPort
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000SetFibmSlaveEepromPath(fm_int localSw,
                                       fm_int offset,
                                       fm_uint32 fibmGlort,
                                       fm_int forwardingPort)
{
    fm_status status;
    fm_uint32 val32;
    fm_int    physPort;
    fm_uint64 rv;
    fm_uint32 cv;
    fm_uint32 physMask;
    fm_uint   camIndex;
    fm_uint   glortIndex;
    fm_status err = FM_OK;
    fm_int    sw;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw=%d\n", localSw);

    FM_LOG_INFO(FM_LOG_CAT_FIBM,
                "Set FIBM eeprom path: fibmGlort 0x%04x --> port %d\n",
                fibmGlort, forwardingPort);

    /* This is where the connection to the master switch */
    err = fmPlatformMapLogicalPortToPhysical(localSw, forwardingPort, &sw, &physPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, err);
    glortIndex = FM4000_FIBM_MASTER_GLORT_INDEX + offset + 1;
    camIndex   = FM4000_FIBM_MASTER_CAM_INDEX - offset - 1;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH_FM4000,
        "Installing a CAM index %d glort index %d to direct "
        "FIBM request messages (glort 0x%04x) to port %d\n",
        camIndex, glortIndex, fibmGlort, forwardingPort);

    /* Write into the RAM first. */
    rv = 0;

    /* strict */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       Strict,
                       FM_GLORT_ENTRY_TYPE_ISL );

    /* destIndex */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestIndex,
                       glortIndex );

    /* rangeSubIndexA */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexA,
                       0 );

    /* rangeSubIndexB */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       RangeSubIndexB,
                       0 );

    /* destCount */
    FM_ARRAY_SET_FIELD((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       DestCount,
                       1 );

    /* hashRotation */
    FM_ARRAY_SET_BIT((fm_uint32 *)&rv,
                       FM4000_GLORT_RAM,
                       HashRotation,
                       FM_GLORT_ENTRY_HASH_A );

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortRam,
                                       (fm_uint32 *)&rv,
                                       camIndex,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);


    /* Now write into the CAM. */
    cv = (fibmGlort & 0xffff) |   /* key */
         ( (0xFFFF & 0xffff) << 16 );                 /* mask */

    status = fmRegCacheWriteSingle1D ( localSw,
                                       &fm4000CacheGlortCam,
                                       &cv,
                                       camIndex,
                                       FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Now write destination glort table. */
    physMask = (1 << physPort); /* forwardingPort port */
    rv = 0;
    FM_ARRAY_SET_FIELD ((fm_uint32 *)&rv,
                        FM4000_GLORT_DEST_TABLE,
                        DestMask,
                        physMask );

    status   = fmRegCacheWriteSingle1D ( localSw,
                                         &fm4000CacheGlortDestTable,
                                         (fm_uint32 *)&rv,
                                         glortIndex,
                                         FALSE );

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

    /* Don't drop mgmt ISL on forwardingPort port */
    status = fmReadUINT32(localSw, FM4000_PORT_CFG_1(physPort), &val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);
    FM_SET_BIT(val32, FM4000_PORT_CFG_1, dropMgmtISL, 0);
    status = fmWriteUINT32(localSw, FM4000_PORT_CFG_1(physPort), val32);
    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_FIBM, status);

ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, err);
} /* fm6000SetFibmSlaveEepromPath */
#endif



/*****************************************************************************/
/** fm4000FibmApplyNewConfig
 * \ingroup intFibm
 *
 * \desc            Reset FM_FIBM_FLAGS_INITED flag and re-enable the switch to 
 *                  use the new fm_fibmSwitchConfig provided by the application.
 *                  The glort table must have been previously modified
 *                  accordingly.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fm6000FibmApplyNewConfig(fm_int sw)
{
    fm_status       status;
    fm_fibmSwInt   *fibmInfo;

    FM_LOG_ENTRY(FM_LOG_CAT_FIBM, "sw = %d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    if ( !FIBM_INFO_VALID(sw) )
    {
        status = FM_ERR_UNINITIALIZED;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);
    }

    TAKE_FIBM_TXRX_LOCK(sw);

    /* Reset the INIT flag in order to be able to use the new
       fm_fibmSwitchConfig provided by the application. */
    fibmInfo = FIBM_INFO(sw);
    fibmInfo->flags &= ~FM_FIBM_FLAGS_INITED;

    DROP_FIBM_TXRX_LOCK(sw);

    /* Enable the switch to use the new config. */
    status = fm6000FibmEnableSlaveSwitch(sw, TRUE);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_FIBM, status);

ABORT:
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_FIBM, status);

} /* end fm6000FibmApplyNewConfig */
