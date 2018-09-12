/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_port.c
 * Creation Date:   2005
 * Description:     Contains functions dealing with the state of individual
 *                  ports
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
#define ROUND_TO_UINT(x)  ( (fm_uint) ( (x) + 0.5 ) )


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
/** fmInitPort
 * \ingroup intPort
 *
 * \desc            Set the state of a port.
 *
 * \param[in]       sw is the switch number the port belongs to
 *
 * \param[in]       portPtr is a pointer to the allocated port state structure
 *                  to be initialized.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitPort(fm_int sw, fm_port *portPtr)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_PORT, 
                 "sw=%d portPtr=%p (port %d)\n",
                 sw, 
                 (void *) portPtr, 
                 portPtr ? portPtr->portNumber : -1);

    if (!portPtr)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PORT, FM_ERR_INVALID_ARGUMENT);
    }

    portPtr->mode                           = FM_PORT_STATE_ADMIN_PWRDOWN;
    portPtr->submode                        = 0;
    portPtr->linkUp                         = FALSE;
    portPtr->lagIndex                       = -1;
    portPtr->memberIndex                    = -1;
    portPtr->linkStateChangePending         = FALSE;
    portPtr->pendingLinkStateValue          = 0;
    portPtr->linkStateChangeExpiration.sec  = 0;
    portPtr->linkStateChangeExpiration.usec = 0;
    portPtr->portSecurityEnabled            = FM_PORT_SECURITY_OFF;
    portPtr->aclAttached                    = FALSE;
    portPtr->swagPort                       = -1;
#if FM_SUPPORT_SWAG
    portPtr->swagLinkType                   = FM_SWAG_LINK_UNDEFINED;
#endif

    memset(&portPtr->phyInfo, 0, sizeof(fmPhyInterfaceTable));

    /* Initialize multicast group membership list */
    fmTreeInit(&portPtr->mcastGroupList);

    if (portPtr->portType == FM_PORT_TYPE_CPU)
    {
        portPtr->linkUp = TRUE;
    }

    /***************************************************
     * Call the port specific initialization.
     **************************************************/
    FM_API_CALL_FAMILY(err, portPtr->InitPort, sw, portPtr);

    FM_LOG_EXIT(FM_LOG_CAT_PORT, err);

}   /* end fmInitPort */




/*****************************************************************************/
/** fmSetPortState
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set the state of a port. For devices that provide more
 *                  than one MAC per port, this function operates on the
 *                  active port as selected with the ''FM_PORT_SELECT_ACTIVE_MAC''
 *                  port attribute. To control a specific MAC for a port,
 *                  use ''fmSetPortStateV2''.
 *
 * \note            When setting a port to the down state, it may be desirable
 *                  to purge any MA Table entries learned on that port. It is 
 *                  the caller's responsibility to call ''fmFlushPortAddresses''
 *                  to effect the purge.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port on which to operate. May
 *                  not be the CPU interface port.
 *
 * \param[in]       mode indicates the port state (see 'Port States') to set.
 *
 * \param[in]       subMode is the port submode (see 'Port State Submodes')
 *                  and is used only if mode is ''FM_PORT_STATE_BIST''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 *
 *****************************************************************************/
fm_status fmSetPortState(fm_int sw, fm_int port, fm_int mode, fm_int subMode)
{
    fm_status err;

    err = fmSetPortStateV2(sw, port, FM_PORT_ACTIVE_MAC, mode, subMode);
    
    return err;

}   /* end fmSetPortState */




/*****************************************************************************/
/** fmSetPortStateV2
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set the state of a port. For devices that provide more
 *                  than one MAC per port, this function operates on the
 *                  specified mac.
 *
 * \note            When setting a port to the down state, it may be desirable
 *                  to purge any MA Table entries learned on that port. It is 
 *                  the caller's responsibility to call ''fmFlushPortAddresses''
 *                  to effect the purge.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port on which to operate. May not be the CPU 
 *                  interface port.
 *
 * \param[in]       mac is the port's MAC on which to operate. May be specified
 *                  as FM_PORT_ACTIVE_MAC to operate on the MAC currently
 *                  selected as the active MAC with the 
 *                  ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute. Must be
 *                  specified as FM_PORT_ACTIVE_MAC if port is not a
 *                  physical port.
 *
 * \param[in]       mode indicates the port state (see 'Port States') to set.
 *
 * \param[in]       subMode is the port submode (see 'Port State Submodes')
 *                  and is used only if mode is ''FM_PORT_STATE_BIST''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 *
 *****************************************************************************/
fm_status fmSetPortStateV2(fm_int sw, 
                           fm_int port, 
                           fm_int mac, 
                           fm_int mode, 
                           fm_int subMode)
{
    fm_status err;
    fm_port * portPtr;
    fm_int    memberPort;
    fm_int    numMembers;
    fm_int    members[FM_MAX_NUM_LAG_MEMBERS];
    fm_int    cnt;

    FM_LOG_ENTRY_API(FM_LOG_CAT_PORT,
                     "sw=%d port=%d mac=%d mode=%d subMode=%d\n",
                     sw,
                     port,
                     mac,
                     mode,
                     subMode);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    VALIDATE_LOGICAL_PORT(sw, port, ALLOW_LAG);

    err = fmGetPhysicalMemberPortList(sw,
                                      port,
                                      &numMembers,
                                      members,
                                      FM_MAX_NUM_LAG_MEMBERS);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    
    /**************************************************
     * Don't allow a specific MAC to be specified if
     * this is not a physical port.
     **************************************************/
    
    if ( (mac != FM_PORT_ACTIVE_MAC) && !fmIsCardinalPort(sw, port) )
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err = FM_ERR_INVALID_PORT_MAC);
    }

    /* Do for each physical members first */
    for (cnt = 0 ; cnt < numMembers ; cnt++)
    {
        memberPort = members[cnt];

        portPtr = GET_PORT_PTR(sw, memberPort);

        FM_API_CALL_FAMILY(err, 
                           portPtr->SetPortState, 
                           sw, 
                           memberPort, 
                           mac,
                           mode, 
                           subMode);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
    }

ABORT:
    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_PORT, err);

}   /* end fmSetPortStateV2 */




/*****************************************************************************/
/** fmGetPortState
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve a port's state information. For devices that 
 *                  provide more than one MAC per port, this function operates 
 *                  on the active port as selected with the 
 *                  ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute. To retrieve
 *                  state of a specific MAC for a port, use ''fmGetPortStateV2''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port for which to retrieve state information.
 *                  May not be the CPU interface port.
 *
 * \param[out]      mode points to caller-allocated storage where this
 *                  function should place the port's mode (see ''Port States'')
 *                  as set in a prior call to ''fmSetPortState''.
 *
 * \param[out]      state points to caller-allocated storage where this
 *                  function should place the port's state (see ''Port States'').
 *
 * \param[out]      info points to a caller-allocated array of four integers
 *                  where this function will place mode-specific information,
 *                  indexed by lane number:
 *                                                                      \lb\lb
 *                          info[0] is for Lane A                           \lb
 *                          info[1] is for Lane B                           \lb
 *                          info[2] is for Lane C                           \lb
 *                          info[3] is for Lane D
 *                                                                      \lb\lb
 *                  For mode = ''FM_PORT_STATE_BIST'',
 *                  info holds the contents of the SERDES_BIST_ERR_CNT
 *                  register, being the number of errors per lane.
 *                  A value of zero indicates that the
 *                  corresponding lane is working. The counters are
 *                  reset each time this function is called when mode
 *                  is ''FM_PORT_STATE_BIST''.
 *                                                                      \lb\lb
 *                  For all other modes, info holds the port lane status
 *                  (see ''Port Lane Status'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 *
 *****************************************************************************/
fm_status fmGetPortState(fm_int  sw,
                         fm_int  port,
                         fm_int *mode,
                         fm_int *state,
                         fm_int *info)
{
    fm_status err;

    err = fmGetPortStateV2(sw, port, FM_PORT_ACTIVE_MAC, mode, state, info);
    
    return err;
    
}   /* end fmGetPortState */




/*****************************************************************************/
/** fmGetPortStateV2
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve a port's state information. For devices that 
 *                  provide more than one MAC per port, this function 
 *                  operates on the specified mac.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port for which to retrieve state information.
 *                  May not be the CPU interface port.
 *
 * \param[in]       mac is the port's MAC on which to operate. May be specified
 *                  as FM_PORT_ACTIVE_MAC to operate on the MAC currently
 *                  selected as the active MAC with the 
 *                  ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute. Must be
 *                  specified as FM_PORT_ACTIVE_MAC if port is not a local
 *                  physical port.
 *
 * \param[out]      mode points to caller-allocated storage where this
 *                  function should place the port's mode (see ''Port States'')
 *                  as set in a prior call to ''fmSetPortState''.
 *
 * \param[out]      state points to caller-allocated storage where this
 *                  function should place the port's state (see ''Port States'').
 *
 * \param[out]      info points to a caller-allocated array of four integers
 *                  where this function will place mode-specific information,
 *                  indexed by lane number:
 *                                                                      \lb\lb
 *                          info[0] is for Lane A                           \lb
 *                          info[1] is for Lane B                           \lb
 *                          info[2] is for Lane C                           \lb
 *                          info[3] is for Lane D
 *                                                                      \lb\lb
 *                  For mode = ''FM_PORT_STATE_BIST'',
 *                  info holds the contents of the SERDES_BIST_ERR_CNT
 *                  register, being the number of errors per lane.
 *                  A value of zero indicates that the
 *                  corresponding lane is working. The counters are
 *                  reset each time this function is called when mode
 *                  is ''FM_PORT_STATE_BIST''.
 *                                                                      \lb\lb
 *                  For all other modes, info holds the port lane status
 *                  (see ''Port Lane Status'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 *
 *****************************************************************************/
fm_status fmGetPortStateV2(fm_int  sw,
                           fm_int  port,
                           fm_int  mac,
                           fm_int *mode,
                           fm_int *state,
                           fm_int *info)
{
    fm_status err;
    fm_port * portPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_PORT,
                     "sw=%d port=%d mac=%d mode=%p state=%p info=%p\n",
                     sw,
                     port,
                     mac,
                     (void *) mode,
                     (void *) state,
                     (void *) info);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    VALIDATE_LOGICAL_PORT(sw, port, ALLOW_REMOTE);

    portPtr = GET_PORT_PTR(sw, port);

    if (portPtr->portType == FM_PORT_TYPE_REMOTE)
    {
        *mode  = portPtr->mode;
        *state = (portPtr->mode==FM_PORT_STATE_UP)?
                  FM_PORT_STATE_UP:FM_PORT_STATE_DOWN;
        *info = 0;
        err = FM_OK;
    }
    else
    {
        FM_API_CALL_FAMILY(err, 
                           portPtr->GetPortState, 
                           sw, 
                           port, 
                           mac, 
                           mode, 
                           state, 
                           info);
    }

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_PORT, err);

}   /* end fmGetPortStateV2 */




/*****************************************************************************/
/** fmSetPortAttributeInternal
 * \ingroup intPort
 *
 * \desc            Set a port attribute, preserving an internal record of
 *                  some attributes.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port on which to operate.
 *
 * \param[in]       attr is the port attribute (see 'Port Attributes') to set.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 * \return          FM_ERR_INVALID_ATTRIB if read-only attribute.
 *
 *****************************************************************************/
fm_status fmSetPortAttributeInternal(fm_int sw,
                                     fm_int port,
                                     fm_int attr,
                                     void * value)
{
    fm_uint32    security;
    fm_bool      securityTrap;
    fm_bool      learning;
    fm_int       phys_port;
    fm_portAttr *portAttr;
    fm_port *    entry;
    fm_status    err;
    fm_bool      portAttrLockTaken = FALSE;

    VALIDATE_AND_PROTECT_SWITCH(sw);
    GET_PORT_STATE_ENTRY(sw, port, entry, &phys_port);

    portAttr = GET_PORT_ATTR(sw, port);
    
    /**************************************************
     * We use PORT_ATTR_LOCK to ensure that 
     * portSecurityState, portSecurityTrap and learning
     * are properly preserved since that lock is taken
     * through the normal path of updating these
     * attributes.
     **************************************************/
    
    switch (attr)
    {
        case FM_PORT_SECURITY:
            FM_FLAG_TAKE_PORT_ATTR_LOCK(sw);
            security           = portAttr->security;
            err                = fmSetPortAttribute(sw, port, attr, value);
            portAttr->security = security;
            break;

        case FM_PORT_SECURITY_TRAP:
            FM_FLAG_TAKE_PORT_ATTR_LOCK(sw);
            securityTrap           = portAttr->securityTrap;
            err                    = fmSetPortAttribute(sw, port, attr, value);
            portAttr->securityTrap = securityTrap;
            break;

        case FM_PORT_LEARNING:
            FM_FLAG_TAKE_PORT_ATTR_LOCK(sw);
            learning           = portAttr->learning;
            err                = fmSetPortAttribute(sw, port, attr, value);
            portAttr->learning = learning;
            break;

        default:
            err = fmSetPortAttribute(sw, port, attr, value);
            break;

    }   /* end switch (attr) */

    if (portAttrLockTaken)
    {
        FM_DROP_PORT_ATTR_LOCK(sw);
    }
    
    UNPROTECT_SWITCH(sw);
    return err;

}   /* end fmSetPortAttributeInternal */




/*****************************************************************************/
/** fmSetPortAttribute
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set a port attribute.
 *                                                                      \lb\lb
 *                  The port may be a physical port or a LAG. If a LAG, all
 *                  member physical ports of the LAG will be set with the
 *                  specified attribute. Note that new physical ports added
 *                  to the LAG afterward must have their attributes set
 *                  manually by the application. If a member port is removed
 *                  from the LAG, its attribute values will not be restored
 *                  to the values it had previously.
 *
 * \note            For FM6000 devices, this function may only be 
 *                  used to set attributes that are per-port and per-MAC for 
 *                  the active MAC of a port. To set per-MAC attributes for 
 *                  the inactive MAC, or to set per-lane attributes, use 
 *                  ''fmSetPortAttributeV2''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port number on which to operate. May 
 *                  be a LAG logical port number. May also be the CPU interface 
 *                  for some attributes. See ''Port Attributes'' for details.
 *
 * \param[in]       attr is the port attribute (see 'Port Attributes') to set.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_PORT_MAC if an attempt was made to configure
 *                  a physical attribute for a port with no active MAC.
 * \return          FM_ERR_INVALID_PORT_LANE if an attempt was made to
 *                  configure a lane attribute for a port with no active lanes.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 * \return          FM_ERR_INVALID_ATTRIB if read-only attribute.
 * \return          FM_ERR_INVALID_VALUE if value points to an invalid value
 *                  for the specified attribute.
 * \return          FM_ERR_NOT_PER_LAG_ATTRIBUTE if an attempt is made to
 *                  set a per-port attribute on a LAG logical port.
 * \return          FM_ERR_PER_LAG_ATTRIBUTE if an attempt is made to set
 *                  a per-LAG attribute on a member port.
 *
 *****************************************************************************/
fm_status fmSetPortAttribute(fm_int sw,
                             fm_int port,
                             fm_int attr,
                             void * value)
{
    fm_status err;

    err = fmSetPortAttributeV2(sw, 
                               port, 
                               FM_PORT_MAC_ALL, 
                               FM_PORT_LANE_ALL, 
                               attr, 
                               value);
    
    return err;
    
}   /* end fmSetPortAttribute */



/*****************************************************************************/
/** fmSetPortAttributeV2
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set a port attribute.
 *                                                                      \lb\lb
 *                  The port may be a physical port or a LAG. If a LAG, all
 *                  member physical ports of the LAG will be set with the
 *                  specified attribute. Note that new physical ports added
 *                  to the LAG afterward must have their attributes set
 *                  manually by the application. If a member port is removed
 *                  from the LAG, its attribute values will not be restored
 *                  to the values it had previously.
 *
 * \note            This function is an enhancement to ''fmSetPortAttribute''
 *                  needed specifically for FM6000 devices only.
 *                  These devices support multiple physical links per logical
 *                  port, though only one may carry frame traffic at a time.
 *                  The mac and lane arguments disambiguate between the
 *                  multiple physical link components. Previous Intel switch 
 *                  devices provide only a single physical link per port, so
 *                  no disambiguation is needed. This function may be
 *                  used for those devices, in which case the mac argument
 *                  must be specified as either 0 or FM_PORT_ACTIVE_MAC and
 *                  the lane argument must be specified as 0 - 3.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port number on which to operate. May 
 *                  be a LAG logical port number. May also be the CPU interface 
 *                  for some attributes. See ''Port Attributes'' for details.
 *
 * \param[in]       mac is the port's zero-based MAC number on which to operate. 
 *                  May be specified as FM_PORT_ACTIVE_MAC to operate on the 
 *                  currently selected active MAC (see the 
 *                  ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute). Must be 
 *                  specified as either FM_PORT_ACTIVE_MAC or zero for ports 
 *                  that have only one MAC (physical link connection).
 *
 * \param[in]       lane is the port's zero-based lane number on which to 
 *                  operate. May be specified as FM_PORT_LANE_NA if the
 *                  lane number is not applicable to the attribute being
 *                  set. Must be set to FM_PORT_LANE_NA (for non-per-lane
 *                  attributes) when the port's 
 *                  ''FM_PORT_ETHERNET_INTERFACE_MODE'' attribute is set to 
 *                  ''FM_ETH_MODE_DISABLED'' on the MAC specified by mac.
 *
 * \param[in]       attr is the port attribute (see 'Port Attributes') to set.
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_PORT_MAC if mac is not valid for the 
 *                  specified port, or if FM_PORT_SELECT_ACTIVE_MAC was
 *                  specified and the port has no active MAC selected.
 * \return          FM_ERR_INVALID_PORT_LANE if lane is not valid.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 * \return          FM_ERR_INVALID_ATTRIB if read-only attribute.
 * \return          FM_ERR_INVALID_VALUE if value points to an invalid value
 *                  for the specified attribute.
 * \return          FM_ERR_NOT_PER_LAG_ATTRIBUTE if an attempt is made to
 *                  set a per-port attribute on a LAG logical port.
 * \return          FM_ERR_PER_LAG_ATTRIBUTE if an attempt is made to set
 *                  a per-LAG attribute on a member port.
 *
 *****************************************************************************/
fm_status fmSetPortAttributeV2(fm_int sw,
                               fm_int port,
                               fm_int mac,
                               fm_int lane,
                               fm_int attr,
                               void * value)
{
    fm_status  err;
    fm_port *  portPtr;
    fm_int     allowMode;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_PORT,
                     "sw=%d port=%d mac=%d lane=%d attr=%d value=%p\n",
                     sw,
                     port,
                     mac,
                     lane,
                     attr,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    switchPtr = GET_SWITCH_PTR(sw);

    /* Some attributes can apply to the CPU interface port. */
    switch (attr)
    {
        case FM_PORT_LEARNING:
        case FM_PORT_DEF_VLAN:
        case FM_PORT_DEF_PRI:
        case FM_PORT_DEF_DSCP:
        case FM_PORT_ROUTABLE:
        case FM_PORT_MAX_FRAME_SIZE:
        case FM_PORT_MIN_FRAME_SIZE:
        case FM_PORT_UPDATE_ROUTED_FRAME:
        case FM_PORT_UPDATE_TTL:
        case FM_PORT_UPDATE_DSCP:
        case FM_PORT_MASK:
        case FM_PORT_MASK_WIDE:
        case FM_PORT_DEF_CFI:
        case FM_PORT_REPLACE_DSCP:
        case FM_PORT_DEF_SWPRI:
        case FM_PORT_PARSER:
        case FM_PORT_PARSER_FLAG_OPTIONS:
        case FM_PORT_LOG_ARP:
        case FM_PORT_TRAP_ARP:
        case FM_PORT_CHECK_LEARNING_VID2:
        case FM_PORT_SWPRI_SOURCE:
        case FM_PORT_TRAP_IEEE_8021X:
        case FM_PORT_TRAP_IEEE_BPDU:
        case FM_PORT_TRAP_IEEE_GARP:
        case FM_PORT_TRAP_IEEE_LACP:
        case FM_PORT_TRAP_IEEE_OTHER:
            allowMode = ALLOW_CPU;
            break;

        default:
            allowMode = DISALLOW_CPU;
            break;

    }   /* end switch (attr) */

    if ( (allowMode == DISALLOW_CPU) &&
         (switchPtr->IsCpuAttribute != NULL) )
    {
        if (switchPtr->IsCpuAttribute(sw, attr))
        {
            allowMode = ALLOW_CPU;
        }
    }

    VALIDATE_LOGICAL_PORT(sw, port, ALLOW_LAG | allowMode);

    portPtr = GET_PORT_PTR(sw, port);

    /* Apply the attribute to the port (can be a LAG logical port) */
    FM_API_CALL_FAMILY(err,
                       portPtr->SetPortAttribute,
                       sw,
                       port,
                       mac,
                       lane,
                       attr,
                       value);

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_PORT, err);

}   /* end fmSetPortAttributeV2 */




/*****************************************************************************/
/** fmGetPortAttribute
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Get a port attribute.
 *
 * \note            For FM6000 devices, this function may only be 
 *                  used to get attributes that are per-port and per-MAC for 
 *                  the active MAC of a port. To get per-MAC attributes for 
 *                  the inactive MAC, or to get per-lane attributes, use 
 *                  ''fmGetPortAttributeV2''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port number on which to operate. May 
 *                  be a LAG logical port number. May also be the CPU interface 
 *                  for some attributes. See ''Port Attributes'' for details.
 *
 * \param[in]       attr is the port attribute (see 'Port Attributes') to get.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_PORT_MAC if an attempt was made to configure
 *                  a physical attribute for a port with no active MAC.
 * \return          FM_ERR_INVALID_PORT_LANE if an attempt was made to
 *                  configure a lane attribute for a port with no active lanes.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 *
 *****************************************************************************/
fm_status fmGetPortAttribute(fm_int sw,
                             fm_int port,
                             fm_int attr,
                             void * value)
{
    fm_status err;
    
    err = fmGetPortAttributeV2(sw,
                               port,
                               FM_PORT_ACTIVE_MAC, 
                               FM_PORT_LANE_NA, 
                               attr,
                               value);
    
    return err;
    
}   /* end fmGetPortAttribute */




/*****************************************************************************/
/** fmGetPortAttributeV2
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Get a port attribute.
 *
 * \note            This function is an enhancement to ''fmGetPortAttribute''
 *                  needed specifically for FM6000 devices only.
 *                  These devices support multiple physical links per logical
 *                  port, though only one may carry frame traffic at a time.
 *                  The mac and lane arguments disambiguate between the
 *                  multiple physical link components. Previous Intel switch 
 *                  devices provide only a single physical link per port, so
 *                  no disambiguation is needed. This function may be
 *                  used for those devices, in which case the mac argument
 *                  must be specified as either 0 or FM_PORT_ACTIVE_MAC and
 *                  the lane argument must be specified as 0 - 3.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port number on which to operate. May 
 *                  be a LAG logical port number. May also be the CPU interface 
 *                  for some attributes. See ''Port Attributes'' for details.
 *
 * \param[in]       mac is the port's zero-based MAC number on which to operate. 
 *                  May be specified as FM_PORT_ACTIVE_MAC to operate on the 
 *                  currently selected active MAC (see the 
 *                  ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute). Must be 
 *                  specified as either FM_PORT_ACTIVE_MAC or zero for ports 
 *                  that have only one MAC (physical link connection).
 *
 * \param[in]       lane is the port's zero-based lane number on which to 
 *                  operate. May be specified as FM_PORT_LANE_NA if the
 *                  lane number is not applicable to the attribute being
 *                  retrieved. Must be set to FM_PORT_LANE_NA (for non-per-lane
 *                  attributes) when the port's 
 *                  ''FM_PORT_ETHERNET_INTERFACE_MODE'' attribute is set to 
 *                  ''FM_ETH_MODE_DISABLED'' on the MAC specified by mac.
 *
 * \param[in]       attr is the port attribute (see 'Port Attributes') to get.
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_PORT_MAC if mac is not valid for the 
 *                  specified port, or if FM_PORT_SELECT_ACTIVE_MAC was
 *                  specified and the port has no active MAC selected.
 * \return          FM_ERR_INVALID_PORT_LANE if lane is not valid.
 * \return          FM_ERR_INVALID_ARGUMENT if unrecognized attribute.
 *
 *****************************************************************************/
fm_status fmGetPortAttributeV2(fm_int sw,
                               fm_int port,
                               fm_int mac,
                               fm_int lane,
                               fm_int attr,
                               void * value)
{
    fm_status  err;
    fm_port *  portPtr;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_PORT,
                     "sw=%d port=%d mac=%d lane=%d attr=%d value=%p\n",
                     sw,
                     port,
                     mac,
                     lane,
                     attr,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    VALIDATE_LOGICAL_PORT(sw, port, ALLOW_CPU|ALLOW_LAG);

    switchPtr = GET_SWITCH_PTR(sw);
    portPtr   = GET_PORT_PTR(sw, port);

    if (portPtr->portType == FM_PORT_TYPE_LAG)
    {
        if (!switchPtr->perLagMgmt)
        {
            /* All the LAG member ports should have the same properties,
             * so we can just return the attribute from one of the
             * member ports. */
            err = fmGetFirstPhysicalMemberPort(sw, port, &port);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);
        }
    }

    FM_API_CALL_FAMILY(err, 
                       portPtr->GetPortAttribute, 
                       sw, 
                       port, 
                       mac,
                       lane,
                       attr, 
                       value);

ABORT:
    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_PORT, err);

}   /* end fmGetPortAttributeV2 */




/*****************************************************************************/
/** fmSetPortSecurity
 * \ingroup port
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Enable or disable port security on a port.
 *
 * \note            On FM2000 devices, this function takes into account 
 *                  whether an ACL is already active on the port and if so, 
 *                  records the port security settings, but does not alter 
 *                  the state of the switch device. This approach is necessary 
 *                  to ensure that the port security feature does not 
 *                  interfere with ACLs (ACLs have priority over port security
 *                  on FM2000 devices).
 *
 * \note            When port security is enabled by this function, trapping
 *                  of violating frames is enabled and learning on the port
 *                  is disabled. When a security violation occurs, the frame
 *                  is trapped to the CPU. The driver will identify it as a
 *                  security violation and will configure the switch device
 *                  to discard all further frames from the offending source
 *                  address so that no more frames from that address will
 *                  be trapped to the CPU. The offending source address will
 *                  be reported to the application as a security violation
 *                  event. The application may add the offending address to
 *                  the MA Table (by calling ''fmAddAddress''), which will
 *                  render it no longer a violator and subsequent frames from
 *                  that address will be forwarded normally.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port for which to enable or disable port
 *                  security.
 *
 * \param[in]       enable should be TRUE to enable port security, FALSE
 *                  to disable port security.
 *
 * \param[in]       strict should be TRUE if a security violation should
 *                  occur when a known source address is received on a port
 *                  that is not in the destination port mask for the address's
 *                  MA Table entry. If false, seeing the address on a different
 *                  port will not generate a security violation. Note that
 *                  strict has no effect if the application never adds a
 *                  violating address to the MA Table.
 *                                                                      \lb\lb
 *                  This argument is ignored if enable is FALSE.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 *
 *****************************************************************************/
fm_status fmSetPortSecurity(fm_int  sw,
                            fm_int  port,
                            fm_bool enable,
                            fm_bool strict)
{
    fm_portAttr *portAttr;
    fm_port *    portState;
    fm_int       physPort;
    fm_bool      learning;
    fm_uint32    portSecurity;
    fm_uint32    portSecurityTrap;
    fm_status    err          = FM_OK;
    fm_bool      regLockTaken = FALSE;
    fm_bool      portAttrLockTaken = FALSE;

    FM_LOG_ENTRY_API(FM_LOG_CAT_PORT,
                     "sw=%d port=%d enable=%s strict=%s\n",
                     sw,
                     port,
                     enable ? "TRUE" : "FALSE",
                     strict ? "TRUE" : "FALSE");

    VALIDATE_AND_PROTECT_SWITCH(sw);
    GET_PORT_STATE_ENTRY(sw, port, portState, &physPort);

    portAttr  = GET_PORT_ATTR(sw, port);
    
    /**************************************************
     * We use TAKE_REG_LOCK to make the configuration
     * applied atomically because ultimately we are
     * programming the hardware and this lock will be
     * taken anyway by fmSetPortAttributeInternal. 
     *  
     * We need to take the PORT_ATTR_LOCK first so we
     * don't invert with the REG lock.
     **************************************************/

    FM_FLAG_TAKE_PORT_ATTR_LOCK(sw);
    FM_FLAG_TAKE_REG_LOCK(sw);

    if (enable)
    {
        /**************************************************
         * Enabling port security on this port.
         **************************************************/

        if (strict)
        {
            portSecurity = FM_PORT_SECURITY_SHV;
        }
        else
        {
            portSecurity = FM_PORT_SECURITY_SAV;
        }

        learning         = FM_DISABLED;
        portSecurityTrap = FM_SECURITY_SEND2CPU;

        portState->portSecurityEnabled = portSecurity;
    }
    else
    {
        /**************************************************
         * Disabling port security on this port.
         **************************************************/

        portSecurity     = portAttr->security;
        learning         = portAttr->learning;
        portSecurityTrap = portAttr->securityTrap;

        portState->portSecurityEnabled = FM_PORT_SECURITY_OFF;

    }   /* end if (enable) */

    /**************************************************
     * ACLs take precedence over port security.
     **************************************************/

    if ( !( fmRootApi->fmSwitchStateTable[sw]->aclInfo.enabled && 
            portState->aclAttached ) )
    {
        /**************************************************
         * Set up the switch.
         **************************************************/

        err = fmSetPortAttributeInternal(sw,
                                         port,
                                         FM_PORT_SECURITY,
                                         &portSecurity);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);


        err = fmSetPortAttributeInternal(sw,
                                         port,
                                         FM_PORT_SECURITY_TRAP,
                                         &portSecurityTrap);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

        err = fmSetPortAttributeInternal(sw,
                                         port,
                                         FM_PORT_LEARNING,
                                         &learning);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PORT, err);

        /**************************************************
         * Now flush the MA Table for entries on this
         * port.
         *
         * We need to drop both LOCKs first so we don't
         * invert with the MA Table maintenance lock.
         **************************************************/

        FM_FLAG_DROP_REG_LOCK(sw);
        FM_FLAG_DROP_PORT_ATTR_LOCK(sw);

        err = fmFlushPortAddresses(sw, port);
        
        if (err == FM_ERR_UNSUPPORTED)
        {
            err = FM_OK;
        }

    }   /* end if( !(fmRootApi->fmSwitchStateTable[sw]->aclInfo.enabled... ) */

ABORT:
    if (regLockTaken)
    {
        DROP_REG_LOCK(sw);
    }

    if (portAttrLockTaken)
    {
        FM_DROP_PORT_ATTR_LOCK(sw);
    }
    
    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_PORT, err);

}   /* end fmSetPortSecurity */




/*****************************************************************************/
/** fmUpdateSwitchPortMasks
 * \ingroup intPort
 *
 * \desc            Update switch port masks based upon port link states.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmUpdateSwitchPortMasks(fm_int sw)
{
    fm_int     cpi;
    fm_int     port;
    fm_switch *swstate;
    fm_status  err = FM_OK;
    fm_port *  portPtr;

    swstate = GET_SWITCH_PTR(sw);

    /**************************************************
     * Capture the PORT_ATTR lock for the exclusive 
     * access to portPtr->portMask and REG lock for the 
     * exclusive access to switch register 
     * (i.e. PORT_CFG_2).
     **************************************************/

    FM_TAKE_PORT_ATTR_LOCK(sw);
    TAKE_REG_LOCK(sw);

    for (cpi = 0 ; cpi < swstate->numCardinalPorts ; cpi++)
    {
        port    = GET_LOGICAL_PORT(sw, cpi);
        portPtr = GET_PORT_PTR(sw, port);

        /***************************************************
         * Note that this causes port changes to all ports
         * but it is necessary: downed ports need their
         * port masks reset (provided by the internal
         * implementation of FM_PORT_MASK) and up ports
         * need their port masks filtered.  Missing these
         * cases can cause backpressure inside the
         * switch fabric via sending to ports that have
         * link down.
         **************************************************/
        if (portPtr->UpdatePortMask)
        {
            err = portPtr->UpdatePortMask(sw, port);
            if (err != FM_OK)
            {
                FM_LOG_WARNING(FM_LOG_CAT_PORT,
                               "fmUpdateSwitchPortMasks unable to set "
                               "port mask for port %d: %s\n",
                               port,
                               fmErrorMsg(err) );
            }
        }
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PORT,
                         "UpdatePortMask not defined for port %d\n",
                         port);
        }
    }

    DROP_REG_LOCK(sw);
    FM_DROP_PORT_ATTR_LOCK(sw);

    return err;

}   /* end fmUpdateSwitchPortMasks */




/*****************************************************************************/
/** fmSetFaultState
 * \ingroup intPort
 *
 * \desc            Enable or disable fault state transmission on a port.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port for which to enable or disable fault
 *                  state transmission.
 *
 * \param[in]       enable should be TRUE to enable fault generation,
 *                  FALSE to disable it.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 *
 *****************************************************************************/
fm_status fmSetFaultState(fm_int sw, fm_int port, fm_bool enable)
{
    fm_port * portState;
    fm_status err;

    portState = GET_PORT_PTR(sw, port);

    FM_API_CALL_FAMILY(err, portState->SetFaultState, sw, port, enable);

    return err;

}   /* end fmSetFaultState */




/*****************************************************************************/
/** fmCheckFaultStates
 * \ingroup intPort
 *
 * \desc            Check ports that are in a fault state to ensure that
 *                  no frames are backed up to them - try to flush the
 *                  frames if there are any.
 * Note: Assumes switch lock has already been taken.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *****************************************************************************/
fm_status fmCheckFaultStates(fm_int sw)
{
    fm_switch *swstate;
    fm_status  err;

    swstate = GET_SWITCH_PTR(sw);

    err = swstate->CheckFaultStates(sw);

    return err;

}   /* end fmCheckFaultStates */




/*****************************************************************************/
/** fmGetCpuPort
 * \ingroup intPort
 *
 * \desc            Returns the logical port number for the CPU port for
 *                  the specified switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      cpuPort contains the logical port number for the CPU port.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *****************************************************************************/
fm_status fmGetCpuPort(fm_int sw, fm_int *cpuPort)
{
    fm_status  err;
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->GetCpuPort != NULL)
    {
        err = switchPtr->GetCpuPort(sw, cpuPort);
    }
    else
    {
        *cpuPort = 0;
        err      = FM_OK;
    }

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmGetCpuPort */




/*****************************************************************************/
/** fmSetCpuPort
 * \ingroup intPort
 *
 * \desc            Set the logical port number for the CPU port for
 *                  the specified switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       cpuPort contains the logical port number to set for the CPU 
 *                  port.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *****************************************************************************/
fm_status fmSetCpuPort(fm_int sw, fm_int cpuPort)
{
    fm_status  err = FM_OK;
    fm_switch *switchPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);
    VALIDATE_LOGICAL_PORT(sw, cpuPort, ALLOW_CPU);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->SetCpuPort != NULL)
    {
        err = switchPtr->SetCpuPort(sw, cpuPort);
    }

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmSetCpuPort */




/*****************************************************************************/
/** fmDbgDumpPortAttributes
 * \ingroup diagMisc 
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Dumps the cached port attributes settings.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       port is the port for which to retrieve attribute settings.
 *                  Can be a LAG logical port.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmDbgDumpPortAttributes(fm_int sw, fm_int port)
{
    fm_switch *switchPtr;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        FM_LOG_PRINT("ERROR: invalid switch %d\n", sw);
        return;
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if ( switchPtr == NULL )
    {
        FM_LOG_PRINT("ERROR: invalid switch %d\n", sw);
        return;
    }

    if ( !fmIsValidPort(sw, port, ALLOW_ALL) )
    {
        FM_LOG_PRINT("ERROR: invalid port %d\n", port);
        return;
    }

    PROTECT_SWITCH(sw);

    FM_API_CALL_FAMILY_VOID(switchPtr->DbgDumpPortAttributes, sw, port);

    UNPROTECT_SWITCH(sw);

}   /* end fmDbgDumpPortAttributes */




/*****************************************************************************/
/** fmPrintPortAttributeValues
 * \ingroup intPort
 *
 * \desc            Utility to print a port attribute values.
 * 
 * \param[in]       attrType is the attribute type.
 *
 * \param[in]       attrName is pointer to the attribute name in text.
 *
 * \param[in]       perLag indicate if it is a per-lag attribute.
 *
 * \param[in]       cachedValue is the attribute cached value.
 *
 * \param[in]       value is the attribute value currently in use.
 *
 * \param[in]       lagValue is the lag attribute value.
 *
 * \return          None.
 *
 *****************************************************************************/
void fmPrintPortAttributeValues(fm_int   attrType,
                                fm_char *attrName,
                                fm_bool  perLag,
                                void *   cachedValue,
                                void *   value,
                                void *   lagValue)
{
    fm_status   err;
    fm_char     buf[200];
    fm_char     buf2[30];
    fm_char     buf3[30];
    fm_char     buf4[30];
    fm_char *   str=NULL;
    fm_char *   str2=NULL;
    fm_char *   formatText      = "%-34s: %-24s %-24d %-24s\n";
    fm_char *   formatInt       = "%-34s: %-24d %-24d %-24d\n";
    fm_char *   formatBool      = "%-34s: %-24s %-24s %-24s\n";
    fm_char *   format64        = "%-34s: 0x%-22llx 0x%-22llx 0x%-22llx\n";
    fm_char *   format64T       = "%-34s: %-24s 0x%-22llx %-24s\n";
    fm_char *   formatPortMask  = "%-34s: %-24s %-24s %-24s\n";
    fm_char *   formatPPT       = "%-34s: PC %-21d Time %-24d\n";
    fm_int      i;
    fm_portmask portMask;

    /* Having lagValue == null indicates that the port is not in lag. */
    if (lagValue == NULL)
    {     
        str = "Not in lag";
    }
    else
    {
        str = "N/A";
    }

    str2 = "N/A";

    switch (attrType)
    {
        case FM_TYPE_INT:
            if (lagValue != NULL && perLag)
            {
                FM_SPRINTF_S(buf,
                             sizeof(buf),
                             formatInt,
                             attrName, 
                             *( (fm_int *) cachedValue ),
                             *( (fm_int *) value ),
                             *( (fm_int *) lagValue ));
            }
            else
            {
                FM_SPRINTF_S(buf,
                             sizeof(buf),
                             formatText,
                             attrName, 
                             str2,
                             *( (fm_int *) value ),
                             str);
            }
            break;

        case FM_TYPE_UINT32:
            if (lagValue != NULL && perLag)
            {
                FM_SPRINTF_S(buf,
                             sizeof(buf),
                             formatInt,
                             attrName, 
                             *( (fm_uint32 *) cachedValue ),
                             *( (fm_uint32 *) value ),
                             *( (fm_uint32 *) lagValue ));
            }
            else
            {
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             formatText,
                             attrName, 
                             str2,
                             *( (fm_uint32 *) value ),
                             str);
            }
            break;

        case FM_TYPE_UINT64:
        case FM_TYPE_MACADDR:
            if (lagValue != NULL && perLag)
            {
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             format64,
                             attrName, 
                             *( (fm_uint64 *) cachedValue ),
                             *( (fm_uint64 *) value ),
                             *( (fm_uint64 *) lagValue ));
            }
            else
            {
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             format64T,
                             attrName, 
                             str2,
                             *( (fm_uint64 *) value ),
                             str);
            }
            break;

        case FM_TYPE_BOOL:
            if (lagValue != NULL && perLag)
            {
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             formatBool,
                             attrName, 
                             (*( (fm_bool *) cachedValue)) ? "TRUE" : "FALSE",
                             (*( (fm_bool *) value ))      ? "TRUE" : "FALSE",
                             (*( (fm_bool *) lagValue ))   ? "TRUE" : "FALSE");
            }
            else
            {
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             formatBool,
                             attrName, 
                             str2,
                             (*( (fm_bool *) value ))      ? "TRUE" : "FALSE",
                             str);
            }
           break;

        case FM_TYPE_PORTMASK:
            /* Convert bitArray to portmask*/
            err = fmBitArrayToPortMask((fm_bitArray *) value,
                                       &portMask,
                                       ((fm_bitArray *) value)->bitCount);
            if (err)
            {
                FM_LOG_ERROR(FM_LOG_CAT_PORT, 
                             "Error while trying to convert bit array to port "
                             "mask");
            }

            memset(buf2, 0, sizeof(buf2));
            for ( i = 0; i < FM_PORTMASK_NUM_WORDS; i++)
            {
                FM_SPRINTF_S(buf2, 
                             sizeof(buf2), 
                             "%s%08x", 
                             buf2, 
                             portMask.maskWord[FM_PORTMASK_NUM_WORDS - 1 - i]);
            }

            if (lagValue != NULL && perLag)
            {
                memset(buf3, 0, sizeof(buf3));
                for ( i = 0; i < FM_PORTMASK_NUM_WORDS; i++)
                {
                    FM_SPRINTF_S(buf3, 
                                 sizeof(buf3), 
                                 "%s%08x", 
                                 buf3,
                                 (*((fm_portmask *) cachedValue)).maskWord[FM_PORTMASK_NUM_WORDS - 1 - i]);
                }

                memset(buf4, 0, sizeof(buf4));
                for ( i = 0; i < FM_PORTMASK_NUM_WORDS; i++)
                {
                    FM_SPRINTF_S(buf4, 
                                 sizeof(buf4), 
                                 "%s%08x", 
                                 buf4,
                                 (*((fm_portmask *) lagValue)).maskWord[FM_PORTMASK_NUM_WORDS - 1 - i]);
                }
                
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             formatPortMask,
                             attrName, 
                             buf3,
                             buf2,
                             buf4);
            }
            else
            {
                FM_SPRINTF_S(buf, 
                             sizeof(buf),
                             formatPortMask,
                             attrName, 
                             str2,
                             buf2,
                             str);
            }
            break;

        case FM_TYPE_PAUSE_PACING_TIME:
            FM_SPRINTF_S(buf, 
                         sizeof(buf),
                         formatPPT,
                         attrName, 
                         ( (fm_pausePacingTime *) value)->pauseClass,
                         ( (fm_pausePacingTime *) value)->timeNs);
            break;

        default:
            snprintf(buf, sizeof(buf), "Unknown variable type\n");
            break;
    }

    FM_LOG_PRINT("%s", buf);
    
}   /* end fmPrintPortAttributeValues */




/*****************************************************************************/
/** fmComparePortAttributes
 * \ingroup intPort
 *
 * \desc            Compares two attributes and returns 0 if they are equal.
 * 
 * \param[in]       attrType type of attribute on which to operate.
 *
 * \param[in]       attr1 first attribute on which to operate.
 *
 * \param[in]       attr2 second attribute  on which to operate.
 *
 * \return          0 if attribute values are equal.
 * \return          1 otherwise
 *
 *****************************************************************************/
fm_int fmComparePortAttributes(fm_int attrType, void *attr1, void *attr2)
{
    fm_int retVal;

    switch (attrType)
    {
        case FM_TYPE_INT:
            retVal = (*( (fm_int *)attr1) == *( (fm_int *)attr2)) ? 0 : 1;
            break;

        case FM_TYPE_UINT32:
            retVal = (*( (fm_uint32 *)attr1) == *( (fm_uint32 *)attr2)) ? 0 : 1;
            break;

        case FM_TYPE_UINT64:
            retVal = (*( (fm_uint64 *)attr1) == *( (fm_uint64 *)attr2)) ? 0 : 1;
            break;

        case FM_TYPE_BOOL:
            retVal = (*( (fm_bool *)attr1) == *( (fm_bool *)attr2)) ? 0 : 1;
           break;

        case FM_TYPE_MACADDR:
            retVal = (*( (fm_macaddr *)attr1) == *( (fm_macaddr *)attr2)) ? 0:1;
            break;

        case FM_TYPE_PORTMASK:
            retVal = (memcmp(attr1, attr2, sizeof(fm_portmask)) == 0) ? 0 : 1;
            break;

        case FM_TYPE_PAUSE_PACING_TIME:
            retVal = ( (( (fm_pausePacingTime *)attr1)->pauseClass == 
                        ( (fm_pausePacingTime *)attr2)->pauseClass) &&
                       (( (fm_pausePacingTime *)attr1)->pauseClass == 
                        ( (fm_pausePacingTime *)attr2)->pauseClass) ) ? 
                     0 : 1;
            break;

        default:
            retVal = 1;
            break;
    }

    return retVal;

}   /* end fmComparePortAttributes */




/*****************************************************************************/
/** fmIsPerLagPortAttribute
 * \ingroup intPort
 *
 * \desc            Indicate whether the given attribute is a per-lag port
 *                  attribute (i.e. can be set on a LAG logical port)
 * 
 * \param[in]       sw is the switch number to operate on.
 * 
 * \param[in]       attr is the attribute on which to operate.
 *
 * \return          TRUE if per-lag attribute.
 * \return          FALSE if not per-lag attribute.
 *
 *****************************************************************************/
fm_bool fmIsPerLagPortAttribute(fm_int sw, fm_uint attr)
{
    fm_switch *switchPtr;
    fm_bool    boolVal = FALSE;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        FM_LOG_PRINT("ERROR: invalid switch %d\n", sw);
        return FALSE;
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if ( switchPtr == NULL )
    {
        FM_LOG_PRINT("ERROR: invalid switch %d\n", sw);
        return FALSE;
    }

    PROTECT_SWITCH(sw);

    if (switchPtr->IsPerLagPortAttribute != NULL)
    {
        boolVal = switchPtr->IsPerLagPortAttribute(sw, attr);
    }

    UNPROTECT_SWITCH(sw);

    return(boolVal);

}   /* end fmIsPerLagPortAttribute */




/*****************************************************************************/
/** fmGetISLTagSize
 * \ingroup intPort
 *
 * \desc            Returns the number of bytes in the specified ISL tag.
 * 
 * \param[in]       islTagFormat is ISL tag format.
 *
 * \return          Number of bytes in the specified ISL tag.
 *
 *****************************************************************************/
fm_int fmGetISLTagSize(fm_islTagFormat islTagFormat)
{
    switch (islTagFormat)
    {
        case FM_ISL_TAG_F32:
        case FM_ISL_TAG_OTHER_32B:
            return 4;
            break;

        case FM_ISL_TAG_F56:
            return 7;
            break;

        case FM_ISL_TAG_F64:
        case FM_ISL_TAG_OTHER_64B:
            return 8;
            break;

        case FM_ISL_TAG_F96:
        case FM_ISL_TAG_OTHER_96B:
            return 12;
            break;

        default:
            break;

    }   /* end switch (islTagFormat) */

    return 0;

}   /* end fmGetISLTagSize */




/*****************************************************************************/
/** fmGetPortISLTagSize
 * \ingroup intPort
 *
 * \desc            Returns the number of ISL tag bytes required for the
 *                  specified port.
 * 
 * \param[in]       sw is the switch number to operate on.
 * 
 * \param[in]       port is the port for which to retrieve the number of bytes.
 *
 * \return          Number of ISL tag bytes required for the
 *                  specified port.
 *
 *****************************************************************************/
fm_int fmGetPortISLTagSize(fm_int sw, fm_int port)
{
    fm_islTagFormat     islTagFormat;

    if (fmGetPortAttributeV2(sw, 
                             port, 
                             FM_PORT_ACTIVE_MAC,
                             FM_PORT_LANE_NA,
                             FM_PORT_ISL_TAG_FORMAT, 
                             &islTagFormat) != FM_OK)
    {
        return 0;
    }

    return fmGetISLTagSize(islTagFormat);    

}   /* end fmGetPortISLTagSize */




/*****************************************************************************/
/** fmNotifyXcvrChange
 * \ingroup port
 *
 * \chips           FM6000
 *
 * \desc            This function can be used by the application to notify the
 *                  state of certain transceiver signals. That is particularly
 *                  useful when there are transceiver signal changes that
 *                  could be used by the API to trigger certain actions.
 *                  The following commonly used signals are currently 
 *                  represented by a single bit in the xcvrSignals bitmask
 *                  argument:                                              \lb
 *                  FM_PORT_XCVRSIG_MODPRES: asserted to indicate the
 *                  transceiver module is present or when the signal is not
 *                  implemented. De-asserted to indicate module is absent  \lb
 *                  FM_PORT_XCVRSIG_RXLOS: asserted to indicate a Receiver
 *                  Loss Of Signal is being reported. De-asserted to indicate
 *                  there is no Loss Of Signal or when the signal is not
 *                  implemented                                            \lb
 *                  FM_PORT_XCVRSIG_TXFAULT: asserted to indicate a Transmitter
 *                  Fault is being reported. De-asserted to indicate there is
 *                  no Fault or when the signal is not implemented
 * 
 * \note            Using this function is optional. When the application does
 *                  not use this function, the following default values are
 *                  used by the API:
 *                  FM_PORT_XCVRSIG_MODPRES: asserted                       \lb
 *                  FM_PORT_XCVRSIG_RXLOS:   de-asserted                    \lb
 *                  FM_PORT_XCVRSIG_TXFAULT: de-asserted                    \lb
 *                  Using this function allows the API to assess quicker the
 *                  current overall link state and take proper actions that
 *                  greatly improve the link bring up time. The internal values
 *                  of the supported signals are reset to their default values
 *                  every time the ethernet interface mode is changed (see
 *                  ''fm_ethMode'').
 * 
 * \note            Every time the application invokes this function, the API
 *                  assumes the correct value of each of the signals (asserted,
 *                  de-asserted, or not implemented) is represented, whether or
 *                  not there has been a transition on that signal. However, if
 *                  FM_PORT_XCVRSIG_MODPRES is de-asserted (i.e. module is
 *                  not present), all other signals are ignored.
 * 
 * \note            For multi-lane transceiver modules (i.e. QSFP) the
 *                  application is expected to invoke this function for
 *                  each lane. It's the application's responsibility to ensure
 *                  consistency of the FM_PORT_XCVRSIG_MODPRES signal across
 *                  all different notifications for the same transceiver.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the logical port number on which to operate. May 
 *                  not be a LAG logical port number nor a CPU interface.
 *
 * \param[in]       mac is the port's zero-based MAC number on which to operate. 
 *                  May be specified as FM_PORT_ACTIVE_MAC to operate on the 
 *                  currently selected active MAC (see the 
 *                  ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute). Must be 
 *                  specified as either FM_PORT_ACTIVE_MAC or zero for ports 
 *                  that have only one MAC (physical link connection). May
 *                  not be specified as FM_PORT_MAC_ALL.
 *
 * \param[in]       lane is the port's zero-based lane number on which to 
 *                  operate. May not be specified as FM_PORT_LANE_NA nor as
 *                  FM_PORT_LANE_ALL.
 * 
 * \param[in]       xcvrSignals a bitmask where each bit represents the status
 *                  of a given signal (see the ''Transceiver Signals'' 
 *                  definitions).
 * 
 * \param[in]       xcvrInfo is a pointer to a caller-allocated area containing
 *                  additional information on this transceiver. 
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_PORT if port is invalid.
 * \return          FM_ERR_INVALID_PORT_MAC if mac is not valid for the 
 *                  specified port, or if FM_PORT_SELECT_ACTIVE_MAC was
 *                  specified and the port has no active MAC selected.
 * \return          FM_ERR_INVALID_PORT_LANE if lane is not valid.
 * \return          FM_ERR_INVALID_ARGUMENT if one of the arguments is invalid
 * 
 *****************************************************************************/
fm_status fmNotifyXcvrChange( fm_int     sw, 
                              fm_int     port, 
                              fm_int     mac, 
                              fm_int     lane, 
                              fm_uint32  xcvrSignals, 
                              void      *xcvrInfo )
{
    fm_status  err;
    fm_port   *portPtr;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    /* No CPU ports or LAG ports allowed */
    VALIDATE_LOGICAL_PORT( sw, port, DISALLOW_CPU );

    portPtr = GET_PORT_PTR(sw, port);

    /* Invoke the chip-specific function */
    FM_API_CALL_FAMILY( err,
                        portPtr->NotifyXcvrState,
                        sw,
                        port,
                        mac,
                        lane,
                        xcvrSignals,
                        xcvrInfo );

    UNPROTECT_SWITCH(sw);
    return err;

}   /* end fmNotifyXcvrChange */

