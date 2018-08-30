/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_swag.c
 * Creation Date:   Feb 6, 2007
 * Description:     Services for managing switch aggregates and switch
 *                  aggregate topologies.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2013 Intel Corporation. All Rights Reserved.
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


#include <fm_sdk_swag_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

#if FM_SUPPORT_SWAG

#if FM_INCLUDE_RING_TOPOLOGY == FM_ENABLED
extern fm_status fmRingTopologySolver(fm_int                  sw,
                                      fm_topologySolverEvent *event);


#endif

#if FM_INCLUDE_FAT_TREE_TOPOLOGY == FM_ENABLED
extern fm_status fmFatTreeTopologySolver(fm_int                  sw,
                                         fm_topologySolverEvent *event);


#endif

#if FM_INCLUDE_MESH_TOPOLOGY == FM_ENABLED
extern fm_status fmMeshTopologySolver(fm_int                  sw,
                                      fm_topologySolverEvent *event);


#endif

extern void fmSWAGEventHandler(fm_event *event);


#endif  /* FM_SUPPORT_SWAG */


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
/** GetDefaultTopologySolver
 * \ingroup intSwag
 *
 * \desc            Returns the default topology solver for a topology,
 *                  if that topology's default solver is known.
 *
 * \param[in]       topology is the topology for which the default solver
 *                  is desired.
 *
 * \return          Default topology solver or NULL.
 *
 *****************************************************************************/
static fm_swagTopologySolver GetDefaultTopologySolver(fm_swagTopology topology)
{
    fm_swagTopologySolver solver;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "topology = %d\n", topology);

    switch (topology)
    {
#if FM_SUPPORT_SWAG

#if FM_INCLUDE_RING_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_RING:
            solver = fmRingTopologySolver;
            break;
#endif

#if FM_INCLUDE_FAT_TREE_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_FAT_TREE:
            solver = fmFatTreeTopologySolver;
            break;
#endif

#if FM_INCLUDE_MESH_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_MESH:
            solver = fmMeshTopologySolver;
            break;
#endif

#endif

        default:
            solver = NULL;
            break;
    } /* end switch (topology) */

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_SWAG,
                       solver,
                       "solver = %p\n",
                       (void *) (fm_uintptr) solver);

}   /* end GetDefaultTopologySolver */




/*****************************************************************************/
/** SetTopology
 * \ingroup intSwag
 *
 * \desc            Sets the topology for a switch, also updating the topology
 *                  solver.
 *
 * \param[in]       sw contains the switch number.
 *
 * \param[in]       swag points to the switch aggregate (see 'fmSWAG_switch')
 *                  on which to operate.
 *
 * \param[in]       topology is the switch aggregate topology type.
 *
 * \param[in]       execute is TRUE if the shutdown and/or initialize functions
 *                  should be executed.
 *
 * \return          Status from solver.
 *
 *****************************************************************************/
static fm_status SetTopology(fm_int          sw,
                             fmSWAG_switch * swag,
                             fm_swagTopology topology,
                             fm_bool         execute)
{
    fm_status              status;
    fm_topologySolverEvent event;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, swag = %p, topology = %d, execute = %d\n",
                 sw,
                 (void *) swag,
                 topology,
                 execute);

    status = FM_OK;

    if (swag->topology != topology)
    {
        if ( execute && (swag->solver != NULL) )
        {
            event.cmd = FM_SOLVER_CMD_SHUTDOWN;
            status    = swag->solver(sw, &event);

            if (status != FM_OK)
            {
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
            }
        }

        swag->topology = topology;

        swag->solver = GetDefaultTopologySolver(topology);

        if ( execute && (swag->solver != NULL) )
        {
            event.cmd = FM_SOLVER_CMD_INITIALIZE;
            status    = swag->solver(sw, &event);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end SetTopology */




/*****************************************************************************/
/** SetTopologySolver
 * \ingroup intSwag
 *
 * \desc            Sets the topology solver for a switch.
 *
 * \param[in]       sw contains the switch number.
 *
 * \param[in]       swag points to the switch aggregate (see 'fmSWAG_switch')
 *                  on which to operate.
 *
 * \param[in]       solver is the switch aggregate topology solver (or NULL).
 *
 * \param[in]       execute is TRUE if the shutdown and/or initialize functions
 *                  should be executed.
 *
 * \return          Status from solver.
 *
 *****************************************************************************/
static fm_status SetTopologySolver(fm_int                sw,
                                   fmSWAG_switch *       swag,
                                   fm_swagTopologySolver solver,
                                   fm_bool               execute)
{
    fm_status              status = FM_OK;
    fm_topologySolverEvent event;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, swag = %p, solver = %p, execute = %d\n",
                 sw,
                 (void *) swag,
                 (void *) (fm_uintptr) solver,
                 execute);

    if (swag->solver != solver)
    {
        if ( execute && (swag->solver != NULL) )
        {
            event.cmd = FM_SOLVER_CMD_SHUTDOWN;
            status    = swag->solver(sw, &event);

            if (status != FM_OK)
            {
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
            }
        }

        swag->solver = solver;

        if ( execute && (solver != NULL) )
        {
            event.cmd = FM_SOLVER_CMD_INITIALIZE;
            status    = solver(sw, &event);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end SetTopologySolver */




/*****************************************************************************/
/** PrepareLink
 * \ingroup intSwag
 *
 * \desc            Prepares a link for use.  The switch aggregate switch (but
 *                  not necessarily the underlying switch or switches) must
 *                  be in the "up" state.
 *
 * \note            This function assumes that the switch aggregate lock
 *                  has already been taken, but that underlying switch
 *                  locks have not.
 *
 * \param[in]       swagId is the ID of the switch aggregate to which the
 *                  link belongs.
 *
 * \param[in]       intLink points to an ''fm_swagIntLink'' structure that
 *                  describes the nature of the link.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if intLink is NULL.
 * \return          FM_ERR_SWITCH_NOT_UP if the switch aggregate is not in
 *                  the up state.
 * \return          FM_ERR_INVALID_PORT if link specifies a switch-aggregate
 *                  or underlying switch port that does not exist.
 * \return          FM_FAIL if a logical port was created but it's port pointer
 *                  is still NULL.
 *
 *****************************************************************************/
static fm_status PrepareLink(fm_int swagId, fm_swagIntLink *intLink)
{
    fm_status      status;
    fm_switch *    aggSwPtr;
    fm_port *      logPortPtr;
    fm_port *      partnerLogPortPtr = NULL;
    fmSWAG_port *  saLogPortPtr        = NULL;
    fmSWAG_port *  saPartnerLogPortPtr = NULL;
    fm_int         logPort;
    fm_int         newPort;
    fm_int         partnerLogPort;
    fm_int         primarySw;
    fm_int         primaryPort;
    fm_int         partnerSw;
    fm_int         partnerPort;
    fm_switch *    primarySwitchPtr       = NULL;
    fm_switch *    partnerSwitchPtr       = NULL;
    fm_port *      primaryPortPtr         = NULL;
    fm_port *      partnerPortPtr         = NULL;
    fm_bool        primarySwitchProtected = FALSE;
    fm_bool        partnerSwitchProtected = FALSE;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "swagId=%d, intLink=%p (type=%d, logicalPort=%d, swId=%d, "
                 "swPort=%d, partnerLogicalPort=%d, partnerSwitch=%d, "
                 "partnerPort=%d)\n",
                 swagId,
                 (void *) intLink,
                 (intLink != NULL) ? intLink->link.type : 99999,
                 (intLink != NULL) ? intLink->link.logicalPort : -1,
                 (intLink != NULL) ? intLink->link.swId : -1,
                 (intLink != NULL) ? intLink->link.swPort : -1,
                 (intLink != NULL) ? intLink->link.partnerLogicalPort : -1,
                 (intLink != NULL) ? intLink->link.partnerSwitch : -1,
                 (intLink != NULL) ? intLink->link.partnerPort : -1);

    if (intLink == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    logPort     = intLink->link.logicalPort;
    primarySw   = intLink->link.swId;
    primaryPort = intLink->link.swPort;

    if (intLink->link.type == FM_SWAG_LINK_INTERNAL)
    {
        partnerLogPort = intLink->link.partnerLogicalPort;
        partnerSw      = intLink->link.partnerSwitch;
        partnerPort    = intLink->link.partnerPort;
    }
    else
    {
        partnerLogPort = -1;
        partnerSw      = -1;
        partnerPort    = -1;
    }

    aggSwPtr = fmRootApi->fmSwitchStateTable[swagId];

    if ( !FM_IS_STATE_ALIVE(aggSwPtr->state) )
    {
        status = FM_ERR_SWITCH_NOT_UP;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
    }

    /* Make sure the port(s) in this link are valid */
    if ( (logPort < 0) || (logPort >= aggSwPtr->maxPort) )
    {
        status = FM_ERR_INVALID_PORT;
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
    }

    logPortPtr = aggSwPtr->portTable[logPort];

    /* Create the switch-aggregate logical port if needed */
    if (logPortPtr == NULL)
    {
        newPort = logPort;

        status = aggSwPtr->AllocLogicalPort(aggSwPtr->switchNumber,
                                            FM_PORT_TYPE_PHYSICAL,
                                            1,
                                            &newPort,
                                            0);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

        if (newPort != logPort)
        {
            status = FM_ERR_INVALID_PORT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
        }

        logPortPtr = aggSwPtr->portTable[logPort];

        if (logPortPtr == NULL)
        {
            status = FM_FAIL;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
        }
    }

    saLogPortPtr = logPortPtr->extension;

    if (partnerLogPort >= 0)
    {
        if (partnerLogPort >= aggSwPtr->maxPort)
        {
            status = FM_ERR_INVALID_PORT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
        }

        partnerLogPortPtr = aggSwPtr->portTable[partnerLogPort];

        /* Create the switch-aggregate partner logical port if needed */
        if (partnerLogPortPtr == NULL)
        {
            newPort = partnerLogPort;

            status = aggSwPtr->AllocLogicalPort(aggSwPtr->switchNumber,
                                                FM_PORT_TYPE_PHYSICAL,
                                                1,
                                                &newPort,
                                                0);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

            if (newPort != partnerLogPort)
            {
                status = FM_ERR_INVALID_PORT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
            }

            partnerLogPortPtr = aggSwPtr->portTable[partnerLogPort];

            if (partnerLogPortPtr == NULL)
            {
                status = FM_FAIL;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
            }
        }

        saPartnerLogPortPtr = partnerLogPortPtr->extension;
    }

    /* Try to protect the primary underlying switch */
    if ( SWITCH_LOCK_EXISTS(primarySw) )
    {
        PROTECT_SWITCH(primarySw);
        primarySwitchProtected = TRUE;

        primarySwitchPtr = fmRootApi->fmSwitchStateTable[primarySw];
    }

    /* Find the underlying switch/port, if the switch exists and is up. */
    if ( (primarySwitchPtr != NULL) &&
         (FM_IS_STATE_ALIVE(primarySwitchPtr->state)) )
    {
        if ( (primaryPort < 0) || (primaryPort >= primarySwitchPtr->maxPort) )
        {
            status = FM_ERR_INVALID_PORT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
        }

        primaryPortPtr = primarySwitchPtr->portTable[primaryPort];

        if (primaryPortPtr == NULL)
        {
            status = FM_ERR_INVALID_PORT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
        }
    }

    if (partnerSw >= 0)
    {
        /* Try to protect the partner underlying switch */
        if ( SWITCH_LOCK_EXISTS(partnerSw) )
        {
            PROTECT_SWITCH(partnerSw);
            partnerSwitchProtected = TRUE;

            partnerSwitchPtr = fmRootApi->fmSwitchStateTable[partnerSw];
        }

        /* Find the underlying partner switch/port, if the switch exists
         * and is up
         */
        if ( (partnerSwitchPtr != NULL) &&
             (FM_IS_STATE_ALIVE(partnerSwitchPtr->state)) )
        {
            if ( (partnerPort < 0) ||
                (partnerPort >= partnerSwitchPtr->maxPort) )
            {
                status = FM_ERR_INVALID_PORT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
            }

            partnerPortPtr = partnerSwitchPtr->portTable[partnerPort];

            if (partnerPortPtr == NULL)
            {
                status = FM_ERR_INVALID_PORT;
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
            }
        }
    }

    /* Cross-reference the link and the port(s) */
    saLogPortPtr->linkPtr     = intLink;
    saLogPortPtr->sw          = primarySw;
    saLogPortPtr->logicalPort = primaryPort;
    saLogPortPtr->linkType    = intLink->link.type;

    if (saPartnerLogPortPtr != NULL)
    {
        saPartnerLogPortPtr->linkPtr     = intLink;
        saPartnerLogPortPtr->sw          = partnerSw;
        saPartnerLogPortPtr->logicalPort = partnerPort;
        saPartnerLogPortPtr->linkType    = intLink->link.type;
    }

    if (primaryPortPtr != NULL)
    {
        primaryPortPtr->swagPort     = logPort;
        primaryPortPtr->swagLinkType = intLink->link.type;

        /* Copy port characteristics from underlying port into SWAG port */
        logPortPtr->capabilities = primaryPortPtr->capabilities;
        logPortPtr->portFamily   = primaryPortPtr->portFamily;
        logPortPtr->portType     = primaryPortPtr->portType;
    }

    if (partnerPortPtr != NULL)
    {
        partnerPortPtr->swagPort     = partnerLogPort;
        partnerPortPtr->swagLinkType = intLink->link.type;

        /* Copy port characteristics from underlying port into SWAG port */
        if (partnerLogPortPtr != NULL)
        {
            partnerLogPortPtr->capabilities = partnerPortPtr->capabilities;
            partnerLogPortPtr->portFamily   = partnerPortPtr->portFamily;
            partnerLogPortPtr->portType     = partnerPortPtr->portType;
        }
    }

    status = FM_OK;

ABORT:

    if (partnerSwitchProtected)
    {
        UNPROTECT_SWITCH(partnerSw);
    }

    if (primarySwitchProtected)
    {
        UNPROTECT_SWITCH(primarySw);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end PrepareLink */




/*****************************************************************************/
/** ConfigureInternalPort
 * \ingroup intSwag
 *
 * \desc            Configures an internal port for proper operation.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \param[in]       port is the port number to configure.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
static fm_status ConfigureInternalPort(fm_int sw,
                                       fm_int port)
{
    fm_status       status;
    fm_port *       portPtr;
    fm_islTagFormat tagFormat = FM_ISL_TAG_F64;
    fm_bool         internal  = TRUE;
    fm_int          vlanId;
    fm_uint32       pvid;
    fm_uint32       portParserCfg = FM_PORT_PARSER_STOP_AFTER_L4;
    fm_uint32       tagging       = FM_TAG_KEEP;
    fm_bool         captureTcpFlags = TRUE;
    fm_uint32       parserFlagOptions = FM_PORT_PARSER_FLAG_IPV4_OPTIONS
                                        | FM_PORT_PARSER_FLAG_IPV6_ROUTING
                                        | FM_PORT_PARSER_FLAG_IPV6_FRAGMENT
                                        | FM_PORT_PARSER_FLAG_IPV6_HOPBYHOP
                                        | FM_PORT_PARSER_FLAG_IPV6_AUTH
                                        | FM_PORT_PARSER_FLAG_IPV6_DEST;
    fm_bool         routable = FM_ENABLED;
    fm_int          maxFrameSize = FM_INTERNAL_PORT_MAX_FRAME_SIZE;
    fm_bool         updateRoutedFrame = FM_DISABLED;
    fm_bool         updateTTL = FM_DISABLED;
    fm_bool         updateVpri = TRUE;
    fm_uint32       updateCfi  = FM_PORT_TXCFI_ISPRIBIT;
    fm_uint32       statGroups;
    fm_int          retries;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw = %d, port = %d\n", sw, port);

    portPtr = GET_PORT_PTR(sw, port);

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_MAX_FRAME_SIZE,
                                &maxFrameSize);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_ISL_TAG_FORMAT,
                                &tagFormat);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_INTERNAL,
                                &internal);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_PARSER,
                                &portParserCfg);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_TAGGING,
                                &tagging);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_CAPTURE_TCP_FLAGS,
                                &captureTcpFlags);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_PARSER_FLAG_OPTIONS,
                                &parserFlagOptions);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_ROUTABLE,
                                &routable);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_UPDATE_ROUTED_FRAME,
                                &updateRoutedFrame);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_UPDATE_TTL,
                                &updateTTL);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_TXVPRI,
                                &updateVpri);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmSetPortAttribute(sw,
                                port,
                                FM_PORT_TXCFI,
                                &updateCfi);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    if (portPtr->portType == FM_PORT_TYPE_PHYSICAL ||
        portPtr->portType == FM_PORT_TYPE_LAG)
    {
        status = fmGetPortAttribute(sw,
                                    port,
                                    FM_PORT_STAT_GROUP_ENABLE,
                                    &statGroups);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        if ( fmGetBoolApiAttribute(FM_AAK_API_SWAG_INTERNAL_VLAN_STATS,
                                   FM_AAD_API_SWAG_INTERNAL_VLAN_STATS) )
        {
            statGroups |= FM_STAT_GROUP_VLAN_RX_FRAME_CLASS
                          | FM_STAT_GROUP_VLAN_RX_OCTETS;
        }
        else
        {
            statGroups &= ~(FM_STAT_GROUP_VLAN_RX_FRAME_CLASS
                            | FM_STAT_GROUP_VLAN_RX_OCTETS);
        }

        status = fmSetPortAttribute(sw,
                                    port,
                                    FM_PORT_STAT_GROUP_ENABLE,
                                    &statGroups);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }
    }

    status = fmGetVlanFirst(sw, &vlanId);

    if ( (status == FM_OK) && (vlanId > 0) )
    {
        pvid = vlanId;

        status = fmSetPortAttribute(sw,
                                    port,
                                    FM_PORT_DEF_VLAN,
                                    &pvid);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }
    }

    /* Add the port to all active VLANs */
    /* FIXME: This could be optimized to only use vlans that are used
     * on both sides of the trunk link.
     */
    retries = 0;

    while ( (status == FM_OK) && (vlanId > 0) )
    {
        status = fmAddVlanPort(sw,
                               (fm_uint16) vlanId,
                               port,
                               TRUE);

        if (status != FM_OK)
        {
            if (retries < 5)
            {
                fmYield();
                retries++;
                status = FM_OK;
                continue;
            }

            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        status = fmSetVlanPortState(sw,
                                    (fm_uint16) vlanId,
                                    port,
                                    FM_STP_STATE_FORWARDING);

        if (status != FM_OK && status != FM_ERR_PORT_IS_INTERNAL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        status = fmGetVlanNext(sw, vlanId, &vlanId);

        retries = 0;
    }

    if (status == FM_ERR_NO_MORE)
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end ConfigureInternalPort */




/*****************************************************************************/
/** ConfigureInternalTrunk
 * \ingroup intSwag
 *
 * \desc            Configures an internal trunk for proper operation.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \param[in]       trunk points to the trunk to be configured.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
static fm_status ConfigureInternalTrunk(fm_int           sw,
                                        fm_swagIntTrunk *trunk)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunk = %p\n",
                 sw,
                 (void *) trunk);

    if (trunk->trunk.trunkType != FM_SWAG_LINK_INTERNAL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);
    }

    status = ConfigureInternalPort(sw, trunk->trunk.swagPort);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end ConfigureInternalTrunk */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/


/*****************************************************************************/
/** fmCompareSWAGLinks
 * \ingroup intSwag
 *
 * \desc            Compare switch aggregate link entries.
 *
 * \param[in]       first points to the first link.
 *
 * \param[in]       second points to the second link.
 *
 * \return          -1 if the first link sorts before the second.
 * \return           0 if the links are identical.
 * \return           1 if the first link sorts after the second.
 *
 *****************************************************************************/
fm_int fmCompareSWAGLinks(const void *first, const void *second)
{
    fm_swagIntLink *firstLink  = (fm_swagIntLink *) first;
    fm_swagIntLink *secondLink = (fm_swagIntLink *) second;
    fm_int          link1port1;
    fm_int          link1port2;
    fm_int          link2port1;
    fm_int          link2port2;
    fm_int          i;

    link1port1 = firstLink->link.logicalPort;

    if (firstLink->link.type == FM_SWAG_LINK_INTERNAL)
    {
        link1port2 = firstLink->link.partnerLogicalPort;
    }
    else
    {
        link1port2 = -1;
    }

    link2port1 = secondLink->link.logicalPort;

    if (secondLink->link.type == FM_SWAG_LINK_INTERNAL)
    {
        link2port2 = secondLink->link.partnerLogicalPort;
    }
    else
    {
        link2port2 = -1;
    }

    /* calculate sort order in case they aren't equal */
    i = link1port1 - link2port1;

    if (i != 0)
    {
        if (link1port1 == link2port2)
        {
            i = 0;
        }
        else if (link1port2 >= 0)
        {
            if (link1port2 == link2port1)
            {
                i = 0;
            }
            else if (link1port2 == link2port2)
            {
                i = 0;
            }
        }
    }

    if (i < 0)
    {
        i = -1;
    }
    else if (i > 0)
    {
        i = 1;
    }

    return i;

}   /* end fmCompareSWAGLinks */




/*****************************************************************************/
/** fmCreateSWAG
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Create a switch aggregate.
 *
 * \note            The switch aggregate will be created in a DOWN state. The
 *                  switch aggregate must be enabled by calling
 *                  ''fmSetSwitchState'' to bring it to the UP state. The
 *                  switch aggregate should not be enabled until all switches
 *                  and links have been added to the aggregate (see
 *                  ''fmAddSWAGSwitch'' and ''fmAddSWAGLink'') and the topology
 *                  has been established (see ''fmSetSWAGTopology'').
 *
 * \param[in]       topology is the switch aggregate topology type.  If the
 *                  application does not want to specify the topology
 *                  when creating the aggregate, use
 *                  ''FM_SWAG_TOPOLOGY_UNDEFINED''.
 *
 * \param[in]       solver points to the topology solver function that should
 *                  be called on switch aggregate configuration changes. May
 *                  be specified as NULL to disable calls for user-defined
 *                  topologies or to use the API's built-in solvers for
 *                  standard topologies. See 'fm_swagTopologySolver' for a
 *                  complete description of this argument type.
 *
 * \param[out]      swagId points to caller-allocated storage where this
 *                  function should place the switch aggregate ID, which can
 *                  then be used as a switch aggregate identifier for other
 *                  services.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if the SWAG cannot be created.
 * \return          FM_ERR_NO_MEM if no memory available for data structures.
 *
 *****************************************************************************/
fm_status fmCreateSWAG(fm_swagTopology       topology,
                       fm_swagTopologySolver solver,
                       fm_int *              swagId)
{
    fm_status               status;
    fm_int                  sw = 0;
    fm_switch *             swPtr;
    fmSWAG_switch *         swag;
    fm_event *              insertEvent;
    fm_eventSwitchInserted *insert;
    fm_topologySolverEvent  solverEvent;

#if 0
printf("Enabling logging...\n");
fmSetLoggingFilter(FM_LOG_CAT_SWAG | FM_LOG_CAT_ACL, FM_LOG_LEVEL_ALL, NULL, NULL);
fmSetLoggingVerbosity(FM_LOG_VERBOSITY_DEFAULT);
fmLoggingEnable();
#endif

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "topology=%d\n",
                     topology);

    switch (topology)
    {
#if FM_SUPPORT_SWAG

#if FM_INCLUDE_RING_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_RING:
            break;
#endif

#if FM_INCLUDE_FAT_TREE_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_FAT_TREE:
            break;
#endif

#if FM_INCLUDE_MESH_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_MESH:
            break;
#endif

#endif

        default:
            FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);

    } /* end switch (topology) */

    if (solver == NULL)
    {
        solver = GetDefaultTopologySolver(topology);
    }

    /* request a switch number from the platform layer */
    status = fmPlatformCreateSWAG(&sw);

    if (status != FM_OK)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);
    }

    /* quick validation of the switch number */
    if ( (sw < 0) || ( sw >= FM_MAX_NUM_SWITCHES ) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_FAIL);
    }

    status = fmLockSwitch(sw);

    if (status != FM_OK)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);
    }

    /* Allocate switch inserted event as high priority to avoid throttling. */
    insertEvent = fmAllocateEvent(sw,
                                  FM_EVID_SYSTEM,
                                  FM_EVENT_SWITCH_INSERTED,
                                  FM_EVENT_PRIORITY_HIGH);

    if (!insertEvent)
    {
        UNLOCK_SWITCH(sw);
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_NO_EVENTS_AVAILABLE);
    }

    insert        = &insertEvent->info.fpSwitchInsertedEvent;
    insert->model = FM_SWITCH_MODEL_SWAG;
    insert->slot  = sw;

    /* create and initialize switch structure */
    status = fmHandleSwitchInserted(sw, insert);

    if (status == FM_OK)
    {
        swPtr = fmRootApi->fmSwitchStateTable[sw];
        swag  = (fmSWAG_switch *) swPtr->extension;

        /* Set up the new topology and solver without executing
         * solver functions
         */
        status = SetTopology(sw, swag, topology, FALSE);
    }

    if (status == FM_OK)
    {
        /* Set the topology solver */
        status = SetTopologySolver(sw, swag, solver, FALSE);
    }

    if (status == FM_OK)
    {
        /* Fully initialized, execute the solver's initialization function */
        if (swag->solver != NULL)
        {
            solverEvent.cmd = FM_SOLVER_CMD_INITIALIZE;
            status          = solver(sw, &solverEvent);
        }
    }

    fmReleaseEvent(insertEvent);

    if (status == FM_OK)
    {
        /* This is a SWAG, allow the platform to complete SWAG initialization */
        status = fmPlatformSWAGInitialize(sw);
    }

    /* Release the switch lock */
    UNLOCK_SWITCH(sw);

    *swagId = sw;

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end fmCreateSWAG */




/*****************************************************************************/
/** fmDeleteSWAG
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Delete a switch aggregate previously created with a call
 *                  to ''fmCreateSWAG''.
 *
 * \note            The switch aggregate cannot be deleted if it has any
 *                  member switches.
 *
 * \param[in]       swagId is the ID of the switch aggregate to delete.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_SWITCH_AGGREGATE_NOT_EMPTY if the switch aggregate
 *                  contains member switches.
 *
 *****************************************************************************/
fm_status fmDeleteSWAG(fm_int swagId)
{
    fm_status              status;
    fm_event *             event;
    fm_eventSwitchRemoved *removeEvent;
    fm_switch *            swPtr;
    fmSWAG_switch *        swag;
    fm_topologySolverEvent solverEvent;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    if ( (swagId < 0) || (swagId >= FM_MAX_NUM_SWITCHES) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH);
    }

    VALIDATE_SWITCH_LOCK(swagId);

    /* Take write access to the switch lock */
    LOCK_SWITCH(swagId);

    swPtr = GET_SWITCH_PTR(swagId);

    if (swPtr == NULL)
    {
        UNLOCK_SWITCH(swagId);
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH);
    }

    swag = (fmSWAG_switch *) swPtr->extension;

    if (swag->numSwitches > 0)
    {
        UNLOCK_SWITCH(swagId);
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_AGGREGATE_NOT_EMPTY);
    }

    /* Tell the topology solver to shut down */
    if (swag->solver != NULL)
    {
        solverEvent.cmd = FM_SOLVER_CMD_SHUTDOWN;
        status          = swag->solver(swagId, &solverEvent);

        if (status != FM_OK)
        {
            UNLOCK_SWITCH(swagId);
            FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);
        }
    }

    /* Allocate switch inserted event as high priority to avoid throttling. */
    event = fmAllocateEvent(swagId,
                            FM_EVID_SYSTEM,
                            FM_EVENT_SWITCH_REMOVED,
                            FM_EVENT_PRIORITY_HIGH);

    if (!event)
    {
        UNLOCK_SWITCH(swagId);
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_NO_EVENTS_AVAILABLE);
    }

    removeEvent       = &event->info.fpSwitchRemovedEvent;
    removeEvent->slot = swagId;

    /* cleanup and delete the switch structure */
    status = fmHandleSwitchRemoved(swagId, removeEvent);

    if (status == FM_OK)
    {
        /* release the switch number back to the platform layer */
        status = fmPlatformDeleteSWAG(swagId);
    }

    if (status == FM_OK)
    {
        /* Deliver event notification to the application */
        fmSendThreadEvent(&fmRootApi->eventThread, event);
    }
    else
    {
        fmReleaseEvent(event);
    }

    /* Release the switch lock */
    UNLOCK_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end fmDeleteSWAG */




/*****************************************************************************/
/** fmAddSWAGSwitch
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Add an individual switch device to a switch aggregate.
 *
 * \note            The switch must be in the down state before it can be
 *                  added to the aggregate. Set it down with
 *                  ''fmSetSwitchState''.
 *
 * \param[in]       swagId is the ID of the switch aggregate to which the
 *                  individual switch device should be added.
 *
 * \param[in]       sw is the switch ID of the switch to be added to the
 *                  switch aggregate.
 *
 * \param[in]       switchRole identifies the individual switch device's
 *                  position in the switch aggregate topology (see
 *                  ''fm_switchRole'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH_AGGREGATE if swagId is invalid.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_SWITCH_ALREADY_IN_AGGREGATE if switch is already in
 *                  the aggregate.
 * \return          FM_ERR_SWITCH_IS_NOT_DOWN if switch is not in the down
 *                  state.
 * \return          FM_ERR_NO_MEM if no memory available for data structures.
 *
 *****************************************************************************/
fm_status fmAddSWAGSwitch(fm_int        swagId,
                          fm_int        sw,
                          fm_switchRole switchRole)
{
    fm_status              status;
    fm_switch *            swPtr;
    fm_switch *            aggSwPtr;
    fmSWAG_switch *        aggExt;
    fm_bool                swagLocked = FALSE;
    fm_bool                swLocked   = FALSE;
    fm_swagMember *        saSw;
    fm_topologySolverEvent solverEvent;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, sw=%d, switchRole=%d\n",
                     swagId,
                     sw,
                     switchRole);

    if ( (swagId < 0) || (swagId >= FM_MAX_NUM_SWITCHES) )
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    if ( !SWITCH_LOCK_EXISTS(swagId) )
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    /* Lock the switch aggregate */
    LOCK_SWITCH(swagId);
    swagLocked = TRUE;
    aggSwPtr   = fmRootApi->fmSwitchStateTable[swagId];

    if (aggSwPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    aggExt = (fmSWAG_switch *) aggSwPtr->extension;

    if ( SWITCH_LOCK_EXISTS(sw) )
    {
        /* Lock the physical switch */
        LOCK_SWITCH(sw);
        swLocked = TRUE;
        swPtr    = fmRootApi->fmSwitchStateTable[sw];

        if (swPtr == NULL)
        {
            status = FM_ERR_INVALID_SWITCH;
            goto ABORT;
        }

        /* Make sure the switch isn't already part of an aggregate */
        if (swPtr->swag >= 0)
        {
            status = FM_ERR_SWITCH_ALREADY_IN_AGGREGATE;
            goto ABORT;
        }

        /* The switch must be down in order to be added to the aggregate */
        if (swPtr->state != FM_SWITCH_STATE_DOWN)
        {
            status = FM_ERR_SWITCH_IS_NOT_DOWN;
            goto ABORT;
        }
    }
    else
    {
        swPtr = NULL;
    }

    /* Create and populate the switch information for the aggregate */
    saSw = (fm_swagMember *) fmAlloc( sizeof(fm_swagMember) );

    if (saSw == NULL)
    {
        status = FM_ERR_NO_MEM;
        goto ABORT;
    }

    FM_CLEAR(*saSw);

    saSw->swId      = sw;
    saSw->role      = switchRole;
    saSw->numTrunks = 0;

    fmTreeInit(&saSw->stackedRemotePorts);

    status = fmInitializeSwitchInSWAG(swagId, saSw, TRUE);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

    /* Add the switch into the aggregate */
    fmAddSwitchToSwitchList(aggExt, saSw);

    /* If glort space has already been assigned, select a glort space for
     * this switch
     */
    if (aggExt->glortMask != 0)
    {
        status = fmSelectGlortSpaceForSwitchInSWAG(swagId, sw);

        if (status != FM_OK)
        {
            goto ABORT;
        }
    }

    if (swPtr != NULL)
    {
        status = fmUpdateSwitchInSWAG(swagId, sw, TRUE);

        if (status != FM_OK)
        {
            goto ABORT;
        }
    }

    /* Inform the topology solver about the new switch */
    if (aggExt->solver != NULL)
    {
        solverEvent.cmd                 = FM_SOLVER_CMD_ADD_SWITCH;
        solverEvent.info.switchEvent.sw = sw;
        status                          = aggExt->solver(swagId, &solverEvent);
    }
    else
    {
        status = FM_OK;
    }

ABORT:

    if (swLocked)
    {
        UNLOCK_SWITCH(sw);
    }

    if (swagLocked)
    {
        UNLOCK_SWITCH(swagId);
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmAddSWAGSwitch */




/*****************************************************************************/
/** fmDeleteSWAGSwitch
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Delete an individual switch device from a switch aggregate.
 *
 * \param[in]       sw is the switch ID of the switch to be deleted from the
 *                  switch aggregate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_SWITCH_NOT_IN_AGGREGATE if the switch is not a
 *                  member of a switch aggregate.
 *
 *****************************************************************************/
fm_status fmDeleteSWAGSwitch(fm_int sw)
{
    fm_status              status;
    fm_int                 swagId = 0;
    fm_switch *            swPtr;
    fm_switch *            aggSwPtr;
    fmSWAG_switch *        aggExt;
    fm_bool                swagLocked = FALSE;
    fm_bool                swLocked   = FALSE;
    fm_swagMember *        saSw;
    fm_swagIntTrunk *      swTr;
    fm_swagIntTrunk *      nextTr;
    fm_swagIntLink *       link;
    fm_topologySolverEvent solverEvent;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "sw=%d\n", sw);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    if ( !SWITCH_LOCK_EXISTS(sw) )
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    /* Lock the physical switch */
    LOCK_SWITCH(sw);
    swLocked = TRUE;
    swPtr    = fmRootApi->fmSwitchStateTable[sw];

    if (swPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    /* Get the switch aggregate ID */
    swagId = swPtr->swag;

    if (swagId == 0)
    {
        status = FM_ERR_SWITCH_NOT_IN_AGGREGATE;
        goto ABORT;
    }

    /* The switch must be down in order to be removed from the aggregate */
    if (swPtr->state != FM_SWITCH_STATE_DOWN)
    {
        status = FM_ERR_SWITCH_IS_NOT_DOWN;
        goto ABORT;
    }

    /* Unlock the physical switch because we always have to lock the
     * switch aggregate first */
    UNLOCK_SWITCH(sw);
    swLocked = FALSE;

    /* Lock the switch aggregate */
    LOCK_SWITCH(swagId);
    swagLocked = TRUE;
    aggSwPtr   = fmRootApi->fmSwitchStateTable[swagId];

    if (aggSwPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    aggExt = (fmSWAG_switch *) aggSwPtr->extension;

    /* Lock the physical switch again */
    LOCK_SWITCH(sw);
    swLocked = TRUE;
    swPtr    = fmRootApi->fmSwitchStateTable[sw];

    if (swPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    /* Find the physical switch in the switch list */
    saSw = fmFindSwitchInSWAG(aggExt, sw);

    if (saSw == NULL)
    {
        status = FM_FAIL;
        goto ABORT;
    }

    /* Inform the topology solver about the switch removal */
    if (aggExt->solver != NULL)
    {
        solverEvent.cmd                 = FM_SOLVER_CMD_REMOVE_SWITCH;
        solverEvent.info.switchEvent.sw = sw;
        status                          = aggExt->solver(swagId, &solverEvent);

        if (status != FM_OK)
        {
            goto ABORT;
        }
    }

    /* Remove the switch from the aggregate */
    fmRemoveSwitchFromSwitchList(aggExt, saSw);

    /* Remove the trunks */
    swTr = fmGetFirstTrunkInSwitch(aggExt);

    while (swTr != NULL)
    {
        nextTr = fmGetNextTrunkInSwitch(swTr);

        if (swTr->trunk.trunkSw == sw)
        {
            while ( ( link = fmSWAGGetFirstTrunkLink(swTr) ) != NULL )
            {
                fmSWAGRemoveTrunkLink(swTr, link);
            }

            fmRemoveTrunkFromTrunkList(aggExt, swTr);
            fmFree(swTr);
        }

        swTr = nextTr;
    }

    /* Update the switch information to show it is not in a switch aggregate */
    status = fmUpdateSwitchInSWAG(swagId, sw, FALSE);

    if (status != FM_OK)
    {
        goto ABORT;
    }

    /* Release the glort space assigned to this switch */
    if (saSw->glortSpace != NULL)
    {
        saSw->glortSpace->inUse = FALSE;
        saSw->glortSpace        = NULL;
    }

    /* Release the memory used for the switch table */
    fmFree(saSw);

    status = FM_OK;

ABORT:

    if (swLocked)
    {
        UNLOCK_SWITCH(sw);
    }

    if (swagLocked)
    {
        UNLOCK_SWITCH(swagId);
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmDeleteSWAGSwitch */




/*****************************************************************************/
/** fmSetSWAGSwitchMaster
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Determines whether a switch is considered a "master"
 *                  within the switch aggregate.  Only master switches
 *                  receive broadcast traffic, in order to avoid multiple
 *                  identical frames being delivered to the CPU.  The
 *                  platform or topology solver should determine which
 *                  switch is to be treated as the master.
 * \note            Only one switch may be a master in a switch aggregate.
 *                  Setting a switch to "master" will cause any prior master
 *                  to be changed to a slave.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       sw is the switch ID of the switch.
 *
 * \param[in]       isMaster is TRUE if the switch is to be considered a master,
 *                  FALSE if not.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH_AGGREGATE if swagId is invalid.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_SWITCH_NOT_IN_AGGREGATE if sw is not in the
 *                  switch aggregate.
 *
 *****************************************************************************/
fm_status fmSetSWAGSwitchMaster(fm_int  swagId,
                                fm_int  sw,
                                fm_bool isMaster)
{
    fm_status              status;
    fmSWAG_switch *        switchExt;
    fm_swagMember *        memberSwitch;
    fm_int                 oldMaster;
    fm_swagMember *        oldMasterMember;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId = %d, sw = %d, isMaster = %d\n",
                     swagId,
                     sw,
                     isMaster);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    switchExt = GET_SWITCH_EXT(swagId);

    /* Find the physical switch in the switch list */
    memberSwitch = fmFindSwitchInSWAG(switchExt, sw);

    if (memberSwitch != NULL)
    {
        /* Do nothing if the switch is already configured correctly */
        if (memberSwitch->masterCpu == isMaster)
        {
            oldMaster = -1;
        }
        /* Are we selecting a new master? */
        else if (isMaster)
        {
            /* Get the switch number of the prior master cpu switch */
            oldMaster = switchExt->masterCpuSw;

            /* Save the new master switch number */
            switchExt->masterCpuSw = sw;
        }
        else if (switchExt->masterCpuSw == sw)
        {
            /* Removing the master without selecting a new master */
            oldMaster              = -1;
            switchExt->masterCpuSw = -1;
        }
        else
        {
            /* Not changing the master switch */
            oldMaster = -1;
        }

        if (oldMaster >= 0)
        {
            /* Find the old master */
            oldMasterMember = fmFindSwitchInSWAG(switchExt, oldMaster);

            if (oldMasterMember != NULL)
            {
                /* Remove the old master */
                oldMasterMember->masterCpu = FALSE;

                status = fmConfigureSWAGSwitchMaster(oldMaster);

                if (status != FM_OK)
                {
                    goto ABORT;
                }
            }
        }

        memberSwitch->masterCpu = isMaster;

        status = fmConfigureSWAGSwitchMaster(sw);
    }
    else
    {
        status = FM_ERR_SWITCH_NOT_IN_AGGREGATE;
    }

ABORT:

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end fmSetSWAGSwitchMaster */




/*****************************************************************************/
/** fmGetSWAGSwitchFirst
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Retrieve the first switch id in the switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate on which to operate.
 *
 * \param[out]      searchToken points to caller-allocated storage of type
 *                  fm_voidptr, where this function will store a token
 *                  to be used in a subsequent call to ''fmGetSWAGSwitchNext''.
 *
 * \param[out]      firstSwitch points to caller-allocated storage where this
 *                  function will store the first underlying switch ID.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_NO_MORE if there are no switches in this aggregate.
 *
 *****************************************************************************/
fm_status fmGetSWAGSwitchFirst(fm_int      swagId,
                               fm_voidptr *searchToken,
                               fm_int *    firstSwitch)
{
    fmSWAG_switch *aggExt;
    fm_status      status;
    fm_swagMember *swPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, searchToken=%p, firstSwitch=%p\n",
                     swagId,
                     (void *) searchToken,
                     (void *) firstSwitch);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    aggExt = fmRootApi->fmSwitchStateTable[swagId]->extension;

    swPtr = fmGetFirstSwitchInSWAG(aggExt);

    *searchToken = (fm_voidptr) swPtr;

    if (swPtr != NULL)
    {
        status       = FM_OK;
        *firstSwitch = swPtr->swId;
    }
    else
    {
        status = FM_ERR_NO_MORE;
    }

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end fmGetSWAGSwitchFirst */




/*****************************************************************************/
/** fmGetSWAGSwitchNext
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Retrieve the next switch id in the switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate on which to operate.
 *
 * \param[out]      searchToken points to caller-allocated storage of type
 *                  fm_voidptr, where this function will store a token
 *                  to be used in a subsequent call to ''fmGetSWAGSwitchNext''.
 *
 * \param[out]      nextSwitch points to caller-allocated storage where this
 *                  function will store the next underlying switch ID.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_NO_MORE if there are no switches in this aggregate.
 *
 *****************************************************************************/
fm_status fmGetSWAGSwitchNext(fm_int      swagId,
                              fm_voidptr *searchToken,
                              fm_int *    nextSwitch)
{
    fm_status      status;
    fm_swagMember *swPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, searchToken=%p, nextSwitch=%p\n",
                     swagId,
                     (void *) searchToken,
                     (void *) nextSwitch);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    swPtr = (fm_swagMember *) *searchToken;

    if (swPtr != NULL)
    {
        swPtr = fmGetNextSwitchInSWAG(swPtr);
    }

    *searchToken = (fm_voidptr) swPtr;

    if (swPtr != NULL)
    {
        status      = FM_OK;
        *nextSwitch = swPtr->swId;
    }
    else
    {
        status = FM_ERR_NO_MORE;
    }

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end fmGetSWAGSwitchNext */




/*****************************************************************************/
/** fmAddSWAGLink
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Add a link within a switch aggregate topology. A link is a
 *                  connection between ports. For some links, this will be
 *                  two ports, one on each of two switch devices. For other
 *                  links, this will be a single port that connects externally
 *                  or to the CPU.
 *
 * \note            The switch aggregate must be in the up state to have links
 *                  added to it.
 *
 * \param[in]       swagId is the ID of the switch aggregate to which the
 *                  link should be added.
 *
 * \param[in]       link points to an ''fm_swagLink'' structure that
 *                  describes the nature of the link. Note that the logicalPort
 *                  member of link must be between ''FM_MIN_USER_LOGICAL_PORT''
 *                  and ''FM_MAX_USER_LOGICAL_PORT''.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH_AGGREGATE if swagId is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if link is NULL.
 * \return          FM_ERR_SWITCH_NOT_UP if the switch aggregate is not in
 *                  the up state.
 * \return          FM_ERR_ALREADY_EXISTS if there is already a link in
 *                  the switch aggregate using the logical port specified in
 *                  link.
 * \return          FM_ERR_INVALID_ARGUMENT if link is NULL or the contents of
 *                  the ''fm_swagLink'' structure pointed to by link are
 *                  invalid.
 * \return          FM_ERR_SWITCH_NOT_IN_AGGREGATE link specifies a switch
 *                  that is not a member of the aggregate.
 * \return          FM_ERR_ALREADYUSED_PORT if link specifies a port that is
 *                  already in use in another link.
 * \return          FM_ERR_NO_MEM if no memory available for data structures.
 *
 *****************************************************************************/
fm_status fmAddSWAGLink(fm_int swagId, fm_swagLink *link)
{
    fm_status              status;
    fm_switch *            aggSwPtr;
    fmSWAG_switch *        aggExt;
    fm_bool                swagLocked = FALSE;
    fm_swagIntLink *       intLink;
    fm_int                 logPort;
    fm_int                 partnerLogPort;
    fm_int                 primarySw;
    fm_int                 partnerSw;
    fm_bool                boolFlag;
    fm_topologySolverEvent solverEvent;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, link=%p (type=%d, logicalPort=%d, swId=%d, "
                     "swPort=%d, partnerLogicalPort=%d, partnerSwitch=%d, "
                     "partnerPort=%d)\n",
                     swagId,
                     (void *) link,
                     (link != NULL) ? link->type : 99999,
                     (link != NULL) ? link->logicalPort : -1,
                     (link != NULL) ? link->swId : -1,
                     (link != NULL) ? link->swPort : -1,
                     (link != NULL) ? link->partnerLogicalPort : -1,
                     (link != NULL) ? link->partnerSwitch : -1,
                     (link != NULL) ? link->partnerPort : -1);

    if ( (swagId < 0) || (swagId >= FM_MAX_NUM_SWITCHES) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH_AGGREGATE);
    }

    if (link == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    logPort     = link->logicalPort;
    primarySw   = link->swId;

    if (link->type == FM_SWAG_LINK_INTERNAL)
    {
        partnerLogPort = link->partnerLogicalPort;
        partnerSw      = link->partnerSwitch;
    }
    else
    {
        partnerLogPort = -1;
        partnerSw      = -1;
    }

    if ( !SWITCH_LOCK_EXISTS(swagId) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH_AGGREGATE);
    }

    /* Lock the switch aggregate */
    LOCK_SWITCH(swagId);
    swagLocked = TRUE;
    aggSwPtr   = fmRootApi->fmSwitchStateTable[swagId];

    if (aggSwPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    aggExt = (fmSWAG_switch *) aggSwPtr->extension;

    /* Make sure the port(s) in this link are not already defined in
     * another link
     */
    intLink = fmFindPortInLinkList(aggExt, logPort);

    if (intLink != NULL)
    {
        status = FM_ERR_ALREADYUSED_PORT;
        goto ABORT;
    }

    if (partnerLogPort >= 0)
    {
        intLink = fmFindPortInLinkList(aggExt, partnerLogPort);

        if (intLink != NULL)
        {
            status = FM_ERR_ALREADYUSED_PORT;
            goto ABORT;
        }
    }

    /* Double-check: Make sure the link isn't already in the list */
    intLink = fmFindLinkInLinkList(aggExt, link);

    if (intLink != NULL)
    {
        status = FM_ERR_ALREADY_EXISTS;
        goto ABORT;
    }

    /* Make sure the underlying switch is a member of the switch aggregate */
    if (fmFindSwitchInSWAG(aggExt, primarySw) == NULL)
    {
        status = FM_ERR_SWITCH_NOT_IN_AGGREGATE;
        goto ABORT;
    }

    if (partnerSw >= 0)
    {
        /* Make sure the underlying partner switch is also a member of
         * the switch aggregate
         */
        if (fmFindSwitchInSWAG(aggExt, partnerSw) == NULL)
        {
            status = FM_ERR_SWITCH_NOT_IN_AGGREGATE;
            goto ABORT;
        }
    }

    /* Create a record for the link */
    intLink = (fm_swagIntLink *) fmAlloc( sizeof(fm_swagIntLink) );

    if (intLink == NULL)
    {
        status = FM_ERR_NO_MEM;
        goto ABORT;
    }

    FM_MEMCPY_S( &intLink->link,
                 sizeof(intLink->link),
                 link, sizeof(fm_swagLink) );
    intLink->ext     = aggExt;
    intLink->enabled = FALSE;
    intLink->trunk   = NULL;
    FM_DLL_INIT_NODE(intLink, nextLink, prevLink);

    /* Add the link to the link list */
    status = fmCustomTreeInsert(&aggExt->links, intLink, intLink);

    if (status != FM_OK)
    {
        fmFree(intLink);
        goto ABORT;
    }

    if (FM_IS_STATE_ALIVE(aggSwPtr->state))
    {
        status = PrepareLink(swagId, intLink);

        if (status != FM_OK)
        {
            fmCustomTreeRemove(&aggExt->links, intLink, NULL);
            fmFree(intLink);
            goto ABORT;
        }
    }

    /* Inform the topology solver about the new link so that it can
     * configure the trunks */
    if (aggExt->solver != NULL)
    {
        solverEvent.cmd = FM_SOLVER_CMD_ADD_LINK;
        FM_MEMCPY_S( &solverEvent.info.linkEvent.link,
                     sizeof(solverEvent.info.linkEvent.link),
                     link,
                     sizeof(fm_swagLink) );
        status = aggExt->solver(swagId, &solverEvent);
    }
    else
    {
        status = FM_OK;
    }

    if (status == FM_OK)
    {
        if ( fmGetBoolApiAttribute(FM_AAK_API_AUTO_ENABLE_SWAG_LINKS,
                                   FM_AAD_API_AUTO_ENABLE_SWAG_LINKS) )
        {
            boolFlag = TRUE;
            status   = fmSetSWAGLinkAttribute(swagId,
                                              link->logicalPort,
                                              FM_LINK_ATTR_ADMIN_STATE,
                                              &boolFlag);
        }
    }
    else
    {
        fmCustomTreeRemove(&aggExt->links, intLink, NULL);
        fmFree(intLink);
    }

ABORT:

    if (swagLocked)
    {
        UNLOCK_SWITCH(swagId);
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmAddSWAGLink */




/*****************************************************************************/
/** fmDeleteSWAGLink
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Delete a link within a switch aggregate topology. A link
 *                  is a connection between ports.
 *
 * \param[in]       swagId is the ID of the switch aggregate from which to
 *                  delete the link.
 *
 * \param[in]       logicalPort is the logical port number associated with
 *                  the link to be deleted. A link must have been added in a
 *                  prior call to ''fmAddSWAGLink'' with this value for either
 *                  the logicalPort or partnerLogicalPort of the ''fm_swagLink''
 *                  link structure.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH_AGGREGATE if swagId is invalid.
 * \return          FM_ERR_NOT_FOUND if there is no link in the switch
 *                  aggregate with the specified logical port.
 *
 *****************************************************************************/
fm_status fmDeleteSWAGLink(fm_int swagId, fm_int logicalPort)
{
    fm_status              status;
    fm_switch *            aggSwPtr;
    fmSWAG_switch *        aggExt;
    fm_bool                swagLocked = FALSE;
    fm_swagIntLink *       intLink;
    fm_int                 primarySw;
    fm_int                 primaryPort;
    fm_int                 partnerSw;
    fm_int                 partnerPort;
    fm_switch *            primarySwitchPtr       = NULL;
    fm_switch *            partnerSwitchPtr       = NULL;
    fm_port *              primaryPortPtr         = NULL;
    fm_port *              partnerPortPtr         = NULL;
    fm_bool                primarySwitchProtected = FALSE;
    fm_bool                partnerSwitchProtected = FALSE;
    fm_port *              portPtr;
    fmSWAG_port *          aggPortPtr;
    fm_topologySolverEvent solverEvent;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    if ( (swagId < 0) || (swagId >= FM_MAX_NUM_SWITCHES) )
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    if ( !SWITCH_LOCK_EXISTS(swagId) )
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    /* Lock the switch aggregate */
    LOCK_SWITCH(swagId);
    swagLocked = TRUE;
    aggSwPtr   = fmRootApi->fmSwitchStateTable[swagId];

    if (aggSwPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    aggExt = (fmSWAG_switch *) aggSwPtr->extension;

    /**************************************************
     * Find and delete the link
     **************************************************/
    intLink = fmFindPortInLinkList(aggExt, logicalPort);

    if (intLink == NULL)
    {
        status = FM_ERR_NOT_FOUND;
        goto ABORT;
    }

    /* Inform the topology solver so that it can adjust or remove the trunks */
    if (aggExt->solver != NULL)
    {
        solverEvent.cmd = FM_SOLVER_CMD_REMOVE_LINK;
        FM_MEMCPY_S( &solverEvent.info.linkEvent.link,
                     sizeof(solverEvent.info.linkEvent.link),
                     &intLink->link,
                     sizeof(fm_swagLink) );
        status = aggExt->solver(swagId, &solverEvent);

        if (status != FM_OK)
        {
            goto ABORT;
        }
    }

    /* If the primary switch exists, try to remove the link from the port */
    primarySw   = intLink->link.swId;
    primaryPort = intLink->link.swPort;

    if ( SWITCH_LOCK_EXISTS(primarySw) )
    {
        PROTECT_SWITCH(primarySw);
        primarySwitchProtected = TRUE;

        primarySwitchPtr = fmRootApi->fmSwitchStateTable[primarySw];
    }

    if (primarySwitchPtr != NULL)
    {
        primaryPortPtr = primarySwitchPtr->portTable[primaryPort];

        if (primaryPortPtr != NULL)
        {
            primaryPortPtr->swagPort     = -1;
            primaryPortPtr->swagLinkType = FM_SWAG_LINK_UNDEFINED;
        }
    }

    /* If the partner switch exists, try to remove the link from the port */
    if (intLink->link.type == FM_SWAG_LINK_INTERNAL)
    {
        partnerSw   = intLink->link.partnerSwitch;
        partnerPort = intLink->link.partnerPort;

        if ( SWITCH_LOCK_EXISTS(partnerSw) )
        {
            PROTECT_SWITCH(partnerSw);
            partnerSwitchProtected = TRUE;

            partnerSwitchPtr = fmRootApi->fmSwitchStateTable[partnerSw];
        }

        if (partnerSwitchPtr != NULL)
        {
            partnerPortPtr = partnerSwitchPtr->portTable[partnerPort];

            if (partnerPortPtr != NULL)
            {
                partnerPortPtr->swagPort     = -1;
                partnerPortPtr->swagLinkType = FM_SWAG_LINK_UNDEFINED;
            }
        }
    }
    else
    {
        partnerSw = -1;
    }

    /* remove the link from the switch-aggregate port(s) */
    portPtr    = aggSwPtr->portTable[intLink->link.logicalPort];
    aggPortPtr = portPtr->extension;

    aggPortPtr->linkPtr     = NULL;
    aggPortPtr->sw          = -1;
    aggPortPtr->logicalPort = -1;
    aggPortPtr->linkType    = FM_SWAG_LINK_UNDEFINED;

    if (intLink->link.type == FM_SWAG_LINK_INTERNAL)
    {
        portPtr    = aggSwPtr->portTable[intLink->link.partnerLogicalPort];
        aggPortPtr = portPtr->extension;

        aggPortPtr->linkPtr     = NULL;
        aggPortPtr->sw          = -1;
        aggPortPtr->logicalPort = -1;
        aggPortPtr->linkType    = FM_SWAG_LINK_UNDEFINED;
    }

    /* remove the link from the list and free it */
    fmCustomTreeRemove(&aggExt->links, intLink, NULL);
    fmFree(intLink);

    status = FM_OK;

ABORT:

    if (primarySwitchProtected)
    {
        UNPROTECT_SWITCH(primarySw);
    }

    if (partnerSwitchProtected)
    {
        UNPROTECT_SWITCH(partnerSw);
    }

    if (swagLocked)
    {
        UNLOCK_SWITCH(swagId);
    }

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmDeleteSWAGLink */




/*****************************************************************************/
/** fmSetSWAGActiveCpuLink
 * \ingroup intSwag
 *
 * (Change ingroup back to swag after this function actually gets implemented)
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Set which of two or more CPUs in the switch aggregate is
 *                  the CPU to which events should be sent. Used to support
 *                  redundant management CPUs with failover. If there is only
 *                  one CPU link in the switch aggregate, there is no need to
 *                  call this function.
 *
 * \note            Redundancy and failover operations are the responsibility
 *                  of the application. The API does not provide any underlying
 *                  functionality to support high availability.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to
 *                  operate.
 *
 * \param[in]       logicalPort is the logical port number associated with
 *                  the link of type ''FM_SWAG_LINK_CPU'' that is to become
 *                  the active CPU link.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH_AGGREGATE if swagId is invalid.
 * \return          FM_ERR_INVALID_SWAG_LINK if logicalPort does not
 *                  identify an existing link in the switch aggregate, or if the
 *                  link it does identify is not of type ''FM_SWAG_LINK_CPU''.
 *
 *****************************************************************************/
fm_status fmSetSWAGActiveCpuLink(fm_int swagId,
                                 fm_int logicalPort)
{
    FM_NOT_USED(swagId);
    FM_NOT_USED(logicalPort);

    /* TODO: Add support */
    return FM_ERR_UNSUPPORTED;

}   /* end  fmSetSWAGActiveCpuLink */




/*****************************************************************************/
/** fmSetSWAGLinkAttribute
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Set a switch aggregate link attribute.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to
 *                  operate.
 *
 * \param[in]       logicalPort is the logical port number associated with
 *                  the link whose attribute is to be set. A link must have
 *                  been added in a prior call to ''fmAddSWAGLink'' with this
 *                  value for either the logicalPort or partnerLogicalPort.
 *
 * \param[in]       attr is the switch aggregate link attribute to set
 *                  (see 'Switch Aggregate Link Attributes').
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_NOT_FOUND if logicalPort does not identify an
 *                  existing link in the switch aggregate.
 * \return          FM_ERR_INVALID_ARGUMENT if value is invalid.
 * \return          FM_ERR_INVALID_ATTRIB if attr is unrecognized.
 *
 *****************************************************************************/
fm_status fmSetSWAGLinkAttribute(fm_int swagId,
                                 fm_int logicalPort,
                                 fm_int attr,
                                 void * value)
{
    fm_status              status;
    fm_switch *            aggSwPtr;
    fmSWAG_switch *        aggExt;
    fm_swagIntLink *       intLink;
    fm_bool                oldEnabled;
    fm_topologySolverEvent solverEvent;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, logicalPort=%d, attr=%d, value=%p\n",
                     swagId,
                     logicalPort,
                     attr,
                     value);

    if ( !SWITCH_LOCK_EXISTS(swagId) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH_AGGREGATE);
    }

    PROTECT_SWITCH(swagId);

    aggSwPtr = fmRootApi->fmSwitchStateTable[swagId];
    aggExt   = aggSwPtr->extension;

    /**************************************************
     * Find the link
     **************************************************/
    intLink = fmFindPortInLinkList(aggExt, logicalPort);

    if (intLink == NULL)
    {
        status = FM_ERR_NOT_FOUND;
        goto ABORT;
    }

    switch (attr)
    {
        case FM_LINK_ATTR_ADMIN_STATE:
            oldEnabled       = intLink->enabled;
            intLink->enabled = *( (fm_bool *) value );

            if ( (intLink->enabled != oldEnabled) && (aggExt->solver != NULL) )
            {
                if (intLink->enabled)
                {
                    solverEvent.cmd = FM_SOLVER_CMD_LINK_ENABLED;
                }
                else
                {
                    solverEvent.cmd = FM_SOLVER_CMD_LINK_DISABLED;
                }

                FM_MEMCPY_S( &solverEvent.info.linkEvent.link,
                             sizeof(solverEvent.info.linkEvent.link),
                             &intLink->link,
                             sizeof(fm_swagLink) );
                status = aggExt->solver(swagId, &solverEvent);
            }
            else
            {
                status = FM_OK;
            }

            break;

        default:
            status = FM_ERR_INVALID_ATTRIB;
            break;
    } /* end switch (attr) */

ABORT:

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmSetSWAGLinkAttribute */




/*****************************************************************************/
/** fmGetSWAGLinkAttribute
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Get a switch aggregate link attribute.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to
 *                  operate.
 *
 * \param[in]       logicalPort is the logical port number associated with
 *                  the link whose attribute is to be retrieved. A link must
 *                  have been added in a prior call to ''fmAddSWAGLink'' with
 *                  this value for either the logicalPort or partnerLogicalPort.
 *
 * \param[in]       attr is the switch aggregate link attribute to get
 *                  (see 'Switch Aggregate Link Attributes').
 *
 * \param[in]       value points to caller-allocated storage where this
 *                  function is to place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_NOT_FOUND if logicalPort does not identify an
 *                  existing link in the switch aggregate.
 * \return          FM_ERR_INVALID_ATTRIB if attr is unrecognized.
 *
 *****************************************************************************/
fm_status fmGetSWAGLinkAttribute(fm_int swagId,
                                 fm_int logicalPort,
                                 fm_int attr,
                                 void * value)
{
    fm_status       status;
    fm_switch *     aggSwPtr;
    fmSWAG_switch * aggExt;
    fm_swagIntLink *intLink;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, logicalPort=%d, attr=%d, value=%p\n",
                     swagId,
                     logicalPort,
                     attr,
                     value);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    aggSwPtr = fmRootApi->fmSwitchStateTable[swagId];
    aggExt   = aggSwPtr->extension;

    /**************************************************
     * Find the link
     **************************************************/
    intLink = fmFindPortInLinkList(aggExt, logicalPort);

    if (intLink == NULL)
    {
        status = FM_ERR_NOT_FOUND;
        goto ABORT;
    }

    switch (attr)
    {
        case FM_LINK_ATTR_ADMIN_STATE:
            *( (fm_bool *) value ) = intLink->enabled;
            status                 = FM_OK;
            break;

        default:
            status = FM_ERR_INVALID_ATTRIB;
            break;
    }

ABORT:

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmGetSWAGLinkAttribute */




/*****************************************************************************/
/** fmGetSWAGLinkFirst
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Retrieve the first link in a switch aggregate.
 *
 * \param[in]       swagId is the ID of the switch aggregate from which to
 *                  retrieve the link.
 *
 * \param[out]      link points to caller-allocated storage where this
 *                  function should place the first link in the switch aggregate
 *                  (see 'fm_swagLink').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_NO_MORE if there are no links in the switch aggregate.
 * \return          FM_ERR_INVALID_ARGUMENT if link is NULL.
 *
 *****************************************************************************/
fm_status fmGetSWAGLinkFirst(fm_int       swagId,
                             fm_swagLink *link)
{
    fmSWAG_switch * aggExt;
    fm_swagIntLink *linkPtr;
    fm_status       err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    aggExt = fmRootApi->fmSwitchStateTable[swagId]->extension;

    linkPtr = fmGetFirstLinkInLinkList(aggExt);

    if (linkPtr == NULL)
    {
        err = FM_ERR_NO_MORE;
    }
    else if (link == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
    }
    else
    {
        FM_MEMCPY_S( link, sizeof(*link), &linkPtr->link, sizeof(fm_swagLink) );
        err = FM_OK;
    }

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, err);

}   /* end fmGetSWAGLinkFirst */




/*****************************************************************************/
/** fmGetSWAGLinkNext
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Retrieve the next link in a switch aggregate following a
 *                  prior call to this function or to ''fmGetSWAGLinkFirst''.
 *
 * \param[in]       swagId is the ID of the switch aggregate from which to
 *                  retrieve the link.
 *
 * \param[in]       startLink points to the last link found by a previous
 *                  call to this function or to ''fmGetSWAGLinkFirst''.
 *
 * \param[out]      nextLink points to caller-allocated storage where this
 *                  function should place the next link in the switch aggregate
 *                  (see 'fm_swagLink').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_NOT_FOUND if startLink is not in the link list.
 * \return          FM_ERR_NO_MORE if there are no more links in the aggregate.
 * \return          FM_ERR_INVALID_ARGUMENT if startLink or nextLink are NULL.
 *
 *****************************************************************************/
fm_status fmGetSWAGLinkNext(fm_int       swagId,
                            fm_swagLink *startLink,
                            fm_swagLink *nextLink)
{
    fmSWAG_switch * aggExt;
    fm_swagIntLink *linkPtr;
    fm_status       err;
    fm_swagIntLink  key;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    if ( (startLink == NULL) || (nextLink == NULL) )
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    aggExt = fmRootApi->fmSwitchStateTable[swagId]->extension;

    FM_MEMCPY_S( &key.link, sizeof(key.link), startLink, sizeof(fm_swagLink) );

    err = fmCustomTreeFind(&aggExt->links, &key, (void **) &linkPtr);

    if (err == FM_OK)
    {
        linkPtr = fmGetNextLinkInLinkList(linkPtr);

        if (linkPtr != NULL)
        {
            FM_MEMCPY_S( nextLink,
                         sizeof(*nextLink),
                         &linkPtr->link,
                         sizeof(fm_swagLink) );
        }
        else
        {
            err = FM_ERR_NO_MORE;
        }
    }

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, err);

}   /* end  fmGetSWAGLinkNext */




/*****************************************************************************/
/** fmSetSWAGTopology
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Set the topology of a switch aggregate.  If the topology is
 *                  changed, the topology solver will be reset to the default
 *                  topology solver for the topology or to NULL.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to
 *                  operate.
 *
 * \param[in]       topology is the switch aggregate topology type
 *                  (see ''fm_swagTopology'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 *
 *****************************************************************************/
fm_status fmSetSWAGTopology(fm_int swagId, fm_swagTopology topology)
{
    fm_switch *    switchPtr;
    fmSWAG_switch *swag;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, topology=%d\n",
                     swagId,
                     topology);

    switch (topology)
    {
#if FM_SUPPORT_SWAG

#if FM_INCLUDE_RING_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_RING:
            break;
#endif

#if FM_INCLUDE_FAT_TREE_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_FAT_TREE:
            break;
#endif

#if FM_INCLUDE_MESH_TOPOLOGY == FM_ENABLED
        case FM_SWAG_TOPOLOGY_MESH:
            break;
#endif

#endif

        default:
            FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);

    } /* end switch (topology) */

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    switchPtr = fmRootApi->fmSwitchStateTable[swagId];
    swag      = (fmSWAG_switch *) switchPtr->extension;

    SetTopology(swagId, swag, topology, TRUE);

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_OK);

}   /* end  fmSetSWAGTopology */




/*****************************************************************************/
/** fmGetSWAGTopology
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Get the topology of a switch aggregate.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to operate.
 *
 * \param[out]      topology points to caller-allocated storage where this
 *                  function should place the switch aggregate topology (see
 *                  ''fm_swagTopology'').
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if topology is NULL.
 *
 *****************************************************************************/
fm_status fmGetSWAGTopology(fm_int swagId, fm_swagTopology *topology)
{
    fm_switch *    switchPtr;
    fmSWAG_switch *swag;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId=%d, topology=%p\n",
                     swagId,
                     (void *) topology);

    if (topology == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    switchPtr = fmRootApi->fmSwitchStateTable[swagId];
    swag      = (fmSWAG_switch *) switchPtr->extension;

    *topology = swag->topology;

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_OK);

}   /* end  fmGetSWAGTopology */




/*****************************************************************************/
/** fmSetSWAGTopologySolver
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Set the topology solver function for a switch aggregate.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to operate.
 *
 * \param[in]       solver points to the topology solver function that should
 *                  be called on switch aggregate configuration changes. May be
 *                  specified as NULL to disable calls for user-defined
 *                  topologies or to use the API's built-in solvers for
 *                  standard topologies. See 'fm_swagTopologySolver' for a
 *                  complete description of this argument type.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if swagId is invalid.
 *
 *****************************************************************************/
fm_status fmSetSWAGTopologySolver(fm_int                swagId,
                                  fm_swagTopologySolver solver)
{
    fm_status      status;
    fm_switch *    switchPtr;
    fmSWAG_switch *swag;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    switchPtr = fmRootApi->fmSwitchStateTable[swagId];
    swag      = (fmSWAG_switch *) switchPtr->extension;

    status = SetTopologySolver(swagId, swag, solver, TRUE);

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end  fmSetSWAGTopologySolver */




/*****************************************************************************/
/** fmGetSWAGTopologySolver
 * \ingroup swag
 *
 * \chips           FM3000, FM4000
 *
 * \desc            Get the topology solver function for a switch aggregate.
 *
 * \param[in]       swagId is the ID of the switch aggregate on which to operate.
 *
 * \param[out]      solver points to caller-allocated storage where this
 *                  function should place the switch aggregate topology solver
 *                  pointer.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if solver is NULL.
 *
 *****************************************************************************/
fm_status fmGetSWAGTopologySolver(fm_int                 swagId,
                                  fm_swagTopologySolver *solver)
{
    fm_switch *    switchPtr;
    fmSWAG_switch *swag;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    if (solver == NULL)
    {
        FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    switchPtr = fmRootApi->fmSwitchStateTable[swagId];
    swag      = (fmSWAG_switch *) switchPtr->extension;

    *solver = swag->solver;

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, FM_OK);

}   /* end  fmGetSWAGTopologySolver */




/*****************************************************************************/
/** fmHandleSWAGSwitchStateChange
 * \ingroup intSwag
 *
 * \desc            Handles switch aggregate topology changes when a switch
 *                  in the aggregate changes state (up/down).
 * \note            This function expects that no switch locks have been
 *                  taken by caller code for either of the switches.  Failure
 *                  to honor this could lead to thread deadlocks.
 *
 * \param[in]       sw is the ID of the switch which changed state.
 *
 * \param[in]       swagId is the ID of the switch aggregate to which the
 *                  switch belongs.
 *
 * \param[in]       state is the new state of the switch.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmHandleSWAGSwitchStateChange(fm_int  sw,
                                        fm_int  swagId,
                                        fm_bool state)
{
    fm_switch *              switchPtr;
    fm_switch *              swagPtr;
    fmSWAG_switch *          swagExt;
    fm_status                status;
    fm_bool                  swagLocked  = FALSE;
    fm_bool                  swProtected = FALSE;
    fm_int                   newSWAGId;
    fm_swagMember *          member;
    fm_topologySolverEvent   solverEvent;
    fm_int                   i;
    fm_swagIntLink *         intLink;
    fm_islTagFormat          tagFormat;
    fm_int                   port;
    fm_bool                  internal;
    fm_port *                portPtr;
    fm_swagIntTrunk *        trunkPtr;
    fmSWAG_allocLags *       lagAlloc;
    fmSWAG_allocMcastGroups *mcastAlloc;
    fmSWAG_allocLbgs *       lbgAlloc;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw=%d, swagId=%d\n", sw, swagId);

RESTART:

    if ( (swagId < 0) || (swagId >= FM_MAX_NUM_SWITCHES) )
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    if ( !SWITCH_LOCK_EXISTS(swagId) )
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    /* Lock the switch aggregate */
    LOCK_SWITCH(swagId);
    swagLocked = TRUE;
    swagPtr    = GET_SWITCH_PTR(swagId);

    if (swagPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH_AGGREGATE;
        goto ABORT;
    }

    swagExt = GET_SWITCH_EXT(swagId);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    if ( !SWITCH_LOCK_EXISTS(sw) )
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    /* Protect the switch */
    PROTECT_SWITCH(sw);
    swProtected = TRUE;
    switchPtr   = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        status = FM_ERR_INVALID_SWITCH;
        goto ABORT;
    }

    newSWAGId = switchPtr->swag;

    if (newSWAGId != swagId)
    {
        /* The switch is now a member of a different aggregate */
        FM_LOG_INFO(FM_LOG_CAT_SWAG,
                    "switch Aggregate ID changed during state change, sw=%d, "
                    "old aggregate=%d, new aggregate=%d\n",
                    sw,
                    swagId,
                    newSWAGId);

        if (newSWAGId >= 0)
        {
            UNPROTECT_SWITCH(sw);
            swProtected = FALSE;

            UNLOCK_SWITCH(swagId);
            swagLocked = FALSE;

            swagId = newSWAGId;
            goto RESTART;
        }

        /* The switch is no longer a member of the switch aggregate, abort out */
        status = FM_OK;
        goto ABORT;
    }

    /* Find the member switch entry in the switch aggregate */
    member = fmFindSwitchInSWAG(swagExt, sw);

    if (member == NULL)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWAG,
                     "switch in aggregate not in linked-list: sw=%d, "
                     "swagId=%d\n",
                     sw,
                     swagId);
        status = FM_FAIL;
        goto ABORT;
    }

    /* Save the new state of the switch */
    member->appState = state;

    /* If the switch is now up, proceed with initial configuration. */
    if ( state)
    {
        for (i = 0 ; i < FM_SWAG_MAX_LAG_ALLOCS ; i++)
        {
            lagAlloc = &swagExt->logicalPortInfo.allocLagsEntry[i];

            if (lagAlloc->glortSize != 0)
            {
                status = fmAllocateStackLAGs(sw,
                                             lagAlloc->glort,
                                             lagAlloc->glortSize,
                                             &lagAlloc->memberBaseHandles[sw],
                                             &lagAlloc->memberNumHandles[sw],
                                             &lagAlloc->memberHandleSteps[sw]);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

                FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                             "LAG glort space allocated for switch %d, "
                             "glort=%X, glortSize=%d, baseHandle=%d, "
                             "numHandles=%d, handleStep=%d\n",
                             sw,
                             lagAlloc->glort,
                             lagAlloc->glortSize,
                             lagAlloc->memberBaseHandles[sw],
                             lagAlloc->memberNumHandles[sw],
                             lagAlloc->memberHandleSteps[sw]);
            }
        }

        for (i = 0 ; i < FM_SWAG_MAX_MCAST_GROUP_ALLOCS ; i++)
        {
            mcastAlloc = &swagExt->logicalPortInfo.allocMcastGroupsEntry[i];

            if (mcastAlloc->glortSize != 0)
            {
                status = fmAllocateStackMcastGroups(sw,
                                                    mcastAlloc->glort,
                                                    mcastAlloc->glortSize,
                                                    &mcastAlloc->memberBaseHandles[sw],
                                                    &mcastAlloc->memberNumHandles[sw],
                                                    &mcastAlloc->memberHandleSteps[sw]);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

                FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                             "Mcast glort space allocated for switch %d, "
                             "glort=%X, glortSize=%d, baseHandle=%d, "
                             "numHandles=%d, handleStep=%d\n",
                             sw,
                             mcastAlloc->glort,
                             mcastAlloc->glortSize,
                             mcastAlloc->memberBaseHandles[sw],
                             mcastAlloc->memberNumHandles[sw],
                             mcastAlloc->memberHandleSteps[sw]);
            }
        }

        for (i = 0 ; i < FM_SWAG_MAX_LBG_ALLOCS ; i++)
        {
            lbgAlloc = &swagExt->logicalPortInfo.allocLbgsEntry[i];

            if (lbgAlloc->glortSize != 0)
            {
                status = fmAllocateStackLBGs(sw,
                                             lbgAlloc->glort,
                                             lbgAlloc->glortSize,
                                             &lbgAlloc->memberBaseHandles[sw],
                                             &lbgAlloc->memberNumHandles[sw],
                                             &lbgAlloc->memberHandleSteps[sw]);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

                FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                             "LBG glort space allocated for switch %d, "
                             "glort=%X, glortSize=%d, baseHandle=%d, "
                             "numHandles=%d, handleStep=%d\n",
                             sw,
                             lbgAlloc->glort,
                             lbgAlloc->glortSize,
                             lbgAlloc->memberBaseHandles[sw],
                             lbgAlloc->memberNumHandles[sw],
                             lbgAlloc->memberHandleSteps[sw]);
            }
        }

        /* Configure the switch attributes */
        status = fmApplySWAGAttributesToSwitch(swagId, sw);

        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

        /* Configure all internal and CPU ports */
        /* Initialize port 0 */
        portPtr               = GET_PORT_PTR(sw, 0);
        portPtr->swagPort     = 0;
        portPtr->swagLinkType = FM_SWAG_LINK_CPU;

        /* Process all internal links */
        intLink = fmGetFirstLinkInLinkList(swagExt);

        while (intLink != NULL)
        {
            if (intLink->link.type == FM_SWAG_LINK_INTERNAL)
            {
                if (intLink->link.swId == sw)
                {
                    port = intLink->link.logicalPort;
                }
                else if (intLink->link.partnerSwitch == sw)
                {
                    port = intLink->link.partnerLogicalPort;
                }
                else
                {
                    port = -1;
                }

                tagFormat = FM_ISL_TAG_F64;
                internal  = TRUE;
            }
            else
            {
                if (intLink->link.swId == sw)
                {
                    port = intLink->link.logicalPort;
                    if (fmFibmSlaveIsLogicalPortMgmt(sw, port))
                    {
                        tagFormat = FM_ISL_TAG_F64;
                        internal  = TRUE;
                    }
                    else
                    {
                        tagFormat = FM_ISL_TAG_NONE;
                        internal  = FALSE;
                    }
                }
                else
                {
                    port = -1;
                }
            }

            if (port >= 0)
            {
                /* If the port is internal and auto-management of internal
                 * ports is enabled, enable the port.
                 */
                if ( internal &&
                    fmGetBoolApiAttribute(FM_AAK_API_SWAG_AUTO_INTERNAL_PORTS,
                                          FM_AAD_API_SWAG_AUTO_INTERNAL_PORTS) )
                {
                    status = fmSetPortState(swagId, port, FM_PORT_STATE_UP, 0);

                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
                }

                status = fmSetPortAttribute(swagId,
                                            port,
                                            FM_PORT_ISL_TAG_FORMAT,
                                            &tagFormat);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

                status = fmSetPortAttribute(swagId,
                                            port,
                                            FM_PORT_INTERNAL,
                                            &internal);

                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);
            }

            intLink = fmGetNextLinkInLinkList(intLink);
        }
    }

    /* Inform the topology solver */
    if (swagExt->solver != NULL)
    {
        if (state)
        {
            solverEvent.cmd = FM_SOLVER_CMD_SWITCH_UP;
        }
        else
        {
            solverEvent.cmd = FM_SOLVER_CMD_SWITCH_DOWN;
        }

        solverEvent.info.switchEvent.sw = sw;

        status = swagExt->solver(swagId, &solverEvent);
    }
    else
    {
        status = FM_OK;
    }

    if (status == FM_OK)
    {
        status = fmUpdateRemotePorts(swagId);
    }

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, status);

    /* Deactivate all trunk links that refer to this switch */
    trunkPtr = swagExt->firstTrunk;

    while (trunkPtr != NULL)
    {
        if ( (trunkPtr->trunk.trunkType == FM_SWAG_LINK_INTERNAL)
            && trunkPtr->trunk.isEnabled
            && ( (trunkPtr->trunk.trunkSw == sw)
                || (trunkPtr->trunk.destSw == sw) ) )
        {
            intLink = trunkPtr->firstLink;

            while (intLink != NULL)
            {
                if (intLink->link.swId == sw)
                {
                    port = intLink->link.logicalPort;
                }
                else if (intLink->link.partnerSwitch == sw)
                {
                    port = intLink->link.partnerLogicalPort;
                }
                else
                {
                    port = -1;
                }

                if (port >= 0)
                {
                    status = fmDeactivateTrunkLink(swagId,
                                                   trunkPtr,
                                                   intLink);

                    if (status != FM_OK)
                    {
                        goto ABORT;
                    }
                }

                status = fmSetPortState(swagId,
                                        port,
                                        FM_PORT_STATE_ADMIN_PWRDOWN,
                                        0);

                switch (status)
                {
                    case FM_OK:
                        break;

                    case FM_ERR_INVALID_PORT:
                        status = FM_OK;
                        break;

                    default:
                        goto ABORT;
                }

                intLink = intLink->nextTrunkLink;
            }
        }

        trunkPtr = trunkPtr->nextTrunk;
    }


ABORT:

    if (swProtected)
    {
        UNPROTECT_SWITCH(sw);
    }

    if (swagLocked)
    {
        UNLOCK_SWITCH(swagId);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmHandleSWAGSwitchStateChange */




/*****************************************************************************/
/** fmFindSwitchInSWAG
 * \ingroup intSwag
 *
 * \desc            Searches a switch linked-list in a switch aggregate for a
 *                  specific switch.
 *
 * \param[in]       aggregatePtr points to the switch aggregate extension table.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          Pointer to the switch entry in the linked-list.
 * \return          NULL if not found.
 *
 *
 *****************************************************************************/
fm_swagMember *fmFindSwitchInSWAG(fmSWAG_switch *aggregatePtr,
                                  fm_int         sw)
{
    fm_swagMember *curSwitch;

    curSwitch = fmGetFirstSwitchInSWAG(aggregatePtr);

    while (curSwitch != NULL)
    {
        if (curSwitch->swId == sw)
        {
            break;
        }

        curSwitch = fmGetNextSwitchInSWAG(curSwitch);
    }

    return curSwitch;

}   /* end fmFindSwitchInSWAG */




/*****************************************************************************/
/** fmSWAGValidateAndProtectSubSwitch
 * \ingroup intSwag
 *
 * \desc            Called to validate the existence of a sub-switch in a
 *                  switch aggregate and protect the switch if it exists.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmSWAGValidateAndProtectSubSwitch(fm_int swagId, fm_int sw)
{
    fm_switch *subSwPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId=%d, sw=%d\n", swagId, sw);

    FM_NOT_USED(swagId);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH);
    }

    if ( !SWITCH_LOCK_EXISTS(sw) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH);
    }

    /* Take read access to the switch lock */
    PROTECT_SWITCH(sw);

    subSwPtr = GET_SWITCH_PTR(sw);

    if (subSwPtr == NULL)
    {
        UNPROTECT_SWITCH(sw);
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_UP);
    }

    if ( (subSwPtr->state < FM_SWITCH_STATE_INIT)
        || (subSwPtr->state > FM_SWITCH_STATE_GOING_DOWN) )
    {
        UNPROTECT_SWITCH(sw);
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_UP);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmSWAGValidateAndProtectSubSwitch */




/*****************************************************************************/
/** fmApplySWAGAttributesToSwitch
 * \ingroup intSwag
 *
 * \desc            Applies switch attributes to a switch in a switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate ID.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmApplySWAGAttributesToSwitch(fm_int swagId,
                                        fm_int sw)
{
    fm_status         err = FM_OK;

#if FM_SUPPORT_SWAG

    fm_switch *       switchPtr;
    fmSWAG_switch *   ext;
    struct attrEntry
    {
        fm_int  attr;
        fm_uint offset;

    }                 attrTable[] =
    {
        { FM_VLAN_TUNNELING,                  offsetof(fmSWAG_switch, vlanTunnelMode)       },
        { FM_CPU_MAC,                         offsetof(fmSWAG_switch, cpuMac)               },
        { FM_TRAP_IEEE_8021X,                 offsetof(fmSWAG_switch, trap8021x)            },
        { FM_TRAP_IEEE_BPDU,                  offsetof(fmSWAG_switch, trapBpdu)             },
        { FM_TRAP_IEEE_LACP,                  offsetof(fmSWAG_switch, trapLacp)             },
        { FM_TRAP_IEEE_GARP,                  offsetof(fmSWAG_switch, trapGarp)             },
        { FM_TRAP_IEEE_OTHER,                 offsetof(fmSWAG_switch, trapOther)            },
        { FM_DROP_PAUSE,                      offsetof(fmSWAG_switch, dropPause)            },
        { FM_TRAP_ETH_TYPE,                   offsetof(fmSWAG_switch, ethTypeTrap)          },
        { FM_TRAP_MTU_VIOLATIONS,             offsetof(fmSWAG_switch, trapMtuViolations)    },
        { FM_MCAST_FLOODING,                  offsetof(fmSWAG_switch, mcastFlooding)        },
        { FM_UCAST_FLOODING,                  offsetof(fmSWAG_switch, ucastFlooding)        },
        { FM_FRAME_AGING_TIME_MSEC,           offsetof(fmSWAG_switch, frameTimeoutMsecs)    },
        { FM_LAG_MODE,                        offsetof(fmSWAG_switch, lagMode)              },
        { FM_ROUTING_HASH,                    offsetof(fmSWAG_switch, routingHash)          },
        { FM_ROUTING_HASH_ROTATION,           offsetof(fmSWAG_switch, hashRotation)         },
        { FM_ROUTING_HASH_PROT_1,             offsetof(fmSWAG_switch, hashProt1)            },
        { FM_ROUTING_HASH_PROT_2,             offsetof(fmSWAG_switch, hashProt2)            },
        { FM_ROUTING_HASH_FLOW_DIFFSERV_MASK, offsetof(fmSWAG_switch, hashFlowDiffservMask) },
        { FM_ROUTING_HASH_FLOW_USER_MASK,     offsetof(fmSWAG_switch, hashFlowUserMask)     },
        { FM_ROUTING_HASH_FLOW_LABEL_MASK,    offsetof(fmSWAG_switch, hashFlowLabelMask)    },
        { FM_REMAP_IEEE_SP15,                 offsetof(fmSWAG_switch, remapIeee)            },
        { FM_REMAP_CPU_SP15,                  offsetof(fmSWAG_switch, remapCpu)             },
        { FM_REMAP_ET_SP15,                   offsetof(fmSWAG_switch, remapEthType)         },
        { FM_VLAN_TYPE,                       offsetof(fmSWAG_switch, vlanType)             },
        { FM_RESERVED_VLAN,                   offsetof(fmSWAG_switch, reservedVlan)         },
        { FM_IP_OPTIONS_DISPOSITION,          offsetof(fmSWAG_switch, optionDisposition)    },
        { FM_STAT_GROUP_ENABLE,               offsetof(fmSWAG_switch, statGroupEnable)      },
        { FM_LBG_MODE,                        offsetof(fmSWAG_switch, lbgMode)              },
        { FM_TRAP_PLUS_LOG,                   offsetof(fmSWAG_switch, trapPlusLog)          },
        { FM_FFU_SLICE_ALLOCATIONS,           offsetof(fmSWAG_switch, sliceAlloc)           },

        /* Must be last */
        { -1,                                 -1 }
    };
    struct attrEntry *curAttr;
    void *            value;
    fm_int            i;
    fm_mtuEntry       mtuEntry;
    fm_uint32         lagHash;
    fm_int            attr;
    fm_switch *       curSwPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId=%d, sw=%d\n", swagId, sw);

    switchPtr = GET_SWITCH_PTR(swagId);
    ext       = GET_SWITCH_EXT(swagId);

    curAttr = attrTable;

    while ( (err == FM_OK) && (curAttr->attr >= 0) )
    {
        value = (void *) ( ( (char *) ext ) + curAttr->offset );

        err = fmSetSwitchAttribute(sw, curAttr->attr, value);

        switch (err)
        {
            case FM_OK:
                break;

            case FM_ERR_INVALID_SWITCH:
                break;

            case FM_ERR_UNSUPPORTED:
            case FM_ERR_INVALID_ATTRIB:
            case FM_ERR_VLAN_ALREADY_EXISTS:
                err = FM_OK;
                break;

            default:
                break;
        } /* end switch (err) */

        curAttr++;
    }

    if (err == FM_ERR_INVALID_SWITCH)
    {
        err = FM_OK;
    }

    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* Configure switch as Master or Slave */
    err = fmConfigureSWAGSwitchMaster(sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* Set Spanning Tree Mode */
    err = fmSetSwitchAttribute(sw,
                               FM_SPANNING_TREE_MODE,
                               &switchPtr->stpMode);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* Apply MAC Table Attributes */
    err = fmSWAGApplyMACAttributesToSwitch(swagId, sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* create and configure vlans */
    err = fmSWAGCreateSWAGVlansForSwitch(swagId, sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* Workaround Bali silicon bug with different lag hashing of untagged and
     * tagged frames when hashing includes VLAN_ID and VLAN_PRI by disabling
     * those hashing fields.
     */
    curSwPtr = GET_SWITCH_PTR(sw);
    switch (curSwPtr->switchFamily)
    {
        case FM_SWITCH_FAMILY_FM4000:
            err = fmGetLAGAttributeExt(sw, FM_LAG_HASH, 0, &lagHash);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

            if ( lagHash & (FM_LAG_HASH_VLANID | FM_LAG_HASH_VLANPRI) )
            {
                lagHash &= ~(FM_LAG_HASH_VLANID | FM_LAG_HASH_VLANPRI);

                err = fmSetLAGAttributeExt(sw, FM_LAG_HASH, 0, &lagHash);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);
            }
            break;

        default:
            break;
    }

    /* Configure LAG Hash Rotation */
    lagHash = FM_DEFAULT_SWAG_LAG_HASH_ROT_VAL;

#if FM_DEFAULT_SWAG_LAG_HASH_ROTATION == 0
    attr = FM_LAG_HASH_ROTATION_A;
#else
    attr = FM_LAG_HASH_ROTATION_B;
#endif

    err = fmSetLAGAttributeExt(sw, attr, 0, &lagHash);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* create and configure LAG groups */
    err = fmSWAGCreateSWAGLagsForSwitch(swagId, sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* configure QOS */
    err = fmSWAGApplyQOSToSwitch(swagId, sw);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

    /* configure MTU list */
    for (i = 0 ; i < FM_MAX_MTU_ENTRIES ; i++)
    {
        mtuEntry.index = i;
        mtuEntry.mtu   = ext->mtuTable[i];

        err = fmSetSwitchAttribute(sw, FM_MTU_LIST, &mtuEntry);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);
    }

    /* Configure L2 and L3 hash profiles */
    switch (curSwPtr->switchFamily)
    {
        case FM_SWITCH_FAMILY_FM4000:
            err = fmSetSwitchAttribute(sw, FM_L2_HASH_KEY, &ext->l2HashKey[0]);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);
            break;

        case FM_SWITCH_FAMILY_FM6000:
            for (i = 0 ; i < FM_SWAG_MAX_HASH_PROFILES ; i++)
            {
                err = fmSetSwitchAttribute(sw, FM_L2_HASH_KEY, &ext->l2HashKey[i]);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

                err = fmSetSwitchAttribute(sw, FM_L2_HASH_ROT_A, &ext->l2HashRotA[i]);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

                err = fmSetSwitchAttribute(sw, FM_L2_HASH_ROT_B, &ext->l2HashRotB[i]);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);

                err = fmSetSwitchAttribute(sw, FM_L3_HASH_CONFIG, &ext->l3HashCfg[i]);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWAG, err);
            }
            break;

        default:
            break;
    }
#else

    FM_NOT_USED(swagId);
    FM_NOT_USED(sw);

#endif

ABORT:

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, err);

}   /* end fmApplySWAGAttributesToSwitch */




/*****************************************************************************/
/** fmFindLinkInLinkList
 * \ingroup intSwag
 *
 * \desc            Find an internal link entry in a switch aggregate
 *                  internal link linked-list.
 *
 * \param[in]       switchExt points to the switch aggregate table.
 *
 * \param[in]       link points to a definition of the link to be found.
 *
 * \return          Pointer to the internal link entry for this link.
 * \return          NULL if not found.
 *
 *****************************************************************************/
fm_swagIntLink *fmFindLinkInLinkList(fmSWAG_switch *switchExt,
                                     fm_swagLink *  link)
{
    fm_swagIntLink *value;
    fm_swagIntLink  key;
    fm_status       status;

    FM_MEMCPY_S( &key.link, sizeof(key.link), link, sizeof(fm_swagLink) );
    key.ext = switchExt;
    status  = fmCustomTreeFind(&switchExt->links,
                               (void *) &key,
                               (void **) &value);

    if (status != FM_OK)
    {
        value = NULL;
    }

    return value;

}   /* end fmFindLinkInLinkList */




/*****************************************************************************/
/** fmFindPortInLinkList
 * \ingroup intSwag
 *
 * \desc            Find an internal link entry in a switch aggregate internal
 *                  link linked-list given the logical port number.
 *
 * \param[in]       switchExt points to the switch aggregate table.
 *
 * \param[in]       port contains the logical port number.
 *
 * \return          Pointer to the internal link entry for this link.
 * \return          NULL if not found.
 *
 *****************************************************************************/
fm_swagIntLink *fmFindPortInLinkList(fmSWAG_switch *switchExt,
                                     fm_int         port)
{
    fm_swagIntLink *value;
    fm_swagIntLink  key;
    fm_status       status;

    FM_CLEAR(key);
    key.link.logicalPort = port;
    key.ext              = switchExt;
    status               = fmTreeFind(&switchExt->portLinkTree,
                                      (fm_uint64) port,
                                      (void **) &value);

    if (status != FM_OK)
    {
        value = NULL;
    }

    return value;

}   /* end fmFindPortInLinkList */




/*****************************************************************************/
/** fmGetSwitchPortForSWAGPort
 * \ingroup intSwag
 *
 * \desc            Retrieves the underlying switch and port numbers for
 *                  a switch aggregate switch and port.
 *
 * \param[in]       switchExt points to the switch aggregate table.
 *
 * \param[in]       port contains the logical port number.
 *
 * \param[out]      realSwPtr points to caller-allocated storage into which
 *                  the underlying switch number will be placed.
 *
 * \param[out]      realPortPtr points to caller-allocated storage into which
 *                  the underlying port number will be placed.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if one of the arguments is invalid.
 *
 *****************************************************************************/
fm_status fmGetSwitchPortForSWAGPort(fmSWAG_switch *switchExt,
                                     fm_int         port,
                                     fm_int *       realSwPtr,
                                     fm_int *       realPortPtr)
{
    fmSWAG_port *saPortPtr;
    fm_status    status;
    fm_int       realSw;
    fm_int       realPort;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_SWAG,
                         "switchExt = %p, port = %d, realSwPtr = %p, "
                         "realPortPtr = %p\n",
                         (void *) switchExt,
                         port,
                         (void *) realSwPtr,
                         (void *) realPortPtr);

    if (switchExt != NULL)
    {
        if (port == 0)
        {
            realSw   = switchExt->masterCpuSw;
            realPort = 0;
        }
        else
        {
            saPortPtr = switchExt->base->portTable[port]->extension;
            realSw    = saPortPtr->sw;
            realPort  = saPortPtr->logicalPort;
        }

        if (realSwPtr != NULL)
        {
            *realSwPtr = realSw;
        }

        if (realPortPtr != NULL)
        {
            *realPortPtr = realPort;
        }

        if (realSw != -1)
        {
            status = FM_OK;
        }
        else
        {
            status = FM_ERR_INVALID_SWITCH;
        }
    }
    else
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_SWAG, status);

}   /* end fmGetSwitchPortForSWAGPort */




/*****************************************************************************/
/** fmIsSwitchInASWAG
 * \ingroup intSwag
 *
 * \desc            Determines if a switch is part of a switch aggregate.
 *                  Assumes that NO switch locks have been taken.  May cause
 *                  deadlock if the current thread holds any switch locks.
 *
 * \param[in]       sw is the switch ID of the switch to be checked.
 *
 * \param[out]      swagPtr points to caller-allocated storage into which
 *                  the switch aggregate number will be placed if the switch
 *                  is a member of a switch aggregate.
 *
 * \return          FM_OK if the switch is in a switch aggregate.
 * \return          FM_ERR_SWITCH_NOT_IN_AGGREGATE if the switch is not in a
 *                  switch aggregate.
 *
 *****************************************************************************/
fm_status fmIsSwitchInASWAG(fm_int sw, fm_int *swagPtr)
{
    fm_status      status;
    fm_switch *    swPtr;
    fmSWAG_switch *ext;
    fm_swagMember *saSw;
    fm_int         swagId;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_SWAG,
                         "sw=%d, swagPtr=%p\n",
                         sw,
                         (void *) swagPtr);

    /* Run through each switch aggregate */
    status = fmGetSwitchFirst(&swagId);

    while (status == FM_OK)
    {
        PROTECT_SWITCH(swagId);
        swPtr = fmRootApi->fmSwitchStateTable[swagId];

        if (swPtr != NULL)
        {
            if (swPtr->switchFamily == FM_SWITCH_FAMILY_SWAG)
            {
                ext  = swPtr->extension;
                saSw = fmFindSwitchInSWAG(ext, sw);

                if (saSw != NULL)
                {
                    if (swagPtr != NULL)
                    {
                        *swagPtr = swagId;
                    }

                    UNPROTECT_SWITCH(swagId);
                    break;
                }
            }
        }

        UNPROTECT_SWITCH(swagId);

        status = fmGetSwitchNext(swagId, &swagId);
    }

    if (status == FM_ERR_NO_SWITCHES)
    {
        status = FM_ERR_SWITCH_NOT_IN_AGGREGATE;

        if (swagPtr != NULL)
        {
            *swagPtr = -1;
        }
    }

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_SWAG, status);

}   /* end fmIsSwitchInASWAG */




/*****************************************************************************/
/** fmUpdateSwitchInSWAG
 * \ingroup intSwag
 *
 * \desc            Updates a switch's table based upon the switch aggregate
 *                  to which it is being added or from which it is being
 *                  removed.  Note: This function assumes that all appropriate
 *                  switch locks have been taken.
 *
 * \param[in]       swagId is the Switch Aggregate ID number.
 *
 * \param[in]       sw is the switch ID of the switch.
 *
 * \param[in]       insert is TRUE if the switch is in the switch aggregate,
 *                  FALSE if the switch is being taken out of the switch aggregate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmUpdateSwitchInSWAG(fm_int  swagId,
                               fm_int  sw,
                               fm_bool insert)
{
#if FM_SUPPORT_SWAG
    fm_status       status;
    fm_switch *     swPtr;
    fmSWAG_switch * aggExt;
    fm_port *       portPtr;
    fm_swagIntLink *curLink;
#endif

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "swagId=%d, sw=%d, insert=%d\n",
                 swagId,
                 sw,
                 insert);

#if FM_SUPPORT_SWAG

    swPtr = fmRootApi->fmSwitchStateTable[sw];

    if (insert)
    {
        swPtr->swag         = swagId;
        swPtr->eventHandler = fmSWAGEventHandler;

        if (swPtr->portTable != NULL)
        {
            /* FIXME: Always TRUE */
            if (insert)
            {
                /* Prepare all links for use */
                status = fmSWAGPrepareLinks(swagId, sw);

                if (status != FM_OK)
                {
                    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
                }
            }
            /* FIXME: Always FALSE */
            else
            {
                /* Search through all links for this switch and update the port
                 * tables */
                aggExt  = fmRootApi->fmSwitchStateTable[swagId]->extension;
                curLink = fmGetFirstLinkInLinkList(aggExt);

                while (curLink != NULL)
                {
                    if (curLink->link.swId == sw)
                    {
                        portPtr = swPtr->portTable[curLink->link.swPort];

                        portPtr->swagPort     = -1;
                        portPtr->swagLinkType = FM_SWAG_LINK_UNDEFINED;
                    }

                    if ( (curLink->link.type == FM_SWAG_LINK_INTERNAL)
                        && (curLink->link.partnerSwitch == sw) )
                    {
                        portPtr = swPtr->portTable[curLink->link.partnerPort];

                        portPtr->swagPort     = -1;
                        portPtr->swagLinkType = FM_SWAG_LINK_UNDEFINED;
                    }

                    curLink = fmGetNextLinkInLinkList(curLink);
                }
            }
        }
    }
    else
    {
        swPtr->swag         = -1;
        swPtr->eventHandler = NULL;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

#else

    FM_NOT_USED(swagId);
    FM_NOT_USED(sw);
    FM_NOT_USED(insert);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);

#endif

}   /* end fmUpdateSwitchInSWAG */




/*****************************************************************************/
/** fmAddMACAddressToSWAGSwitches
 * \ingroup intSwag
 *
 * \desc            Adds a MAC address to all switches in a switch aggregate.
 *
 * \param[in]       sw is the switch ID of the switch aggregate.
 *
 * \param[in]       tblEntry points to the address table entry.
 *
 * \param[in]       sourceSwitch is -1 if the address should be added
 *                  to the source switch, i.e., the switch which contains
 *                  the source port.  It is a switch # if the address should
 *                  not be added to that switch.  Use -1 when the application
 *                  is adding an address to the aggregate, switch if the address
 *                  has been learned on the source switch.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmAddMACAddressToSWAGSwitches(fm_int                   sw,
                                        fm_internalMacAddrEntry *tblEntry,
                                        fm_int                   sourceSwitch)
{
    fm_switch *               switchPtr;
    fmSWAG_switch *           switchExt;
    fm_int                    curSw;
    fm_macAddressEntry        curEntry;
    fm_status                 status;
    fm_status                 err;
    fm_swagMember *           srcMember;
    fm_swagMember *           curSwPtr;
    fm_int                    srcSw;
    fm_int                    srcPort;
    fm_port *                 portPtr;
    fmSWAG_port *             portExt;
    fm_portType               portType;
    fm_intMulticastGroup *    mcastGroup;
    fmSWAG_intMulticastGroup *swagGroup;
    fm_int                    trigger;
    fm_bool                   swProtected;
    fm_multicastMacAddress    mcastAddr;
    fm_intMulticastGroup *    curGroup;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw=%d, tblEntry=%p, sourceSwitch=%d\n",
                 sw,
                 (void *) tblEntry,
                 sourceSwitch);

    if (tblEntry == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    swProtected = FALSE;

    /* Validate the logical port number */
    VALIDATE_LOGICAL_PORT(sw, tblEntry->port, ALLOW_ALL);

    FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                 "macAddress=%012llX, destMask=%08X, port=%d, state=%d, "
                 "memError=%d, entryType=%d, trigger=%d, vlanID=%u, "
                 "agingCounter=%llX\n",
                 tblEntry->macAddress,
                 tblEntry->destMask,
                 tblEntry->port,
                 tblEntry->state,
                 tblEntry->memError,
                 tblEntry->entryType,
                 (fm_int) tblEntry->trigger,
                 tblEntry->vlanID,
                 tblEntry->agingCounter);

    switchPtr  = GET_SWITCH_PTR(sw);
    switchExt  = GET_SWITCH_EXT(sw);
    portPtr    = GET_PORT_PTR(sw, tblEntry->port);
    portExt    = portPtr->extension;
    portType   = portPtr->portType;
    mcastGroup = NULL;
    swagGroup  = NULL;

    FM_CLEAR(mcastAddr);

    if (tblEntry->port == 0)
    {
        srcSw        = -1;
        srcPort      = 0;
        sourceSwitch = -1;
    }
    else if (portType == FM_PORT_TYPE_PHYSICAL)
    {
        status = fmGetSwitchPortForSWAGPort(switchExt,
                                            tblEntry->port,
                                            &srcSw,
                                            &srcPort);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        srcMember = fmFindSwitchInSWAG(switchExt, srcSw);

        if (srcMember == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
        }
    }
    else if (portType == FM_PORT_TYPE_REMOTE)
    {
        srcSw     = sourceSwitch;
        srcPort   = tblEntry->port;

        if (srcSw != -1)
        {
            srcMember = fmFindSwitchInSWAG(switchExt, srcSw);

            if (srcMember == NULL)
            {
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
            }
        }
    }
    else
    {
        srcSw   = -1;
        srcPort = 0;

        if (portType != FM_PORT_TYPE_LAG)
        {
            sourceSwitch = -1;
        }

        if (portType == FM_PORT_TYPE_MULTICAST)
        {
            mcastGroup = fmFindMcastGroupByPort(sw, tblEntry->port);

            if (mcastGroup == NULL)
            {
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_FAIL);
            }

            swagGroup = mcastGroup->extension;

            mcastAddr.destMacAddress = tblEntry->macAddress;
            mcastAddr.vlan           = tblEntry->vlanID;
            mcastAddr.vlan2          = tblEntry->vlanID2;
        }
    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->FillInUserEntryFromTable,
                       sw,
                       tblEntry,
                       &curEntry);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    curSwPtr = fmGetFirstSwitchInSWAG(switchExt);

    while (curSwPtr != NULL)
    {
        curSw         = curSwPtr->swId;
        curEntry.port = -1;
        trigger       = -1;

        switch (portType)
        {
            case FM_PORT_TYPE_PHYSICAL:
            case FM_PORT_TYPE_CPU:
            case FM_PORT_TYPE_CPU_MGMT:
                if ( curSw != sourceSwitch )
                {
                    if ( (curSw == srcSw) || (srcPort == 0) )
                    {
                        curEntry.port = srcPort;
                    }
                    else
                    {
                        if (curSwPtr->remotePorts[srcSw][srcPort] >= 0)
                        {
                            curEntry.port = curSwPtr->remotePorts[srcSw][srcPort];
                        }
                    }
                }
                break;

            case FM_PORT_TYPE_REMOTE:
                if (curSw != sourceSwitch)
                {
                    curEntry.port = portExt->memberPorts[curSw];
                }
                break;

            case FM_PORT_TYPE_LAG:
                if (sourceSwitch != curSw)
                {
                    curEntry.port = curSwPtr->lagGroups[portPtr->lagIndex];
                }
                break;

            case FM_PORT_TYPE_MULTICAST:
                if ( (mcastGroup == NULL) || (swagGroup == NULL) )
                {
                    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
                }

                status = fmMcastBuildMacEntry(curSw,
                                              mcastGroup,
                                              &mcastAddr,
                                              &curEntry,
                                              &trigger);

                if (status != FM_OK)
                {
                    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
                }

                curGroup = fmFindMcastGroup(curSw,
                                            curSwPtr->mcastGroups[swagGroup->index]);

                if (curGroup)
                {
                    curEntry.port = curGroup->logicalPort;
                }
                break;

            case FM_PORT_TYPE_LBG:
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);
                break;

            default:
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);
                break;
        }

        if (curEntry.port >= 0)
        {
#if 0
            {
                char macAddr[100];
                fmDbgConvertMacAddressToString(curEntry.macAddress, macAddr);
                fmDbgPrintf("adding mac addr: curSw=%d, addr=%s, vlan=%u, type=%u, "
                            "destMask=%08X, port=%d, age=%d\n",
                            curSw, macAddr, curEntry.vlanID, curEntry.type, curEntry.destMask,
                            curEntry.port, curEntry.age);
            }
#endif

            if (portType == FM_PORT_TYPE_MULTICAST)
            {
                VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(err, curSw);

                if (err == FM_OK)
                {
                    status = fmAddAddressToTable(curSw,
                                                 &curEntry,
                                                 trigger,
                                                 TRUE,
                                                 -1);
                    UNPROTECT_SWITCH(curSw);
                }
            }
            else
            {
                status = fmAddAddress(curSw, &curEntry);
            }

            if (status != FM_OK)
            {
                break;
            }
        }

        curSwPtr = fmGetNextSwitchInSWAG(curSwPtr);

    }   /* end while (curSwPtr != NULL) */

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmAddMACAddressToSWAGSwitches */




/*****************************************************************************/
/** fmDeleteMACAddressFromSWAGSwitches
 * \ingroup intSwag
 *
 * \desc            Deletes a MAC address from all switches in a switch
 *                  aggregate.
 *
 * \param[in]       sw is the switch ID of the switch aggregate.
 *
 * \param[in]       tblEntry points to the address table entry.
 *
 * \param[in]       sourceSwitch is -1 if the address should be
 *                  removed from the source switch, i.e., the switch which
 *                  contains the source port.  It is a switch number if the
 *                  address should not be deleted from that switch.  Use -1 when
 *                  the application is deleting an address to the aggregate,
 *                  or a switch number if the address has been aged from the
 *                  source switch.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmDeleteMACAddressFromSWAGSwitches(fm_int                   sw,
                                             fm_internalMacAddrEntry *tblEntry,
                                             fm_int                   sourceSwitch)
{
    fm_switch *               switchPtr;
    fmSWAG_switch *           switchExt;
    fm_int                    curSw;
    fm_macAddressEntry        curEntry;
    fm_status                 status;
    fm_status                 err;
    fm_swagMember *           srcMember;
    fm_swagMember *           curSwPtr;
    fm_int                    srcSw;
    fm_int                    srcPort;
    fm_port *                 portPtr;
    fm_portType               portType;
    fm_intMulticastGroup *    mcastGroup;
    fmSWAG_intMulticastGroup *swagGroup;
    fm_int                    trigger;
    fm_multicastMacAddress    mcastAddr;
    fm_bool                   swProtected;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw=%d, tblEntry=%p, sourceSwitch=%d\n",
                 sw,
                 (void *) tblEntry,
                 sourceSwitch);

    switchPtr   = GET_SWITCH_PTR(sw);
    switchExt   = GET_SWITCH_EXT(sw);
    mcastGroup  = NULL;
    swagGroup   = NULL;
    swProtected = FALSE;

    /* Validate the logical port number */
    VALIDATE_LOGICAL_PORT(sw, tblEntry->port, ALLOW_ALL);

    portPtr   = GET_PORT_PTR(sw, tblEntry->port);
    portType  = portPtr->portType;

    FM_CLEAR(mcastAddr);

    if (tblEntry->port == 0)
    {
        srcSw        = -1;
        srcPort      = 0;
        sourceSwitch = -1;
    }
    else if (portType == FM_PORT_TYPE_PHYSICAL)
    {
        status = fmGetSwitchPortForSWAGPort(switchExt,
                                            tblEntry->port,
                                            &srcSw,
                                            &srcPort);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        srcMember = fmFindSwitchInSWAG(switchExt, srcSw);

        if (srcMember == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
        }
    }
    else
    {
        srcSw   = -1;
        srcPort = 0;

        if (portType != FM_PORT_TYPE_LAG)
        {
            sourceSwitch = -1;
        }

        if (portType == FM_PORT_TYPE_MULTICAST)
        {
            mcastGroup = fmFindMcastGroupByPort(sw, tblEntry->port);

            if (mcastGroup == NULL)
            {
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_FAIL);
            }

            swagGroup = mcastGroup->extension;

            mcastAddr.destMacAddress = tblEntry->macAddress;
            mcastAddr.vlan           = tblEntry->vlanID;
            mcastAddr.vlan2          = tblEntry->vlanID2;
        }
    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->FillInUserEntryFromTable,
                       sw,
                       tblEntry,
                       &curEntry);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    curSwPtr = fmGetFirstSwitchInSWAG(switchExt);

    while (curSwPtr != NULL)
    {
        curSw         = curSwPtr->swId;
        curEntry.port = -1;
        trigger       = -1;

        switch (portType)
        {
            case FM_PORT_TYPE_PHYSICAL:
            case FM_PORT_TYPE_CPU:
            case FM_PORT_TYPE_CPU_MGMT:
            case FM_PORT_TYPE_REMOTE:
                if ( curSw != sourceSwitch )
                {
                    if ( (curSw == srcSw) || (srcPort == 0) )
                    {
                        curEntry.port = srcPort;
                    }
                    else
                    {
                        if (curSwPtr->remotePorts[srcSw][srcPort] >= 0)
                        {
                            curEntry.port = curSwPtr->remotePorts[srcSw][srcPort];
                        }
                    }
                }
                break;

            case FM_PORT_TYPE_LAG:
                if (sourceSwitch != curSw)
                {
                    curEntry.port = curSwPtr->lagGroups[portPtr->lagIndex];
                }
                break;

            case FM_PORT_TYPE_MULTICAST:
                if ( (mcastGroup == NULL) || (swagGroup == NULL) )
                {
                    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
                }

                curEntry.port = swagGroup->groups[curSw];

                /* add mac address to MAC table */
                status = fmMcastBuildMacEntry(curSw,
                                              mcastGroup,
                                              &mcastAddr,
                                              &curEntry,
                                              &trigger);

                if (status != FM_OK)
                {
                    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
                }
                break;

            case FM_PORT_TYPE_LBG:
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);
                break;

            default:
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);
                break;
        }

        if (curEntry.port >= 0)
        {
#if 0
            {
                char macAddr[100];
                fmDbgConvertMacAddressToString(curEntry.macAddress, macAddr);
                fmDbgPrintf("removing mac addr: curSw=%d, addr=%s, vlan=%u, "
                            "type=%u, destMask=%08X, port=%d, age=%d\n",
                            curSw, macAddr, curEntry.vlanID, curEntry.type,
                            curEntry.destMask, curEntry.port, curEntry.age);
            }
#endif

            if (portType == FM_PORT_TYPE_MULTICAST)
            {
                VALIDATE_AND_PROTECT_SWITCH_NO_RETURN(err, curSw);

                if (err == FM_OK)
                {
                    status = fmDeleteAddressFromTable(curSw,
                                                      &curEntry,
                                                      FALSE,
                                                      TRUE,
                                                      -1);
                    UNPROTECT_SWITCH(curSw);
                }
            }
            else
            {
                status = fmDeleteAddress(curSw, &curEntry);
            }

            if (status == FM_ERR_ADDR_NOT_FOUND)
            {
                status = FM_OK;
            }

            else if (status != FM_OK)
            {
                break;
            }
        }

        curSwPtr = fmGetNextSwitchInSWAG(curSwPtr);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmDeleteMACAddressFromSWAGSwitches */




/*****************************************************************************/
/** fmGetSWAGTrunkForSwitchPair
 * \ingroup intSwag
 *
 * \desc            Returns the trunk for a connection between switches.
 *                  If a trunk record is not found, NULL will be returned.
 *
 * \param[in]       sw is the switch-aggregate id number.
 *
 * \param[in]       sourceSwitch is the underlying switch number for the
 *                  local end of the trunk.
 *
 * \param[in]       destSwitch is the underlying switch number for the
 *                  remote end of the trunk.
 *
 * \return          Returns FM_OK if the trunk was found.
 * \return          Returns FM_ERR_NOT_FOUND if the trunk does not exist.
 *
 *****************************************************************************/
fm_swagIntTrunk *fmGetSWAGTrunkForSwitchPair(fm_int sw,
                                             fm_int sourceSwitch,
                                             fm_int destSwitch)
{
    fm_swagIntTrunk *trunk;
    fm_switch *      switchPtr;
    fmSWAG_switch *  switchExt;
    fm_swagMember *  sourcePtr;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, sourceSwitch = %d, destSwitch = %d\n",
                 sw,
                 sourceSwitch,
                 destSwitch);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    switchExt = switchPtr->extension;
    sourcePtr = fmFindSwitchInSWAG(switchExt, sourceSwitch);

    if (sourcePtr != NULL)
    {
        trunk = sourcePtr->switchTrunks[destSwitch];
    }
    else
    {
        trunk = NULL;
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_SWAG,
                       trunk,
                       "trunk = %p\n",
                       (void *) trunk);

}   /* end fmGetSWAGTrunkForSwitchPair */




/*****************************************************************************/
/** fmIsSWAGLinkEnabled
 * \ingroup swagTopology
 *
 * \desc            Returns TRUE if a SWAG's link is enabled, FALSE
 *                  if the link is disabled or cannot be found.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       port is a switch aggregate logical port # used in the link.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_bool fmIsSWAGLinkEnabled(fm_int sw, fm_int port)
{
    fmSWAG_switch * ext;
    fm_swagIntLink *link;
    fm_bool         enabled;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw = %d, port = %d\n", sw, port);

    ext = fmRootApi->fmSwitchStateTable[sw]->extension;

    link = fmFindPortInLinkList(ext, port);

    if (link != NULL)
    {
        enabled = link->enabled;
    }
    else
    {
        enabled = FALSE;
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_SWAG, enabled, "enabled = %d\n", enabled);

}   /* end fmIsSWAGLinkEnabled */




/*****************************************************************************/
/** fmClearAllSWAGTrunkCrossReferences
 * \ingroup swagTopology
 *
 * \desc            Clear all trunk cross-reference tables.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmClearAllSWAGTrunkCrossReferences(fm_int sw)
{
    fm_switch *    switchPtr;
    fmSWAG_switch *switchExt;
    fm_swagMember *curSwPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw = %d\n", sw);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    switchExt = switchPtr->extension;

    curSwPtr = fmGetFirstSwitchInSWAG(switchExt);

    while (curSwPtr != NULL)
    {
        FM_CLEAR(curSwPtr->switchTrunks);
        curSwPtr = fmGetNextSwitchInSWAG(curSwPtr);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmClearAllSWAGTrunkCrossReferences */




/*****************************************************************************/
/** fmGetFirstTrunk
 * \ingroup swagTopology
 *
 * \desc            Get an ID for the first trunk in a switch aggregate.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[out]      trunkId is a pointer to a void pointer which will
 *                  contain the ID for the first trunk.  This ID may be
 *                  used with fmGetTrunkFromId and fmGetNextTrunk.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MORE if the switch aggregate doesn't contain
 *                  any trunks.
 *
 *****************************************************************************/
fm_status fmGetFirstTrunk(fm_int sw, fm_voidptr *trunkId)
{
    fmSWAG_switch *  switchExt;
    fm_swagIntTrunk *trunk;
    fm_status        status;

    switchExt = fmRootApi->fmSwitchStateTable[sw]->extension;

    trunk = fmGetFirstTrunkInSwitch(switchExt);

    *trunkId = (fm_voidptr) trunk;

    if (trunk != NULL)
    {
        status = FM_OK;
    }
    else
    {
        status = FM_ERR_NO_MORE;
    }

    return status;

}   /* end fmGetFirstTrunk */




/*****************************************************************************/
/** fmGetNextTrunk
 * \ingroup swagTopology
 *
 * \desc            Get a token for the next trunk in a switch aggregate.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in,out]   trunkId is a pointer to a void pointer which contains
 *                  the last trunk ID returned by fmGetFirstTrunk or
 *                  fmGetNextTrunk.  On return from this function, it will
 *                  contain the ID for the next trunk.  This ID may be used
 *                  with fmGetTrunkFromId and fmGetNextTrunk.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MORE if the switch aggregate doesn't contain
 *                  any more trunks.
 *
 *****************************************************************************/
fm_status fmGetNextTrunk(fm_int sw, fm_voidptr *trunkId)
{
    fm_swagIntTrunk *trunk;
    fm_status        status;

    FM_NOT_USED(sw);

    trunk = (fm_swagIntTrunk *) *trunkId;

    if (trunk != NULL)
    {
        trunk = fmGetNextTrunkInSwitch(trunk);
    }

    *trunkId = (fm_voidptr) trunk;

    if (trunk != NULL)
    {
        status = FM_OK;
    }
    else
    {
        status = FM_ERR_NO_MORE;
    }

    return status;


}   /* end fmGetNextTrunk */




/*****************************************************************************/
/** fmGetTrunkFromId
 * \ingroup swagTopology
 *
 * \desc            Get a trunk's information given the trunk's ID.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId is a void pointer containing the ID number
 *                  for the trunk.
 *
 * \param[out]      trunk is a pointer to a trunk structure which will
 *                  contain the information for the trunk.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId is NULL.
 *
 *****************************************************************************/
fm_status fmGetTrunkFromId(fm_int        sw,
                           fm_voidptr    trunkId,
                           fm_swagTrunk *trunk)
{
    fm_swagIntTrunk *trunkPtr;
    fm_status        status;

    FM_NOT_USED(sw);

    trunkPtr = (fm_swagIntTrunk *) trunkId;

    if (trunkPtr != NULL)
    {
        status = FM_OK;
        FM_MEMCPY_S( trunk,
                     sizeof(*trunk),
                     &trunkPtr->trunk,
                     sizeof(fm_swagTrunk) );
    }
    else
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }

    return status;

}   /* end fmGetTrunkFromId */




/*****************************************************************************/
/** fmConnectTrunk
 * \ingroup swagTopology
 *
 * \desc            Specifies the trunk to be used to connect the trunk's
 *                  source switch to a specified destination switch.
 *                  This allows a topology solver to define the inter-switch
 *                  topology.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       destSwitch is the destination switch within the aggregate.
 *
 * \param[in]       trunkId is a void pointer containing the ID number
 *                  for the trunk.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId is invalid or
 *                  the source switch does not exist.
 *
 *****************************************************************************/
fm_status fmConnectTrunk(fm_int     sw,
                         fm_int     destSwitch,
                         fm_voidptr trunkId)
{
    fmSWAG_switch *  switchExt;
    fm_swagMember *  srcMember;
    fm_swagIntTrunk *trunk;
    fm_status        status;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, destSwitch = %d, trunkId = %p\n",
                 sw,
                 destSwitch,
                 (void *) trunkId);

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk != NULL)
    {
        FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                     "isEnabled=%d, isLag=%d, lagId=%d, swagPort=%d, "
                     "trunkSw=%d, trunkPort=%d, destSw=%d, trunkType=%d\n",
                     trunk->trunk.isEnabled,
                     trunk->trunk.isLag,
                     trunk->trunk.lagId,
                     trunk->trunk.swagPort,
                     trunk->trunk.trunkSw,
                     trunk->trunk.trunkPort,
                     trunk->trunk.destSw,
                     trunk->trunk.trunkType);

        switchExt = fmRootApi->fmSwitchStateTable[sw]->extension;
        srcMember = fmFindSwitchInSWAG(switchExt, trunk->trunk.trunkSw);

        if (srcMember != NULL)
        {
            srcMember->switchTrunks[destSwitch] = trunk;

            status = fmUpdateRemotePorts(sw);
        }
        else
        {
            status = FM_ERR_INVALID_ARGUMENT;
        }
    }
    else
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmConnectTrunk */




/*****************************************************************************/
/** fmGetSWAGSwitchRole
 * \ingroup swagTopology
 *
 * \desc            Gets the role for a given switch within a switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \param[in]       sw is the underlying switch number.
 *
 * \return          Switch role if the switch was found in the aggregate.
 * \return          FM_SWITCH_ROLE_UNDEFINED if the switch was not found
 *                  in the topology.
 *
 *****************************************************************************/
fm_switchRole fmGetSWAGSwitchRole(fm_int swagId, fm_int sw)
{
    fmSWAG_switch *switchExt;
    fm_swagMember *swPtr;
    fm_switchRole  role;

    switchExt = fmRootApi->fmSwitchStateTable[swagId]->extension;
    swPtr     = fmFindSwitchInSWAG(switchExt, sw);

    if (swPtr != NULL)
    {
        role = swPtr->role;
    }
    else
    {
        role = FM_SWITCH_ROLE_UNDEFINED;
    }

    return role;

}   /* end fmGetSWAGSwitchRole */




/*****************************************************************************/
/** fmGetFirstLinkFromTrunk
 * \ingroup swagTopology
 *
 * \desc            Returns the first link attached to a given trunk.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId is a void pointer containing the ID number for
 *                  the trunk.
 *
 * \param[out]      linkId is a pointer to a void pointer which will
 *                  contain the ID number for the first link.
 *
 * \param[out]      link is a pointer to caller-allocated memory into which
 *                  the link information will be copied.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MORE if there are no links attached to the trunk.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId is invalid.
 *
 *****************************************************************************/
fm_status fmGetFirstLinkFromTrunk(fm_int       sw,
                                  fm_voidptr   trunkId,
                                  fm_voidptr * linkId,
                                  fm_swagLink *link)
{
    fm_swagIntTrunk *trunkPtr;
    fm_swagIntLink * linkPtr;
    fm_status        status;

    FM_NOT_USED(sw);

    trunkPtr = (fm_swagIntTrunk *) trunkId;

    if (trunkPtr != NULL)
    {
        linkPtr = fmSWAGGetFirstTrunkLink(trunkPtr);

        *linkId = (fm_voidptr) linkPtr;

        if (linkPtr != NULL)
        {
            status = FM_OK;
            FM_MEMCPY_S( link,
                         sizeof(*link),
                         &linkPtr->link,
                         sizeof(fm_swagLink) );
        }
        else
        {
            status = FM_ERR_NO_MORE;
        }
    }
    else
    {
        status = FM_ERR_INVALID_ARGUMENT;
    }

    return status;

}   /* end fmGetFirstLinkFromTrunk */




/*****************************************************************************/
/** fmGetNextLinkFromTrunk
 * \ingroup swagTopology
 *
 * \desc            Returns the next link attached to a given trunk.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in,out]   linkId is a pointer to a void pointer which contains
 *                  the last link ID returned by fmGetFirstLinkFromTrunk or
 *                  fmGetNextLinkFromTrunk.  On return from this function,
 *                  it will contain the ID for the next link in the trunk.
 *
 * \param[out]      link is a pointer to caller-allocated memory into which
 *                  the link information will be copied.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MORE if there are no more links attached to
 *                  the trunk.
 *
 *****************************************************************************/
fm_status fmGetNextLinkFromTrunk(fm_int       sw,
                                 fm_voidptr * linkId,
                                 fm_swagLink *link)
{
    fm_swagIntLink *linkPtr;
    fm_status       status;

    FM_NOT_USED(sw);

    linkPtr = (fm_swagIntLink *) *linkId;

    if (linkPtr != NULL)
    {
        linkPtr = fmSWAGGetNextTrunkLink(linkPtr);
    }

    *linkId = (fm_voidptr) linkPtr;

    if (linkPtr != NULL)
    {
        status = FM_OK;
        FM_MEMCPY_S( link,
                     sizeof(*link),
                     &linkPtr->link,
                     sizeof(fm_swagLink) );
    }
    else
    {
        status = FM_ERR_NO_MORE;
    }

    return status;

}   /* end fmGetNextLinkFromTrunk */




/*****************************************************************************/
/** fmGetSwitchAndPortForSWAGPort
 * \ingroup swagTopology
 *
 * \desc            Gets underlying switch and port numbers for a switch
 *                  aggregate logical port.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \param[in]       swagPort is the aggregate logical port number.
 *
 * \param[out]      sw points to caller-allocated storage which will contain
 *                  the underlying switch number for the specified port.
 *
 * \param[out]      port points to caller-allocated storage which will contain
 *                  the logical port number within the underlying switch that
 *                  corresponds to the specified aggregate logical port.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if the port cannot be found
 *                  within the aggregate.
 *
 *****************************************************************************/
fm_status fmGetSwitchAndPortForSWAGPort(fm_int  swagId,
                                        fm_int  swagPort,
                                        fm_int *sw,
                                        fm_int *port)
{
    fmSWAG_switch *switchExt;
    fm_status      status;

    switchExt = fmRootApi->fmSwitchStateTable[swagId]->extension;

    status = fmGetSwitchPortForSWAGPort(switchExt, swagPort, sw, port);

    return status;

}   /* end fmGetSwitchAndPortForSWAGPort */




/*****************************************************************************/
/** fmFindTrunkForSwitchPair
 * \ingroup swagTopology
 *
 * \desc            Searches for a trunk connecting two specified switches
 *                  within a switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \param[in]       sourceSwitch is the switch number for one end of the trunk.
 *
 * \param[in]       destSwitch is the switch number for the other end of the
 *                  trunk.
 *
 * \param[out]      trunkId is a pointer to a void pointer which will
 *                  contain the ID number for the trunk, if found.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NOT_FOUND if the trunk is not found.
 *
 *****************************************************************************/
fm_status fmFindTrunkForSwitchPair(fm_int      swagId,
                                   fm_int      sourceSwitch,
                                   fm_int      destSwitch,
                                   fm_voidptr *trunkId)
{
    fm_swagIntTrunk *trunk;
    fm_status        status;

    trunk = fmGetSWAGTrunkForSwitchPair(swagId, sourceSwitch, destSwitch);

    *trunkId = (fm_voidptr) trunk;

    if (trunk != NULL)
    {
        status = FM_OK;
    }
    else
    {
        status = FM_ERR_NOT_FOUND;
    }

    return status;

}   /* end fmFindTrunkForSwitchPair */




/*****************************************************************************/
/** fmCreateTrunk
 * \ingroup swagTopology
 *
 * \desc            Creates and initializes a trunk record.
 *
 * \param[in]       sw is the switch-aggregate id number.
 *
 * \param[in]       sourcePort is the logical port number for the trunk.
 *                  The trunk must be associated with a logical port, which
 *                  may be a physical port or a LAG port.
 *
 * \param[in]       trunkType is the type of trunk being created.
 *
 * \param[out]      trunkId is a pointer to a void pointer which will
 *                  contain the ID number for the trunk.
 *
 * \return          Returns FM_OK if the trunk was successfully created.
 *
 *****************************************************************************/
fm_status fmCreateTrunk(fm_int          sw,
                        fm_int          sourcePort,
                        fm_swagLinkType trunkType,
                        fm_voidptr *    trunkId)
{
    fm_switch *      switchPtr;
    fmSWAG_switch *  switchExt;
    fmSWAG_port *    portPtr;
    fm_swagIntTrunk *trunk;
    fm_int           i;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, sourcePort = %d, trunkId = %p\n",
                 sw,
                 sourcePort,
                 (void *) trunkId);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    switchExt = switchPtr->extension;
    portPtr   = switchPtr->portTable[sourcePort]->extension;

    trunk = (fm_swagIntTrunk *) fmAlloc( sizeof(fm_swagIntTrunk) );

    if (trunk == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_NO_MEM);
    }

    FM_CLEAR(*trunk);

    trunk->trunk.isLag     = FALSE;
    trunk->trunk.lagId     = -1;
    trunk->trunk.swagPort  = sourcePort;
    trunk->trunk.trunkSw   = portPtr->sw;
    trunk->trunk.trunkPort = portPtr->logicalPort;
    trunk->trunk.destSw    = -1;
    trunk->trunk.trunkType = trunkType;

    for (i = 0 ; i < FM_MAX_NUM_SWITCHES ; i++)
    {
        trunk->forwardingRuleIds[i] = -1;
    }

    FM_DLL_INIT_LIST(trunk, firstLink, lastLink);
    FM_DLL_INIT_NODE(trunk, nextTrunk, prevTrunk);
    fmAddTrunkToTrunkList(switchExt, trunk);

    *trunkId = (fm_voidptr) trunk;

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmCreateTrunk */




/*****************************************************************************/
/** fmGetTrunkLinkCount
 * \ingroup swagTopology
 *
 * \desc            Returns the number of links in a given trunk.
 *
 * \param[in]       sw is the switch-aggregate id number.
 *
 * \param[in]       trunkId is a void pointer containing the trunk ID.
 *
 * \return          Returns the number of links found in the trunk.
 *
 *****************************************************************************/
fm_int fmGetTrunkLinkCount(fm_int sw, fm_voidptr trunkId)
{
    fm_swagIntLink * link;
    fm_swagIntTrunk *trunk;
    fm_int           count;

    FM_NOT_USED(sw);

    count = 0;
    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        return 0;
    }

    link = fmSWAGGetFirstTrunkLink(trunk);

    while (link != NULL)
    {
        count++;
        link = fmSWAGGetNextTrunkLink(link);
    }

    return count;

}   /* end fmGetTrunkLinkCount */




/*****************************************************************************/
/** fmIsLagTrunk
 * \ingroup swagTopology
 *
 * \desc            Returns the number of links in a given trunk.
 *
 * \param[in]       sw is the switch-aggregate id number.
 *
 * \param[in]       trunkId is a void pointer containing the trunk ID.
 *
 * \return          Returns the number of links found in the trunk.
 *
 *****************************************************************************/
fm_bool fmIsLagTrunk(fm_int sw, fm_voidptr trunkId)
{
    fm_swagIntTrunk *trunk;
    fm_bool          isLag;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p\n",
                 sw,
                 (void *) trunkId);

    FM_NOT_USED(sw);

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        isLag = FALSE;
    }
    else
    {
        isLag = trunk->trunk.isLag;
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_SWAG,
                       isLag,
                       "returning %s\n",
                       (isLag) ? "TRUE" : "FALSE");

}   /* end fmIsLagTrunk */




/*****************************************************************************/
/** fmAttachTrunkToPort
 * \ingroup swagTopology
 *
 * \desc            Attaches a trunk record to a logical port.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId points to the trunk record.
 *
 * \param[in]       port is the logical port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId is invalid.
 *
 *****************************************************************************/
fm_status fmAttachTrunkToPort(fm_int     sw,
                              fm_voidptr trunkId,
                              fm_int     port)
{
    fmSWAG_port *    portExt;
    fm_swagIntTrunk *trunk;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p, port = %d\n",
                 sw,
                 (void *) trunkId,
                 port);

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    portExt = fmRootApi->fmSwitchStateTable[sw]->portTable[port]->extension;

    portExt->trunk = trunk;

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmAttachTrunkToPort */




/*****************************************************************************/
/** fmDetachTrunkFromPort
 * \ingroup swagTopology
 *
 * \desc            Detaches a trunk record from a logical port.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       port is the logical port number.
 *
 * \return          FM_OK.
 *
 *****************************************************************************/
fm_status fmDetachTrunkFromPort(fm_int sw, fm_int port)
{
    fmSWAG_port *portExt;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw = %d, port = %d\n", sw, port);

    portExt = fmRootApi->fmSwitchStateTable[sw]->portTable[port]->extension;

    portExt->trunk = NULL;

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmDetachTrunkFromPort */




/*****************************************************************************/
/** fmAddLinkToTrunk
 * \ingroup swagTopology
 *
 * \desc            Adds a link to a trunk.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId points to the trunk record.
 *
 * \param[in]       link points to a copy of the link record.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId is invalid.
 *
 *****************************************************************************/
fm_status fmAddLinkToTrunk(fm_int sw, fm_voidptr trunkId, fm_swagLink *link)
{
    fmSWAG_switch *  switchExt;
    fm_swagIntTrunk *trunk;
    fm_swagIntLink * intLink;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p, link = %p\n",
                 sw,
                 (void *) trunkId,
                 (void *) link);

    switchExt = fmRootApi->fmSwitchStateTable[sw]->extension;

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    /* Find internal link record */
    intLink = fmFindLinkInLinkList(switchExt, link);

    if (intLink == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    fmSWAGAppendTrunkLink(trunk, intLink);

    if ( (trunk->trunk.destSw < 0)
        && (trunk->trunk.trunkType == FM_SWAG_LINK_INTERNAL) )
    {
        if (trunk->trunk.trunkSw == link->swId)
        {
            trunk->trunk.destSw = link->partnerSwitch;
        }
        else
        {
            trunk->trunk.destSw = link->swId;
        }
    }

    FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                 "added link %p to trunk %p, firstLink = %p, lastLink = %p\n",
                 (void *) intLink,
                 (void *) trunk,
                 (void *) trunk->firstLink,
                 (void *) trunk->lastLink);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmAddLinkToTrunk */




/*****************************************************************************/
/** fmRemoveLinkFromTrunk
 * \ingroup swagTopology
 *
 * \desc            Removes a link from a trunk.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId points to the trunk record.
 *
 * \param[in]       link points to a copy of the link record.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId is invalid or the
 *                  link record cannot be found.
 *
 *****************************************************************************/
fm_status fmRemoveLinkFromTrunk(fm_int       sw,
                                fm_voidptr   trunkId,
                                fm_swagLink *link)
{
    fm_switch *      switchPtr;
    fmSWAG_switch *  switchExt;
    fm_swagIntTrunk *trunk;
    fm_swagIntLink * intLink;
    fm_int           port;
    fmSWAG_port *    portPtr;
    fm_status        status;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p, link = %p\n",
                 sw,
                 (void *) trunkId,
                 (void *) link);

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    switchExt = switchPtr->extension;

    /* Find internal link record */
    intLink = fmFindLinkInLinkList(switchExt, link);

    if (intLink == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    /* If the link is enabled, deactivate it first */
    if (intLink->enabled)
    {
        status = fmDeactivateTrunkLink(sw, trunkId, (fm_voidptr) intLink);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }
    }

    status = FM_OK;

    if (trunk->trunk.trunkSw == link->swId)
    {
        port = link->logicalPort;
    }
    else
    {
        port = link->partnerLogicalPort;
    }

    portPtr        = switchPtr->portTable[port]->extension;
    portPtr->trunk = NULL;

    fmSWAGRemoveTrunkLink(trunk, intLink);

    if ( (trunk->firstLink == NULL) && !trunk->trunk.isLag )
    {
        /* The trunk is now empty of links, delete the trunk */
        portPtr = switchPtr->portTable[trunk->trunk.swagPort]->extension;

        portPtr->trunk = NULL;

        fmRemoveTrunkFromTrunkList(switchExt, trunk);

        fmFree(trunk);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmRemoveLinkFromTrunk */




/*****************************************************************************/
/** fmGetTrunkForPort
 * \ingroup swagTopology
 *
 * \desc            Gets a trunk identifier for a specified logical port.
 *
 * \param[in]       sw is the switch-aggregate id number.
 *
 * \param[in]       port is the logical port number for the trunk.
 *
 * \param[out]      trunkId is a pointer to a void pointer which will
 *                  contain the ID number for the trunk.
 *
 * \return          Returns FM_OK if the trunk was found.
 * \return          Returns FM_ERR_NOT_FOUND if the trunk was not found.
 *
 *****************************************************************************/
fm_status fmGetTrunkForPort(fm_int sw, fm_int port, fm_voidptr *trunkId)
{
    fm_switch *      switchPtr;
    fmSWAG_port *    portPtr;
    fm_status        status;
    fm_swagIntTrunk *trunk;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, port = %d, trunkId = %p\n",
                 sw,
                 port,
                 (void *) trunkId);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    portPtr   = switchPtr->portTable[port]->extension;
    trunk     = portPtr->trunk;

    *trunkId = (fm_voidptr) trunk;

    if (trunk == NULL)
    {
        status = FM_ERR_NOT_FOUND;
    }
    else
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmGetTrunkForPort */




/*****************************************************************************/
/** fmGetLinkForPort
 * \ingroup swagTopology
 *
 * \desc            Gets a link identifier for a specified logical port.
 *
 * \param[in]       sw is the switch-aggregate id number.
 *
 * \param[in]       port is the logical port number for the trunk.
 *
 * \param[out]      linkId is a pointer to a void pointer which will
 *                  contain the ID number for the link.
 *
 * \return          Returns FM_OK if the link was found.
 * \return          Returns FM_ERR_NOT_FOUND if the link was not found.
 *
 *****************************************************************************/
fm_status fmGetLinkForPort(fm_int sw, fm_int port, fm_voidptr *linkId)
{
    fm_switch *     switchPtr;
    fmSWAG_port *   portPtr;
    fm_status       status;
    fm_swagIntLink *link;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, port = %d, linkId = %p\n",
                 sw,
                 port,
                 (void *) linkId);

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    portPtr   = switchPtr->portTable[port]->extension;
    link      = portPtr->linkPtr;

    *linkId = (fm_voidptr) link;

    if (link == NULL)
    {
        status = FM_ERR_NOT_FOUND;
    }
    else
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmGetLinkForPort */




/*****************************************************************************/
/** fmUpdateRemotePorts
 * \ingroup swagTopology
 *
 * \desc            Updates stacking logical ports for all switches
 *                  in a switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if swagId is invalid.
 * \return          FM_ERR_SWITCH_NOT_IN_AGGREGATE is the switch is not
 *                  a member of the specified swag.
 *
 *****************************************************************************/
fm_status fmUpdateRemotePorts(fm_int swagId)
{
#if FM_SUPPORT_SWAG
    fmSWAG_switch *  switchExt;
    fm_status        status;
    fm_swagMember *  curMember;
    fm_int           curSwitch;
    fm_swagMember *  targetMember;
    fm_int           targetSwitch;
    fm_switch *      targetSwitchPtr;
    fm_bool          targetSwitchProtected;
    fm_int           port;
    fm_uint32        glortBase;
    fm_uint32        remoteGlort;
    fm_int           remotePort;
    fm_port *        targetPortPtr;
    fm_port *        curPortPtr;
    fm_int           portMode;
    fm_int           portState;
    fm_int           portInfo[4];
    fm_bool          targetIsUp;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId = %d\n", swagId);

    switchExt = GET_SWITCH_EXT(swagId);

    status    = FM_OK;
    targetMember = fmGetFirstSwitchInSWAG(switchExt);

    while (targetMember != NULL)
    {
        targetIsUp            = FALSE;
        targetSwitch          = targetMember->swId;
        targetSwitchProtected = FALSE;

        /* Does target switch exist? */
        if ( SWITCH_LOCK_EXISTS(targetSwitch) )
        {
            PROTECT_SWITCH(targetSwitch);

            targetSwitchProtected = TRUE;

            targetSwitchPtr = GET_SWITCH_PTR(targetSwitch);

            if (targetSwitchPtr != NULL)
            {
                if (targetSwitchPtr->state == FM_SWITCH_STATE_UP)
                {
                    if (targetSwitchPtr->swag == swagId)
                    {
                        targetIsUp = TRUE;
                    }
                }
                else
                {
                }
            }
        }

        curMember = fmGetFirstSwitchInSWAG(switchExt);

        while (curMember != NULL)
        {
            if (curMember == targetMember)
            {
                goto NEXT_CURMEMBER;
            }

            curSwitch = curMember->swId;

            if (targetIsUp)
            {
                /* Create remote logical ports on the target switch for
                 * all ports on the current switch.
                 */
                targetMember->remotePorts[curSwitch][0] = -1;

                if (curMember->glortSpace == NULL)
                {
                    /* current switch is not up, delete remote
                     * logical ports on target switch for all ports
                     * on the current switch
                     */
                    status = fmReleaseRemoteLogicalPorts(targetMember,
                                                         curSwitch);

                    goto NEXT_CURMEMBER;
                }

                glortBase = curMember->glortSpace->base;

                /* Is there a working path between the target switch and the
                 * current switch? If not, remove all Remote logical ports
                 * related to the current switch since those ports are not
                 * reachable. */
                if (fmFindForwardingRulePortByGlort(targetSwitch,
                                                    glortBase,
                                                    &remotePort) != FM_OK)
                {
                    status = fmReleaseRemoteLogicalPorts(targetMember,
                                                         curSwitch);

                    goto NEXT_CURMEMBER;
                }

                for (port = 1 ;
                     port < FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH ;
                     port++)
                {
                    remoteGlort = glortBase + port;

                    if (targetMember->remotePorts[curSwitch][port] == -1)
                    {
                        status = fmCreateStackLogicalPort(targetSwitch,
                                                          remoteGlort,
                                                          &remotePort);

                        if (status != FM_OK)
                        {
                            if (status == FM_ERR_NO_FORWARDING_RULES)
                            {
                                status = FM_OK;
                            }
                            break;
                        }

                        targetMember->remotePorts[curSwitch][port] = remotePort;

                        FM_LOG_DEBUG(FM_LOG_CAT_SWAG,
                                     "Switch %d created remote port %d for "
                                     "remote switch %d port %d\n",
                                     targetSwitch,
                                     remotePort,
                                     curSwitch,
                                     port);
                    }
                    else
                    {
                        remotePort = targetMember->remotePorts[curSwitch][port];
                    }

                    /* Set the logical port state to match the physical
                     * port state
                     */
                    status = fmGetPortState(curSwitch,
                                            port,
                                            &portMode,
                                            &portState,
                                            portInfo);

                    if (status == FM_OK)
                    {
                        curPortPtr = GET_PORT_PTR(curSwitch, port);
                    }
                    else
                    {
                        portState  = FM_PORT_STATE_DOWN;
                        curPortPtr = NULL;
                    }

                    status = fmSetStackLogicalPortState(targetSwitch,
                                                        remotePort,
                                                        portState);

                    if (status != FM_OK)
                    {
                        break;
                    }

                    targetPortPtr = GET_PORT_PTR(targetSwitch, remotePort);

                    if (curPortPtr != NULL)
                    {
                        targetPortPtr->swagPort     = curPortPtr->swagPort;
                        targetPortPtr->capabilities = curPortPtr->capabilities;
                    }
                    else
                    {
                        targetPortPtr->swagPort = -1;
                    }
                }
            }
            else
            {
                status = fmReleaseRemoteLogicalPorts(targetMember, -1);
            }

        NEXT_CURMEMBER:

            if (status != FM_OK)
            {
                break;
            }

            curMember = fmGetNextSwitchInSWAG(curMember);
        }

        if (targetSwitchProtected)
        {
            UNPROTECT_SWITCH(targetSwitch);
        }

        targetMember = fmGetNextSwitchInSWAG(targetMember);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

#else

    FM_NOT_USED(swagId);

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId = %d\n", swagId);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);

#endif

}   /* end fmUpdateRemotePorts */




/*****************************************************************************/
/** fmReleaseRemoteLogicalPorts
 * \ingroup intSwag
 *
 * \desc            Releases remote logical ports belonging to a given
 *                  destination switch, or to all switches, for a specific
 *                  member switch.
 *
 * \param[in]       member is the switch member with the logical ports
 *                  to be released.
 *
 * \param[in]       destSw is the destination switch, or -1 for all switches.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmReleaseRemoteLogicalPorts(fm_swagMember *member, fm_int destSw)
{
    fm_int    firstSw;
    fm_int    lastSw;
    fm_int    sw;
    fm_int    port;
    fm_int    remotePort;
    fm_status status;
    fm_port * portPtr;
    fm_int    lagIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "member = %p(%d), destSw = %d\n",
                 (void *) member,
                 (member != NULL) ? member->swId : -1,
                 destSw);

    if (member == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    if (destSw == -1)
    {
        firstSw = 0;
        lastSw  = FM_MAX_NUM_SWITCHES - 1;
    }
    else
    {
        firstSw = destSw;
        lastSw  = destSw;
    }

    for (sw = firstSw ; sw <= lastSw ; sw++)
    {
        for (port = 0 ; port < FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH ; port++)
        {
            remotePort = member->remotePorts[sw][port];

            if (remotePort != -1)
            {
                portPtr = GET_PORT_PTR(member->swId, remotePort);

                if (portPtr != NULL)
                {
                    if (fmPortIsInALAG(member->swId, remotePort))
                    {
                        lagIndex = fmGetPortLagIndex(member->swId, remotePort);
                        status = fmRemoveLAGMember(member->swId,
                                                   lagIndex,
                                                   remotePort);

                        if (status != FM_OK)
                        {
                            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
                        }
                    }
                    status = fmFreeLogicalPort(member->swId, remotePort);

                    switch (status)
                    {
                        case FM_OK:
                            break;
                        case FM_ERR_INVALID_SWITCH:
                            break;
                        default:
                            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
                    }
                }

                member->remotePorts[sw][port] = -1;
            }
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmReleaseRemoteLogicalPorts */




/*****************************************************************************/
/** fmActivateTrunkLink
 * \ingroup swagTopology
 *
 * \desc            Attempts to activate a trunk link, provided that nothing
 *                  has caused the link to be disabled.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId points to the trunk record.
 *
 * \param[in]       linkId points to the link record.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId or linkId are invalid.
 *
 *****************************************************************************/
fm_status fmActivateTrunkLink(fm_int     sw,
                              fm_voidptr trunkId,
                              fm_voidptr linkId)
{
#if FM_SUPPORT_SWAG
    fm_switch *      switchPtr;
    fmSWAG_switch *  switchExt;
    fm_swagIntTrunk *trunk;
    fm_swagIntLink * link;
    fm_status        status;
    fm_int           linkPort;
    fm_int           lagId;
    fm_int           srcSwitch;
    fm_swagMember *  srcMember;
    fm_int           trunkPort;
    fm_forwardRule   fwdRule;
    fm_int           fwdRuleId;
    fm_swagMember *  destMember;
    fm_int           destSwitch;
    fm_int           port;
    fm_uint32        glortBase;
    fm_uint32        glortMask;
    fm_int           remotePort;
    fm_bool          srcIsSpine;
    fm_int           remoteLag;
    fm_int           i;
    fm_int           destPortLag;
    fm_int           lagIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p, linkId = %p\n",
                 sw,
                 (void *) trunkId,
                 (void *) linkId);

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    link = (fm_swagIntLink *) linkId;

    if (link == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = GET_SWITCH_PTR(sw);
    switchExt = GET_SWITCH_EXT(sw);

    srcSwitch = trunk->trunk.trunkSw;

    srcMember = fmFindSwitchInSWAG(switchExt, srcSwitch);

    if (srcMember == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
    }

    link->enabled = TRUE;

    /* If not internal trunk, set the trunk active and quit */
    if (trunk->trunk.trunkType != FM_SWAG_LINK_INTERNAL)
    {
        trunk->trunk.isEnabled = TRUE;
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);
    }

    if (link->link.swId == srcSwitch)
    {
        linkPort = link->link.logicalPort;
    }
    else
    {
        linkPort = link->link.partnerLogicalPort;
    }

    switch (srcMember->role)
    {
        case FM_SWITCH_ROLE_SPINE:
        case FM_SWITCH_ROLE_SPINE_LEAF:
            srcIsSpine = TRUE;
            break;

        default:
            srcIsSpine = FALSE;
            break;
    }

    /* Is this the first active link?  If so, prepare the trunk for use */
    if (trunk->trunk.lagId == -1)
    {
        /* Lock LAG Access */
        status = fmCaptureLock(&switchPtr->lagLock, FM_WAIT_FOREVER);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        /* Get the next available LAG Id */
        status = fmSWAGGetNextAvailableLag(sw, &lagId);

        if (status != FM_OK)
        {
            fmReleaseLock(&switchPtr->lagLock);
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        /* Create the stacked lag */
        status = fmCreateStackLAG(sw, lagId);

        if (status != FM_OK)
        {
            fmReleaseLock(&switchPtr->lagLock);
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        /* Release LAG Access */
        fmReleaseLock(&switchPtr->lagLock);

        trunk->trunk.isLag = TRUE;
        trunk->trunk.lagId = lagId;
        trunk->trunk.swagPort = lagId;

        status = fmLogicalPortToLagIndex(sw, trunk->trunk.swagPort, &lagIndex);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        /* Get the logical port for the trunk on the source switch */
        trunkPort = srcMember->lagGroups[lagIndex];

        trunk->trunk.trunkPort = trunkPort;

        /* Configure the trunk */
        status = ConfigureInternalTrunk(sw, trunk);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

        /* create forwarding rules and logical ports for all remote switches
         * accessable via this trunk.
         */
        for (destSwitch = 0 ; destSwitch < FM_MAX_NUM_SWITCHES ; destSwitch++)
        {
            if ( (srcMember->switchTrunks[destSwitch] == trunk)
                && (destSwitch != srcSwitch) )
            {
                destMember = fmFindSwitchInSWAG(switchExt, destSwitch);

                if (destMember == NULL)
                {
                    continue;
                }

                if (destMember->glortSpace == NULL)
                {
                    continue;
                }

                glortBase = destMember->glortSpace->base;
                glortMask = destMember->glortSpace->mask;

                fwdRule.glort       = glortBase;
                fwdRule.mask        = glortMask;
                fwdRule.logicalPort = trunkPort;

                /* install the forwarding rule */

                status = fmCreateStackForwardingRule(srcSwitch,
                                                     &fwdRuleId,
                                                     &fwdRule);

                if (status != FM_OK)
                {
                    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
                }

                trunk->forwardingRuleIds[destSwitch] = fwdRuleId;

                for (port = 1 ;
                     port < FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH ;
                     port++)
                {
                    /* If the source switch is a spine switch and the
                     * destination port is a LAG port, add the destination
                     * port to the same LAG on the spine.
                     */
                    remotePort = srcMember->remotePorts[destSwitch][port];

                    if ( srcIsSpine && (remotePort != -1) )
                    {
                        remoteLag = fmGetLAGForPort(destSwitch, port);

                        if (remoteLag >= 0)
                        {
                            /* Convert destination switch LAG number to
                             * SWAG lag number, then to source switch
                             * lag number.
                             */
                            destPortLag = -1;
                            for (i = 0 ; i < FM_MAX_NUM_LAGS ; i++)
                            {
                                if (destMember->lagGroups[i] == remoteLag)
                                {
                                    destPortLag = srcMember->lagGroups[i];
                                    break;
                                }
                            }

                            if (destPortLag >= 0)
                            {
                                status = fmAddLAGPort(srcSwitch,
                                                      destPortLag,
                                                      remotePort);

                                if (status == FM_ERR_ALREADYUSED_PORT)
                                {
                                    status = FM_OK;
                                }

                                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);
                            }
                        }
                    }
                }
            }
        }
    }

    /* Add link port to LAG */
    status = fmAddLAGPort(sw, trunk->trunk.lagId, linkPort);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    /* In per-LAG management mode the port has been configure with its
     * corresponding LAG's settings when previously added to the LAG, so no
     * need to do it here, otherwise an error will be returned because per-LAG
     * port attributes cannot be set on a member port, but only on the LAG
     * logical port. */
    if (!switchPtr->perLagMgmt)
    {
        /* Configure the port */
        status = ConfigureInternalPort(sw, linkPort);

        if (status != FM_OK)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
        }

    }
    trunk->trunk.isEnabled = TRUE;

    status = fmUpdateRemotePorts(sw);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmUpdateLagRemotePorts(sw);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    status = fmUpdateMulticastRemoteListeners(sw);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

#else

    FM_NOT_USED(sw);
    FM_NOT_USED(trunkId);
    FM_NOT_USED(linkId);

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p, linkId = %p\n",
                 sw,
                 (void *) trunkId,
                 (void *) linkId);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_UNSUPPORTED);

#endif

}   /* end fmActivateTrunkLink */




/*****************************************************************************/
/** fmDeactivateTrunkLink
 * \ingroup swagTopology
 *
 * \desc            Deactivates a trunk link.
 *
 * \param[in]       sw is the switch aggregate number.
 *
 * \param[in]       trunkId points to the trunk record.
 *
 * \param[in]       linkId points to the link record.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if trunkId or linkId are invalid.
 *
 *****************************************************************************/
fm_status fmDeactivateTrunkLink(fm_int     sw,
                                fm_voidptr trunkId,
                                fm_voidptr linkId)
{
    fmSWAG_switch *  switchExt;
    fm_swagIntTrunk *trunk;
    fm_swagIntLink * link;
    fm_status        status = FM_OK;
    fm_int           linkPort;
    fm_int           trunkPort;
    fm_int           srcSwitch;
    fm_swagMember *  srcMember;
    fm_swagMember *  destMember;
    fm_int           destSwitch;
    fm_int           port;
    fm_int           remotePort;
    fm_bool          srcIsSpine;
    fm_int           remoteLag;
    fm_int           i;
    fm_int           destPortLag;
    fm_int           lagIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, trunkId = %p, linkId = %p\n",
                 sw,
                 (void *) trunkId,
                 (void *) linkId);

    switchExt = GET_SWITCH_EXT(sw);

    trunk = (fm_swagIntTrunk *) trunkId;

    if (trunk == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    link = (fm_swagIntLink *) linkId;

    if (link == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_ARGUMENT);
    }

    srcSwitch = trunk->trunk.trunkSw;

    srcMember = fmFindSwitchInSWAG(switchExt, srcSwitch);

    if (srcMember == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
    }

    link->enabled = FALSE;

    /* If not internal trunk, set the trunk inactive and quit */
    if (trunk->trunk.trunkType != FM_SWAG_LINK_INTERNAL)
    {
        trunk->trunk.isEnabled = FALSE;
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);
    }

    /* Determine link port */
    if (link->link.swId == srcSwitch)
    {
        linkPort = link->link.logicalPort;
    }
    else
    {
        linkPort = link->link.partnerLogicalPort;
    }

    /* Remove link port from LAG */
    status = fmDeleteLAGPort(sw, trunk->trunk.lagId, linkPort);

    switch (status)
    {
        case FM_ERR_INVALID_LAG:
        case FM_ERR_INVALID_PORT:
            status = FM_OK;
            break;

        case FM_OK:
            status = fmLAGNumberToLogicalPort(sw,
                                              trunk->trunk.lagId,
                                              &trunkPort);

            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);

            status = fmLogicalPortToLagIndex(sw, trunkPort, &lagIndex);

            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);

            /* Are there any members still in the trunk? */
            if (fmCountActiveLAGMembers(srcSwitch, lagIndex) > 0)
            {
                /* yes, we're done */
                FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);
            }
            break;

        default:
            break;
    }

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);

    trunk->trunk.isEnabled = FALSE;

    switch (srcMember->role)
    {
        case FM_SWITCH_ROLE_SPINE:
        case FM_SWITCH_ROLE_SPINE_LEAF:
            srcIsSpine = TRUE;
            break;

        default:
            srcIsSpine = FALSE;
            break;
    }

    /* Delete remote LAG ports, then delete logical ports and forwarding
     * rules for all remote switches previously accessable via this trunk. */
    for (destSwitch = 0 ; destSwitch < FM_MAX_NUM_SWITCHES ; destSwitch++)
    {
        if ( (srcMember->switchTrunks[destSwitch] == trunk)
            && (destSwitch != srcSwitch) )
        {
            destMember = fmFindSwitchInSWAG(switchExt, destSwitch);

            if (destMember == NULL)
            {
                continue;
            }

            if (destMember->glortSpace == NULL)
            {
                continue;
            }

            for (port = 1 ;
                 port < FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH ;
                 port++)
            {
                /* If the source switch is a spine switch and the
                 * destination port is a LAG port, remove the destination
                 * port from the same LAG on the spine.
                 */
                remotePort = srcMember->remotePorts[destSwitch][port];

                if ( srcIsSpine && (remotePort != -1) )
                {
                    remoteLag = fmGetLAGForPort(destSwitch, port);

                    if (remoteLag >= 0)
                    {
                        /* Convert destination switch LAG number to
                         * SWAG lag number, then to source switch
                         * lag number.
                         */
                        destPortLag = -1;
                        for (i = 0 ; i < FM_MAX_NUM_LAGS ; i++)
                        {
                            if (destMember->lagGroups[i] == remoteLag)
                            {
                                destPortLag = srcMember->lagGroups[i];
                                break;
                            }
                        }

                        if (destPortLag >= 0)
                        {
                            status = fmDeleteLAGPort(srcSwitch,
                                                     destPortLag,
                                                     remotePort);

                            if (status != FM_ERR_INVALID_PORT)
                            {
                                FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);
                            }
                        }
                    }
                }
            }   /* end for (port = 1 ; ...) */

            /* delete the forwarding rule */
            status = fmDeleteStackForwardingRule(srcSwitch,
                                                 trunk->forwardingRuleIds[destSwitch]);

            switch (status)
            {
                case FM_OK:
                    break;

                case FM_FAIL:
                case FM_ERR_NOT_FOUND:
                case FM_ERR_SWITCH_NOT_UP:
                case FM_ERR_INVALID_SWITCH:
                    status = FM_OK;
                    break;

                default:
                    break;
            }

            FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);
        }
    }

    /* Delete the lag */
    status = fmDeleteLAGExt(sw, trunk->trunk.lagId);

    switch (status)
    {
        case FM_OK:
            break;

        case FM_ERR_INVALID_LAG:
        case FM_ERR_INVALID_PORT:
            status = FM_OK;
            break;

        default:
            break;
    }

    FM_LOG_EXIT_ON_ERR(FM_LOG_CAT_SWAG, status);

    trunk->trunk.isLag     = FALSE;
    trunk->trunk.lagId     = -1;
    trunk->trunk.trunkPort = -1;

    status = fmUpdateRemotePorts(sw);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmDeactivateTrunkLink */




/*****************************************************************************/
/** fmDeactivateTrunkPort
 * \ingroup intSwag
 *
 * \desc            This function disables the trunk associated with
 *                  a port in a switch aggregate.
 *
 * \param[in]       sw is the switch aggregate switch number.
 *
 * \param[in]       port is the port number.
 *
 * \return          Status Code.
 *
 *****************************************************************************/
fm_status fmDeactivateTrunkPort(fm_int sw, fm_int port)
{
    fm_voidptr trunkId;
    fm_voidptr linkId;
    fm_status  status;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw = %d, port = %d\n", sw, port);

    /* get the trunk record for the first port */
    status = fmGetTrunkForPort(sw, port, &trunkId);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    /* get the link record for the first port */
    status = fmGetLinkForPort(sw, port, &linkId);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    /* Deactivate the trunk link */
    status = fmDeactivateTrunkLink(sw, trunkId, linkId);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmDeactivateTrunkPort */




/*****************************************************************************/
/** fmPreInitializeSwitchInSWAG
 * \ingroup intSwag
 *
 * \desc            If a switch is in a switch aggregate, perform any
 *                  pre-initialization that has to be done after the switch
 *                  exists but before it is brought up.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmPreInitializeSwitchInSWAG(fm_int sw)
{
    fm_int              swagId;
    fm_status           status;
    fm_switch *         swagPtr;
    fmSWAG_switch *     swagExt;
    fm_switch *         switchPtr;
    fm_swagMember *     member;
    fmSWAG_switchGlort *glortSpace;
    fm_glortRange       glortRange;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw=%d\n", sw);

    /* Verify that the switch is in a switch aggregate */
    status = fmIsSwitchInASWAG(sw, &swagId);

    if (status != FM_OK)
    {
        if (status == FM_ERR_SWITCH_NOT_IN_AGGREGATE)
        {
            status = FM_OK;
        }

        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    swagPtr = GET_SWITCH_PTR(swagId);
    swagExt = GET_SWITCH_EXT(swagId);

    /* Find the switch entry in the switch aggregate */
    member = fmFindSwitchInSWAG(swagExt, sw);

    if (member == NULL)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWAG,
                     "switch in aggregate not in linked-list: sw=%d, "
                     "swagId=%d\n",
                     sw,
                     swagId);
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_FAIL);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    status = fmSelectGlortSpaceForSwitchInSWAG(swagId, sw);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);
    }

    glortSpace = member->glortSpace;

    glortRange = swagPtr->glortRange;

    glortRange.glortBase     = glortSpace->base;
    glortRange.glortMask     = glortSpace->mask;
    glortRange.portBaseGlort = glortSpace->base;
    glortRange.portCount     = switchPtr->maxPhysicalPort + 1;

    /* Assign Stacking Master Glort Range */
    status = fmSetStackGlortRangeExt(sw, &glortRange);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmPreInitializeSwitchInSWAG */




/*****************************************************************************/
/** fmSWAGPrepareLinks
 * \ingroup intSwag
 *
 * \desc            Prepares zero or more links for use.
 *                  The switch aggregate switch must be in the "up" state.
 *
 * \note            This function assumes that the switch aggregate lock
 *                  has already been taken, but that underlying switch
 *                  locks have not.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       sw is the ID of the underlying switch, if only links
 *                  for a single underlying switch are to be prepared for
 *                  use.  Use -1 to specify all switches in the swag.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if intLink is NULL.
 * \return          FM_ERR_SWITCH_NOT_UP if the switch aggregate is not in
 *                  the up state.
 * \return          FM_ERR_INVALID_PORT if link specifies a switch-aggregate
 *                  or underlying switch port that does not exist.
 * \return          FM_FAIL if a logical port was created but it's port pointer
 *                  is still NULL.
 *
 *****************************************************************************/
fm_status fmSWAGPrepareLinks(fm_int swagId, fm_int sw)
{
    fm_status       status = FM_OK;
    fm_switch *     aggSwPtr;
    fmSWAG_switch * aggExt;
    fm_swagIntLink *intLink;
    fm_bool         prepLink;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId = %d, sw = %d\n", swagId, sw);

    aggSwPtr = fmRootApi->fmSwitchStateTable[swagId];
    aggExt   = aggSwPtr->extension;

    if ( !FM_IS_STATE_ALIVE(aggSwPtr->state) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_UP);
    }

    intLink = fmGetFirstLinkInLinkList(aggExt);

    while (intLink != NULL)
    {
        prepLink = FALSE;

        if (sw == -1)
        {
            prepLink = TRUE;
        }
        else if (intLink->link.swId == sw)
        {
            prepLink = TRUE;
        }
        else if (intLink->link.type == FM_SWAG_LINK_INTERNAL)
        {
            if (intLink->link.partnerSwitch == sw)
            {
                prepLink = TRUE;
            }
        }

        if (prepLink)
        {
            status = PrepareLink(swagId, intLink);

            if (status != FM_OK)
            {
                break;
            }
        }

        intLink = fmGetNextLinkInLinkList(intLink);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmSWAGPrepareLinks */




/*****************************************************************************/
/** fmSWAGGetNextAvailableLag
 * \ingroup intLag
 *
 * \desc            Finds an available stacking lag group number for the SWAG.
 *
 * \param[in]       sw is the switch aggregate on which to operate.
 *
 * \param[out]      swagLag points to caller-allocated space into which the
 *                  index of the next available LAG in the SWAG's LAG tables
 *                  will be placed.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_FREE_LAG if all LAGs are in use.
 *
 *****************************************************************************/
fm_status fmSWAGGetNextAvailableLag(fm_int sw, fm_int *swagLag)
{
    fm_status      status;
    fm_switch *    switchPtr;
    fmSWAG_switch *switchExt;
    fm_int         i;

    FM_LOG_ENTRY(FM_LOG_CAT_LAG, "sw=%d, swagLag=%p\n", sw, (void *) swagLag);

    if (swagLag == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_LAG, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = fmRootApi->fmSwitchStateTable[sw];
    switchExt = switchPtr->extension;

    if (!switchExt->localLagsAllocated)
    {
        FM_LOG_EXIT(FM_LOG_CAT_LAG, FM_ERR_NO_GLORTS_ALLOCATED);
    }

    status = FM_ERR_NO_FREE_LAG;

    for (i = 0 ; i < FM_MAX_LAGS_PER_SWAG * FM_SWAG_MAX_LAG_ALLOCS ; i++)
    {
        if (!switchExt->lagsUsed[i])
        {
            *swagLag = switchExt->lagGroupHandles[i];
            status   = FM_OK;
            break;
        }
    }

    FM_LOG_EXIT_CUSTOM(FM_LOG_CAT_LAG,
                       status,
                       "status=%d, lag=%d\n",
                       status,
                       *swagLag);

}   /* end fmSWAGGetNextAvailableLag */




/*****************************************************************************/
/** fmSelectGlortSpaceForSwitchInSWAG
 * \ingroup intSwag
 *
 * \desc            Select a glort space for a switch in a swag.  The actual
 *                  assignment of that glort space in the hardware is done
 *                  when the switch is brought up.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmSelectGlortSpaceForSwitchInSWAG(fm_int swagId, fm_int sw)
{
    fmSWAG_switch *     swagExt;
    fm_swagMember *     curSwitch;
    fm_int              i;
    fmSWAG_switchGlort *glortSpace;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId = %d, sw = %d\n", swagId, sw);

    swagExt = fmRootApi->fmSwitchStateTable[swagId]->extension;

    /* Find the switch entry in the switch aggregate */
    curSwitch = fmFindSwitchInSWAG(swagExt, sw);

    if (curSwitch == NULL)
    {
        FM_LOG_FATAL(FM_LOG_CAT_SWAG,
                     "switch in aggregate not in linked-list: sw=%d, "
                     "swagId=%d\n",
                     sw,
                     swagId);
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_FAIL);
    }

    if (curSwitch->glortSpace == NULL)
    {
        /* Find an available glort space */
        glortSpace = swagExt->switchGlortSpace;

        for (i = 0 ; i < FM_MAX_SWITCHES_PER_SWAG ; i++, glortSpace++)
        {
            if (!glortSpace->inUse)
            {
                glortSpace->inUse     = TRUE;
                curSwitch->glortSpace = glortSpace;
                break;
            }
        }

        if (i >= FM_MAX_SWITCHES_PER_SWAG)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_GLORT_RANGE_TOO_SMALL);
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmSelectGlortSpaceForSwitchInSWAG */




/*****************************************************************************/
/** fmGetInternalPortList
 * \ingroup intSwag
 *
 * \desc            Retrieves a list of all internal ports.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \param[out]      portList points to caller-allocated storage into which
 *                  the list of internal ports will be placed.
 *
 * \param[in]       maxPorts is the maximum number of ports that can be
 *                  stored in portList.
 *
 * \param[out]      portCount points to caller-allocated storage into which
 *                  the number of ports which were put into portList.  Note
 *                  that if *portCount is equal to maxPorts, it is possible
 *                  that additional internal ports exist which were not
 *                  returned in the list.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmGetInternalPortList(fm_int  sw,
                                fm_int *portList,
                                fm_int  maxPorts,
                                fm_int *portCount)
{
    fmSWAG_switch * switchExt;
    fm_swagIntLink *intLink;
    fm_int          curCount = 0;
    fm_int *        curPtr   = portList;

    switchExt = GET_SWITCH_EXT(sw);

    intLink = fmGetFirstLinkInLinkList(switchExt);

    while (intLink != NULL)
    {
        if (intLink->link.type == FM_SWAG_LINK_INTERNAL)
        {
            if (curCount >= maxPorts)
            {
                break;
            }

            *curPtr++ = intLink->link.logicalPort;
            curCount++;

            if (curCount >= maxPorts)
            {
                break;
            }

            *curPtr++ = intLink->link.partnerLogicalPort;
            curCount++;
        }

        intLink = fmGetNextLinkInLinkList(intLink);
    }

    *portCount = curCount;

    return FM_OK;

}   /* end fmGetInternalPortList */




/*****************************************************************************/
/** fmGetInternalTrunkList
 * \ingroup intSwag
 *
 * \desc            Retrieves a list of all internal trunks.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \param[out]      portList points to caller-allocated storage into which
 *                  the list of internal ports will be placed.
 *
 * \param[in]       maxPorts is the maximum number of ports that can be
 *                  stored in portList.
 *
 * \param[out]      portCount points to caller-allocated storage into which
 *                  the number of ports which were put into portList.  Note
 *                  that if *portCount is equal to maxPorts, it is possible
 *                  that additional internal ports exist which were not
 *                  returned in the list.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmGetInternalTrunkList(fm_int  sw,
                                 fm_int *portList,
                                 fm_int  maxPorts,
                                 fm_int *portCount)
{
    fmSWAG_switch * switchExt;
    fm_swagIntTrunk *trunk;
    fm_int          curCount = 0;
    fm_int *        curPtr   = portList;

    switchExt = GET_SWITCH_EXT(sw);

    trunk = fmGetFirstTrunkInSwitch(switchExt);

    while (trunk != NULL)
    {
        if (trunk->trunk.trunkType == FM_SWAG_LINK_INTERNAL)
        {
            if (curCount >= maxPorts)
            {
                break;
            }

            FM_LOG_DEBUG(FM_LOG_CAT_SWAG, "Find trunk %d\n", trunk->trunk.swagPort);

            *curPtr++ = trunk->trunk.swagPort;
            curCount++;
        }

        trunk = fmGetNextTrunkInSwitch(trunk);
    }

    *portCount = curCount;

    return FM_OK;

}   /* end fmGetInternalTrunkList */




/*****************************************************************************/
/** fmConfigureSWAGSwitchMaster
 * \ingroup intSwag
 *
 * \desc            Configures a switch as either a master or non-master,
 *                  based upon the switch member's masterCpu flag.
 *
 * \note            This function assumes that the SWAG switch lock has
 *                  been taken and that the individual switch lock can
 *                  be taken without causing any deadlocks.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *
 *****************************************************************************/
fm_status fmConfigureSWAGSwitchMaster(fm_int sw)
{
    fm_status      status;
    fm_int         swagId;
    fm_switch *    switchPtr;
    fmSWAG_switch *swagExt;
    fm_swagMember *member;
    fm_int         bcastFlooding;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "sw = %d\n", sw);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_SWITCHES) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_SWITCH);
    }

    if ( !SWITCH_LOCK_EXISTS(sw) )
    {
        /* The switch lock doesn't exist, so the switch can't exist,
         * but it might be inserted later, so this isn't an error.
         */
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);
    }

    PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        /* Switch doesn't yet exist, nothing to do for now, but not an error */
        status = FM_OK;
        goto ABORT;
    }

    if (switchPtr->state != FM_SWITCH_STATE_UP)
    {
        /* Switch exists but isn't yet up.  nothing to do, but not an error. */
        status = FM_OK;
        goto ABORT;
    }

    swagId = switchPtr->swag;

    swagExt = GET_SWITCH_EXT(swagId);

    member = fmFindSwitchInSWAG(swagExt, sw);

    if (member == NULL)
    {
        /* Switch is supposed to be a swag member, but can't be found
         * in the swag's member list!
         */
        status = FM_FAIL;
        goto ABORT;
    }

    /* Initialize settings using SWAG defaults. */
    bcastFlooding = swagExt->bcastFlooding;

    /* Modify settings based upon switch master/slave status */
    if (member->masterCpu)
    {
        /* MASTER SWITCH */

        /* Use SWAG broadcast setting. */
    }
    else
    {
        /* SLAVE SWITCH */

        if (bcastFlooding == FM_BCAST_FWD)
        {
            /* Do not flood broadcasts to CPU */
            bcastFlooding = FM_BCAST_FWD_EXCPU;
        }
    }

    status = fmSetSwitchAttribute(sw,
                                  FM_BCAST_FLOODING,
                                  &bcastFlooding);

    if (status != FM_OK)
    {
        goto ABORT;
    }


ABORT:

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmConfigureSWAGSwitchMaster */




/*****************************************************************************/
/** fmDbgGetSwitchAndPortForSWAGPort
 * \ingroup debug
 *
 * \desc            Gets underlying switch and port numbers for a switch
 *                  aggregate logical port.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \param[in]       swagPort is the aggregate logical port number.
 *
 * \param[out]      sw points to caller-allocated storage which will contain
 *                  the underlying switch number for the specified port.
 *
 * \param[out]      port points to caller-allocated storage which will contain
 *                  the logical port number within the underlying switch that
 *                  corresponds to the specified aggregate logical port.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if the switch number is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if the port cannot be found
 *                  within the aggregate.
 *
 *****************************************************************************/
fm_status fmDbgGetSwitchAndPortForSWAGPort(fm_int  swagId,
                                           fm_int  swagPort,
                                           fm_int *sw,
                                           fm_int *port)
{
    fm_status status;

    FM_LOG_ENTRY_API(FM_LOG_CAT_SWAG,
                     "swagId = %d, swagPort = %d, sw = %p, port = %p\n",
                     swagId,
                     swagPort,
                     (void *) sw,
                     (void *) port);

    VALIDATE_AND_PROTECT_SWITCH(swagId);

    status = fmGetSwitchAndPortForSWAGPort(swagId, swagPort, sw, port);

    UNPROTECT_SWITCH(swagId);

    FM_LOG_EXIT_API(FM_LOG_CAT_SWAG, status);

}   /* end fmDbgGetSwitchAndPortForSWAGPort */




/*****************************************************************************/
/** fmSWAGDeleteLagCallback
 * \ingroup intSwag
 *
 * \desc            Called when a sub-switch has completed the process of
 *                  deleting a LAG.  Determines whether all sub-switches
 *                  have finished deleting the LAG and completes LAG
 *                  deletion at the SWAG level.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       sw is the ID of the switch.
 *
 * \param[in]       lagIndex is the index of the LAG for the sub-switch.
 *
 * \return          FM_OK if successful.
 *
 *
 *****************************************************************************/
fm_status fmSWAGDeleteLagCallback(fm_int swagId, fm_int sw, fm_int lagIndex)
{
    fm_status      status;
    fm_switch *    swagPtr;
    fmSWAG_switch *swagExt;
    fm_swagMember *member;
    fm_swagMember *curMember;
    fm_int         swagLagIndex;
    fm_lag *       swagLagPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_LAG,
                 "swagId = %d, sw = %d, lagIndex = %d\n",
                 swagId,
                 sw,
                 lagIndex);

    swagPtr = GET_SWITCH_PTR(swagId);
    swagExt = GET_SWITCH_EXT(swagId);
    member  = fmFindSwitchInSWAG(swagExt, sw);

    if (member == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_LAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
    }

    swagLagIndex = member->swagLagGroups[lagIndex];
    swagLagPtr   = swagPtr->lagInfoTable.lag[swagLagIndex];

    member->lagGroupActive[swagLagIndex]    = FALSE;
    member->lagGroupPortCount[swagLagIndex] = 0;

    curMember = fmGetFirstSwitchInSWAG(swagExt);

    while (curMember != NULL)
    {
        if (curMember->lagGroupActive[swagLagIndex])
        {
            break;
        }

        curMember = fmGetNextSwitchInSWAG(curMember);
    }

    if (curMember == NULL)
    {
        swagExt->lagsUsed[swagLagIndex] = FALSE;

        if (swagLagPtr != NULL)
        {
            status = fmDeleteLagCallback(swagId, swagLagPtr);
        }
        else
        {
            status = FM_OK;
        }
    }
    else
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_LAG, status);

}   /* end fmSWAGDeleteLagCallback */




#if FM_SUPPORT_SWAG




/*****************************************************************************/
/** fmAddRemoteLagPort
 * \ingroup intSwag
 *
 * \desc            Add a remote port to a link aggregation group.
 *
 * \param[in]       swagId is the SWAG switch on which to operate.
 *
 * \param[in]       swagLag is the LAG ID for the LAG in the SWAG.
 *
 * \param[in]       port is the number of the port.  The corresponding remote
 *                  port for each switch in the SWAG (except for the actual
 *                  switch to which this port is connected) will be added
 *                  as a LAG member on each remote switch.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmAddRemoteLagPort(fm_int swagId, fm_int swagLag, fm_int port)
{
    fm_status      status;
    fmSWAG_switch *switchExt;
    fm_int         realSw;
    fm_int         realPort;
    fm_int         sw;
    fm_swagMember *member;
    fm_int         remotePort;
    fm_int         memberLag;
    fm_int         memberLagIndex;

    FM_LOG_ENTRY(FM_LOG_CAT_LAG,
                 "swagId = %d, swagLag = %d, port = %d\n",
                 swagId,
                 swagLag,
                 port);

    switchExt = GET_SWITCH_EXT(swagId);

    status = fmGetSwitchPortForSWAGPort(switchExt,
                                        port,
                                        &realSw,
                                        &realPort);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_LAG, status);
    }

    /* Add this port as a remote port to all other switches which
     * use this LAG
     */
    member = fmGetFirstSwitchInSWAG(switchExt);

    while (member != NULL)
    {
        sw = member->swId;

        if (sw != realSw)
        {
            if (member->lagGroupActive[swagLag])
            {
                remotePort = member->remotePorts[realSw][realPort];

                if (remotePort >= 0)
                {
                    memberLag = member->lagGroups[swagLag];

                    status = fmLogicalPortToLagIndex(sw,
                                                     memberLag,
                                                     &memberLagIndex);
                    if (status == FM_OK)
                    {
                        if (!fmPortIsInLAG(sw, remotePort, memberLagIndex))
                        {
                            status = fmAddLAGPort(sw, memberLag, remotePort);

                            if (status != FM_OK)
                            {
                                FM_LOG_EXIT(FM_LOG_CAT_LAG, status);
                            }
                        }
                    }
                }
            }
        }

        member = fmGetNextSwitchInSWAG(member);
    }

    FM_LOG_EXIT(FM_LOG_CAT_LAG, status);

}   /* end fmAddRemoteLagPort */




/*****************************************************************************/
/** fmUpdateLagRemotePorts
 * \ingroup intSwag
 *
 * \desc            Updates LAG logical ports for all switches
 *                  in a switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmUpdateLagRemotePorts(fm_int swagId)
{
    fm_status status;
    fm_int    port;
    fm_int    lagNum;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId = %d\n", swagId);

    status = fmGetLAGFirstExt(swagId, &lagNum);

    while (status == FM_OK)
    {
        status = fmGetLAGPortFirstExt(swagId, lagNum, &port);

        while (status == FM_OK)
        {
            status = fmAddRemoteLagPort(swagId, lagNum, port);

            if (status != FM_OK)
            {
                break;
            }

            status = fmGetLAGPortNextExt(swagId, lagNum, port, &port);
        }

        if (status == FM_ERR_NO_PORTS_IN_LAG)
        {
            status = fmGetLAGNextExt(swagId, lagNum, &lagNum);
        }
    }

    if (status == FM_ERR_NO_LAGS)
    {
        status = FM_OK;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmUpdateLagRemotePorts */




/*****************************************************************************/
/** fmAddRemoteMulticastListener
 * \ingroup intSwag
 *
 * \desc            Add a multicast listener to switches other than the
 *                  switch local to the listener.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       group points to the multicast group entry
 *
 * \param[in]       listener points to the listener entry which is being
 *                  added.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmAddRemoteMulticastListener(fm_int                   sw,
                                       fm_intMulticastGroup *   group,
                                       fm_intMulticastListener *listener)
{
    fm_status                 status;
    fmSWAG_switch *           switchExt;
    fm_int                    realSw;
    fm_int                    realPort;
    fm_multicastListener      curListener;
    fm_swagMember *           curMember;
    fm_int                    remotePort;
    fm_int                    groupIndex;
    fm_int                    curGroup;
    fm_int                    curSw;
    fmSWAG_intMulticastGroup *groupExt;

    FM_LOG_ENTRY(FM_LOG_CAT_MULTICAST,
                 "sw=%d group=%p<%d> listener=%p:<%d,%d>\n",
                 sw,
                 (void *) group,
                 (group != NULL) ? group->logicalPort : 0,
                 (void *) listener,
                 (listener != NULL) ? listener->port : 0,
                 (listener != NULL) ? listener->vlan : 0);

    if (group == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_MULTICAST, FM_ERR_INVALID_ARGUMENT);
    }

    if (listener == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_MULTICAST, FM_ERR_INVALID_ARGUMENT);
    }

    switchExt = GET_SWITCH_EXT(sw);

    status = fmGetSwitchPortForSWAGPort(switchExt,
                                        listener->port,
                                        &realSw,
                                        &realPort);

    if (status != FM_OK)
    {
        FM_LOG_EXIT(FM_LOG_CAT_MULTICAST, status);
    }

    groupExt   = group->extension;
    groupIndex = groupExt->index;

    /* Add this listener as a remote listener to all other switches. */
    curMember = fmGetFirstSwitchInSWAG(switchExt);

    while (curMember != NULL)
    {
        if (curMember->swId != realSw)
        {
            curGroup   = curMember->mcastGroups[groupIndex];
            curSw      = curMember->swId;
            remotePort = curMember->remotePorts[realSw][realPort];

            if ( (remotePort >= 0) && (curGroup > 0) )
            {
                curListener.vlan = listener->vlan;
                curListener.port = remotePort;

                status = fmAddMulticastListener(curSw,
                                                curGroup,
                                                &curListener);

                if (status != FM_OK)
                {
                    FM_LOG_EXIT(FM_LOG_CAT_MULTICAST, status);
                }
            }
        }

        curMember = fmGetNextSwitchInSWAG(curMember);
    }

    FM_LOG_EXIT(FM_LOG_CAT_MULTICAST, status);

}   /* end fmAddRemoteMulticastListener */




/*****************************************************************************/
/** fmUpdateMulticastRemoteListeners
 * \ingroup intSwag
 *
 * \desc            Updates multicast listeners for all switches
 *                  in a switch aggregate.
 *
 * \param[in]       swagId is the switch aggregate number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmUpdateMulticastRemoteListeners(fm_int swagId)
{
    fm_status                status;
    fm_switch *              switchPtr;
    fm_bool                  lockTaken = FALSE;
    fm_treeIterator          groupIter;
    fm_uint64                key;
    fm_intMulticastGroup *   group;
    fm_treeIterator          listenerIter;
    fm_intMulticastListener *listener;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId = %d\n", swagId);

    switchPtr = GET_SWITCH_PTR(swagId);

    status = fmCaptureWriteLock(&switchPtr->routingLock, FM_WAIT_FOREVER);
    if (status != FM_OK)
    {
        goto ABORT;
    }

    lockTaken = TRUE;

    fmTreeIterInit(&groupIter, &switchPtr->mcastTree);

    while (1)
    {
        status = fmTreeIterNext( &groupIter, &key, (void **) &group);

        if (status != FM_OK)
        {
            if (status == FM_ERR_NO_MORE)
            {
                status = FM_OK;
            }

            break;
        }

        fmTreeIterInit(&listenerIter, &group->listenerTree);

        while (1)
        {
            status = fmTreeIterNext( &listenerIter, &key, (void **) &listener);

            if (status != FM_OK)
            {
                if (status == FM_ERR_NO_MORE)
                {
                    status = FM_OK;
                }

                break;
            }

            status = fmAddRemoteMulticastListener(swagId, group, listener);

            if (status != FM_OK)
            {
                break;
            }
        }

        if (status != FM_OK)
        {
            break;
        }
    }


ABORT:

    if (lockTaken)
    {
        fmReleaseWriteLock(&switchPtr->routingLock);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmUpdateMulticastRemoteListeners */




/*****************************************************************************/
/** fmInitializeSwitchInSWAG
 * \ingroup intSwag
 *
 * \desc            Initializes a switch in a swag, usually called when
 *                  adding a switch to a swag or during swag switch
 *                  startup.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       member points to the member structure for the switch.
 *
 * \param[in]       fullInit is TRUE if the member should be fully initialized,
 *                  usually this is only done when adding a switch to a swag.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitializeSwitchInSWAG(fm_int         swagId,
                                   fm_swagMember *member,
                                   fm_bool        fullInit)
{
    fm_int i;
    fm_int j;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "swagId=%d, member=%p(%d)\n",
                 swagId,
                 (void *) member,
                 member->swId);

    FM_NOT_USED(swagId);

    for (i = 0 ; i < FM_MAX_NUM_LAGS ; i++)
    {
        if (fullInit)
        {
            member->lagGroups[i] = -1;
        }

        member->lagGroupActive[i] = FALSE;
    }

    for (i = 0 ; i < FM_MAX_LBGS_PER_SWAG ; i++)
    {
        if (fullInit)
        {
            member->lbgGroups[i] = -1;
        }

        member->lbgGroupActive[i] = FALSE;
    }

    for (i = 0 ; i < FM_MAX_MCAST_GROUPS_PER_SWAG ; i++)
    {
        if (fullInit)
        {
            member->mcastGroups[i] = -1;
        }

        member->mcastGroupActive[i] = FALSE;
    }

    for (i = 0 ; i < FM_MAX_NUM_STORM_CTRL ; i++)
    {
        member->stormControllers[i] = -1;
    }

    for (i = 0 ; i < FM_MAX_ARPS ; i++)
    {
        member->ecmpGroups[i] = -1;
    }

    for (i = 0 ; i < FM_MAX_NUM_SWITCHES ; i++)
    {
        for (j = 0 ; j <= FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH ; j++)
        {
            member->remotePorts[i][j] = -1;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmInitializeSwitchInSWAG */




/*****************************************************************************/
/** fmInitializeAllSwitchesInSWAG
 * \ingroup intSwag
 *
 * \desc            Initializes all switches in a swag, usually called
 *                  during swag switch startup.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH_AGGREGATE if swagId is invalid.
 * \return          FM_ERR_NO_MEM if no memory available for data structures.
 *
 *****************************************************************************/
fm_status fmInitializeAllSwitchesInSWAG(fm_int swagId)
{
    fm_status      status;
    fmSWAG_switch *swagExt;
    fm_swagMember *member;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG, "swagId=%d\n", swagId);

    status  = FM_OK;
    swagExt = GET_SWITCH_EXT(swagId);
    member  = swagExt->firstSwitch;

    while (member != NULL)
    {
        status = fmInitializeSwitchInSWAG(swagId, member, FALSE);

        if (status != FM_OK)
        {
            break;
        }

        member = member->nextSwitch;
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmInitializeAllSwitchesInSWAG */




/*****************************************************************************/
/** fmGetGlortForSwagPhysicalPort
 * \ingroup intSwag
 *
 * \desc            Gets the glort number associated with a SWAG physical port
 *                  by finding the underlying switch and retrieving the glort
 *                  number associated with the physical port.
 *
 * \param[in]       swagId is the ID of the switch aggregate.
 *
 * \param[in]       port is the SWAG port number.
 *
 * \param[out]      glort points to caller-allocated space into which the
 *                  corresponding glort will be placed.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_PORT if the port can't be found.
 *
 *****************************************************************************/
fm_status fmGetGlortForSwagPhysicalPort(fm_int     swagId,
                                        fm_int     port,
                                        fm_uint32 *glortPtr)
{
    fmSWAG_switch * swagExt;
    fmSWAG_switch * curSwagExt;
    fm_switch *     curSwitchPtr;
    fm_int          curPort;
    fm_int          realSw;
    fm_int          realPort;
    fm_swagMember * member;
    fm_uint32       glort;
    fm_swagIntLink *linkPtr;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "swagId = %d, port = %d, glortPtr = %p\n",
                 swagId,
                 port,
                 (void *) glortPtr);

    swagExt    = GET_SWITCH_EXT(swagId);
    curSwagExt = swagExt;
    curPort    = port;

    while (1)
    {
        linkPtr = fmFindPortInLinkList(curSwagExt, curPort);

        if (linkPtr == NULL)
        {
            FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_INVALID_PORT);
        }

        if (linkPtr->link.logicalPort == port)
        {
            realSw   = linkPtr->link.swId;
            realPort = linkPtr->link.swPort;
        }
        else
        {
            realSw   = linkPtr->link.partnerSwitch;
            realPort = linkPtr->link.partnerPort;
        }

        curSwitchPtr = GET_SWITCH_PTR(realSw);

        if (curSwitchPtr != NULL)
        {
            if (curSwitchPtr->switchFamily == FM_SWITCH_FAMILY_SWAG)
            {
                curSwagExt = GET_SWITCH_EXT(realSw);
                curPort    = realPort;
                continue;
            }
        }

        break;
    }

    member = fmFindSwitchInSWAG(curSwagExt, realSw);

    if (member == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_ERR_SWITCH_NOT_IN_AGGREGATE);
    }

    if (member->glortSpace != NULL)
    {
        glort = member->glortSpace->base + realPort;
    }
    else
    {
        glort = (fm_uint32) ~0;
    }

    *glortPtr = glort;

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, FM_OK);

}   /* end fmGetGlortForSwagPhysicalPort */




/*****************************************************************************/
/** fmCrossReferencePortToLink
 * \ingroup intPort
 *
 * \desc            Adds or removes a port-to-link cross reference entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port on which to operate.
 *
 * \param[in]       link points to the internal link record.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCrossReferencePortToLink(fm_int          sw,
                                     fm_int          port,
                                     fm_swagIntLink *link)
{
    fmSWAG_switch *switchExt;
    fm_status      status;

    FM_LOG_ENTRY(FM_LOG_CAT_SWAG,
                 "sw = %d, port = %d, link = %p\n",
                 sw,
                 port,
                 (void *) link);

    switchExt = GET_SWITCH_EXT(sw);

     if (link != NULL)
    {
        status = fmTreeInsert( &switchExt->portLinkTree,
                              (fm_uint64) port,
                              link );
    }
    else
    {
        status = fmTreeRemove(&switchExt->portLinkTree, (fm_uint64) port, NULL);
    }

    FM_LOG_EXIT(FM_LOG_CAT_SWAG, status);

}   /* end fmCrossReferencePortToLink */




#endif
