/* vim:et:sw=4:ts=4:tw=79:
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_api_init.c
 * Creation Date:   December 3, 2012
 * Description:     FM5xxx specific API initialization functions
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

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/*****************************************************************************
 * Global Variables
 *****************************************************************************/
fm_status hlpGetSwitchInfo(fm_int sw, fm_switchInfo *info)
{
    fm_switch *switchPtr;
    fm_status  status = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH, "sw=%d info=%p\n", sw, (void *) info);

    switchPtr = GET_SWITCH_PTR(sw);

    info->numPorts = switchPtr->maxPhysicalPort + 1;
    info->numCardPorts = switchPtr->numCardinalPorts;
    info->memorySize = switchPtr->maxSegments * switchPtr->segmentSize;

    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end hlpGetSwitchInfo */



/* TODO: Replace hard-coded values below with defines */
const fm_switch HLPSwitchDefaultTable =
{
    /* Constants */

    /* FIXME why do we need + 1? LHS qty is "maxPhysicalPort" */
    .maxPhysicalPort                 = HLP_MAX_FABRIC_PHYS_PORT + 1,
    .mirrorTableSize                 = 4,
    .vlanTableSize                   = 4096,

    /* Switch management functions.  */
    .GetSwitchInfo                   = hlpGetSwitchInfo,
//    .SetSwitchAttribute              = hlpSetSwitchAttribute,
//    .GetSwitchAttribute              = hlpGetSwitchAttribute,
//    .SetSwitchState                  = hlpSetSwitchState,

    /* Function Pointers */
    .AllocateDataStructures          = hlpAllocateDataStructures,
    .AllocateVlanTableDataStructures = hlpAllocateVlanTableDataStructures,

    .InitSwitch                      = hlpInitSwitch,
    .ResetSwitch                     = hlpResetSwitch,                  /* Reset Switch */
    .ReleaseSwitch                   = hlpReleaseSwitch,                /* Release Switch */


    .DbgDumpRegister                 = hlpDbgDumpRegister,
    .DbgDumpRegisterV2               = hlpDbgDumpRegisterV2,
    .DbgDumpRegisterV3               = hlpDbgDumpRegisterV3,
    .DbgGetRegisterName              = hlpDbgGetRegisterName,
    .DbgListRegisters                = hlpDbgListRegisters,
    .DbgTakeChipSnapshot             = hlpDbgTakeChipSnapshot,
//    .DbgReadRegister                 = hlpDbgReadRegister,
    .DbgWriteRegister                = hlpDbgWriteRegister,
    .DbgWriteRegisterV2              = hlpDbgWriteRegisterV2,
    .DbgWriteRegisterV3              = hlpDbgWriteRegisterV3,
    .DbgWriteRegisterField           = hlpDbgWriteRegisterField,

    .SendPacketDirected              = fmPlatformSendPacketDirected,
    .SendPacketSwitched              = fmPlatformSendPacketSwitched,

};

/*****************************************************************************
 * Local Function Prototypes
 *****************************************************************************/

static fm_status InitCardinalPortMap(fm_switch *switchPtr);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** InitCardinalPortMap
 * \ingroup intSwitch
 *
 * \desc            Initializes the cardinal port map.
 *
 * \param[in]       switchPtr is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status InitCardinalPortMap(fm_switch *switchPtr)
{
    fm_cardinalPortInfo *cardinalPortInfo;
    fm_status            status;
    fm_int               cpi;
    fm_int               logPort;
    fm_int               logSwitch;
    fm_int               physPort;

    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH,
                 "switchPtr=%p<%d>\n",
                 (void *) switchPtr,
                 switchPtr->switchNumber);

    cardinalPortInfo = &switchPtr->cardinalPortInfo;

    /***************************************************
     * Allocate the cardinal port map.
     **************************************************/

    status = fmAllocCardinalPortMap(switchPtr);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);

    /***************************************************
     * Initialize the cardinal port map.
     **************************************************/

    cpi = 0;

    /* Enumerate the physical ports. */
    for (physPort = 0 ; physPort <= switchPtr->maxPhysicalPort ; physPort++)
    {
        status = fmPlatformMapPhysicalPortToLogical(switchPtr->switchNumber,
                                                    physPort,
                                                    &logSwitch,
                                                    &logPort);

        if (status != FM_OK)
        {
            FM_LOG_DEBUG(FM_LOG_CAT_SWITCH,
                         "physical port %d not defined, skipped\n",
                         physPort);
            continue;
        }

        if (logPort > cardinalPortInfo->maxLogicalPort)
        {
            cardinalPortInfo->maxLogicalPort = logPort;
        }

        cardinalPortInfo->portMap[cpi].logPort  = logPort;
        cardinalPortInfo->portMap[cpi].physPort = physPort;
        cpi++;
    }

    switchPtr->numCardinalPorts = cpi;

    /***************************************************
     * Order cardinal ports by logical port number.
     **************************************************/

    qsort(cardinalPortInfo->portMap,
          switchPtr->numCardinalPorts,
          sizeof(fm_cardinalPort),
          fmCompareCardinalPorts);

    /***************************************************
     * Create the cardinal port index table.
     **************************************************/

    status = fmCreateCardinalPortIndexTable(switchPtr);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);

    /***************************************************
     * Set maximum reserved port number.
     **************************************************/

#if defined(FM_PLATFORM_MAX_RESERVED_PORT)
    switchPtr->maxReservedPort = FM_PLATFORM_MAX_RESERVED_PORT;
#else
    switchPtr->maxReservedPort = switchPtr->maxPhysicalPort;
#endif

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end InitCardinalPortMap */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

fm_status hlpAllocateDataStructures(fm_switch *switchPtr)
{
    FM_LOG_ENTRY( FM_LOG_CAT_SWITCH,
                  "switchPtr=%p<sw=%d>\n",
                  (void *) switchPtr,
                  switchPtr != NULL ? switchPtr->switchNumber : -1 );

    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_OK);

}   /* end hlpAllocateDataStructures */




fm_status hlpGetSwitchPartNumber(fm_int sw, fm_switchPartNum *pn)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH, "sw=%d pn=%p\n", sw, (void *) pn);

    status = fmPlatformGetSwitchPartNumber(sw, pn);

    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end hlpGetSwitchPartNumber */




fm_status hlpIdentifySwitch(fm_int            sw,
                               fm_switchFamily * family,
                               fm_switchModel *  model,
                               fm_switchVersion *version)
{
    FM_LOG_ENTRY(FM_LOG_CAT_SWITCH,
                 "sw=%d family=%p model=%p version=%p\n", 
                 sw,
                 (void *) family,
                 (void *) model,
                 (void *) version);

    *family  = FM_SWITCH_FAMILY_HLP;
    *model   = FM_SWITCH_MODEL_FM10440;
    *version = FM_SWITCH_VERSION_FM10440_A0;

    FM_LOG_DEBUG(FM_LOG_CAT_SWITCH, "Force FM10440 series device\n");

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_OK);

}   /* end hlpIdentifySwitch */




fm_status hlpInitSwitch(fm_switch *switchPtr)
{
    fm_status status;

    FM_LOG_ENTRY( FM_LOG_CAT_SWITCH,
                  "switchPtr=%p<sw=%d>\n",
                  (void *) switchPtr,
                  switchPtr != NULL ? switchPtr->switchNumber : -1 );

    if (switchPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_SWITCH, FM_ERR_INVALID_ARGUMENT);
    }

    /* Initialize the cardinal port map. */
    status = InitCardinalPortMap(switchPtr);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_SWITCH, status);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end hlpInitSwitch */




/*****************************************************************************/
/** hlpResetSwitch
 * \ingroup intSwitch
 *
 * \desc            Puts FocalPoint chip into reset.
 *                  Called through the ResetSwitch function pointer.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status hlpResetSwitch(fm_int sw)
{
    fm_status err;

    err = fmPlatformReset(sw);

    return err;

}   /* end hlpResetSwitch */




/*****************************************************************************/
/** hlpReleaseSwitch
 * \ingroup intSwitch
 *
 * \desc            Takes FocalPoint chip out of reset.
 *                  Called through the ReleaseSwitch function pointer.
 *
 * \param[in]       sw is the switch number.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status hlpReleaseSwitch(fm_int sw)
{
    fm_status err;

    err = fmPlatformRelease(sw);

    return err;

}   /* end hlpReleaseSwitch */

