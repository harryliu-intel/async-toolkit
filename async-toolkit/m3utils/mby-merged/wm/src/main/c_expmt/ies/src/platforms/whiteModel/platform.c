/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform.c
 * Creation Date:   June 13, 2012
 * Description:     Generic platform implementation for software model
 *                  platform.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2006-2013 Intel Corporation. All Rights Reserved.
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


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <fm_sdk_int.h>
#include <platforms/common/instrument/platform_instrument.h>

#ifdef FM_SUPPORT_FM4000
#include <fm_sdk_fm4000_int.h>
#endif

#ifdef FM_SUPPORT_FM6000
#include <fm_sdk_fm6000_int.h>
#endif

#ifdef FM_SUPPORT_FM10000
#include <fm_sdk_fm10000_int.h>
#endif

#ifdef FM_SUPPORT_HLP
#include <fm_sdk_hlp_int.h>
#endif

#ifdef FM_SUPPORT_CPK
#include <fm_sdk_cpk_int.h>
#endif

#include <platforms/whiteModel/platform_remote.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define PRIMARY_SW 0

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

fm_rootPlatform *fmRootPlatform = NULL;

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/**************************************************
 * Logical/physical port map
 *
 * Indexed by logical port, entry is physical port.
 **************************************************/
static fm_int *fmPlatformPortRemapTable = NULL;
static fm_int  fmMaxPlatformLogicalPort;

/**************************************************
 * Physical/logical port map
 *
 * Indexed by physical port, entry is logical port.
 **************************************************/
static fm_int *fmPhysicalPortRemapTable = NULL;

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static fm_status GetPlatformSwitchTypes(void);

static void *PlatformInterruptListener(void *args);

/*****************************************************************************
 * Local Functions
 *****************************************************************************/
static fm_status ModelReceivePacket(fm_int                sw,
                                    fm_int *              port,
                                    fm_byte *             packet,
                                    fm_int *              length,
                                    fm_int                maxPktSize,
                                    fm_modelSidebandData *sbData)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(port);
    FM_NOT_USED(packet);
    FM_NOT_USED(length);
    FM_NOT_USED(maxPktSize);
    FM_NOT_USED(sbData);

    return FM_ERR_UNSUPPORTED;

}   /* end ModelReceivePacket */


static fm_status ModelSendPacket(fm_int                sw,
                                 fm_int                port,
                                 fm_byte *             packet,
                                 fm_int                length,
                                 fm_modelSidebandData *sbData)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(port);
    FM_NOT_USED(packet);
    FM_NOT_USED(length);
    FM_NOT_USED(sbData);

	return FM_ERR_UNSUPPORTED;

}   /* end ModelSendPacket */





/*****************************************************************************/
/** GetPlatformSwitchTypes
 * \ingroup intPlatform
 *
 * \desc            Parses api.platform.model.switchType API attribute and
 *                  initializes the switch type for each modeled switch.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
static fm_status GetPlatformSwitchTypes(void)
{
    fm_platform_state *ps;
    fm_status          status = FM_OK;
    fm_int             sw;
    fm_int             switchNum;
    fm_text            nextToken;
    fm_text            nextType;
    fm_text            switchFamily;
    fm_text            switchNumber;
    fm_text            switchType;
    fm_text            switchType2;
    fm_text            textValue;
    rsize_t            length;
    rsize_t            length2;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    textValue = fmGetTextApiAttribute(FM_AAK_API_PLATFORM_MODEL_SWITCH_TYPE,
                                      FM_AAD_API_PLATFORM_MODEL_SWITCH_TYPE);
    /* Create a local copy of the attribute value as it will be modified by
     * FM_STRTOK_S. */
    switchType = fmStringDuplicate(textValue);

    if (switchType == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_NO_MEM);
    }

    /* fmStringDuplicate is guaranteed to NUL-terminate the returned buffer. */
    length = strlen(switchType) + 1;
    switchType2 = FM_STRTOK_S(switchType, &length, ",", &nextType);
    for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++)
    {
        if (switchType2 == NULL)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Not enough switch types specified\n");
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
        }

        FM_LOG_DEBUG(FM_LOG_CAT_PLATFORM, "sw=%d type=%s\n", sw, switchType2);

        /* FM_STRTOK_S is guaranteed to NUL-terminate the returned buffer. */
        length2 = strlen(switchType2) + 1;
        switchNumber = FM_STRTOK_S(switchType2, &length2, "-", &nextToken);
        switchFamily = FM_STRTOK_S(NULL, &length2, "-", &nextToken);
        if (switchNumber == NULL || switchFamily == NULL)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Invalid \"%s\" attribute format.\n",
                         FM_AAK_API_PLATFORM_MODEL_SWITCH_TYPE);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
        }

        switchNum = atoi(switchNumber);
        if ( (switchNum < 0) || (switchNum >= FM_MAX_NUM_FOCALPOINTS) )
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Switch number not in [0..%d]\n",
                         FM_MAX_NUM_FOCALPOINTS - 1);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
        }

        ps = &fmRootPlatform->fmPlatformState[switchNum];

        if (ps->switchType != FM_PLATFORM_SWITCH_TYPE_UNKNOWN)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Duplicate switch type for switch %d\n",
                         switchNum);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
        }

        if (strcasecmp(switchFamily, "FM4000") == 0)
        {
            ps->switchType = FM_PLATFORM_SWITCH_TYPE_FM4000;
        }
        else if (strcasecmp(switchFamily, "FM6000") == 0)
        {
            ps->switchType = FM_PLATFORM_SWITCH_TYPE_FM6000;
        }
        else if (strcasecmp(switchFamily, "FM10000") == 0)
        {
            ps->switchType = FM_PLATFORM_SWITCH_TYPE_FM10000;
        }
        else if (strcasecmp(switchFamily, "HLP") == 0)
        {
            ps->switchType = FM_PLATFORM_SWITCH_TYPE_HLP;
        }
        else if (strcasecmp(switchFamily, "CPK") == 0)
        {
            ps->switchType = FM_PLATFORM_SWITCH_TYPE_CPK;
        } 
        else
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "%s: Invalid switch family type\n",
                         switchFamily);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, status = FM_FAIL);
        }

        switchType2 = nextToken = FM_STRTOK_S(NULL, &length, ",", &nextType);
    }

    if (switchType2 != NULL)
    {
        FM_LOG_INFO(FM_LOG_CAT_PLATFORM, "Too many switch types specified\n");
    }

ABORT:
    fmFree(switchType);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, status);

}   /* end GetPlatformSwitchTypes */




/*****************************************************************************/
/** InitPortTables
 * \ingroup intPlatform
 *
 * \desc            Initializes the data structures used to map between
 *                  logical and physical port numbers.
 *
 * \param[in]       portTable points to the master port table.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status InitPortTables(const fm_platformPort * portTable)
{
    fm_platform_state *ps;
    fm_status          err = FM_OK;
    fm_int             i;
    fm_int             logPort;
    fm_int             nbytes;
    fm_int             numEntries;
    fm_int             physPort;
    fm_int             primarySw;
    errno_t            rv;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "portTable=%p\n", (void *) portTable);

    primarySw = fmRootPlatform->primarySw;
    ps = &fmRootPlatform->fmPlatformState[primarySw];

    /**************************************************
     * Determine the maximum logical port number.
     **************************************************/

    fmMaxPlatformLogicalPort = -1;

    for (numEntries = 0 ;
         (logPort = portTable[numEntries].logPort) >= 0 ;
         numEntries++)
    {
        if (logPort > fmMaxPlatformLogicalPort)
        {
            fmMaxPlatformLogicalPort = logPort;
        }
    }

    /**************************************************
     * Allocate the logical-to-physical and
     * physical-to-logical port maps.
     **************************************************/

    nbytes = sizeof(fm_int) * (fmMaxPlatformLogicalPort + 1);

    fmPlatformPortRemapTable = fmAlloc(nbytes);
    if (fmPlatformPortRemapTable == NULL)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_NO_MEM);
    }

    rv = FM_MEMSET_S(fmPlatformPortRemapTable, nbytes, -1, nbytes);
    if (rv != 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_FAIL);
    }

    nbytes = sizeof(fm_int) * ps->maxPorts;

    fmPhysicalPortRemapTable = fmAlloc(nbytes);
    if (fmPhysicalPortRemapTable == NULL)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_NO_MEM);
    }

    rv = FM_MEMSET_S(fmPhysicalPortRemapTable, nbytes, -1, nbytes);
    if (rv != 0)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_FAIL);
    }

    /**************************************************
     * Initialize both port remap tables.
     **************************************************/

    for (i = 0 ; i < numEntries ; i++)
    {
        physPort = portTable[i].physPort;
        logPort  = portTable[i].logPort;

        if (physPort < 0 || physPort > (ps->maxPorts - 1) )
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Physical port %d invalid, not in [0, %d]\n",
                         physPort,
                         ps->maxPorts - 1);
            err = FM_ERR_INVALID_PORT;
            continue;
        }

        if (fmPlatformPortRemapTable[logPort] != -1)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Logical port %d mapped to both %d and %d\n",
                         logPort,
                         fmPlatformPortRemapTable[logPort],
                         physPort);
            err = FM_ERR_INVALID_PORT;
            continue;
        }

        if (fmPhysicalPortRemapTable[physPort] != -1)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Physical port %d mapped to both %d and %d\n",
                         physPort,
                         fmPhysicalPortRemapTable[physPort],
                         logPort);
            err = FM_ERR_INVALID_PORT;
            continue;
        }

        fmPlatformPortRemapTable[logPort]  = physPort;
        fmPhysicalPortRemapTable[physPort] = logPort;
    }

ABORT:
    if (err != FM_OK)
    {
        if (fmPlatformPortRemapTable != NULL)
        {
            fmFree(fmPlatformPortRemapTable);
            fmPlatformPortRemapTable = NULL;
        }

        if (fmPhysicalPortRemapTable != NULL)
        {
            fmFree(fmPhysicalPortRemapTable);
            fmPhysicalPortRemapTable = NULL;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end InitPortTables */



/*****************************************************************************/
/* fmPlatformRootInit
 * \ingroup intPlatform
 *
 * \desc            Platform initialization that is only done once for all
 *                  processes.
 *
 * \param           None.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if memory allocation error.
 * \return          FM_ERR_UNINITIALIZED if ALOS not intialized.
 * \return          FM_ERR_INVALID_ARGUMENT if lck is NULL.
 * \return          FM_ERR_LOCK_INIT if unable to initialize lock.
 * \return          FM_ERR_CANT_IDENTIFY_SWITCH if unable to identify switch.
 *
 *****************************************************************************/
static fm_status fmPlatformRootInit(void)
{
    fm_event *                  insertEvent;
    fm_eventSwitchInserted *    insert;
    fm_modelPacketQueueServices pktQueueServices;
    fm_platform_state *         ps;
    fm_status                   err;
    fm_uint32 *                 bufferMemoryPool = NULL;
    fm_int                      i;
    fm_int                      primarySw;
    fm_int                      sw;
    fm_text                     attrFile;
    fm_bool                     boolValue;

    FM_LOG_ENTRY_NOARGS(FM_LOG_CAT_PLATFORM);

    fmRootPlatform = fmAlloc(sizeof(fm_rootPlatform));
    if (fmRootPlatform == NULL)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_NO_MEM);
    }

    /* initialize attributes */
    if ( (attrFile = getenv("FM_API_ATTR_FILE")) != NULL )
    {
        err = fmPlatformLoadAttributes(attrFile);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }

    err = fmPlatformLoadAttributes("fm_api_attributes.cfg");
    if ( ( err != FM_OK ) && ( err != FM_ERR_INVALID_ARGUMENT ) )
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }

    /* initialize globals */
    FM_CLEAR(*fmRootPlatform);

    primarySw = fmRootPlatform->primarySw = PRIMARY_SW;
    ps = &fmRootPlatform->fmPlatformState[primarySw];

    /* Fetches the attribute api.platform.model.switchType and fills in the
     * switchType in platform_state */
    err = GetPlatformSwitchTypes();
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    switch (ps->switchType)
    {
#ifdef FM_SUPPORT_FM4000
        case FM_PLATFORM_SWITCH_TYPE_FM4000:
            i = 240;
            err = fmSetApiAttribute(FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    FM_AAT_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    &i);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            /* Initialize function pointers for model packet queue. */
            pktQueueServices.SendPacket = fm4000ModelSendPacket;
            pktQueueServices.ReceivePacket = fm4000ModelReceivePacket;
            pktQueueServices.CheckBpduDropping =
                fm4000ModelPlatformCheckBpduDropping;
            pktQueueServices.GetCpuMaxFrameSize =
                fm4000ModelPlatformGetCpuMaxFrameSize;
            pktQueueServices.GetCpuVlanTag = fm4000ModelPlatformGetCpuVlanTag;
            pktQueueServices.GetNumPorts = fm4000ModelPlatformGetNumPorts;
            break;
#endif

#ifdef FM_SUPPORT_FM6000
        case FM_PLATFORM_SWITCH_TYPE_FM6000:
            /***************************************************
             * On Alta white model a single MA Table purge
             * operation takes close to 2 seconds to completes.
             * So set the semaphore timeout high enough to
             * not disturb the regression tests.
             **************************************************/
            i = 240;
            err = fmSetApiAttribute(FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    FM_AAT_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    &i);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            /* Disable loading of SPICO code */
            boolValue = FALSE;
            err = fmSetApiAttribute(FM_AAK_API_FM6000_LOAD_SPICO_CODE,
                                    FM_AAT_API_FM6000_LOAD_SPICO_CODE,
                                    &boolValue);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
            pktQueueServices.SendPacket = fm6000ModelSendPacket;
            pktQueueServices.ReceivePacket = fm6000ModelReceivePacket;
            pktQueueServices.CheckBpduDropping =
                fm6000ModelPlatformCheckBpduDropping;
            pktQueueServices.GetCpuMaxFrameSize =
                fm6000ModelPlatformGetCpuMaxFrameSize;
            pktQueueServices.GetCpuVlanTag = fm6000ModelPlatformGetCpuVlanTag;
            pktQueueServices.GetNumPorts = fm6000ModelPlatformGetNumPorts;
            break;
#endif

#ifdef FM_SUPPORT_FM10000
        case FM_PLATFORM_SWITCH_TYPE_FM10000:
            i = 240;
            err = fmSetApiAttribute(FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    FM_AAT_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    &i);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            /* Initialize function pointers for model packet queue. */
            pktQueueServices.SendPacket = fm10000ModelSendPacket;
            pktQueueServices.ReceivePacket = fm10000ModelReceivePacket;
            pktQueueServices.CheckBpduDropping =
                fm10000ModelPlatformCheckBpduDropping;
            pktQueueServices.GetCpuMaxFrameSize =
                fm10000ModelPlatformGetCpuMaxFrameSize;
            pktQueueServices.GetCpuVlanTag = fm10000ModelPlatformGetCpuVlanTag;
            pktQueueServices.GetNumPorts = fm10000ModelPlatformGetNumPorts;
            break;
#endif

#ifdef FM_SUPPORT_HLP
        case FM_PLATFORM_SWITCH_TYPE_HLP:
            i = 240;
            err = fmSetApiAttribute(FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    FM_AAT_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    &i);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            /* Initialize function pointers for model packet queue. */
            if (strlen(fmGetTextApiAttribute(
                    FM_AAK_API_PLATFORM_MODEL_SERVER,
                    FM_AAD_API_PLATFORM_MODEL_SERVER)))
            {
                /* Model is remote */
                pktQueueServices.SendPacket = ModelSendPacket;
                pktQueueServices.ReceivePacket = ModelReceivePacket;
            }
            else
            {
                pktQueueServices.SendPacket = hlpModelSendPacket;
                pktQueueServices.ReceivePacket = hlpModelReceivePacket;
            }
            pktQueueServices.CheckBpduDropping =
                hlpModelPlatformCheckBpduDropping;
            pktQueueServices.GetCpuMaxFrameSize =
                hlpModelPlatformGetCpuMaxFrameSize;
            pktQueueServices.GetCpuVlanTag = hlpModelPlatformGetCpuVlanTag;
            pktQueueServices.GetNumPorts = hlpModelPlatformGetNumPorts;
            break;
#endif

#ifdef FM_SUPPORT_CPK
        case FM_PLATFORM_SWITCH_TYPE_CPK:
            i = 240;
            err = fmSetApiAttribute(FM_AAK_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    FM_AAT_API_LAG_DELETE_SEMAPHORE_TIMEOUT,
                                    &i);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            /* Initialize function pointers for model packet queue. */
            pktQueueServices.SendPacket = cpkModelSendPacket;
            pktQueueServices.ReceivePacket = cpkModelReceivePacket;
            pktQueueServices.CheckBpduDropping =
                cpkModelPlatformCheckBpduDropping;
            pktQueueServices.GetCpuMaxFrameSize =
                cpkModelPlatformGetCpuMaxFrameSize;
            pktQueueServices.GetCpuVlanTag = cpkModelPlatformGetCpuVlanTag;
            pktQueueServices.GetNumPorts = cpkModelPlatformGetNumPorts;
            break;
#endif

        default:
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_UNSUPPORTED);
    }

    err = fmModelPacketQueueInitialize(&fmRootPlatform->packetQueue,
                                       &pktQueueServices);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    boolValue = fmGetBoolApiAttribute(FM_AAK_API_PLATFORM_MODEL_SEND_EOT,
                                      FM_AAD_API_PLATFORM_MODEL_SEND_EOT);

    err = fmModelPacketQueueSetAttribute(FM_MODEL_PACKET_QUEUE_SEND_EOT,
                                         &boolValue);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    /* Initialize buffer memory */
    bufferMemoryPool = fmAlloc(FM_BUFFER_SIZE_BYTES * FM_NUM_BUFFERS);
    if (bufferMemoryPool == NULL)
    {
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_NO_MEM);
    }

    err = fmPlatformInitBuffers(bufferMemoryPool);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++)
    {
        ps = &fmRootPlatform->fmPlatformState[sw];

        ps->intrSource = FM_INTERRUPT_SOURCE_NONE;
        /* assign switch number */
        ps->sw = sw;

        /* Initialize function pointers for white model platform */
        switch (ps->switchType)
        {
#ifdef FM_SUPPORT_FM4000
            case FM_PLATFORM_SWITCH_TYPE_FM4000:
                ps->maxPorts           = FM4000_MAX_PORT + 1;

                ps->ModelGetPortMap    = fm4000ModelGetPortMap;
                ps->ModelInitialize    = fm4000ModelInitialize;
                ps->ModelReadCSR       = fm4000ModelReadCSR;
                ps->ModelReadCSRMult   = fm4000ModelReadCSRMult;
                ps->ModelReadCSR64     = fm4000ModelReadCSR64;
                ps->ModelReadCSRMult64 = fm4000ModelReadCSRMult64;
                ps->ModelReset         = fm4000ModelReset;
                ps->ModelTick          = fm4000ModelTick;
                ps->ModelWriteCSR      = fm4000ModelWriteCSR;
                break;
#endif

#ifdef FM_SUPPORT_FM6000
            case FM_PLATFORM_SWITCH_TYPE_FM6000:
                ps->maxPorts           = FM6000_MAX_PORT + 1;

                ps->ModelGetPortMap    = fm6000ModelGetPortMap;
                ps->ModelInitialize    = fm6000ModelInitialize;
                ps->ModelReadCSR       = fm6000ModelReadCSR;
                ps->ModelReadCSRMult   = fm6000ModelReadCSRMult;
                ps->ModelReadCSR64     = fm6000ModelReadCSR64;
                ps->ModelReadCSRMult64 = fm6000ModelReadCSRMult64;
                ps->ModelReset         = fm6000ModelReset;
                ps->ModelTick          = fm6000ModelTick;
                ps->ModelWriteCSR      = fm6000ModelWriteCSR;
                break;
#endif

#ifdef FM_SUPPORT_FM10000
            case FM_PLATFORM_SWITCH_TYPE_FM10000:
                /* see bug 24962 on max port issues for fm10000 */
                ps->maxPorts           = FM10000_MAX_FABRIC_PHYS_PORT + 1;

                ps->ModelGetPortMap    = fm10000ModelGetPortMap;
                ps->ModelInitialize    = fm10000ModelInitialize;
                ps->ModelReadCSR       = fm10000ModelReadCSR;
                ps->ModelReadCSRMult   = fm10000ModelReadCSRMult;
                ps->ModelReadCSR64     = fm10000ModelReadCSR64;
                ps->ModelReadCSRMult64 = fm10000ModelReadCSRMult64;
                ps->ModelReset         = fm10000ModelReset;
                ps->ModelResetV2       = fm10000ModelResetV2;
                ps->ModelTick          = fm10000ModelTick;
                ps->ModelWriteCSR      = fm10000ModelWriteCSR;
                break;
#endif

#ifdef FM_SUPPORT_HLP
            case FM_PLATFORM_SWITCH_TYPE_HLP:
                ps->maxPorts           = HLP_MAX_FABRIC_PHYS_PORT + 1;

                ps->ModelGetPortMap    = hlpModelGetPortMap;
                if (strlen(fmGetTextApiAttribute(
                        FM_AAK_API_PLATFORM_MODEL_SERVER,
                        FM_AAD_API_PLATFORM_MODEL_SERVER)))
                {
                    /* Model is remote */
                    ps->ModelInitialize    = platformRemoteInitialize;
                    ps->ModelReadCSR       = platformRemoteReadCSR;
                    ps->ModelReadCSRMult   = platformRemoteReadCSRMult;
                    ps->ModelReadCSR64     = platformRemoteReadCSR64;
                    ps->ModelReadCSRMult64 = platformRemoteReadCSRMult64;
                    ps->ModelReset         = platformRemoteReset;
                    ps->ModelResetV2       = platformRemoteResetV2;
                    ps->ModelTick          = platformRemoteTick;
                    ps->ModelWriteCSR      = platformRemoteWriteCSR;
                }
                else
                {
                    ps->ModelInitialize    = hlpModelInitialize;
                    ps->ModelReadCSR       = hlpModelReadCSR;
                    ps->ModelReadCSRMult   = hlpModelReadCSRMult;
                    ps->ModelReadCSR64     = hlpModelReadCSR64;
                    ps->ModelReadCSRMult64 = hlpModelReadCSRMult64;
                    ps->ModelReset         = hlpModelReset;
                    ps->ModelResetV2       = hlpModelResetV2;
                    ps->ModelTick          = hlpModelTick;
                    ps->ModelWriteCSR      = hlpModelWriteCSR;
                }
                break;
#endif

#ifdef FM_SUPPORT_CPK
            case FM_PLATFORM_SWITCH_TYPE_CPK:
                ps->maxPorts           = CPK_MAX_FABRIC_PHYS_PORT + 1;

                ps->ModelGetPortMap    = cpkModelGetPortMap;
                ps->ModelInitialize    = cpkModelInitialize;
                ps->ModelReadCSR       = cpkModelReadCSR;
                ps->ModelReadCSRMult   = cpkModelReadCSRMult;
                ps->ModelReadCSR64     = cpkModelReadCSR64;
                ps->ModelReadCSRMult64 = cpkModelReadCSRMult64;
                ps->ModelReset         = cpkModelReset;
                ps->ModelResetV2       = cpkModelResetV2;
                ps->ModelTick          = cpkModelTick;
                ps->ModelWriteCSR      = cpkModelWriteCSR;
                break;
#endif

            default:
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM,
                                    err = FM_ERR_UNSUPPORTED);
        }

        /* initialize locks */
        for (i = 0 ; i < FM_MAX_PLAT_LOCKS ; i++)
        {
            if (i == FM_MEM_TYPE_CSR)
            {
                err = fmCreateLockV2("Platform Register Access",
                                     ps->sw,              /* switch number */
                                     FM_LOCK_PREC_PLATFORM,
                                     &ps->accessLocks[i]);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
            }
            else
            {
                err = fmCreateLock("Platform Access", &ps->accessLocks[i]);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
            }
        }

        /***************************************************
         * Allocate the model instance, currently only one
         **************************************************/
        switch (ps->switchType)
        {
#ifdef FM_SUPPORT_FM4000
            case FM_PLATFORM_SWITCH_TYPE_FM4000:
                err = ps->ModelInitialize(&ps->chipModel, ps->sw, NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_FM6000
            case FM_PLATFORM_SWITCH_TYPE_FM6000:
                err = fm6000ModelPlatformInitializeMicrocodeLib(sw);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
               
                err = ps->ModelInitialize(&ps->chipModel, ps->sw, ps->ucFuncs);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_FM10000
            case FM_PLATFORM_SWITCH_TYPE_FM10000:
                err = ps->ModelInitialize(&ps->chipModel, ps->sw, NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_HLP
            case FM_PLATFORM_SWITCH_TYPE_HLP:
                err = ps->ModelInitialize(&ps->chipModel, ps->sw, NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_CPK
            case FM_PLATFORM_SWITCH_TYPE_CPK:
                err = ps->ModelInitialize(&ps->chipModel, ps->sw, NULL);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif
                
            default:
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM,
                                    err = FM_ERR_UNSUPPORTED);
        }

        /***************************************************
         * Allocate the packet queue used by the models
         **************************************************/
        err = fmModelPacketQueueAddSwitch(fmRootPlatform->packetQueue,
                                          ps->sw,
                                          ps->maxPorts);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        /***************************************************
         * Identify the device.
         **************************************************/
        switch (ps->switchType)
        {
#ifdef FM_SUPPORT_FM4000
            case FM_PLATFORM_SWITCH_TYPE_FM4000:
                ps->family  = FM_SWITCH_FAMILY_FM4000;
                ps->model   = FM_SWITCH_MODEL_FM4224;
                ps->version = FM_SWITCH_VERSION_FM4224_A3;
                break;
#endif

#ifdef FM_SUPPORT_FM6000
            case FM_PLATFORM_SWITCH_TYPE_FM6000:
                ps->family  = FM_SWITCH_FAMILY_FM6000;
                ps->model   = FM_SWITCH_MODEL_FM6224;
                ps->version = FM_SWITCH_VERSION_FM6364_B0;
                break;
#endif

#ifdef FM_SUPPORT_FM10000
            case FM_PLATFORM_SWITCH_TYPE_FM10000:
                ps->family  = FM_SWITCH_FAMILY_FM10000;
                ps->model   = FM_SWITCH_MODEL_FM10440;
                ps->version = FM_SWITCH_VERSION_FM10440_A0;
                break;
#endif

#ifdef FM_SUPPORT_HLP
            case FM_PLATFORM_SWITCH_TYPE_HLP:
                ps->family  = FM_SWITCH_FAMILY_HLP;
                ps->model   = FM_SWITCH_MODEL_FM10440;
                ps->version = FM_SWITCH_VERSION_FM10440_A0;
                break;
#endif

#ifdef FM_SUPPORT_CPK
            case FM_PLATFORM_SWITCH_TYPE_CPK:
                ps->family  = FM_SWITCH_FAMILY_CPK;
                ps->model   = FM_SWITCH_MODEL_FM10440;
                ps->version = FM_SWITCH_VERSION_FM10440_A0;
                break;
#endif
                
            default:
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM,
                                    err = FM_ERR_UNSUPPORTED);
        }

        /***************************************************
         * Allocate and generate the switch inserted event
         * for this switch.
         **************************************************/

        /* This is priority high because we don't want to be throttled */
        insertEvent = fmAllocateEvent(ps->sw,
                                      FM_EVID_SYSTEM,
                                      FM_EVENT_SWITCH_INSERTED,
                                      FM_EVENT_PRIORITY_HIGH);

        if (insertEvent == NULL)
        {
            FM_LOG_FATAL(FM_LOG_CAT_PLATFORM,
                         "Unable to allocate event for switch insertion\n");
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_FAIL);
        }

        insert = &insertEvent->info.fpSwitchInsertedEvent;
        insert->model = -1;
        insert->slot  = ps->sw;

        err = fmSendThreadEvent(&fmRootApi->eventThread, insertEvent);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        /* create thread to listen for when an interrupt occurs */
        err = fmCreateThread("interrupt_listener",
                             FM_EVENT_QUEUE_SIZE_NONE,
                             &PlatformInterruptListener,
                             ps,
                             &fmRootPlatform->fmPlatformState[sw].intrListener);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }

ABORT:
    if (err != FM_OK)
    {
        if (bufferMemoryPool != NULL)
        {
            fmFree(bufferMemoryPool);
        }

        if (fmRootPlatform != NULL)
        {
            if (fmRootPlatform->packetQueue != NULL)
            {
                fmFree(fmRootPlatform->packetQueue);
            }

            fmFree(fmRootPlatform);
            fmRootPlatform = NULL;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformRootInit */




/*****************************************************************************/
/** PlatformInterruptListener
 * \ingroup intPlatform
 *
 * \desc            Thread to call white model tick function, which returns
 *                  the current interrupt state of the model.
 *
 * \param[in]       args contains thread-initialization parameters
 *
 * \return          None.
 *
 *****************************************************************************/
static void *PlatformInterruptListener(void *args)
{
    fm_thread *        thread;
    fm_platform_state *ps;
    fm_status          err;
    fm_uint32          globalInterruptDetect;
    fm_switch *        swPtr;

    /* grab arguments */
    thread = FM_GET_THREAD_HANDLE(args);
    ps     = FM_GET_THREAD_PARAM(fm_platform_state, args);

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "thread = %s, ps = %p, ps->sw = %d\n",
                 thread->name,
                 (void *) ps,
                 ps->sw);

    while (TRUE)
    {
        fmDelay(FM_MODEL_TICK_SECS, FM_MODEL_TICK_NANOSECS);

        swPtr = GET_SWITCH_PTR(ps->sw);
        if (swPtr == NULL)
        {
            continue;
        }

        if ( swPtr->state == FM_SWITCH_STATE_INIT )
        {
            TAKE_PLAT_LOCK(ps->sw, FM_PLAT_INFO);

            err = ps->ModelTick(ps->sw, &globalInterruptDetect);
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            if (ps->swInterruptEnable && globalInterruptDetect)
            {
                ps->intrSource       |= FM_INTERRUPT_SOURCE_ISR;
                ps->swInterruptEnable = FALSE;

                FM_LOG_DEBUG(FM_LOG_CAT_EVENT, "Signaling semaphore\n");

                err = fmSignalSemaphore(&fmRootApi->intrAvail);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

            }   /* end if (globalInterruptDetect) */
ABORT:
            DROP_PLAT_LOCK(ps->sw, FM_PLAT_INFO);
        }

    }   /* end while (TRUE) */

    return NULL;

}   /* end PlatformInterruptListener */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmPlatformInitialize
 * \ingroup platform
 *
 * \desc            Called as part of API initialization to perform basic
 *                  platform-specific initialization. In particular, this
 *                  function should do the following:
 *                                                                      \lb\lb
 *                  1. Initialize any platform-global constructs (e.g.,
 *                  structures, variables, etc.).
 *                                                                      \lb\lb
 *                  2. Perform actions required by the operating system for
 *                  the API to run on this platform. For example, under
 *                  Linux, memory must be mapped into user space for use as
 *                  packet buffers, the switch device register file must be
 *                  memory mapped into user space and an IRQ must be reserved.
 *                                                                      \lb\lb
 *                  3. For platforms with fixed switch devices (i.e., not
 *                  a hot-swappable bladed chassis), an
 *                  ''FM_EVENT_SWITCH_INSERTED'' event should be sent to the
 *                  API's global event handler (fmGlobalEventHandler) for
 *                  each switch device by calling ''fmSendThreadEvent''.
 *                  Note: if ''FM_EVENT_SWITCH_INSERTED'' events are sent, it
 *                  should be the last thing done by this function before
 *                  returning.
 *
 * \param[out]      nSwitches points to storage where this function should
 *                  place the maximum number of switch devices supported
 *                  by the platform. Note that this value may differ from the
 *                  actual number of existing devices on platforms that
 *                  support hot-swappable switches, such as bladed chassis.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformInitialize(fm_int *nSwitches)
{
    fm_platformPort *  portMap = NULL;
    fm_platform_state *ps;
    fm_status          err;
    fm_int             primarySw;
    fm_int             sw;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "nSwitches = %p\n", (void *) nSwitches);

    *nSwitches = FM_MAX_NUM_FOCALPOINTS;

#if INSTRUMENT_LOG_LEVEL > 0
    fmPlatformOpenInstrumentation();
#endif

    err = fmGetRoot("platform-wm-reg",
                    (void **) &fmRootPlatform,
                    fmPlatformRootInit);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    primarySw = fmRootPlatform->primarySw;
    ps = &fmRootPlatform->fmPlatformState[primarySw];

    for (sw = 0 ; sw < FM_MAX_NUM_FOCALPOINTS ; sw++)
    {
        err = fmPlatformEnableInterrupt(sw, FM_INTERRUPT_SOURCE_ISR);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }

    /***************************************************
     * Initialize the port remap tables.
     **************************************************/
    if (fmPlatformPortRemapTable == NULL)
    {
        /* Allocate enough memory for the number of ports + 1. The extra entry
         * is for the end of table delimiter */
        portMap = fmAlloc( (ps->maxPorts + 1) * sizeof(fm_platformPort));
        if (portMap == NULL)
        {
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err = FM_ERR_NO_MEM);
        }

        err = ps->ModelGetPortMap(portMap, ps->maxPorts);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        err = InitPortTables(portMap);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }

ABORT:
    if (portMap != NULL)
    {
        fmFree(portMap);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformInitialize */




/*****************************************************************************/
/** fmPlatformSwitchPreInitialize
 * \ingroup platform
 *
 * \desc            Called prior to initializing a switch device. This
 *                  function is responsible for initializing any aspect
 *                  of the platform required to bring up the device, for
 *                  example, applying power, deasserting the reset signal,
 *                  etc.
 *
 * \param[in]       sw is the switch about to be initialized.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSwitchPreInitialize(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_OK;

}   /* end fmPlatformSwitchPreInitialize */




/*****************************************************************************/
/** fmPlatformSwitchPostInitialize
 * \ingroup platform
 *
 * \desc            Called after initializing a switch device. This
 *                  function is responsible for initializing any aspect
 *                  of the switch that is dependent on the platform
 *                  hardware. Examples would be setting lane polarity, lane
 *                  ordering, SERDES drive strength and emphasis, LCI
 *                  endianness, etc.
 *
 * \note            The switch is not yet in an UP state when this function
 *                  is called. Operations that depend on event reporting
 *                  from the switch should not be performed by this function,
 *                  for example, retrieving port state with ''fmGetPortState''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSwitchPostInitialize(fm_int sw)
{
    fm_platform_state *ps;
    fm_switch *        switchPtr;
    fm_status          err = FM_OK;
    fm_int             physPort;
    fm_int             port;
    fm_int             cpi;
    fm_bool            bEnabled = FM_ENABLED;
#ifdef FM_SUPPORT_FM6000
    fm_dfeMode         dfeMode = FM_DFE_MODE_STATIC;
    fm_ethMode         ethMode;
    fm_int             channel;
    fm_int             epl;
    fm_bool            bDisabled = FM_DISABLED;
#endif

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d\n", sw);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];
    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    /**************************************************
     * Set all ports to 1-lane 10G mode.
     **************************************************/
    for (cpi = 1 ; cpi < switchPtr->numCardinalPorts ; cpi++)
    {
        err = fmMapCardinalPortInternal(switchPtr, cpi, &port, &physPort);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

        switch (ps->switchType)
        {
#ifdef FM_SUPPORT_FM4000
            case FM_PLATFORM_SWITCH_TYPE_FM4000:
                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_LINK_INTERRUPT,
                                         &bEnabled);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_FM6000
            case FM_PLATFORM_SWITCH_TYPE_FM6000:
                err = fm6000MapPhysicalPortToEplChannel(sw,
                                                        physPort,
                                                        &epl,
                                                        &channel);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

                if (epl == 0)
                {
                    /* This physical port is associated with a management port.
                     */
                    continue;
                }

                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_LINK_INTERRUPT,
                                         &bEnabled);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

                ethMode = FM_ETH_MODE_10GBASE_CR;
                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_ETHERNET_INTERFACE_MODE,
                                         &ethMode);
                if (err != FM_OK)
                {
                    FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                                 "Unable to put port %d into Ethernet mode %d!\n",
                                 port,
                                 ethMode);
                    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                }

                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_DFE_MODE,
                                         &dfeMode);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_HLP
            case FM_PLATFORM_SWITCH_TYPE_HLP:
                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_LINK_INTERRUPT,
                                         &bEnabled);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif
   
#ifdef FM_SUPPORT_CPK
            case FM_PLATFORM_SWITCH_TYPE_CPK:
                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_LINK_INTERRUPT,
                                         &bEnabled);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

#ifdef FM_SUPPORT_FM10000
            case FM_PLATFORM_SWITCH_TYPE_FM10000:
                err = fmSetPortAttribute(sw,
                                         port,
                                         FM_PORT_LINK_INTERRUPT,
                                         &bEnabled);
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
                break;
#endif

            default:
                FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM,
                                    err = FM_ERR_UNSUPPORTED);
        }
    }


#ifdef FM_SUPPORT_FM6000
    if (ps->switchType == FM_PLATFORM_SWITCH_TYPE_FM6000)
    {
        err = fmSetApiAttribute("api.FM6000.linkDependsOnDfe",
                                FM_API_ATTR_BOOL,
                                &bDisabled);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);
    }
#endif

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformSwitchPostInitialize */




/*****************************************************************************/
/** fmPlatformGetPortCapabilities
 * \ingroup platform
 *
 * \desc            Returns a mask of capabilities for the specified physical
 *                  port.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       physPort is the physical port number.
 *
 * \param[out]      capabilities points to storage where a bitmap of
 *                  capabilities will be written. See ''Port Capabilities''
 *                  for a list of possible bits.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if either sw or physPort are not
 *                  valid values.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformGetPortCapabilities(fm_int     sw,
                                        fm_int     physPort,
                                        fm_uint32 *capabilities)
{
    fm_status err;
    fm_switch *switchPtr;
    fm_int logPort;
    fm_int logSwitch;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d physPort=%d capabilities=%p\n",
                 sw,
                 physPort,
                 (void *) capabilities);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    if ( (physPort < 0) || (physPort > switchPtr->maxPhysicalPort) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }
    
    err = fmPlatformMapPhysicalPortToLogical(sw, physPort, &logSwitch, &logPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    if (logPort == 0)
    {
        *capabilities = 0;
    }
    else
    {
        *capabilities = FM_PORT_CAPABILITY_LAG_CAPABLE |
                        FM_PORT_CAPABILITY_SPEED_10M |
                        FM_PORT_CAPABILITY_SPEED_100M |
                        FM_PORT_CAPABILITY_SPEED_1G |
                        FM_PORT_CAPABILITY_SPEED_10G;
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformGetPortCapabilities */




/*****************************************************************************/
/** fmPlatformMapLogicalPortToPhysical
 * \ingroup platform
 *
 * \desc            Maps a logical port number to a (switch, physical port)
 *                  tuple.
 *                                                                      \lb\lb
 *                  Logical port numbers are used by system end users to
 *                  identify ports in the system CLI or other user interface
 *                  and typically match labelling on the system chassis.
 *                  Logical port numbers are system-global for systems that
 *                  consist of more than one switch device.
 *                                                                      \lb\lb
 *                  The (switch, physical port) tuple identifies the physical
 *                  port number of a particular switch device, as
 *                  identified on the pin-out of the device.
 *
 * \param[in]       logicalSwitch is the logical switch number.
 *
 * \param[in]       logicalPort is the logical port number.
 *
 * \param[out]      switchNum points to storage where the switch number of the
 *                  (switch, physical port) tuple should be stored.
 *
 * \param[out]      physPort points to storage where the physical port number
 *                  of the (switch, physical port) tuple should be stored.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if logicalSwitch or logicalPort are 
 *                  not valid values.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformMapLogicalPortToPhysical(fm_int  logicalSwitch,
                                             fm_int  logicalPort,
                                             fm_int *switchNum,
                                             fm_int *physPort)
{
    if ( (logicalPort < 0) || (logicalPort > fmMaxPlatformLogicalPort) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    *switchNum = logicalSwitch;
    *physPort  = fmPlatformPortRemapTable[logicalPort];

    return ( (*physPort >= 0) ? FM_OK : FM_ERR_INVALID_PORT );


}   /* end fmPlatformMapLogicalPortToPhysical */




/*****************************************************************************/
/** fmPlatformMapPhysicalPortToLogical
 * \ingroup platform
 *
 * \desc            Maps a (switch, physical port) tuple to a logical port
 *                  number.
 *                                                                      \lb\lb
 *                  Logical port numbers are used by system end users to
 *                  identify ports in the system CLI or other user interface
 *                  and typically match labelling on the system chassis.
 *                  Logical port numbers are system-global for systems that
 *                  consist of more than one switch device.
 *                                                                      \lb\lb
 *                  The (switch, physical port) tuple identifies the physical
 *                  port number of a particular switch device, as
 *                  identified on the pin-out of the device.
 *
 * \param[in]       switchNum is the switch number of the (switch, physical
 *                  port) tuple.
 *
 * \param[in]       physPort is the physical port number of the (switch,
 *                  physical port) tuple.
 *
 * \param[out]      logicalSwitch points to storage where the corresponding
 *                  logical switch number should be stored.
 *
 * \param[out]      logicalPort points to storage where the corresponding
 *                  logical port number should be stored.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if either switchNum or physPort are
 *                  not valid values.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformMapPhysicalPortToLogical(fm_int  switchNum,
                                             fm_int  physPort,
                                             fm_int *logicalSwitch,
                                             fm_int *logicalPort)
{
    fm_platform_state *ps;

    if ( (switchNum < 0) || (switchNum >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[switchNum];

    if ( (physPort < 0) || (physPort >= ps->maxPorts) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    *logicalSwitch = switchNum;
    *logicalPort = fmPhysicalPortRemapTable[physPort];

    return ( (*logicalPort >= 0) ? FM_OK : FM_ERR_INVALID_PORT );


}   /* end fmPlatformMapPhysicalPortToLogical */




/*****************************************************************************/
/** fmPlatformMapCardinalToPhysical
 * \ingroup intPlatform
 *
 * \desc            Maps a (switch, cpi) tuple to a physical port number.
 *
 * \note            Currently defined for the white model only. Supports
 *                  testing of sparse logical port configurations.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       cpi is the cardinal port index.
 *
 * \param[out]      physPort is a pointer to caller-provided memory in which
 *                  this function should store the physical port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if either sw or cpi is invalid.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformMapCardinalToPhysical(fm_int    sw,
                                          fm_int    cpi,
                                          fm_int *  physPort)
{
    fm_switch * switchPtr;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    return fmMapCardinalPortInternal(switchPtr, cpi, NULL, physPort);


}   /* end fmPlatformMapCardinalToPhysical */




/*****************************************************************************/
/** fmPlatformMapPhysicalToCardinal
 * \ingroup intPlatform
 *
 * \desc            Maps a (switch, physical port) tuple to a cardinal
 *                  port index.
 *
 * \note            Currently defined for the white model only. Supports
 *                  testing of sparse logical port configurations.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       physPort is the physical port number.
 *
 * \param[out]      cpi is a pointer to caller-provided memory in which
 *                  this function should store the cardinal port index.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if either sw or physPort is
 *                  not valid.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformMapPhysicalToCardinal(fm_int    sw,
                                          fm_int    physPort,
                                          fm_int *  cpi)
{
    fm_switch *switchPtr;
    fm_int     logPort;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    if ( (physPort < 0) || (physPort > switchPtr->maxPhysicalPort) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    logPort = fmPhysicalPortRemapTable[physPort];

    if (logPort < 0 || logPort > switchPtr->cardinalPortInfo.maxLogicalPort)
    {
        return FM_ERR_INVALID_PORT;
    }

    if (cpi == NULL || switchPtr->cardinalPortInfo.indexTable == NULL)
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    *cpi = switchPtr->cardinalPortInfo.indexTable[logPort];

    return FM_OK;

}   /* end fmPlatformMapPhysicalToCardinal */




/*****************************************************************************/
/** fmPlatformReadCSR
 * \ingroup intPlatform
 *
 * \desc            Read a CSR register.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read
 *
 * \param[out]      value points to storage where this function will place
 *                  the read register value.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value)
{
    fm_platform_state *ps;
    fm_status          err;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    err = ps->ModelReadCSR(sw, addr, value);

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformReadCSR */




/*****************************************************************************/
/** fmPlatformWriteCSR
 * \ingroup intPlatform
 *
 * \desc            Write a CSR register.
 *                                                                      \lb\lb
 *                  The register access table is first scanned to see if there
 *                  are any fields in the register word that are not RW.
 *                  If there are, then special processing is done to manage
 *                  the non-RW bit fields.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[in]       newValue is the data value to write to the register.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformWriteCSR(fm_int sw, fm_uint32 addr, fm_uint32 newValue)
{
    fm_platform_state *ps;
    fm_status          err;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    INSTRUMENT_REG_WRITE(sw, addr, newValue);
    err = ps->ModelWriteCSR(sw, addr, newValue);

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformWriteCSR */




/*****************************************************************************/
/** fmPlatformMaskCSR
 * \ingroup platform
 *
 * \desc            Mask on or off the bits in a single 32-bit register.
 *
 * \note            This function is not called by the API directly, but by
 *                  platform layer code that is commonly available to all
 *                  platforms.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       reg is the word offset into the switch's register file.
 *
 * \param[in]       mask is the bit mask to turn on or off.
 *
 * \param[in]       on should be TRUE to set the masked bits in the register
 *                   or FALSE to clear the masked bits in the register.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_BAD_IOCTL if reg is invalid.
 *
 *****************************************************************************/
fm_status fmPlatformMaskCSR(fm_int sw, fm_uint reg, fm_uint32 mask, fm_bool on)
{
    fm_status err = FM_OK;
    fm_uint32 value;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    err = fmPlatformReadCSR(sw, reg, &value);

    if (err == FM_OK)
    {
        value = on ? (value | mask) : (value & ~mask);

        err = fmPlatformWriteCSR(sw, reg, value);
    }

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformMaskCSR */




/*****************************************************************************/
/** fmPlatformReadCSRMult
 * \ingroup intPlatform
 *
 * \desc            Read multiple CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an array to be filled in with
 *                  the register data read. The array must be n elements in
 *                  length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformReadCSRMult(fm_int     sw,
                                fm_uint32  addr,
                                fm_int     n,
                                fm_uint32 *value)
{
    fm_platform_state *ps;
    fm_status          err;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    err = ps->ModelReadCSRMult(sw, addr, n, value);

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformReadCSRMult */




/*****************************************************************************/
/** fmPlatformWriteCSRMult
 * \ingroup intPlatform
 *
 * \desc            Write multiple CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of consecutive register addresses
 *                  to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of values to be written. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformWriteCSRMult(fm_int     sw,
                                 fm_uint32  addr,
                                 fm_int     n,
                                 fm_uint32 *value)
{
    fm_platform_state *ps;
    fm_status          err = FM_OK;
    fm_int             i;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    for (i = 0 ; i < n ; i++)
    {
        INSTRUMENT_REG_WRITE(sw, addr + i, value[i]);
        err = ps->ModelWriteCSR(sw, addr + i, value[i]);

        if (err != FM_OK)
        {
            break;
        }
    }

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformWriteCSRMult */




/*****************************************************************************/
/** fmPlatformReadCSR64
 * \ingroup intPlatform
 *
 * \desc            Read a 64-bit CSR register.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to read.
 *
 * \param[out]      value points to storage where the 64-bit read data value
 *                  will be stored by this function.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformReadCSR64(fm_int sw, fm_uint32 addr, fm_uint64 *value)
{
    fm_platform_state *ps;
    fm_status          err;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    err = ps->ModelReadCSR64(sw, addr, value);

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformReadCSR64 */




/*****************************************************************************/
/** fmPlatformWriteCSR64
 * \ingroup intPlatform
 *
 * \desc            Writes a 64-bit CSR register.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       addr contains the CSR register address to write.
 *
 * \param[in]       value is the 64-bit data value to write.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformWriteCSR64(fm_int sw, fm_uint32 addr, fm_uint64 value)
{
    fm_platform_state *ps;
    fm_status          err = FM_OK;
    fm_uint32          hi  = (value >> 32);
    fm_uint32          lo  = (value & FM_LITERAL_64(0xffffffff));

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    INSTRUMENT_REG_WRITE(sw, addr + 0, lo);
    err = ps->ModelWriteCSR(sw, addr + 0, lo);

    if (err == FM_OK)
    {
        INSTRUMENT_REG_WRITE(sw, addr + 4, hi);
        err = ps->ModelWriteCSR(sw, addr + 4, hi);
    }

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformWriteCSR64 */




/*****************************************************************************/
/** fmPlatformReadCSRMult64
 * \ingroup intPlatform
 *
 * \desc            Read multiple 64-bit CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of register addresses to read.
 *
 * \param[in]       addr contains the starting CSR register address to read.
 *
 * \param[out]      value points to an array of to be filled in with
 *                  the 64-bit register data read. The array must be n elements
 *                  in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformReadCSRMult64(fm_int     sw,
                                  fm_uint32  addr,
                                  fm_int     n,
                                  fm_uint64 *value)
{
    fm_platform_state *ps;
    fm_status          err;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    err = ps->ModelReadCSRMult64(sw, addr, n, value);

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformReadCSRMult64 */




/*****************************************************************************/
/** fmPlatformWriteCSRMult64
 * \ingroup intPlatform
 *
 * \desc            Write multiple 64-bit CSR registers.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       n contains the number of register addresses to write.
 *
 * \param[in]       addr contains the starting CSR register address to write.
 *
 * \param[in]       value points to an array of 64-bit values to write. The
 *                  array must be n elements in length.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformWriteCSRMult64(fm_int     sw,
                                   fm_uint32  addr,
                                   fm_int     n,
                                   fm_uint64 *value)
{
    fm_platform_state *ps;
    fm_status          err = FM_OK;
    fm_int             i;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    TAKE_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    for (i = 0 ; i < n ; i++)
    {
        INSTRUMENT_REG_WRITE(sw, addr + i*4, value[i]);
        err = ps->ModelWriteCSR(sw, addr + i*4, value[i]);

        if (err != FM_OK)
        {
            break;
        }
    }

    DROP_PLAT_LOCK(sw, FM_MEM_TYPE_CSR);

    return err;

}   /* end fmPlatformWriteCSRMult64 */




/*****************************************************************************/
/** fmPlatformGetPktMemoryBase
 * \ingroup intPlatform
 *
 * \desc            Get the base address of the packet buffer memory pool.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      baseAddr points to a pointer that this function will
 *                  fill in with the base address of packet memory.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformGetPktMemoryBase(int sw, fm_uint32 **baseAddr)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(baseAddr);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformGetPktMemoryBase */




/*****************************************************************************/
/** fmPlatformReset
 * \ingroup platform
 *
 * \desc            Assert the reset signal into the switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformReset(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_OK;

}   /* end fmPlatformReset */




/*****************************************************************************/
/** fmPlatformRelease
 * \ingroup platform
 *
 * \desc            Deassert the reset signal into the switch.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformRelease(fm_int sw)
{
    fm_platform_state *ps;
    fm_status          err;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_ARGUMENT;
    }

    ps        = &fmRootPlatform->fmPlatformState[sw];

    err = ps->ModelReset(sw);

    return err;

}   /* end fmPlatformRelease */




/*****************************************************************************/
/** fmPlatformEnableInterrupt
 * \ingroup platform
 *
 * \desc            Enable the interrupt signal from the switch device.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       intrTypes is a mask of the different interrupt types to
 *                  enable.  Only FM_INTERRUPT_SOURCE_ISR is supported in
 *                  this platform.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformEnableInterrupt(fm_int sw, fm_uint intrTypes)
{
    fm_platform_state *ps;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_INTR,
                 "sw=%d intrTypes=%u\n",
                 sw,
                 intrTypes);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_INTR, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    if (intrTypes & FM_INTERRUPT_SOURCE_ISR)
    {
        TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);
        ps->swInterruptEnable = TRUE;
        DROP_PLAT_LOCK(sw, FM_PLAT_INFO);
    }

    FM_LOG_EXIT(FM_LOG_CAT_EVENT_INTR, FM_OK);

}   /* end fmPlatformEnableInterrupt */




/*****************************************************************************/
/** fmPlatformDisableInterrupt
 * \ingroup intPlatform
 *
 * \desc            Disable the interrupt signal from the switch device.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       intrTypes is a mask of the different interrupt types to
 *                  enable.  Only FM_INTERRUPT_SOURCE_ISR is supported in
 *                  this platform.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformDisableInterrupt(fm_int sw, fm_uint intrTypes)
{
    fm_platform_state *ps;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, intrTypes = %u\n",
                 sw,
                 intrTypes);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_EVENT_INTR, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    if (intrTypes & FM_INTERRUPT_SOURCE_ISR)
    {
        TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);
        ps->swInterruptEnable = FALSE;
        DROP_PLAT_LOCK(sw, FM_PLAT_INFO);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmPlatformDisableInterrupt */




/*****************************************************************************/
/** fmPlatformTriggerInterrupt
 * \ingroup intPlatform
 *
 * \desc            Artificially signals the interrupt semaphore.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       intrTypes is a mask of the different interrupt types to
 *                  trigger.  Only FM_INTERRUPT_SOURCE_API is supported in
 *                  this platform.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformTriggerInterrupt(fm_int sw, fm_uint intrTypes)
{
    fm_platform_state *ps = &fmRootPlatform->fmPlatformState[sw];

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw = %d, intrTypes = %u\n",
                 sw,
                 intrTypes);

    if (intrTypes & FM_INTERRUPT_SOURCE_API)
    {
        TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);

        ps->intrSource |= FM_INTERRUPT_SOURCE_API;

        DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT,
                     "fmPlatformTriggerInterrupt: signaling semaphore\n");

        fmSignalSemaphore(&fmRootApi->intrAvail);
    }

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmPlatformTriggerInterrupt */




/*****************************************************************************/
/** fmPlatformGetInterrupt
 * \ingroup platform
 *
 * \desc            Called by the interrupt handler to identify the source of
 *                  an interrupt and dispatch the appropriate handler.
 *                                                                      \lb\lb
 *                  Typically, this function is responsible for taking the
 *                  following steps:
 *                                                                      \lb\lb
 *                  - Identify the source of the interrupt as being from a
 *                  switch device, a PHY or some other device not managed
 *                  by the API.
 *                                                                      \lb\lb
 *                  - If the source is a switch device, disable further
 *                  interrupts and dispatch the API's switch interrupt handler.
 *                  If a PHY, disable further interrupts and dispatch the
 *                  PHY interrupt handler (which may be platform-specific). If
 *                  the interrupt is from a device not managed by the
 *                  API, the appropriate platform-specific action should be
 *                  taken.
 *
 * \param[in]       sw is the switch number to get the interrupt state for
 *
 * \param[in]       intrType should contain only one bit, indicating the
 *                  interrupt to block on.
 *
 * \param[out]      intrSrc is the resultant source
 *                  trigger.  On the FM85XXEP, only FM_INTERRUPT_SOURCE_ISR is
 *                  supported.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformGetInterrupt(fm_int sw, fm_uint intrType, fm_uint *intrSrc)
{
    fm_platform_state *ps;
    fm_status          err = FM_OK;

    FM_NOT_USED(intrType);

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_PLATFORM,
                         "sw = %d, intrTypes = %u, intrSrc = %p\n",
                         sw,
                         intrType,
                         (void *) intrSrc);

    if (fmRootPlatform == NULL)
    {
        *intrSrc = FM_INTERRUPT_SOURCE_NONE;
        goto ABORT;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    /*  Check for pending interrupt source before taking platform lock.
     *  If an interrupt is pending, we will have to take the lock before
     *  we can clear it, but this will save a lot of lock take/drop
     *  operations and is safe to do.
     */
    if (ps->intrSource == FM_INTERRUPT_SOURCE_NONE)
    {
        *intrSrc = FM_INTERRUPT_SOURCE_NONE;
        goto ABORT;
    }

    TAKE_PLAT_LOCK(sw, FM_PLAT_INFO);

    /* grab source and clear */
    *intrSrc = ps->intrSource;

    ps->intrSource = FM_INTERRUPT_SOURCE_NONE;

    DROP_PLAT_LOCK(sw, FM_PLAT_INFO);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_PLATFORM,
                           (*intrSrc != FM_INTERRUPT_SOURCE_NONE),
                           err = FM_FAIL,
                           "ASSERTION FAILURE: "
                           "no interrupt source but semaphore signaled!\n");

    FM_LOG_DEBUG(FM_LOG_CAT_EVENT,
                 "fmPlatformGetInterrupt: interrupt with source 0x%x\n",
                 *intrSrc);

ABORT:
    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformGetInterrupt */




/*****************************************************************************/
/** fmPlatformSwitchInitialize
 * \ingroup platform
 *
 * \desc            Called by the API in response to a
 *                  ''FM_EVENT_SWITCH_INSERTED''. This function performs
 *                  platform specific initializations in the switch state table,
 *                  particularly to initialize the function pointers for
 *                  accessing the hardware device registers. See the source
 *                  code for this function as implemented for the fm85xxep
 *                  platform for the list of function pointers needing
 *                  initialization and for examples of the functions to which
 *                  they should point.
 *                                                                      \lb\lb
 *                  If desired, this function can override device model
 *                  detection, if for example, an FM2224 should be treated as
 *                  as an FM2112.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSwitchInitialize(fm_int sw)
{
    fm_switch *     switchPtr;
    fm_status       err = FM_OK;
#ifdef FM_SUPPORT_FM6000
    fm6000_switch * switchExt6k;
#endif

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw = %d\n", sw);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    /* Initialize function pointers */

    switch (fmRootPlatform->fmPlatformState[sw].family)
    {
#ifdef FM_SUPPORT_FM6000
        case FM_SWITCH_FAMILY_FM6000:
            switchExt6k = GET_SWITCH_EXT(sw);

            if (switchExt6k != NULL)
            {
                memcpy( &switchExt6k->ucFuncs,
                        fmRootPlatform->fmPlatformState[sw].ucFuncs,
                        sizeof(fm6000_ucFuncs) );
            }

            /* Fall through to initialize the management function pointers. */
#endif

#ifdef FM_SUPPORT_FM4000
        case FM_SWITCH_FAMILY_FM4000:
#endif
#ifdef FM_SUPPORT_FM10000
        case FM_SWITCH_FAMILY_FM10000:
#endif
#ifdef FM_SUPPORT_HLP
        case FM_SWITCH_FAMILY_HLP:
#endif  
#ifdef FM_SUPPORT_CPK
        case FM_SWITCH_FAMILY_CPK:
#endif           
            switchPtr->WriteUINT32          = fmPlatformWriteCSR;
            switchPtr->ReadUINT32           = fmPlatformReadCSR;
            switchPtr->MaskUINT32           = fmPlatformMaskCSR;
            switchPtr->WriteUINT32Mult      = fmPlatformWriteCSRMult;
            switchPtr->ReadUINT32Mult       = fmPlatformReadCSRMult;
            switchPtr->WriteUINT64          = fmPlatformWriteCSR64;
            switchPtr->ReadUINT64           = fmPlatformReadCSR64;
            switchPtr->WriteUINT64Mult      = fmPlatformWriteCSRMult64;
            switchPtr->ReadUINT64Mult       = fmPlatformReadCSRMult64;
            break;

        default:
            FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_UNSUPPORTED);

    }   /* end switch (fmPlatformState[sw].family) */

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformSwitchInitialize */




/*****************************************************************************/
/** fmPlatformSwitchTerminate
 * \ingroup platform
 *
 * \desc            Called after shutting down a switch device. This
 *                  function is responsible for any platform-specific actions
 *                  that must be taken when a switch is removed from the
 *                  system, for example, turning off power, asserting the reset
 *                  signal, etc. This function is called when all common API
 *                  resouces are freed.
 *
 * \param[in]       sw is the switch that has been shut down.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSwitchTerminate(fm_int sw)
{
    FM_NOT_USED(sw);

#if INSTRUMENT_LOG_LEVEL > 0
    fmPlatformCloseInstrumentation();
#endif

    return FM_OK;

}   /* end fmPlatformSwitchTerminate */




/*****************************************************************************/
/** fmPlatformSetPortInterruptMask
 * \ingroup intPlatform
 *
 * \desc            Called by the other platform code to restrict what ports
 *                  the ISR task will react to.
 *                                                                      \lb\lb
 *                  Typically, this function would be used in a PHY
 *                  initialization routine to prevent the port interrupt
 *                  from confusing the system.
 *
 * \param[in]       sw is the switch this port belongs to
 *
 * \param[in]       port is the logical port to mask
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSetPortInterruptMask(fm_int sw, fm_int port)
{
    fm_platform_state *ps;
    fm_status          err;
    fm_int             physPort;
    fm_int             switchNum;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d port=%d\n", sw, port);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    err = fmPlatformMapLogicalPortToPhysical(sw, port, &switchNum, &physPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    ps->portInterruptMask |= (1 << physPort);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformSetPortInterruptMask */




/*****************************************************************************/
/** fmPlatformClearPortInterruptMask
 * \ingroup intPlatform
 *
 * \desc            Called by the other platform code to activate what ports
 *                  the ISR task will react to.
 *
 * \param[in]       sw is the switch this port belongs to
 *
 * \param[in]       port is the logical port to unmask
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformClearPortInterruptMask(fm_int sw, fm_int port)
{
    fm_platform_state *ps;
    fm_status          err;
    fm_int             physPort;
    fm_int             switchNum;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM, "sw=%d port=%d\n", sw, port);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    err = fmPlatformMapLogicalPortToPhysical(sw, port, &switchNum, &physPort);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_PLATFORM, err);

    ps->portInterruptMask &= ~(1 << physPort);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformClearPortInterruptMask */




/*****************************************************************************/
/** fmPlatformGetPortInterruptMask
 * \ingroup platform
 *
 * \desc            Provide a port bit mask identifying which ports have
 *                  interrupts masked.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      portMask is the destination to store the port bit mask.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformGetPortInterruptMask(fm_int sw, fm_uint32 *portMask)
{
    fm_platform_state *ps;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d portMask=%p\n",
                 sw,
                 (void *) portMask);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    *portMask = ps->portInterruptMask;

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);

}   /* end fmPlatformGetPortInterruptMask */




/*****************************************************************************/
/** fmPlatformSendPackets
 * \ingroup platform
 *
 * \desc            Called to trigger the sending (or attempt thereof) of the
 *                  current packet queue.
 *
 * \param[in]       sw is the switch on which to trigger the sending of packets.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSendPackets(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformSendPackets */




/*****************************************************************************/
/** fmPlatformSendPacket
 * \ingroup platform
 *
 * \desc            Called to add the given packet to the internal packet
 *                  queue.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       info is a pointer to associated information about
 *                  the packet including where it is going.
 *
 * \param[in]       packet is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSendPacket(fm_int         sw,
                               fm_packetInfo *info,
                               fm_buffer *    packet)
{
    fm_int port;
    fm_packetInfoV2 infoV2;

    FM_CLEAR(infoV2);

    /**************************************************
     * ---WARNING---: The packetInfo has numerous
     * complicated options, I have selected to only
     * support switched and directed.
     **************************************************/

    if (info->logicalPort == FM_DIRECT_VLAN_SEND)
    {
        /* This mode is not supported */

        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "FM_DIRECT_VLAN_SEND not supported\n");

    }
    else if (info->useEgressRules)
        {
        /* This mode is not supported */

        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "useEgressRules not supported\n");

    }
    else if (info->sourcePort != 0)
                {
        /* This mode is not supported */

        FM_LOG_ERROR(FM_LOG_CAT_PLATFORM,
                     "sourcePort not supported\n");

    }
    else if (info->directSendToCpu != 0)
    {
        port = 0;
        infoV2.switchPriority = info->switchPriority;
        return fmPlatformSendPacketDirected(sw, &port, 1, packet, &infoV2);
    }
    else if (info->logicalPort != 0)
                {
        port = info->logicalPort;
        infoV2.switchPriority = info->switchPriority;
        return fmPlatformSendPacketDirected(sw, &port, 1, packet, &infoV2);
                }
                else
                {
        return fmPlatformSendPacketSwitched(sw, packet);
                }

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformSendPacket */




/*****************************************************************************/
/** fmPlatformSendPacketDirected
 * \ingroup platform
 *
 * \desc            Called to add the given packet to the internal packet
 *                  queue in the directed packet send mode.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       portList points to an array of logical port numbers the
 *                  switch is to send the packet.
 *
 * \param[in]       numPorts is the number of elements in portList.
 *
 * \param[in]       buffer is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \param[in]       info is a pointer to the packet information structure which
 *                  contains some relevant information describing the packet.
 *                  See 'fm_packetInfoV2' for more information.
 *                  Note: the structure pointed to by info must have all fields
 *                  initialized. Any unused field should be set to zero.
 *                  Failure to initialize all fields can result in the packet
 *                  being mishandled.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSendPacketDirected(fm_int           sw,
                                       fm_int *         portList,
                                       fm_int           numPorts,
                                       fm_buffer *      buffer,
                                       fm_packetInfoV2 *info)
{
    if (strlen(fmGetTextApiAttribute(
            FM_AAK_API_PLATFORM_MODEL_SERVER,
            FM_AAD_API_PLATFORM_MODEL_SERVER)))
    {
        return FM_ERR_UNSUPPORTED;
    }

    return fmModelPacketQueueSendDirected(sw, portList, numPorts, buffer, info);

}   /* end fmPlatformSendPacketDirected */




/*****************************************************************************/
/** fmPlatformSendPacketSwitched
 * \ingroup platform
 *
 * \desc            Called to add the given packet to the internal packet
 *                  queue in the lookup packet send mode.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       buffer is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSendPacketSwitched(fm_int sw, fm_buffer *buffer)
{
    return fmModelPacketQueueSendSwitched(sw, buffer);

}   /* end fmPlatformSendPacketSwitched */




/*****************************************************************************/
/* fmPlatformSendPacketISL
 * \ingroup platform
 *
 * \desc            Called to add the given packet to the internal packet
 *                  queue in the lookup packet send mode.
 *
 * \param[in]       sw is the switch on which to send the packet.
 *
 * \param[in]       islTag points to the ISL tag contents
 *
 * \param[in]       islTagFormat is the ISL tag format value.
 *
 * \param[in]       buffer is a pointer to a chain of fm_buffer structures
 *                  containing the payload.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of
 *                  failure.
 *
 *****************************************************************************/
fm_status fmPlatformSendPacketISL(fm_int          sw,
                                  fm_uint32 *     islTag,
                                  fm_islTagFormat islTagFormat,
                                  fm_buffer *     buffer)
{
    return fmModelPacketQueueSendISL(sw, islTagFormat, islTag, buffer);

}   /* end fmPlatformSendPacketISL */




/*****************************************************************************/
/** fmPlatformReceivePackets
 * \ingroup platform
 *
 * \desc            Called to try and receive packets.  Responsible for
 *                  generating events for packets received.
 *
 * \param[in]       sw is the switch on which to receive packets.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformReceivePackets(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformReceivePackets */




/*****************************************************************************/
/* fmPlatformReceiveProcess
 * \ingroup intPlatform
 *
 * \desc            Processes the received packet and enqueues it to a higher
 *                  layer.
 *                  Select the proper switch specific function.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       buffer points to the receive packet buffer. If an ISL tag
 *                  is present in the data buffer, the ISL tag should be in
 *                  network-byte order.
 *
 * \param[in]       pIslTag points to the host-byte order ISL tag if an ISL tag
 *                  is not present in the data buffer. Set to NULL if an ISL
 *                  tag is present in the data buffer. If an ISL tag is not
 *                  present in the data buffer, the data buffer must have
 *                  enough space for a VLAN tag to be inserted.
 *
 * \param[in]       flags to pass to the function, if any.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformReceiveProcess(fm_int     sw,
                                   fm_buffer *buffer,
                                   fm_uint32 *pIslTag,
                                   fm_uint    flags)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(buffer);
    FM_NOT_USED(pIslTag);
    FM_NOT_USED(flags);

    return FM_ERR_UNSUPPORTED;

}   /* end fmPlatformReceiveProcess */




/*****************************************************************************/
/** fmPlatformShowPortState
 * \ingroup platform
 *
 * \desc            Show the port status for this platform. This function is
 *                  called by the API whenever the port state changes and could
 *                  be used by the platform to implement some actions depending
 *                  on port state. An example is changing the color of an LED
 *                  for that port.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       logPort is the logical port number.
 *
 * \param[in]       state indicates the port's state (see 'Port States') to
 *                  show.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if logPort is invalid.
 *
 *****************************************************************************/
fm_status fmPlatformShowPortState(fm_int sw,
                                  fm_int logPort,
                                    fm_int state)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(logPort);
    FM_NOT_USED(state);

    return FM_OK;

}   /* end fmPlatformShowPortState */




/*****************************************************************************/
/** fmPlatformSetAttribute
 * \ingroup platformApp
 *
 * \desc            Sets a platform attribute.
 *
 * \param[in]       sw is the switch on which to operate.  Not all
 *                  attributes will use this argument. See the attribute's
 *                  documentation for specific use.
 *
 * \param[in]       index refers to the logical entiry on which to
 *                  operate.  Not all attributes will use this argument.
 *                  See the attribute's documentation for specific use.
 *                  If the attribute is a port attribute, then the index
 *                  is the logical port.
 *
 * \param[in]       attr is the attribute constant.
 *
 * \param[in]       value points to the specific value type.  See the
 *                  attribute documentation for what to use.
 *
 * \return          FM_OK on success.
 * \return          FM_ERR_INVALID_ARGUMENT on an invalid attribute.
 *
 *****************************************************************************/
fm_status fmPlatformSetAttribute(fm_int    sw,
                                 fm_int    index,
                                 fm_uint32 attr,
                                 void *    value)
{
    fm_status err = FM_OK;

    FM_NOT_USED(sw);
    FM_NOT_USED(index);

    switch (attr)
    {
        case FM_PLATFORM_ATTR_EGRESS_LIMIT:
            err = fmModelPacketQueueSetAttribute(FM_MODEL_PACKET_QUEUE_EGRESS_LIMIT,
                                                 value);
            break;

        case FM_PLATFORM_ATTR_TOPOLOGY:
            err = fmModelPacketQueueSetAttribute(FM_MODEL_PACKET_QUEUE_TOPOLOGY,
                                                 value);
            break;

        default:
            err = FM_ERR_INVALID_ATTRIB;
            break;
    }


    return err;

}   /* end fmPlatformSetAttribute */




/*****************************************************************************/
/** fmPlatformGetAttribute
 * \ingroup platformApp
 *
 * \desc            Gets a platform attribute.
 *
 * \param[in]       sw is the switch on which to operate.  Not all
 *                  attributes will use this argument. See the attribute's
 *                  documentation for specific use.
 *
 * \param[in]       index refers to the logical entity on which to
 *                  operate.  Not all attributes will use this argument.
 *                  See the attribute's documentation for specific use.
 *                  If the attribute is a port attribute, then the index
 *                  is the logical port.
 *
 * \param[in]       attr is the attribute constant.
 *
 * \param[in]       value points to caller allocated storage where the value
 *                  will be written.  See the attribute documentation for
 *                  what to use.
 *
 * \return          FM_OK on success.
 * \return          FM_ERR_INVALID_ARGUMENT on an invalid attribute.
 *
 *****************************************************************************/
fm_status fmPlatformGetAttribute(fm_int    sw,
                                 fm_int    index,
                                 fm_uint32 attr,
                                 void *    value)
{
    fm_char * name;
    fm_status err = FM_OK;
    fm_platform_state *ps;

    FM_LOG_ENTRY(FM_LOG_CAT_PLATFORM,
                 "sw=%d index=%d attribute=%d data=%p\n",
                 sw,
                 index,
                 attr,
                 (void *) value);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_ERR_INVALID_ARGUMENT);
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    switch (attr)
    {
        case FM_PLATFORM_ATTR_NAME:
            /* Note: the returned name string must be no longer than 32
             * characters including the NULL terminator. */
            name = (fm_char *) value;
            switch (ps->switchType)
            {
#ifdef FM_SUPPORT_FM4000
                case FM_PLATFORM_SWITCH_TYPE_FM4000:
                    fmStringCopy(name, "FM4000-Model", 12);
                    break;
#endif

#ifdef FM_SUPPORT_FM6000
                case FM_PLATFORM_SWITCH_TYPE_FM6000:
                    fmStringCopy(name, "FM6000-Model", 12);
                    break;
#endif

#ifdef FM_SUPPORT_FM10000
                case FM_PLATFORM_SWITCH_TYPE_FM10000:
                    fmStringCopy(name, "FM10000-Model", 12);
                    break;
#endif
#ifdef FM_SUPPORT_HLP
                case FM_PLATFORM_SWITCH_TYPE_HLP:
                    fmStringCopy(name, "HLP-Model", 12);
                    break;
#endif
#ifdef FM_SUPPORT_CPK
                case FM_PLATFORM_SWITCH_TYPE_CPK:
                    fmStringCopy(name, "CPK-Model", 12);
                    break;
#endif
                default:
                    err = FM_ERR_UNSUPPORTED;
            }
            break;

        default:
            err = FM_ERR_INVALID_ATTRIB;

    }   /* end switch */

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, err);

}   /* end fmPlatformGetAttribute */




/*****************************************************************************/
/** fmPlatformGetPortClockSel
 * \ingroup platform
 *
 * \desc            Returns which clock to use (0=REFCLKA, 1=REFCLKB) for
 *                  the physical port.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       physPort is the physical port number.
 *
 * \param[in]       speed of the port in Mbps.
 *
 * \param[out]      clockSel points to a fm_int which will be set to 0 or 1
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformGetPortClockSel(fm_int  sw,
                                    fm_int  physPort,
                                    fm_int  speed,
                                    fm_int *clockSel)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(physPort);

    *clockSel = (speed < 2500) ? 0 : 1;

    return FM_OK;

}   /* end fmPlatformGetPortClockSel */




/*****************************************************************************/
/* fmPlatformCreateSWAG
 * \ingroup platform
 *
 * \desc            Requests the platform to assign a new switch aggregate
 *                  switch number.
 *
 * \param[out]      sw points to caller-allocated storage for the switch
 *                  number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if sw is NULL.
 * \return          FM_ERR_INVALID_SWITCH if the SWAG cannot be created.
 *
 *****************************************************************************/
fm_status fmPlatformCreateSWAG(fm_int *sw)
{
    FM_NOT_USED(sw);

    return FM_OK;

}   /* end fmPlatformCreateSWAG */




/*****************************************************************************/
/* fmPlatformDeleteSWAG
 * \ingroup platform
 *
 * \desc            Instructs the platform to release a switch aggregate
 *                  switch number.
 *
 * \param[out]      sw is the switch number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if the switch aggregate number
 *                  is invalid.
 *
 *****************************************************************************/
fm_status fmPlatformDeleteSWAG(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_OK;

}   /* end fmPlatformDeleteSWAG */




#ifdef FM_SUPPORT_SWAG
/*****************************************************************************/
/* fmPlatformSWAGInitialize
 * \ingroup platform
 *
 * \desc            Called by the API during switch inserted event processing
 *                  if the switch is a SWAG.  This function performs platform-
 *                  specific initialization for the SWAG.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformSWAGInitialize(fm_int sw)
{
    FM_NOT_USED(sw);

    return FM_OK;

}   /* end fmPlatformSWAGInitialize */
#endif




/*****************************************************************************/
/** fmPlatformMACMaintenanceSupported
 * \ingroup platform
 *
 * \desc            Indicates if MAC table maintenance should be performed
 *                  by the API. There are two aspects to MAC table maintenance.
 *                                                                      \lb\lb
 *                  The first aspect is whether the MAC table maintenance
 *                  thread in the API should process the MAC table on a
 *                  periodic basis or be strictly event driven. FM2000,
 *                  FM3000 and FM4000 devices generally require periodic
 *                  processing while FM4000 devices can be strictly event
 *                  driven. The API will call this function at initialization
 *                  time with the sw argument set to -1 to determine if the
 *                  thread should run periodically.
 *                                                                      \lb\lb
 *                  Return TRUE to enable periodic processing.              \lb
 *                  Return FALSE to disable periodic processing.
 *                                                                      \lb\lb
 *                  The second aspect is whether an individual switch
 *                  should be processed at all. The API will call
 *                  this function each time the maintenance thread wakes up
 *                  (whether because of an event or a timeout) for each switch
 *                  in the platform, with the sw argument set to the switch
 *                  number. Generally, this function should return TRUE for
 *                  all switches. The function might return FALSE in the case
 *                  of a switch that has learning disabled. A classic example
 *                  is a spine switch in a fat-tree architecture that is
 *                  controlled via FIBM. Spine switches may be configured to
 *                  do no learning and the FIBM traffic that would be
 *                  generated by the maintenance thread could be a significant
 *                  waste of bandwidth.
 *
 * \param[in]       sw will be -1 if the API is asking whether periodic MAC
 *                  Table maintenance is needed by any switch in the platform.
 *                  Otherwise, sw is a specific switch number, in which case
 *                  the API is asking whether this switch's MAC Table should
 *                  be processed at all.
 *
 * \return          TRUE if sw is -1 and periodic maintenance is required by at
 *                  least one switch in the platform, or if sw is positive and
 *                  that switch number should have its MAC table processed.
 * \return          FALSE if sw is -1 and periodic maintenance is not required
 *                  by any switches in the platform, or if sw is positive and
 *                  that switch number should not have its MAC table processed.
 *
 *****************************************************************************/
fm_bool fmPlatformMACMaintenanceSupported(fm_int sw)
{
    if (sw == -1)
    {
        return FALSE;
    }

    return TRUE;

}   /* end fmPlatformMACMaintenanceSupported */




/*****************************************************************************/
/** fmPlatformI2cRead
 * \ingroup platformApp
 *
 * \desc            Read from an I2C device.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       bus is the I2C bus number.
 *
 * \param[in]       address is the I2C device address (0x00 - 0x7F).
 *
 * \param[out]      data points to caller allocate area to store the data
 *                  read.
 *
 * \param[in]       length is the number of data bytes to read (1..4)
 *
 * \return          The number of bytes actually read. This value can be
 *                  smaller than the specified length if the device limits
 *                  the number of bytes read. A value of 0 means no device
 *                  was detected at specified device address.
 *
 *****************************************************************************/
fm_int fmPlatformI2cRead(fm_int     sw,
                         fm_int     bus,
                         fm_int     address,
                         fm_uint32 *data,
                         fm_int     length)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(bus);
    FM_NOT_USED(address);
    FM_NOT_USED(data);
    FM_NOT_USED(length);

    return FM_FAIL;

}   /* end fmPlatformI2cRead */




/*****************************************************************************/
/** fmPlatformI2cWrite
 * \ingroup platformApp
 *
 * \desc            Write to an I2C device.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       bus is the I2C bus number.
 *
 * \param[in]       address is the I2C device address (0x00 - 0x7F).
 *
 * \param[in]       data points to an array of 8-bit data values to write to
 *                  the device. The array must be length bytes long.
 *
 * \param[in]       length is the number of data bytes to write.
 *
 * \return          The number of bytes actually written. This value can be
 *                  smaller than the specified length if the device limits
 *                  the number of bytes written. A value of 0 means no device
 *                  was detected at specified device address.
 *
 *****************************************************************************/
fm_int fmPlatformI2cWrite(fm_int    sw,
                          fm_int    bus,
                          fm_int    address,
                          fm_uint32 data,
                          fm_int    length)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(bus);
    FM_NOT_USED(address);
    FM_NOT_USED(data);
    FM_NOT_USED(length);

    return FM_FAIL;

}   /* end fmPlatformI2cWrite */




/*****************************************************************************/
/** fmPlatformI2cWriteRead
 * \ingroup platformApp
 *
 * \desc            Write to then immediately read from an I2C device.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       bus is the I2C bus number.
 *
 * \param[in]       address is the I2C device address (0x00 - 0x7F).
 *
 * \param[in,out]   data points to an array from which data is written and
 *                  into which data is read.
 *
 * \param[in]       wl is the number of bytes to write.
 *
 * \param[in]       rl is the number of bytes to read.
 *
 * \return          The total number of bytes actually written and read. This
 *                  value can be smaller than the sum of wl and rl if the
 *                  device limits the number of bytes written or read. A value
 *                  of 0 means no device was detected at the specified device
 *                  address.
 *
 *****************************************************************************/
fm_int fmPlatformI2cWriteRead(fm_int     sw,
                              fm_int     bus,
                              fm_int     address,
                              fm_uint32 *data,
                              fm_int     wl,
                              fm_int     rl)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(bus);
    FM_NOT_USED(address);
    FM_NOT_USED(data);
    FM_NOT_USED(wl);
    FM_NOT_USED(rl);

    return FM_FAIL;

}   /* end fmPlatformI2cWriteRead */




/*****************************************************************************/
/** fmPlatformMdioRead
 * \ingroup platformApp
 *
 * \desc            Read 16 bits of data from the MDIO bus.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       type is a bit mask specifying MDIO access options.
 *                  See ''MDIO Management Options''.
 *
 * \param[in]       port is the MDIO physical port number.
 *
 * \param[in]       dev is the device on the MDIO port. dev is only used if
 *                  type includes the ''FM_SMGMT_MDIO_10G'' bit, otherwise it
 *                  is ignored.
 *
 * \param[in]       reg is the register number on the MDIO device.
 *
 * \param[out]      data contains the value read from the MDIO device.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_int fmPlatformMdioRead(fm_int     sw,
                          fm_int     type,
                          fm_int     port,
                          fm_int     dev,
                          fm_int     reg,
                          fm_uint16 *data)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(type);
    FM_NOT_USED(port);
    FM_NOT_USED(dev);
    FM_NOT_USED(reg);
    FM_NOT_USED(data);

    return FM_FAIL;

}   /* end fmPlatformMdioRead */




/*****************************************************************************/
/** fmPlatformMdioWrite
 * \ingroup platformApp
 *
 * \desc            Write 16 bits of data to the MDIO bus.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       type is a bit mask specifying MDIO access options.
 *                  See ''MDIO Management Options''.
 *
 * \param[in]       port is the MDIO physical port number.
 *
 * \param[in]       dev is the device on the MDIO port. dev is only used if
 *                  type includes the ''FM_SMGMT_MDIO_10G'' bit, otherwise it
 *                  is ignored.
 *
 * \param[in]       reg is the register number on the MDIO device.
 *
 * \param[in]       data contains the value to write to the MDIO device.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_int fmPlatformMdioWrite(fm_int    sw,
                           fm_int    type,
                           fm_int    port,
                           fm_int    dev,
                           fm_int    reg,
                           fm_uint16 data)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(type);
    FM_NOT_USED(port);
    FM_NOT_USED(dev);
    FM_NOT_USED(reg);
    FM_NOT_USED(data);

    return FM_FAIL;

}   /* end fmPlatformMdioWrite */




/*****************************************************************************/
/** fmPlatformSetPortDefaultVlan
 * \ingroup platform
 *
 * \desc            Inform the platform code about the default VLAN
 *                  change on a given port.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       physPort is the physical port being updated.
 *
 * \param[in]       defaultVlan is the new default vlan.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformSetPortDefaultVlan(fm_int sw,
                                       fm_int physPort,
                                       fm_int defaultVlan)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(physPort);
    FM_NOT_USED(defaultVlan);

    return FM_OK;

} /* end fmPlatformSetPortDefaultVlan */




/*****************************************************************************/
/** fmPlatformAddGlortToPortMapping
 * \ingroup platform
 *
 * \desc            Inform the platform code about the glort to port mapping.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       glort is the glort value being mapped.
 *
 * \param[in]       physPort is the physical port the glort maps to.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformAddGlortToPortMapping(fm_int sw,
                                          fm_int glort,
                                          fm_int physPort)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(glort);
    FM_NOT_USED(physPort);

    return FM_OK;

} /* end fmPlatformAddGlortToPortMapping */



/*****************************************************************************/
/** fmPlatformSetBypassMode
 * \ingroup platformApp
 *
 * \desc            This services enables bypass mode for the given switch
 *                  on platforms which support the feature.  Typically the
 *                  mode is set to TRUE sometime during the platform
 *                  initialization procedure, and is set to FALSE later
 *                  from the application.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[in]       mode is the bypass mode to set.
 *
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformSetBypassMode(fm_int sw, fm_bool mode)
{
    FM_NOT_USED(sw);
    FM_NOT_USED(mode);

    return FM_ERR_UNSUPPORTED;

} /* end fmPlatformSetBypassMode */




/*****************************************************************************/
/** fmPlatformBypassEnabled
 * \ingroup platform
 *
 * \desc            This services is called by both platform and API services
 *                  to query whether the platform is currently in bypass mode.
 *                  The value FALSE doubles as both as an indicator that
 *                  bypass mode is not supported as well as the case that
 *                  bypass mode is supported but not enabled.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 *
 * \return          TRUE if bypass mode is enabled, FALSE otherwise.
 *
 *****************************************************************************/
fm_bool fmPlatformBypassEnabled(fm_int sw)
{
    FM_NOT_USED(sw);

    return FALSE;

} /* end fmPlatformBypassEnabled */




/*****************************************************************************/
/** fmPlatformGetSwitchPartNumber
 * \ingroup platform
 *
 * \desc            This services is called by API services
 *                  to query the part number of the switch.
 *
 * \param[in]       sw is the switch number to operate on.
 *
 * \param[out]      partNum points to caller-allocated storage where this
 *                  function should place the switch part number.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformGetSwitchPartNumber(fm_int sw, fm_switchPartNum *partNum)
{
    fm_platform_state *ps;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_SWITCH;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    switch (ps->switchType)
    {
#ifdef FM_SUPPORT_FM4000
        case FM_PLATFORM_SWITCH_TYPE_FM4000:
            *partNum = FM_SWITCH_PART_NUM_FM4224;
            break;
#endif

#ifdef FM_SUPPORT_FM10000
        case FM_PLATFORM_SWITCH_TYPE_FM10000:
            *partNum = FM_SWITCH_PART_NUM_FM10440;
            break;
#endif

#ifdef FM_SUPPORT_HLP
        case FM_PLATFORM_SWITCH_TYPE_HLP:
            *partNum = FM_SWITCH_PART_NUM_FM10440;
            break;
#endif
#ifdef FM_SUPPORT_CPK
        case FM_PLATFORM_SWITCH_TYPE_CPK:
            *partNum = FM_SWITCH_PART_NUM_FM10440;
            break;
#endif

        default:
            return FM_ERR_UNSUPPORTED;
    }

    return FM_OK;

}   /* end fmPlatformGetSwitchPartNumber */



/*****************************************************************************/
/** fmPlatformGetPortDefaultSettings
 * \ingroup platform
 *
 * \desc            For FM6000 devices only, returns the default values for
 *                  certain port attributes as set by the platform layer
 *                  implementation at initialization for the specified
 *                  logical port. This function is provided strictly for the
 *                  benefit of the diagnostic function, ''fmDbgDumpPortMap''
 *                  and is not called for non-FM6000 devices.
 *                                                                      \lb\lb
 *                  Since the muxing of MACs (EPLs), lane ordering and lane
 *                  polarity are all dependent on how a platform is physically
 *                  wired, the platform layer will typically set the
 *                  associated attributes at initialization time:
 *                                                                      \lb\lb
 *                  ''FM_PORT_ETHERNET_INTERFACE_MODE''                     \lb
 *                  ''FM_PORT_SELECT_ACTIVE_MAC''                           \lb
 *                  ''FM_PORT_TX_LANE_ORDERING''                            \lb
 *                  ''FM_PORT_RX_LANE_ORDERING''                            \lb
 *                  ''FM_PORT_TX_LANE_POLARITY''                            \lb
 *                  ''FM_PORT_RX_LANE_POLARITY''
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logPort is the logical port number whose values are
 *                  to be returned.
 *
 * \param[out]      activeMac points to caller-allocated storage where this
 *                  function should place the default active MAC, corresponding
 *                  to the ''FM_PORT_SELECT_ACTIVE_MAC'' port attribute.
 *
 * \param[out]      ethMode points to caller-allocated storage where this
 *                  function should place the default Ethernet mode, corresponding
 *                  to the ''FM_PORT_ETHERNET_INTERFACE_MODE'' port attribute.
 *
 * \param[out]      ordering points to caller-allocated storage where this
 *                  function should place the default RX and TX lane ordering,
 *                  corresponding to the ''FM_PORT_RX_LANE_ORDERING'' and
 *                  ''FM_PORT_TX_LANE_ORDERING'' port attributes. See
 *                  ''Platform Lane Reversal Values'' for possible values.
 *
 * \param[out]      polarity points to caller-allocated storage where this
 *                  function should place the default RX and TX lane polarity,
 *                  corresponding to the ''FM_PORT_RX_LANE_POLARITY'' and
 *                  ''FM_PORT_TX_LANE_POLARITY'' port attributes. See
 *                  ''Platform Lane Polarity Values'' for possible values.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_PORT if logical port is out of range.
 *
 *****************************************************************************/
fm_status fmPlatformGetPortDefaultSettings(fm_int  sw,
                                           fm_int  logPort,
                                           fm_int *activeMac,
                                           fm_int *ethMode,
                                           fm_int *ordering,
                                           fm_int *polarity)
{
    fm_platform_state *ps;

    FM_NOT_USED(logPort);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_SWITCH;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    switch (ps->switchType)
    {
        case FM_PLATFORM_SWITCH_TYPE_FM10000:
            *activeMac = 0;
            *ethMode   = 0;
            *ordering  = 0;
            *polarity  = 0;
            return FM_OK;

        default:
            return FM_ERR_UNSUPPORTED;
    }

}   /* end fmPlatformGetPortDefaultSettings */




/*****************************************************************************/
/** fmPlatformLoadMicrocode
 * \ingroup platform
 *
 * \desc            For FM6000 devices, this function is called by the
 *                  API to request the platform layer to load microcode
 *                  into the device. This function is not called for
 *                  non-FM6000 devices.
 *
 * \param[in]       sw is the switch on which to operate. The switch
 *                  number must have already been validated.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformLoadMicrocode(fm_int sw)
{
    fm_platform_state *ps;

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_SWITCH;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    switch (ps->switchType)
    {
        default:
            return FM_ERR_UNSUPPORTED;
    }

}   /* end fmPlatformLoadMicrocode */




/*****************************************************************************/
/** fmPlatformSetRingMode
 * \ingroup platform
 *
 * \desc            Set ring mode for FM6000 devices only.
 *
 * \param[in]       sw is the switch on which to operate (ignored).
 *
 * \param[in]       mode is the ring mode.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmPlatformSetRingMode(fm_int sw, fm_int mode)
{
    fm_platform_state *ps;

    FM_NOT_USED(mode);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_SWITCH;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    switch (ps->switchType)
    {
        default:
            return FM_ERR_UNSUPPORTED;
    }

}   /* end fmPlatformSetRingMode */



/*****************************************************************************/
/** fmPlatformGetSchedulerConfig
 * \ingroup platform
*
 * \desc            Indicates to the API how the scheduler should be
 *                  initialized for a FM6000 device. Note that this
 *                  function is not called for non-FM6000 switch devices.
 *                  There are three modes supported to initialize the scheduler.
 *                  Each mode is described below:
 *                                                                      \lb\lb
 *                  In FM_SCHED_INIT_MODE_AUTOMATIC mode, the
 *                  fm_schedulerConfig structure pointer passed by the API
 *                  should be filled with 40G and 10G port capabilities.
 *                                                                      \lb\lb
 *                  Each port can be in one of three classes. The purpose of
 *                  this mode is to identify the class that each port
 *                  belongs to. The structure takes two lists of ports, a 40G
 *                  list and a 10G list. A port's appearance in one or the
 *                  other or neither list serves to identify its class. The
 *                  classes are as follows:
 *                                                                      \lb\lb
 *                  40G capable ports: a port that may be operated at 40G. It
 *                  might also be used for 10G or less. Such ports must appear
 *                  in the 40G list. Only the lowest numbered port of the four
 *                  ports sharing a MAC (EPL) is put into the 40G port list;
 *                  the other three ports are implied and should not appear in
 *                  either list. A port that appears in the 40G list should
 *                  not appear in the 10G list, even if it will sometimes
 *                  operate at 10G or less. If a port is equipped with two
 *                  MACs and may run at 40G on only one MAC and will run only
 *                  at 10G or less on the other MAC, it is still considered a
 *                  40G capable port and should appear in the 40G list only.
 *                                                                      \lb\lb
 *                  10G (or less) only ports: a port that will never be
 *                  operated at 40G, but will be used at 10G or less. Such
 *                  ports must appear in the 10G list, unless the port shares
 *                  a MAC (EPL) with another port that appears in the 40G list.
 *                                                                      \lb\lb
 *                  Unused ports: a port that will never be used at any speed
 *                  should not appear in either list.
 *                                                                      \lb\lb
 * \note            The number of 40G ports should not exceed 18 and the number
 *                  of 10G ports should not exceed 72.
 *                                                                      \lb\lb
 *                  In the FM_SCHED_INIT_MODE_MANUAL mode, the
 *                  fm_schedulerConfig structure should be filled with token
 *                  information to be directly written to the scheduler.
 *                  The API validates this data to ensure the token list does
 *                  not violate any scheduler initalization rule. The rules
 *                  are as follows:
 *                                                                      \lb\lb
 *                  1. Tokens should be loaded in alternating port color
 *                  2. Port tokens for the same PORT4 are at least four tokens
 *                     appart.
 *                  3. 40G capable ports should be spaced as evenly as possible.
 *                     Note that this rule is not validated but should be
 *                     applied.
 *
 * \note            The port numbers passed into this structure should
 *                  use physical port numbers, being the numbers of the
 *                  switch fabric's "internal ports". The ports may be listed
 *                  in any order.
 *
 *                  In the FM_SCHED_INIT_MODE_NONE mode, the API will not
 *                  initialize the scheduler, it is therefore the platform's
 *                  responsability to do so. In this mode, it is required
 *                  to configure the nbTokens and nbSyncTokens fields.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      sc is a pointer to the caller allocated scheduler config
 *                  structure and should be filled with the desired mode and
 *                  data. Refer to fm_schedulerConfig for details on the
 *                  limits of certains structure fields and which fields
 *                  are required in each mode.
 *
 * \return          FM_OK if successful.
 * \return          Other ''Status Codes'' as appropriate in case of failure.
 *
 *****************************************************************************/
fm_status fmPlatformGetSchedulerConfig(fm_int sw, fm_schedulerConfig *sc)
{
    fm_platform_state *ps;

    FM_NOT_USED(sc);

    if ( (sw < 0) || (sw >= FM_MAX_NUM_FOCALPOINTS) )
    {
        return FM_ERR_INVALID_SWITCH;
    }

    ps = &fmRootPlatform->fmPlatformState[sw];

    switch (ps->switchType)
    {
        default:
            return FM_ERR_UNSUPPORTED;
    }

}   /* end fmPlatformGetSchedulerConfig */

