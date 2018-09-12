/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP core & interfaces.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <sys/time.h>

#include <lldp.h>
#include <lldp_int.h>
#include <lldp_macros.h>

#include <lldp_rx.h>
#include <lldp_tx.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

const char *adminStatus_s[] = {
    "tx_only",
    "RX_only",
    "tx_and_rx",
    "disabled"
};

const char *rxState_s[] = {
    "unknown",
    "wait_for_port_operational",
    "delete_aged_info",
    "rx_lldp_initialize",
    "rx_wait_for_frame",
    "rx_frame",
    "delete_info",
    "update_info"
};

const char *txState_s[] = {
    "unknown",
    "tx_lldp_initialize",
    "tx_idle",
    "tx_info_frame",
    "tx_shutdown_frame"
};

/*****************************************************************************
 * Global Variables
 *****************************************************************************/

int lldpLogLevel = 3;


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static int lldpClearSomethingChangedLocal();
static int lldpCheckLinkStatus();


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** lldpTask
 * \ingroup lldp
 *
 * \desc            LLDP state-machine thread entry.
 *
 * \param[in]       args is not used.
 *
 * \return          NULL always.
 *
 *****************************************************************************/
static void *lldpTask(void *args __attribute__((__unused__)))
{
    lldp_event *event;
    struct timeval timeout;

    timeout.tv_sec  = 1;
    timeout.tv_usec = 0;

    LLDP_INFO("LLDP: LLDP task running.\n");

    while (!lldpDB->shutdown)
    {
        /* get thread event (or timeout) */
        event = lldpEventQueueWait(&lldpDB->queue, &timeout);

        if (event) 
            LLDP_DBG("LLDP: Got event (type:%d portNum:%d).\n", event->type, event->port);

        pthread_mutex_lock(&lldpDB->lock);

        /* check link-status */
        lldpCheckLinkStatus();

        /* run lldp RX module */
        lldpRxRun(event);

        /* run lldp TX module */
        lldpTxRun(event);

        /* clear something-changed local flags */
        lldpClearSomethingChangedLocal();

        pthread_mutex_unlock(&lldpDB->lock);

        if (event) {
            free(event);
            event = NULL;
        }
    }

    LLDP_INFO("LLDP: LLDP task exiting.\n");

    return NULL;
}

/*****************************************************************************/
/** lldpClearSomethingChangedLocal
 * \ingroup lldp
 *
 * \desc            Clears the somethingChangedLocal flag on all local ports.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
static int lldpClearSomethingChangedLocal()
{
    struct lldpLocPort *iter, *next;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpLocPort, node, &lldpDB->lldpLocalSystemData.lldpLocPortTable)
    {
        iter->somethingChangedLocal = 0;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpCheckLinkStatus
 * \ingroup lldp
 *
 * \desc            Checks link-status for all local ports.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
static int lldpCheckLinkStatus()
{
    struct lldpPortConfig *iter, *next;

    if (!lldpDB->physical || !lldpDB->physical->getLinkStatus)
        return LLDP_OK;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpPortConfig, node, &lldpDB->lldpConfiguration.lldpPortConfigTable)
    {
        lldpDB->physical->getLinkStatus(iter->lldpPortConfigPortNum, &iter->lldpPortConfigEnabled);
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpPostChangedEvent
 * \ingroup lldp
 *
 * \desc            Post local-changed event to LLDP state-machine.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
static int lldpPostChangedEvent(int portNum)
{
    lldp_event *event = (lldp_event *)calloc(1, sizeof(lldp_event));
    if (!event) return LLDP_ERR_NO_MEMORY;

    event->type = LLDP_EVENT_LOCAL_CHANGED;
    event->port = portNum;
    event->size = 0;

    lldpEventQueuePost(&lldpDB->queue, event);

    return LLDP_OK;

} /* end lldpPostChangedEvent */

/*****************************************************************************/
/** lldpPostPacketEvent
 * \ingroup lldp
 *
 * \desc            Post ingress-packet event to LLDP state-machine.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       buf is a pointer to ingress packet data.
 *
 * \param[in]       size is the number of bytes in the packet data buffer.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
static int lldpPostPacketEvent(int portNum, uint8_t *buf, size_t size)
{
    lldp_event *event = (lldp_event *)calloc(1, sizeof(lldp_event));
    if (!event) return LLDP_ERR_NO_MEMORY;

    event->type = LLDP_EVENT_PACKET;
    event->port = portNum;
    event->size = size;
    memcpy(event->data, buf, MIN(size, LLDP_EVENT_MAX_DATA_SIZE));

    lldpEventQueuePost(&lldpDB->queue, event);

    return LLDP_OK;

} /* end lldpPostPacketEvent */

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** lldpHandleLocalChange
 * \ingroup lldp
 *
 * \desc            Handle change in local data-base. This function sets the
 *                  port somethingChangedLocal to true, and post a change
 *                  event to the LLDP state-machine.
 *
 * \param[in]       portNum is the local port number, or -1 for all ports.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpHandleLocalChange(int portNum)
{
    int status = LLDP_OK;

    struct lldpLocPort *locPort, *locNext;

    pthread_mutex_lock(&lldpDB->lock);

    if (portNum == -1)
    {
        LLDP_LIST_FOR_EACH_SAFE(locPort, locNext, struct lldpLocPort, node, &lldpDB->lldpLocalSystemData.lldpLocPortTable)
        {
            locPort->somethingChangedLocal = 1;
        }
    }
    else {
        if ((locPort = lldpLocPortGet(portNum)) == NULL) {
            status = LLDP_ERR_NOT_FOUND;
            goto ABORT;
        }

        locPort->somethingChangedLocal = 1;
    }

    /* wake-up LLDP thread */
    status = lldpPostChangedEvent(portNum);

ABORT:
    pthread_mutex_unlock(&lldpDB->lock);

    return status;

} /* end lldpHandleLocalChange */

/*****************************************************************************/
/** lldpInitialize
 * \ingroup lldp
 *
 * \desc            Initializes LLDP manager, by creating the LLDP data-base,
 *                  and starting the LLDP state-machine thread.
 *
 * \param[in]       chassisMacAddress is local chassis mac address that is used
 *                  as source address for sending outgoing LLDP messages.
 *
 * \param[in]       physicalIf is an interface to the physical layer, which
 *                  provides the LLDP state-machine interface for sending packets
 *                  and for querying the ports link-status.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpInitialize(uint64_t chassisMacAddress, struct lldpPhysicalIf *physicalIf)
{
    int i, status = LLDP_OK;
    uint8_t mac[6];

    /* initialize LLDP database */
    lldpDB = lldpObjectsCreate();
    if (!lldpDB) return LLDP_ERR_NO_MEMORY;

    /* convert chassis MAC address to string (for default chassis-id) */
    for (i=0; i<6; i++)
        mac[i] = (chassisMacAddress >> (40 - 8*i)) & 0xff;

    /* initialize LLDP chassis MAC address, used as source MAC address for
       outgoing LLDPDU packets. */
    lldpDB->lldpConfiguration.chassisMacAddress = chassisMacAddress;

    /* initialize chassis-id, name and description */
    lldpDB->lldpLocalSystemData.lldpLocChassisIdSubtype = LLDP_CHASSIS_ID_MAC_ADDRESS;
    lldpDB->lldpLocalSystemData.lldpLocChassisIdLen = sizeof(mac);
    memcpy(lldpDB->lldpLocalSystemData.lldpLocChassisId, mac, lldpDB->lldpLocalSystemData.lldpLocChassisIdLen);
    
    /* setup hooks and callbacks */
    lldpDB->physical = physicalIf;
    lldpDB->protocol = NULL;

    /* initialize LLDP RX module */
    if ((status = lldpRxInitialize()) != LLDP_OK)
        return status;

    /* initialize LLDP TX module */
    if ((status = lldpTxInitialize()) != LLDP_OK)
        return status;

    /* create LLDP state-machine thread */
    if ((pthread_create(&lldpDB->thread, NULL, lldpTask, lldpDB)) != 0)
        status = LLDP_ERR_UNKNOWN;

    LLDP_INFO("LLDP: Initialized (chassisId:%s).\n", 
              lldpDB->lldpLocalSystemData.lldpLocChassisId);

    return status;
}

/*****************************************************************************/
/** lldpTerminate
 * \ingroup lldp
 *
 * \desc            Terminates LLDP manager, by stopping the LLDP state-machine
 *                  thread and destroying the LLDP data-base.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpTerminate(void)
{
    if (!lldpDB) return LLDP_OK;

    lldpDB->shutdown = true;
    pthread_join(lldpDB->thread, NULL);

    /* destroy LLDP MIB data object */
    lldpObjectsDestroy(lldpDB);
    lldpDB = NULL;

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpRegisterProtocol
 * \ingroup lldp
 *
 * \desc            Registers high-level protocols (such as DCBX) for receiving
 *                  LLDP events, and for sending organizationally TLVs.
 *
 * \param[in]       protocolIf is an interface to the protocol layer, through
 *                  which the LLDP state-machine notify the protocol layer upon
 *                  incoming/outgoing LLDP TLVs, and LLDP events.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpRegisterProtocol(struct lldpProtocolIf *protocolIf)
{
    if (lldpDB->protocol)
        return LLDP_ERR_ALREADY_EXISTS;

    lldpDB->protocol = protocolIf;

    if (protocolIf && protocolIf->initializeModules)
        protocolIf->initializeModules();

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpUnregisterProtocol
 * \ingroup lldp
 *
 * \desc            Unregister protocol layer which was registered by the
 *                  lldpRegisterProtocol() function.
 *
 * \param[in]       protocolIf is an interface to the protocol layer that need
 *                  to be unregistered.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpUnregisterProtocol(struct lldpProtocolIf *protocolIf)
{
    if (!lldpDB->protocol)
        return LLDP_ERR_NOT_FOUND;

    if (protocolIf && protocolIf->terminateModules)
        protocolIf->terminateModules();

    lldpDB->protocol = NULL;

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpAddPort
 * \ingroup lldp
 *
 * \desc            Add port to LLDP.
 *
 * \param[in]       portNum is the local port number to be added.
 *
 * \param[in]       portIdSubtype is the port-id subtype (one of 
 *                  lldpPortIdSubtype enum).
 *
 * \param[in]       portId is a pointer to the port-id buffer/string.
 *
 * \param[in]       portIdLen is the port-id length.
 *
 * \param[in]       portDesc is a pointer to the port-description string.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpAddPort(int    portNum, 
                int    portIdSubtype, 
                char  *portId,
                size_t portIdLen,
                char  *portDesc)
{
    int status = LLDP_OK;

    struct lldpLocPort    *locPort    = NULL;
    struct lldpPortConfig *portConfig = NULL;
    struct lldpStatsPort  *statsPort  = NULL;

    pthread_mutex_lock(&lldpDB->lock);

    /* allocate local port entry */
    if ((locPort = lldpLocPortCreate(portNum, portIdSubtype, portId, portIdLen, portDesc)) == NULL) {
        status = LLDP_ERR_ALREADY_EXISTS;
        goto ABORT;
    }

    /* allocate port config entry */
    if ((portConfig = lldpPortConfigCreate(portNum)) == NULL) {
        status = LLDP_ERR_ALREADY_EXISTS;
        goto ABORT;
    }

    /* allocate port statistics entry */
    if ((statsPort = lldpStatsPortCreate(portNum)) == NULL) {
        status = LLDP_ERR_ALREADY_EXISTS;
        goto ABORT;
    }

    LLDP_INFO("LLDP: Added Port (portNum:%d).\n", portNum);

ABORT:
    pthread_mutex_unlock(&lldpDB->lock);

    if (status == LLDP_OK)
        status = lldpHandleLocalChange(portNum);
    else {
        if (locPort)    lldpLocPortDestroy(locPort);
        if (portConfig) lldpPortConfigDestroy(portConfig);
        if (statsPort)  lldpStatsPortDestroy(statsPort);
    }

    return status;

} /* end lldpAddPort */

/*****************************************************************************/
/** lldpRemovePort
 * \ingroup lldp
 *
 * \desc            Remove port from LLDP, that was added with lldpAddPort().
 *
 * \param[in]       portNum is the local port number to be removed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpRemovePort(int portNum)
{
    int status = LLDP_OK;

    pthread_mutex_lock(&lldpDB->lock);;

    lldpLocPortDestroy(lldpLocPortGet(portNum));
    lldpPortConfigDestroy(lldpPortConfigGet(portNum));
    lldpStatsPortDestroy(lldpStatsPortGet(portNum));

    LLDP_INFO("LLDP: Removed Port (portNum:%d).\n", portNum);

    pthread_mutex_unlock(&lldpDB->lock);

    return status;

} /* end lldpRemovePort */

/*****************************************************************************/
/** lldpHandleIngressPacket
 * \ingroup lldp
 *
 * \desc            Entry point for physical layer to push LLDP packets to the
 *                  LLDP manager. This function generates a incoming packet
 *                  event that is being posted to the LLDP state-machine.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       buf is a pointer to ingress packet data.
 *
 * \param[in]       size is the number of bytes in the packet data buffer.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpHandleIngressPacket(int portNum, uint8_t *buf, size_t size)
{
    /* dump incoming packet */
    if (LLDP_DBG_ENABLED()) {
        char stmp[128];
        size_t i, l;

        LLDP_DBG("LLDP: Got incoming frame (portNum:%d size:%zu).\n", portNum, size);

        l = 0;
        stmp[0] = '\0';
        for (i=0; i<size; i++) {
            l += snprintf(stmp + l, sizeof(stmp) - l, " %02x", buf[i]);
            if (((i % 16) == 15) || (i == (size - 1))) {
                LLDP_DBG("LLDP:%s\n", stmp);
                l = 0;
                stmp[0] = '\0';
            }
        }
    }

    return lldpPostPacketEvent(portNum, buf, size);

} /* end lldpHandleIngressPacket */

/*****************************************************************************/
/** lldpHandleLinkStatusChange
 * \ingroup lldp
 *
 * \desc            Entry point for physical layer to push port link-status
 *                  change notifications to the LLDP manager. If the 
 *                  physicalIf interface contains a valid pointer to the 
 *                  getLinkStatus(), the LLDP polls for change, and thus the
 *                  physical layer is not required to call this function. If
 *                  however the getLinkStatus() is not provided, physical layer
 *                  should notify the LLDP state-machine by calling this function.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       linkStatus is port link-status (0=down, 1=up).
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpHandleLinkStatusChange(int portNum, int linkStatus)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        portConfig->lldpPortConfigEnabled = linkStatus;
        LLDP_INFO("LLDP: Link-Status Changed (portNum:%d LinkStatus:%d).\n", portNum, linkStatus);
    }
    END_CRITICAL(portNum)

} /* end lldpProcessLinkStatusChange */

/*****************************************************************************/
/** lldpSetTxTiming
 * \ingroup lldp
 *
 * \desc            Set the LLDP state-machine TX timing parameters.
 *
 * \param[in]       msgTxInterval is the interval at which LLDP frames are
 *                  transmitted on behalf of this LLDP manager.
 *
 * \param[in]       msgTxHold is the msgTxInterval multiplier, which results
 *                  with the time-to-live settings.
 *
 * \param[in]       reinitDelay is the delay (in units of seconds) from when
 *                  port becomes 'disabled' until re-initialization will be
 *                  attempted.
 *
 * \param[in]       txDelay is the minimum delay between LLDP transmits.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetTxTiming(int msgTxInterval, int msgTxHold, int reinitDelay, int txDelay)
{
    BEGIN_CRITICAL() {
        if (msgTxInterval != -1) {
            lldpDB->lldpConfiguration.lldpMessageTxInterval = msgTxInterval;
            LLDP_INFO("LLDP: Tx-Timing Changed (msgTxInterval:%d).\n", msgTxInterval);
        }
        if (msgTxHold != -1) {
            lldpDB->lldpConfiguration.lldpMessageTxHoldMultiplier = msgTxHold;
            LLDP_INFO("LLDP: Tx-Timing Changed (msgTxHold:%d).\n", msgTxHold);
        }
        if (reinitDelay != -1) {
            lldpDB->lldpConfiguration.lldpReinitDelay = reinitDelay;
            LLDP_INFO("LLDP: Tx-Timing Changed (reinitDelay:%d).\n", reinitDelay);
        }
        if (txDelay != -1) {
            lldpDB->lldpConfiguration.lldpTxDelay = txDelay;
            LLDP_INFO("LLDP: Tx-Timing Changed (txDelay:%d).\n", txDelay);
        }
    }
    END_CRITICAL(-1)

} /* end lldpSetTxTiming */

/*****************************************************************************/
/** lldpSetChassisId
 * \ingroup lldp
 *
 * \desc            Set local chassis-id.
 *
 * \param[in]       chassisIdSubtype is the chassis-id type (one of 
 *                  lldpChassisIdSubtype enum).
 *
 * \param[in]       chassisId is a pointer to the chassis-id buffer/string.
 *
 * \param[in]       chassisIdLen is the chassis-id length.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetChassisId(int chassisIdSubtype, char *chassisId, size_t chassisIdLen)
{
    BEGIN_CRITICAL() {
        lldpDB->lldpLocalSystemData.lldpLocChassisIdSubtype = chassisIdSubtype;
        lldpDB->lldpLocalSystemData.lldpLocChassisIdLen = chassisIdLen;
        memcpy(lldpDB->lldpLocalSystemData.lldpLocChassisId, chassisId, chassisIdLen);
        lldpDB->lldpLocalSystemData.lldpLocChassisId[chassisIdLen] = '\0';

        LLDP_INFO("LLDP: Chassis-Id Changed (chassisId:%s chassisIdLen:%zu chassisIdSubtype:%d).\n", chassisId, chassisIdLen, chassisIdSubtype);
    }
    END_CRITICAL(-1)

} /* end lldpSetChassisId */

/*****************************************************************************/
/** lldpSetSystemName
 * \ingroup lldp
 *
 * \desc            Set local system-name.
 *
 * \param[in]       sysName is the system-name to be set.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetSystemName(char *sysName)
{
    BEGIN_CRITICAL() {
        lldp_strcpy(lldpDB->lldpLocalSystemData.lldpLocSysName, sysName);
        LLDP_INFO("LLDP: System-Name Changed (sysName:%s).\n", sysName);
    }
    END_CRITICAL(-1)

} /* end lldpSetSystemName */

/*****************************************************************************/
/** lldpSetSystemDescription
 * \ingroup lldp
 *
 * \desc            Set local system-description.
 *
 * \param[in]       sysDesc is the system-description to be set.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetSystemDescription(char *sysDesc)
{
    BEGIN_CRITICAL() {
        lldp_strcpy(lldpDB->lldpLocalSystemData.lldpLocSysDesc, sysDesc);
        LLDP_INFO("LLDP: System-Description Changed (sysDesc:%s).\n", sysDesc);
    }
    END_CRITICAL(-1)

} /* end lldpSetSystemDescription */

/*****************************************************************************/
/** lldpSetSystemCapabilities
 * \ingroup lldp
 *
 * \desc            Set local system-capabilities.
 *
 * \param[in]       sysCapSupported is the support system capabilities (mask of
 *                  lldpSystemCapability enums).
 *
 * \param[in]       sysCapEnabled is the enabled system capabilities (mask of
 *                  lldpSystemCapability enums).
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetSystemCapabilities(uint16_t sysCapSupported, uint16_t sysCapEnabled)
{
    BEGIN_CRITICAL() {
        lldpDB->lldpLocalSystemData.lldpLocSysCapSupported = sysCapSupported;
        lldpDB->lldpLocalSystemData.lldpLocSysCapEnabled = sysCapEnabled;
        LLDP_INFO("LLDP: System-Capabilities Changed (sysCapSupported:%04x sysCapEnabled:%04x).\n", sysCapSupported, sysCapEnabled);
    }
    END_CRITICAL(-1)

} /* end lldpSetSystemCapabilities */

/*****************************************************************************/
/** lldpSetPortAdminStatus
 * \ingroup lldp
 *
 * \desc            Set local port admin-status.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       adminStatus is the port admin status (one of
 *                  lldpAdminStatus enum).
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetPortAdminStatus(int portNum, int adminStatus)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        portConfig->lldpPortConfigAdminStatus = adminStatus;
        LLDP_INFO("LLDP: Admin-Status Changed (portNum:%d adminStatus:%d).\n", portNum, adminStatus);
    }
    END_CRITICAL(portNum)

} /* end lldpSetPortAdminStatus */

/*****************************************************************************/
/** lldpSetTLVsTxEnable
 * \ingroup lldp
 *
 * \desc            Set which optional TLVs are enabled for transmit.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvsTxMask is the mask of TLVs to be changed.
 *
 * \param[in]       tlvsTxEnable is the mask of TLVs to be enabled.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetTLVsTxEnable(int portNum, uint16_t tlvsTxMask, uint16_t tlvsTxEnable)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        portConfig->lldpPortConfigTLVsTxEnable = 
            (portConfig->lldpPortConfigTLVsTxEnable & (~tlvsTxMask)) |
            (portConfig->lldpPortConfigTLVsTxEnable & ( tlvsTxMask) & (tlvsTxEnable));
        LLDP_INFO("LLDP: TLVs Tx-Enable Changed (portNum:%d tlvsTxMask:%04x tlvsTxEnable:%04x).\n", portNum, tlvsTxMask, tlvsTxEnable);
    }
    END_CRITICAL(portNum)

} /* end lldpSetTLVsTxEnable */

/*****************************************************************************/
/** lldpSetPortId
 * \ingroup lldp
 *
 * \desc            Set local port-id.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       portIdSubtype is the port-id type (one of 
 *                  lldpPortIdSubtype enum).
 *
 * \param[in]       portId is a pointer to the port-id buffer/string.
 *
 * \param[in]       portIdLen is the port-id length.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetPortId(int portNum, int portIdSubtype, char *portId, size_t portIdLen)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        locPort->lldpLocPortIdSubtype = portIdSubtype;
        locPort->lldpLocPortIdLen = portIdLen;
        memcpy(locPort->lldpLocPortId, portId, portIdLen);
        locPort->lldpLocPortId[portIdLen] = '\0';

        LLDP_INFO("LLDP: Port-Id Changed (portNum:%d portId:%s portIdLen:%zu portIdSubtype:%d).\n", portNum, portId, portIdLen, portIdSubtype);
    }
    END_CRITICAL(portNum)

} /* end lldpSetPortId */

/*****************************************************************************/
/** lldpSetPortDesc
 * \ingroup lldp
 *
 * \desc            Set local port-description.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       portDesc is the port description to be set.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpSetPortDesc(int portNum, char *portDesc)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        lldp_strcpy(locPort->lldpLocPortDesc, portDesc);
        LLDP_INFO("LLDP: Port-Description Changed (portNum:%d portDesc:%s).\n", portNum, portDesc);
    }
    END_CRITICAL(portNum)

} /* end lldpSetPortDesc */


/*****************************************************************************
 * Status
 *****************************************************************************/

/*****************************************************************************/
/** lldpShowConfiguration
 * \ingroup lldp
 *
 * \desc            Dumps LLDP configuration information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpShowConfiguration(int portNum)
{
    struct lldpPortConfig *citer, *cnext;

    if (portNum == -1) {
        printf
            ("MsgTxInterval                    : %d\n",
             lldpDB->lldpConfiguration.
             lldpMessageTxInterval);
        printf
            ("MsgTxHold                        : %d\n",
             lldpDB->lldpConfiguration.
             lldpMessageTxHoldMultiplier);
        printf
            ("ReinitDelay                      : %d\n",
             lldpDB->lldpConfiguration.lldpReinitDelay);
        printf
            ("TxDelay                          : %d\n",
             lldpDB->lldpConfiguration.lldpTxDelay);
        printf
            ("ChassisMacAddress                : %02x:%02x:%02x:%02x:%02x:%02x\n\n",
             (uint8_t)((lldpDB->lldpConfiguration.chassisMacAddress >> 40) & 0xff),
             (uint8_t)((lldpDB->lldpConfiguration.chassisMacAddress >> 32) & 0xff),
             (uint8_t)((lldpDB->lldpConfiguration.chassisMacAddress >> 24) & 0xff),
             (uint8_t)((lldpDB->lldpConfiguration.chassisMacAddress >> 16) & 0xff),
             (uint8_t)((lldpDB->lldpConfiguration.chassisMacAddress >>  8) & 0xff),
             (uint8_t)((lldpDB->lldpConfiguration.chassisMacAddress      ) & 0xff));
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(citer, cnext,
                            struct lldpPortConfig, node,
                            &lldpDB->lldpConfiguration.
                            lldpPortConfigTable) {
        if (citer->lldpPortConfigPortNum != portNum) continue;

        printf
            ("port-%02d.AdminStatus              : %s\n",
             citer->lldpPortConfigPortNum,
             adminStatus_s[citer->
                           lldpPortConfigAdminStatus]);
        printf
            ("port-%02d.PortEnabled              : %s\n",
             citer->lldpPortConfigPortNum,
             citer->lldpPortConfigEnabled ? "up" : "down");
        printf
            ("port-%02d.TlvsTxEnable             : 0x%04x\n\n",
             citer->lldpPortConfigPortNum,
             citer->lldpPortConfigTLVsTxEnable);
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpShowLocalData
 * \ingroup lldp
 *
 * \desc            Dumps LLDP local-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpShowLocalData(int portNum)
{
    char stmp[256];
    uint32_t now = time(NULL);
    struct lldpLocPort *liter, *lnext;

    if (portNum == -1) {
        printf
            ("ChassisId                        : %s\n",
             lldpChassisIdString(
             lldpDB->lldpLocalSystemData.lldpLocChassisIdSubtype,
             lldpDB->lldpLocalSystemData.lldpLocChassisId,
             lldpDB->lldpLocalSystemData.lldpLocChassisIdLen, stmp, sizeof(stmp)));
        printf
            ("SystemName                       : %s\n",
             lldpDB->lldpLocalSystemData.lldpLocSysName);
        printf
            ("SystemDescription                : %s\n",
             lldpDB->lldpLocalSystemData.lldpLocSysDesc);
        printf
            ("SysCapSupported                  : 0x%04x\n",
             lldpDB->lldpLocalSystemData.
             lldpLocSysCapSupported);
        printf
            ("SysCapEnabled                    : 0x%04x\n\n",
             lldpDB->lldpLocalSystemData.
             lldpLocSysCapEnabled);
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(liter, lnext,
                            struct lldpLocPort, node,
                            &lldpDB->lldpLocalSystemData.
                            lldpLocPortTable) {
        if (liter->lldpLocPortNum != portNum) continue;

        printf
            ("port-%02d.PortId                   : %s\n",
             liter->lldpLocPortNum,
             lldpPortIdString(
             liter->lldpLocPortIdSubtype,
             liter->lldpLocPortId,
             liter->lldpLocPortIdLen, stmp, sizeof(stmp)));
        printf
            ("port-%02d.PortDescription          : %s\n",
             liter->lldpLocPortNum,
             liter->lldpLocPortDesc);
        printf
            ("port-%02d.RxState                  : %s (rxInfoAged:%d rcvFrame:%d badFrame:%d)\n",
             liter->lldpLocPortNum,
             rxState_s[liter->rxState], liter->rxInfoAged,
             liter->rcvFrame, liter->badFrame);
        printf
            ("port-%02d.TxState                  : %s (txTTL:%d txTTR:%d txDelayWhile:%d txShutdownWhile:%d)\n",
             liter->lldpLocPortNum,
             txState_s[liter->txState], liter->txTTL,
             TIMER_A(liter->txTTR),
             TIMER_A(liter->txDelayWhile),
             TIMER_A(liter->txShutdownWhile));
        printf
            ("port-%02d.Neighbors (port / mgmt)  : %d / %d (tooMany:%d tooManyTimer:%d)\n",
             liter->lldpLocPortNum, liter->portNeighbors,
             liter->mgmtNeighbors, liter->tooManyNeighbors,
             TIMER_A(liter->tooManyNeighborsTimer));
        printf
            ("port-%02d.SomethingChangedRemote   : %d (remote:%d local:%d)\n\n",
             liter->lldpLocPortNum,
             liter->somethingChangedRemote || liter->somethingChangedLocal,
             liter->somethingChangedRemote,
             liter->somethingChangedLocal);
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpShowRemoteData
 * \ingroup lldp
 *
 * \desc            Dumps LLDP remote-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpShowRemoteData(int portNum)
{
    char stmp[256];
    uint32_t now = time(NULL);
    struct lldpRem *riter, *rnext;

    if (portNum == -1) {
        printf("No global state in remote data-set.\n");
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(riter, rnext, struct lldpRem,
                            node,
                            &lldpDB->lldpRemoteSystemsData.
                            lldpRemTable) {
        if (riter->lldpRemLocalPortNum != portNum) continue;

        printf
            ("port-%02d.TimeMark                 : %d\n",
             riter->lldpRemLocalPortNum,
             TIMER_S(riter->lldpRemTimeMark));
#if 0
        printf
            ("port-%02d.index                    : %d\n",
             riter->lldpRemLocalPortNum,
             riter->lldpRemIndex);
#endif
        printf
            ("port-%02d.ChassisId                : %s\n",
             riter->lldpRemLocalPortNum,
             lldpChassisIdString(
             riter->lldpRemChassisIdSubtype,
             riter->lldpRemChassisId,
             riter->lldpRemChassisIdLen, stmp, sizeof(stmp)));
        printf
            ("port-%02d.PortId                   : %s\n",
             riter->lldpRemLocalPortNum,
             lldpPortIdString(
             riter->lldpRemPortIdSubtype,
             riter->lldpRemPortId,
             riter->lldpRemPortIdLen, stmp, sizeof(stmp)));
        printf
            ("port-%02d.PortDescription          : %s\n",
             riter->lldpRemLocalPortNum,
             riter->lldpRemPortDesc);
        printf
            ("port-%02d.SystemName               : %s\n",
             riter->lldpRemLocalPortNum,
             riter->lldpRemSysName);
        printf
            ("port-%02d.SystemDescription        : %s\n",
             riter->lldpRemLocalPortNum,
             riter->lldpRemSysDesc);;
        printf
            ("port-%02d.SysCapSupported          : 0x%04x\n",
             riter->lldpRemLocalPortNum,
             riter->lldpRemSysCapSupported);
        printf
            ("port-%02d.SysCapEnabled            : 0x%04x\n",
             riter->lldpRemLocalPortNum,
             riter->lldpRemSysCapEnabled);
        printf
            ("port-%02d.RxInfoTTL                : %d\n\n",
             riter->lldpRemLocalPortNum,
             TIMER_A(riter->rxInfoTTL));
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpShowCounters
 * \ingroup lldp
 *
 * \desc            Dumps LLDP counters information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int lldpShowCounters(int portNum)
{
    struct lldpStatsPort *riter, *rnext;
    uint32_t now = time(NULL);

    if (portNum == -1) {
        printf
            ("RemoteLastChangeTime             : %d\n",
             TIMER_S(lldpDB->lldpStatistics.lldpStatsRemTablesLastChangeTime));
        printf
            ("RemoteInserts                    : %d\n",
             lldpDB->lldpStatistics.lldpStatsRemTablesInserts);
        printf
            ("RemoteDeletes                    : %d\n",
             lldpDB->lldpStatistics.lldpStatsRemTablesDeletes);
        printf
            ("RemoteDrops                      : %d\n",
             lldpDB->lldpStatistics.lldpStatsRemTablesDrops);
        printf
            ("RemoteAgeouts                    : %d\n\n",
             lldpDB->lldpStatistics.lldpStatsRemTablesAgeouts);
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(riter, rnext,
                            struct lldpStatsPort, node,
                            &lldpDB->lldpStatistics.
                            lldpStatsPortTable) {
        if (riter->lldpStatsPortNum != portNum) continue;

        printf
            ("port-%02d.Tx.FramesTotal           > %d\n\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsTxPortFramesTotal);
        printf
            ("port-%02d.Rx.FramesDiscardedTotal  > %d\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsRxPortFramesDiscardedTotal);
        printf
            ("port-%02d.Rx.FramesErrors          > %d\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsRxPortFramesErrors);
        printf
            ("port-%02d.Rx.FramesTotal           > %d\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsRxPortFramesTotal);
        printf
            ("port-%02d.Rx.TlvsDiscardedTotal    > %d\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsRxPortTLVsDiscardedTotal);
        printf
            ("port-%02d.Rx.TlvsUnrecognizedTotal > %d\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsRxPortTLVsUnrecognizedTotal);
        printf
            ("port-%02d.Rx.AgeoutsTotal          > %d\n\n",
             riter->lldpStatsPortNum,
             riter->lldpStatsRxPortAgeoutsTotal);
    }

    return LLDP_OK;
}

/*****************************************************************************
 * Debug
 *****************************************************************************/

int lldpSetLogLevel(int logLevel)
{
    lldpLogLevel = logLevel;
    LLDP_INFO("LLDP: Log Level Changed (logLevel:%d).\n", logLevel);
    return LLDP_OK;
}

