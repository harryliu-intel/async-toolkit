/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_proto_lldp.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP Fulcrum Interface.
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

#include <fm_sdk.h>
#include <platform_buffer_defs.h>

#include <lldp.h>
#include <fm_proto_lldp.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define FM_LLDP_VALID_SW(sw_) \
    ((sw_) >= 0 && (sw_) < FM_LLDP_MAX_SYSTEM_SWITCHES)

#define FM_LLDP_VALID_PORT(port_) \
    ((port_) >= 0 && (port_) < FM_LLDP_MAX_SWITCH_PORTS)

#define FM_LLDP_VALID_PORT_NUM(portNum_) \
    ((portNum_) >= 0 && (portNum_) < FM_LLDP_MAX_PORTS)

#define VERIFY_LLDP_IS_RUNNING() \
    do { \
        if (!fmLldpIsRunning()) { \
            FM_LOG_PRINT("LLDP module is not initialized. run 'lldp start' to initialize.\n"); \
            return FM_FAIL; \
        } \
    } while (0)

typedef struct
{
     fm_int sw[FM_LLDP_MAX_PORTS];
     fm_int port[FM_LLDP_MAX_PORTS];
     fm_int portNum[FM_LLDP_MAX_SYSTEM_SWITCHES][FM_LLDP_MAX_SWITCH_PORTS];
} fm_lldpPortMapping;


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static int fmLldpSendEgressPacket(int portNum, uint8_t *buf, size_t size);
static int fmLldpGetLinkStatus(int portNum, bool *linkStatus);

fm_status fmLldpAddPort(fm_int sw, fm_int port, fm_int portNum, char *portIfName, char *portDesc);
fm_status fmLldpRemovePort(fm_int portNum);


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*  The lldpPhysicalIf interface is implemented by low level physical layers, 
    which provides LLDP interface for sending packets and query the ports
    status. The fmLldpPhysicalInterface implements the lldpPhysicalIf for
    Fulcrum hardware. */
static struct lldpPhysicalIf fmLldpPhysicalInterface = {
    /* Send egress packet to physical port (sendEgressPacket). */
    fmLldpSendEgressPacket,

    /* Get port link status (getLinkStatus). */
    fmLldpGetLinkStatus
};

static fm_lldpPortMapping lldpPm;


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmLldpPortMappingInitialize
 * \ingroup lldp
 *
 * \desc            Initializes Fulcrum sw/port pair to LLDP port mapping.
 *
 * \return          FM_OK always.
 *
 *****************************************************************************/
static fm_status fmLldpPortMappingInitialize()
{
    fm_int sw, port, portNum;

    /* initialize portNum to sw/port mapping */
    for (portNum=0; portNum<FM_LLDP_MAX_PORTS; portNum++) {
        lldpPm.sw[portNum] = -1;
        lldpPm.port[portNum] = -1;
    }

    /* initialize sw/port to portNum mapping */
    for (sw=0; sw<FM_LLDP_MAX_SYSTEM_SWITCHES; sw++) {
        for (port=0; port<FM_LLDP_MAX_SWITCH_PORTS; port++)
        lldpPm.portNum[sw][port] = -1;
    }

    return FM_OK;
}

/*****************************************************************************/
/** fmLldpSetPortMapping
 * \ingroup lldp
 *
 * \desc            Maps Fulcrum sw/port pair to LLDP port number.
 *
 * \param[in]       sw is the hardware platform switch number.
 *
 * \param[in]       port is the hardware switch port number.
 *
 * \param[in]       portNum is the LLDP port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if ''sw'' is invalid.
 * \return          FM_ERR_INVALID_PORT if ''port'' is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if ''portNum'' is invalid.
 *
 *****************************************************************************/
static fm_status fmLldpSetPortMapping(fm_int sw, fm_int port, fm_int portNum)
{
    if (!FM_LLDP_VALID_SW(sw))
        return FM_ERR_INVALID_SWITCH;

    if (!FM_LLDP_VALID_PORT(port))
        return FM_ERR_INVALID_PORT;

    if (!FM_LLDP_VALID_PORT_NUM(portNum))
        return FM_ERR_INVALID_ARGUMENT;

    lldpPm.sw[portNum] = sw;
    lldpPm.port[portNum] = port;
    lldpPm.portNum[sw][port] = portNum;

    return FM_OK;
}

/*****************************************************************************/
/** fmLldpResetPortMapping
 * \ingroup lldp
 *
 * \desc            Unmaps Fulcrum sw/port pair to LLDP port number.
 *
 * \param[in]       portNum is the LLDP port number to unmap.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_ARGUMENT if ''portNum'' is invalid.
 *
 *****************************************************************************/
static fm_status fmLldpResetPortMapping(fm_int portNum)
{
    fm_int sw, port;

    sw = lldpPm.sw[portNum];
    port = lldpPm.port[portNum];

    if (FM_LLDP_VALID_SW(sw) && FM_LLDP_VALID_PORT(port))
        lldpPm.portNum[sw][port] = -1;

    lldpPm.sw[portNum] = -1;
    lldpPm.port[portNum] = -1;

    return FM_OK;
}

/*****************************************************************************/
/** fmLldpGetPortNum
 * \ingroup lldp
 *
 * \desc            Gets LLDP port number from Fulcrum switch/port pair.
 *
 * \param[in]       sw is the hardware platform switch number.
 *
 * \param[in]       port is the hardware switch port number.
 *
 * \return          LLDP port number if successful.
 * \return          -1 on failure.
 *
 *****************************************************************************/
static fm_int fmLldpGetPortNum(fm_int sw, fm_int port)
{
    return (FM_LLDP_VALID_SW(sw) && FM_LLDP_VALID_PORT(port)) ?
        lldpPm.portNum[sw][port] : -1;
}

/*****************************************************************************/
/** fmLldpGetSwitch
 * \ingroup lldp
 *
 * \desc            Gets Fulcrum switch from LLDP port number.
 *
 * \param[in]       portNum is the LLDP port number.
 *
 * \return          Fulcrum switch number if successful.
 * \return          -1 on failure.
 *
 *****************************************************************************/
static fm_int fmLldpGetSwitch(fm_int portNum)
{
    return (FM_LLDP_VALID_PORT_NUM(portNum)) ?
        lldpPm.sw[portNum] : -1;
}

/*****************************************************************************/
/** fmLldpGetPort
 * \ingroup lldp
 *
 * \desc            Gets Fulcrum port in switch from LLDP port number.
 *
 * \param[in]       portNum is the LLDP port number.
 *
 * \return          Fulcrum port in switch number if successful.
 * \return          -1 on failure.
 *
 *****************************************************************************/
static fm_int fmLldpGetPort(fm_int portNum)
{
    return (FM_LLDP_VALID_PORT_NUM(portNum)) ?
        lldpPm.port[portNum] : -1;
}

/*****************************************************************************/
/** fmLldpSendEgressPacket
 * \ingroup lldp
 *
 * \desc            Implements physical layer send egress packet interface. This
 *                  function forwards a packet to physical layer port.
 *
 * \param[in]       portNum is the LLDP port number from which the packet should
 *                  be transmitted.
 *
 * \param[in]       buf is the egress packet buffer to be transmitted.
 *
 * \param[in]       size is the egress packet size (not including CRC).
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
static int fmLldpSendEgressPacket(int portNum, uint8_t *buf, size_t size)
{
    fm_int     i;
    fm_int     sw, port;
    fm_buffer *pkt;
    fm_status  status;

    sw = fmLldpGetSwitch(portNum);
    port = fmLldpGetPort(portNum);

    if ((pkt = fmAllocateBuffer(sw)) == NULL)
        return LLDP_ERR_NO_MEMORY;

    for (i=0; i<(int)size && i<FM_BUFFER_SIZE_BYTES; i++)
        ((uint8_t *)pkt->data)[i] = buf[i];

    for (; i<(int)size + 4 && i<FM_BUFFER_SIZE_BYTES; i++)
        ((uint8_t *)pkt->data)[i] = 0;

    pkt->len  = size;
    pkt->next = NULL;

    if ((status = fmSendPacketDirected(sw, &port, 1, pkt)) != FM_OK) {
        LLDP_ERR("Error sending directed packet to hardware"
                 " (fmSendPacketDirected:%s sw:%d port:%d len:%d).",
                 fmErrorMsg(status), sw, port, pkt->len);
        fmFreeBufferChain(sw, pkt);
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** fmLldpGetLinkStatus
 * \ingroup lldp
 *
 * \desc            Implements physical layer get link-status interface. This
 *                  function gets the status of the phyiscal port.
 *
 * \param[in]       portNum is the LLDP port number from which the packet should
 *                  be transmitted.
 *
 * \param[in]       linkStatus points to caller-allocated storage where this
 *                  function should place the port link-status (0=down, 1=up).
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
static int fmLldpGetLinkStatus(int portNum, bool *linkStatus)
{
    fm_int sw, port;
    fm_int mode, state, info[4];

    sw = fmLldpGetSwitch(portNum);
    port = fmLldpGetPort(portNum);

    /* get port link status */
    if (fmGetPortState(sw, port, &mode, &state, &info[0]) != FM_OK) {
        *linkStatus = 0;
        return LLDP_ERR_UNKNOWN;
    }

    *linkStatus = (state == FM_PORT_STATE_UP);
    return LLDP_OK;
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/**
 * \desc        Handles an LLDP packet.
 *
 * \param[in]   event the packet receive event associated with the LLDP packet
 *              to be handled.
 *
 * \return      TRUE if the LLDP packet has been handled successfully.
 * \return      FALSE otherwise.
 *
 *****************************************************************************/
fm_status fmPacketHandleLLDP(fm_eventPktRecv *event)
{
    int portNum = fmLldpGetPortNum(event->switchNum, event->srcPort);
    fm_buffer *buf = (fm_buffer *)event->pkt;

    return lldpHandleIngressPacket(portNum, 
               (uint8_t *)(buf->data), buf->len) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpInitialize
 * \ingroup lldp
 *
 * \desc            Initializes LLDP manager, by creating the LLDP data-base,
 *                  and starting the LLDP state-machine thread.
 *
 * \param[in]       chassisMacAddress is local chassis mac address that is used
 *                  as source address for sending outgoing LLDP messages.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpInitialize(fm_macaddr chassisMacAddress)
{
	fmLldpPortMappingInitialize();
 
    return lldpInitialize((uint64_t)chassisMacAddress, &fmLldpPhysicalInterface) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpTerminate
 * \ingroup lldp
 *
 * \desc            Terminates LLDP manager, by stopping the LLDP state-machine
 *                  thread and destroying the LLDP data-base.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpTerminate()
{
    fm_int portNum;

    if (!fmLldpIsRunning())
        return FM_FAIL;

    /* remove all LLDP ports */
    for (portNum=0; portNum<FM_LLDP_MAX_PORTS; portNum++) {
        if (lldpPm.sw[portNum] != -1)
            fmLldpRemovePort(portNum);
    }

    return lldpTerminate() == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpAddPort
 * \ingroup lldp
 *
 * \desc            Add port to LLDP.
 *
 * \param[in]       sw is the hardware platform switch number.
 *
 * \param[in]       port is the hardware switch port number.
 *
 * \param[in]       portNum is the local port number to be added.
 *
 * \param[in]       portIfName is the port-interface alias string.
 *
 * \param[in]       portDesc is a pointer to the port-description string.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpAddPort(fm_int sw, fm_int port, fm_int portNum, char *portIfName, char *portDesc)
{
    fm_status status;

    if (!fmLldpIsRunning())
        return FM_FAIL;

    /* set port mapping */
    if ((status = fmLldpSetPortMapping(sw, port, portNum)) != FM_OK)
        return status;

    /* add LLDP port */
    if ((lldpAddPort(portNum, LLDP_PORT_ID_INTERFACE_ALIAS, portIfName, strlen(portIfName), portDesc) != LLDP_OK))
        return FM_FAIL;

    return FM_OK;
}

/*****************************************************************************/
/** fmLldpRemovePort
 * \ingroup lldp
 *
 * \desc            Remove port from LLDP, that was added with fmLldpAddPort().
 *
 * \param[in]       portNum is the local port number to be removed.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpRemovePort(fm_int portNum)
{
    fm_status status;

    if (!fmLldpIsRunning())
        return FM_FAIL;

    /* reset port mapping */
    if ((status = fmLldpResetPortMapping(portNum)) != FM_OK)
        return status;

    /* remove LLDP port */
    if ((lldpRemovePort(portNum) != LLDP_OK))
        return FM_FAIL;

    return FM_OK;
}

/*****************************************************************************/
/** fmLldpStart
 * \ingroup lldp
 *
 * \desc            Initializes and starts LLDP manager on all ports.
 *
 * \param[in]       chassisMacAddress is local chassis mac address that is used
 *                  as source address for sending outgoing LLDP messages.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpStart(fm_macaddr chassisMacAddress)
{
    fm_status     status;
    fm_bool       attr;
    fm_int        sw;
    fm_int        port;
    fm_switchInfo info;
    fm_int        cpuPort;
    fm_int        portNum = 1;

	if ((status = fmLldpInitialize(chassisMacAddress)) != FM_OK) {
        FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "LLDP: Error initializing LLDP module.\n");
        return status;
    }

    /* get first switch */
    fmGetSwitchFirst(&sw);

    while (sw != -1) {
        /* get switch info (number of ports, ...) */
        if ((status = fmGetSwitchInfo(sw, &info)) != FM_OK) {
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "LLDP: Error getting switch:%d info.\n", sw);
            return status;
        }

        /* get cpu port id */
        if ((status = fmGetCpuPort(sw, &cpuPort)) != FM_OK) {
            FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "LLDP: Error getting switch:%d cpu-port.\n", sw);
            return status;
        }

        /* initialize ports */
        for (port = 0; port < info.numPorts; port++) {
            if (port != cpuPort &&
                fmGetPortAttribute(sw, port, FM_PORT_INTERNAL, &attr) == FM_OK &&
                attr == 0) {
                char portId[256];
                char portDesc[256];

                snprintf(portId, sizeof(portId), "port%d", portNum);
                snprintf(portDesc, sizeof(portDesc), "Fulcrum Port %d/%d", sw, port);

                if (fmLldpAddPort(sw, port, portNum, portId, portDesc) != FM_OK)
                {    
                    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "LLDP: Error adding switch:%d / port:%d as LLDP portNum:%d\n", sw, port, portNum);
                }
                else
                {    
                    FM_LOG_DEBUG(FM_LOG_CAT_DEFAULT, "LLDP: Added switch:%d / port:%d as LLDP portNum:%d\n", sw, port, portNum);
                }

                portNum++;
            }
        }

        /* get next switch */
        fmGetSwitchNext(sw, &sw);
    }

    return FM_OK;
}

/*****************************************************************************/
/** fmLldpStop
 * \ingroup lldp
 *
 * \desc            Stops and destroys LLDP manager on all ports.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpStop()
{
    return fmLldpTerminate();
}

/*****************************************************************************/
/** fmLldpIsRunning
 * \ingroup lldp
 *
 * \desc            Checks if LLDP manager is running.
 *
 * \return          TRUE if running.
 * \return          FALSE if not-running.
 *
 *****************************************************************************/
fm_bool fmLldpIsRunning()
{
    return (lldpDB != NULL);
}

/*****************************************************************************/
/** fmLldpPortIsRunning
 * \ingroup lldp
 *
 * \desc            Checks if LLDP port is running.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \return          TRUE if running.
 * \return          FALSE if not-running.
 *
 *****************************************************************************/
fm_bool fmLldpPortIsRunning(fm_int portNum)
{
    return (lldpPm.sw[portNum] != -1);
}


/*****************************************************************************
 * Management
 *****************************************************************************/

/*****************************************************************************/
/** fmLldpSetTxTiming
 * \ingroup lldp
 *
 * \desc            Set the LLDP state-machine TX timing parmeters.
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
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetTxTiming(int msgTxInterval, int msgTxHold, int reinitDelay, int txDelay)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetTxTiming(msgTxInterval, msgTxHold, reinitDelay, txDelay) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetChassisId
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
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetChassisId(int chassisIdSubtype, char *chassisId, size_t chassisIdLen)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetChassisId(chassisIdSubtype, chassisId, chassisIdLen) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetSystemName
 * \ingroup lldp
 *
 * \desc            Set local system-name.
 *
 * \param[in]       sysName is the system-name to be set.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetSystemName(char *sysName)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetSystemName(sysName) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetSystemDescription
 * \ingroup lldp
 *
 * \desc            Set local system-description.
 *
 * \param[in]       sysDesc is the system-description to be set.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetSystemDescription(char *sysDesc)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetSystemDescription(sysDesc) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetSystemCapabilities
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
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetSystemCapabilities(int sysCapSupported, int sysCapEnabled)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetSystemCapabilities(sysCapSupported, sysCapEnabled) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetPortAdminStatus
 * \ingroup lldp
 *
 * \desc            Set local port admin-status.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       adminStatus is the port admin status (one of
 *                  lldpAdminStatus enum).
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetPortAdminStatus(int portNum, int adminStatus)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetPortAdminStatus(portNum, adminStatus) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetTLVsTxEnable
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
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetTLVsTxEnable(int portNum, int tlvsTxMask, int tlvsTxEnable)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetTLVsTxEnable(portNum, tlvsTxMask, tlvsTxEnable) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetPortId
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
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetPortId(int portNum, int portIdSubtype, char *portId, size_t portIdLen)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetPortId(portNum, portIdSubtype, portId, portIdLen) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpSetPortDesc
 * \ingroup lldp
 *
 * \desc            Set local port-description.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       portDesc is the port description to be set.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
int fmLldpSetPortDesc(int portNum, char *portDesc)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpSetPortDesc(portNum, portDesc) == LLDP_OK ? FM_OK : FM_FAIL;
}


/*****************************************************************************
 * Status
 *****************************************************************************/

/*****************************************************************************/
/** fmLldpShowConfiguration
 * \ingroup lldp
 *
 * \desc            Dumps LLDP configuration information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpShowConfiguration(int portNum)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpShowConfiguration(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpShowLocalData
 * \ingroup lldp
 *
 * \desc            Dumps LLDP local-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpShowLocalData(int portNum)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpShowLocalData(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpShowRemoteData
 * \ingroup lldp
 *
 * \desc            Dumps LLDP remote-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpShowRemoteData(int portNum)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpShowRemoteData(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}

/*****************************************************************************/
/** fmLldpShowCounters
 * \ingroup lldp
 *
 * \desc            Dumps LLDP counters information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpShowCounters(int portNum)
{
    VERIFY_LLDP_IS_RUNNING();
    return lldpShowCounters(portNum) == LLDP_OK ? FM_OK : FM_FAIL;
}


/*****************************************************************************
 * Debug
 *****************************************************************************/

/*****************************************************************************/
/** fmLldpSetLogLevel
 * \ingroup lldp
 *
 * \desc            Change LLDP logging level.
 *
 * \param[in]       logLevel is the log-level to set.
 *                  0 - No Logging
 *                  1 - Error
 *                  2 - Warning
 *                  3 - Info
 *                  4 - Debug
 *
 * \return          FM_OK if successful.
 * \return          FM_FAIL on failure.
 *
 *****************************************************************************/
fm_status fmLldpSetLogLevel(int logLevel)
{
    return lldpSetLogLevel(logLevel) == LLDP_OK ? FM_OK : FM_FAIL;
}
