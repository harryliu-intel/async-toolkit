/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_tx.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP TX.
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

#include <stdlib.h>

#include <lldp.h>
#include <lldp_mib.h>
#include <lldp_int.h>
#include <lldp_tlv.h>
#include <lldp_event.h>

#include <lldp_tx.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

/* LLDP direct-encoded packet address */
static const uint8_t LLDP_DST_ADDRESS[] = {
    0x01, 0x80, 0xC2, 0x00, 0x00, 0x0E
};

/* LLDP direct-encoded packet address */
static const uint8_t LLDP_DIRECT_ENCODED[] = {
    0x88, 0xCC
};

/* LLDP SNAP-encoded packet address */
static const uint8_t LLDP_SNAP_ENCODED[] = {
    0xAA, 0xAA, 0x03, 0x00, 0x00, 0x00, 0x88, 0xCC
};

#define STAGGER_TRANSMISSIONS 1

#define DIRECT_ENCODED_HEADER_LEN \
    (sizeof(LLDP_DST_ADDRESS) * 2 + sizeof(LLDP_DIRECT_ENCODED))

#define SNAP_ENCODED_HEADER_LEN \
    (sizeof(LLDP_DST_ADDRESS) * 2 + sizeof(LLDP_SNAP_ENCODED))

#define ENCODED_HEADER_LEN(snapEnabled_) \
    ((snapEnabled_) ? SNAP_ENCODED_HEADER_LEN : DIRECT_ENCODED_HEADER_LEN)

#define TX_CHANGE_STATE(state_) \
    do { \
        locPort->txStateLast = locPort->txState; \
        locPort->txState = state_; \
    } while (0)

#define TX_NO_CHANGE_STATE() \
    do { \
        locPort->txStateLast = locPort->txState; \
    } while (0)

#define TX_UPON_STATE_ENTRY() \
    (locPort->txStateLast != locPort->txState)

#define TLV_APPEND(CHAIN_BASE, CHAIN_LAST, TLV) \
    do { \
        if (CHAIN_BASE == NULL) { \
            (TLV)->next = NULL; \
            (CHAIN_BASE) = (CHAIN_LAST) = (TLV); \
        } \
        else { \
            (CHAIN_LAST)->next = (TLV); \
            (CHAIN_LAST) = (TLV); \
        } \
    } while (0)

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** lldpTxFrameInitialization
 * \ingroup lldp
 *
 * \desc            Initializes header of TX LLDPDU packet.
 *
 * \param[in]       buf is a pointer to ingress packet data.
 *
 * \param[in]       size is the number of bytes in the packet data buffer.
 *
 * \param[in]       snapEncoded indicates if this packet should be snap encoded
 *                  or direct encoded.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
static int lldpTxFrameInitialization(uint8_t *buf, size_t size, 
                                     int snapEncoded)
{
    size_t i;

    if (!buf || size < 16) 
        return LLDP_ERR_INVALID_PACKET;

    /* append LLDP multicast-address */
    for (i=0; i<sizeof(LLDP_DST_ADDRESS); i++)
        buf[i] = LLDP_DST_ADDRESS[i];

    /* append local chassis MAC address */
    for (i=0; i<6; i++)
        buf[i+6] = (lldpDB->lldpConfiguration.chassisMacAddress >> (40 - i*8)) & 0xff;

    if (snapEncoded) {
        /* append snap-encoded Ethernet-type */
        for (i=0; i<sizeof(LLDP_SNAP_ENCODED); i++)
            buf[i+12] = LLDP_SNAP_ENCODED[i];
    }
    else {
        /* append direct-encoded Ethernet-type */
        for (i=0; i<sizeof(LLDP_DIRECT_ENCODED); i++)
            buf[i+12] = LLDP_DIRECT_ENCODED[i];
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** lldpTxCleanupLocalPort
 * \ingroup lldp
 *
 * \desc            Cleans-up local port somethingChangedLocal indication.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK always.
 * \return          LLDP_ERR_NOT_FOUND if local port does not exists.
 *
 *****************************************************************************/
static int lldpTxCleanupLocalPort(int portNum)
{
    struct lldpLocPort *locPort = NULL;

    /* get local port entry */
    locPort = lldpLocPortGet(portNum);

    if (!locPort) {
        return LLDP_ERR_NOT_FOUND;
    }

    locPort->somethingChangedLocal = 0;

    return LLDP_OK;

}   /* end lldpTxCleanupLocalPort */

/*****************************************************************************/
/** lldpTxSendLLDPDU
 * \ingroup lldp
 *
 * \desc            Create a LLDPDU (based on database) and send it to
 *                  physical layer to be transmitted. This function also calls
 *                  the protocol layer allowing it to add organizationally TLVs
 *                  to the outgoing LLDPDU packet.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NOT_FOUND if local port does not exists.
 * \return          LLDP_ERR_INVALID_ARG if data is invalid.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 *
 *****************************************************************************/
static int lldpTxSendLLDPDU(int portNum)
{
    int                    status = LLDP_OK;

    int                    len = 0;
    uint8_t                buf[LLDP_MAX_PACKET_SIZE];

    struct lldpTlv        *tlv;
    struct lldpTlv        *tlvLast = NULL;
    struct lldpTlv        *tlvChain = NULL;
    int                    tlvStrLen;

    struct lldpLocPort    *locPort = NULL;
    struct lldpPortConfig *portConfig = NULL;
    struct lldpStatsPort  *statsPort = NULL;

    /* get port entries */
    locPort    = lldpLocPortGet(portNum);
    portConfig = lldpPortConfigGet(portNum);
    statsPort  = lldpStatsPortGet(portNum);

    if (!locPort || !portConfig || !statsPort) {
        status = LLDP_ERR_NOT_FOUND;
        goto ABORT;
    }

    /* append chassis id TLV */
    status = lldpEncodeChassisTlv(&tlv, lldpDB->lldpLocalSystemData.lldpLocChassisIdSubtype,
                                        lldpDB->lldpLocalSystemData.lldpLocChassisId,
                                        lldpDB->lldpLocalSystemData.lldpLocChassisIdLen);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* append port id TLV */
    status = lldpEncodePortTlv(&tlv, locPort->lldpLocPortIdSubtype,
                                     locPort->lldpLocPortId,
                                     locPort->lldpLocPortIdLen);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* append TTL id TLV */
    status = lldpEncodeTtlTlv(&tlv, locPort->txTTL);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* append port description TLV */
    if (locPort->lldpLocPortDesc && 
        (portConfig->lldpPortConfigTLVsTxEnable & LLDP_TLV_ENABLE_PORT_DESC)) {
        tlvStrLen = MIN(strlen(locPort->lldpLocPortDesc), 256);      
        status = lldpEncodeGenericStringTlv(&tlv, LLDP_TLV_TYPE_PORT_DESCRIPTION, 
                                            locPort->lldpLocPortDesc, tlvStrLen);
        if (status != LLDP_OK) goto ABORT;
        TLV_APPEND(tlvChain, tlvLast, tlv);
    }

    /* append system name TLV */
    if (lldpDB->lldpLocalSystemData.lldpLocSysName && 
        (portConfig->lldpPortConfigTLVsTxEnable & LLDP_TLV_ENABLE_SYS_NAME)) {
        tlvStrLen = MIN(strlen(lldpDB->lldpLocalSystemData.lldpLocSysName), 256);
        status = lldpEncodeGenericStringTlv(&tlv, LLDP_TLV_TYPE_SYSTEM_NAME, 
                                            lldpDB->lldpLocalSystemData.lldpLocSysName, tlvStrLen);
        if (status != LLDP_OK) goto ABORT;
        TLV_APPEND(tlvChain, tlvLast, tlv);
    }

    /* append system description TLV */
    if (lldpDB->lldpLocalSystemData.lldpLocSysDesc && 
        (portConfig->lldpPortConfigTLVsTxEnable & LLDP_TLV_ENABLE_SYS_DESC)) {
        tlvStrLen = MIN(strlen(lldpDB->lldpLocalSystemData.lldpLocSysDesc), 256);
        status = lldpEncodeGenericStringTlv(&tlv, LLDP_TLV_TYPE_SYSTEM_DESCRIPTION, 
                                            lldpDB->lldpLocalSystemData.lldpLocSysDesc, tlvStrLen);
        if (status != LLDP_OK) goto ABORT;
        TLV_APPEND(tlvChain, tlvLast, tlv);
    }

    /* append system capabilities TLV */
    if (lldpDB->lldpLocalSystemData.lldpLocSysCapSupported && 
        (portConfig->lldpPortConfigTLVsTxEnable & LLDP_TLV_ENABLE_SYS_CAP)) {
        status = lldpEncodeSystemCapTlv(&tlv, lldpDB->lldpLocalSystemData.lldpLocSysCapSupported, 
                                              lldpDB->lldpLocalSystemData.lldpLocSysCapEnabled);
        if (status != LLDP_OK) goto ABORT;
        TLV_APPEND(tlvChain, tlvLast, tlv);
    }

    /* append End-of-LLDPDU TLV */
    status = lldpEncodeEndTlv(&tlv);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* send LLDP to DCBX module before we send it */
    LLDP_PROTOCOL(appendEgressTLVs, portNum, tlvChain);

    /* send LLDP packet to physical layer */
    if (lldpDB->physical &&
        lldpDB->physical->sendEgressPacket) {
 
        status = lldpTxFrameInitialization(buf, sizeof(buf), 0);
        if (status != LLDP_OK) goto ABORT;

        status = lldpEncode(buf, sizeof(buf), ENCODED_HEADER_LEN(0), tlvChain, &len);
        if (status != LLDP_OK) goto ABORT;

        status = lldpDB->physical->sendEgressPacket(portNum, buf, len);
        if (status != LLDP_OK) goto ABORT;
    }

    statsPort->lldpStatsTxPortFramesTotal++;

    LLDP_DBG("LLDP: Sent LLDPDU (portNum:%d).\n", portNum);

ABORT:
    if (tlvChain)
        lldpFreeTlvChain(tlvChain);

    return status;

}   /* end lldpTxSendLLDPDU */

/*****************************************************************************/
/** lldpTxSendShutdownLLDPDU
 * \ingroup lldp
 *
 * \desc            Create a shutdown LLDPDU and send it to physical layer to
 *                  be transmitted.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_NOT_FOUND if local port does not exists.
 * \return          LLDP_ERR_INVALID_ARG if data is invalid.
 * \return          LLDP_ERR_NO_MEMORY if no memory is available.
 *
 *****************************************************************************/
static int lldpTxSendShutdownLLDPDU(int portNum)
{
    int               status = LLDP_OK;

    int               len = 0;
    uint8_t           buf[LLDP_MAX_PACKET_SIZE];

    struct lldpTlv   *tlv;
    struct lldpTlv   *tlvLast = NULL;
    struct lldpTlv   *tlvChain = NULL;

    struct lldpLocPort   *locPort = NULL;
    struct lldpStatsPort *statsPort = NULL;

    /* get port entries */
    locPort   = lldpLocPortGet(portNum);
    statsPort = lldpStatsPortGet(portNum);

    if (!locPort || !statsPort) {
        status = LLDP_ERR_NOT_FOUND;
        goto ABORT;
    }

    /* append chassis id TLV */
    status = lldpEncodeChassisTlv(&tlv, lldpDB->lldpLocalSystemData.lldpLocChassisIdSubtype,
                                        lldpDB->lldpLocalSystemData.lldpLocChassisId,
                                        lldpDB->lldpLocalSystemData.lldpLocChassisIdLen);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* append port id TLV */
    status = lldpEncodePortTlv(&tlv, locPort->lldpLocPortIdSubtype,
                                     locPort->lldpLocPortId,
                                     locPort->lldpLocPortIdLen);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* append TTL id TLV */
    status = lldpEncodeTtlTlv(&tlv, 0);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* append End-of-LLDPDU TLV */
    status = lldpEncodeEndTlv(&tlv);
    if (status != LLDP_OK) goto ABORT;
    TLV_APPEND(tlvChain, tlvLast, tlv);

    /* send LLDP packet to physical layer */
    if (lldpDB->physical &&
        lldpDB->physical->sendEgressPacket) {
        
        status = lldpTxFrameInitialization(buf, sizeof(buf), 0);
        if (status != LLDP_OK) goto ABORT;

        status = lldpEncode(buf, sizeof(buf), ENCODED_HEADER_LEN(0), tlvChain, &len);
        if (status != LLDP_OK) goto ABORT;

        status = lldpDB->physical->sendEgressPacket(portNum, buf, len);
        if (status != LLDP_OK) goto ABORT;
    }

    statsPort->lldpStatsTxPortFramesTotal++;

ABORT:
    if (tlvChain)
        lldpFreeTlvChain(tlvChain);

    return status;

}   /* end lldpTxSendShutdownLLDPDU */

/*****************************************************************************/
/** lldpTxStateMachineCycle
 * \ingroup lldp
 *
 * \desc            Cycle TX state machine, until no state-change is detected.
 *
 * \param[in]       portNum is the local port to run it's state-machine.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_xxxxx on failure.
 *
 *****************************************************************************/
static int lldpTxStateMachineCycle(int portNum)
{
    int               status = LLDP_OK;
    uint32_t          now;

    struct lldpLocPort    *locPort = NULL;
    struct lldpPortConfig *portConfig = NULL;
    struct lldpStatsPort  *statsPort = NULL;

    /* get current time */
    now = (uint32_t)time(NULL);

    /* get local/remote port entries */
    locPort     = lldpLocPortGet(portNum);
    portConfig  = lldpPortConfigGet(portNum);
    statsPort   = lldpStatsPortGet(portNum);

    /* verify valid entries found */
    if (!locPort || !portConfig || !statsPort) {
        status = LLDP_ERR_NOT_FOUND;
        goto ABORT;
    }

    do {
        switch (locPort->txState) {
        default:
            {
                TX_CHANGE_STATE(LLDP_TX_STATE_TX_LLDP_INITIALIZE);
            }
            break;

        case LLDP_TX_STATE_TX_LLDP_INITIALIZE:
            {
                if (TX_UPON_STATE_ENTRY()) {
                    lldpTxCleanupLocalPort(portNum);
                    LLDP_DBG("LLDP: TX State -> LLDP_TX_STATE_TX_LLDP_INITIALIZE (portNum:%d)\n", portNum);
                }

                if ((portConfig->lldpPortConfigAdminStatus == LLDP_ADMIN_TX_ONLY ||
                     portConfig->lldpPortConfigAdminStatus == LLDP_ADMIN_TX_AND_RX) && portConfig->lldpPortConfigEnabled)
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_IDLE);
                else
                    TX_NO_CHANGE_STATE();
            }
            break;

        case LLDP_TX_STATE_TX_IDLE:
            {
                if (TX_UPON_STATE_ENTRY()) {
                    locPort->txTTL = MIN(65535, lldpDB->lldpConfiguration.lldpMessageTxInterval * lldpDB->lldpConfiguration.lldpMessageTxHoldMultiplier);
#if (STAGGER_TRANSMISSIONS)
                    /* When stagger-transmission is enabled we initialize the first tx timer based on
                     * the port number. */
                    if (locPort->txStateLast == LLDP_TX_STATE_TX_LLDP_INITIALIZE)
                        locPort->txTTR = now + (portNum * 3 % lldpDB->lldpConfiguration.lldpMessageTxInterval);
                    else
                        locPort->txTTR = now + lldpDB->lldpConfiguration.lldpMessageTxInterval;
#else
                    locPort->txTTR = now + lldpDB->lldpConfiguration.lldpMessageTxInterval;
#endif
                    locPort->somethingChangedLocal = 0;
                    locPort->txDelayWhile = now + lldpDB->lldpConfiguration.lldpTxDelay;
                    LLDP_DBG("LLDP: TX State -> LLDP_TX_STATE_TX_IDLE (txTTL:%d txTTR:%d txDelayWhile:%d portNum:%d)\n", 
                           locPort->txTTL, locPort->txTTR - now, locPort->txDelayWhile - now, portNum);
                }

                if (!portConfig->lldpPortConfigEnabled)
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_LLDP_INITIALIZE);
                else if (portConfig->lldpPortConfigAdminStatus != LLDP_ADMIN_TX_ONLY &&
                         portConfig->lldpPortConfigAdminStatus != LLDP_ADMIN_TX_AND_RX)
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_SHUTDOWN_FRAME);
                else if ((locPort->txDelayWhile <= now) && 
                         (locPort->txTTR <= now || locPort->somethingChangedLocal))
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_INFO_FRAME);
                else
                    TX_NO_CHANGE_STATE();
            }
            break;

        case LLDP_TX_STATE_TX_INFO_FRAME:
            {
                if (TX_UPON_STATE_ENTRY()) {
                    lldpTxSendLLDPDU(portNum);
                    LLDP_DBG("LLDP: TX State -> LLDP_TX_STATE_TX_INFO_FRAME (portNum:%d)\n", portNum);
                }

                if (!portConfig->lldpPortConfigEnabled)
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_LLDP_INITIALIZE);
                else
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_IDLE);
            }
            break;

        case LLDP_TX_STATE_TX_SHUTDOWN_FRAME:
            {
                if (TX_UPON_STATE_ENTRY()) {
                    lldpTxSendShutdownLLDPDU(portNum);
                    locPort->txShutdownWhile = now + lldpDB->lldpConfiguration.lldpReinitDelay;
                    LLDP_DBG("LLDP: TX State -> LLDP_TX_STATE_TX_SHUTDOWN_FRAME (portNum:%d)\n", portNum);
                }

                if (!portConfig->lldpPortConfigEnabled)
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_LLDP_INITIALIZE);
                else if (locPort->txShutdownWhile <= now)
                    TX_CHANGE_STATE(LLDP_TX_STATE_TX_LLDP_INITIALIZE);
                else
                    TX_NO_CHANGE_STATE();
            }
            break;
        }
    }
    while (TX_UPON_STATE_ENTRY());

ABORT:

    return status;

}   /* end lldpTxStateMachine */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** lldpTxInitialize
 * \ingroup lldp
 *
 * \desc            Initializes LLDP TX module.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpTxInitialize()
{
    return LLDP_OK;

}   /* end lldpTxInitialize */

/*****************************************************************************/
/** lldpTxTerminate
 * \ingroup lldp
 *
 * \desc            Terminates LLDP TX module.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpTxTerminate()
{
    return LLDP_OK;

}   /* end lldpTxTerminate */

/*****************************************************************************/
/** lldpTxRun
 * \ingroup lldp
 *
 * \desc            Run TX state-machine for all ports.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpTxRun(lldp_event *event __attribute__((__unused__)))
{
    struct lldpLocPort *iter, *next;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpLocPort, node, &lldpDB->lldpLocalSystemData.lldpLocPortTable)
    {
        lldpTxStateMachineCycle(iter->lldpLocPortNum);
    }

    return LLDP_OK;

}   /* end lldpTxRun */

