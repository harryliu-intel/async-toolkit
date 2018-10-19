/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp_rx.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of LLDP RX.
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
#include <string.h>

#include <lldp.h>
#include <lldp_mib.h>
#include <lldp_int.h>
#include <lldp_tlv.h>
#include <lldp_event.h>

#include <lldp_rx.h>

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

#define DIRECT_ENCODED_HEADER_LEN \
    (sizeof(LLDP_DST_ADDRESS) * 2 + sizeof(LLDP_DIRECT_ENCODED))

#define SNAP_ENCODED_HEADER_LEN \
    (sizeof(LLDP_DST_ADDRESS) * 2 + sizeof(LLDP_SNAP_ENCODED))

#define ENCODED_HEADER_LEN(snapEnabled_) \
    ((snapEnabled_) ? SNAP_ENCODED_HEADER_LEN : DIRECT_ENCODED_HEADER_LEN)

#define RX_CHANGE_STATE(state_) \
    do { \
        locPort->rxStateLast = locPort->rxState; \
        locPort->rxState = state_; \
    } while (0)

#define RX_NO_CHANGE_STATE() \
    do { \
        locPort->rxStateLast = locPort->rxState; \
    } while (0)

#define RX_UPON_STATE_ENTRY() \
    (locPort->rxStateLast != locPort->rxState)

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** lldptRxFrameRecognition
 * \ingroup lldp
 *
 * \desc            Checks if the incoming packet is LLDP packet (direct
 *                  encoded or SNAP encoded).
 *
 * \param[in]       buf is the packet buffer.
 *
 * \param[in]       size is the packet buffer size.
 *
 * \param[in]       statsPort is the statistics entry of the local port.
 *
 * \param[out]      snapEncoded points to caller-allocated storage where this
 *                  function should place '1' if the packet is snap encoded, 
 *                  or '0' if the packet is direct encoded.
 *
 * \param[out]      remoteMac points to caller-allocated storage where this
 *                  function should place the src MAC address in packet.
 *
 * \return          LLDP_OK if packet is LLDP packet.
 * \return          LLDP_ERR_INVALID_PACKET if packet is invalid.
 * \return          LLDP_ERR_NOT_FOUND if packet is not LLDP packet.
 *
 *****************************************************************************/
static int lldptRxFrameRecognition(uint8_t *buf, size_t size, 
                                   struct lldpStatsPort *statsPort,                                            
                                   int *snapEncoded, uint64_t *remoteMac)
{
    size_t i;

    if (!buf || size < 16) {
        LLDP_ERR("LLDP: Got invalid Ethernet frame.\n");
        return LLDP_ERR_INVALID_PACKET;
    }

    /* check LLDP multicast-address */
    for (i=0; i<sizeof(LLDP_DST_ADDRESS); i++)
      if (buf[i] != LLDP_DST_ADDRESS[i]) break;

    if (sizeof(LLDP_DST_ADDRESS) != i) {
        LLDP_DBG("LLDP: Got none-LLDP frame (dst-address).\n");
        return LLDP_ERR_NOT_FOUND;
    }

    /* get remote mac address */
    *remoteMac = 0;
    for (i=0; i<sizeof(LLDP_DST_ADDRESS); i++)
        *remoteMac = (*remoteMac << 8) | buf[i+6];

    /* check Ethernet-address */
    for (i=0; i<sizeof(LLDP_DIRECT_ENCODED); i++)
      if (buf[i+12] != LLDP_DIRECT_ENCODED[i]) break;

    if (sizeof(LLDP_DIRECT_ENCODED) == i) {
        if (snapEncoded) *snapEncoded = 0;

        /* increment total frames received */
        if (statsPort) statsPort->lldpStatsRxPortFramesTotal++;

        LLDP_DBG("LLDP: Got direct-encoded LLDP frame.\n");

        return LLDP_OK;
    }

    /* check snap */
    for (i=0; i<sizeof(LLDP_SNAP_ENCODED); i++)
      if (buf[i+12] != LLDP_SNAP_ENCODED[i]) break;

    if (sizeof(LLDP_SNAP_ENCODED) == i) {
        if (snapEncoded) *snapEncoded = 1;

        /* increment total frames received */
        if (statsPort) statsPort->lldpStatsRxPortFramesTotal++;

        LLDP_DBG("LLDP: Got SNAP-encoded LLDP frame.\n");

        return LLDP_OK;
    }

    LLDP_DBG("LLDP: Got none-LLDP frame (ether-type).\n");

    return LLDP_ERR_NOT_FOUND;
}

/*****************************************************************************/
/** lldpRxFrameValidation
 * \ingroup lldp
 *
 * \desc            Validates LLDP packet TLVs format, and parse the mandatory 
 *                  TLVs (ChassisId / PortId / TTL).
 *
 * \param[in]       locPort is the local port entry on which the packet was 
 *                  received.
 *
 * \param[in]       statsPort is the statistics entry of the local port.
 *
 * \param[in]       tlvChain is the list of TLVs from the incoming packet.
 *
 * \param[out]      msap points to caller-allocated storage where this
 *                  function should place the packet chassisId / portId.
 *
 * \param[out]      rxTTL points to caller-allocated storage where this
 *                  function should place the packet rx-TTL.
 *
 * \return          LLDP_OK if packet TLVs are formatted correctly.
 * \return          LLDP_ERR_INVALID_FORMAT if packet TLVs are not formatted
 *                  correctly.
 *
 *****************************************************************************/
static int lldpRxFrameValidation(struct lldpLocPort *locPort __attribute__((__unused__)),
                                 struct lldpStatsPort *statsPort,
                                 struct lldpTlv *tlvChain, 
                                 struct lldpMsap *msap, 
                                 int *rxTTL)
{
    struct lldpTlv *tlv = tlvChain;

    int portDescTlvCnt = 0;
    int sysNameTlvCnt  = 0;
    int sysDescTlvCnt  = 0;
    int sysCapTlvCnt   = 0;

    /* 10.3.2 / a - validate that first TLV is present and it is valid chassis-id TLV */
    if (!tlv || tlv->type != LLDP_TLV_TYPE_CHASSIS_ID ||
        tlv->len < 2 || tlv->len > 256) {

        /* increment error counters */        
        statsPort->lldpStatsRxPortFramesDiscardedTotal++;
        statsPort->lldpStatsRxPortFramesErrors++;

        LLDP_ERR("LLDP: Got invalid LLDP frame (invalid chassis-id TLV).\n");
        return LLDP_ERR_INVALID_FORMAT;
    }
    else {
        lldpDecodeChassisTlv(tlv, &msap->lldpMsapChassisIdSubtype, msap->lldpMsapChassisId, &msap->lldpMsapChassisIdLen);
        tlv = tlv->next;
    }

    /* 10.3.2 / b - validate that second TLV is present and it is valid port-id TLV */
    if (!tlv || tlv->type != LLDP_TLV_TYPE_PORT_ID ||
        tlv->len < 2 || tlv->len > 256) {

        /* increment error counters */        
        statsPort->lldpStatsRxPortFramesDiscardedTotal++;
        statsPort->lldpStatsRxPortFramesErrors++;

        LLDP_ERR("LLDP: Got invalid LLDP frame (invalid port-id TLV).\n");
        return LLDP_ERR_INVALID_FORMAT;
    }
    else {
        lldpDecodePortTlv(tlv, &msap->lldpMsapPortIdSubtype, msap->lldpMsapPortId, &msap->lldpMsapPortIdLen);
        tlv = tlv->next;
    }

    /* 10.3.2 / c - validate that third TLV is present and it is valid time-to-live TLV */
    if (!tlv || tlv->type != LLDP_TLV_TYPE_TIME_TO_LIVE ||
        tlv->len < 2) {

        /* increment error counters */        
        statsPort->lldpStatsRxPortFramesDiscardedTotal++;
        statsPort->lldpStatsRxPortFramesErrors++;

        LLDP_ERR("LLDP: Got invalid LLDP frame (invalid ttl TLV).\n");
        return LLDP_ERR_INVALID_FORMAT;
    }
    else {
        lldpDecodeTtlTlv(tlv, rxTTL);
        tlv = tlv->next;        
    }

    /* 10.3.2 / c.3.i - shutdown LLDPDU, terminate validation */
    if (rxTTL == 0)
        return LLDP_OK;

    for (; tlv; tlv = tlv->next) {
        /* 10.3.2.1 / f - delete any TLV which is located after the end LLDPDU TLV */
        if (tlv->type == LLDP_TLV_TYPE_END_OF_LLDPDU &&
            tlv->next != NULL) {
            lldpFreeTlvChain(tlv->next);
            tlv->next = NULL;
        }

        /* 10.3.2.1 / a - check for duplicate chassis-id / port-id / ttl TLVs */
        if (tlv->type == LLDP_TLV_TYPE_CHASSIS_ID ||
            tlv->type == LLDP_TLV_TYPE_PORT_ID ||
            tlv->type == LLDP_TLV_TYPE_TIME_TO_LIVE) {

            /* increment error counters */        
            statsPort->lldpStatsRxPortFramesDiscardedTotal++;
            statsPort->lldpStatsRxPortFramesErrors++;

            LLDP_ERR("LLDP: Got invalid LLDP frame (duplicate %d TLV).\n", tlv->type);
            return LLDP_ERR_INVALID_FORMAT;
        }

        /* 10.3.2 / f - check for unknown TLV type */
        if (tlv->type > LLDP_TLV_TYPE_MANAGEMENT_ADDRESS &&
            tlv->type < LLDP_TLV_TYPE_ORGANIZATIONALLY_SPECIFIC) {
            statsPort->lldpStatsRxPortTLVsUnrecognizedTotal++;
        }

        /* 9.5.5.3, 9.5.6.3, 9.5.7.3, 9.5.8.3 - verify that some TLVs appear only once */
        if ((tlv->type == LLDP_TLV_TYPE_PORT_DESCRIPTION && portDescTlvCnt++ > 0) ||
            (tlv->type == LLDP_TLV_TYPE_SYSTEM_NAME && sysNameTlvCnt++ > 0) ||
            (tlv->type == LLDP_TLV_TYPE_SYSTEM_DESCRIPTION && sysDescTlvCnt++ > 0) ||
            (tlv->type == LLDP_TLV_TYPE_SYSTEM_CAPABILITIES && sysCapTlvCnt++ > 0)) {

            /* increment error counters */        
            statsPort->lldpStatsRxPortFramesDiscardedTotal++;
            statsPort->lldpStatsRxPortFramesErrors++;

            LLDP_ERR("LLDP: Got invalid LLDP frame (duplicate %d TLV).\n", tlv->type);
            return LLDP_ERR_INVALID_FORMAT;
        }
    }

    LLDP_DBG("LLDP: Got a valid LLDP frame.\n");

    return LLDP_OK;

} /* end lldpRxFrameValidation */

/*****************************************************************************/
/** lldpRxUpdateRemotePort
 * \ingroup lldp
 *
 * \desc            Add / modify remote entry data.
 *
 * \param[in]       locPort is the local port entry on which the packet was 
 *                  received.
 *
 * \param[in]       statsPort is the statistics entry of the local port.
 *
 * \param[in]       tlvChain is the list of TLVs from the incoming packet.
 *
 * \param[in]       msap is the packet ChassisId / PortId.
 *
 * \param[in]       remoteMac is the src MAC address in packet.
 *
 * \param[in]       rxTTL is the packet rx-TTL.
 *
 * \return          LLDP_OK if succesfull.
 * \return          LLDP_ERR_NO_MEMORY if too many neighbors.
 *
 *****************************************************************************/
static int lldpRxUpdateRemotePort(struct lldpLocPort *locPort,
                                  struct lldpStatsPort *statsPort,
                                  struct lldpTlv *tlvChain, 
                                  struct lldpMsap *msap,
                                  uint64_t remoteMac,
                                  int rxTTL)
{
    int status = LLDP_OK;

    struct lldpRem *remPort;
    struct lldpRemManAddr *remMgmt;
    struct lldpTlv *tlv;

    char     stmp[256];

    uint16_t sysCapSupported;
    uint16_t sysCapEnabled;

    char     addr[32];
    int      addrSubtype;
    int      intfIdSubtype;
    uint32_t intfId;
    char     oid[256];

    int      tlvType;
    uint32_t tlvValidMask = 0;

    uint32_t now = (uint32_t)time(NULL);

    if ((remPort = lldpRemGetByMsap(msap)) == NULL) {
        /* check neighbors count */
        if (locPort->portNeighbors > LLDP_MAX_NEIGHBORS) {
            /* too many neighbors */
            locPort->tooManyNeighbors = 1;
            locPort->tooManyNeighborsTimer = 
                      MAX(locPort->tooManyNeighborsTimer, now + rxTTL);

            /* increment error counters */        
            statsPort->lldpStatsRxPortFramesDiscardedTotal++;

            /* update global counters */
            lldpDB->lldpStatistics.lldpStatsRemTablesLastChangeTime = now;
            lldpDB->lldpStatistics.lldpStatsRemTablesDrops++;

            status = LLDP_ERR_NO_MEMORY;
            goto ABORT;
        }

        /* allocate remote port entry */
        if ((remPort = lldpRemCreate(locPort->lldpLocPortNum, msap)) == NULL) {
            status = LLDP_ERR_NO_MEMORY;
            goto ABORT;
        }

        /* update global counters */
        lldpDB->lldpStatistics.lldpStatsRemTablesLastChangeTime = now;
        lldpDB->lldpStatistics.lldpStatsRemTablesInserts++;

        locPort->somethingChangedRemote = 1;

        /* increase neighbors count */
        locPort->portNeighbors++;

        LLDP_INFO("LLDP: Remote Port Added"
                  " (index:%d chassisId:%s portId:%s).\n",
                  remPort->lldpRemIndex, remPort->lldpRemChassisId, remPort->lldpRemPortId);

        /* notify protocol layer that a new remote was detected */
        LLDP_PROTOCOL(handleRemoteLearn, locPort->lldpLocPortNum, msap);
    }

    for (tlv = tlvChain; tlv; tlv = tlv->next) {
        switch (tlv->type) {
        case LLDP_TLV_TYPE_PORT_DESCRIPTION:
            status = lldpDecodeGenericStringTlv(tlv, stmp, sizeof(stmp));
            if (status != LLDP_OK) {
                statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                continue;
            }

            if (strcmp(stmp, remPort->lldpRemPortDesc)) {
                lldp_strcpy(remPort->lldpRemPortDesc, stmp);
                locPort->somethingChangedRemote = 1;
                LLDP_INFO("LLDP: Remote Port-Description Changed"
                          " (index:%d portDesc:%s).\n",
                          remPort->lldpRemIndex, remPort->lldpRemPortDesc);
            }
            break;

        case LLDP_TLV_TYPE_SYSTEM_NAME:
            status = lldpDecodeGenericStringTlv(tlv, stmp, sizeof(stmp));
            if (status != LLDP_OK) {
                statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                continue;
            }

            if (strcmp(stmp, remPort->lldpRemSysName)) {
                lldp_strcpy(remPort->lldpRemSysName, stmp);
                locPort->somethingChangedRemote = 1;
                LLDP_INFO("LLDP: Remote System-Name Changed"
                          " (index:%d sysName:%s).\n",
                          remPort->lldpRemIndex, remPort->lldpRemSysName);
            }
            break;

        case LLDP_TLV_TYPE_SYSTEM_DESCRIPTION:
            status = lldpDecodeGenericStringTlv(tlv, stmp, sizeof(stmp));
            if (status != LLDP_OK) {
                statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                continue;
            }

            if (strcmp(stmp, remPort->lldpRemSysDesc)) {
                lldp_strcpy(remPort->lldpRemSysDesc, stmp);
                locPort->somethingChangedRemote = 1;
                LLDP_INFO("LLDP: Remote System-Description Changed"
                          " (index:%d sysDesc:%s).\n", 
                          remPort->lldpRemIndex, remPort->lldpRemSysDesc);
            }
            break;

        case LLDP_TLV_TYPE_SYSTEM_CAPABILITIES:
            status = lldpDecodeSystemCapTlv(tlv, &sysCapSupported, &sysCapEnabled);
            if (status != LLDP_OK) {
                statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                continue;
            }

            if ((sysCapSupported != remPort->lldpRemSysCapSupported) ||
                (sysCapEnabled != remPort->lldpRemSysCapEnabled)) {
                remPort->lldpRemSysCapSupported = sysCapSupported;
                remPort->lldpRemSysCapEnabled = sysCapEnabled;
                locPort->somethingChangedRemote = 1;
                LLDP_INFO("LLDP: Remote System-Capabilities Changed"
                          " (index:%d sysCapSupported:0x%04x sysCapEnabled:0x%04x).\n", 
                          remPort->lldpRemIndex, remPort->lldpRemSysCapSupported, remPort->lldpRemSysCapEnabled);
            }
            break;

        case LLDP_TLV_TYPE_MANAGEMENT_ADDRESS:
            status = lldpDecodeMgmtAddressTlv(tlv, &addrSubtype, addr, 
                                              &intfIdSubtype, &intfId, oid);
            if (status != LLDP_OK) {
                statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                continue;
            }

            if ((remMgmt = lldpRemManAddrGet(remPort, addr)) == NULL) {
                if (locPort->mgmtNeighbors > LLDP_MAX_NEIGHBORS) {
                    status = LLDP_ERR_NO_MEMORY;
                    statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                    continue;
                }

                /* allocate remote management address entry */
                if ((remMgmt = lldpRemManAddrCreate(remPort, addrSubtype, addr)) == NULL) {
                    status = LLDP_ERR_NO_MEMORY;
                    statsPort->lldpStatsRxPortTLVsDiscardedTotal++;
                    continue;
                }

                remMgmt->lldpRemManAddrIfSubtype = intfIdSubtype;
                remMgmt->lldpRemManAddrIfId = intfId;
                lldp_strcpy(remMgmt->lldpRemManAddrOID, oid);

                locPort->somethingChangedRemote = 1;

                /* increase neighbors count */
                locPort->mgmtNeighbors++;

                LLDP_INFO("LLDP: Remote Management Address Added"
                          " (index:%d addr:%s intfId:0x%04x OID:%s).\n", 
                          remPort->lldpRemIndex, addr, intfId, oid);
            }
            else if ((addrSubtype != remMgmt->lldpRemManAddrSubtype) ||
                     (intfIdSubtype != remMgmt->lldpRemManAddrIfSubtype) ||
                     (intfId != remMgmt->lldpRemManAddrIfId) ||
                     (strcmp(oid, remMgmt->lldpRemManAddrOID))) {
                remMgmt->lldpRemManAddrSubtype = addrSubtype;
                remMgmt->lldpRemManAddrIfSubtype = intfIdSubtype;
                remMgmt->lldpRemManAddrIfId = intfId;
                lldp_strcpy(remMgmt->lldpRemManAddrOID, stmp);
                locPort->somethingChangedRemote = 1;
                LLDP_INFO("LLDP: Remote Management Address Changed"
                          " (index:%d intfId:0x%04x OID:%s).\n", 
                          remPort->lldpRemIndex, intfId, remMgmt->lldpRemManAddrOID);
            }
            break;
        default:
            break;
        }

        /* marked handled type */
        tlvValidMask |= (1 << tlv->type);
    }

    /* notify protocol managers about unhandled TLV types */
    for (tlvType=LLDP_TLV_TYPE_PORT_DESCRIPTION; tlvType<=LLDP_TLV_TYPE_MANAGEMENT_ADDRESS; tlvType++) {
        if ((tlvValidMask & (1 << tlvType)) == 0) {
            switch (tlvType) {
            case LLDP_TLV_TYPE_PORT_DESCRIPTION:
                if (strcmp("", remPort->lldpRemPortDesc)) {
                    lldp_strcpy(remPort->lldpRemPortDesc, "");
                    locPort->somethingChangedRemote = 1;
                    LLDP_INFO("LLDP: Remote Port-Description Changed"
                              " (index:%d portDesc:%s).\n",
                              remPort->lldpRemIndex, remPort->lldpRemPortDesc);
                }
                break;

            case LLDP_TLV_TYPE_SYSTEM_NAME:
                if (strcmp("", remPort->lldpRemSysName)) {
                    lldp_strcpy(remPort->lldpRemSysName, "");
                    locPort->somethingChangedRemote = 1;
                    LLDP_INFO("LLDP: Remote System-Name Changed"
                              " (index:%d sysName:%s).\n",
                              remPort->lldpRemIndex, remPort->lldpRemSysName);
                }
                break;

            case LLDP_TLV_TYPE_SYSTEM_DESCRIPTION:
                if (strcmp("", remPort->lldpRemSysDesc)) {
                    lldp_strcpy(remPort->lldpRemSysDesc, "");
                    locPort->somethingChangedRemote = 1;
                    LLDP_INFO("LLDP: Remote System-Description Changed"
                              " (index:%d sysDesc:%s).\n", 
                              remPort->lldpRemIndex, remPort->lldpRemSysDesc);
                }
                break;

            case LLDP_TLV_TYPE_SYSTEM_CAPABILITIES:
                if ((0 != remPort->lldpRemSysCapSupported) ||
                    (0 != remPort->lldpRemSysCapEnabled)) {
                    remPort->lldpRemSysCapSupported = 0;
                    remPort->lldpRemSysCapEnabled = 0;
                    locPort->somethingChangedRemote = 1;
                    LLDP_INFO("LLDP: Remote System-Capabilities Changed"
                              " (index:%d sysCapSupported:0x%04x sysCapEnabled:0x%04x).\n", 
                              remPort->lldpRemIndex, remPort->lldpRemSysCapSupported, remPort->lldpRemSysCapEnabled);
                }
                break;

            case LLDP_TLV_TYPE_MANAGEMENT_ADDRESS: {
                    struct lldpRemManAddr *iter, *next;

                    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpRemManAddr, node,
                                            &remPort->lldpRemManAddrTable) {
                        /* find and delete remote entry */
                        if ((status = lldpRemManAddrDestroy(iter)) != LLDP_OK)
                            return status;
                        else
                            locPort->mgmtNeighbors--;
                    }
                }
                break;
            default:
                break;
            }
        }
    }

    /* update remote mac address */
    remPort->lldpRemMacAddress = remoteMac;

    /* update time-stamp */
    remPort->lldpRemTimeMark = now;

    /* update TTL */
    remPort->rxInfoTTL = now + rxTTL;

    /* send TLV to upper level DCBX */
    LLDP_PROTOCOL(handleIngressTLVs, locPort->lldpLocPortNum, tlvChain, msap, remoteMac, rxTTL);

ABORT:

    return status;

}   /* end lldpRxUpdateRemotePort */

/*****************************************************************************/
/** lldpRxRemoveRemotePort
 * \ingroup lldp
 *
 * \desc            Delete remote entry data.
 *
 * \param[out]      msap is the entry ChassisId / PortId to be deleted.
 *
 * \return          LLDP_OK if succesfull.
 * \return          LLDP_ERR_NOT_FOUND if MSAP is not found.
 *
 *****************************************************************************/
static int lldpRxRemoveRemotePort(struct lldpMsap *msap)
{
    int status = LLDP_OK;

    struct lldpLocPort    *locPort;
    struct lldpRem        *remPort;
    struct lldpRemManAddr *iter, *next;

    uint32_t now = time(NULL);

    /* get remote port entry */
    if (!(remPort = lldpRemGetByMsap(msap)))
        return LLDP_ERR_NOT_FOUND;

    if (!(locPort = lldpLocPortGet(remPort->lldpRemLocalPortNum)))
        return LLDP_ERR_NOT_FOUND;

    /* notify protocol layer that remote entry has age-out */
    LLDP_PROTOCOL(handleRemoteAging, locPort->lldpLocPortNum, msap);

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpRemManAddr, node,
                            &remPort->lldpRemManAddrTable) {
        /* find and delete remote entry */
        if ((status = lldpRemManAddrDestroy(iter)) != LLDP_OK)
            return status;
        else
            locPort->mgmtNeighbors--;
    }

    /* find and delete remote entry */
    if ((status = lldpRemDestroy(remPort)) != LLDP_OK)
        return status;

    /* update global counters */
    lldpDB->lldpStatistics.lldpStatsRemTablesLastChangeTime = now;
    lldpDB->lldpStatistics.lldpStatsRemTablesDeletes++;

    /* decrease neighbors count */
    locPort->portNeighbors--;

    /* update something-changed flag */
    locPort->somethingChangedRemote = 1;

    return LLDP_OK;

}   /* end lldpRxRemoveEntry */

static int lldpRxCleanupLocalPort(int portNum __attribute__((__unused__)))
{
    struct lldpRem *iter, *next;
    struct lldpMsap        msap;

    LLDP_LIST_FOR_EACH_SAFE(iter, next, struct lldpRem, node, &lldpDB->lldpRemoteSystemsData.lldpRemTable)
    {
        lldpRxRemoveRemotePort(lldpMsapFromRemPort(&msap, iter));
    }

    return LLDP_OK;

}   /* end lldpRxCleanupLocalPort */

/*****************************************************************************/
/** lldpRxStateMachineCycle
 * \ingroup lldp
 *
 * \desc            Cycle RX state machine, until no state-change is detected.
 *
 * \param[in]       portNum is the local port to run it's state-machine.
 *
 * \param[in]       msap is the remote msap to check for age-out. if state-
 *                  machine was not triggered by age-out, this argument should
 *                  be NULL.
 *
 * \param[in]       event is the incoming event (LLDP packet or port link-
 *                  status update event). If state-machine was not triggered by
 *                  an event, this argument should be NULL.
 *
 *****************************************************************************/
static int lldpRxStateMachineCycle(int portNum, struct lldpMsap *msap, lldp_event *event)
{
    int                    status = LLDP_OK;
    int                    snapEncoded = 0;
    uint32_t               now;

    struct lldpTlv        *pkt_tlv = NULL;
    int                    pkt_ttl = 1;
    uint64_t               pkt_mac = 0;
    struct lldpMsap        pkt_msap;
    struct lldpMsap        age_msap;

    struct lldpRem        *remPort = NULL;
    struct lldpLocPort    *locPort = NULL;
    struct lldpPortConfig *portConfig = NULL;
    struct lldpStatsPort  *statsPort = NULL;

    /* clear local pkt msap */
    lldpMsapClear(&pkt_msap);

    /* clear aged port msap */
    lldpMsapClear(&age_msap);

    /* get current time */
    now = (uint32_t)time(NULL);

    /* get local/remote port entries */
    remPort    = msap ? lldpRemGetByMsap(msap) : NULL;
    locPort    = lldpLocPortGet(portNum);
    portConfig = lldpPortConfigGet(portNum);
    statsPort  = lldpStatsPortGet(portNum);

    /* verify valid entries found */
    if (!locPort || !portConfig || !statsPort) {
        status = LLDP_ERR_NOT_FOUND;
        goto ABORT;
    }

    /* update too many neighbors */
    if (locPort->tooManyNeighbors && locPort->tooManyNeighborsTimer <= now) {
        locPort->tooManyNeighbors = 0;
    }

    do {
        switch (locPort->rxState) {
        default:
            {
                RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
            }
            break;

        case LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL:
            {
                if (RX_UPON_STATE_ENTRY()) {
                    LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL\n");
                }

                /* check if remote port entry has expired */
                locPort->rxInfoAged = remPort && (remPort->rxInfoTTL <= now);

                if (locPort->rxInfoAged)
                    RX_CHANGE_STATE(LLDP_RX_STATE_DELETE_AGED_INFO);
                else if (portConfig->lldpPortConfigEnabled)
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_LLDP_INITIALIZE);
                else
                    RX_NO_CHANGE_STATE();
            }
            break;

        case LLDP_RX_STATE_DELETE_AGED_INFO:
            {
                if (RX_UPON_STATE_ENTRY()) {
                    /* create MSAP from remote port entry */
                    lldpMsapFromRemPort(&age_msap, remPort);

                    /* remove remote-port */
                    lldpRxRemoveRemotePort(&age_msap);

                    /* update global counters */
                    lldpDB->lldpStatistics.lldpStatsRemTablesLastChangeTime = now;
                    lldpDB->lldpStatistics.lldpStatsRemTablesAgeouts++;

                    /* after we delete remote port, it's entry is no longer valid */
                    lldpMsapClear(&age_msap);
                    remPort = NULL;

                    locPort->rxInfoAged = 0;
                    /* somethingRemoteChanged is set by lldpRxUpdateRemotePort */

                    LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_DELETE_AGED_INFO\n");
                }

                RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
            }
            break;

        case LLDP_RX_STATE_RX_LLDP_INITIALIZE:
            {
                if (RX_UPON_STATE_ENTRY()) {
                    lldpRxCleanupLocalPort(portNum);
                    locPort->tooManyNeighbors = 0;
                    locPort->rcvFrame = 0;

                    LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_RX_LLDP_INITIALIZE\n");
                }

                if (!portConfig->lldpPortConfigEnabled)
                    RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
                else if (portConfig->lldpPortConfigAdminStatus == LLDP_ADMIN_RX_ONLY ||
                    portConfig->lldpPortConfigAdminStatus == LLDP_ADMIN_TX_AND_RX)
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_WAIT_FOR_FRAME);
                else
                    RX_NO_CHANGE_STATE();
            }
            break;

        case LLDP_RX_STATE_RX_WAIT_FOR_FRAME:
            {
                if (RX_UPON_STATE_ENTRY()) {
                    locPort->badFrame = 0;
                    locPort->rxInfoAged = 0;
                    locPort->somethingChangedRemote = 0;

                    LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_RX_WAIT_FOR_FRAME\n");
                }

                /* check if remote port entry has expired */
                locPort->rxInfoAged = remPort && (remPort->rxInfoTTL <= now);

                /* check if port is enabled */
                locPort->rcvFrame = event && (event->type == LLDP_EVENT_PACKET);

                if (!portConfig->lldpPortConfigEnabled)
                    RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
                else if (portConfig->lldpPortConfigAdminStatus != LLDP_ADMIN_RX_ONLY &&
                         portConfig->lldpPortConfigAdminStatus != LLDP_ADMIN_TX_AND_RX)                    
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_LLDP_INITIALIZE);
                else if (locPort->rxInfoAged)
                    RX_CHANGE_STATE(LLDP_RX_STATE_DELETE_INFO);
                else if (locPort->rcvFrame)
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_FRAME);
                else
                    RX_NO_CHANGE_STATE();
            }
            break;

        case LLDP_RX_STATE_RX_FRAME:
            {
                if (RX_UPON_STATE_ENTRY()) {
                    locPort->rcvFrame = 0;

                    LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_RX_FRAME\n");
                }

                /* release tlv chain if been already used */
                if (pkt_tlv) {
                    lldpFreeTlvChain(pkt_tlv);
                    pkt_tlv = NULL;
                }

                /* decode TLVs from incoming-packet, create TLVs chain */
                locPort->badFrame = 
                    (lldptRxFrameRecognition(event->data, event->size, statsPort, &snapEncoded, &pkt_mac) != LLDP_OK ||
                     lldpDecode(event->data, event->size, ENCODED_HEADER_LEN(snapEncoded), &pkt_tlv) != LLDP_OK ||
                     lldpRxFrameValidation(locPort, statsPort, pkt_tlv, &pkt_msap, &pkt_ttl) != LLDP_OK);

                /* clear event pointer */
                event = NULL;

                if (!portConfig->lldpPortConfigEnabled)
                    RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
                else if (locPort->badFrame)
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_WAIT_FOR_FRAME);
                else if (pkt_ttl == 0)
                    RX_CHANGE_STATE(LLDP_RX_STATE_DELETE_INFO);
                else
                    RX_CHANGE_STATE(LLDP_RX_STATE_UPDATE_INFO);
            }
            break;

        case LLDP_RX_STATE_DELETE_INFO: 
            {
                if (RX_UPON_STATE_ENTRY()) {
                    if (remPort) {
                        /* create MSAP from remote port entry */
                        lldpMsapFromRemPort(&age_msap, remPort);

                        LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_DELETE_INFO "
                                 "(age chassisId:%s portId:%s)\n", age_msap.lldpMsapChassisId, age_msap.lldpMsapPortId);

                        /* remove remote-port */
                        lldpRxRemoveRemotePort(&age_msap);

                        LLDP_INFO("LLDP: Remote Port Age-out (chassisId:%s portId:%s).\n",
                                  age_msap.lldpMsapChassisId, age_msap.lldpMsapPortId);

                        /* after we delete remote port, it's entry is no longer valid */
                        lldpMsapClear(&age_msap);
                        remPort = NULL;

                        /* increment age-out counter */
                        statsPort->lldpStatsRxPortAgeoutsTotal++;
                    }
                    else {
                        LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_DELETE_INFO "
                                 "(shutdown chassisId:%s portId:%s)\n", pkt_msap.lldpMsapChassisId, pkt_msap.lldpMsapPortId);

                        lldpRxRemoveRemotePort(&pkt_msap);

                        LLDP_INFO("LLDP: Remote Port Shutdown (chassisId:%s portId:%s).\n",
                                  pkt_msap.lldpMsapChassisId, pkt_msap.lldpMsapPortId);

                        lldpMsapClear(&pkt_msap);
                    }
                    
                    /* somethingRemoteChanged is set by lldpRxRemoveRemotePort */
                }

                if (!portConfig->lldpPortConfigEnabled)
                    RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
                else 
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_WAIT_FOR_FRAME);                
            }
            break;

        case LLDP_RX_STATE_UPDATE_INFO:
            {
                if (RX_UPON_STATE_ENTRY()) {
                    LLDP_DBG("LLDP: RX State -> LLDP_RX_STATE_UPDATE_INFO "
                             "(chassisId:%s portId:%s)\n", pkt_msap.lldpMsapChassisId, pkt_msap.lldpMsapPortId);

                    lldpRxUpdateRemotePort(locPort, statsPort, pkt_tlv, &pkt_msap, pkt_mac, pkt_ttl);

                    /* somethingRemoteChanged is set by lldpRxUpdateRemotePort */
                }

                if (!portConfig->lldpPortConfigEnabled)
                    RX_CHANGE_STATE(LLDP_RX_STATE_WAIT_FOR_PORT_OPERATIONAL);
                else
                    RX_CHANGE_STATE(LLDP_RX_STATE_RX_WAIT_FOR_FRAME);
            }
            break;
        }
    }
    while (RX_UPON_STATE_ENTRY());

ABORT:

    /* release tlv chain */
    if (pkt_tlv) {
        lldpFreeTlvChain(pkt_tlv);
        pkt_tlv = NULL;
    }

    return status;

}   /* end lldpRxStateMachine */


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** lldpRxInitialize
 * \ingroup lldp
 *
 * \desc            Initializes LLDP RX module.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpRxInitialize()
{
    return LLDP_OK;

}   /* end lldpRxInitialize */

/*****************************************************************************/
/** lldpRxTerminate
 * \ingroup lldp
 *
 * \desc            Terminates LLDP Rx module.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpRxTerminate()
{
    return LLDP_OK;

}   /* end lldpRxTerminate */

/*****************************************************************************/
/** lldpRxRun
 * \ingroup lldp
 *
 * \desc            Run RX state-machine for all ports.
 *
 * \return          LLDP_OK always.
 *
 *****************************************************************************/
int lldpRxRun(lldp_event *event)
{
    struct lldpRem *remPort, *remNext;
    struct lldpLocPort  *locPort, *locNext;
    struct lldpMsap        msap;
    uint32_t         now;

    /* get current time */
    now = (uint32_t)time(NULL);

    if (event && (event->type == LLDP_EVENT_PACKET)) {
        lldpRxStateMachineCycle(event->port, NULL, event);
    }

    LLDP_LIST_FOR_EACH_SAFE(locPort, locNext, struct lldpLocPort, node, 
                            &lldpDB->lldpLocalSystemData.lldpLocPortTable)
    {
        if (locPort->somethingChangedLocal) {
            lldpRxStateMachineCycle(locPort->lldpLocPortNum, NULL, NULL);
        }
    }

    LLDP_LIST_FOR_EACH_SAFE(remPort, remNext, struct lldpRem, node, 
                            &lldpDB->lldpRemoteSystemsData.lldpRemTable)
    {
        if (remPort->rxInfoTTL <= now) {
            lldpMsapFromRemPort(&msap, remPort);
            lldpRxStateMachineCycle(remPort->lldpRemLocalPortNum, &msap, NULL);
        }
    }

    lldpMsapClear(&msap);

    return LLDP_OK;

}   /* end lldpRxRun */

