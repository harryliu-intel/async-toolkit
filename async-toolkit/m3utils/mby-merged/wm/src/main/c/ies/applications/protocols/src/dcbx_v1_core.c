/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_core.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX P802.1Qaz/D2.1 Core Protocol.   
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

#include <lldp.h>
#include <lldp_mib.h>
#include <lldp_int.h>
#include <lldp_tlv.h>

#include <dcbx.h>
#include <dcbx_mib.h>

#include <dcbx_v1_tlv.h>
#include <dcbx_v1_ets.h>
#include <dcbx_v1_pfc.h>
#include <dcbx_v1_app.h>
#include <dcbx_v1_cn.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static int dcbxV1InitializeModules();
static int dcbxV1TerminateModules();
static int dcbxV1HandleIngressTLVs(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);
static int dcbxV1HandleRemoteLearn(int portNum, const struct lldpMsap *msap);
static int dcbxV1HandleRemoteAging(int portNum, const struct lldpMsap *msap);
static int dcbxV1AppendEgressTLVs(int portNum, struct lldpTlv *tlvChain);


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

struct lldpProtocolIf dcbxV1LldpProtocolInterface = {
    dcbxV1InitializeModules,
    dcbxV1TerminateModules,
    dcbxV1HandleIngressTLVs,
    dcbxV1HandleRemoteLearn,
    dcbxV1HandleRemoteAging,
    dcbxV1AppendEgressTLVs,
    NULL
};


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV1RxFrameValidation
 * \ingroup dcbx
 *
 * \desc            Validates TLV as valid DCBX P802.1Qaz/D2.1 TLV.
 *
 * \param[in]       tlv is ingress TLV to be validated.
 *
 * \param[out]      subType points to caller-allocated storage where this
 *                  function should place the TLV sub-type.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1RxFrameValidation(const struct lldpTlv *tlv, enum dcbxTlvSubType *subType)
{
    if (!tlv || !tlv->data || tlv->len < 4)
        return LLDP_ERR_INVALID_PACKET;

    if (dcbxV1ValidateOrganizationallyTlv(tlv) != LLDP_OK)
        return LLDP_ERR_NOT_FOUND;

    /* organizationally TLV sub-type */
    *subType = (enum dcbxTlvSubType)tlv->data[3];

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1InitializeModules
 * \ingroup dcbx
 *
 * \desc            Initializes DCBX Protocol.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1InitializeModules()
{
    dcbxV1EtsInitialize();
    dcbxV1PfcInitialize();
    dcbxV1CnInitialize();
    dcbxV1AppInitialize();

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1TerminateModules
 * \ingroup dcbx
 *
 * \desc            Terminates DCBX Protocol.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1TerminateModules()
{
    dcbxV1EtsTerminate();
    dcbxV1PfcTerminate();
    dcbxV1CnTerminate();
    dcbxV1AppTerminate();

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1HandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress TLV.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvChain is ingress TLV chain received on the local port.
 *
 * \param[in]       msap is the remote port identifier.
 *
 * \param[in]       remoteMac is the source MAC address of the ingress LLDPDU.
 *
 * \param[in]       rxTTL is the TTL received by the ingress LLDPDU.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1HandleIngressTLVs(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int  status = LLDP_OK;
    const struct lldpTlv *tlv;
    enum dcbxTlvSubType subType;
    struct lldpXdot1dcbxRemoteData *remote;

    bool dcbxValid = false;
    uint32_t dcbxValidMask = 0;

    /* auto-learn entry if currently not present */
    if (!dcbxRemoteDataGet(portNum)) {
        LLDP_INFO("DCBX: Remote entry not found. Creating...\n");
        if ((status = dcbxV1HandleRemoteLearn(portNum, msap)) != LLDP_OK) 
            return status;
    }

	for (tlv = tlvChain; tlv; tlv = tlv->next)
    {
        /* verify TLV is valid organizationally DCBX TLV */
        if (dcbxV1RxFrameValidation(tlv, &subType) != LLDP_OK) continue;

        switch (subType) {
        case DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION:
        case DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION:
            dcbxV1EtsHandleIngressTLVs(portNum, tlv, msap, remoteMac, rxTTL);
            dcbxValid = true;
            break;

        case DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL:
            dcbxV1PfcHandleIngressTLVs(portNum, tlv, msap, remoteMac, rxTTL);
            dcbxValid = true;
            break;

        case DCBX_V1_TLV_SUBTYPE_APPLICATION_PRIORITY:
            dcbxV1AppHandleIngressTLVs(portNum, tlv, msap, remoteMac, rxTTL);
            dcbxValid = true;
            break;

        case DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION:
            dcbxV1CnHandleIngressTLVs(portNum, tlv, msap, remoteMac, rxTTL);
            dcbxValid = true;
            break;

        default:
            break;
        }

        /* marked handled type */
        dcbxValidMask |= (1 << subType);
    }

    if (dcbxValid) {
        LLDP_DBG("DCBX: Received DCBX Version 1 TLVs\n");

        /* update time-stamp and remote mac address */
        remote = dcbxRemoteDataGet(portNum);

        remote->lldpV2RemTimeMark = time(NULL);
        remote->lldpV2RemLocalDestMACAddress = remoteMac;
        remote->lldpXdot1dcbxRemoteDataValid = true;
    }

    /* notify protocol managers about unhandled TLV types */
    for (subType=0; subType<DCBX_V1_TLV_SUBTYPE_MAX; subType++) {
        if ((dcbxValidMask & (1 << subType)) == 0) {
            switch (subType) {
            case DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION:
            case DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION:
                dcbxV1EtsHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            case DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL:
                dcbxV1PfcHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            case DCBX_V1_TLV_SUBTYPE_APPLICATION_PRIORITY:
                dcbxV1AppHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            case DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION:
                dcbxV1CnHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            default:
                break;
            }
        }
    }

    return dcbxValid ? LLDP_OK : LLDP_ERR_NOT_FOUND;
}

/*****************************************************************************/
/** dcbxV1HandleRemoteLearn
 * \ingroup dcbx
 *
 * \desc            Handles remote entry learning by the LLDP manager.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       msap is the remote port identifier.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1HandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    struct lldpXdot1dcbxRemoteData *remote;
    struct lldpXdot1dcbxLocalData  *local = dcbxLocalDataGet(portNum);

    /* get number of lldp neighbors (peers) on port */
    int neighbors = lldpLocPortGet(portNum)->portNeighbors;

    /* check for multiple peers */
    if (neighbors > 1) {
       if (!local->lldpMultiplePeers) {
           local->lldpMultiplePeers = true;
           LLDP_ERR("DCBX: *** Multiple peers detected (portNum:%d) ***\n", portNum);

           /* destroy any current remote data on multiple-peers port */
           if (dcbxRemoteDataGet(portNum))
               dcbxV1HandleRemoteAging(portNum, msap);          
       }
 
       /* ignore learning when multiple peers alarm is active */
       return LLDP_ERR_ALREADY_EXISTS;

    } else {
       if (local->lldpMultiplePeers) {
           local->lldpMultiplePeers = false;
           LLDP_INFO("DCBX: *** Multiple peers no longer detected (portNum:%d) ***\n", portNum);
       }
    }

    /* allocate remote-data object */
    remote = dcbxRemoteDataCreate(portNum, msap->lldpRemIndex);
    if (!remote) return LLDP_ERR_NO_MEMORY;

    dcbxV1EtsHandleRemoteLearn(portNum, msap);
    dcbxV1PfcHandleRemoteLearn(portNum, msap);
    dcbxV1CnHandleRemoteLearn(portNum, msap);
    dcbxV1AppHandleRemoteLearn(portNum, msap);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1HandleRemoteAging
 * \ingroup dcbx
 *
 * \desc            Handles remote entry aging by the LLDP manager.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       msap is the remote port identifier.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1HandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    dcbxV1EtsHandleRemoteAging(portNum, msap);
    dcbxV1PfcHandleRemoteAging(portNum, msap);
    dcbxV1CnHandleRemoteAging(portNum, msap);
    dcbxV1AppHandleRemoteAging(portNum, msap);

    /* destroy remote-data object */
    return dcbxRemoteDataDestroy(dcbxRemoteDataGet(portNum));
}

/*****************************************************************************/
/** dcbxV1AppendEgressTLVs
 * \ingroup dcbx
 *
 * \desc            Appends DCBX TLV to the egress TLVs chain.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvChain is egress TLV chain received on the local port.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV1AppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    /* get local-port pointers */
    struct lldpXdot1dcbxLocalData *local = dcbxLocalDataGet(portNum);
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config || !tlvChain) 
        return LLDP_ERR_INVALID_ARG;

    LLDP_DBG("DCBX: Sending DCBX Version 1 TLVs\n");

    dcbxV1EtsAppendEgressTLVs(portNum, tlvChain);
    dcbxV1PfcAppendEgressTLVs(portNum, tlvChain);
    dcbxV1CnAppendEgressTLVs(portNum, tlvChain);
    dcbxV1AppAppendEgressTLVs(portNum, tlvChain);

    return LLDP_OK;
}

