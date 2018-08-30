/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v0_core.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX Base Protocol v1.01 Core Protocol.
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

#include <dcbx_v0_tlv.h>
#include <dcbx_v0_ets.h>
#include <dcbx_v0_pfc.h>
#include <dcbx_v0_app.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define DCBX_V0_MAX_VERSION     0


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static int dcbxV0InitializeModules();
static int dcbxV0TerminateModules();
static int dcbxV0HandleIngressTLVs(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);
static int dcbxV0HandleRemoteLearn(int portNum, const struct lldpMsap *msap);
static int dcbxV0HandleRemoteAging(int portNum, const struct lldpMsap *msap);
static int dcbxV0AppendEgressTLVs(int portNum, struct lldpTlv *tlvChain);


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

/*****************************************************************************
 * Local Variables
 *****************************************************************************/

struct lldpProtocolIf dcbxV0LldpProtocolInterface = {
    dcbxV0InitializeModules,
    dcbxV0TerminateModules,
    dcbxV0HandleIngressTLVs,
    dcbxV0HandleRemoteLearn,
    dcbxV0HandleRemoteAging,
    dcbxV0AppendEgressTLVs,
    NULL
};


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV0RxFrameValidation
 * \ingroup dcbx
 *
 * \desc            Validates TLV as valid DCBX Base Protocol v1.01 TLV.
 *
 * \param[in]       tlv is ingress TLV to be validated.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV0RxFrameValidation(const struct lldpTlv *tlv)
{
    if (!tlv || !tlv->data || tlv->len < 4)
        return LLDP_ERR_INVALID_PACKET;

    if (dcbxV0ValidateOrganizationallyTlv(tlv) != LLDP_OK)
        return LLDP_ERR_NOT_FOUND;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0InitializeModules
 * \ingroup dcbx
 *
 * \desc            Initializes DCBX Protocol.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV0InitializeModules()
{
    dcbxV0EtsInitialize();
    dcbxV0PfcInitialize();
    dcbxV0AppInitialize();

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0TerminateModules
 * \ingroup dcbx
 *
 * \desc            Terminates DCBX Protocol.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int dcbxV0TerminateModules()
{
    dcbxV0EtsTerminate();
    dcbxV0PfcTerminate();
    dcbxV0AppTerminate();

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0HandleIngressTLVs
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
static int dcbxV0HandleIngressTLVs(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int  status = LLDP_OK;
    const struct lldpTlv *tlv;
    enum dcbxV0TlvSubType subType;
    struct lldpXdot1dcbxRemoteData *remote;
    struct lldpXdot1dcbxLocalData *local;

    uint8_t  peerOperVersion;
    uint8_t  peerMaxVersion;
    uint32_t peerSeqNo;
    uint32_t peerAckNo;

    bool dcbxValid = false;
    uint32_t dcbxValidMask = 0;

    /* get local entry */
    local = dcbxLocalDataGet(portNum);
    if (!local) return LLDP_ERR_NOT_FOUND;

    /* auto-learn entry if currently not present */
    if (!dcbxRemoteDataGet(portNum)) {
        LLDP_INFO("DCBX: Remote entry not found. Creating...\n");
        if ((status = dcbxV0HandleRemoteLearn(portNum, msap)) != LLDP_OK) 
            return status;
    }

	for (tlv = tlvChain; tlv; tlv = tlv->next)
    {
        struct lldpTlv *tlvSubChain;
        const struct lldpTlv *tlvSub;
        
        /* verify TLV is valid organizationally DCBX TLV */
        if (dcbxV0RxFrameValidation(tlv) != LLDP_OK ||
            dcbxV0Decode(tlv, &tlvSubChain) != LLDP_OK) continue;

        /* process control TLV */
		for (tlvSub = tlvSubChain; tlvSub; tlvSub = tlvSub->next)
        {
	        switch (tlvSub->type) {
            case DCBX_V0_TLV_SUBTYPE_CONTROL:
                if (dcbxV0DecodeControlTlv(tlvSub, &peerOperVersion, &peerMaxVersion, &peerSeqNo, &peerAckNo) == LLDP_OK) {
                    local->dcbxV0RcvdAckNo = peerAckNo;
                    local->dcbxV0AckNo = peerSeqNo;
                    local->dcbxV0OperVersion = DCBX_V0_MAX_VERSION;
            	    dcbxValid = true;
                }
	            break;

    	    default:
        	    break;
        	}

    	    /* marked handled type */
        	dcbxValidMask |= (1 << tlvSub->type);
        }

        /* ignore all feature TLVs if control TLV is not present, or if version mismatch */
        if (!dcbxValid || local->dcbxV0OperVersion != peerOperVersion) 
            break; 

        LLDP_DBG("DCBX: Received DCBX Version 0 TLVs (SeqNo:%d AckNo:%d)\n", peerSeqNo, peerAckNo);

        /* process feature TLVs */
		for (tlvSub = tlvSubChain; tlvSub; tlvSub = tlvSub->next)
        {
	        switch (tlvSub->type) {
            case DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS:
        	    dcbxV0EtsHandleIngressTLVs(portNum, tlvSub, msap, remoteMac, rxTTL);
            	dcbxValid = true;
	            break;

            case DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL:
        	    dcbxV0PfcHandleIngressTLVs(portNum, tlvSub, msap, remoteMac, rxTTL);
            	dcbxValid = true;
	            break;

            case DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY:
        	    dcbxV0AppHandleIngressTLVs(portNum, tlvSub, msap, remoteMac, rxTTL);
            	dcbxValid = true;
	            break;

    	    default:
        	    break;
        	}

    	    /* marked handled type */
        	dcbxValidMask |= (1 << tlvSub->type);
		}

        lldpFreeTlvChain(tlvSubChain);  
    }

    if (dcbxValid) {
        /* update time-stamp and remote mac address */
        remote = dcbxRemoteDataGet(portNum);

        remote->lldpV2RemTimeMark = time(NULL);
        remote->lldpV2RemLocalDestMACAddress = remoteMac;
        remote->lldpXdot1dcbxRemoteDataValid = true;
    }
    else {
        /* initialize state-machine */
        local->dcbxV0OperVersion = DCBX_V0_MAX_VERSION;
        local->dcbxV0SeqNo = 0;
        local->dcbxV0AckNo = 0;
        local->dcbxV0RcvdAckNo = 0;
    }

    /* notify protocol managers about unhandled TLV types */
    for (subType=0; subType<DCBX_V0_TLV_SUBTYPE_MAX; subType++) {
        if ((dcbxValidMask & (1 << subType)) == 0) {
            switch (subType) {
            case DCBX_V0_TLV_SUBTYPE_CONTROL:
                break;

            case DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS:
                dcbxV0EtsHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            case DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL:
                dcbxV0PfcHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            case DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY:
                dcbxV0AppHandleMissingTLVs(portNum, subType, msap, remoteMac, rxTTL);
                break;

            default:
                break;
            }
        }
    }

    return dcbxValid ? LLDP_OK : LLDP_ERR_NOT_FOUND;
}

/*****************************************************************************/
/** dcbxV0HandleRemoteLearn
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
static int dcbxV0HandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    struct lldpXdot1dcbxRemoteData *remote;
    struct lldpXdot1dcbxLocalData  *local;

    /* get local entry */
    local = dcbxLocalDataGet(portNum);
    if (!local) return LLDP_ERR_NOT_FOUND;

    /* get number of lldp neighbors (peers) on port */
    int neighbors = lldpLocPortGet(portNum)->portNeighbors;

    /* check for multiple peers */
    if (neighbors > 1) {
       if (!local->lldpMultiplePeers) {
           local->lldpMultiplePeers = true;
           LLDP_ERR("DCBX: *** Multiple peers detected (portNum:%d) ***\n", portNum);

           /* destroy any current remote data on multiple-peers port */
           if (dcbxRemoteDataGet(portNum))
               dcbxV0HandleRemoteAging(portNum, msap);          
       }
 
       /* ignore learning when multiple peers alarm is active */
       return LLDP_ERR_ALREADY_EXISTS;

    } else {
       if (local->lldpMultiplePeers) {
           local->lldpMultiplePeers = false;
           LLDP_INFO("DCBX: *** Multiple peers no longer detected (portNum:%d) ***\n", portNum);
       }
    }

    /* initialize state-machine */
    local->dcbxV0OperVersion = DCBX_V0_MAX_VERSION;
    local->dcbxV0SeqNo = 0;
    local->dcbxV0AckNo = 0;
    local->dcbxV0RcvdAckNo = 0;

    /* allocate remote-data object */
    remote = dcbxRemoteDataCreate(portNum, msap->lldpRemIndex);
    if (!remote) return LLDP_ERR_NO_MEMORY;

    dcbxV0EtsHandleRemoteLearn(portNum, msap);
    dcbxV0PfcHandleRemoteLearn(portNum, msap);
    dcbxV0AppHandleRemoteLearn(portNum, msap);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0HandleRemoteAging
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
static int dcbxV0HandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    dcbxV0EtsHandleRemoteAging(portNum, msap);
    dcbxV0PfcHandleRemoteAging(portNum, msap);
    dcbxV0AppHandleRemoteAging(portNum, msap);

    /* destroy remote-data object */
    return dcbxRemoteDataDestroy(dcbxRemoteDataGet(portNum));
}

/*****************************************************************************/
/** dcbxV0AppendEgressTLVs
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
static int dcbxV0AppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    int status = LLDP_OK;
    struct lldpTlv *tlvSubChain = NULL, *tlv;

    /* get local-port pointers */
    struct lldpXdot1dcbxLocalData *local = dcbxLocalDataGet(portNum);
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config || !tlvChain) 
        return LLDP_ERR_INVALID_ARG;

    /* if !syncd we need to increment the sequence-no */
    if (!local->dcbxV0Changed) {
        local->dcbxV0Changed = true;
        local->dcbxV0SeqNo++;
    }

    LLDP_DBG("DCBX: Sending DCBX Version 0 TLVs (SeqNo:%d AckNo:%d)\n", local->dcbxV0SeqNo, local->dcbxV0AckNo);

    if ((status = dcbxV0EncodeControlTlv(&tlvSubChain, 
                                         local->dcbxV0OperVersion, 
                                         DCBX_V0_MAX_VERSION,
                                         local->dcbxV0SeqNo,
                                         local->dcbxV0AckNo)) != LLDP_OK)
        return status;

    dcbxV0EtsAppendEgressTLVs(portNum, tlvSubChain);
    dcbxV0PfcAppendEgressTLVs(portNum, tlvSubChain);
    dcbxV0AppAppendEgressTLVs(portNum, tlvSubChain);

    /* encode dcbx parent TLVs, and insert it to chain */
    if (dcbxV0Encode(tlvSubChain, &tlv) != LLDP_OK ||
        lldpInsertBeforeTlv(tlvChain, tlv, LLDP_TLV_TYPE_END_OF_LLDPDU) != LLDP_OK) {
        LLDP_ERR("DCBX: Error appending DCBX TLV to egress LLDP message (portNum:%d).\n", portNum);
        status = LLDP_ERR_UNKNOWN;       
    }

    if (tlvSubChain)
        lldpFreeTlvChain(tlvSubChain);

    return status;
}

