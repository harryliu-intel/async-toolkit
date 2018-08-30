/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v0_pfc.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of -
 *                    DCBX Base Protocol v1.01 PFC. 
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

#include <dcbx.h>
#include <dcbx_mib.h>
#include <dcbx_macros.h>

#include <dcbx_v0_tlv.h>
#include <dcbx_v0_pfc.h>
#include <dcbx_v1_pfc.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define DCBX_V0_PFC_MAX_VERSION 0


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV0PfcIsCompatible
 * \ingroup dcbx
 *
 * \desc            Checks if admin-data (local configuration) is compatible
 *                  with remote-data (remote operational).
 *
 * \param[in]       admin is the admin-data.
 *
 * \param[in]       remote is remote-data.
 *
 * \return          TRUE if admin-data is compatible with remote-data.
 * \return          FALSE if admin-data is not compatible with remote-data.
 *
 *****************************************************************************/
static bool dcbxV0PfcIsCompatible(struct lldpXdot1dcbxAdminData  *admin,
                                  struct lldpXdot1dcbxRemoteData *remote)
{
    int i;

    for (i=0; i<8; i++) {
        if (remote->lldpXdot1dcbxRemPFC.
                    lldpXdot1dcbxRemPFCEnable[i] !=
             admin->lldpXdot1dcbxAdminPFC.
                    lldpXdot1dcbxAdminPFCEnable[i]) return false;
    }

    return true;
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV0PfcStateMachineRun
 * \ingroup dcbx
 *
 * \desc            Executes state machine, to update operational state.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       local is the local-data (operational state) entry.
 *
 * \param[in]       admin is the admin-data (configuration) entry.
 *
 * \param[in]       remote is the remote-data entry (if state-machine is
 *                  triggered due to message from remote peer, otherwise NULL
 *                  should be specified).
 *
 * \param[in]       remoteMac is the remote peer MAC address (if state-machine
 *                  is triggered due to message from remote peer, otherwise 0
 *                  should be specified).
 *
 * \param[in]       adminChange indicates if the state-machine is triggered due
 *                  to local admin-data change.
 *
 * \param[out]      changed points to caller-allocated storage where this
 *                  function should place '1' the state-machine cycle changed 
 *                  the local-data or '0' if there was no change.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0PfcStateMachineRun(int portNum,
                             struct lldpXdot1dcbxLocalData  *local,
                             struct lldpXdot1dcbxAdminData  *admin,
                             struct lldpXdot1dcbxRemoteData *remote,
                             uint64_t remoteMac, bool adminChange, bool *changed)
{
    int status;
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config) 
        return LLDP_ERR_INVALID_ARG;

    if (remote) {
        /* update feature version */
        local->lldpXdot1dcbxLocPFC.dcbxV0LocPFCOperVersion = 
            MIN(DCBX_V0_PFC_MAX_VERSION, remote->lldpXdot1dcbxRemPFC.
                                         dcbxV0RemPFCMaxVersion);

        /* update feature synchronization error flag */
        local->lldpXdot1dcbxLocPFC.dcbxV0LocPFCError = 
            remote->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCWilling == 
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCWilling &&
            !dcbxV0PfcIsCompatible(admin, remote);
    }

    /* verify that remote data is valid */
    bool validRemote = remote &&
                       config->lldpXdot1dcbxConfigPFC.lldpXdot1dcbxConfigPFCTxEnable &&
                       !local->lldpXdot1dcbxLocPFC.dcbxV0LocPFCError &&
                       remote->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCValid &&
                       local->lldpXdot1dcbxLocPFC.dcbxV0LocPFCOperVersion ==
                       remote->lldpXdot1dcbxRemPFC.dcbxV0RemPFCOperVersion;

    status = dcbxV1PfcStateMachineRun(portNum, local, admin, validRemote ? remote : NULL, remoteMac, adminChange, changed);

    if (*changed) local->dcbxV0Changed = false;

    return status;
}

/*****************************************************************************/
/** dcbxV0PfcHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress PFC TLV.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlv is ingress TLV received on the local port.
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
int dcbxV0PfcHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      i;
    int      status;
    uint8_t  capability;
    bool     enabled[8];
    bool     changed = false;
    struct dcbxV0Feature feature;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    /* organizationally TLV sub-type */
    if (tlv->type != DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin || !remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV0DecodePfcTlv(tlv, &feature, &capability, enabled)) != LLDP_OK)
        return status;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCWilling = feature.willing;

    remote->lldpXdot1dcbxRemPFC.
            dcbxV0RemPFCError = feature.error;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCMBC = 0;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCCap = capability;

    for (i=0; i<8; i++)
        remote->lldpXdot1dcbxRemPFC.
                lldpXdot1dcbxRemPFCEnable[i] = enabled[i];

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCValid = feature.enable;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCStatsRxTLVs++;

    return dcbxV0PfcStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV0PfcHandleMissingTLVs
 * \ingroup dcbx
 *
 * \desc            Handles missing PFC TLV from DCBX TLVs 
 *                  chain.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       subType is the DCBX type of the missing TLV.
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
int dcbxV0PfcHandleMissingTLVs(int portNum, enum dcbxV0TlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;
    bool changed = false;

    /* organizationally TLV sub-type */
    if (subType != DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin)
        return LLDP_ERR_NOT_FOUND;

    /* mark remote entry as invalid */
    if (remote) remote->lldpXdot1dcbxRemPFC.
                        lldpXdot1dcbxRemPFCValid = false;

    return dcbxV0PfcStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV0PfcHandleRemoteLearn
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
int dcbxV0PfcHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0PfcHandleRemoteAging
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
int dcbxV0PfcHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    /* reset state-machines */
    dcbxV0PfcHandleMissingTLVs(portNum, DCBX_V0_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL, msap, 0, 0);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0PfcAppendEgressTLVs
 * \ingroup dcbx
 *
 * \desc            Appends PFC TLV to the egress TLVs chain.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvChain is egress TLV chain received on the local port.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0PfcAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    struct lldpTlv *tlv;

    /* get local-port pointers */
    struct lldpXdot1dcbxLocalData *local = dcbxLocalDataGet(portNum);
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config || !tlvChain) 
        return LLDP_ERR_INVALID_ARG;

    /* call physical layer to update data (if needed) */
    if (dcbxDB->physical &&
        dcbxDB->physical->updatePFCStatus)
        dcbxDB->physical->updatePFCStatus(portNum, &local->lldpXdot1dcbxLocPFC);

    if (config->lldpXdot1dcbxConfigPFC.lldpXdot1dcbxConfigPFCTxEnable) {
        struct dcbxV0Feature dcbxV0PfcFeature;

        /* setup feature state data */
        dcbxV0PfcFeature.operVersion = local->lldpXdot1dcbxLocPFC.
                                              dcbxV0LocPFCOperVersion;
        dcbxV0PfcFeature.maxVersion  = DCBX_V0_PFC_MAX_VERSION;
        dcbxV0PfcFeature.subType     = 0;
        dcbxV0PfcFeature.enable      = true;
        dcbxV0PfcFeature.willing     = local->lldpXdot1dcbxLocPFC.
                                              lldpXdot1dcbxLocPFCWilling;
        dcbxV0PfcFeature.error       = local->lldpXdot1dcbxLocPFC.
                                              dcbxV0LocPFCError;

        /* encode PFC TLVs, and insert it to chain */
        if (dcbxV0EncodePfcTlv(&tlv,
            &dcbxV0PfcFeature,
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCCap,            
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable) != LLDP_OK ||
            lldpAppendTlv(tlvChain, tlv) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending PFC TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCStatsTxTLVs++;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0PfcTerminate
 * \ingroup dcbx
 *
 * \desc            Initializes PFC DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0PfcInitialize(void)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0PfcTerminate
 * \ingroup dcbx
 *
 * \desc            Initializes PFC DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0PfcTerminate(void)
{
    return LLDP_OK;
}

