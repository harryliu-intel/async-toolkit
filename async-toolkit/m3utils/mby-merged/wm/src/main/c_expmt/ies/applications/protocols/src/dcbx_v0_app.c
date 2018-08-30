/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v0_app.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of -
 *                    DCBX Base Protocol v1.01 Application-Priority.    
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
#include <dcbx_v0_app.h>
#include <dcbx_v1_app.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define DCBX_V0_APP_MAX_VERSION 0


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
/** dcbxV0AppIsCompatible
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
static bool dcbxV0AppIsCompatible(struct lldpXdot1dcbxAdminData  *admin,
                                  struct lldpXdot1dcbxRemoteData *remote)
{
    int i, j, n = 0;

    if (remote->lldpXdot1dcbxRemApplicationPriority.
                lldpXdot1dcbxRemApplicationPriorityAEPriorityNum !=
         admin->lldpXdot1dcbxAdminApplicationPriority.
                lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum) return false;

    for (i=0; i<admin->lldpXdot1dcbxAdminApplicationPriority.
                       lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum; i++) {
        bool match = false;
        for (j=n; j<remote->lldpXdot1dcbxRemApplicationPriority.
                            lldpXdot1dcbxRemApplicationPriorityAEPriorityNum && !match; j++) {
            match = (admin->lldpXdot1dcbxAdminApplicationPriority.
                            lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                            lldpXdot1dcbxAdminApplicationPriorityAESelector ==
                    remote->lldpXdot1dcbxRemApplicationPriority.
                            lldpXdot1dcbxRemApplicationPriorityAEPriority[j].
                            lldpXdot1dcbxRemApplicationPriorityAESelector) &&
                    (admin->lldpXdot1dcbxAdminApplicationPriority.
                            lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                            lldpXdot1dcbxAdminApplicationPriorityAEProtocol ==
                    remote->lldpXdot1dcbxRemApplicationPriority.
                            lldpXdot1dcbxRemApplicationPriorityAEPriority[j].
                            lldpXdot1dcbxRemApplicationPriorityAEProtocol) &&
                    (admin->lldpXdot1dcbxAdminApplicationPriority.
                            lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                            lldpXdot1dcbxAdminApplicationPriorityAEPriority ==
                    remote->lldpXdot1dcbxRemApplicationPriority.
                            lldpXdot1dcbxRemApplicationPriorityAEPriority[j].
                            lldpXdot1dcbxRemApplicationPriorityAEPriority);
        }
        if (!match) return false;
        else if (j==n) n++;
    }

    return true;
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV0AppStateMachineRun
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
int dcbxV0AppStateMachineRun(int portNum,
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
        local->lldpXdot1dcbxLocApplicationPriority.dcbxV0LocApplicationPriorityOperVersion = 
            MIN(DCBX_V0_APP_MAX_VERSION, remote->lldpXdot1dcbxRemApplicationPriority.
                                         dcbxV0RemApplicationPriorityMaxVersion);

        /* update feature synchronization error flag */
        local->lldpXdot1dcbxLocApplicationPriority.dcbxV0LocApplicationPriorityError = 
            remote->lldpXdot1dcbxRemApplicationPriority.lldpXdot1dcbxRemApplicationPriorityWilling == 
            local->lldpXdot1dcbxLocApplicationPriority.lldpXdot1dcbxLocApplicationPriorityWilling &&
            !dcbxV0AppIsCompatible(admin, remote);
    }
    else {
        /* update feature synchronization error flag */
        local->lldpXdot1dcbxLocApplicationPriority.dcbxV0LocApplicationPriorityError =
            config->lldpXdot1dcbxConfigApplicationPriority.lldpXdot1dcbxConfigApplicationPriorityTxEnable; 
    }

    /* verify that remote data is valid */
    bool validRemote = remote &&
                       config->lldpXdot1dcbxConfigApplicationPriority.lldpXdot1dcbxConfigApplicationPriorityTxEnable &&
                       !local->lldpXdot1dcbxLocApplicationPriority.dcbxV0LocApplicationPriorityError &&
                       remote->lldpXdot1dcbxRemApplicationPriority.lldpXdot1dcbxRemApplicationPriorityValid &&
                       local->lldpXdot1dcbxLocApplicationPriority.dcbxV0LocApplicationPriorityOperVersion ==
                       remote->lldpXdot1dcbxRemApplicationPriority.dcbxV0RemApplicationPriorityOperVersion;

    status = dcbxV1AppStateMachineRun(portNum, local, admin, validRemote ? remote : NULL, remoteMac, adminChange, changed);

    if (*changed) local->dcbxV0Changed = false;

    return status;
}

/*****************************************************************************/
/** dcbxV0AppHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress Application-Priority TLV.
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
int dcbxV0AppHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      status;
    uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES];
    size_t   i, size;
    bool     changed = false;
    struct dcbxV0Feature feature;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    /* organizationally TLV sub-type */
    if (tlv->type != DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV0DecodeApplicationPriorityTlv(tlv, &feature, apcode, &size)) != LLDP_OK)
        return status;

    remote->lldpXdot1dcbxRemApplicationPriority.
            lldpXdot1dcbxRemApplicationPriorityWilling = feature.willing;

    remote->lldpXdot1dcbxRemApplicationPriority.
            dcbxV0RemApplicationPriorityError = feature.error;

    for (i=0; i<size; i++) {
        remote->lldpXdot1dcbxRemApplicationPriority.
                lldpXdot1dcbxRemApplicationPriorityAEPriority[i].
                lldpXdot1dcbxRemApplicationPriorityAESelector = DCBX_TLV_APCODE_SEL(apcode[i]) == 0 ? 1 : 4;

        remote->lldpXdot1dcbxRemApplicationPriority.
                lldpXdot1dcbxRemApplicationPriorityAEPriority[i].
                lldpXdot1dcbxRemApplicationPriorityAEProtocol = DCBX_TLV_APCODE_PROTO(apcode[i]);

        remote->lldpXdot1dcbxRemApplicationPriority.
                lldpXdot1dcbxRemApplicationPriorityAEPriority[i].
                lldpXdot1dcbxRemApplicationPriorityAEPriority = DCBX_TLV_APCODE_PRI(apcode[i]);
    }

    remote->lldpXdot1dcbxRemApplicationPriority.
            lldpXdot1dcbxRemApplicationPriorityAEPriorityNum = size;

    remote->lldpXdot1dcbxRemApplicationPriority.
            lldpXdot1dcbxRemApplicationPriorityValid = feature.enable;

    remote->lldpXdot1dcbxRemApplicationPriority.
            lldpXdot1dcbxRemApplicationPriorityStatsRxTLVs++;

    return dcbxV0AppStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV0AppHandleMissingTLVs
 * \ingroup dcbx
 *
 * \desc            Handles missing Application-Priority TLV from DCBX TLVs 
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
int dcbxV0AppHandleMissingTLVs(int portNum, enum dcbxV0TlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;
    bool changed = false;

    /* organizationally TLV sub-type */
    if (subType != DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin)
        return LLDP_ERR_NOT_FOUND;

    /* mark remote entry as invalid */
    if (remote) remote->lldpXdot1dcbxRemApplicationPriority.
                        lldpXdot1dcbxRemApplicationPriorityValid = false;

    return dcbxV0AppStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
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
int dcbxV0AppHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0AppHandleRemoteAging
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
int dcbxV0AppHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    /* reset state-machines */
    dcbxV0AppHandleMissingTLVs(portNum, DCBX_V0_TLV_SUBTYPE_APPLICATION_PRIORITY, msap, 0, 0);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0AppAppendEgressTLVs
 * \ingroup dcbx
 *
 * \desc            Appends Application-Priority TLV to the egress TLVs chain.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvChain is egress TLV chain received on the local port.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0AppAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    struct lldpTlv *tlv;

    size_t   i, size;
    uint32_t apcode[DCBX_TLV_MAX_APPLICATION_PRIORITIES];

    /* get local-port pointers */
    struct lldpXdot1dcbxLocalData *local = dcbxLocalDataGet(portNum);
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config || !tlvChain) 
        return LLDP_ERR_INVALID_ARG;

    /* call physical layer to update data (if needed) */
    if (dcbxDB->physical &&
        dcbxDB->physical->updateApplicationPriorityStatus)
        dcbxDB->physical->updateApplicationPriorityStatus(portNum, &local->lldpXdot1dcbxLocApplicationPriority);

    if (config->lldpXdot1dcbxConfigApplicationPriority.lldpXdot1dcbxConfigApplicationPriorityTxEnable) {
        size = local->lldpXdot1dcbxLocApplicationPriority.
                      lldpXdot1dcbxLocApplicationPriorityAEPriorityNum;

        for (i=0; i<size; i++) {
            apcode[i] = DCBX_TLV_APCODE(
                            local->lldpXdot1dcbxLocApplicationPriority.
                                   lldpXdot1dcbxLocApplicationPriorityAEPriority[i].
                                   lldpXdot1dcbxLocApplicationPriorityAESelector == 1 ? 0 : 1,
                            local->lldpXdot1dcbxLocApplicationPriority.
                                   lldpXdot1dcbxLocApplicationPriorityAEPriority[i].
                                   lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                            local->lldpXdot1dcbxLocApplicationPriority.
                                   lldpXdot1dcbxLocApplicationPriorityAEPriority[i].
                                   lldpXdot1dcbxLocApplicationPriorityAEPriority);
        }

        struct dcbxV0Feature dcbxV0AppFeature;

        /* setup feature state data */
        dcbxV0AppFeature.operVersion = local->lldpXdot1dcbxLocApplicationPriority.
                                              dcbxV0LocApplicationPriorityOperVersion;
        dcbxV0AppFeature.maxVersion  = DCBX_V0_APP_MAX_VERSION;
        dcbxV0AppFeature.subType     = 0;
        dcbxV0AppFeature.enable      = true;
        dcbxV0AppFeature.willing     = local->lldpXdot1dcbxLocApplicationPriority.
                                              lldpXdot1dcbxLocApplicationPriorityWilling;
        dcbxV0AppFeature.error       = local->lldpXdot1dcbxLocApplicationPriority.
                                              dcbxV0LocApplicationPriorityError;

        /* encode application-priority TLVs, and insert it to chain */
        if (dcbxV0EncodeApplicationPriorityTlv(&tlv, &dcbxV0AppFeature, apcode, size) != LLDP_OK ||
            lldpAppendTlv(tlvChain, tlv) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending Application-Priority TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocApplicationPriority.lldpXdot1dcbxLocApplicationPriorityStatsTxTLVs++;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0AppInitialize
 * \ingroup dcbx
 *
 * \desc            Initializes Application-Priority DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0AppInitialize(void)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0AppTerminate
 * \ingroup dcbx
 *
 * \desc            Terminates Application-Priority DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0AppTerminate(void)
{
    return LLDP_OK;
}

