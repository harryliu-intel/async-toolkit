/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v0_ets.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of -
 *                    DCBX Base Protocol v1.01 ETS (PG). 
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
#include <dcbx_v0_ets.h>
#include <dcbx_v1_ets.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define DCBX_V0_ETS_MAX_VERSION 0


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
/** dcbxV0EtsIsCompatible
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
static bool dcbxV0EtsIsCompatible(struct lldpXdot1dcbxAdminData  *admin,
                                  struct lldpXdot1dcbxRemoteData *remote)
{
    int i;

    for (i=0; i<8; i++) {
        if (remote->lldpXdot1dcbxRemETSConfiguration.
                    lldpXdot1dcbxRemETSConPriorityAssignmentTable[i] !=
             admin->lldpXdot1dcbxAdminETSConfiguration.
                    lldpXdot1dcbxAdminETSConPriorityAssignmentTable[i]) return false;

        if (remote->lldpXdot1dcbxRemETSConfiguration.
                    lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[i] !=
             admin->lldpXdot1dcbxAdminETSConfiguration.
                    lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[i]) return false;
    }

    return true;
}

/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV0EtsStateMachineRun
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
int dcbxV0EtsStateMachineRun(int portNum,
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
        local->lldpXdot1dcbxLocETSConfiguration.dcbxV0LocETSConOperVersion = 
            MIN(DCBX_V0_ETS_MAX_VERSION, remote->lldpXdot1dcbxRemETSConfiguration.
                                         dcbxV0RemETSConMaxVersion);

        /* update feature synchronization error flag */
        local->lldpXdot1dcbxLocETSConfiguration.dcbxV0LocETSConError = 
            remote->lldpXdot1dcbxRemETSConfiguration.lldpXdot1dcbxRemETSConWilling == 
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConWilling &&
            !dcbxV0EtsIsCompatible(admin, remote);
    }

    /* verify that remote data is valid */
    bool validRemote = remote &&
                       config->lldpXdot1dcbxConfigETSConfiguration.lldpXdot1dcbxConfigETSConfigurationTxEnable &&
                       !local->lldpXdot1dcbxLocETSConfiguration.dcbxV0LocETSConError &&
                       remote->lldpXdot1dcbxRemETSConfiguration.lldpXdot1dcbxRemETSConValid &&
                       local->lldpXdot1dcbxLocETSConfiguration.dcbxV0LocETSConOperVersion ==
                       remote->lldpXdot1dcbxRemETSConfiguration.dcbxV0RemETSConOperVersion;

    status = dcbxV1EtsStateMachineRun(portNum, local, admin, validRemote ? remote : NULL, remoteMac, adminChange, changed);

    if (*changed) local->dcbxV0Changed = false;

    return status;
}

/*****************************************************************************/
/** dcbxV0EtsHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress ETS TLV.
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
int dcbxV0EtsHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      i;
    int      status;
    uint8_t  max_tcs;
    uint8_t  pri_assignment[8];
    uint8_t  bandwidth[8];
    bool     changed = false;
    struct dcbxV0Feature feature;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin || !remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV0DecodeEtsTlv(
                      tlv, &feature, &max_tcs, 
                      pri_assignment, bandwidth)) != LLDP_OK) return status;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConWilling = feature.willing;

    remote->lldpXdot1dcbxRemETSConfiguration.
            dcbxV0RemETSConError = feature.error;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConTrafficClassesSupported = max_tcs;

    remote->lldpXdot1dcbxRemETSConfiguration.
            dcbxV0RemETSConOperVersion = feature.operVersion;

    remote->lldpXdot1dcbxRemETSConfiguration.
            dcbxV0RemETSConMaxVersion = feature.maxVersion;

    remote->lldpXdot1dcbxRemETSConfiguration.
            dcbxV0RemETSConError = feature.error;

    for (i=0; i<8; i++) {
        remote->lldpXdot1dcbxRemETSConfiguration.
                lldpXdot1dcbxRemETSConPriorityAssignmentTable[i] = pri_assignment[i];

        remote->lldpXdot1dcbxRemETSRecommendation.
                lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[i] = pri_assignment[i];

        remote->lldpXdot1dcbxRemETSConfiguration.
                lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[i] = bandwidth[i];

        remote->lldpXdot1dcbxRemETSRecommendation.
                lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[i] = bandwidth[i];
    }

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConValid = feature.enable;

    remote->lldpXdot1dcbxRemETSRecommendation.
            lldpXdot1dcbxRemETSRecoValid = feature.enable;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConStatsRxTLVs++;

    return dcbxV0EtsStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV0EtsHandleMissingTLVs
 * \ingroup dcbx
 *
 * \desc            Handles missing ETS TLV from DCBX TLVs 
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
int dcbxV0EtsHandleMissingTLVs(int portNum, enum dcbxV0TlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;
    bool changed = false;

    /* organizationally TLV sub-type */
    if (subType != DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin)
        return LLDP_ERR_NOT_FOUND;

    /* mark remote entry as invalid */
    if (remote) remote->lldpXdot1dcbxRemETSConfiguration.
                        lldpXdot1dcbxRemETSConValid = false;

    return dcbxV0EtsStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV0EtsHandleRemoteLearn
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
int dcbxV0EtsHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EtsHandleRemoteAging
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
int dcbxV0EtsHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    /* reset state-machines */
    dcbxV0EtsHandleMissingTLVs(portNum, DCBX_V0_TLV_SUBTYPE_PRIORITY_GROUPS, msap, 0, 0);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EtsAppendEgressTLVs
 * \ingroup dcbx
 *
 * \desc            Appends ETS TLV to the egress TLVs chain.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvChain is egress TLV chain received on the local port.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EtsAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    struct lldpTlv *tlv;

    /* get local-port pointers */
    struct lldpXdot1dcbxLocalData *local = dcbxLocalDataGet(portNum);
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config || !tlvChain) 
        return LLDP_ERR_INVALID_ARG;

    /* call physical layer to update data (if needed) */
    if (dcbxDB->physical &&
        dcbxDB->physical->updateETSStatus)
        dcbxDB->physical->updateETSStatus(portNum, &local->lldpXdot1dcbxLocETSConfiguration);

    if (config->lldpXdot1dcbxConfigETSConfiguration.lldpXdot1dcbxConfigETSConfigurationTxEnable) {
        struct dcbxV0Feature dcbxV0EtsFeature;

        /* setup feature state data */
        dcbxV0EtsFeature.operVersion = local->lldpXdot1dcbxLocETSConfiguration.
                                              dcbxV0LocETSConOperVersion;
        dcbxV0EtsFeature.maxVersion  = DCBX_V0_ETS_MAX_VERSION;
        dcbxV0EtsFeature.subType     = 0;
        dcbxV0EtsFeature.enable      = true;
        dcbxV0EtsFeature.willing     = local->lldpXdot1dcbxLocETSConfiguration.
                                              lldpXdot1dcbxLocETSConWilling;
        dcbxV0EtsFeature.error       = local->lldpXdot1dcbxLocETSConfiguration.
                                              dcbxV0LocETSConError;

        /* encode Priority-Groups TLVs, and insert it to chain */
        if (dcbxV0EncodeEtsTlv(&tlv,
            &dcbxV0EtsFeature,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConTrafficClassesSupported,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConPriorityAssignmentTable,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConTrafficClassBandwidthTable) != LLDP_OK ||
            lldpAppendTlv(tlvChain, tlv) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending PG TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConStatsTxTLVs++;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EtsInitialize
 * \ingroup dcbx
 *
 * \desc            Initializes ETS DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EtsInitialize(void)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV0EtsTerminate
 * \ingroup dcbx
 *
 * \desc            Terminates ETS DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV0EtsTerminate(void)
{
    return LLDP_OK;
}

