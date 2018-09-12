/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_ets.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of -
 *                    DCBX P802.1Qaz/D2.1 ETS.      
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

#include <dcbx_v1_tlv.h>
#include <dcbx_v1_ets.h>
#include <dcbx_v1_stm.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


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
/** dcbxV1EtsConHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress ETS-Configuration TLV.
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
static int dcbxV1EtsConHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      i;
    int      status;
    bool     willing;
    bool     cbs;
    uint8_t  max_tcs;
    uint8_t  pri_assignment[8];
    uint8_t  bandwidth[8];
    uint8_t  tsa_assignment[8];
    bool     changed = false;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin || !remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV1DecodeEtsConfigurationTlv(
                      tlv, &willing, &cbs, &max_tcs, 
                      pri_assignment, bandwidth, 
                      tsa_assignment)) != LLDP_OK) return status;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConWilling = willing;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConCreditBasedShaperSupport = cbs;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConTrafficClassesSupported = max_tcs;

    for (i=0; i<8; i++) {
        remote->lldpXdot1dcbxRemETSConfiguration.
                lldpXdot1dcbxRemETSConPriorityAssignmentTable[i] = pri_assignment[i];

        remote->lldpXdot1dcbxRemETSConfiguration.
                lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[i] = bandwidth[i];

        remote->lldpXdot1dcbxRemETSConfiguration.
                lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[i] = tsa_assignment[i];
    }

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConValid = true;

    remote->lldpXdot1dcbxRemETSConfiguration.
            lldpXdot1dcbxRemETSConStatsRxTLVs++;

    return dcbxV1EtsStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV1EtsRecoHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress ETS-Recommendation TLV.
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
static int dcbxV1EtsRecoHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      i;
    int      status;
    uint8_t  pri_assignment[8];
    uint8_t  bandwidth[8];
    uint8_t  tsa_assignment[8];
    bool     changed = false;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin || !remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV1DecodeEtsRecommendationTlv(
                     tlv, pri_assignment, bandwidth, 
                     tsa_assignment)) != LLDP_OK) return status;

    for (i=0; i<8; i++) {
        remote->lldpXdot1dcbxRemETSRecommendation.
                lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[i] = pri_assignment[i];

        remote->lldpXdot1dcbxRemETSRecommendation.
                lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[i] = bandwidth[i];

        remote->lldpXdot1dcbxRemETSRecommendation.
                lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[i] = tsa_assignment[i];
    }

    remote->lldpXdot1dcbxRemETSRecommendation.
            lldpXdot1dcbxRemETSRecoValid = true;

    remote->lldpXdot1dcbxRemETSRecommendation.
            lldpXdot1dcbxRemETSRecoStatsRxTLVs++;

    return dcbxV1EtsStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV1EtsStateMachineRun
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
int dcbxV1EtsStateMachineRun(int portNum,
                             struct lldpXdot1dcbxLocalData  *local,
                             struct lldpXdot1dcbxAdminData  *admin,
                             struct lldpXdot1dcbxRemoteData *remote,
                             uint64_t remoteMac, bool adminChange, bool *changed)
{
    int  i;
    bool validRemote;

    *changed = false;

    if (adminChange)
    {
        /* update non-negotiable variables */
        if (local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConCreditBasedShaperSupport !=
            admin->lldpXdot1dcbxAdminETSConfiguration.
                   lldpXdot1dcbxAdminETSConCreditBasedShaperSupport) {

            local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConCreditBasedShaperSupport =
            admin->lldpXdot1dcbxAdminETSConfiguration.
                   lldpXdot1dcbxAdminETSConCreditBasedShaperSupport;
            *changed = true;
        }

        if (local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConTrafficClassesSupported !=
            admin->lldpXdot1dcbxAdminETSConfiguration.
                   lldpXdot1dcbxAdminETSConTrafficClassesSupported) {

            local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConTrafficClassesSupported =
            admin->lldpXdot1dcbxAdminETSConfiguration.
                   lldpXdot1dcbxAdminETSConTrafficClassesSupported;
            *changed = true;
        }

        if (local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConWilling !=
            admin->lldpXdot1dcbxAdminETSConfiguration.
                   lldpXdot1dcbxAdminETSConWilling) {

            local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConWilling =
            admin->lldpXdot1dcbxAdminETSConfiguration.
                   lldpXdot1dcbxAdminETSConWilling;
            *changed = true;
        }

        for (i=0; i<8; i++) {
            if (local->lldpXdot1dcbxLocETSRecommendation.
                       lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[i] !=
                admin->lldpXdot1dcbxAdminETSRecommendation.
                       lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[i]) {

                local->lldpXdot1dcbxLocETSRecommendation.
                       lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[i] =
                admin->lldpXdot1dcbxAdminETSRecommendation.
                       lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[i];
                *changed = true;
            }

            if (local->lldpXdot1dcbxLocETSRecommendation.
                       lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[i] !=
                admin->lldpXdot1dcbxAdminETSRecommendation.
                       lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[i]) {

                local->lldpXdot1dcbxLocETSRecommendation.
                       lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[i] =
                admin->lldpXdot1dcbxAdminETSRecommendation.
                       lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[i];
                *changed = true;
            }

            if (local->lldpXdot1dcbxLocETSRecommendation.
                       lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[i] !=
                admin->lldpXdot1dcbxAdminETSRecommendation.
                       lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[i]) {

                local->lldpXdot1dcbxLocETSRecommendation.
                       lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[i] =
                admin->lldpXdot1dcbxAdminETSRecommendation.
                       lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[i];
                *changed = true;
            }
        }

        for (i=0; i<8; i++) {
            if (local->lldpXdot1dcbxLocETSConfiguration.
                       lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[i] !=
                admin->lldpXdot1dcbxAdminETSConfiguration.
                       lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[i]) {

                local->lldpXdot1dcbxLocETSConfiguration.
                       lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[i] =
                admin->lldpXdot1dcbxAdminETSConfiguration.
                       lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[i];
                *changed = true;
            }

            if (local->lldpXdot1dcbxLocETSConfiguration.
                       lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[i] !=
                admin->lldpXdot1dcbxAdminETSConfiguration.
                       lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[i]) {

                local->lldpXdot1dcbxLocETSConfiguration.
                       lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[i] =
                admin->lldpXdot1dcbxAdminETSConfiguration.
                       lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[i];
                *changed = true;
            }

            if (local->lldpXdot1dcbxLocETSConfiguration.
                       lldpXdot1dcbxLocETSConPriorityAssignmentTable[i] !=
                admin->lldpXdot1dcbxAdminETSConfiguration.
                       lldpXdot1dcbxAdminETSConPriorityAssignmentTable[i]) {

                local->lldpXdot1dcbxLocETSConfiguration.
                       lldpXdot1dcbxLocETSConPriorityAssignmentTable[i] =
                admin->lldpXdot1dcbxAdminETSConfiguration.
                       lldpXdot1dcbxAdminETSConPriorityAssignmentTable[i];
                *changed = true;
            }
        }
    }

    /* check that remote is valid */
    validRemote = remote && 
                  remote->lldpXdot1dcbxRemETSRecommendation.lldpXdot1dcbxRemETSRecoValid;

    /* update negotiable variables */
    for (i=0; i<8; i++) {
        bool localChanged = false;

        dcbxV1StmAsymmetricI(
            /* local willing */
            local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConWilling,
            /* remote willing */
            validRemote ?
            remote->lldpXdot1dcbxRemETSConfiguration.
                    lldpXdot1dcbxRemETSConWilling : false,
            /* operation parameter */
            &local->lldpXdot1dcbxLocETSConfiguration.
                    lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[i],
            /* admin parameter */
            &admin->lldpXdot1dcbxAdminETSConfiguration.
                    lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[i],
            /* remote parameter */
            validRemote ?
            &remote->lldpXdot1dcbxRemETSRecommendation.
                     lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[i] : NULL,
            /* local changed */
            localChanged);

        if (localChanged)
            *changed = true;

        dcbxV1StmAsymmetricI(
            /* local willing */
            local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConWilling,
            /* remote willing */
            validRemote ?
            remote->lldpXdot1dcbxRemETSConfiguration.
                    lldpXdot1dcbxRemETSConWilling : false,
            /* operation parameter */
            &local->lldpXdot1dcbxLocETSConfiguration.
                    lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[i],
            /* admin parameter */
            &admin->lldpXdot1dcbxAdminETSConfiguration.
                    lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[i],
            /* remote parameter */
            validRemote ?
            &remote->lldpXdot1dcbxRemETSRecommendation.
                     lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[i] : NULL,
            /* local changed */
            localChanged);

        if (localChanged)
            *changed = true;

        dcbxV1StmAsymmetricI(
            /* local willing */
            local->lldpXdot1dcbxLocETSConfiguration.
                   lldpXdot1dcbxLocETSConWilling,
            /* remote willing */
            validRemote ?
            remote->lldpXdot1dcbxRemETSConfiguration.
                    lldpXdot1dcbxRemETSConWilling : false,
            /* operation parameter */
            &local->lldpXdot1dcbxLocETSConfiguration.
                    lldpXdot1dcbxLocETSConPriorityAssignmentTable[i],
            /* admin parameter */
            &admin->lldpXdot1dcbxAdminETSConfiguration.
                    lldpXdot1dcbxAdminETSConPriorityAssignmentTable[i],
            /* remote parameter */
            validRemote ?
            &remote->lldpXdot1dcbxRemETSRecommendation.
                     lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[i] : NULL,
            /* local changed */
            localChanged);

        if (localChanged)
            *changed = true;
    }

    if (*changed && dcbxDB->physical &&
        dcbxDB->physical->applyETSSettings)
        dcbxDB->physical->applyETSSettings(portNum, &local->lldpXdot1dcbxLocETSConfiguration);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EtsHandleIngressTLVs
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
int dcbxV1EtsHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    /* organizationally TLV sub-type */
    enum dcbxTlvSubType subType = (enum dcbxTlvSubType)tlv->data[3];

    switch (subType) {
        case DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION: 
            return dcbxV1EtsConHandleIngressTLVs(portNum, tlv, msap, remoteMac, rxTTL);
        case DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION:
            return dcbxV1EtsRecoHandleIngressTLVs(portNum, tlv, msap, remoteMac, rxTTL);
        default:
            return LLDP_ERR_UNKNOWN;
    }
}

/*****************************************************************************/
/** dcbxV1EtsHandleMissingTLVs
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
int dcbxV1EtsHandleMissingTLVs(int portNum, enum dcbxTlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;
    bool changed = false;

    /* organizationally TLV sub-type */
    if (subType != DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION &&
        subType != DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin)
        return LLDP_ERR_NOT_FOUND;

    switch (subType) {
        case DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION: 
            /* mark remote entry as invalid */
            if (remote) remote->lldpXdot1dcbxRemETSConfiguration.
                                lldpXdot1dcbxRemETSConValid = false;

            return dcbxV1EtsStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
        case DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION:
            /* mark remote entry as invalid */
            if (remote) remote->lldpXdot1dcbxRemETSRecommendation.
                                lldpXdot1dcbxRemETSRecoValid = false;

            return dcbxV1EtsStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
        default:
            return LLDP_ERR_UNKNOWN;
    }
}

/*****************************************************************************/
/** dcbxV1EtsHandleRemoteLearn
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
int dcbxV1EtsHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EtsHandleRemoteAging
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
int dcbxV1EtsHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    /* reset state-machines */
    dcbxV1EtsHandleMissingTLVs(portNum, DCBX_V1_TLV_SUBTYPE_ETS_CONFIGURATION, msap, 0, 0);
    dcbxV1EtsHandleMissingTLVs(portNum, DCBX_V1_TLV_SUBTYPE_ETS_RECOMMENDATION, msap, 0, 0);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EtsAppendEgressTLVs
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
int dcbxV1EtsAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
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
        /* encode ETS Configuration TLVs, and insert it to chain */
        if (dcbxV1EncodeEtsConfigurationTlv(&tlv,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConWilling,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConCreditBasedShaperSupport,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConTrafficClassesSupported,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConPriorityAssignmentTable,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConTrafficClassBandwidthTable,
            local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable) != LLDP_OK ||
            lldpInsertBeforeTlv(tlvChain, tlv, LLDP_TLV_TYPE_END_OF_LLDPDU) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending ETS-Configuration TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocETSConfiguration.lldpXdot1dcbxLocETSConStatsTxTLVs++;
    }

    if (config->lldpXdot1dcbxConfigETSRecommendation.lldpXdot1dcbxConfigETSRecommendationTxEnable) {
        /* encode ETS Recommendation TLVs, and insert it to chain */
        if (dcbxV1EncodeEtsRecommendationTlv(&tlv,
            local->lldpXdot1dcbxLocETSRecommendation.lldpXdot1dcbxLocETSRecoPriorityAssignmentTable,
            local->lldpXdot1dcbxLocETSRecommendation.lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable,
            local->lldpXdot1dcbxLocETSRecommendation.lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable) != LLDP_OK ||
            lldpInsertBeforeTlv(tlvChain, tlv, LLDP_TLV_TYPE_END_OF_LLDPDU) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending ETS-Recommendation TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocETSRecommendation.lldpXdot1dcbxLocETSRecoStatsTxTLVs++;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EtsInitialize
 * \ingroup dcbx
 *
 * \desc            Initializes ETS DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EtsInitialize(void)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1EtsTerminate
 * \ingroup dcbx
 *
 * \desc            Terminates ETS DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1EtsTerminate(void)
{
    return LLDP_OK;
}

