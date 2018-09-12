/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_pfc.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of -
 *                    DCBX P802.1Qaz/D2.1 PFC.
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
#include <dcbx_v1_pfc.h>
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


/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxV1PfcStateMachineRun
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
int dcbxV1PfcStateMachineRun(int portNum,
                             struct lldpXdot1dcbxLocalData  *local,
                             struct lldpXdot1dcbxAdminData  *admin,
                             struct lldpXdot1dcbxRemoteData *remote,
                             uint64_t remoteMac, bool adminChange, bool *changed)
{
    int i;
    bool validRemote;
    uint64_t localMac;

    *changed = false;

    /* update non-negotiable variables */
    if (adminChange)
    {
        if (local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCWilling != 
            admin->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCWilling) {

            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCWilling = 
            admin->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCWilling;
            *changed = true;
        }

        if (local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCMBC != 
            admin->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCMBC) {

            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCMBC = 
            admin->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCMBC;
            *changed = true;
        }

        if (local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCCap !=
            admin->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCCap) {

            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCCap =
            admin->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCCap;
            *changed = true;
        }
    }

    /* get local chassis MAC address */
    localMac = lldpDB->lldpConfiguration.chassisMacAddress;

    /* check that remote is valid */
    validRemote = remote &&
                  remote->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCValid;

    if (validRemote) {
        /* check how many PFCs are enabled in remote configuration */
        int remote_pfc_num = 0;
        for (i=0; i<8; i++)
            if (remote->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[i]) remote_pfc_num++;

        /* verify that local machine can apply remote settings. if not invalidate remote. */
        if (remote_pfc_num > local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCCap) validRemote = false;
     }

    /* update negotiable variables */
    for (i=0; i<8; i++) {
        bool localChanged = false;

        dcbxV1StmSymmetricI(
            /* local willing */
            local->lldpXdot1dcbxLocPFC.
                   lldpXdot1dcbxLocPFCWilling,
            /* remote willing */
            validRemote ?
            remote->lldpXdot1dcbxRemPFC.
                    lldpXdot1dcbxRemPFCWilling : false,
            /* local mac */
            localMac,
            /* remote mac */
            remoteMac,
            /* operation parameter */
            &local->lldpXdot1dcbxLocPFC.
                    lldpXdot1dcbxLocPFCEnable[i],
            /* admin parameter */
            &admin->lldpXdot1dcbxAdminPFC.
                    lldpXdot1dcbxAdminPFCEnable[i],
            /* remote parameter */
            validRemote ?
            &remote->lldpXdot1dcbxRemPFC.
                     lldpXdot1dcbxRemPFCEnable[i] : NULL,
            /* local changed */
            localChanged);

        if (localChanged)
            *changed = true;
    }

    if (*changed && dcbxDB->physical &&
        dcbxDB->physical->applyPFCSettings)
        dcbxDB->physical->applyPFCSettings(portNum, &local->lldpXdot1dcbxLocPFC);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1PfcHandleIngressTLVs
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
int dcbxV1PfcHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      i;
    int      status;
    bool     willing;
    bool     mbc;
    uint8_t  capability;
    bool     enabled[8];
    bool     changed = false;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    /* organizationally TLV sub-type */
    enum dcbxTlvSubType subType = (enum dcbxTlvSubType)tlv->data[3];
    if (subType != DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin || !remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV1DecodePfcTlv(tlv, &willing, &mbc, &capability, enabled)) != LLDP_OK)
        return status;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCWilling = willing;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCMBC = mbc;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCCap = capability;

    for (i=0; i<8; i++)
        remote->lldpXdot1dcbxRemPFC.
                lldpXdot1dcbxRemPFCEnable[i] = enabled[i];

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCValid = true;

    remote->lldpXdot1dcbxRemPFC.
            lldpXdot1dcbxRemPFCStatsRxTLVs++;

    return dcbxV1PfcStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV1PfcHandleMissingTLVs
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
int dcbxV1PfcHandleMissingTLVs(int portNum, enum dcbxTlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;
    bool changed = false;

    /* organizationally TLV sub-type */
    if (subType != DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin)
        return LLDP_ERR_NOT_FOUND;

    /* mark remote entry as invalid */
    if (remote) remote->lldpXdot1dcbxRemPFC.
                        lldpXdot1dcbxRemPFCValid = false;

    return dcbxV1PfcStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV1PfcHandleRemoteLearn
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
int dcbxV1PfcHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1PfcHandleRemoteAging
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
int dcbxV1PfcHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    /* reset state-machines */
    dcbxV1PfcHandleMissingTLVs(portNum, DCBX_V1_TLV_SUBTYPE_PRIORITY_BASED_FLOW_CONTROL, msap, 0, 0);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1PfcAppendEgressTLVs
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
int dcbxV1PfcAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
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
        /* encode PFC TLVs, and insert it to chain */
        if (dcbxV1EncodePfcTlv(&tlv,
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCWilling,
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCMBC,
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCCap,            
            local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable) != LLDP_OK ||
            lldpInsertBeforeTlv(tlvChain, tlv, LLDP_TLV_TYPE_END_OF_LLDPDU) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending PFC TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCStatsTxTLVs++;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1PfcTerminate
 * \ingroup dcbx
 *
 * \desc            Initializes PFC DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1PfcInitialize(void)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1PfcTerminate
 * \ingroup dcbx
 *
 * \desc            Initializes PFC DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1PfcTerminate(void)
{
    return LLDP_OK;
}

