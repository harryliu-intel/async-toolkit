/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_cn.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of -
 *                    DCBX P802.1Qaz/D2.1 Congestion-Notification.   
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
#include <dcbx_v1_cn.h>
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
/** dcbxV1CnStateMachineRun
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
int dcbxV1CnStateMachineRun(int portNum,
                            struct lldpXdot1dcbxLocalData  *local,
                            struct lldpXdot1dcbxAdminData  *admin,
                            struct lldpXdot1dcbxRemoteData *remote,
                            uint64_t remoteMac, bool adminChange, bool *changed)
{
    int i;

    *changed = false;

    if (adminChange)
    {
        /* update non-negotiable variables */
        for (i=0; i<8; i++) {
            if (local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvSupported[i] !=
                admin->lldpXdot1dcbxAdminCongestionNotification.lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[i]) {

                local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvSupported[i] =
                admin->lldpXdot1dcbxAdminCongestionNotification.lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[i];
                *changed = true;
            }

            if (local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvReady[i] !=
                admin->lldpXdot1dcbxAdminCongestionNotification.lldpXdot1dcbxAdminCongestionNotificationCnpvReady[i]) {

                local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvReady[i] =
                admin->lldpXdot1dcbxAdminCongestionNotification.lldpXdot1dcbxAdminCongestionNotificationCnpvReady[i];
                *changed = true;
            }
        }
    }

    if (*changed && dcbxDB->physical &&
        dcbxDB->physical->applyCongestionNotificationSettings)
        dcbxDB->physical->applyCongestionNotificationSettings(portNum, &local->lldpXdot1dcbxLocCongestionNotification);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1CnHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress Congestion-Notification TLV.
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
int dcbxV1CnHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int      i;
    int      status;
    bool     cnpv_supported[8];
    bool     cnpv_ready[8];
    bool     changed = false;

    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;

    /* organizationally TLV sub-type */
    enum dcbxTlvSubType subType = (enum dcbxTlvSubType)tlv->data[3];
    if (subType != DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin || !remote)
        return LLDP_ERR_NOT_FOUND;

    if ((status = dcbxV1DecodeCnTlv(tlv, cnpv_supported, cnpv_ready)) != LLDP_OK)
        return status;

    for (i=0; i<8; i++) {
        remote->lldpXdot1dcbxRemCongestionNotification.
                lldpXdot1dcbxRemCongestionNotificationCnpvSupported[i] = cnpv_supported[i];

        remote->lldpXdot1dcbxRemCongestionNotification.
                lldpXdot1dcbxRemCongestionNotificationCnpvReady[i] = cnpv_ready[i];
    }

    remote->lldpXdot1dcbxRemCongestionNotification.
            lldpXdot1dcbxRemCongestionNotificationValid = true;

    remote->lldpXdot1dcbxRemCongestionNotification.
            lldpXdot1dcbxRemCongestionNotificationStatsRxTLVs++;

    return dcbxV1CnStateMachineRun(portNum, local, admin, remote, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV1CnHandleMissingTLVs
 * \ingroup dcbx
 *
 * \desc            Handles missing Congestion-Notification TLV from DCBX TLVs 
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
int dcbxV1CnHandleMissingTLVs(int portNum, enum dcbxTlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    struct lldpXdot1dcbxLocalData  *local;
    struct lldpXdot1dcbxAdminData  *admin;
    struct lldpXdot1dcbxRemoteData *remote;
    bool changed = false;

    /* organizationally TLV sub-type */
    if (subType != DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION)
        return LLDP_ERR_UNKNOWN;

    local  = dcbxLocalDataGet(portNum);
    admin  = dcbxAdminDataGet(portNum);
    remote = dcbxRemoteDataGet(portNum);

    if (!local || !admin)
        return LLDP_ERR_NOT_FOUND;

    /* mark remote entry as invalid */
    if (remote) remote->lldpXdot1dcbxRemCongestionNotification.
                        lldpXdot1dcbxRemCongestionNotificationValid = false;

    return dcbxV1CnStateMachineRun(portNum, local, admin, NULL, remoteMac, false, &changed);
}

/*****************************************************************************/
/** dcbxV1CnHandleRemoteLearn
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
int dcbxV1CnHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1CnHandleRemoteAging
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
int dcbxV1CnHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    /* reset state-machines */
    dcbxV1CnHandleMissingTLVs(portNum, DCBX_V1_TLV_SUBTYPE_CONGESTION_NOTIFICATION, msap, 0, 0);

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1CnAppendEgressTLVs
 * \ingroup dcbx
 *
 * \desc            Appends Congestion-Notification TLV to the egress TLVs 
 *                  chain.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvChain is egress TLV chain received on the local port.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1CnAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    struct lldpTlv *tlv;

    /* get local-port pointers */
    struct lldpXdot1dcbxLocalData *local = dcbxLocalDataGet(portNum);
    struct lldpXdot1dcbxConfig *config = dcbxConfigGet(portNum);

    if (!local || !config || !tlvChain) 
        return LLDP_ERR_INVALID_ARG;

    /* call physical layer to update data (if needed) */
    if (dcbxDB->physical &&
        dcbxDB->physical->updateCongestionNotificationStatus)
        dcbxDB->physical->updateCongestionNotificationStatus(portNum, &local->lldpXdot1dcbxLocCongestionNotification);

    /* get CNPV bit map */
    unsigned int cnpv_bmp = 0;
    for (int i=0; i<8; i++)
        if (local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvSupported[i]) cnpv_bmp |= (1 << i);

    if (cnpv_bmp != 0 && config->lldpXdot1dcbxConfigCongestionNotification.lldpXdot1dcbxConfigCongestionNotificationTxEnable) {
        /* encode CN TLVs, and insert it to chain */
        if (dcbxV1EncodeCnTlv(&tlv,
            local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvSupported,
            local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationCnpvReady) != LLDP_OK ||
            lldpInsertBeforeTlv(tlvChain, tlv, LLDP_TLV_TYPE_END_OF_LLDPDU) != LLDP_OK) {
            LLDP_ERR("DCBX: Error appending Congestion-Notification TLV to egress LLDP message (portNum:%d).\n", portNum);
            return LLDP_ERR_UNKNOWN;
        }

        local->lldpXdot1dcbxLocCongestionNotification.lldpXdot1dcbxLocCongestionNotificationStatsTxTLVs++;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1CnInitialize
 * \ingroup dcbx
 *
 * \desc            Initializes Congestion-Notification DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1CnInitialize(void)
{
    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxV1CnTerminate
 * \ingroup dcbx
 *
 * \desc            Terminates Congestion-Notification DCBX Module.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
int dcbxV1CnTerminate(void)
{
    return LLDP_OK;
}

