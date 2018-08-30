/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx.c
 * Creation Date:   September 1, 2010
 * Description:     Implementation of DCBX interfaces.
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
#include <dcbx_macros.h>

#include <dcbx_v0_tlv.h>
#include <dcbx_v0_ets.h>
#include <dcbx_v0_pfc.h>
#include <dcbx_v0_app.h>

#include <dcbx_v1_tlv.h>
#include <dcbx_v1_ets.h>
#include <dcbx_v1_pfc.h>
#include <dcbx_v1_app.h>
#include <dcbx_v1_cn.h>

/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

#define AppSelectorString(sel_) \
	((sel_ == 1) ? "EtherType" : \
     (sel_ == 2) ? "TCP" : \
     (sel_ == 3) ? "UDP" : \
     (sel_ == 4) ? "TCP/UDP" : "Unknown") \


/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/

static int demuxDcbxInitializeModules();
static int demuxDcbxTerminateModules();
static int demuxDcbxHandleIngressTLVs(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);
static int demuxDcbxHandleRemoteLearn(int portNum, const struct lldpMsap *msap);
static int demuxDcbxHandleRemoteAging(int portNum, const struct lldpMsap *msap);
static int demuxDcbxAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain);


/*****************************************************************************
 * Global Variables
 *****************************************************************************/

extern struct lldpProtocolIf dcbxV1LldpProtocolInterface;
extern struct lldpProtocolIf dcbxV0LldpProtocolInterface;


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

static struct lldpProtocolIf demuxDcbxLldpProtocolInterface = {
    demuxDcbxInitializeModules,
    demuxDcbxTerminateModules,
    demuxDcbxHandleIngressTLVs,
    demuxDcbxHandleRemoteLearn,
    demuxDcbxHandleRemoteAging,
    demuxDcbxAppendEgressTLVs,
    NULL
};


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** demuxDcbxInitializeModules
 * \ingroup dcbx
 *
 * \desc            Initializes DCBX Protocol. This function is called by the
 *                  LLDP manager, upon registration of the DCBX protocol.
 *                  This function calls the initialization functions of both
 *                  DCBX protocols (base DCBX protocol, and IEEE DCBX protocol)
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int demuxDcbxInitializeModules()
{
    dcbxV1LldpProtocolInterface.obj = dcbxDB;    
    dcbxV0LldpProtocolInterface.obj = dcbxDB;

    if (dcbxV0LldpProtocolInterface.initializeModules)
        dcbxV0LldpProtocolInterface.initializeModules();

    if (dcbxV1LldpProtocolInterface.initializeModules)
        dcbxV1LldpProtocolInterface.initializeModules();

    return LLDP_OK;
}

/*****************************************************************************/
/** demuxDcbxTerminateModules
 * \ingroup dcbx
 *
 * \desc            Terminates DCBX Protocol. This function is called by the
 *                  LLDP manager, upon unregistration of the DCBX protocol.
 *                  This function calls the termination functions of both
 *                  DCBX protocols (base DCBX protocol, and IEEE DCBX protocol)
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_FAIL if is failed.
 *
 *****************************************************************************/
static int demuxDcbxTerminateModules()
{
    if (dcbxV0LldpProtocolInterface.terminateModules)
        dcbxV0LldpProtocolInterface.terminateModules();

    if (dcbxV1LldpProtocolInterface.terminateModules)
        dcbxV1LldpProtocolInterface.terminateModules();

    return LLDP_OK;
}

/*****************************************************************************/
/** demuxDcbxHandleIngressTLVs
 * \ingroup dcbx
 *
 * \desc            Handles ingress TLV, by delegating the ingress TLV to the
 *                  DCBX protocol manager that controls the local port.
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
static int demuxDcbxHandleIngressTLVs(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL)
{
    int ret = LLDP_ERR_NOT_FOUND;

    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        if (dcbxV0LldpProtocolInterface.handleIngressTLVs)
            ret = dcbxV0LldpProtocolInterface.handleIngressTLVs(portNum, tlvChain, msap, remoteMac, rxTTL); 

        /* If DCBX version 0 TLV not found, and auto-detect is enable, try DCBX version 1 */
        if (ret == LLDP_ERR_NOT_FOUND && dcbxGetProtocolVersionAutoDetection(portNum) &&
            dcbxV1LldpProtocolInterface.handleIngressTLVs)
        {
            ret = dcbxV1LldpProtocolInterface.handleIngressTLVs(portNum, tlvChain, msap, remoteMac, rxTTL);
            if (ret == LLDP_OK) dcbxSetProtocolVersion(portNum, DCBX_VERSION_IEEE, 1);
        }
        break;
    case DCBX_VERSION_IEEE:
        if (dcbxV1LldpProtocolInterface.handleIngressTLVs)
            ret = dcbxV1LldpProtocolInterface.handleIngressTLVs(portNum, tlvChain, msap, remoteMac, rxTTL); 

        /* If DCBX version 0 TLV not found, and auto-detect is enable, try DCBX version 1 */
        if (ret == LLDP_ERR_NOT_FOUND && dcbxGetProtocolVersionAutoDetection(portNum) &&
            dcbxV0LldpProtocolInterface.handleIngressTLVs)
        {
            ret = dcbxV0LldpProtocolInterface.handleIngressTLVs(portNum, tlvChain, msap, remoteMac, rxTTL);
            if (ret == LLDP_OK) dcbxSetProtocolVersion(portNum, DCBX_VERSION_BASE, 1);
        }
        break;
    default:
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** demuxDcbxHandleRemoteLearn
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
static int demuxDcbxHandleRemoteLearn(int portNum, const struct lldpMsap *msap)
{
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        if (dcbxV0LldpProtocolInterface.handleRemoteLearn)
            return dcbxV0LldpProtocolInterface.handleRemoteLearn(portNum, msap); 
        break;
    case DCBX_VERSION_IEEE:
        if (dcbxV1LldpProtocolInterface.handleRemoteLearn)
            return dcbxV1LldpProtocolInterface.handleRemoteLearn(portNum, msap); 
        break;
    default:
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** demuxDcbxHandleRemoteAging
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
static int demuxDcbxHandleRemoteAging(int portNum, const struct lldpMsap *msap)
{
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        if (dcbxV0LldpProtocolInterface.handleRemoteAging)
            return dcbxV0LldpProtocolInterface.handleRemoteAging(portNum, msap); 
        break;
    case DCBX_VERSION_IEEE:
        if (dcbxV1LldpProtocolInterface.handleRemoteAging)
            return dcbxV1LldpProtocolInterface.handleRemoteAging(portNum, msap); 
        break;
    default:
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** demuxDcbxAppendEgressTLVs
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
static int demuxDcbxAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain)
{
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        if (dcbxV0LldpProtocolInterface.appendEgressTLVs)
            return dcbxV0LldpProtocolInterface.appendEgressTLVs(portNum, tlvChain); 
        break;
    case DCBX_VERSION_IEEE:
        if (dcbxV1LldpProtocolInterface.appendEgressTLVs)
            return dcbxV1LldpProtocolInterface.appendEgressTLVs(portNum, tlvChain); 
        break;
    default:
        return LLDP_ERR_UNKNOWN;
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** demuxDcbxPfcStateMachineRun
 * \ingroup dcbx
 *
 * \desc            Executes PFC state machine, to update operational state.
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
static int demuxDcbxPfcStateMachineRun(int portNum,
                                       struct lldpXdot1dcbxLocalData  *local,
                                       struct lldpXdot1dcbxAdminData  *admin,
                                       struct lldpXdot1dcbxRemoteData *remote,
                                       uint64_t remoteMac, bool adminChange, bool *changed)
{
    /* run state-machine to update operational variables (local) */
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        return dcbxV0PfcStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    case DCBX_VERSION_IEEE:
        return dcbxV1PfcStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    default:
        return LLDP_ERR_UNKNOWN;
    }
}

/*****************************************************************************/
/** demuxDcbxEtsStateMachineRun
 * \ingroup dcbx
 *
 * \desc            Executes ETS state machine, to update operational state.
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
static int demuxDcbxEtsStateMachineRun(int portNum,
                                       struct lldpXdot1dcbxLocalData  *local,
                                       struct lldpXdot1dcbxAdminData  *admin,
                                       struct lldpXdot1dcbxRemoteData *remote,
                                       uint64_t remoteMac, bool adminChange, bool *changed)
{
    /* run state-machine to update operational variables (local) */
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        return dcbxV0EtsStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    case DCBX_VERSION_IEEE:
        return dcbxV1EtsStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    default:
        return LLDP_ERR_UNKNOWN;
    }
}

/*****************************************************************************/
/** demuxDcbxAppStateMachineRun
 * \ingroup dcbx
 *
 * \desc            Executes APP state machine, to update operational state.
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
static int demuxDcbxAppStateMachineRun(int portNum,
                                       struct lldpXdot1dcbxLocalData  *local,
                                       struct lldpXdot1dcbxAdminData  *admin,
                                       struct lldpXdot1dcbxRemoteData *remote,
                                       uint64_t remoteMac, bool adminChange, bool *changed)
{
    /* run state-machine to update operational variables (local) */
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        return dcbxV0AppStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    case DCBX_VERSION_IEEE:
        return dcbxV1AppStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    default:
        return LLDP_ERR_UNKNOWN;
    }
}

/*****************************************************************************/
/** demuxDcbxCnStateMachineRun
 * \ingroup dcbx
 *
 * \desc            Executes CN state machine, to update operational state.
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
static int demuxDcbxCnStateMachineRun(int portNum,
                                       struct lldpXdot1dcbxLocalData  *local,
                                       struct lldpXdot1dcbxAdminData  *admin,
                                       struct lldpXdot1dcbxRemoteData *remote,
                                       uint64_t remoteMac, bool adminChange, bool *changed)
{
    /* run state-machine to update operational variables (local) */
    switch (dcbxGetProtocolVersion(portNum)) {
    case DCBX_VERSION_BASE:
        *changed = false;
        return LLDP_OK;
    case DCBX_VERSION_IEEE:
        return dcbxV1CnStateMachineRun(portNum, local, admin, remote, remoteMac, adminChange, changed);
    default:
        return LLDP_ERR_UNKNOWN;
    }
}
/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxInitialize
 * \ingroup dcbx
 *
 * \desc            Initializes DCBX manager, by creating the DCBX data-base,
 *                  and registering the DCBX manager as LLDP protocol.
 *
 * \param[in]       physicalIf is an interface to the physical layer, which
 *                  provides the DCBX state-machine interface for configuring
 *                  and monitoring the hardware traffic-control settings.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxInitialize(struct dcbxPhysicalIf *physicalIf)
{
    int status;

    /* create DCBX MIB data object */
    dcbxDB = dcbxObjectsCreate();
    if (!dcbxDB) return LLDP_ERR_NO_MEMORY;

    /* setup hooks and callbacks */
    dcbxDB->physical = physicalIf;

    /* register DCBX handlers at LLDP protocol layer */
    demuxDcbxLldpProtocolInterface.obj = dcbxDB;
    status = lldpRegisterProtocol(&demuxDcbxLldpProtocolInterface);

    LLDP_INFO("DCBX: Initialized.\n");

    return status;
}

/*****************************************************************************/
/** dcbxTerminate
 * \ingroup dcbx
 *
 * \desc            Terminates DCBX manager, by untegistering the DCBX as LLDP
 *                  protocol and destroying the DCBX data-base.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxTerminate(void)
{
    int status;

    if (!dcbxDB) return LLDP_OK;

    /* unregister handlers from LLDP protocol layer */
    status = lldpUnregisterProtocol(&demuxDcbxLldpProtocolInterface);

    /* destroy DCBX MIB data object */
    dcbxObjectsDestroy(dcbxDB);
    dcbxDB = NULL;

    return status;
}

/*****************************************************************************/
/** dcbxAddPort
 * \ingroup dcbx
 *
 * \desc            Add port to DCBX.
 *
 * \param[in]       portNum is the local port number to be added.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAddPort(int portNum)
{
    pthread_mutex_lock(&dcbxDB->lock);

    dcbxConfigCreate(portNum);
    dcbxLocalDataCreate(portNum);
    dcbxAdminDataCreate(portNum);

    LLDP_INFO("DCBX: Added Port (portNum:%d).\n", portNum);

    pthread_mutex_unlock(&dcbxDB->lock);

    return LLDP_OK;

} /* end dcbxAddPort */

/*****************************************************************************/
/** dcbxRemovePort
 * \ingroup dcbx
 *
 * \desc            Remove port from DCBX, that was added with dcbxAddPort().
 *
 * \param[in]       portNum is the local port number to be removed.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxRemovePort(int portNum)
{
    pthread_mutex_lock(&dcbxDB->lock);

    dcbxConfigDestroy(dcbxConfigGet(portNum));
    dcbxLocalDataDestroy(dcbxLocalDataGet(portNum));
    dcbxAdminDataDestroy(dcbxAdminDataGet(portNum));
    dcbxRemoteDataDestroy(dcbxRemoteDataGet(portNum));

    LLDP_INFO("DCBX: Removed Port (portNum:%d).\n", portNum);

    pthread_mutex_unlock(&dcbxDB->lock);

    return LLDP_OK;

} /* end dcbxRemovePort */

/*****************************************************************************/
/** dcbxGetProtocolVersion
 * \ingroup dcbx
 *
 * \desc            Gets DCBX protocol version for port.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \return          DCBX_VERSION_BASE if current protocol is DCBX Version 0.
 * \return          DCBX_VERSION_IEEE if current protocol is DCBX IEEE Version.
 * \return          DCBX_VERSION_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxGetProtocolVersion(int portNum)
{
    int protVersion;

    BEGIN_CRITICAL_LOCAL_PORT_RET(portNum,DCBX_VERSION_UNKNOWN) {
        protVersion = local->lldpXdot1dcbxVersion;
    }
    END_CRITICAL_RET(portNum, protVersion)
} /* end dcbxGetProtocolVersion */

/*****************************************************************************/
/** dcbxSetProtocolVersion
 * \ingroup dcbx
 *
 * \desc            Sets DCBX protocol version for port.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       protVersion is the DCBX protocol version, which can be -
 *                  DCBX_VERSION_BASE, DCBX_VERSION_IEEE or DCBX_VERSION_AUTO
 *
 * \param[in]       autoDetected is TRUE if version is being set due to auto
 *                  detection, and FALSE if version is being set due to admin
 *                  change.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetProtocolVersion(int portNum, int protVersion, int autoDetected)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        if (!autoDetected)
            admin->lldpXdot1dcbxVersion = protVersion;

        /* flush remote entry on port (if exists) */
        if (protVersion != DCBX_VERSION_AUTO)
        {
            struct lldpXdot1dcbxRemoteData *remote = dcbxRemoteDataGet(portNum);
            if (remote) dcbxRemoteDataDestroy(remote);

            switch (protVersion) {
            case DCBX_VERSION_BASE:
                local->lldpXdot1dcbxVersion = protVersion;
                LLDP_INFO("DCBX: DCBX version changed (portNum:%d autoDetected:%d protVersion:%s).\n",
                          portNum, autoDetected, DCBX_VERSION_BASE_STR);
                break;
            case DCBX_VERSION_IEEE:
                local->lldpXdot1dcbxVersion = protVersion;
                LLDP_INFO("DCBX: DCBX version changed (portNum:%d autoDetected:%d protVersion:%s).\n",
                          portNum, autoDetected, DCBX_VERSION_IEEE_STR);
                break;
            default:
                LLDP_INFO("DCBX: Error changing DCBX version (portNum:%d).\n", portNum);
                break;
            }

            changed = true;
        }
        else {
            LLDP_INFO("DCBX: DCBX version auto-detection enabled (portNum:%d).\n", portNum);
        }
    }
    END_CRITICAL(portNum)
} /* end dcbxSetProtocolVersion */

/*****************************************************************************/
/** dcbxGetProtocolVersionAutoDetection
 * \ingroup dcbx
 *
 * \desc            Gets DCBX protocol version auto-detection for port.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \return          TRUE if version auto-detection is enabled.
 * \return          FALSE if version auto-detection is disabled or on failure.
 *
 *****************************************************************************/
int dcbxGetProtocolVersionAutoDetection(int portNum)
{
    int protVersionAutoDetection;

    BEGIN_CRITICAL_ADMIN_PORT_RET(portNum,false) {
        protVersionAutoDetection = (admin->lldpXdot1dcbxVersion == DCBX_VERSION_AUTO);
    }
    END_CRITICAL_RET(portNum, protVersionAutoDetection)
} /* end dcbxGetProtocolVersionAutoDetection */

/*****************************************************************************/
/** dcbxSetTLVsTxEnable
 * \ingroup dcbx
 *
 * \desc            Set which optional TLVs are enabled for transmit.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[in]       tlvsTxMask is the mask of TLVs to be changed.
 *
 * \param[in]       tlvsTxEnable is the mask of TLVs to be enabled.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetTLVsTxEnable(int portNum, uint16_t tlvsTxMask, uint16_t tlvsTxEnable)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        if (tlvsTxMask & DCBX_TLV_ENABLE_PFC)
            config->lldpXdot1dcbxConfigPFC.
                    lldpXdot1dcbxConfigPFCTxEnable = (tlvsTxEnable & DCBX_TLV_ENABLE_PFC) != 0;

        if (tlvsTxMask & DCBX_TLV_ENABLE_ETS_CONF)
            config->lldpXdot1dcbxConfigETSConfiguration.
                    lldpXdot1dcbxConfigETSConfigurationTxEnable = (tlvsTxEnable & DCBX_TLV_ENABLE_ETS_CONF) != 0;

        if (tlvsTxMask & DCBX_TLV_ENABLE_ETS_RECO)
            config->lldpXdot1dcbxConfigETSRecommendation.
                    lldpXdot1dcbxConfigETSRecommendationTxEnable = (tlvsTxEnable & DCBX_TLV_ENABLE_ETS_RECO) != 0;

        if (tlvsTxMask & DCBX_TLV_ENABLE_APP)
            config->lldpXdot1dcbxConfigApplicationPriority.
                    lldpXdot1dcbxConfigApplicationPriorityTxEnable = (tlvsTxEnable & DCBX_TLV_ENABLE_APP) != 0;

        if (tlvsTxMask & DCBX_TLV_ENABLE_CN)
            config->lldpXdot1dcbxConfigCongestionNotification.
                    lldpXdot1dcbxConfigCongestionNotificationTxEnable = (tlvsTxEnable & DCBX_TLV_ENABLE_CN) != 0;

        LLDP_INFO("DCBX: TLVs tx-enable changed (portNum:%d tlvsTxMask:%04x tlvsTxEnable:%04x).\n", portNum, tlvsTxMask, tlvsTxEnable);
    }
    END_CRITICAL(portNum)
}

/*****************************************************************************/
/** dcbxSetAdminPfcWilling
 * \ingroup dcbx
 *
 * \desc            Sets port PFC willing state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       willing is the willing state.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminPfcWilling(int portNum, bool willing)
{
    int status;
    struct lldpXdot1dcbxAdminPFC ad;

    LLDP_INFO("DCBX: Setting PFC willing (portNum:%d willing:%d).\n", portNum, willing);

    if ((status = dcbxPfcAdminGet(portNum, &ad)) != LLDP_OK) {
        LLDP_ERR("DCBX: Error getting admin-data object (portNum:%d, 1).\n", portNum);
        return status;
    }

    ad.lldpXdot1dcbxAdminPFCWilling = willing;

    if ((status = dcbxPfcAdminSet(portNum, &ad)) != LLDP_OK) {
        LLDP_ERR("DCBX: Error getting admin-data object (portNum:%d, 2).\n", portNum);
        return status;
    }

    return status;
}

/*****************************************************************************/
/** dcbxSetAdminPfcMBC
 * \ingroup dcbx
 *
 * \desc            Sets port PFC MBC indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       mbc is the MACsec-Bypass-Capability indication.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminPfcMBC(int portNum, bool mbc)
{
    int status;
    struct lldpXdot1dcbxAdminPFC ad;

    LLDP_INFO("DCBX: Setting PFC MBC (portNum:%d mbc:%d).\n", portNum, mbc);

    if ((status = dcbxPfcAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    ad.lldpXdot1dcbxAdminPFCMBC = mbc;

    return dcbxPfcAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminPfcCap
 * \ingroup dcbx
 *
 * \desc            Sets port PFC capability indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       cap indicates the limitation of how many traffic classes
 *                  may simultaneously support PFC.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminPfcCap(int portNum, int cap)
{
    int status;
    struct lldpXdot1dcbxAdminPFC ad;

    LLDP_INFO("DCBX: Setting PFC capability (portNum:%d cap:%d).\n", portNum, cap);

    if ((status = dcbxPfcAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    ad.lldpXdot1dcbxAdminPFCCap = cap;

    if ((status = dcbxPfcAdminSet(portNum, &ad)) != LLDP_OK)
        return status;

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxSetAdminPfcEnable
 * \ingroup dcbx
 *
 * \desc            Sets port PFC enable-on-priority state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       enable is the enable state array for priorities 0-7.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminPfcEnable(int portNum, bool enable[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminPFC ad;

    LLDP_INFO("DCBX: Setting PFC enable (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              enable[0], enable[1], enable[2], enable[3],
              enable[4], enable[5], enable[6], enable[7]);

    if ((status = dcbxPfcAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminPFCEnable[i] = enable[i];

    return dcbxPfcAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsConfigurationWilling
 * \ingroup dcbx
 *
 * \desc            Sets port ETS willing state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       willing is the willing state.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsConfigurationWilling(int portNum, bool willing)
{
    int status;
    struct lldpXdot1dcbxAdminETSConfiguration ad;

    LLDP_INFO("DCBX: Setting ETS-Conf willing (portNum:%d willing:%d).\n", portNum, willing);

    if ((status = dcbxEtsConfigurationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    ad.lldpXdot1dcbxAdminETSConWilling = willing;

    return dcbxEtsConfigurationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsConfigurationCreditBasedShaperSupport
 * \ingroup dcbx
 *
 * \desc            Sets port ETS CBS indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       cbs is the Credit-Based-Shaper-Support indication.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsConfigurationCreditBasedShaperSupport(int portNum, bool cbs)
{
    int status;
    struct lldpXdot1dcbxAdminETSConfiguration ad;

    LLDP_INFO("DCBX: Setting ETS-Conf CBS (portNum:%d cbs:%d).\n", portNum, cbs);

    if ((status = dcbxEtsConfigurationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    ad.lldpXdot1dcbxAdminETSConCreditBasedShaperSupport = cbs;

    return dcbxEtsConfigurationAdminSet(portNum, &ad);

}

/*****************************************************************************/
/** dcbxSetAdminEtsConfigurationTrafficClassesSupported
 * \ingroup dcbx
 *
 * \desc            Sets port ETS maximum traffic classes supported.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       max_tc is the maximum traffic classes supported indication.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsConfigurationTrafficClassesSupported(int portNum, int max_tc)
{
    int status;
    struct lldpXdot1dcbxAdminETSConfiguration ad;

    LLDP_INFO("DCBX: Setting ETS-Conf max traffic-classes (portNum:%d mbc:%d).\n", portNum, max_tc);

    if ((status = dcbxEtsConfigurationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    ad.lldpXdot1dcbxAdminETSConTrafficClassesSupported = max_tc;

    return dcbxEtsConfigurationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsConfigurationTrafficClassBandwidth
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Configuration bandwidth.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       bandwidth is the bandwidth allocated array for traffic-
 *                  classes 0-7.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsConfigurationTrafficClassBandwidth(int portNum, int bandwidth[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminETSConfiguration ad;

    LLDP_INFO("DCBX: Setting ETS-Conf bandwidth (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              bandwidth[0], bandwidth[1], bandwidth[2], bandwidth[3],
              bandwidth[4], bandwidth[5], bandwidth[6], bandwidth[7]);

    if ((status = dcbxEtsConfigurationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[i] = bandwidth[i];

    return dcbxEtsConfigurationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Configuration traffic-algorithm.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       traffic_algorithm is the traffic-algorithm set for traffic-
 *                  classes 0-7.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm(int portNum, int traffic_algorithm[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminETSConfiguration ad;

    LLDP_INFO("DCBX: Setting ETS-Conf traffic-algorithm (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              traffic_algorithm[0], traffic_algorithm[1], traffic_algorithm[2], traffic_algorithm[3],
              traffic_algorithm[4], traffic_algorithm[5], traffic_algorithm[6], traffic_algorithm[7]);

    if ((status = dcbxEtsConfigurationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[i] = traffic_algorithm[i];

    return dcbxEtsConfigurationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsConfigurationPriorityAssignment
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Configuration priority-assignment.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       pri_assignment is the traffic-class assigned to 
 *                  priorities 0-7.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsConfigurationPriorityAssignment(int portNum, int pri_assignment[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminETSConfiguration ad;

    LLDP_INFO("DCBX: Setting ETS-Conf priority-assignment (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              pri_assignment[0], pri_assignment[1], pri_assignment[2], pri_assignment[3],
              pri_assignment[4], pri_assignment[5], pri_assignment[6], pri_assignment[7]);

    if ((status = dcbxEtsConfigurationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminETSConPriorityAssignmentTable[i] = pri_assignment[i];

    return dcbxEtsConfigurationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsRecommendationTrafficClassBandwidth
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Recommendation bandwidth.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       bandwidth is the bandwidth recommended for traffic-class X.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsRecommendationTrafficClassBandwidth(int portNum, int bandwidth[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminETSRecommendation ad;

    LLDP_INFO("DCBX: Setting ETS-Reco bandwidth (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              bandwidth[0], bandwidth[1], bandwidth[2], bandwidth[3],
              bandwidth[4], bandwidth[5], bandwidth[6], bandwidth[7]);

    if ((status = dcbxEtsRecommendationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[i] = bandwidth[i];

    return dcbxEtsRecommendationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Recommendation traffic-algorithm.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       traffic_algorithm is the traffic-algorithm recommended for traffic-
 *                  class X.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm(int portNum, int traffic_algorithm[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminETSRecommendation ad;

    LLDP_INFO("DCBX: Setting ETS-Reco traffic-algorithm (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              traffic_algorithm[0], traffic_algorithm[1], traffic_algorithm[2], traffic_algorithm[3],
              traffic_algorithm[4], traffic_algorithm[5], traffic_algorithm[6], traffic_algorithm[7]);

    if ((status = dcbxEtsRecommendationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[i] = traffic_algorithm[i];

    return dcbxEtsRecommendationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminEtsRecommendationPriorityAssignment
 * \ingroup dcbx
 *
 * \desc            Sets port ETS-Recommendation priority-assignment.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       pri_assignment is the traffic-class recommended for priority X.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminEtsRecommendationPriorityAssignment(int portNum, int pri_assignment[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminETSRecommendation ad;

    LLDP_INFO("DCBX: Setting ETS-Reco priority-assignment (portNum:%d enable:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              pri_assignment[0], pri_assignment[1], pri_assignment[2], pri_assignment[3],
              pri_assignment[4], pri_assignment[5], pri_assignment[6], pri_assignment[7]);

    if ((status = dcbxEtsRecommendationAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[i] = pri_assignment[i];

    return dcbxEtsRecommendationAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminApplicationPriorityWilling
 * \ingroup dcbx
 *
 * \desc            Sets port Application-Priority willing state.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       willing is the willing state.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminApplicationPriorityWilling(int portNum, bool willing)
{
    int status;
    struct lldpXdot1dcbxAdminApplicationPriority ad;

    LLDP_INFO("DCBX: Setting Application-Priority willing (portNum:%d willing:%d).\n", portNum, willing);

    if ((status = dcbxAppAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    ad.lldpXdot1dcbxAdminApplicationPriorityWilling = willing;

    return dcbxAppAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminApplicationPriorityAddMod
 * \ingroup dcbx
 *
 * \desc            Adds/Modifies port Application-Priority entry.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       sel is the protocol type selection.
 *
 * \param[in]       proto is the protocol code.
 *
 * \param[in]       pri is the priority to which the protocol should be 
 *                  assigned.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminApplicationPriorityAddMod(int portNum, int sel, int proto, int pri)
{
    int i, status;
    struct lldpXdot1dcbxAdminApplicationPriority ad;

    LLDP_INFO("DCBX: Adding Application-Priority entry (portNum:%d sel:%d proto:0x%04x pri:%d).\n", portNum, sel, proto, pri);

    if ((status = dcbxAppAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum &&
                i < DCBX_TLV_MAX_APPLICATION_PRIORITIES; i++)
    {
        if ((int)ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                    lldpXdot1dcbxAdminApplicationPriorityAESelector == sel &&
            (int)ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                    lldpXdot1dcbxAdminApplicationPriorityAEProtocol == proto) break;
    }

    if (i == DCBX_TLV_MAX_APPLICATION_PRIORITIES)
        return LLDP_ERR_NO_MEMORY;

    if (i == ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum)
        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum++;

    ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
       lldpXdot1dcbxAdminApplicationPriorityAESelector = sel;
    ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
       lldpXdot1dcbxAdminApplicationPriorityAEProtocol = proto;
    ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
       lldpXdot1dcbxAdminApplicationPriorityAEPriority = pri;

    return dcbxAppAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminApplicationPriorityRemove
 * \ingroup dcbx
 *
 * \desc            Removes port Application-Priority entry.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       sel is the protocol type selection.
 *
 * \param[in]       proto is the protocol code.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminApplicationPriorityRemove(int portNum, int sel, int proto)
{
    int i, status;
    struct lldpXdot1dcbxAdminApplicationPriority ad;

    LLDP_INFO("DCBX: Removing Application-Priority entry (portNum:%d sel:%d proto:0x%04x).\n", portNum, sel, proto);

    if ((status = dcbxAppAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum; i++)
    {
        if ((int)ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                    lldpXdot1dcbxAdminApplicationPriorityAESelector == sel &&
            (int)ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                    lldpXdot1dcbxAdminApplicationPriorityAEProtocol == proto) break;
    }

    if (i == ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum)
        return LLDP_ERR_NOT_FOUND;

    ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum--;

    for (;i < ad.lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum; i++)
    {
        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
            lldpXdot1dcbxAdminApplicationPriorityAESelector =
        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i+1].
            lldpXdot1dcbxAdminApplicationPriorityAESelector;

        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
            lldpXdot1dcbxAdminApplicationPriorityAEProtocol =
        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i+1].
            lldpXdot1dcbxAdminApplicationPriorityAEProtocol;

        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
           lldpXdot1dcbxAdminApplicationPriorityAEPriority =
        ad.lldpXdot1dcbxAdminApplicationPriorityAEPriority[i+1].
           lldpXdot1dcbxAdminApplicationPriorityAEPriority;
    }

    return dcbxAppAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminCongestionNotificationCnpvSupported
 * \ingroup dcbx
 *
 * \desc            Sets port Congestion-Notification supported CNPV indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       supported indicates if the CNPV is supported for priority.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminCongestionNotificationCnpvSupported(int portNum, bool supported[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminCongestionNotification ad;

    LLDP_INFO("DCBX: Setting CN supported CNPV (portNum:%d supported:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              supported[0], supported[1], supported[2], supported[3],
              supported[4], supported[5], supported[6], supported[7]);

    if ((status = dcbxCnAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[i] = supported[i];

    return dcbxCnAdminSet(portNum, &ad);
}

/*****************************************************************************/
/** dcbxSetAdminCongestionNotificationCnpvSupported
 * \ingroup dcbx
 *
 * \desc            Sets port Congestion-Notification ready CNPV indication.
 *
 * \param[in]       portNum is the local port number to be checked.
 *
 * \param[in]       ready indicates if the CNPV is ready for priority.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxSetAdminCongestionNotificationCnpvReady(int portNum, bool ready[8])
{
    int i, status;
    struct lldpXdot1dcbxAdminCongestionNotification ad;

    LLDP_INFO("DCBX: Setting CN ready CNPV (portNum:%d supported:%d/%d/%d/%d/%d/%d/%d/%d).\n", portNum,
              ready[0], ready[1], ready[2], ready[3],
              ready[4], ready[5], ready[6], ready[7]);

    if ((status = dcbxCnAdminGet(portNum, &ad)) != LLDP_OK)
        return status;

    for (i = 0; i < 8; i++)
        ad.lldpXdot1dcbxAdminCongestionNotificationCnpvReady[i] = ready[i];

    return dcbxCnAdminSet(portNum, &ad);
}


/*****************************************************************************
 * Status
 *****************************************************************************/

/*****************************************************************************/
/** dcbxShowConfiguration
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX configuration information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxShowConfiguration(int portNum)
{
    int v;
    struct lldpXdot1dcbxConfig *iter, *next;

    if (portNum == -1) {
        printf("No global state in configuration data-set.\n");
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(iter, next,
                            struct lldpXdot1dcbxConfig, node,
                            &dcbxDB->lldpXdot1dcbxConfigList) {
        if (iter->lldpV2LocPortIfIndex != portNum) continue;
        v = dcbxGetProtocolVersion(iter->lldpV2LocPortIfIndex);

        printf
            ("port-%02d.Pfc.TxEnable                      : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxConfigPFC.
             lldpXdot1dcbxConfigPFCTxEnable ? "enabled" : "disabled");
        printf
            ("port-%02d.EtsConfiguration.TxEnable         : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxConfigETSConfiguration.
             lldpXdot1dcbxConfigETSConfigurationTxEnable ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsRecommendation.TxEnable        : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxConfigETSRecommendation.
             lldpXdot1dcbxConfigETSRecommendationTxEnable ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.ApplicationPriority.TxEnable      : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxConfigApplicationPriority.
             lldpXdot1dcbxConfigApplicationPriorityTxEnable ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.CongestionNotification.TxEnable   : %s\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxConfigCongestionNotification.
             lldpXdot1dcbxConfigCongestionNotificationTxEnable ? "enabled" : "disabled");
        }
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxShowLocalData
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX local-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxShowLocalData(int portNum)
{
    int i, v;
    struct lldpXdot1dcbxLocalData *iter, *next;

    if (portNum == -1) {
        printf("No global state in local data-set.\n");
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(iter, next,
                            struct lldpXdot1dcbxLocalData, node,
                            &dcbxDB->lldpXdot1dcbxLocalDataList) {
        if (iter->lldpV2LocPortIfIndex != portNum) continue;
        v = dcbxGetProtocolVersion(iter->lldpV2LocPortIfIndex);

        printf
            ("port-%02d.MultiplePeersAlarm                : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpMultiplePeers ? "raised" : "cleared");
        printf
            ("port-%02d.ProtocolVersion                   : %s\n\n",
             iter->lldpV2LocPortIfIndex,
            (iter->lldpXdot1dcbxVersion == DCBX_VERSION_BASE) ? 
            DCBX_VERSION_BASE_STR : DCBX_VERSION_IEEE_STR);

        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.OperVersion                       : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->dcbxV0OperVersion);
            printf
            ("port-%02d.SeqNo                             : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->dcbxV0SeqNo);
            printf
            ("port-%02d.AckNo                             : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->dcbxV0AckNo);
            printf
            ("port-%02d.RcvdAckNo                         : %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->dcbxV0RcvdAckNo);
        }

        printf
            ("port-%02d.Pfc.Willing                       : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocPFC.
             lldpXdot1dcbxLocPFCWilling ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Pfc.MBC                           : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocPFC.
             lldpXdot1dcbxLocPFCMBC ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.Pfc.Capability                    : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocPFC.
             lldpXdot1dcbxLocPFCCap);
        printf
            ("port-%02d.Pfc.Enable                        : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[0],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[1],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[2],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[3],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[4],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[5],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[6],
             iter->lldpXdot1dcbxLocPFC.lldpXdot1dcbxLocPFCEnable[7]);
        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.Pfc.OperVersion                   : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocPFC.dcbxV0LocPFCOperVersion);
            printf
            ("port-%02d.Pfc.FeatureError                  : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocPFC.dcbxV0LocPFCError ? "raised" : "cleared");
        }
        printf("\n");
        printf
            ("port-%02d.EtsConf.Willing                   : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConWilling ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsConf.CreditBasedShaperSupport  : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConCreditBasedShaperSupport ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.EtsConf.TrafficClassesSupported   : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassesSupported);
        printf
            ("port-%02d.EtsConf.TrafficClassBandwidth     : %d%% %d%% %d%% %d%% %d%% %d%% %d%% %d%%\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[0],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[1],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[2],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[3],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[4],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[5],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[6],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[7]);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsConf.TrafficSelectionAlgorithm : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[0],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[1],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[2],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[3],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[4],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[5],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[6],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[7]);
        }
        printf
            ("port-%02d.EtsConf.PriorityAssignment        : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[0],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[1],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[2],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[3],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[4],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[5],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[6],
             iter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConPriorityAssignmentTable[7]);
        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.EtsConf.OperVersion               : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             dcbxV0LocETSConOperVersion);
            printf
            ("port-%02d.EtsConf.FeatureError              : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSConfiguration.
             dcbxV0LocETSConError ? "raised" : "cleared");
        }
        printf("\n");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsReco.TrafficClassBandwidth     : %d%% %d%% %d%% %d%% %d%% %d%% %d%% %d%%\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[0],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[1],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[2],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[3],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[4],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[5],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[6],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[7]);
        printf
            ("port-%02d.EtsReco.TrafficSelectionAlgorithm : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[0],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[1],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[2],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[3],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[4],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[5],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[6],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[7]);
        printf
            ("port-%02d.EtsReco.PriorityAssignment        : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[0],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[1],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[2],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[3],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[4],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[5],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[6],
             iter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[7]);
        }
        printf
            ("port-%02d.AppPri.Willing                    : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocApplicationPriority.
             lldpXdot1dcbxLocApplicationPriorityWilling ? "enabled" : "disabled");
        for (i=0; i<iter->lldpXdot1dcbxLocApplicationPriority.
                          lldpXdot1dcbxLocApplicationPriorityAEPriorityNum; i++)
            printf
            ("port-%02d.AppPri.Sel/Proto -> Pri           : %s/%04x -> %d\n",
                 iter->lldpV2LocPortIfIndex,
                 AppSelectorString(
                 iter->lldpXdot1dcbxLocApplicationPriority.
                 lldpXdot1dcbxLocApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxLocApplicationPriorityAESelector),
                 iter->lldpXdot1dcbxLocApplicationPriority.
                 lldpXdot1dcbxLocApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxLocApplicationPriorityAEProtocol,
                 iter->lldpXdot1dcbxLocApplicationPriority.
                 lldpXdot1dcbxLocApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxLocApplicationPriorityAEPriority);
        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.AppPri.OperVersion                : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocApplicationPriority.
             dcbxV0LocApplicationPriorityOperVersion);
            printf
            ("port-%02d.AppPri.FeatureError               : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocApplicationPriority.
             dcbxV0LocApplicationPriorityError ? "raised" : "cleared");
        }
        printf("\n");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Cn.CnpvSupported                  : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[0],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[1],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[2],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[3],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[4],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[5],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[6],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvSupported[7]);
        printf
            ("port-%02d.Cn.CnpvReady                      : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[0],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[1],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[2],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[3],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[4],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[5],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[6],
             iter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationCnpvReady[7]);
        }
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxShowAdminData
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX admin-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxShowAdminData(int portNum)
{
    int i, v;
    struct lldpXdot1dcbxAdminData *iter, *next;

    if (portNum == -1) {
        printf("No global state in admin data-set.\n");
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(iter, next,
                            struct lldpXdot1dcbxAdminData, node,
                            &dcbxDB->lldpXdot1dcbxAdminDataList) {
        if (iter->lldpV2LocPortIfIndex != portNum) continue;
        v = dcbxGetProtocolVersion(iter->lldpV2LocPortIfIndex);

        printf
            ("port-%02d.ProtocolVersion                   : %s\n\n",
             iter->lldpV2LocPortIfIndex,
            (iter->lldpXdot1dcbxVersion == DCBX_VERSION_BASE) ? 
            DCBX_VERSION_BASE_STR : 
            (iter->lldpXdot1dcbxVersion == DCBX_VERSION_BASE) ?
            DCBX_VERSION_IEEE_STR : "Auto Detect");

        printf
            ("port-%02d.Pfc.Willing                       : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminPFC.
             lldpXdot1dcbxAdminPFCWilling ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Pfc.MBC                           : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminPFC.
             lldpXdot1dcbxAdminPFCMBC ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.Pfc.Capability                    : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminPFC.
             lldpXdot1dcbxAdminPFCCap);
        printf
            ("port-%02d.Pfc.Enable                        : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[0],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[1],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[2],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[3],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[4],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[5],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[6],
             iter->lldpXdot1dcbxAdminPFC.lldpXdot1dcbxAdminPFCEnable[7]);
        printf
            ("port-%02d.EtsConf.Willing                   : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConWilling ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsConf.CreditBasedShaperSupport  : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConCreditBasedShaperSupport ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.EtsConf.TrafficClassesSupported   : %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassesSupported);
        printf
            ("port-%02d.EtsConf.TrafficClassBandwidth     : %d%% %d%% %d%% %d%% %d%% %d%% %d%% %d%%\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[0],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[1],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[2],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[3],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[4],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[5],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[6],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[7]);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsConf.TrafficSelectionAlgorithm : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[0],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[1],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[2],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[3],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[4],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[5],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[6],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[7]);
        }
        printf
            ("port-%02d.EtsConf.PriorityAssignment        : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[0],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[1],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[2],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[3],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[4],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[5],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[6],
             iter->lldpXdot1dcbxAdminETSConfiguration.
             lldpXdot1dcbxAdminETSConPriorityAssignmentTable[7]);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsReco.TrafficClassBandwidth     : %d%% %d%% %d%% %d%% %d%% %d%% %d%% %d%%\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[0],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[1],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[2],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[3],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[4],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[5],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[6],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[7]);
        printf
            ("port-%02d.EtsReco.TrafficSelectionAlgorithm : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[0],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[1],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[2],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[3],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[4],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[5],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[6],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[7]);
        printf
            ("port-%02d.EtsReco.PriorityAssignment        : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[0],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[1],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[2],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[3],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[4],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[5],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[6],
             iter->lldpXdot1dcbxAdminETSRecommendation.
             lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[7]);
        }
        printf
            ("port-%02d.AppPri.Willing                    : %s\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminApplicationPriority.
             lldpXdot1dcbxAdminApplicationPriorityWilling ? "enabled" : "disabled");
        for (i=0; i<iter->lldpXdot1dcbxAdminApplicationPriority.
                          lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum; i++)
            printf
            ("port-%02d.AppPri.Sel/Proto -> Pri           : %s/%04x -> %d\n",
                 iter->lldpV2LocPortIfIndex,
                 AppSelectorString(
                 iter->lldpXdot1dcbxAdminApplicationPriority.
                 lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxAdminApplicationPriorityAESelector),
                 iter->lldpXdot1dcbxAdminApplicationPriority.
                 lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxAdminApplicationPriorityAEProtocol,
                 iter->lldpXdot1dcbxAdminApplicationPriority.
                 lldpXdot1dcbxAdminApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxAdminApplicationPriorityAEPriority);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Cn.CnpvSupported                  : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[0],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[1],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[2],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[3],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[4],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[5],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[6],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[7]);
        printf
            ("port-%02d.Cn.CnpvReady                      : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2LocPortIfIndex,
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[0],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[1],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[2],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[3],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[4],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[5],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[6],
             iter->lldpXdot1dcbxAdminCongestionNotification.
             lldpXdot1dcbxAdminCongestionNotificationCnpvReady[7]);
        }
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxShowRemoteData
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX remote-data information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxShowRemoteData(int portNum)
{
    int i, v;
    uint32_t now = time(NULL);
    struct lldpXdot1dcbxRemoteData *iter, *next;

    if (portNum == -1) {
        printf("No global state in remote data-set.\n");
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(iter, next,
                            struct lldpXdot1dcbxRemoteData, node,
                            &dcbxDB->lldpXdot1dcbxRemoteDataList) {
        if (iter->lldpV2RemLocalIfIndex != portNum ||
            !iter->lldpXdot1dcbxRemoteDataValid) continue;
        v = dcbxGetProtocolVersion(iter->lldpV2RemLocalIfIndex);

        printf
            ("port-%02d.TimeMark                          : %d\n",
             iter->lldpV2RemLocalIfIndex,
             TIMER_S(iter->lldpV2RemTimeMark));
        printf
            ("port-%02d.DestMACAddress                    : %02x:%02x:%02x:%02x:%02x:%02x\n\n",
             iter->lldpV2RemLocalIfIndex,
             (uint8_t)((iter->lldpV2RemLocalDestMACAddress >> 40) & 0xff),
             (uint8_t)((iter->lldpV2RemLocalDestMACAddress >> 32) & 0xff),
             (uint8_t)((iter->lldpV2RemLocalDestMACAddress >> 24) & 0xff),
             (uint8_t)((iter->lldpV2RemLocalDestMACAddress >> 16) & 0xff),
             (uint8_t)((iter->lldpV2RemLocalDestMACAddress >>  8) & 0xff),
             (uint8_t)((iter->lldpV2RemLocalDestMACAddress      ) & 0xff));
        printf
            ("port-%02d.Pfc.Willing                       : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.
             lldpXdot1dcbxRemPFCWilling ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Pfc.MBC                           : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.
             lldpXdot1dcbxRemPFCMBC ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.Pfc.Capability                    : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.
             lldpXdot1dcbxRemPFCCap);
        printf
            ("port-%02d.Pfc.Enable                        : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[0],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[1],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[2],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[3],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[4],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[5],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[6],
             iter->lldpXdot1dcbxRemPFC.lldpXdot1dcbxRemPFCEnable[7]);
        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.Pfc.OperVersion                   : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.dcbxV0RemPFCOperVersion);
            printf
            ("port-%02d.Pfc.MaxVersion                    : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.dcbxV0RemPFCMaxVersion);
            printf
            ("port-%02d.Pfc.FeatureError                  : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemPFC.dcbxV0RemPFCError ? "raised" : "cleared");
        }
        printf("\n");
        printf
            ("port-%02d.EtsConf.Willing                   : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConWilling ? "enabled" : "disabled");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsConf.CreditBasedShaperSupport  : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConCreditBasedShaperSupport ? "enabled" : "disabled");
        }
        printf
            ("port-%02d.EtsConf.TrafficClassesSupported   : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassesSupported);
        printf
            ("port-%02d.EtsConf.TrafficClassBandwidth     : %d%% %d%% %d%% %d%% %d%% %d%% %d%% %d%%\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[0],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[1],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[2],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[3],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[4],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[5],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[6],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[7]);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsConf.TrafficSelectionAlgorithm : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[0],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[1],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[2],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[3],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[4],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[5],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[6],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[7]);
        }
        printf
            ("port-%02d.EtsConf.PriorityAssignment        : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[0],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[1],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[2],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[3],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[4],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[5],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[6],
             iter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConPriorityAssignmentTable[7]);
        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.EtsConf.OperVersion               : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             dcbxV0RemETSConOperVersion);
            printf
            ("port-%02d.EtsConf.MaxVersion                : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             dcbxV0RemETSConMaxVersion);
            printf
            ("port-%02d.EtsConf.FeatureError              : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSConfiguration.
             dcbxV0RemETSConError ? "raised" : "cleared");
        }
        printf("\n");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsReco.TrafficClassBandwidth     : %d%% %d%% %d%% %d%% %d%% %d%% %d%% %d%%\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[0],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[1],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[2],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[3],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[4],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[5],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[6],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[7]);
        printf
            ("port-%02d.EtsReco.TrafficSelectionAlgorithm : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[0],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[1],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[2],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[3],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[4],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[5],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[6],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[7]);
        printf
            ("port-%02d.EtsReco.PriorityAssignment        : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[0],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[1],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[2],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[3],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[4],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[5],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[6],
             iter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[7]);
        }
        printf
            ("port-%02d.AppPri.Willing                    : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemApplicationPriority.
             lldpXdot1dcbxRemApplicationPriorityWilling ? "enabled" : "disabled");
        for (i=0; i<iter->lldpXdot1dcbxRemApplicationPriority.
                          lldpXdot1dcbxRemApplicationPriorityAEPriorityNum; i++)
            printf
            ("port-%02d.AppPri.Sel/Proto -> Pri           : %s/%04x -> %d\n",
                 iter->lldpV2RemLocalIfIndex,
                 AppSelectorString(
                 iter->lldpXdot1dcbxRemApplicationPriority.
                 lldpXdot1dcbxRemApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxRemApplicationPriorityAESelector),
                 iter->lldpXdot1dcbxRemApplicationPriority.
                 lldpXdot1dcbxRemApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxRemApplicationPriorityAEProtocol,
                 iter->lldpXdot1dcbxRemApplicationPriority.
                 lldpXdot1dcbxRemApplicationPriorityAEPriority[i].
                 lldpXdot1dcbxRemApplicationPriorityAEPriority);
        if (v == DCBX_VERSION_BASE) {
            printf
            ("port-%02d.AppPri.OperVersion                : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemApplicationPriority.
             dcbxV0RemApplicationPriorityOperVersion);
            printf
            ("port-%02d.AppPri.MaxVersion                 : %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemApplicationPriority.
             dcbxV0RemApplicationPriorityMaxVersion);
            printf
            ("port-%02d.AppPri.FeatureError               : %s\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemApplicationPriority.
             dcbxV0RemApplicationPriorityError ? "raised" : "cleared");
        }
        printf("\n");
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Cn.CnpvSupported                  : %d %d %d %d %d %d %d %d\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[0],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[1],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[2],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[3],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[4],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[5],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[6],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvSupported[7]);
        printf
            ("port-%02d.Cn.CnpvReady                      : %d %d %d %d %d %d %d %d\n\n",
             iter->lldpV2RemLocalIfIndex,
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[0],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[1],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[2],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[3],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[4],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[5],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[6],
             iter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationCnpvReady[7]);
        }
    }

    return LLDP_OK;
}

/*****************************************************************************/
/** dcbxShowCounters
 * \ingroup dcbx
 *
 * \desc            Dumps DCBX counters information to stdout.
 *
 * \param[in]       portNum is the local port number.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxShowCounters(int portNum)
{
    int v;
    struct lldpXdot1dcbxRemoteData *riter, *rnext;
    struct lldpXdot1dcbxLocalData  *liter, *lnext;

    if (portNum == -1) {
        printf("No global state in counters data-set.\n");
        return LLDP_OK;
    }

    LLDP_LIST_FOR_EACH_SAFE(liter, lnext,
                            struct lldpXdot1dcbxLocalData, node,
                            &dcbxDB->lldpXdot1dcbxLocalDataList) {
        if (liter->lldpV2LocPortIfIndex != portNum) continue;
        v = dcbxGetProtocolVersion(liter->lldpV2LocPortIfIndex);

        printf
            ("port-%02d.Pfc.Tx.TlvsTotal                  : %d\n",
             liter->lldpV2LocPortIfIndex,
             liter->lldpXdot1dcbxLocPFC.
             lldpXdot1dcbxLocPFCStatsTxTLVs);
        printf
            ("port-%02d.EtsConf.Tx.TlvsTotal              : %d\n",
             liter->lldpV2LocPortIfIndex,
             liter->lldpXdot1dcbxLocETSConfiguration.
             lldpXdot1dcbxLocETSConStatsTxTLVs);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsReco.Tx.TlvsTotal              : %d\n",
             liter->lldpV2LocPortIfIndex,
             liter->lldpXdot1dcbxLocETSRecommendation.
             lldpXdot1dcbxLocETSRecoStatsTxTLVs);
        }
        printf
            ("port-%02d.AppPri.Tx.TlvsTotal               : %d\n",
             liter->lldpV2LocPortIfIndex,
             liter->lldpXdot1dcbxLocApplicationPriority.
             lldpXdot1dcbxLocApplicationPriorityStatsTxTLVs);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Cn.Tx.TlvsTotal                   : %d\n\n",
             liter->lldpV2LocPortIfIndex,
             liter->lldpXdot1dcbxLocCongestionNotification.
             lldpXdot1dcbxLocCongestionNotificationStatsTxTLVs);
        }
    }

    LLDP_LIST_FOR_EACH_SAFE(riter, rnext,
                            struct lldpXdot1dcbxRemoteData, node,
                            &dcbxDB->lldpXdot1dcbxRemoteDataList) {
        if (riter->lldpV2RemLocalIfIndex != portNum ||
            !riter->lldpXdot1dcbxRemoteDataValid) continue;
        v = dcbxGetProtocolVersion(riter->lldpV2RemLocalIfIndex);

        printf
            ("port-%02d.Pfc.Rx.TlvsTotal                  : %d\n",
             riter->lldpV2RemLocalIfIndex,
             riter->lldpXdot1dcbxRemPFC.
             lldpXdot1dcbxRemPFCStatsRxTLVs);
        printf
            ("port-%02d.EtsConf.Rx.TlvsTotal              : %d\n",
             riter->lldpV2RemLocalIfIndex,
             riter->lldpXdot1dcbxRemETSConfiguration.
             lldpXdot1dcbxRemETSConStatsRxTLVs);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.EtsReco.Rx.TlvsTotal              : %d\n",
             riter->lldpV2RemLocalIfIndex,
             riter->lldpXdot1dcbxRemETSRecommendation.
             lldpXdot1dcbxRemETSRecoStatsRxTLVs);
        }
        printf
            ("port-%02d.AppPri.Rx.TlvsTotal               : %d\n",
             riter->lldpV2RemLocalIfIndex,
             riter->lldpXdot1dcbxRemApplicationPriority.
             lldpXdot1dcbxRemApplicationPriorityStatsRxTLVs);
        if (v == DCBX_VERSION_IEEE) {
        printf
            ("port-%02d.Cn.Rx.TlvsTotal                   : %d\n\n",
             riter->lldpV2RemLocalIfIndex,
             riter->lldpXdot1dcbxRemCongestionNotification.
             lldpXdot1dcbxRemCongestionNotificationStatsRxTLVs);     
        }
    }

    return LLDP_OK;
}


/*****************************************************************************
 * Public PFC API Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxPfcConfigGet
 * \ingroup dcbx
 *
 * \desc            Gets PFC configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigPFC points to caller-allocated storage where this
 *                  function should place the PFC configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxPfcConfigGet(int portNum, struct lldpXdot1dcbxConfigPFC *dcbxConfigPFC)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        memcpy(dcbxConfigPFC, 
               &(config->lldpXdot1dcbxConfigPFC),
               sizeof(struct lldpXdot1dcbxConfigPFC));
    }
    END_CRITICAL(portNum)

} /* end dcbxPfcConfigGet */

/*****************************************************************************/
/** dcbxPfcConfigSet
 * \ingroup dcbx
 *
 * \desc            Sets PFC configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigPFC is the PFC configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxPfcConfigSet(int portNum, const struct lldpXdot1dcbxConfigPFC *dcbxConfigPFC)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        changed = memcmp(&(config->lldpXdot1dcbxConfigPFC),
                         dcbxConfigPFC,               
                         sizeof(struct lldpXdot1dcbxConfigPFC)) != 0;

        if (changed) {
            memcpy(&(config->lldpXdot1dcbxConfigPFC),
                   dcbxConfigPFC,               
                   sizeof(struct lldpXdot1dcbxConfigPFC));

            LLDP_INFO("DCBX: PFC Config Changed "
                      "(portNum:%d TxEnable:%d).\n", 
                      portNum,
                      dcbxConfigPFC->lldpXdot1dcbxConfigPFCTxEnable);
        }
    }
    END_CRITICAL(portNum)

} /* end dcbxPfcConfigSet */

/*****************************************************************************/
/** dcbxPfcAdminGet
 * \ingroup dcbx
 *
 * \desc            Gets PFC admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminPFC points to caller-allocated storage where this
 *                  function should place the PFC admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxPfcAdminGet(int portNum, struct lldpXdot1dcbxAdminPFC *dcbxAdminPFC)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        memcpy(dcbxAdminPFC,
               &(admin->lldpXdot1dcbxAdminPFC),
               sizeof(struct lldpXdot1dcbxAdminPFC));
    }
    END_CRITICAL(portNum)

} /* end dcbxPfcAdminGet */

/*****************************************************************************/
/** dcbxPfcAdminSet
 * \ingroup dcbx
 *
 * \desc            Sets PFC admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminPFC is the PFC admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxPfcAdminSet(int portNum, const struct lldpXdot1dcbxAdminPFC *dcbxAdminPFC)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        struct lldpXdot1dcbxRemoteData *remote = dcbxRemoteDataGet(portNum);

        memcpy(&(admin->lldpXdot1dcbxAdminPFC),
               dcbxAdminPFC,
               sizeof(struct lldpXdot1dcbxAdminPFC));

        /* run state-machine to update operational variables (local) */
        demuxDcbxPfcStateMachineRun(portNum, local, admin, remote, 
                                    remote ? remote->lldpV2RemLocalDestMACAddress : 0, true, &changed);
    }
    END_CRITICAL(portNum)

} /* end dcbxPfcAdminSet */

/*****************************************************************************/
/** dcbxPfcLocGet
 * \ingroup dcbx
 *
 * \desc            Gets PFC local-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxLocPFC points to caller-allocated storage where this
 *                  function should place the PFC local-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxPfcLocGet(int portNum, struct lldpXdot1dcbxLocPFC *dcbxLocPFC)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        memcpy(dcbxLocPFC, 
               &(local->lldpXdot1dcbxLocPFC),
               sizeof(struct lldpXdot1dcbxLocPFC));
    }
    END_CRITICAL(portNum)

} /* end dcbxPfcLocGet */

/*****************************************************************************/
/** dcbxPfcRemGet
 * \ingroup dcbx
 *
 * \desc            Gets PFC remote-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxRemPFC points to caller-allocated storage where this
 *                  function should place the PFC remote-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxPfcRemGet(int portNum, struct lldpXdot1dcbxRemPFC *dcbxRemPFC)
{
    BEGIN_CRITICAL_REMOTE_PORT(portNum) {
        memcpy(dcbxRemPFC, 
               &(remote->lldpXdot1dcbxRemPFC),
               sizeof(struct lldpXdot1dcbxRemPFC));
    }
    END_CRITICAL(portNum)

} /* end dcbxPfcRemGet */


/*****************************************************************************
 * Public ETS API Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxEtsConfigurationConfigGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Configuration configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigETSConfiguration points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Configuration configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsConfigurationConfigGet(int portNum, struct lldpXdot1dcbxConfigETSConfiguration *dcbxConfigETSConfiguration)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        memcpy(dcbxConfigETSConfiguration, 
               &(config->lldpXdot1dcbxConfigETSConfiguration),
               sizeof(struct lldpXdot1dcbxConfigETSConfiguration));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsConfigurationConfigGet */

/*****************************************************************************/
/** dcbxEtsConfigurationConfigSet
 * \ingroup dcbx
 *
 * \desc            Sets ETS-Configuration configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigETSConfiguration is the ETS-Configuration
 *                  configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsConfigurationConfigSet(int portNum, const struct lldpXdot1dcbxConfigETSConfiguration *dcbxConfigETSConfiguration)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        changed = memcmp(&(config->lldpXdot1dcbxConfigETSConfiguration),
                         dcbxConfigETSConfiguration,               
                         sizeof(struct lldpXdot1dcbxConfigETSConfiguration)) != 0;

        if (changed) {
            memcpy(&(config->lldpXdot1dcbxConfigETSConfiguration),
                   dcbxConfigETSConfiguration,               
                   sizeof(struct lldpXdot1dcbxConfigETSConfiguration));

            LLDP_INFO("DCBX: ETS-Configuration Config Changed "
                      "(portNum:%d TxEnable:%d).\n", 
                      portNum,
                      dcbxConfigETSConfiguration->lldpXdot1dcbxConfigETSConfigurationTxEnable);
        }
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsConfigurationConfigSet */

/*****************************************************************************/
/** dcbxEtsConfigurationAdminGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Configuration admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminETSConfiguration points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Configuration admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsConfigurationAdminGet(int portNum, struct lldpXdot1dcbxAdminETSConfiguration *dcbxAdminETSConfiguration)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        memcpy(dcbxAdminETSConfiguration, 
               &(admin->lldpXdot1dcbxAdminETSConfiguration),
               sizeof(struct lldpXdot1dcbxAdminETSConfiguration));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsConfigurationAdminGet */

/*****************************************************************************/
/** dcbxEtsConfigurationAdminSet
 * \ingroup dcbx
 *
 * \desc            Sets ETS-Configuration admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminETSConfiguration is the ETS-Configuration
 *                  admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsConfigurationAdminSet(int portNum, const struct lldpXdot1dcbxAdminETSConfiguration *dcbxAdminETSConfiguration)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        struct lldpXdot1dcbxRemoteData *remote = dcbxRemoteDataGet(portNum);

        memcpy(&(admin->lldpXdot1dcbxAdminETSConfiguration),
               dcbxAdminETSConfiguration,                
               sizeof(struct lldpXdot1dcbxAdminETSConfiguration));

        /* run state-machine to update operational variables (local) */
        demuxDcbxEtsStateMachineRun(portNum, local, admin, remote, 
                                    remote ? remote->lldpV2RemLocalDestMACAddress : 0, true, &changed);
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsConfigurationAdminSet */

/*****************************************************************************/
/** dcbxEtsConfigurationLocGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Configuration local-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxLocETSConfiguration points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Configuration local-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsConfigurationLocGet(int portNum, struct lldpXdot1dcbxLocETSConfiguration *dcbxLocETSConfiguration)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        memcpy(dcbxLocETSConfiguration, 
               &(local->lldpXdot1dcbxLocETSConfiguration),
               sizeof(struct lldpXdot1dcbxLocETSConfiguration));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsConfigurationLocGet */

/*****************************************************************************/
/** dcbxEtsConfigurationRemGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Configuration remote-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxRemETSConfiguration points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Configuration remote-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsConfigurationRemGet(int portNum, struct lldpXdot1dcbxRemETSConfiguration *dcbxRemETSConfiguration)
{
    BEGIN_CRITICAL_REMOTE_PORT(portNum) {
        memcpy(dcbxRemETSConfiguration, 
               &(remote->lldpXdot1dcbxRemETSConfiguration),
               sizeof(struct lldpXdot1dcbxRemETSConfiguration));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsConfigurationRemGet */

/*****************************************************************************/
/** dcbxEtsRecommendationConfigGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Recommendation configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigETSRecommendation points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Recommendation configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsRecommendationConfigGet(int portNum, struct lldpXdot1dcbxConfigETSRecommendation *dcbxConfigETSRecommendation)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        memcpy(dcbxConfigETSRecommendation, 
               &(config->lldpXdot1dcbxConfigETSRecommendation),
               sizeof(struct lldpXdot1dcbxConfigETSRecommendation));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsRecommendationConfigGet */

/*****************************************************************************/
/** dcbxEtsRecommendationConfigSet
 * \ingroup dcbx
 *
 * \desc            Sets ETS-Recommendation configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigETSRecommendation is the ETS-Recommendation
 *                  configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsRecommendationConfigSet(int portNum, const struct lldpXdot1dcbxConfigETSRecommendation *dcbxConfigETSRecommendation)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        changed = memcmp(&(config->lldpXdot1dcbxConfigETSRecommendation),
                         dcbxConfigETSRecommendation,               
                         sizeof(struct lldpXdot1dcbxConfigETSRecommendation)) != 0;

        if (changed) {
            memcpy(&(config->lldpXdot1dcbxConfigETSRecommendation),
                   dcbxConfigETSRecommendation,               
                   sizeof(struct lldpXdot1dcbxConfigETSRecommendation));

            LLDP_INFO("DCBX: ETS-Recommendation Config Changed "
                      "(portNum:%d TxEnable:%d).\n", 
                      portNum,
                      dcbxConfigETSRecommendation->lldpXdot1dcbxConfigETSRecommendationTxEnable);
        }
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsRecommendationConfigSet */

/*****************************************************************************/
/** dcbxEtsRecommendationAdminGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Recommendation admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminETSRecommendation points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Recommendation admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsRecommendationAdminGet(int portNum, struct lldpXdot1dcbxAdminETSRecommendation *dcbxAdminETSRecommendation)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        memcpy(dcbxAdminETSRecommendation, 
               &(admin->lldpXdot1dcbxAdminETSRecommendation),
               sizeof(struct lldpXdot1dcbxAdminETSRecommendation));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsRecommendationAdminGet */

/*****************************************************************************/
/** dcbxEtsRecommendationAdminSet
 * \ingroup dcbx
 *
 * \desc            Sets ETS-Recommendation admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminETSRecommendation is the ETS-Recommendation
 *                  admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsRecommendationAdminSet(int portNum, const struct lldpXdot1dcbxAdminETSRecommendation *dcbxAdminETSRecommendation)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        struct lldpXdot1dcbxRemoteData *remote = dcbxRemoteDataGet(portNum);

        memcpy(&(admin->lldpXdot1dcbxAdminETSRecommendation),
               dcbxAdminETSRecommendation,                
               sizeof(struct lldpXdot1dcbxAdminETSRecommendation));

        /* run state-machine to update operational variables (local) */
        demuxDcbxEtsStateMachineRun(portNum, local, admin, remote, 
                                    remote ? remote->lldpV2RemLocalDestMACAddress : 0, true, &changed);
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsRecommendationAdminSet */

/*****************************************************************************/
/** dcbxEtsRecommendationLocGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Recommendation local-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxLocETSRecommendation points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Recommendation local-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsRecommendationLocGet(int portNum, struct lldpXdot1dcbxLocETSRecommendation *dcbxLocETSRecommendation)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        memcpy(dcbxLocETSRecommendation, 
               &(local->lldpXdot1dcbxLocETSRecommendation),
               sizeof(struct lldpXdot1dcbxLocETSRecommendation));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsRecommendationLocGet */

/*****************************************************************************/
/** dcbxEtsRecommendationRemGet
 * \ingroup dcbx
 *
 * \desc            Gets ETS-Recommendation remote-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxRemETSRecommendation points to caller-allocated 
 *                  storage where this function should place the ETS-
 *                  Recommendation remote-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxEtsRecommendationRemGet(int portNum, struct lldpXdot1dcbxRemETSRecommendation *dcbxRemETSRecommendation)
{
    BEGIN_CRITICAL_REMOTE_PORT(portNum) {
        memcpy(dcbxRemETSRecommendation, 
               &(remote->lldpXdot1dcbxRemETSRecommendation),
               sizeof(struct lldpXdot1dcbxRemETSRecommendation));
    }
    END_CRITICAL(portNum)

} /* end dcbxEtsRecommendationRemGet */


/*****************************************************************************
 * Public Application-Priority API Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxAppConfigGet
 * \ingroup dcbx
 *
 * \desc            Gets Application-Priority configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigApplicationPriority points to caller-allocated 
 *                  storage where this function should place the Application-
 *                  Priority configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAppConfigGet(int portNum, struct lldpXdot1dcbxConfigApplicationPriority *dcbxConfigApplicationPriority)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        memcpy(dcbxConfigApplicationPriority, 
               &(config->lldpXdot1dcbxConfigApplicationPriority),
               sizeof(struct lldpXdot1dcbxConfigApplicationPriority));
    }
    END_CRITICAL(portNum)

} /* end dcbxAppConfigGet */

/*****************************************************************************/
/** dcbxAppConfigSet
 * \ingroup dcbx
 *
 * \desc            Sets Application-Priority configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigApplicationPriority is the Application-
 *                  Priority configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAppConfigSet(int portNum, const struct lldpXdot1dcbxConfigApplicationPriority *dcbxConfigApplicationPriority)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        changed = memcmp(&(config->lldpXdot1dcbxConfigApplicationPriority),
                         dcbxConfigApplicationPriority,               
                         sizeof(struct lldpXdot1dcbxConfigApplicationPriority)) != 0;

        if (changed) {
            memcpy(&(config->lldpXdot1dcbxConfigApplicationPriority),
                   dcbxConfigApplicationPriority,               
                   sizeof(struct lldpXdot1dcbxConfigApplicationPriority));

            LLDP_INFO("DCBX: Application-Priority Config Changed "
                      "(portNum:%d TxEnable:%d).\n", 
                      portNum,
                      dcbxConfigApplicationPriority->lldpXdot1dcbxConfigApplicationPriorityTxEnable);
        }
    }
    END_CRITICAL(portNum)

} /* end dcbxAppConfigSet */

/*****************************************************************************/
/** dcbxAppAdminGet
 * \ingroup dcbx
 *
 * \desc            Gets Application-Priority admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminApplicationPriority points to caller-allocated 
 *                  storage where this function should place the Application-
 *                  Priority admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAppAdminGet(int portNum, struct lldpXdot1dcbxAdminApplicationPriority *dcbxAdminApplicationPriority)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        memcpy(dcbxAdminApplicationPriority,
               &(admin->lldpXdot1dcbxAdminApplicationPriority),
               sizeof(struct lldpXdot1dcbxAdminApplicationPriority));
    }
    END_CRITICAL(portNum)

} /* end dcbxAppAdminGet */

/*****************************************************************************/
/** dcbxAppAdminSet
 * \ingroup dcbx
 *
 * \desc            Sets Application-Priority admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminApplicationPriority is the Application-
 *                  Priority admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAppAdminSet(int portNum, const struct lldpXdot1dcbxAdminApplicationPriority *dcbxAdminApplicationPriority)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        struct lldpXdot1dcbxRemoteData *remote = dcbxRemoteDataGet(portNum);

        memcpy(&(admin->lldpXdot1dcbxAdminApplicationPriority),
               dcbxAdminApplicationPriority,
               sizeof(struct lldpXdot1dcbxAdminApplicationPriority));

        /* run state-machine to update operational variables (local) */
        demuxDcbxAppStateMachineRun(portNum, local, admin, remote, 
                                    remote ? remote->lldpV2RemLocalDestMACAddress : 0, true, &changed);
    }
    END_CRITICAL(portNum)

} /* end dcbxAppLocSet */

/*****************************************************************************/
/** dcbxAppLocGet
 * \ingroup dcbx
 *
 * \desc            Gets Application-Priority local-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxLocApplicationPriority points to caller-allocated 
 *                  storage where this function should place the Application-
 *                  Priority local-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAppLocGet(int portNum, struct lldpXdot1dcbxLocApplicationPriority *dcbxLocApplicationPriority)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        memcpy(dcbxLocApplicationPriority,
               &(local->lldpXdot1dcbxLocApplicationPriority),
               sizeof(struct lldpXdot1dcbxLocApplicationPriority));
    }
    END_CRITICAL(portNum)

} /* end dcbxAppLocGet */

/*****************************************************************************/
/** dcbxAppRemGet
 * \ingroup dcbx
 *
 * \desc            Gets Application-Priority remote-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxRemApplicationPriority points to caller-allocated 
 *                  storage where this function should place the Application-
 *                  Priority remote-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxAppRemGet(int portNum, struct lldpXdot1dcbxRemApplicationPriority *dcbxRemApplicationPriority)
{
    BEGIN_CRITICAL_REMOTE_PORT(portNum) {
        memcpy(dcbxRemApplicationPriority,
               &(remote->lldpXdot1dcbxRemApplicationPriority),
               sizeof(struct lldpXdot1dcbxRemApplicationPriority));
    }
    END_CRITICAL(portNum)

} /* end dcbxAppRemGet */


/*****************************************************************************
 * Public Congestion-Notification API Functions
 *****************************************************************************/

/*****************************************************************************/
/** dcbxAppConfigGet
 * \ingroup dcbx
 *
 * \desc            Gets Congestion-Notification configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigCongestionNotification points to caller- 
 *                  allocated storage where this function should place the 
 *                  Congestion-Notification configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxCnConfigGet(int portNum, struct lldpXdot1dcbxConfigCongestionNotification *dcbxConfigCongestionNotification)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        memcpy(dcbxConfigCongestionNotification, 
               &(config->lldpXdot1dcbxConfigCongestionNotification),
               sizeof(struct lldpXdot1dcbxConfigCongestionNotification));
    }
    END_CRITICAL(portNum)

} /* end dcbxCnConfigGet */

/*****************************************************************************/
/** dcbxCnConfigSet
 * \ingroup dcbx
 *
 * \desc            Sets Congestion-Notification configuration.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxConfigCongestionNotification is the Congestion-
 *                  Notification configuration.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxCnConfigSet(int portNum, const struct lldpXdot1dcbxConfigCongestionNotification *dcbxConfigCongestionNotification)
{
    BEGIN_CRITICAL_PORT_CONFIG(portNum) {
        changed = memcmp(&(config->lldpXdot1dcbxConfigCongestionNotification),
                         dcbxConfigCongestionNotification,               
                         sizeof(struct lldpXdot1dcbxConfigCongestionNotification)) != 0;

        if (changed) {
            memcpy(&(config->lldpXdot1dcbxConfigCongestionNotification),
                   dcbxConfigCongestionNotification,               
                   sizeof(struct lldpXdot1dcbxConfigCongestionNotification));

            LLDP_INFO("DCBX: Congestion-Notification Config Changed "
                      "(portNum:%d TxEnable:%d).\n", 
                      portNum,
                      dcbxConfigCongestionNotification->lldpXdot1dcbxConfigCongestionNotificationTxEnable);
        }
    }
    END_CRITICAL(portNum)

} /* end dcbxCnConfigSet */

/*****************************************************************************/
/** dcbxCnAdminGet
 * \ingroup dcbx
 *
 * \desc            Gets Congestion-Notification admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminCongestionNotification points to caller- 
 *                  allocated storage where this function should place the 
 *                  Congestion-Notification admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxCnAdminGet(int portNum, struct lldpXdot1dcbxAdminCongestionNotification *dcbxAdminCongestionNotification)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        memcpy(dcbxAdminCongestionNotification, 
               &(admin->lldpXdot1dcbxAdminCongestionNotification),
               sizeof(struct lldpXdot1dcbxAdminCongestionNotification));
    }
    END_CRITICAL(portNum)

} /* end dcbxCnAdminGet */

/*****************************************************************************/
/** dcbxCnAdminSet
 * \ingroup dcbx
 *
 * \desc            Sets Congestion-Notification admin-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxAdminCongestionNotification is the Congestion-
 *                  Notification admin-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxCnAdminSet(int portNum, const struct lldpXdot1dcbxAdminCongestionNotification *dcbxAdminCongestionNotification)
{
    BEGIN_CRITICAL_ADMIN_PORT(portNum) {
        struct lldpXdot1dcbxRemoteData *remote = dcbxRemoteDataGet(portNum);

        memcpy(&(admin->lldpXdot1dcbxAdminCongestionNotification),
               dcbxAdminCongestionNotification,                
               sizeof(struct lldpXdot1dcbxAdminCongestionNotification));

        /* run state-machine to update operational variables (local) */
        demuxDcbxCnStateMachineRun(portNum, local, admin, remote, 
                                    remote ? remote->lldpV2RemLocalDestMACAddress : 0, true, &changed);
    }
    END_CRITICAL(portNum)

} /* end dcbxCnAdminSet */

/*****************************************************************************/
/** dcbxCnLocGet
 * \ingroup dcbx
 *
 * \desc            Gets Congestion-Notification local-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxLocCongestionNotification points to caller- 
 *                  allocated storage where this function should place the 
 *                  Congestion-Notification local-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxCnLocGet(int portNum, struct lldpXdot1dcbxLocCongestionNotification *dcbxLocCongestionNotification)
{
    BEGIN_CRITICAL_LOCAL_PORT(portNum) {
        memcpy(dcbxLocCongestionNotification, 
               &(local->lldpXdot1dcbxLocCongestionNotification),
               sizeof(struct lldpXdot1dcbxLocCongestionNotification));
    }
    END_CRITICAL(portNum)

} /* end dcbxCnLocGet */

/*****************************************************************************/
/** dcbxCnRemGet
 * \ingroup dcbx
 *
 * \desc            Gets Congestion-Notification remote-data.
 *
 * \param[in]       portNum is the local port number.
 *
 * \param[out]      dcbxRemCongestionNotification points to caller- 
 *                  allocated storage where this function should place the 
 *                  Congestion-Notification remote-data.
 *
 * \return          LLDP_OK if successful.
 * \return          LLDP_ERR_UNKNOWN on failure.
 *
 *****************************************************************************/
int dcbxCnRemGet(int portNum, struct lldpXdot1dcbxRemCongestionNotification *dcbxRemCongestionNotification)
{
    BEGIN_CRITICAL_REMOTE_PORT(portNum) {
        memcpy(dcbxRemCongestionNotification, 
               &(remote->lldpXdot1dcbxRemCongestionNotification),
               sizeof(struct lldpXdot1dcbxRemCongestionNotification));
    }
    END_CRITICAL(portNum)

} /* end dcbxCnRemGet */

