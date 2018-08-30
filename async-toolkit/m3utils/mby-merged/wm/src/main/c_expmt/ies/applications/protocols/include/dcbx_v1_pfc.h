/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_v1_pfc.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for DCBX Priority 
 *                  based Flow Control Feature.
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

#ifndef DCBX_V1_PFC_H
#define DCBX_V1_PFC_H

#include <stdint.h>

#include <dcbx_mib.h>
#include <dcbx_v1_tlv.h>

/*****************************************************************************
 * Initialization 
 *****************************************************************************/

/** Initialize DCBX Priority based Flow Control feature */
int dcbxV1PfcInitialize(void);

/** Terminate DCBX Priority based Flow Control feature */
int dcbxV1PfcTerminate(void);


/*****************************************************************************
 * Callbacks
 *****************************************************************************/

struct lldpTlv;  /* defined by lldp_tlv.h */
struct lldpMsap; /* defined by lldp_mib.h */

/** Handle ingress PFC TLV */
int dcbxV1PfcHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);

/** Handle ingress PFC TLV */
int dcbxV1PfcHandleIngressTLVs(int portNum, const struct lldpTlv *tlv, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);

/** Handle missing PFC TLV from DCBX TLVs chain */
int dcbxV1PfcHandleMissingTLVs(int portNum, enum dcbxTlvSubType subType, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);

/** Handle remote LLDP learn */
int dcbxV1PfcHandleRemoteLearn(int portNum, const struct lldpMsap *msap);

/** Handle remote LLDP age-out */
int dcbxV1PfcHandleRemoteAging(int portNum, const struct lldpMsap *msap);

/** Append PFC TLV to egress LLDP TLVs chain */
int dcbxV1PfcAppendEgressTLVs(int portNum, struct lldpTlv *tlvChain);


/*****************************************************************************
 * State Machine
 *****************************************************************************/

/** Run PFC feature state machine */
int dcbxV1PfcStateMachineRun(int portNum,
                           struct lldpXdot1dcbxLocalData  *local,
                           struct lldpXdot1dcbxAdminData  *admin,
                           struct lldpXdot1dcbxRemoteData *remote,
                           uint64_t remoteMac, bool adminChange, bool *changed);

#endif /* DCBX_V1_PFC_H */
