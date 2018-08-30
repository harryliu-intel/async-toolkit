/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_fulcrum.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for Fulcrum 
 *                  instantiation of DCBX.
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

#ifndef DCBX_FULCRUM_H
#define DCBX_FULCRUM_H

#include <lldp_int.h>

/* ACL used for application priority rules */
#define DCBX_APPLICATION_PRIORITIES_ACL_ID 1 

/** Initialize Fulcrum DCBX */
fm_status fmDcbxInitialize();

/** Terminate Fulcrum DCBX */
fm_status fmDcbxTerminate();

/** Add Fulcrum Port to DCBX */
fm_status fmDcbxAddPort(fm_int portNum);

/** Add Fulcrum Port from DCBX */
fm_status fmDcbxRemovePort(fm_int portNum);

/** Check if DCBX manager is running */
fm_bool   fmDcbxIsRunning();

/** Check if DCBX port is running */
fm_bool   fmDcbxPortIsRunning(fm_int portNum);

/*****************************************************************************
 * Management
 *****************************************************************************/

/** Set DCBX protocol version for port (-1:DCBX_VERSION_AUTO/0:DCBX_VERSION_BASE/1:DCBX_VERSION_IEEE) */
fm_status fmDcbxSetProtocolVersion(int portNum, int protVersion);

/** Change port TLVs tx-enable (bit-mask of dcbxTlvTxEnable) */
fm_status fmDcbxSetTLVsTxEnable(int portNum, int tlvsTxMask, int tlvsTxEnable);

/** Change port PFC willing settings */
fm_status fmDcbxSetAdminPfcWilling(int portNum, bool willing);

/** Change port PFC MBC settings */
fm_status fmDcbxSetAdminPfcMBC(int portNum, bool mbc);

/** Change port PFC capability settings */
fm_status fmDcbxSetAdminPfcCap(int portNum, int cap);

/** Change port PFC enable settings */
fm_status fmDcbxSetAdminPfcEnable(int portNum, bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7);

/** Change port ETS configuration willing settings */
fm_status fmDcbxSetAdminEtsConfigurationWilling(int portNum, bool willing);

/** Change port ETS configuration CBS settings */
fm_status fmDcbxSetAdminEtsConfigurationCreditBasedShaperSupport(int portNum, bool supported);

/** Change port ETS configuration supported traffic-classes settings */
fm_status fmDcbxSetAdminEtsConfigurationTrafficClassesSupported(int portNum, int max_tc);

/** Change port ETS configuration bandwidth settings */
fm_status fmDcbxSetAdminEtsConfigurationTrafficClassBandwidth(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7);

/** Change port ETS configuration traffic-algorithm settings */
fm_status fmDcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7);

/** Change port ETS configuration priority-assignment settings */
fm_status fmDcbxSetAdminEtsConfigurationPriorityAssignment(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7);

/** Change port ETS recommendation bandwidth settings */
fm_status fmDcbxSetAdminEtsRecommendationTrafficClassBandwidth(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7);

/** Change port ETS recommendation traffic-algorithm settings */
fm_status fmDcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7);

/** Change port ETS recommendation priority-assignment settings */
fm_status fmDcbxSetAdminEtsRecommendationPriorityAssignment(int portNum, int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7);

/** Change port application-priority willing settings */
fm_status fmDcbxSetAdminApplicationPriorityWilling(int portNum, bool willing);

/** Add/Modify port application-priority entry */
fm_status fmDcbxSetAdminApplicationPriorityAddMod(int portNum, int sel, int proto, int pri);

/** Remove port application-priority entry */
fm_status fmDcbxSetAdminApplicationPriorityRemove(int portNum, int sel, int proto);

/** Change port congestion-notification supported CNPV settings */
fm_status fmDcbxSetAdminCongestionNotificationCnpvSupported(int portNum, bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7);

/** Change port congestion-notification ready CNPV settings */
fm_status fmDcbxSetAdminCongestionNotificationCnpvReady(int portNum, bool b0, bool b1, bool b2, bool b3, bool b4, bool b5, bool b6, bool b7);


/*****************************************************************************
 * Status
 *****************************************************************************/

/** Dump DCBX Configuration */
fm_status fmDcbxShowConfiguration(int portNum);

/** Dump DCBX Local Data */
fm_status fmDcbxShowLocalData(int portNum);

/** Dump DCBX Admin Data */
fm_status fmDcbxShowAdminData(int portNum);

/** Dump DCBX Remote Data */
fm_status fmDcbxShowRemoteData(int portNum);

/** Dump DCBX Counters */
fm_status fmDcbxShowCounters(int portNum);

#endif /* DCBX_FULCRUM_H */
