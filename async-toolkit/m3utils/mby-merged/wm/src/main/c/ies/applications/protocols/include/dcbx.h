/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for DCBX control API.
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

#ifndef DCBX_H
#define DCBX_H

#include <dcbx_mib.h>

/** The dcbxPhysicalIf interface is implemented by low level physical layers, 
    which provides DCBX interface for controling / monitoring port state and
    status */
struct dcbxPhysicalIf {
    /** Update to PFC operational status before generating egress TLV */
    int (*updatePFCStatus)(int portNum, struct lldpXdot1dcbxLocPFC *locPFC);

    /** Update to ETS operational status before generating egress TLV */
    int (*updateETSStatus)(int portNum, struct lldpXdot1dcbxLocETSConfiguration *locETSConfiguration);

    /** Update to APP operational status before generating egress TLV */
    int (*updateApplicationPriorityStatus)(int portNum, struct lldpXdot1dcbxLocApplicationPriority *locApplicationPriority);

    /** Update to CN operational status before generating egress TLV */
    int (*updateCongestionNotificationStatus)(int portNum, struct lldpXdot1dcbxLocCongestionNotification *locCongestionNotification);

    /** Apply to hardware changes in PFC settings */
    int (*applyPFCSettings)(int portNum, struct lldpXdot1dcbxLocPFC *locPFC); 

    /** Apply to hardware changes in ETS settings */
    int (*applyETSSettings)(int portNum, struct lldpXdot1dcbxLocETSConfiguration *locETSConfiguration); 

    /** Apply to hardware changes in APP settings */
    int (*applyApplicationPrioritySettings)(int portNum, struct lldpXdot1dcbxLocApplicationPriority *locApplicationPriority); 

    /** Apply to hardware changes in CN settings */
    int (*applyCongestionNotificationSettings)(int portNum, struct lldpXdot1dcbxLocCongestionNotification *locCongestionNotification); 
};


/*****************************************************************************
 * Initialization 
 *****************************************************************************/

/** Initialize DCBX protocol */
int dcbxInitialize(struct dcbxPhysicalIf *physicalIf);

/** Terminate DCBX protocol */
int dcbxTerminate(void);

/** Add port to DCBX protocol */
int dcbxAddPort(int portNum);

/** Remove port from DCBX protocol */
int dcbxRemovePort(int portNum);


/*****************************************************************************
 * Global Managment
 *****************************************************************************/

/** Set DCBX protocol version for port (DCBX_VERSION_BASE/DCBX_VERSION_IEEE) */
int dcbxSetProtocolVersion(int portNum, int protVersion, int autoDetected);

/** Get DCBX protocol version for port (DCBX_VERSION_BASE/DCBX_VERSION_IEEE) */
int dcbxGetProtocolVersion(int portNum);

/** Get DCBX protocol version auto-detection for port */
int dcbxGetProtocolVersionAutoDetection(int portNum);

/** Change port TLVs tx-enable (bit-mask of dcbxTlvTxEnable) */
int dcbxSetTLVsTxEnable(int portNum, uint16_t tlvsTxMask, uint16_t tlvsTxEnable);


/*****************************************************************************
 * Global Status
 *****************************************************************************/

/** Dump DCBX Configuration */
int dcbxShowConfiguration(int portNum);

/** Dump DCBX Local Data */
int dcbxShowLocalData(int portNum);

/** Dump DCBX Admin Data */
int dcbxShowAdminData(int portNum);

/** Dump DCBX Remote Data */
int dcbxShowRemoteData(int portNum);

/** Dump DCBX Counters */
int dcbxShowCounters(int portNum);


/*****************************************************************************
 * PFC Management
 *****************************************************************************/

int dcbxSetAdminPfcWilling(int portNum, bool willing);
int dcbxSetAdminPfcMBC(int portNum, bool mbc);
int dcbxSetAdminPfcCap(int portNum, int cap);
int dcbxSetAdminPfcEnable(int portNum, bool enable[8]);

int dcbxPfcConfigGet(int portNum, struct lldpXdot1dcbxConfigPFC *dcbxConfigPFC);
int dcbxPfcConfigSet(int portNum, const struct lldpXdot1dcbxConfigPFC *dcbxConfigPFC);
int dcbxPfcAdminGet(int portNum, struct lldpXdot1dcbxAdminPFC *dcbxAdminPFC);
int dcbxPfcAdminSet(int portNum, const struct lldpXdot1dcbxAdminPFC *dcbxAdminPFC);
int dcbxPfcLocGet(int portNum, struct lldpXdot1dcbxLocPFC *dcbxLocPFC);
int dcbxPfcRemGet(int portNum, struct lldpXdot1dcbxRemPFC *dcbxRemPFC);


/*****************************************************************************
 * ETS-Configuration Management
 *****************************************************************************/

int dcbxSetAdminEtsConfigurationWilling(int portNum, bool willing);
int dcbxSetAdminEtsConfigurationCreditBasedShaperSupport(int portNum, bool supported);
int dcbxSetAdminEtsConfigurationTrafficClassesSupported(int portNum, int max_tc);
int dcbxSetAdminEtsConfigurationTrafficClassBandwidth(int portNum, int bandwidth[8]);
int dcbxSetAdminEtsConfigurationTrafficSelectionAlgorithm(int portNum, int traffic_algorithm[8]);
int dcbxSetAdminEtsConfigurationPriorityAssignment(int portNum, int pri_assignment[8]);

int dcbxEtsConfigurationConfigGet(int portNum, struct lldpXdot1dcbxConfigETSConfiguration *dcbxConfigETSConfiguration);
int dcbxEtsConfigurationConfigSet(int portNum, const struct lldpXdot1dcbxConfigETSConfiguration *dcbxConfigETSConfiguration);
int dcbxEtsConfigurationAdminGet(int portNum, struct lldpXdot1dcbxAdminETSConfiguration *dcbxAdminETSConfiguration);
int dcbxEtsConfigurationAdminSet(int portNum, const struct lldpXdot1dcbxAdminETSConfiguration *dcbxAdminETSConfiguration);
int dcbxEtsConfigurationLocGet(int portNum, struct lldpXdot1dcbxLocETSConfiguration *dcbxLocETSConfiguration);
int dcbxEtsConfigurationRemGet(int portNum, struct lldpXdot1dcbxRemETSConfiguration *dcbxRemETSConfiguration);


/*****************************************************************************
 * ETS-Recommendation Management
 *****************************************************************************/

int dcbxSetAdminEtsRecommendationTrafficClassBandwidth(int portNum, int bandwidth[8]);
int dcbxSetAdminEtsRecommendationTrafficSelectionAlgorithm(int portNum, int traffic_algorithm[8]);
int dcbxSetAdminEtsRecommendationPriorityAssignment(int portNum, int pri_assignment[8]);

int dcbxEtsRecommendationConfigGet(int portNum, struct lldpXdot1dcbxConfigETSRecommendation *dcbxConfigETSRecommendation);
int dcbxEtsRecommendationConfigSet(int portNum, const struct lldpXdot1dcbxConfigETSRecommendation *dcbxConfigETSRecommendation);
int dcbxEtsRecommendationAdminGet(int portNum, struct lldpXdot1dcbxAdminETSRecommendation *dcbxAdminETSRecommendation);
int dcbxEtsRecommendationAdminSet(int portNum, const struct lldpXdot1dcbxAdminETSRecommendation *dcbxAdminETSRecommendation);
int dcbxEtsRecommendationLocGet(int portNum, struct lldpXdot1dcbxLocETSRecommendation *dcbxLocETSRecommendation);
int dcbxEtsRecommendationRemGet(int portNum, struct lldpXdot1dcbxRemETSRecommendation *dcbxRemETSRecommendation);


/*****************************************************************************
 * Application-Priority Management
 *****************************************************************************/

int dcbxSetAdminApplicationPriorityWilling(int portNum, bool willing);
int dcbxSetAdminApplicationPriorityAddMod(int portNum, int sel, int proto, int pri);
int dcbxSetAdminApplicationPriorityRemove(int portNum, int sel, int proto);

int dcbxAppConfigGet(int portNum, struct lldpXdot1dcbxConfigApplicationPriority *dcbxConfigApplicationPriority);
int dcbxAppConfigSet(int portNum, const struct lldpXdot1dcbxConfigApplicationPriority *dcbxConfigApplicationPriority);
int dcbxAppAdminGet(int portNum, struct lldpXdot1dcbxAdminApplicationPriority *dcbxAdminApplicationPriority);
int dcbxAppAdminSet(int portNum, const struct lldpXdot1dcbxAdminApplicationPriority *dcbxAdminApplicationPriority);
int dcbxAppLocGet(int portNum, struct lldpXdot1dcbxLocApplicationPriority *dcbxLocApplicationPriority);
int dcbxAppRemGet(int portNum, struct lldpXdot1dcbxRemApplicationPriority *dcbxRemApplicationPriority);


/*****************************************************************************
 * Congestion-Notification Management
 *****************************************************************************/

int dcbxSetAdminCongestionNotificationCnpvSupported(int portNum, bool supported[8]);
int dcbxSetAdminCongestionNotificationCnpvReady(int portNum, bool ready[8]);

int dcbxCnConfigGet(int portNum, struct lldpXdot1dcbxConfigCongestionNotification *dcbxConfigCongestionNotification);
int dcbxCnConfigSet(int portNum, const struct lldpXdot1dcbxConfigCongestionNotification *dcbxConfigCongestionNotification);
int dcbxCnAdminGet(int portNum, struct lldpXdot1dcbxAdminCongestionNotification *dcbxAdminCongestionNotification);
int dcbxCnAdminSet(int portNum, const struct lldpXdot1dcbxAdminCongestionNotification *dcbxAdminCongestionNotification);
int dcbxCnLocGet(int portNum, struct lldpXdot1dcbxLocCongestionNotification *dcbxLocCongestionNotification);
int dcbxCnRemGet(int portNum, struct lldpXdot1dcbxRemCongestionNotification *dcbxRemCongestionNotification);

#endif /** DCBX_H */

