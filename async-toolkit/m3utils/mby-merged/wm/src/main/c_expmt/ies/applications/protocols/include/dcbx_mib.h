/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            dcbx_mib.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for DCBX Agent.
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

#ifndef DCBX_MIB_H
#define DCBX_MIB_H

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include <lldp.h>
#include <lldp_list.h>

/* maximum application priority codes in TLV */
#define DCBX_TLV_MAX_APPLICATION_PRIORITIES 128 

enum dcbxProtocolVersion {
    DCBX_VERSION_UNKNOWN = -2,
    DCBX_VERSION_AUTO    = -1, /* Auto DCBX Version Detection */
    DCBX_VERSION_BASE    =  0, /* DCBX Base Protocol V1.01 [Version 0] */
    DCBX_VERSION_IEEE    =  1  /* DCBX IEEE 802.1Qaz/D2.1  [Version 1] */
};

#define DCBX_VERSION_BASE_STR "DCBX Base Protocol V1.01 [Version 0]"
#define DCBX_VERSION_IEEE_STR "DCBX IEEE 802.1Qaz/D2.1  [Version 1]"

enum dcbxTlvTxEnable {
    DCBX_TLV_ENABLE_PFC       = (1 << 0),
    DCBX_TLV_ENABLE_ETS_CONF  = (1 << 1),
    DCBX_TLV_ENABLE_ETS_RECO  = (1 << 2),
    DCBX_TLV_ENABLE_APP       = (1 << 3),
    DCBX_TLV_ENABLE_CN        = (1 << 4),
    DCBX_TLV_ENABLE_ALL       = DCBX_TLV_ENABLE_PFC      |
                                DCBX_TLV_ENABLE_ETS_CONF |
                                DCBX_TLV_ENABLE_ETS_RECO |
                                DCBX_TLV_ENABLE_APP      |
                                DCBX_TLV_ENABLE_CN
};

#define DCBX_TLV_APCODE_PRI(apcode_) \
    (((apcode_) >> 21) & 0x0007)

#define DCBX_TLV_APCODE_SEL(apcode_) \
    (((apcode_) >> 16) & 0x0007)

#define DCBX_TLV_APCODE_PROTO(apcode_) \
    (((apcode_)      ) & 0xffff)

#define DCBX_TLV_APCODE(sel_,proto_,pri_) \
    ((((pri_) & 0x07) << 21) | (((sel_) & 0x07) << 16) | (((proto_) & 0xffff)))

#define DCBX_TLV_INVALID_APCODE 0xffffffff


/*****************************************************************************
 * lldpXdot1dcbxConfig
 *****************************************************************************/

/** DCBX Port Configuration ETS-Configuration Data */
struct lldpXdot1dcbxConfigETSConfiguration {
    /** The lldpXdot1dcbxConfigETSConfigurationTxEnable, which is
        defined as a truth value and configured by the network 
        management, determines whether the IEEE 802.1 organizationally
        defined ETS Configuration TLV transmission is allowed on a 
        given LLDP transmission capable port.

        The value of this object is restored from non-volatile
        storage after a re-initialization of the management system. */
    bool lldpXdot1dcbxConfigETSConfigurationTxEnable;
};

/** DCBX Port Configuration ETS-Recommendation Data */
struct lldpXdot1dcbxConfigETSRecommendation {
    /** The lldpXdot1dcbxConfigETSRecommendationTxEnable, which is
        defined as a truth value and configured by the network 
        management, determines whether the IEEE 802.1 organizationally
        defined ETS Recommendation TLV transmission is allowed on a 
        given LLDP transmission capable port.

        The value of this object is restored from non-volatile
        storage after a re-initialization of the management system. */
    bool lldpXdot1dcbxConfigETSRecommendationTxEnable;
};

/** DCBX Port Configuration PFC Data */
struct lldpXdot1dcbxConfigPFC {
    /** The lldpXdot1dcbxConfigPFCTxEnable, which is defined
        as a truth value and configured by the network management,
        determines whether the IEEE 802.1 organizationally defined
        Priority-based Flow Control TLV transmission is allowed on
        a given LLDP transmission capable port.

        The value of this object is restored from non-volatile
        storage after a re-initialization of the management system. */
    bool lldpXdot1dcbxConfigPFCTxEnable;
};

/** DCBX Port Configuration Application-Priority Data */
struct lldpXdot1dcbxConfigApplicationPriority {
    /** The lldpXdot1dcbxConfigApplicationPriorityTxEnable, which
        is defined as a truth value and configured by the network 
        management, determines whether the IEEE 802.1 organizationally
        defined Application Priority TLV transmission is allowed on
        a given LLDP transmission capable port.

        The value of this object is restored from non-volatile
        storage after a re-initialization of the management system. */
    bool lldpXdot1dcbxConfigApplicationPriorityTxEnable;
};

/** DCBX Port Configuration Congestion-Notification Data */
struct lldpXdot1dcbxConfigCongestionNotification {
    /** The lldpXdot1dcbxConfigCongestionNotificationTxEnable, which
        is defined as a truth value and configured by the network 
        management, determines whether the IEEE 802.1 organizationally
        defined Congestion Notification TLV transmission is allowed on
        a given LLDP transmission capable port.

        The value of this object is restored from non-volatile
        storage after a re-initialization of the management system. */
    bool lldpXdot1dcbxConfigCongestionNotificationTxEnable;
};

/** DCBX Port Configuration Data */
struct lldpXdot1dcbxConfig {
    lldp_list          node;

    /* Index */

    /** The interface index value used to identify the local port
        associated with this entry. */
    int lldpV2LocPortIfIndex;

    /** An entry that controls selection of ETS Configuration
        TLVs to be transmitted on individual ports. */
    struct lldpXdot1dcbxConfigETSConfiguration lldpXdot1dcbxConfigETSConfiguration;

    /** An entry that controls selection of ETS Recommendation
        TLVs to be transmitted on port. */
    struct lldpXdot1dcbxConfigETSRecommendation lldpXdot1dcbxConfigETSRecommendation;

    /** An entry that controls selection of Priorit-based
        Flow Control TLVs to be transmitted on port. */
    struct lldpXdot1dcbxConfigPFC lldpXdot1dcbxConfigPFC;

    /** An entry that controls selection of Application Priority
        TLVs to be transmitted on port. */
    struct lldpXdot1dcbxConfigApplicationPriority lldpXdot1dcbxConfigApplicationPriority;

    /** An entry that controls selection of Congestion Notification
        TLVs to be transmitted on port. */
    struct lldpXdot1dcbxConfigCongestionNotification lldpXdot1dcbxConfigCongestionNotification;
};


/*****************************************************************************
 * lldpXdot1dcbxLocalData
 *****************************************************************************/

/** DCBX Port Local ETS-Configuration Data */
struct lldpXdot1dcbxLocETSConfiguration {
    /** Indicates if the credit-based shaper Traffic Selection
        Algorithm is supported on the local system. */
    bool lldpXdot1dcbxLocETSConCreditBasedShaperSupport;

    /** Indicates the number of Traffic Classes supported on the
        local system. A value of 0 indicates that 8 Traffic Classes
        are supported. */
    uint8_t lldpXdot1dcbxLocETSConTrafficClassesSupported;

    /** Indicates if the local system is willing to accept the
         ETS configuration recommended by the remote system. */
    bool lldpXdot1dcbxLocETSConWilling;

    /** Each octet corresponds to one traffic class.  The first octet
        corresponds to traffic class 0, the second to traffic class 1,
        and so on.  Each octet contains the bandwidth in percent to be 
        allocated to the traffic class.  Valid values are between
        0 and 100 inclusive.  The total of all eight octets must
        equal 100.

        Note that an octet string is used instead of a table to enable
        atomic programming of these values which is required to fulfill
        the requirement that they always total to 100. */
    uint8_t lldpXdot1dcbxLocETSConTrafficClassBandwidthTable[8];

    /** Indicates a traffic class to traffic selection algorithm
        assignment.

            0: Strict Priority
            1: Credit-based shaper
            2: Enhanced transmission selection
        3-254: Reserved for future standardization
          255: Vendor specific */
    uint8_t lldpXdot1dcbxLocETSConTrafficSelectionAlgorithmTable[8];

    /** Indicates the traffic class to which the priority is 
        assigned. 15 indicates that the priority is not assigned to 
        any traffic class. */
    uint8_t lldpXdot1dcbxLocETSConPriorityAssignmentTable[8];

    /* Data (Non-MIB) */

    /** Local ETS configuration TLVs transmitted counter. */
    uint32_t lldpXdot1dcbxLocETSConStatsTxTLVs;

    /** Data (Non-MIB, DBCX Version 0) */

    /** Indicates the current operational feature version (DCBX Version 0) */
    uint8_t dcbxV0LocETSConOperVersion;

    /** Indicates if sync error is active (DCBX Version 0) */
    bool dcbxV0LocETSConError;
};

/** DCBX Port Local ETS-Recommendation Data */
struct lldpXdot1dcbxLocETSRecommendation {
    /** Each octet corresponds to one traffic class.  The first octet
        corresponds to traffic class 0, the second to traffic class 1,
        and so on.  Each octet contains the bandwidth in percent that
        the remote station is recommending to be allocated to the 
        traffic class.  Valid values are between 0 and 100 inclusive. 
        The total of all eight octets must equal 100.

        Note that an octet string is used instead of a table to enable
        atomic programming of these values which is required to fulfill
        the requirement that they always total to 100. */
    uint8_t lldpXdot1dcbxLocETSRecoTrafficClassBandwidthTable[8];

    /** Indicates a traffic class to traffic selection algorithm
        assignment.

            0: Strict Priority
            1: Credit-based shaper
            2: Enhanced transmission selection
        3-254: Reserved for future standardization
          255: Vendor specific */
    uint8_t lldpXdot1dcbxLocETSRecoTrafficSelectionAlgorithmTable[8];

    /** Indicates the traffic class to which the priority is 
        assigned. 15 indicates that the priority is not assigned to 
        any traffic class. */
    uint8_t lldpXdot1dcbxLocETSRecoPriorityAssignmentTable[8];

    /* Data (Non-MIB) */

    /** Local ETS recommendation TLVs transmitted counter. */
    uint32_t lldpXdot1dcbxLocETSRecoStatsTxTLVs;
};

/** DCBX Port Local PFC Data */
struct lldpXdot1dcbxLocPFC {
    /** Indicates if the local system is willing to accept the 
        PFC configuration of the remote system. */
    bool lldpXdot1dcbxLocPFCWilling;

    /** Indicates if the local system is capable of bypassing
        MACsec processing when MACsec is disabled. */
    bool lldpXdot1dcbxLocPFCMBC;

    /** Indicates the number of traffic classes on the local device
        that may simultaneously have PFC enabled.  Zero indicates no
        limitation, i.e. all available traffic classes may have PFC
        enabled. */
    uint8_t lldpXdot1dcbxLocPFCCap;

    /** Indicates if PFC is enabled on the corresponding priority */
    bool lldpXdot1dcbxLocPFCEnable[8];

    /* Data (Non-MIB) */

    /** Local PFC TLVs transmitted counter. */
    uint32_t lldpXdot1dcbxLocPFCStatsTxTLVs;

    /** Data (Non-MIB, DBCX Version 0) */

    /** Indicates the current operational feature version (DCBX Version 0) */
    uint8_t dcbxV0LocPFCOperVersion;

    /** Indicates if sync error is active (DCBX Version 0) */
    bool dcbxV0LocPFCError;
};

/** DCBX Port Local Application-Priority Entry */
struct lldpXdot1dcbxLocApplicationPriorityAEPriority {
    /** Indicates the contents of the protocol object
        (lldpXdot1dcbxLocApplicationPriorityAEProtocol)
        1: Ethertype
        2: Well Known Port number over TCP, or SCTP
        3: Well Known Port number over UDP, or DCCP
        4: Well Known Port number over TCP, SCTP, UDP, and DCCP */
    uint8_t lldpXdot1dcbxLocApplicationPriorityAESelector;

    /** The protocol indicator of the type indicated by
        lldpXdot1dcbxLocApplicationPriorityAESelector. */
    uint16_t lldpXdot1dcbxLocApplicationPriorityAEProtocol;

    /* Data */

    /** The priority code point that should be used in
        frames transporting the protocol indicated by
        lldpXdot1dcbxLocApplicationPriorityAESelector and
        lldpXdot1dcbxLocApplicationPriorityAEProtocol. */
    uint8_t lldpXdot1dcbxLocApplicationPriorityAEPriority;

    /* Data (Non-MIB) */

    /** The hardware ACL rule-id used for this rule. ACL id
        is specified by DCBX_APPLICATION_PRIORITIES_ACL_ID. */
    uint32_t lldpXdot1dcbxLocApplicationPriorityAERuleId;
};

/** DCBX Port Local Application-Priority Data */
struct lldpXdot1dcbxLocApplicationPriority {
    /** Indicates if the local system is willing to accept
        the Application Priority configuration of the remote system. */
    bool lldpXdot1dcbxLocApplicationPriorityWilling;

    /** Table containing entries indicating the priority code point
        that should be used in frames transporting the protocol 
        indicated by lldpXdot1dcbxLocApplicationPriorityAESelector
        and lldpXdot1dcbxLocApplicationPriorityAEProtocol. */
    struct lldpXdot1dcbxLocApplicationPriorityAEPriority lldpXdot1dcbxLocApplicationPriorityAEPriority[DCBX_TLV_MAX_APPLICATION_PRIORITIES];

    /** Number of entries in lldpXdot1dcbxLocApplicationPriorityAEPriority Array */
    uint16_t lldpXdot1dcbxLocApplicationPriorityAEPriorityNum;

    /* Data (Non-MIB) */

    /** Local application-priority TLVs transmitted counter. */
    uint32_t lldpXdot1dcbxLocApplicationPriorityStatsTxTLVs;

    /** Data (Non-MIB, DBCX Version 0) */

    /** Indicates the current operational feature version (DCBX Version 0) */
    uint8_t dcbxV0LocApplicationPriorityOperVersion;

    /** Indicates if sync error is active (DCBX Version 0) */
    bool dcbxV0LocApplicationPriorityError;
};

/** DCBX Port Local Congestion-Notification Data */
struct lldpXdot1dcbxLocCongestionNotification {
    /** Indicates if CNPV is supported for the corresponding priority */
    bool lldpXdot1dcbxLocCongestionNotificationCnpvSupported[8];

    /** Indicates if CNPV is ready for the corresponding priority */
    bool lldpXdot1dcbxLocCongestionNotificationCnpvReady[8];

    /* Data (Non-MIB) */

    /** Local congestion-notification TLVs transmitted counter. */
    uint32_t lldpXdot1dcbxLocCongestionNotificationStatsTxTLVs;
};

/** DCBX Port Local Data */
struct lldpXdot1dcbxLocalData {
    lldp_list          node;

    /* Index */

    /** The interface index value used to identify the local port
        associated with this entry. */
    int lldpV2LocPortIfIndex;

    /* Data */

    /** Contains the information for the ETS Configuration TLV. */
    struct lldpXdot1dcbxLocETSConfiguration lldpXdot1dcbxLocETSConfiguration;

    /** This table contains one row per port for the IEEE 802.1
        organizationally defined LLDP ETS Recommendation TLV on 
        the local system known to this agent. */
    struct lldpXdot1dcbxLocETSRecommendation lldpXdot1dcbxLocETSRecommendation;

    /** Contains the information for the PFC TLV. */
    struct lldpXdot1dcbxLocPFC lldpXdot1dcbxLocPFC;

    /** Contains the information for the Application Priority TLV. */
    struct lldpXdot1dcbxLocApplicationPriority lldpXdot1dcbxLocApplicationPriority;

    /** Contains the information for the Congestion Notification TLV */
    struct lldpXdot1dcbxLocCongestionNotification lldpXdot1dcbxLocCongestionNotification;

    /** Data (Non-MIB, LLDP) */

    /** Indicates that port has multiple peers */
    bool lldpMultiplePeers;

    /* Data (Non-MIB, DCBX) */

    /** DCBX protocol version */
    enum dcbxProtocolVersion lldpXdot1dcbxVersion;

    /** Data (Non-MIB, DCBX Version 0) */

    /** Indicates the operational version (DCBX Version 0) */
    uint8_t  dcbxV0OperVersion;

    /** Indicates the last sequence (DCBX Version 0) */
    uint32_t dcbxV0SeqNo;

    /** Indicates the last acknowledge (DCBX Version 0) */
    uint32_t dcbxV0AckNo;

    /** Indicates the last acknowledge received from peer (DCBX Version 0) */
    uint32_t dcbxV0RcvdAckNo;

    /** Indicates that operational data changed (DCBX Version 0) */
    bool     dcbxV0Changed;
};


/*****************************************************************************
 * lldpXdot1dcbxRemoteData
 *****************************************************************************/

/** DCBX Remote Entry ETS-Configuration Data */
struct lldpXdot1dcbxRemETSConfiguration {
    /** Indicates that this entry is valid */
    bool lldpXdot1dcbxRemETSConValid;

    /** Indicates if the credit-based shaper Traffic Selection
        Algorithm is supported on the remote system. */
    bool lldpXdot1dcbxRemETSConCreditBasedShaperSupport;

    /** Indicates the number of Traffic Classes supported on the
        remote system. A value of 0 indicates that 8 Traffic Classes
        are supported. */
    uint8_t lldpXdot1dcbxRemETSConTrafficClassesSupported;

    /** Indicates if the local system is willing to accept the
         ETS configuration recommended by the remote system. */
    bool lldpXdot1dcbxRemETSConWilling;

    /** Each octet corresponds to one traffic class.  The first octet
        corresponds to traffic class 0, the second to traffic class 1,
        and so on.  Each octet contains the bandwidth in percent to be 
        allocated to the traffic class.  Valid values are between
        0 and 100 inclusive.  The total of all eight octets must
        equal 100.

        Note that an octet string is used instead of a table to enable
        atomic programming of these values which is required to fulfill
        the requirement that they always total to 100. */
    uint8_t lldpXdot1dcbxRemETSConTrafficClassBandwidthTable[8];

    /** Indicates a traffic class to traffic selection algorithm
        assignment.

            0: Strict Priority
            1: Credit-based shaper
            2: Enhanced transmission selection
        3-254: Reserved for future standardization
          255: Vendor specific */
    uint8_t lldpXdot1dcbxRemETSConTrafficSelectionAlgorithmTable[8];

    /** Indicates the traffic class to which the priority is 
        assigned. 15 indicates that the priority is not assigned to 
        any traffic class. */
    uint8_t lldpXdot1dcbxRemETSConPriorityAssignmentTable[8];

    /* Data (Non-MIB) */

    /** Remote ETS configuration TLVs received counter. */
    uint32_t lldpXdot1dcbxRemETSConStatsRxTLVs;

    /** Data (Non-MIB, DCBX Version 0) */

    /** Indicates the operational version (DCBX Version 0) */
    uint8_t dcbxV0RemETSConOperVersion;

    /** Indicates the max version (DCBX Version 0) */
    uint8_t dcbxV0RemETSConMaxVersion;

    /** Indicates if sync error is active (DCBX Version 0) */
    bool dcbxV0RemETSConError;
};

/** DCBX Remote Entry ETS-Recommendation Data */
struct lldpXdot1dcbxRemETSRecommendation {
    /** Indicates that this entry is valid */
    bool lldpXdot1dcbxRemETSRecoValid;

    /** Each octet corresponds to one traffic class.  The first octet
        corresponds to traffic class 0, the second to traffic class 1,
        and so on.  Each octet contains the bandwidth in percent that
        the remote station is recommending to be allocated to the 
        traffic class.  Valid values are between 0 and 100 inclusive. 
        The total of all eight octets must equal 100.

        Note that an octet string is used instead of a table to enable
        atomic programming of these values which is required to fulfill
        the requirement that they always total to 100. */
    uint8_t lldpXdot1dcbxRemETSRecoTrafficClassBandwidthTable[8];

    /** Indicates a traffic class to traffic selection algorithm
        assignment.

            0: Strict Priority
            1: Credit-based shaper
            2: Enhanced transmission selection
        3-254: Reserved for future standardization
          255: Vendor specific */
    uint8_t lldpXdot1dcbxRemETSRecoTrafficSelectionAlgorithmTable[8];

    /** Indicates the traffic class to which the priority is 
        assigned. 15 indicates that the priority is not assigned to 
        any traffic class. */
    uint8_t lldpXdot1dcbxRemETSRecoPriorityAssignmentTable[8];

    /* Data (Non-MIB) */

    /** Remote ETS recommendation TLVs received counter. */
    uint32_t lldpXdot1dcbxRemETSRecoStatsRxTLVs;
};

/** DCBX Remote Entry PFC Data */
struct lldpXdot1dcbxRemPFC {
    /** Indicates that this entry is valid */
    bool lldpXdot1dcbxRemPFCValid;

    /** Indicates if the remote system is willing to accept the 
        PFC configuration of the remote system. */
    bool lldpXdot1dcbxRemPFCWilling;

    /** Indicates if the remote system is capable of bypassing
        MACsec processing when MACsec is disabled. */
    bool lldpXdot1dcbxRemPFCMBC;

    /** Indicates the number of traffic classes on the remote device
        that may simultaneously have PFC enabled.  Zero indicates no
        limitation, i.e. all available traffic classes may have PFC
        enabled. */
    uint8_t lldpXdot1dcbxRemPFCCap;

    /** Indicates if PFC is enabled on the corresponding priority */
    bool lldpXdot1dcbxRemPFCEnable[8];

    /* Data (Non-MIB) */

    /** Remote PFC TLVs received counter. */
    uint32_t lldpXdot1dcbxRemPFCStatsRxTLVs;

    /** Data (Non-MIB, DCBX Version 0) */

    /** Indicates the operational version (DCBX Version 0) */
    uint8_t dcbxV0RemPFCOperVersion;

    /** Indicates the max version (DCBX Version 0) */
    uint8_t dcbxV0RemPFCMaxVersion;

    /** Indicates if sync error is active (DCBX Version 0) */
    bool dcbxV0RemPFCError;
};

/** DCBX Remote Entry Application-Priority Entry */
struct lldpXdot1dcbxRemApplicationPriorityAEPriority {
    /** Indicates the contents of the protocol object
        (lldpXdot1dcbxRemApplicationPriorityAEProtocol)
        1: Ethertype
        2: Well Known Port number over TCP, or SCTP
        3: Well Known Port number over UDP, or DCCP
        4: Well Known Port number over TCP, SCTP, UDP, and DCCP */
    uint8_t lldpXdot1dcbxRemApplicationPriorityAESelector;

    /** The protocol indicator of the type indicated by
        lldpXdot1dcbxRemApplicationPriorityAESelector. */
    uint16_t lldpXdot1dcbxRemApplicationPriorityAEProtocol;

    /* Data */

    /** The priority code point that should be used in
        frames transporting the protocol indicated by
        lldpXdot1dcbxRemApplicationPriorityAESelector and
        lldpXdot1dcbxRemApplicationPriorityAEProtocol. */
    uint8_t lldpXdot1dcbxRemApplicationPriorityAEPriority;
};

/** DCBX Remote Entry Application-Priority Data */
struct lldpXdot1dcbxRemApplicationPriority {
    /** Indicates that this entry is valid */
    bool lldpXdot1dcbxRemApplicationPriorityValid;

    /** Indicates if the remote system is willing to accept
        the Application Priority configuration of the remote system. */
    bool lldpXdot1dcbxRemApplicationPriorityWilling;

    /** Table containing entries indicating the priority code point
        that should be used in frames transporting the protocol 
        indicated by lldpXdot1dcbxRemApplicationPriorityAESelector
        and lldpXdot1dcbxRemApplicationPriorityAEProtocol. */
    struct lldpXdot1dcbxRemApplicationPriorityAEPriority lldpXdot1dcbxRemApplicationPriorityAEPriority[DCBX_TLV_MAX_APPLICATION_PRIORITIES];

    /** Number of entries in lldpXdot1dcbxLocApplicationPriorityAEPriority Array */
    uint16_t lldpXdot1dcbxRemApplicationPriorityAEPriorityNum;

    /* Data (Non-MIB) */

    /** Remote application-priority TLVs received counter. */
    uint32_t lldpXdot1dcbxRemApplicationPriorityStatsRxTLVs;

    /** Data (Non-MIB, DCBX Version 0) */

    /** Indicates the operational version (DCBX Version 0) */
    uint8_t dcbxV0RemApplicationPriorityOperVersion;

    /** Indicates the max version (DCBX Version 0) */
    uint8_t dcbxV0RemApplicationPriorityMaxVersion;

    /** Indicates if sync error is active (DCBX Version 0) */
    bool dcbxV0RemApplicationPriorityError;
};

/** DCBX Remote Entry Congestion-Notification Data */
struct lldpXdot1dcbxRemCongestionNotification {
    /** Indicates that this entry is valid */
    bool lldpXdot1dcbxRemCongestionNotificationValid;

    /** Indicates if CNPV is supported for the corresponding priority */
    bool lldpXdot1dcbxRemCongestionNotificationCnpvSupported[8];

    /** Indicates if CNPV is ready for the corresponding priority */
    bool lldpXdot1dcbxRemCongestionNotificationCnpvReady[8];

    /* Data (Non-MIB) */

    /** Remote congestion-notification TLVs received counter. */
    uint32_t lldpXdot1dcbxRemCongestionNotificationStatsRxTLVs;
};

/** DCBX Remote Entry Data */
struct lldpXdot1dcbxRemoteData {
    lldp_list          node;

    /* Index */

    /** Time-stamp of the record last change. */
    uint32_t lldpV2RemTimeMark;

    /** The interface index value used to identify the port
        associated with this entry.

        The value of this object is used as an index to the lldpV2RemTable.*/
    uint16_t lldpV2RemLocalIfIndex;
  
    /** The index value used to identify the destination
        MAC address associated with this entry. Its value identifies
        the row in the lldpV2DestAddressTable where the MAC address
        can be found.

        The value of this object is used as an index to the lldpV2RemTable. */
    uint64_t lldpV2RemLocalDestMACAddress;

    /** This object represents an arbitrary local integer value used
        by this agent to identify a particular connection instance,
        unique only for the indicated remote system.

        An agent is encouraged to assign monotonically increasing
        index values to new entries, starting with one, after each
        reboot. It is considered unlikely that the lldpRemIndex
        can wrap between reboots. */
    uint16_t lldpV2RemIndex; 

    /* Data */

    /** Indicates that this entry is valid */
    bool lldpXdot1dcbxRemoteDataValid;

    /** Information about the IEEE 802.1 organizational defined
        ETS Configuration TLV LLDP extension. */
    struct lldpXdot1dcbxRemETSConfiguration lldpXdot1dcbxRemETSConfiguration;

    /** Information about the IEEE 802.1 organizational defined
        ETS Recommendation TLV LLDP extension. */
    struct lldpXdot1dcbxRemETSRecommendation lldpXdot1dcbxRemETSRecommendation;

    /** Information about the IEEE 802.1 organizational defined
        PFC TLV LLDP extension. */
    struct lldpXdot1dcbxRemPFC lldpXdot1dcbxRemPFC;

    /** Information about the IEEE 802.1 organizational defined
        Application Priority TLV LLDP extension. */
    struct lldpXdot1dcbxRemApplicationPriority lldpXdot1dcbxRemApplicationPriority;

    /** Information about the IEEE 802.1 organizational defined
        Congestion Notification TLV LLDP extension. */
    struct lldpXdot1dcbxRemCongestionNotification lldpXdot1dcbxRemCongestionNotification;
};


/*****************************************************************************
 * lldpXdot1dcbxAdminData
 *****************************************************************************/

/** DCBX Port Admin ETS-Configuration Data */
struct lldpXdot1dcbxAdminETSConfiguration {
    /** Indicates if the credit-based shaper Traffic Selection
        Algorithm is supported on the local system. */
    bool lldpXdot1dcbxAdminETSConCreditBasedShaperSupport;

    /** Indicates the number of Traffic Classes supported on the
        local system. A value of 0 indicates that 8 Traffic Classes
        are supported. */
    uint8_t lldpXdot1dcbxAdminETSConTrafficClassesSupported;

    /** Indicates if the local system is willing to accept the
         ETS configuration recommended by the remote system. */
    bool lldpXdot1dcbxAdminETSConWilling;

    /** Each octet corresponds to one traffic class.  The first octet
        corresponds to traffic class 0, the second to traffic class 1,
        and so on.  Each octet contains the bandwidth in percent to be 
        allocated to the traffic class.  Valid values are between
        0 and 100 inclusive.  The total of all eight octets must
        equal 100.

        Note that an octet string is used instead of a table to enable
        atomic programming of these values which is required to fulfill
        the requirement that they always total to 100. */
    uint8_t lldpXdot1dcbxAdminETSConTrafficClassBandwidthTable[8];

    /** Indicates a traffic class to traffic selection algorithm
        assignment.

            0: Strict Priority
            1: Credit-based shaper
            2: Enhanced transmission selection
        3-254: Reserved for future standardization
          255: Vendor specific */
    uint8_t lldpXdot1dcbxAdminETSConTrafficSelectionAlgorithmTable[8];

    /** Indicates the traffic class to which the priority is 
        assigned. 15 indicates that the priority is not assigned to 
        any traffic class. */
    uint8_t lldpXdot1dcbxAdminETSConPriorityAssignmentTable[8];
};

/** DCBX Port Admin ETS-Recommendation Data */
struct lldpXdot1dcbxAdminETSRecommendation {
    /** Each octet corresponds to one traffic class.  The first octet
        corresponds to traffic class 0, the second to traffic class 1,
        and so on.  Each octet contains the bandwidth in percent that
        the remote station is recommending to be allocated to the 
        traffic class.  Valid values are between 0 and 100 inclusive. 
        The total of all eight octets must equal 100.

        Note that an octet string is used instead of a table to enable
        atomic programming of these values which is required to fulfill
        the requirement that they always total to 100. */
    uint8_t lldpXdot1dcbxAdminETSRecoTrafficClassBandwidthTable[8];

    /** Indicates a traffic class to traffic selection algorithm
        assignment.

            0: Strict Priority
            1: Credit-based shaper
            2: Enhanced transmission selection
        3-254: Reserved for future standardization
          255: Vendor specific */
    uint8_t lldpXdot1dcbxAdminETSRecoTrafficSelectionAlgorithmTable[8];

    /** Indicates the traffic class to which the priority is 
        assigned. 15 indicates that the priority is not assigned to 
        any traffic class. */
    uint8_t lldpXdot1dcbxAdminETSRecoPriorityAssignmentTable[8];
};

/** DCBX Port Admin PFC Data */
struct lldpXdot1dcbxAdminPFC {
    /** Indicates if the local system is willing to accept the 
        PFC configuration of the remote system. */
    bool lldpXdot1dcbxAdminPFCWilling;

    /** Indicates if the local system is capable of bypassing
        MACsec processing when MACsec is disabled. */
    bool lldpXdot1dcbxAdminPFCMBC;

    /** Indicates the number of traffic classes on the local device
        that may simultaneously have PFC enabled.  Zero indicates no
        limitation, i.e. all available traffic classes may have PFC
        enabled. */
    uint8_t lldpXdot1dcbxAdminPFCCap;

    /** Indicates if PFC is enabled on the corresponding priority */
    bool lldpXdot1dcbxAdminPFCEnable[8];
};

/** DCBX Port Admin Application-Priority Entry */
struct lldpXdot1dcbxAdminApplicationPriorityAEPriority {

    /* Index */

    /** Indicates the contents of the protocol object
        (lldpXdot1dcbxAdminApplicationPriorityAEProtocol)
        1: Ethertype
        2: Well Known Port number over TCP, or SCTP
        3: Well Known Port number over UDP, or DCCP
        4: Well Known Port number over TCP, SCTP, UDP, and DCCP */
    uint8_t lldpXdot1dcbxAdminApplicationPriorityAESelector;

    /** The protocol indicator of the type indicated by
        lldpXdot1dcbxAdminApplicationPriorityAESelector. */
    uint16_t lldpXdot1dcbxAdminApplicationPriorityAEProtocol;

    /* Data */

    /** The priority code point that should be used in
        frames transporting the protocol indicated by
        lldpXdot1dcbxAdminApplicationPriorityAESelector and
        lldpXdot1dcbxAdminApplicationPriorityAEProtocol. */
    uint8_t lldpXdot1dcbxAdminApplicationPriorityAEPriority;
};

/** DCBX Port Admin Application-Priority Data */
struct lldpXdot1dcbxAdminApplicationPriority {
    /** Indicates if the local system is willing to accept
        the Application Priority configuration of the remote system. */
    bool lldpXdot1dcbxAdminApplicationPriorityWilling;

    /** Table containing entries indicating the priority code point
        that should be used in frames transporting the protocol 
        indicated by lldpXdot1dcbxAdminApplicationPriorityAESelector
        and lldpXdot1dcbxAdminApplicationPriorityAEProtocol. */
    struct lldpXdot1dcbxAdminApplicationPriorityAEPriority lldpXdot1dcbxAdminApplicationPriorityAEPriority[DCBX_TLV_MAX_APPLICATION_PRIORITIES];

    /** Number of entries in lldpXdot1dcbxLocApplicationPriorityAEPriority Array */
    uint16_t lldpXdot1dcbxAdminApplicationPriorityAEPriorityNum;
};

/** DCBX Port Admin Congestion-Notification Data */
struct lldpXdot1dcbxAdminCongestionNotification {
    /** Indicates if CNPV is supported for the corresponding priority */
    bool lldpXdot1dcbxAdminCongestionNotificationCnpvSupported[8];

    /** Indicates if CNPV is ready for the corresponding priority */
    bool lldpXdot1dcbxAdminCongestionNotificationCnpvReady[8];
};

/** DCBX Port Admin Data */
struct lldpXdot1dcbxAdminData {
    lldp_list          node;

    /* Index */

    /** The interface index value used to identify the local port
        associated with this entry. */
    int lldpV2LocPortIfIndex;

    /* Data */

    /** Contains the information for the ETS Configuration TLV. */
    struct lldpXdot1dcbxAdminETSConfiguration lldpXdot1dcbxAdminETSConfiguration;

    /** This table contains one row per port for the IEEE 802.1
        organizationally defined LLDP ETS Recommendation TLV on 
        the local system known to this agent. */
    struct lldpXdot1dcbxAdminETSRecommendation lldpXdot1dcbxAdminETSRecommendation;

    /** Contains the information for the PFC TLV. */
    struct lldpXdot1dcbxAdminPFC lldpXdot1dcbxAdminPFC;

    /** Contains the information for the Application Priority TLV. */
    struct lldpXdot1dcbxAdminApplicationPriority lldpXdot1dcbxAdminApplicationPriority;

    /** Contains the information for the Congestion Notification TLV */
    struct lldpXdot1dcbxAdminCongestionNotification lldpXdot1dcbxAdminCongestionNotification;

    /* Data (Non-MIB, DCBX) */

    /** DCBX protocol version */
    enum dcbxProtocolVersion lldpXdot1dcbxVersion;
};


/*****************************************************************************
 * lldpXdot1dcbxObjects
 *****************************************************************************/

/** DCBX Database Objects */
struct lldpXdot1dcbxObjects {
    /** DCBX local configuration */
    struct lldpXdot1dcbxConfig *lldpXdot1dcbxConfig[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpXdot1dcbxConfigList;

    /** DCBX admin data (local operational status) */
    struct lldpXdot1dcbxLocalData *lldpXdot1dcbxLocalData[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpXdot1dcbxLocalDataList;

    /** DCBX remote entries (DCBX is expected to operate over a point to point link,
        thus we index remote data based on local port). */
    struct lldpXdot1dcbxRemoteData *lldpXdot1dcbxRemoteData[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpXdot1dcbxRemoteDataList;

    /** DCBX admin data (local admin settings) */
    struct lldpXdot1dcbxAdminData *lldpXdot1dcbxAdminData[LLDP_MAX_LOCAL_PORTS];
    lldp_list lldpXdot1dcbxAdminDataList;

    /** protocol interface to physical layer */
    struct dcbxPhysicalIf        *physical;

    /** database lock */
    pthread_mutex_t               lock;

    /** remote index counter (for unique id) */
    uint16_t                      remoteIndex;
};


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/

extern struct lldpXdot1dcbxObjects *dcbxDB;


/*****************************************************************************
 * Data Management Functions 
 *****************************************************************************/

/** Create DCBX Database */
struct lldpXdot1dcbxObjects    *dcbxObjectsCreate();

/** Delete DCBX Database */
int                             dcbxObjectsDestroy(struct lldpXdot1dcbxObjects *obj);

/** Create Port Configuration Entry */
struct lldpXdot1dcbxConfig     *dcbxConfigCreate(int portNum);

/** Delete Port Configuration Entry */
int                             dcbxConfigDestroy(struct lldpXdot1dcbxConfig *obj);

/** Get Port Configuration Entry from Local PortNum */
struct lldpXdot1dcbxConfig     *dcbxConfigGet(int portNum);

/** Create Local Port Entry */
struct lldpXdot1dcbxLocalData  *dcbxLocalDataCreate(int portNum);

/** Delete Local Port Entry */
int                             dcbxLocalDataDestroy(struct lldpXdot1dcbxLocalData *obj);

/** Get Local Port Entry from Local PortNum */
struct lldpXdot1dcbxLocalData  *dcbxLocalDataGet(int portNum);

/** Create Remote Port Entry */
struct lldpXdot1dcbxRemoteData *dcbxRemoteDataCreate(int portNum, int remoteIndex);

/** Delete Remote Port Entry */
int                             dcbxRemoteDataDestroy(struct lldpXdot1dcbxRemoteData *obj);

/** Get Remote Port Entry from Local PortNum */
struct lldpXdot1dcbxRemoteData *dcbxRemoteDataGet(int portNum);

/** Create Admin Port Entry */
struct lldpXdot1dcbxAdminData  *dcbxAdminDataCreate(int portNum);

/** Delete Admin Port Entry */
int                             dcbxAdminDataDestroy(struct lldpXdot1dcbxAdminData *obj);

/** Get Admin Port Entry from Local PortNum */
struct lldpXdot1dcbxAdminData  *dcbxAdminDataGet(int portNum);

#endif /* DCBX_MIB_H */
