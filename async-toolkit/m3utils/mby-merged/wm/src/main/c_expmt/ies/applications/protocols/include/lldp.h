/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            lldp.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for LLDP manager.
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

#ifndef LLDP_H
#define LLDP_H

#include <stdint.h>
#include <stdlib.h>

#include <lldp_mib.h>
#include <lldp_tlv.h>
#include <lldp_list.h>

/** This interface is implemented by higher level LLDP protocols such as DCBX,
    which wishes to gets incoming/outgoing TLV events. */
struct lldpProtocolIf {
    /** Initialize */
    int (*initializeModules)();

    /** Terminate */
    int (*terminateModules)();

    /** Handle ingress TLVs (full TLV chain is provided to this callback) */
    int (*handleIngressTLVs)(int portNum, const struct lldpTlv *tlvChain, const struct lldpMsap *msap, uint64_t remoteMac, int rxTTL);

    /** Handle remote port learning */
    int (*handleRemoteLearn)(int portNum, const struct lldpMsap *msap);

    /** Handle remote port aging */
    int (*handleRemoteAging)(int portNum, const struct lldpMsap *msap);

    /** Append organizationally TLV before it is sent to physical layer */
    int (*appendEgressTLVs)(int portNum, struct lldpTlv *tlvChain);

    /** Protocol object */
    void *obj;
};

#define LLDP_PROTOCOL(func_,...) \
    do { \
        if (lldpDB->protocol && lldpDB->protocol->func_) \
            lldpDB->protocol->func_(__VA_ARGS__); \
    } while (0)


/** The lldpPhysicalIf interface is implemented by low level physical layers, 
    which provides LLDP interface for sending packets and queuring the ports
    status */
struct lldpPhysicalIf {
    /** Send egress packet to physical port */
    int (*sendEgressPacket)(int portNum, uint8_t *buf, size_t size);

    /** Get port link status */
    int (*getLinkStatus)(int portNum, bool *linkStatus);
};

#define LLDP_PHYSICAL(func_,...) \
    do { \
        if (lldpDB->physical && lldpDB->physical->func_) \
            lldpDB->physical->func_(__VA_ARGS__); \
    } while (0)


/*****************************************************************************
 * Initialization 
 *****************************************************************************/

/** Initialize LLDP protocol */
int lldpInitialize(uint64_t chassisMacAddress, struct lldpPhysicalIf *physicalIf);

/** Terminate LLDP protocol */
int lldpTerminate(void);

/** Add port to LLDP protocol */
int lldpAddPort(int portNum, int portIdSubtype, char *portId, size_t portIdLen, char *portDesc);

/** Remove port from LLDP protocol */
int lldpRemovePort(int portNum);

/** Register / unregister organizationally protocol */
int lldpRegisterProtocol(struct lldpProtocolIf *protocolIf);
int lldpUnregisterProtocol(struct lldpProtocolIf *protocolIf);

/** Notify LLDP upon local change in port status/configuration */
int lldpHandleLocalChange(int portNum);

/*****************************************************************************
 * Managment
 *****************************************************************************/

/** Set TX timing */
int lldpSetTxTiming(int msgTxInterval, int msgTxHold, int reinitDelay, int txDelay);

/** Set chassis-id */
int lldpSetChassisId(int chassisIdSubtype, char *chassisId, size_t chassisIdLen);

/** Set system-name */
int lldpSetSystemName(char *sysName);

/** Set system-description */
int lldpSetSystemDescription(char *sysDesc);

/** Set system-capabilities */
int lldpSetSystemCapabilities(uint16_t sysCapSupported, uint16_t sysCapEnabled);

/** Change port admin-status */
int lldpSetPortAdminStatus(int portNum, int adminStatus);

/** Change port TLVs tx-enable (bit-mask of lldpTlvTxEnable) */
int lldpSetTLVsTxEnable(int portNum, uint16_t tlvsTxMask, uint16_t tlvsTxEnable);

/** Change port-id */
int lldpSetPortId(int portNum, int portIdSubtype, char *portId, size_t portIdLen);

/** Change port-description */
int lldpSetPortDesc(int portNum, char *portDesc);


/*****************************************************************************
 * Events from physical layer
 *****************************************************************************/

/** Handle ingress packet to LLDP agent */
int lldpHandleIngressPacket(int portNum, uint8_t *buf, size_t size);

/** Handle port link-status change */
int lldpHandleLinkStatusChange(int portNum, int linkStatus);


/*****************************************************************************
 * Status
 *****************************************************************************/

/** Dump LLDP Configuration */
int lldpShowConfiguration(int portNum);

/** Dump LLDP Local Data */
int lldpShowLocalData(int portNum);

/** Dump LLDP Remote Data */
int lldpShowRemoteData(int portNum);

/** Dump LLDP Counters */
int lldpShowCounters(int portNum);


/*****************************************************************************
 * Debug
 *****************************************************************************/

int lldpSetLogLevel(int logLevel);

#endif /* LLDP_H */

