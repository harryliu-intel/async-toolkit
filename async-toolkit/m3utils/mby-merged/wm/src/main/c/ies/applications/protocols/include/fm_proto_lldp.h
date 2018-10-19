/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_proto_lldp.h
 * Creation Date:   September 1, 2010
 * Description:     Prototypes and structure definitions for Fulcrum 
 *                  instantiation of LLDP.
 *
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

#ifndef LLDP_FULCRUM_H
#define LLDP_FULCRUM_H

#define FM_LLDP_MAX_SYSTEM_SWITCHES  64
#define FM_LLDP_MAX_SWITCH_PORTS     64
#define FM_LLDP_MAX_PORTS            4096

/** Initialize Fulcrum LLDP */
fm_status fmLldpInitialize(fm_macaddr chassisMacAddress);

/** Terminate Fulcrum LLDP */
fm_status fmLldpTerminate();

/** Add Fulcrum Port to LLDP */
fm_status fmLldpAddPort(fm_int sw, fm_int port, fm_int portNum, char *portIfName, char *portDesc);

/** Remove Fulcrum Port from LLDP */
fm_status fmLldpRemovePort(fm_int portNum);

/** Check if LLDP manager is running */
fm_bool   fmLldpIsRunning();

/** Check if LLDP port is running */
fm_bool   fmLldpPortIsRunning(fm_int portNum);

/** Callback from packet classifier */
fm_status fmPacketHandleLLDP(fm_eventPktRecv *event);


/*****************************************************************************
 * Management
 *****************************************************************************/

/** Set TX timing */
int fmLldpSetTxTiming(int msgTxInterval, int msgTxHold, int reinitDelay, int txDelay);

/** Set chassis id */
int fmLldpSetChassisId(int chassisIdSubtype, char *chassisId, size_t chassisIdLen);

/** Set system name */
int fmLldpSetSystemName(char *sysName);

/** Set system description */
int fmLldpSetSystemDescription(char *sysDesc);

/** Set system capabilities */
int fmLldpSetSystemCapabilities(int sysCapSupported, int sysCapEnabled);

/** Change port admin-status */
int fmLldpSetPortAdminStatus(int portNum, int adminStatus);

/** Change port TLVs tx-enable (bit-mask of lldpTlvTxEnable) */
int fmLldpSetTLVsTxEnable(int portNum, int tlvsTxMask, int tlvsTxEnable);

/** Change port id */
int fmLldpSetPortId(int portNum, int portIdSubtype, char *portId, size_t portIdLen);

/** Change port description */
int fmLldpSetPortDesc(int portNum, char *portDesc);


/*****************************************************************************
 * Status
 *****************************************************************************/

/** Dump LLDP Configuration */
fm_status fmLldpShowConfiguration(int portNum);

/** Dump LLDP Local Data */
fm_status fmLldpShowLocalData(int portNum);

/** Dump LLDP Remote Data */
fm_status fmLldpShowRemoteData(int portNum);

/** Dump LLDP Counters */
fm_status fmLldpShowCounters(int portNum);


/*****************************************************************************
 * Debug
 *****************************************************************************/

/** Change LLDP Log Level */
fm_status fmLldpSetLogLevel(int logLevel);

#endif /* LLDP_FULCRUM_H */
