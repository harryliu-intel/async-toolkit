/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_init.h
 * Creation Date:   April 20, 2005
 * Description:     Prototypes for initialization and switch
 *                  status/information retrieval functions for the API
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_INIT_H
#define __FM_FM_API_INIT_H


/**************************************************/
/** \ingroup macroSynonym
 * A legacy synonym for ''fmGetSwitchCount''.
 **************************************************/
#define fmSwitchCount fmGetSwitchCount


/**************************************************/
/** \ingroup typeEnum
 *  Referemced by ''fm_schedulerConfig'', these
 *  are the set of possible modes for initializing the 
 *  scheduler.
 **************************************************/
typedef enum
{
    /** The scheduler initialization will be done based on the number
     *  of supported ports. */ 
    FM_SCHED_INIT_MODE_AUTOMATIC = 0,

    /** The scheduler initialization will be done directly from the 
     *  token list provided by the caller. */ 
    FM_SCHED_INIT_MODE_MANUAL,

    /** The scheduler initialization will not be done by the API, the 
     *  platform layer is responsible for scheduler initialization. */ 
    FM_SCHED_INIT_MODE_NONE,

} fm_schedulerConfigMode;


/**************************************************/
/** \ingroup typeStruct
 * A structure returned by ''fmGetSwitchInfo'' for
 * communicating information about the switch.
 **************************************************/
typedef struct _fm_switchInfo
{
    /** EBI clock speed in MHZ. */
    fm_int           ebiClock;

    /** FH Ref clock speed in MHZ. */
    fm_int           fhRefClock;

    /** Switch device version number. */
    fm_int           version;

    /** Switch device part number. */
    fm_int           partNumber;

    /** JEDEC manufacturer's ID for Intel. */
    fm_int           jtagId;

    /** Switch Family */
    fm_switchFamily  switchFamily;

    /** Switch Model */
    fm_switchModel   switchModel;

    /** Switch device version ID */
    fm_switchVersion switchVersion;

    /** The number of ports supported by the device, including the CPU port. */
    fm_int           numPorts;

    /** The number of cardinal ports on the switch. */
    fm_int           numCardPorts;

    /** The number of VLANs supported by the device. */
    fm_int           maxVLAN;

    /** The number of port mirrors supported by the device. */
    fm_int           maxPortMirrors;

    /** The number of triggers supported by the device. */
    fm_int           maxTrigs;

    /** The number of LAGs supported by the device. */
    fm_int           maxLags;

    /** The maximum number of ports per LAG supported by the device. */
    fm_int           maxPortsPerLag;

    /** The number of VLANs for which statistics may be
     *  simultaneously collected. */
    fm_int           maxVlanCounters;

    /** Number of bytes of on-chip RAM. */
    fm_int           memorySize;

    /** Number of bytes of available on-chip RAM. */
    fm_int           availableMemorySize;

    /** Package type (not supported). */
    fm_int           packageType;

    /** Indicates whether the switch is UP or not. */
    fm_bool          up;

} fm_switchInfo;


/**************************************************/
/** \ingroup typeStruct
 *  Structure used to hold the FIBM configuration
 *  for a switch. Used with the
 *  FM_PLATFORM_ATTR_FIBM_CONFIG platform attribute
 *  and as an argument to the platform-defined
 *  function pointed to by the ''GetFibmSwitchConfig''
 *  function pointer.
 **************************************************/
typedef struct _fm_fibmSwitchConfig
{
    /** Master switch number that controls this remote switch.
     *  This is used for multiple master switch support.
     *  Set to -1, if not used, such as when being controlled from
     *  a workstation with a NIC. */
    fm_int                masterSwitch;

    /** The glort on the master switch that can process FIBM messages.
     *  This is normally the master CPU glort. */
    fm_uint32             masterGlort;
    
    /** The glort on the slave/remote switch that directs FIBM message to 
     *  the CPU port. An entry with this glort must be created in the CAM to 
     *  send packets with this glort to the slave/remote CPU port so 
     *  the slave switch can process the FIBM packets. */
    fm_uint32             slaveGlort;

    /** The logical port of the master switch that is connected to the  
     *  slave/remote switch. Set to -1 if not used, such as when being 
     *  controlled from a workstation with a NIC. */
    fm_int                masterMgmtPort; 

    /** The logical port of the slave/remote switch that is connected to the
     *  master switch. */
    fm_int                slaveMgmtPort; 
    
} fm_fibmSwitchConfig;


/**************************************************/
/** \ingroup typeStruct
 *  Used as the argument type for the 
 *  ''FM_PARITY_SWEEPER_CONFIG'' switch attribute.
 *  Specifies the behavior of the parity sweeper
 *  for the switch.
 *                                          \lb\lb
 *  Note that this configuration has no effect if
 *  the ''api.paritySweeper.enable'' API attribute
 *  is not set to TRUE.
 **************************************************/
typedef struct _fm_paritySweeperConfig 
{
    /** Indicates whether parity sweeping will occur on this switch. 
     *  the default for switches accessed via FIBM will be FALSE.
     *  The default for non-FIBM switches will be TRUE. */
    fm_bool enabled;

    /** Indicates the number of register reads performed by the parity 
     *  sweeper between sleep cycles. A larger number makes the sweeper
     *  scan the entire chip more quickly, resulting in faster reporting
     *  of detected errors, but consumes more CPU bandwidth
     *  resulting in a negative impact on the perfomance of other API threads. 
     *  The ''api.paritySweeper.readBurstSize'' API attribute provides the 
     *  default value for this parameter. */
    fm_int  readBurstSize;

    /** Indicates how long the parity sweeper should sleep between read bursts.
     *  A smaller number makes the sweeper scan the entire chip more quickly, 
     *  resulting in faster reporting of detected errors, but consumes more 
     *  CPU bandwidth resulting in a negative impact on the perfomance of 
     *  other API threads. The ''api.paritySweeper.sleepPeriod'' API attribute 
     *  provides the default value for this parameter. */
    fm_int  sleepPeriod;

} fm_paritySweeperConfig;


/**************************************************/
/** \ingroup typeStruct
 *  Referenced by ''fm_schedulerConfig'', this
 *  structure is used to hold scheduler token
 *  information. 
 **************************************************/
typedef struct _fm_schedulerToken
{
    /** The token's associated physical port. */ 
    fm_int  port;

    /** Indicates if the token should be locked or not. */ 
    fm_bool locked;

    /** Indicates if the token should be synchronized or not. */ 
    fm_bool synchronized;

    /** Indicates if the token should be inactive (true if used for balancing)
     *  or active (false if used by a port). */
    fm_bool inactive;
    
} fm_schedulerToken;


/**************************************************/
/** \ingroup typeStruct
 *  Used as the argument type for the 
 *  ''fmPlatformGetSchedulerConfig'' platform
 *  service. Specifies the mode and data for
 *  initializing the scheduler. 
 *                                              \lb\lb
 *  See ''fmPlatformGetSchedulerConfig'' for 
 *  more information.
 **************************************************/
typedef struct _fm_schedulerConfig 
{
    /** Indicates the mode in which the scheduler will be initialized.
     *  Depending on the mode, different fields of this structure need to
     *  be filled. Refer to each structure field to know which fields
     *  are required for each mode. */ 
    fm_schedulerConfigMode mode;

    /** List of 10G physical ports supported by the platform.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_AUTOMATIC''.
     *                                                                  \lb\lb
     *  Note that this pointer points to an array of size equal to the
     *  maximum number of 10G ports for this chip family (72 on FM6000). */ 
    fm_int *physPortsFor10G;

    /** The number of 10G ports loaded in the physPortsFor10G array.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_AUTOMATIC''.
     *                                                                  \lb\lb
     *  Note that this field should not exceed the maximum number of
     *  10G ports for this chip family (72 on FM6000). */ 
    fm_int numPortsFor10G;

    /** List of 40G physical ports supported by the platform.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_AUTOMATIC''.
     *                                                                  \lb\lb
     *  Note that this pointer points to an array of size equal to the
     *  maximum number of 40G ports for this chip family (18 on FM6000). */ 
    fm_int *physPortsFor40G;

    /** The number of 40G ports loaded in the physPortsFor40G array.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_AUTOMATIC''.
     *                                                                  \lb\lb
     *  Note that this field should not exceed the maximum number of
     *  40G ports for this chip family (18 on FM6000). */ 
    fm_int numPortsFor40G;

    /** List of tokens in the order that they will be initialized.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_MANUAL''.
     *                                                                  \lb\lb
     *  Note that this field should not exceed the maximum number of
     *  tokens for this chip family (74 on FM6000).  */ 
    fm_schedulerToken *tokenList;

    /** The number of tokens in tokenList.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_MANUAL'' or
     *  ''FM_SCHED_INIT_MODE_NONE''.
     *                                                                  \lb\lb
     *  Note that this field should not exceed the maximum number of
     *  tokens for this chip family (74 on FM6000). */ 
    fm_int nbTokens;

    /** The number of tokens with the synchronized bit.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_NONE''. */
    fm_int nbSyncTokens;

    /** List of next ports for locked tokens. This list will be written
     *  as-is to HW.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_MANUAL''.
     *                                                                  \lb\lb
     *  Note that this pointer points to an array of size equal to the number
     *  of valid next port config registers for this chip family (20 on 
     *  FM6000). */ 
    fm_uint32 *nextPortCfg;

    /** List of slow ports. This list will be written as-is to HW.
     *                                                                  \lb\lb
     *  This field is only used when mode is ''FM_SCHED_INIT_MODE_MANUAL''.
     *                                                                  \lb\lb
     *  Note that this pointer points to an array of size equal to the number
     *  of valid slow port config registers for this chip family (5 on 
     *  FM6000). */ 
    fm_uint16 *slowPort;

    /** Mask to skip post validation tests for the specified ring. This
     *  parameter should be set to 0, unless otherwise specified by
     *  Intel. */
    fm_uint32 postValidationSkipMask;

} fm_schedulerConfig;


/** DEPRECATED. The maximum number of entries in the MAC Address Table. This
 *  constant should no longer be referenced by application programs as the
 *  size of the MA Table can vary according to the family of the device.
 *  Family-specific constants should be used instead, such as
 *  ''FM2000_MAX_ADDR'', ''FM4000_MAX_ADDR'' and ''FM6000_MAX_ADDR''.
 *  \ingroup constSystem */
#define FM_MAX_ADDR                16384

#define FM_MA_TABLE_ENTRY_SIZE     3

#if 0
#define FM_DO_TRACE
#endif


/**************************************************/
/** \ingroup typeScalar
 * An event handler call-back function, provided by
 * the application and called by the API when an
 * event is being reported by the driver. This function
 * is registered with the API in the call to ''fmInitialize''
 * or ''fmSetEventHandler''. This function returns
 * void and takes as arguments:
 *                                                                      \lb\lb
 *  - fm_int event - An event ID code (see
 *  ''Event Identifiers''),
 *                                                                      \lb\lb
 *  - fm_int sw - The switch reporting the event,
 *                                                                      \lb\lb
 *  - void *ptr - a pointer to an event-specific structure
 *                used to carry information about the event.
 *                See the description of ''Event Identifiers''
 *                for the type of structure associated
 *                with each type of event.
 *                                                                      \lb\lb
 * The call-back function is not responsible for
 * disposing of the event structure pointed to by ptr;
 * however, if the event structure is of type
 * ''fm_eventPktRecv'', the call-back function is
 * responsible for disposing of the ''fm_buffer''
 * chain pointed to by the ''fm_eventPktRecv'' pkt member.
 * Disposal may be accomplished by calling ''fmFreeBufferChain''
 * with pkt as the argument.
 **************************************************/
typedef void (*fm_eventHandler)(fm_int event, fm_int sw, void *ptr);

#define FM_PRE_INIT_FLAG_NO_RESET  1

fm_status fmSetPreInitializationFlags(fm_int sw, fm_uint32 flags);


/* main function which initializes the API and other components */
fm_status fmInitialize(fm_eventHandler fPtr);

/* terminates access to the API */
fm_status fmTerminate(void);

#if 0
/* TODO: Implement this function if needed */
/* Function to allow the platform layer to pre-create switches prior to
 * switch insertion so that the application can pre-configure them */
fm_status fmPreCreateSwitch(fm_int sw);
#endif


/* sets the on/off state of the given switch */
fm_status fmSetSwitchState(fm_int sw, fm_bool state);

/* gets the on/off state of the given switch */
fm_status fmGetSwitchState(fm_int sw, fm_bool *state);

/* gets the state of the given switch */
fm_status fmGetSwitchStateExt(fm_int sw, fm_switchState *state);

/* sets the event handler used for upper layer notification */
fm_status fmSetEventHandler(fm_eventHandler fPtr);


/* sets which events are delivered to the current process */
fm_status fmSetProcessEventMask(fm_uint32 mask);


/* retrieves information about the state of a switch */
fm_status fmGetSwitchInfo(fm_int sw, fm_switchInfo *info);


/* Retrieves the first switch number in the system. */
fm_status fmGetSwitchFirst(fm_int *firstSwitch);


/* Retrieves the next switch number in the system. */
fm_status fmGetSwitchNext(fm_int currentSwitch, fm_int *nextSwitch);


/* Returns the number of active, booted switches in the system.
 * The index of the last switch in the system is not guaranteed to be equal
 * to the number of switches minus one!
 */
fm_uint fmGetSwitchCount(void);


/* different boot related enables */
fm_status fmEnableEEPROMMode(fm_int sw, fm_bool enable);
fm_status fmEnableLEDInvert(fm_int sw, fm_bool enable);
fm_status fmEnableLED(fm_int sw, fm_bool enable);
fm_status fmEnableDFT(fm_int sw, fm_bool enable);
fm_status fmEnableFrameHandlerPLLBypass(fm_int sw, fm_bool enable);
fm_status fmUseShadowFuseboxEntries(fm_int sw, fm_bool enable);
fm_status fmSetFrameHandlerPLLDividers(fm_int sw, fm_int p, fm_int n, fm_int m);
fm_status fmSetAutoBoot(fm_int sw, fm_bool enable);
fm_float fmComputeFHClockFreq(fm_int sw);


/* for debugging purposes */
fm_status fmTestReadRegister(fm_int sw, fm_uint offset, fm_uint32 *pVal);
fm_status fmTestRead64BitRegister(fm_int sw, fm_uint offset, fm_uint64 *pVal);


#ifdef FM_DO_TRACE
extern void fmTrace(fm_uint32 event,
                    fm_uint32 arg1,
                    fm_uint32 arg2,
                    fm_uint32 arg3);

int fmTestTraceMacAddress(fm_macaddr macAddr);


#define FM_TRACE(e, a1, a2, a3)  fmTrace(e, a1, a2, a3)
#else
#define FM_TRACE
#endif


#endif /* __FM_FM_API_INIT_H */
