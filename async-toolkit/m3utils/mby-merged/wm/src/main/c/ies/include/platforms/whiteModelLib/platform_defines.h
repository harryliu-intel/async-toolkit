/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            platform_defines.h
 * Creation Date:   2005
 * Description:     Platform specific definitions
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2014 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_PLATFORM_DEFINES_H
#define __FM_PLATFORM_DEFINES_H


/* Only used in header files for compilation */
#define FM_ALOS_INTERNAL_MAX_LOCKS         1
#define FM_ALOS_INTERNAL_MAX_DBG_RW_LOCKS  1
#define FM_ALOS_INTERNAL_MAX_SEMAPHORES    1
#define FM_ALOS_INTERNAL_DYN_LOAD_LIBS     1
#define FM_ALOS_PTHREAD_STACK_SIZE         1
#define FM_LOG_MAX_LINE_SIZE               1
#define FM_LOG_MAX_LINES                   1
#define FM_LOG_MAX_FILTER_LEN              1
#define FM_MAX_FILENAME_LENGTH             1
#define FM_MAX_THREADS                     1
#define FM_MAX_NUM_LAGS             1
#define FM_MAX_LOGICAL_PORT         1
#define FM_MAX_NUM_LAG_MEMBERS      1
#define FM_DBG_MAX_TIMER_MEAS       1
#define FM_DBG_EXCLUSION_TABLE_SIZE 1
#define FM_DBG_TRACE_BFR_SIZE       1
#define FM_DBG_MAX_EYE_DIAGRAMS     1
#define FM_DBG_MAX_SNAPSHOTS        1
#define FM_DBG_MAX_MONITORED_REGS   1
#define FM_DBG_MAX_PACKET_SIZE      1
#define FM_DBG_MAX_SNAPSHOT_REGS    1






/* To enable logging */
#define FM_ALOS_LOGGING_SUBSYSTEM
#define FM_ALOS_INCLUDE_LOG_VERBOSE     FM_ENABLED
#define FM_ALOS_FUNCTION_LOGGING        FM_ENABLED

#if defined(__APPLE__)
#define FM_SHARED_MEMORY_ADDR              0x200000000
#else
/**************************************************
 * The virtual address to map the shared memory at.
 * 0x60000000 is a decent location, fairly far away
 * from anything else on PPC, IA-32, and AMD-64
 **************************************************/
#define FM_SHARED_MEMORY_ADDR              0x60000000
#endif

#define FM_SHARED_MEMORY_SIZE              0x1fffffff

/*****************************************************************************
 * Maximum number of switches possible (physical and logical)
 *****************************************************************************/
#define FM_MAX_NUM_FOCALPOINTS      1
#define FM_MAX_NUM_SWITCHES         (FM_MAX_NUM_FOCALPOINTS + 1)


/**************************************************
 * White Model Support
 **************************************************/

/**************************************************/
/** White Model Constants
 *  \ingroup constModel
 *  \page whiteModelConstants
 *
 *  When the ''White Model'' is instantiated by a
 *  platform layer implementation, the platform
 *  must define the following set of constants.
 **************************************************/
/** \ingroup constModel
 * @{ */

/** In conjunction with ''FM_MODEL_TICK_NANOSECS'', indicates the 
 *  periodicity with which calls will be made to ''fm6000ModelTick'' by the 
 *  platform layer implementation. That function will use these values to
 *  calculate actual elapsed wall clock time for MAC address table aging and
 *  other time-based processing. FM_MODEL_TICK_SECS is the number
 *  of whole seconds between calls to ''fm6000ModelTick'' (and may be zero
 *  for sub-second periodicities) and ''FM_MODEL_TICK_NANOSECS'' is 
 *  the additional fractional part of a second in nanoseconds. */
#define FM_MODEL_TICK_SECS                  0

/** See ''FM_MODEL_TICK_SECS''. */
#define FM_MODEL_TICK_NANOSECS              1000000


/** The maximum packet size for messages sent to the model. Note that 
 *  the model can only change the first 256 bytes of a packet and 
 *  potentially add 48 bytes. The rest is a simple copy. So there isn't much 
 *  value in using the model for packets greater than 256. Packet size 
 *  must be big enough to allow expansion of up to 48 bytes. Packet size
 *  must also be big enough to support jumbo frames. */
#define FM_MODEL_MAX_PACKET_SIZE            /* 65791 */ 32767

/** The maximum size in units of bytes reserved for sideband type-length-value
 *  (TLV) elements. */
#define FM_MODEL_MAX_TLV_SIZE               256

/** When the white model packet queue interface is used, this constant 
 *  indicates the TCP port number on which the packet queue interface will 
 *  listen for command and data packets. If specified as zero, a port number 
 *  will be automatically allocated. Whether specified or automatically 
 *  allocated, the actual port number used will be reported in the first 
 *  line of the text file models.packetServer. See ''White Model Packet 
 *  Queue Interface'' in the Software API User Guide for more information. */
#define FM_MODEL_PKTQ_SERVER_PORT           0

/** The white model packet queue interface implements a queue to hold packets 
 *  egressing the switch model. When packets are removed from the queue,
 *  they are either passed into the API (if they egressed the CPU port on
 *  the switch model) or are sent on a socket previously registered on the
 *  egress port by the application. When the white model packet queue interface 
 *  is used, this constant indicates the maximum number of packets that the
 *  queue will hold. */
#define FM_MODEL_PKTQ_MAX_PACKETS           100

/** @} (end of Doxygen group) */


/***************************************************************************/
/** \ingroup macroPlatform
 *  When the ''White Model'' is instantiated by a platform layer 
 *  implementation, the platform must define this macro, which provides 
 *  a pointer to the white model state structure.
 *                                                                      \lb\lb
 *  sw is the switch number for the model instance whose state structure
 *  is to be retrieved.
 *                                                                      \lb\lb
 *  This sample implementation is from the altaWhiteModel platform where
 *  the pointer to the model state is kept in the platform's own state
 *  structure, but there is no requirement for this to be the case. For
 *  example, this macro could be implemented to call a function which
 *  returns the pointer.
 ***************************************************************************/
#define FM_MODEL_GET_STATE(sw) (fmRootChipModel[(sw)])


/** @} (end of Doxygen group) */

#endif /* __FM_PLATFORM_DEFINES_H */
