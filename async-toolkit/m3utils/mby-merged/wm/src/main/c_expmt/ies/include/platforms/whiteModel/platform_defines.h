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


/*****************************************************************************
 * Customer-Configurable ALOS Constants
 *****************************************************************************/

#define FM_ALOS_INTERNAL_MAX_LOCKS         256
#define FM_ALOS_INTERNAL_MAX_DBG_RW_LOCKS  64
#define FM_ALOS_INTERNAL_MAX_SEMAPHORES    1000
#define FM_ALOS_INTERNAL_DYN_LOAD_LIBS     4


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


/***************************************************/
/** The pthread stack size
 * \ingroup intConstSystem
 **************************************************/
#define FM_ALOS_PTHREAD_STACK_SIZE         524288


/***************************************************
 * The line size for a line in a log; may contain
 * multiple newlines.
 **************************************************/
#define FM_LOG_MAX_LINE_SIZE               256
#define FM_LOG_MAX_LINES                   1000
#define FM_LOG_MAX_FILTER_LEN              16384
#define FM_MAX_FILENAME_LENGTH             256


/***************************************************/
/** The number of threads we expect to use the
 *  rw locks, not the maximum number of
 * supported threads.
 * \ingroup intConstSystem
 **************************************************/
#define FM_MAX_THREADS                     64


/***************************************************
 * The following log control macros are used to
 * compile in or out various logging macros that
 * are useful for debugging. Compiling them out
 * may be desireable to increase overall performance
 * of the API.
 **************************************************/

/** \ingroup intConstSystem
 * @{ */

/** Controls whether all logging functions are enabled or compiled out (except 
 *  those functions that log entry to or exit from a function. */
#define FM_ALOS_LOGGING_SUBSYSTEM

/** Controls whether logging functions that log entry to or exit from a 
 *  function are enabled or compiled out. #undef to compile out. */
#define FM_ALOS_FUNCTION_LOGGING

/** Controls whether logging functions that log entry to or exit from ALOS 
 *  lock and reader-writer lock functions are enabled or compiled out. 
 *  #undef to compile out. Normally left disabled to improve API performance.*/
#undef FM_ALOS_LOCK_FUNCTION_LOGGING

/** Controls whether ''FM_LOG_ENTRY_VERBOSE'' and
 *  ''FM_LOG_EXIT_VERBOSE'' are enabled or compiled out. Use
 *  FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out these logging macros will result in better
 *  packet throughput to and from the CPU at the cost of losing
 *  debug logging capability. */
#define FM_ALOS_INCLUDE_LOG_VERBOSE            FM_ENABLED

/** Controls whether ''FM_LOG_DEBUG'', ''FM_LOG_DEBUG_VERBOSE'', 
 *  ''FM_LOG_DEBUG2'' and ''FM_LOG_DEBUG3'' are enabled or compiled out.
 *  Use FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out these logging macros will result in better packet throughput
 *  to and from the CPU at the cost of losing debug logging capability. */
#define FM_ALOS_INCLUDE_LOG_DEBUG            FM_ENABLED

/** Controls whether ''FM_LOG_WARNING'' is enabled or compiled out.
 *  Use FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out this logging macro will result in better packet throughput
 *  to and from the CPU at the cost of losing warning message logging 
 *  capability. */
#define FM_ALOS_INCLUDE_LOG_WARNING          FM_ENABLED

/** Controls whether ''FM_LOG_ERROR'' is enabled or compiled out.
 *  Use FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out this logging macro will result in better packet throughput
 *  to and from the CPU at the cost of losing error message logging 
 *  capability. */
#define FM_ALOS_INCLUDE_LOG_ERROR            FM_ENABLED

/** Controls whether ''FM_LOG_FATAL'' is enabled or compiled out.
 *  Use FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out this logging macro will result in better packet throughput
 *  to and from the CPU at the cost of losing fatal message logging 
 *  capability. */
#define FM_ALOS_INCLUDE_LOG_FATAL            FM_ENABLED

/** Controls whether ''FM_LOG_INFO'' is enabled or compiled out.
 *  Use FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out this logging macro will result in better packet throughput
 *  to and from the CPU at the cost of losing info message logging 
 *  capability. */
#define FM_ALOS_INCLUDE_LOG_INFO             FM_ENABLED

/** Controls whether ''FM_LOG_ASSERT'' and ''FM_LOG_ABORT_ON_ASSERT''
 *  are enabled or compiled out.
 *  Use FM_ENABLED to include or FM_DISABLED to compile out.
 *  Compiling out this logging macro will result in better packet throughput
 *  to and from the CPU at the cost of losing info message logging 
 *  capability. */
#define FM_ALOS_INCLUDE_LOG_ASSERT           FM_ENABLED

/** @} (end of Doxygen group) */

/**************************************************
 * Don't let FM_ALOS_FUNCTION_LOGGING be enabled
 * without FM_ALOS_LOGGING_SUBSYSTEM
 **************************************************/
 
#ifndef FM_ALOS_LOGGING_SUBSYSTEM
#undef FM_ALOS_FUNCTION_LOGGING
#endif


/***************************************************/
/** \ingroup intConstSystem
 *
 *  This constant is used to enable the lock inversion 
 *  defense mechanism within the API. The lock 
 *  inversion defense generates error messages if 
 *  multi-threaded access to (and within) the 
 *  API could possibly lead to a lock
 *  inversion. Locks are assigned a precedence value
 *  and an error message is generated if two locks
 *  are taken out of order. Generation of an error
 *  message does not necessarily constitute an
 *  operational failure; it merely serves notice
 *  that operational failure is possible and the
 *  inversion should be corrected.
 *                                              \lb\lb
 *  Intel performs all API regression testing with
 *  the lock inversion defense enabled and corrects
 *  any detected inversions before releasing the
 *  software. Users may wish to leave the defense
 *  enabled on the chance that a use case resulting
 *  in an inversion was not detected during Intel's
 *  testing.
 *                                              \lb\lb
 *  If this constant is not defined by the platform
 *  layer, the lock inversion defense will default
 *  to enabled. 
 **************************************************/
#define FM_LOCK_INVERSION_DEFENSE       FM_ENABLED


/*****************************************************************************
 * Customer-Configurable API Constants
 *****************************************************************************/
/** The maximum number of physical switches in the system.
 *  \ingroup intConstSystem */
#define FM_MAX_NUM_FOCALPOINTS             1


/** The maximum number of logical ports supported in the system.
 *  \ingroup intConstSystem */
#define FM_MAX_LOGICAL_PORT                         0xFFFF


/** The number of event buffers to allocate.
 *  \ingroup intConstSystem */
#define FM_MAX_EVENTS                      49152

/** The event queue size for API threads that require a large event queue.
 *  Memory will not be allocated until an event is actually sent to the
 *  thread. This value dictates the maximum number of events that can be in
 *  the queue at once.
 *  \ingroup intConstSystem */
#define FM_EVENT_QUEUE_SIZE_LARGE          8192

/** The event queue size for API threads that require a small event queue.
 *  Memory will not be allocated until an event is actually sent to the
 *  thread. This value dictates the maximum number of events that can be in
 *  the queue at once.
 *  \ingroup intConstSystem */
#define FM_EVENT_QUEUE_SIZE_SMALL          1024

/** The event queue size for API threads that do not use an event queue.
 *  Memory will not be allocated until an event is actually sent to the
 *  thread. This value dictates the maximum number of events that can be in
 *  the queue at once.
 *  \ingroup intConstSystem */
#define FM_EVENT_QUEUE_SIZE_NONE           1


/** The size of the MAC Table Purge Event Queue.
 *  \ingroup intConstSystem
 *  Used in the FM4000-series switches.
 *  Default value provides room for simultaneous events:
 *      4K vlan events, 24 port events, plus 1 global event.
 *  This is likely far more than will ever be needed in real life. */
#define FM_API_MA_PURGE_EVENT_QUEUE_SIZE   (4096 + 24 + 1)


/** The delay in nano-seconds between link-state debounce checks
 *  \ingroup intConstSystem */
#define FM_API_LINK_STATE_DEBOUNCE_DELAY   250000000            /* nanosecs */


/** The delay in seconds between MAC Table Maintenance Polls
 *  \ingroup intConstSystem */
#define FM_API_MAC_TABLE_MAINT_DELAY       0

/** The minimum delay in seconds we enforce between MA TABLE
 *  Maintenance tasks. Note that we make sure that the maximum delay
 *  between the MA Table Maintenance tasks is ''FM_API_MAC_TABLE_MAINT_DELAY''.
 * \ingroup intConstSystem */
#define FM_API_MAC_TABLE_MAINT_THROTTLE    0

/** The maximum number of MA Table updates (learn/age) reported together in
 *  a single ''FM_EVENT_TABLE_UPDATE'' event.
 *  \ingroup intConstSystem */
#define FM_TABLE_UPDATE_BURST_SIZE         64


/** The maximum number of link aggregation groups that may be created.
 *  Note that this number may be greater than the number of LAGs supported
 *  by one switch device.
 *  \ingroup intConstSystem */
#define FM_MAX_NUM_LAGS                    32


/** The maximum number of ports that can belong to a single link aggregation
 *  group. Note that this number may be greater than the number of ports
 *  per LAG supported by one switch device.
 *  \ingroup intConstSystem */
#define FM_MAX_NUM_LAG_MEMBERS             32


/** The maximum number of multicast groups. Note that this number must be a
 *  multiple of 256.
 *  \ingroup intConstSystem */
#define FM_MAX_NUM_MULTICAST_GROUP         4096

/** The maximum number of replication groups. Note that this
 *  must be equal to FM6000_MCAST_DEST_TABLE_ENTRIES.
 *  \ingroup intConstSystem */
#define FM_MAX_NUM_REPLICATION_GROUP       4096


/** The default VLAN number that is reserved for internal use by the API
 *  for FM2000 devices. Whatever VLAN is chosen as the reserved VLAN must
 *  not be used for any other purpose. The recommended value is 4095.
 *                                                                      \lb\lb
 *  This value can be overridden at run-time by setting the ''FM_RESERVED_VLAN''
 *  switch attribute.
 *  \ingroup intConstSystem */
#define FM_FM2000_DEFAULT_RESERVED_VLAN    4095


/** The default VLAN number that is reserved for internal use by the API
 *  for FM4000 devices. One VLAN must be held in reserve for use in 
 *  the per-VLAN and multiple spanning tree modes (see the 
 *  ''FM_SPANNING_TREE_MODE'' switch attribute). If either of these spanning
 *  tree modes is to be used, the recommended value for the reserved VLAN 
 *  is 4095. If only shared spanning tree mode will be used, the reserved 
 *  VLAN can be set to zero to indicate that no VLAN is held in reserve.
 *                                                                     \lb\lb
 *  The ''FM_RESERVED_VLAN'' switch attribute cannot be used to change the 
 *  reserved VLAN at run-time on FM4000 devices.
 *  \ingroup intConstSystem */
#define FM_FM4000_RESERVED_VLAN            4095


/** The number of Sweeper FIFO entries that will be processed before allowing
 *  other MA Table maintenance operations to occur.
 *  \ingroup intConstSystem */
#define FM_MAX_MA_TABLE_FIFO_ITERATIONS    512

/** The type of IPv4 multicast routes that will be supported by the
  * FM6000 hardware.  Note that less restrictive route types will be
  * promoted to this route type if possible.  If not possible, attempts
  * to add less restrictive route types will be rejected.
  * Possible values, the number of TCAM slices that will be cascaded
  * to support each respective route type, and the types that can be
  * supported by that setting are:
  *     0   IPv4 Multicast not supported.
  *     1   1 slice, supports FM_MCAST_ADDR_TYPE_DSTIP.
  *     2   2 slices, supports FM_MCAST_ADDR_TYPE_DSTIP and
  *             FM_MCAST_ADDR_TYPE_DSTIP_VLAN.
  *     3   2 slices, supports FM_MCAST_ADDR_TYPE_DSTIP and
  *             FM_MCAST_ADDR_TYPE_DSTIP_SRCIP.
  *     4   3 slices, supports FM_MCAST_ADDR_TYPE_DSTIP,
  *             FM_MCAST_ADDR_TYPE_DSTIP_VLAN,
  *             FM_MCAST_ADDR_TYPE_DSTIP_SRCIP, and
  *             FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN.
  * \ingroup intConstSystem */
#define FM_FM6000_ACTIVE_IPV4_MCAST        4

/** The type of IPv6 multicast routes that will be supported in the
  * FM6000 hardware.  Note that less restrictive route types will be
  * promoted to this route type if possible.  If not possible, attempts
  * to add less restrictive route types will be rejected.
  * Possible values, the number of TCAM slices that will be cascaded
  * to support each respective route type, and the types that can be
  * supported by that setting are:
  *     0   IPv6 Multicast not supported.
  *     1   4 slices, supports FM_MCAST_ADDR_TYPE_DSTIP and
  *             FM_MCAST_ADDR_TYPE_DSTIP_VLAN.
  *     2   8 slices, supports FM_MCAST_ADDR_TYPE_DSTIP,
  *             FM_MCAST_ADDR_TYPE_DSTIP_VLAN,
  *             FM_MCAST_ADDR_TYPE_DSTIP_SRCIP, and
  *             FM_MCAST_ADDR_TYPE_DSTIP_SRCIP_VLAN.
  * \ingroup intConstSystem */
#define FM_FM6000_ACTIVE_IPV6_MCAST        2


/*****************************************************************************
 * Customer-Configurable Per-Switch Sizing Constants
 *  Note that some switch models may support less than the numbers used here,
 *  and will report the appropriate error if an invalid value is used, but
 *  that no switches will be able to support more than the maximum values
 *  listed in this section.
 *****************************************************************************/

/** Define the maximum number of traffic classes supported. */
#define FM_MAX_TRAFFIC_CLASSES                      8


/** Define the maximum number of memory partitions supported per device. */
#define FM_MAX_MEMORY_PARTITIONS                    2


/** Define the maximum number of switch priorities supported per device. */
#define FM_MAX_SWITCH_PRIORITIES                    16


/** Define the maximum number of VLAN priorities supported per device. */
#define FM_MAX_VLAN_PRIORITIES                      16


/** Define the maximum number of DSCP priorities supported per device. */
#define FM_MAX_DSCP_PRIORITIES                      64


/*****************************************************************************
 * Customer-Configurable Switch Aggregate Constants
 *****************************************************************************/

/** The maximum number of switch aggregates.
 *  \ingroup intConstSystem */
#define FM_MAX_SWITCH_AGGREGATES           1

/*****************************************************************************
 * Customer-Configurable Switch Aggregate Topology Solvers
 *****************************************************************************/
 
/** Whether Fat Tree Switch Aggregate Topology is supported.
 *  \ingroup intConstSystem
 *  Use FM_ENABLED to enable Fat Tree support, FM_DISABLED to disallow it. */
#define FM_INCLUDE_FAT_TREE_TOPOLOGY       FM_ENABLED

/** Whether Ring Switch Aggregate Topology is supported.
 *  \ingroup intConstSystem
 *  Use FM_ENABLED to enable Ring support, FM_DISABLED to disallow it. */
#define FM_INCLUDE_RING_TOPOLOGY           FM_DISABLED


/** Maximum number of switches in a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_SWITCHES_PER_SWAG           FM_MAX_NUM_FOCALPOINTS



/*****************************************************************************
 * Customer-Configurable Switch Aggregate Constants
 * --  Used for GLORT Resource Allocations --
 *****************************************************************************/

/** The maximum number of local ports supported on a single physical switch
 *  within a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_LOCAL_PORTS_PER_SWAG_SWITCH 24

/** The maximum number of customer LAGs supported by a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_LAGS_PER_SWAG               FM_MAX_NUM_LAGS

/** The maximum number of customer LAGs supported by a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_EXTERNAL_LAGS_PER_SWAG      32

/** The maximum number of ports supported per internal or external LAG
 *  in a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_PORTS_PER_SWAG_LAG          16

/** The maximum number of glorts supported per LAG in a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_SWAG_GLORTS_PER_LAG             32

/** The maximum number of Load-Balancing Groups supported by a switch
 *  aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_LBGS_PER_SWAG               32

/** The maximum number of bins per LBG in a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_LBG_BINS_PER_SWAG_LBG       16

/** The maximum number of multicast groups supported in a switch aggregate.
 *  \ingroup intConstSystem */
#define FM_MAX_MCAST_GROUPS_PER_SWAG       4096

/** The maximum number of Next Hops per ECMP Group supported in a switch
 *  aggregate.
 * \ingroup intConstSystem  */
#define FM_MAX_ECMP_GROUP_SIZE_SWAG        16


/*****************************************************************************
 * Customer-Configurable FIBM support
 *****************************************************************************/
/* Fibm Task timeout in usec for periodically checking retries */
#define FM_FIBM_TASK_TIMEOUT_USEC 50000

/* Max number of fibm task timeout, before a message is resent
 * or trigger a operation timeout when exceeding max retries below */
#define FM_FIBM_MAX_TIMEOUT     3

/* Max number of fibm retries, before operation is timeout */
#define FM_FIBM_MAX_RETRIES     3


/*****************************************************************************
 * Maximum number of switches possible (physical and logical)
 *****************************************************************************/
#define FM_MAX_NUM_SWITCHES     \
    (FM_MAX_NUM_FOCALPOINTS + FM_MAX_SWITCH_AGGREGATES + 1)

#define FM_FIRST_FOCALPOINT                0

#define FM_LAST_FOCALPOINT      \
    (FM_FIRST_FOCALPOINT + FM_MAX_NUM_FOCALPOINTS - 1)


/*****************************************************************************
 * Customer-Configurable Debug Constants
 *****************************************************************************/
#define FM_DBG_MAX_MONITORED_REGS          16
#define FM_DBG_MAX_SNAPSHOT_REGS           700000

/*
 * Note that the calling-sequence to the fmDbgCompareChipSnapshots function
 * assumes that there will only be a maximum of 32 snapshots.  If this
 * number ever needs to be increased, that calling-sequence will have to
 * be changed. (It receives a bit-mask of the snapshots to be compared.) */
#define FM_DBG_MAX_SNAPSHOTS               32
#define FM_DBG_MAX_PACKET_SIZE             10240
#define FM_DBG_TRACE_BFR_SIZE              1024
#define FM_DBG_EXCLUSION_TABLE_SIZE        128
#define FM_DBG_MAX_TIMER_MEAS              16
#define FM_DBG_MAX_EYE_DIAGRAMS            4


/*****************************************************************************
 * Platform-Specific Constants
 *****************************************************************************/

/** Default EBI (External Bus Interface) clock speed in MHZ. The EBI is the bus
 *  between the CPU and the switch device. This parameter can be overridden
 *  by an API attribute (see ''API Attributes'').
 *  \ingroup intConstSystem */
#define FM_EBI_MHZ                         33.0

/*****************************************************************************
 * Per-Switch Routing Support Constants
 *****************************************************************************/
/* FM_MAX_ROUTES defaults to 24 FFU slices * 1024 entries per slice */
#define FM_MAX_ROUTES                      (24 * 1024)
#define FM_MAX_ARPS                        65536

/** The maximum number of router interfaces permitted.
 *  \ingroup intConstSystem */
#define FM_MAX_IP_INTERFACES               1024

/** The maximum number of virtual routers permitted.
 *  \ingroup intConstSystem */
#define FM_MAX_VIRTUAL_ROUTERS             1024

/** The number of 512 bit times that the link partner needs to pause
 *  \ingroup intConstSystem */
#define FM_PAUSE_PARTNER_TIME              0x00ff

/** The number of 512 bit times before the TX resends pause on frame
 *  \ingroup intConstSystem */
#define FM_PAUSE_RESEND_TIME               0x007f

/** The max number of times the LCI should be polled looking for a
 *  ready signal indicating that the next word can be read or written
 *  from or to the LCI when tranferring packets. The value of this constant
 *  is dependent on processor speed and any wait states incurred on the
 *  platform when accessing the LCI. Generally, any value greater than 1
 *  should be acceptable. Larger values may make the LCI accesses more
 *  efficient. Too large a value may cause the LCI accesses to block
 *  for too long. The value will be used as an unsigned int (fm_int).
 *  \ingroup intConstSystem */
#define FM_LCI_POLL_COUNT                  1024

/* interrupt number used */
#define FP_INTERUPT_NUM                    3

#define FM2224_PCS_CFG_1_RX_LANE_ORDER     (1 << 20)

/* the number of different mapped memory zones we have */
#define FM_MAX_MAPPED_MEMS                 6

/* the lockable areas */
enum
{
    FM_MEM_TYPE_CSR = 0,

    FM_PLAT_INFO,
    
    FM_MAX_PLAT_LOCKS
};

#define FM_PACKET_QUEUE_SIZE               256

/* special indices for the packet offset */
#define FM_PACKET_OFFSET_HEADER0           -2
#define FM_PACKET_OFFSET_HEADER1           -1
#define FM_PACKET_OFFSET_ETHERTYPE          3

/** define the platform EBI clock frequency for fm85xxep platform
 * \ingroup intConstSystem
 */
#define FM_PLAT_EBI_MHZ                    33

#define FM_EBI_MHZ_MIN                     20
#define FM_EBI_DIVIDER_MIN                 2

#define FM_PLATFORM_ATTR_NAME              0
#define FM_PLATFORM_ATTR_EQUALIZER         1

 /* FIXME. verify frame size */
/*  The maximum frame size used for internal ports in a SWAG.  Can be reduced
 *  if jumbo frame support is not required. Not actually used on altaWhiteModel
 *  but provided so SWAG support can be compiled without errors. */
#define FM_INTERNAL_PORT_MAX_FRAME_SIZE    65791

/** The default 12-bit Lag Hash Rotation value to use for internal LAGs
 *  in a switch aggregate.  Not actually used on altaWhiteModel
 *  but provided so SWAG support can be compiled without errors. */
#define FM_DEFAULT_SWAG_LAG_HASH_ROT_VAL   3

/** The default LAG Hash Rotation to use for internal LAGs in a switch
 *  aggregate.  As required by LAG attribute FM_LAG_HASH_ROTATION,
 *  a value of zero implies rotation A.  A value of 1 implies rotation B.
 *  Not actually used on altaWhiteModel but provided so SWAG support can be 
 *  compiled without errors. */
#define FM_DEFAULT_SWAG_LAG_HASH_ROTATION  1


/* Needed for TestPoint */
#define FM_PHY_EQUALIZER_AUTO  (1 << 15)

/* Alta memory initialization is performed using the CRM subsystem,
 * which is not currently emulated by the altaWhiteModel.
 * This #define allows to correctly initialize the model memory.
 */
#define FM_MODEL_USE_ALTERNATE_MEMORY_INIT   1


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

/* Enumerated data type for storing the type of switch needed in white model */
typedef enum _fm_platformSwitchType
{
    FM_PLATFORM_SWITCH_TYPE_UNKNOWN = 0,

    FM_PLATFORM_SWITCH_TYPE_FM4000,

    FM_PLATFORM_SWITCH_TYPE_FM6000,

    FM_PLATFORM_SWITCH_TYPE_FM10000,

    FM_PLATFORM_SWITCH_TYPE_HLP,

    FM_PLATFORM_SWITCH_TYPE_CPK

} fm_platformSwitchType;

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
#define FM_MODEL_GET_STATE(sw) (fmRootPlatform->fmPlatformState[(sw)].chipModel)



/***************************************************************************/
/*  Platform Macros
 ***************************************************************************/

/** \ingroup intMacroPlatform
 * @{ */

/***************************************************************************/
/** Called by the API, this macro captures the switch register lock, 
 *  defined by the platform, for ensuring atomic access to switch registers. 
 *  Normally this lock is used within the platform layer to make multi-word 
 *  register and read-modify-write accesses be atomic.
 *                                                                      \lb\lb
 *  The API will use these macros to operate on the same lock when atomic 
 *  access is required within the API for read-modify-write operations or 
 *  to keep multiple registers in sync.
 *                                                                      \lb\lb
 *  sw is the switch on which to operate.
 ***************************************************************************/
#define FM_PLAT_TAKE_REG_LOCK(sw) \
    fmCaptureLock( &fmRootPlatform->fmPlatformState[(sw)].accessLocks[FM_MEM_TYPE_CSR], \
                   FM_WAIT_FOREVER);

/***************************************************************************/
/** Called by the API, this macro releases the switch register lock
 *  previously captured by ''FM_PLAT_TAKE_REG_LOCK''.
 *                                                                      \lb\lb
 *  sw is the switch on which to operate.
 ***************************************************************************/
#define FM_PLAT_DROP_REG_LOCK(sw)  \
    fmReleaseLock( &fmRootPlatform->fmPlatformState[(sw)].accessLocks[FM_MEM_TYPE_CSR]);


/** @} (end of Doxygen group) */


/**************************************************
 * Functions.
 **************************************************/

fm_status fmPlatformReadCSR(fm_int sw, fm_uint32 addr, fm_uint32 *value);
fm_status fmPlatformReadCSRMult(fm_int     sw,
                                fm_uint32  addr,
                                fm_int     n,
                                fm_uint32 *value);
fm_status fmPlatformReadCSR64(int sw, fm_uint32 addr, fm_uint64 *value);
fm_status fmPlatformWriteCSR64(int sw, fm_uint32 addr, fm_uint64 value);
fm_status fmPlatformReadCSRMult64(fm_int     sw,
                                  fm_uint32  addr,
                                  fm_int     n,
                                  fm_uint64 *value);

void fmPlatformCloseInstrumentation(void);
void fmPlatformOpenInstrumentation(void);

#endif /* __FM_PLATFORM_DEFINES_H */
