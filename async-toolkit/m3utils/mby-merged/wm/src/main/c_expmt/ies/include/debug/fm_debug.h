/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_debug.h
 * Creation Date:   April 27, 2006
 * Description:     Provide debugging functions.
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

#ifndef __FM_FM_DEBUG_H
#define __FM_FM_DEBUG_H


/* Flag value for port argument to fmDbgDumpPortMapV2 indicating that
 * the mapping for all ports should be displayed. */
#define FM_ALL_PORTS    -1

/**************************************************/
/** \ingroup constPortMapDumpTypes
 *  These Port Map Dump Types are the values used 
 *  for the portType argument to ''fmDbgDumpPortMapV2'' 
 *  and indicate how the port argument to that function 
 *  should be interpreted.
 **************************************************/
enum _fm_portMapDumpTypes
{
    /** The port argument specifies a logical port number. */
    FM_PORT_DUMP_TYPE_LOGICAL = 0,

    /** The port argument specifies a physical port number. */
    FM_PORT_DUMP_TYPE_PHYSICAL,

    /** The port argument specifies an EPL number (which will
     *  correspond to more than one logical/phyiscal port). */
    FM_PORT_DUMP_TYPE_EPL,

    /** For internal use only. */
    FM_PORT_DUMP_TYPE_MAX

};



/*****************************************************************************/
/** \ingroup typeEnum
 *  Identifies individual diagnostic counters. Used as an argument to
 *  ''fmDbgDiagCountGet'' and ''fmDbgDiagCountClear''. Counters are kept for:
 *                                                                      \lb\lb
 *  * Tracking the flow of frames through the API.
 *                                                                      \lb\lb
 *  * MA Table update events.
 *                                                                      \lb\lb
 *  * Various error conditions.
 *****************************************************************************/
typedef enum
{
    /** Incremented when RX request message is handled. */
    FM_CTR_RX_REQ_MSG = 0,

    /** Incremented when API's RX packet array is full. THis condition implies
     * no more packets can be received. */
    FM_CTR_RX_PKT_ARRAY_FULL,

    /** Incremented when LCI RX bit timeout occurs. */
    FM_CTR_RX_LCI_TIMEOUT,

    /** Incremented when a RX packet is allocated from the RX packet array. */
    FM_CTR_RX_PKT_ALLOCS,

    /** Incremented when a RX packet is deallocated from the RX packet array.
     */
    FM_CTR_RX_PKT_FREES,

    /** Incremented when unable to allocate an event to propogate packet RX. */
    FM_CTR_RX_OUT_OF_EVENTS,

    /** Incremented when rx frame too large. */
    FM_CTR_RX_FRAME_TOO_LARGE,

    /** Incremented when whole frame rx'd into pkt. */
    FM_CTR_RX_PKT_COMPLETE,

    /** Incremented when an FIBM message is received. */
    FM_CTR_RX_PKT_FIBM,

    /** Incremented when the user part of dirver is dispatched by the kernel. */
    FM_CTR_USER_DISPATCH,

    /** Incremented when the user part ioctl wakes up in error. */
    FM_CTR_USER_IOCTL_ERROR,

    /** Incremented when the user part ioctl wakes up okay. */
    FM_CTR_USER_IOCTL_RETURN,

    /** Incremented when the kernel driver drops a low-priority packet due to congestion */
    FM_CTR_RX_KERNEL_PKT_DROPS,

    /** Incremented when the user part receives a pkt. */
    FM_CTR_RX_USER_PKT_SEEN,

    /** Incremented when the user part receives a pkt, but drop due to LACP. */
    FM_CTR_RX_PKT_DROPS_LACP,

    /** Incremented when the user part receives a pkt, but drop due to STP. */
    FM_CTR_RX_PKT_DROPS_STP,

    /** Incremented when the user part receives a pkt, but drop due to security violations . */
    FM_CTR_RX_PKT_DROPS_SECURITY,

    /** Incremented when the user part receives a pkt, but unable to match to a logical port. */
    FM_CTR_RX_PKT_DROPS_NO_PORT,

    /** Incremented when the user part receives a pkt, but drop due to lack of event to send up. */
    FM_CTR_RX_PKT_DROPS_NO_EVENT,

    /** Incremented when the user part receives a pkt, but drop due to zero length. */
    FM_CTR_RX_PKT_DROPS_NO_DATA,

    /** Incremented when the user part receives a BPDU pkt, but the ingress port is discarding */
    FM_CTR_RX_PKT_DROPS_BPDU_DISC,

    /** Incremented when the API receives a pkt, but does not forward it. */
    FM_CTR_RX_API_PKT_DROPS,

    /** Incremented when the API receives a pkt, and forwards it. */
    FM_CTR_RX_API_PKT_FWD,

    /** Incremented when the API receives a pkt but the status
     *  words says this packet in in error. */
    FM_CTR_RX_PKT_DROPS_FOR_ERROR,

    /** Incremented when the application receives a rx pkt. */
    FM_CTR_RX_APPL_PKT_SEEN,

    /** Incremented when the application forwards a rx frame. */
    FM_CTR_RX_APPL_FRM_FWD,

    /** Incremented when the application drops a rx frame. */
    FM_CTR_RX_APPL_FRM_DROPS,

    /** Incremented when the application forwards a tx pkt. */
    FM_CTR_TX_APPL_PKT_FWD,

    /** Incremented when tx frame too large. */
    FM_CTR_TX_FRAME_TOO_LARGE,

    /** Incremented when tx pkt array is full (cannot send another). */
    FM_CTR_TX_PKT_ARRAY_FULL,

    /** Incremented when LCI tx interrupt seen. */
    FM_CTR_TX_LCI_INT,

    /** Incremented when tx request msg seen. */
    FM_CTR_TX_REQ_MSG,

    /** Incremented when LCI tx bit timeout occurs. */
    FM_CTR_TX_LCI_TIMEOUT,

    /** Incremented when a whole packet has been sent to the switch for TX. */
    FM_CTR_TX_PKT_COMPLETE,

    /** Incremented when transferring a packet to the switch for TX fails. */
    FM_CTR_TX_PKT_DROP,

    /** Incremented when EPL interrupt seen */
    FM_CTR_EPL_INT,

    /**************************************************
     * MA Table Learn Events
     **************************************************/

    /** Incremented when a table scan causes an address to be learned. */
    FM_CTR_MAC_SCAN_LEARNED,

    /** Incremented when a LEARNED event causes an address to be learned. */
    FM_CTR_MAC_LEARN_LEARNED,

    /** Incremented when an ACL change generates a LEARNED event. */
    FM_CTR_MAC_ACL_LEARNED,

    /** Incremented when an address offload operation generates a LEARNED
     *  event. */
    FM_CTR_MAC_TCAM_LEARNED,

    /** Incremented when a hard learning event occurs. */
    FM_CTR_MAC_HARD_LEARNED,

    /** Incremented when a soft learning event occurs.  */
    FM_CTR_MAC_SOFT_LEARNED,

    /** Incremented when an API call generates an LEARNED event. */
    FM_CTR_MAC_API_LEARNED,

    /** Incremented when a learn event is reported. */
    FM_CTR_MAC_REPORT_LEARN,

    /** Incremented when a learn event is being sent to the ALPS. */
    FM_CTR_MAC_ALPS_LEARN,

    /** Incremented when a stale LEARNED event is detected. */
    FM_CTR_MAC_LEARN_STALE,

    /** Incremented when a LEARNED event is ignored. */
    FM_CTR_MAC_LEARN_IGNORED,

    /** Incremented when a LEARNED event is suppressed. */
    FM_CTR_MAC_LEARN_SUPPRESSED,

    /** Incremented when a learn event is discarded. */
    FM_CTR_MAC_LEARN_DISCARDED,

    /**************************************************
     * MA Table Age Events
     **************************************************/

    /** Incremented when a table scan causes an address to be aged. */
    FM_CTR_MAC_SCAN_AGED,

    /** Incremented when a LEARNED event causes an address to be aged. */
    FM_CTR_MAC_LEARN_AGED,

    /** Incremented when an AGED event causes an address to be aged. */
    FM_CTR_MAC_AGE_AGED,

    /** Incremented when an ACL change generates an AGED event. */
    FM_CTR_MAC_ACL_AGED,

    /** Incremented when an API call generates an AGED event. */
    FM_CTR_MAC_API_AGED,

    /** Incremented when a hard aging event occurs. */
    FM_CTR_MAC_HARD_AGED,

    /** Incremented when a soft aging event occurs. */
    FM_CTR_MAC_SOFT_AGED,

    /** Incremented when an address offload operation generates an AGED
     *  event. */
    FM_CTR_MAC_TCAM_AGED,

    /** Incremented when an age event is reported. */
    FM_CTR_MAC_REPORT_AGE,

    /** Incremented when an age event is being sent to the ALPS. */
    FM_CTR_MAC_ALPS_AGE,

    /** Incremented when a stale AGED event is detected. */
    FM_CTR_MAC_AGE_STALE,

    /** Incremented when an AGED event is ignored.  */
    FM_CTR_MAC_AGE_IGNORED,

    /**************************************************
     * MA Table Change Events
     **************************************************/

    /** Incremented when a table scan causes a young-to-old transition. */
    FM_CTR_MAC_SCAN_YOUNG_TO_OLD,

    /** Incremented when an address offload operation causes a young-to-old
     *  transition. */
    FM_CTR_MAC_TCAM_YOUNG_TO_OLD,

    /** Incremented on MA Table entry software flush. */
    FM_CTR_MAC_SCAN_FLUSHED,

    /** Incremented when a table scan causes an address to be replaced. */
    FM_CTR_MAC_SCAN_REPLACED,

    /** Incremented when a LEARNED event causes an address to be replaced. */
    FM_CTR_MAC_LEARN_REPLACED,

    /** Incremented when a table scan causes a destination port change. */
    FM_CTR_MAC_SCAN_PORT_CHANGED,

    /** Incremented when a LEARNED event causes a destination port change. */
    FM_CTR_MAC_LEARN_PORT_CHANGED,

    /**************************************************
     * MA Table Restore Events
     **************************************************/

    /* Incremented when a table scan causes static entries to be
     * restored to the MA Table. */
    FM_CTR_MAC_SCAN_RESTORED,

    /* Incremented when a LEARNED event causes static entries to be
     * restored to the MA Table. */
    FM_CTR_MAC_LEARN_RESTORED,

    /* Incremented when an AGED event causes static entries to be
     * restored to the MA Table. */
    FM_CTR_MAC_AGE_RESTORED,

    /**************************************************
     * MA Table Other Events
     **************************************************/

    /** Incremented when a static entry is restored. */
    FM_CTR_MAC_RESTORE_STATIC,

    /** Incremented when a dynamic entry is replaced with a static entry.
     *  The RESTORE_STATIC counter will also be incremented. */
    FM_CTR_MAC_REPLACE_STATIC,

    /** Incremented when an entry in the MA Table is invalidated due to a
     *  write error. */
    FM_CTR_MAC_ENTRY_DELETED,

    /** Incremented when DeleteAllDynamicAddresses removes an entry from
     *  the software cache. */
    FM_CTR_MAC_CACHE_DELETED,

    /** Incremented when an address migrates from TCAM to the MA_TABLE. */
    FM_CTR_MAC_TCAM_MIGRATED,

    /** Incremented when DeleteAllDynamicAddresses removes an entry from
     *  the TCAM address table. */
    FM_CTR_MAC_TCAM_DELETED,

    /** Incremented when a port is removed from a destination mask. */
    FM_CTR_MAC_PORT_REMOVED,

    /**************************************************
     * MA Table Scan Events
     **************************************************/

    /** Incremented when an MA Table scan starts. */
    FM_CTR_MAC_SCAN_STARTED,

    /** Incremented when an MA Table scan finishes. */
    FM_CTR_MAC_SCAN_FINISHED,

    /** Incremented when the MAC table scan is suspended. */
    FM_CTR_MAC_SCAN_SUSPENDED,

    /** Incremented when an MA Table row is scanned (row mode). */
    FM_CTR_MAC_ROW_SCANNED,

    /** Incremented when a row is flagged for rescanning. */
    FM_CTR_MAC_ROW_RESCANNED,

    /** Incremented when an MA Table entry is scanned (entry mode). */
    FM_CTR_MAC_ENTRY_SCANNED,

    /**************************************************
     * MA Table Errors
     **************************************************/

    /** Incremented when a learn event is discarded because SA is invalid. */
    FM_CTR_MAC_INVALID,

    /** Incremented when a learn event is discarded because it occurs on
     *  the reserved VLAN. */
    FM_CTR_MAC_RESERVED,

    /** Incremented when an invalid locked entry is detected. */
    FM_CTR_MAC_LOCKED,

    /** Incremented when a duplicate address is removed from the MA Table. */
    FM_CTR_MAC_TABLE_DUP,

    /** Incremented when a duplicate address is removed from the software
     *  cache. */
    FM_CTR_MAC_CACHE_DUP,

    /** Incremented when a security violation is reported. */
    FM_CTR_MAC_SECURITY,

    /** Incremented when an error occurs during an MA table read. */
    FM_CTR_MAC_READ_ERR,

    /** Incremented when an error occurs during an MA table write. */
    FM_CTR_MAC_WRITE_ERR,

    /** Incremented when an invalid port is detected. */
    FM_CTR_MAC_PORT_ERR,

    /** Incremented when an unspecified error occurs while scanning an MA
     *  Table entry. */
    FM_CTR_MAC_SCAN_ERR,

    /** Incremented when an unspecified error occurs while processing a
     *  LEARN event. */
    FM_CTR_MAC_LEARN_ERR,

    /** Incremented when an event buffer cannot be allocated. */
    FM_CTR_MAC_EVENT_ALLOC_ERR,

    /** Incremented when an event buffer cannot be sent. */
    FM_CTR_MAC_EVENT_SEND_ERR,

    /** Incremented when an update cannot be added to an event buffer
     *  because the buffer is full. */
    FM_CTR_MAC_EVENT_FULL_ERR,

    /** Incremented when an address cannot be learned because the address
     *  table bank is full. */
    FM_CTR_MAC_BANK_FULL_ERR,

    /** Incremented when we hit the rescan limit for a row. */
    FM_CTR_MAC_RESCAN_ERR,

    /**************************************************
     * MA Table Flush Requests
     **************************************************/

    /** Incremented when ''fmDeleteAllAddresses'' is called. */
    FM_CTR_ALL_DEL,

    /** Incremented when ''fmDeleteAllDynamicAddresses'' is called. */
    FM_CTR_DYN_DEL,

    /** Incremented when ''fmFlushAllDynamicAddresses'' is called or 
     *  ''fmFlushAddresses'' is called with the ''FM_FLUSH_MODE_ALL_DYNAMIC'' 
     *  mode. */
    FM_CTR_DYN_FLUSH,

    /** Incremented when ''fmFlushPortAddresses'' is called or 
     *  ''fmFlushAddresses'' is called with the ''FM_FLUSH_MODE_PORT'' mode. */
    FM_CTR_PORT_FLUSH,

    /** Incremented when ''fmFlushVlanAddresses'' is called or 
     *  ''fmFlushAddresses'' is called with the ''FM_FLUSH_MODE_VLAN'' mode. */
    FM_CTR_VLAN_FLUSH,

    /** Incremented when ''fmFlushPortVlanAddresses'' is called or 
     *  ''fmFlushAddresses'' is called with the ''FM_FLUSH_MODE_PORT_VLAN'' 
     *  mode. */
    FM_CTR_VLAN_PORT_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_VID1_VID2'' mode. */
    FM_CTR_VID1_VID2_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_PORT_VID1_VID2'' mode. */
    FM_CTR_PORT_VID1_VID2_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_PORT_VID2'' mode. */
    FM_CTR_PORT_VID2_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_VID2'' mode. */
    FM_CTR_VID2_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_PORT_VID1_REMOTEID'' mode. */
    FM_CTR_PORT_VID1_REMOTEID_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_VID1_REMOTEID'' mode. */
    FM_CTR_VID1_REMOTEID_FLUSH,

    /** Incremented when ''fmFlushAddresses'' is called with
     *  the ''FM_FLUSH_MODE_REMOTEID'' mode. */
    FM_CTR_REMOTEID_FLUSH,

    /**************************************************
     * MA Table Purge Events
     **************************************************/

    /** Incremented when an MA Table hardware-based purge request is
     *  enqueued within the API. */
    FM_CTR_PURGE_REQ,

    /** Incremented when an MA Table hardware-based purge request is
     *  actually executed. */
    FM_CTR_PURGE_EX,

    /** Incremented when hardware notifies software that a purge has
     *  completed. */
    FM_CTR_PURGE_HW_COMPLETE,

    /** Incremented when execution of an MA Table hardware-based purge has
     *  completed. */
    FM_CTR_PURGE_COMPLETE,

    /** Incremented when a purge is merged with an existing request */
    FM_CTR_PURGE_MERGED,

    /** Incremented when a NOP purge is requested. */
    FM_CTR_NOP_PURGE_REQ,

    /** Incremented when a NOP purge request is executed. */
    FM_CTR_NOP_PURGE_EX,

    /** Incremented when a NOP purge has completed. */
    FM_CTR_NOP_PURGE_COMPLETE,

    /**************************************************
     * MA Table Purge Errors
     **************************************************/

    /** Incremented when a purge request fails because a purge could not be
     *  enqueued due to lack of memory. */
    FM_CTR_PURGE_ALLOC_ERR,

    /** Incremented when a purge request fails because an event buffer could
     *  not be added to the queue. */
    FM_CTR_PURGE_QUEUE_ERR,

    /** Incremented when a purge request fails because the purge could not
     *  be executed. */
    FM_CTR_PURGE_EXEC_ERR,

    /** Incremented when execution of an MA Table hardware-based purge has
     *  timed out. */
    FM_CTR_PURGE_TIMEOUT,

    /**************************************************
     * TCN FIFO Interrupts
     **************************************************/

    /** Incremented when a TCN interrupt is dispatched. */
    FM_CTR_TCN_INTERRUPT,

    /** Incremented when a TCN PendingEvents condition is detected. */
    FM_CTR_TCN_PENDING_EVENTS,

    /** Incremented when a TCN LearnedEvents overflow is detected. */
    FM_CTR_TCN_LEARNED_OVERFLOW,

    /** Incremented when a TCN AgedEvents overflow is detected. */
    FM_CTR_TCN_AGED_OVERFLOW,

    /** Incremented when a TCN ErrorEvents overflow is detected. */
    FM_CTR_TCN_ERROR_OVERFLOW,

    /**************************************************
     * TCN FIFO Events
     **************************************************/

    /** Incremented when a Learned event is removed from the TCN FIFO. */
    FM_CTR_TCN_LEARNED_EVENT,

    /** Incremented when an Aged event is removed from the TCN FIFO. */
    FM_CTR_TCN_AGED_EVENT,

    /** Incremented when a BinFull event is removed from the TCN FIFO. */
    FM_CTR_TCN_BIN_FULL_EVENT,

    /** Incremented when a ParityError event is removed from the TCN FIFO. */
    FM_CTR_TCN_PARITY_ERROR_EVENT,

    /** Incremented when a SecurityViolation_New event is removed from the
     *  TCN FIFO. */
    FM_CTR_TCN_SEC_VIOL_NEW_EVENT,

    /** Incremented when a SecurityViolation_Moved event is removed from
     *  the TCN FIFO. */
    FM_CTR_TCN_SEC_VIOL_MOVED_EVENT,

    /** Incremented when a PurgeComplete event is removed from the TCN
     *  FIFO. */
    FM_CTR_TCN_PURGE_COMPLETE_EVENT,

    /** (FM6000 only ) Incremented when an entry was posted to the FIFO then
     *  refreshed by hardware before software could see it. */
    FM_CTR_TCN_STALE,

    /** Incremented when an unknown event type is removed from the TCN
     *  FIFO. (Should never happen.) */
    FM_CTR_TCN_UNKNOWN_EVENT,

    /**************************************************
     * TCN FIFO Errors
     **************************************************/

    /** Incremented when a TCN PTR read error is detected. */
    FM_CTR_TCN_PTR_READ_ERR,

    /** Incremented when a TCN PTR write error is detected. */
    FM_CTR_TCN_PTR_WRITE_ERR,

    /** Incremented when a TCN FIFO read error is detected. */
    FM_CTR_TCN_FIFO_READ_ERR,

    /** Incremented when a TCN FIFO parity error is detected. */
    FM_CTR_TCN_FIFO_PARITY_ERR,

    /** Incremented when a TCN FIFO conversion error is detected. */
    FM_CTR_TCN_FIFO_CONV_ERR,

    /**************************************************
     * MAC Maintenance Requests
     **************************************************/

    /** Incremented when the MAC table maintenance handler polls the pending
     *  work list. */
    FM_CTR_MAC_WORK_POLL_COUNT,

    /** The total number of service requests the MAC table maintenance
     *  handler has received. */
    FM_CTR_MAC_WORK_TOTAL_TASKS,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to perform a dynamic address flush. */
    FM_CTR_MAC_WORK_FLUSH_DYN_ADDR,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to service a TCN FIFO overflow. */
    FM_CTR_MAC_WORK_UPD_OVFLW,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to perform a periodic table scan. */
    FM_CTR_MAC_WORK_PERIODIC_SCAN,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to perform a port address flush. */
    FM_CTR_MAC_WORK_PORT_ADDR_FLUSH,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to perform an ACL update. */
    FM_CTR_MAC_WORK_PORT_ACL_UPDATE,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to perform a VLAN address flush. */
    FM_CTR_MAC_WORK_VLAN_ADDR_FLUSH,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to perform a VLAN port address flush. */
    FM_CTR_MAC_WORK_VLAN_PORT_ADDR_FLUSH,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to service the TCN FIFO. */
    FM_CTR_MAC_WORK_SERVICE_FIFO,

    /** Incremented when the MAC table maintenance handler receives a request
     *  to service the purge request queue. */
    FM_CTR_MAC_WORK_HANDLE_PURGE,

    /** Incremented when the MAC table maintenance handler removes an event
     *  from the TCN FIFO. */
    FM_CTR_MAC_WORK_FIFO_EVENTS,

    /**************************************************
     * Link change status
     **************************************************/

    /** Incremented when an event is sent up for a link change. */
    FM_CTR_LINK_CHANGE_EVENT,

    /** Incremented when an event cannot be allocate to send link change. */
    FM_CTR_LINK_CHANGE_OUT_OF_EVENTS,

    /**************************************************
     * Timestamp events
     **************************************************/

    /** Incremented when an egress timestamp event is signaled. */
    FM_CTR_EGRESS_TIMESTAMP_EVENT,

    /** Incremented when an egress timestamp event is lost. */
    FM_CTR_EGRESS_TIMESTAMP_LOST,

    /**************************************************
     * FM4000 Parity Errors
     **************************************************/

    /** Incremented when a parity error is detected in the VLAN TAG TABLE.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_VLAN_TAG_TABLE,

    /** Incremented when a parity error is detected in the IP MULTICAST TABLE.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_IP_MULTICAST_TABLE,

    /** Incremented when a parity error is detected in the INGRESS VID TABLE.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_INGRESS_VID_TABLE,

    /** Incremented when a parity error is detected in the EGRESS VID TABLE.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_EGRESS_VID_TABLE,

    /** Incremented when a parity error is detected in the INGRESS FID TABLE.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_INGRESS_FID_TABLE,

    /** Incremented when a parity error is detected in the EGRESS FID TABLE.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_EGRESS_FID_TABLE,

    /** Incremented when a parity error is detected in the GLORT RAM.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_GLORT_RAM,

    /** Incremented when a parity error is detected in the GLORT CAM.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_GLORT_CAM,

    /** Incremented when a parity error is detected in the GLORT Destination
     *  table.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_GLORT_DEST_TABLE,

    /** Incremented when a parity error is detected in the FFU TCAM.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_FFU_TCAM,

    /** Incremented when a parity error is detected in the FFU SRAM.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_FFU_SRAM,

    /** Incremented when a parity error is detected in the FFU MAP VLAN.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_FFU_MAP_VLAN,

    /** Incremented when a parity error is detected in the ARP table.
     *  \chips  FM4000 */
    FM_CTR_PARITY_ERR_ARP_TABLE,

    /**************************************************
     * FM6000 Parity Errors
     **************************************************/

    /** Incremented when an ARRAY memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_ARRAY,

    /** Incremented when a CM memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_CM,

    /** Incremented when a CMM memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_CMM,

    /** Incremented when an EACL memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_EACL,

    /** Incremented when an FFU memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_FFU,

    /** Incremented when a GLORT memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_GLORT,

    /** Incremented when an INTERNAL memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_INTERNAL,

    /** Incremented when an L2AR memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_L2AR,

    /** Incremented when an L2F memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_L2F,

    /** Incremented when an L2L memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_L2L,

    /** Incremented when an L2L_MAC memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_L2L_MAC,

    /** Incremented when an L2L_SWEEPER memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_L2L_SWEEPER,

    /** Incremented when a MAPPER memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_MAPPER,

    /** Incremented when an MCAST_MID memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_MCAST_MID,

    /** Incremented when an MCAST_POST memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_MCAST_POST,

    /** Incremented when a MODIFY memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_MODIFY,

    /** Incremented when a NEXTHOP memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_NEXTHOP,

    /** Incremented when a PARSER memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_PARSER,

    /** Incremented when a POLICER memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_POLICER,

    /** Incremented when a STATS_BANK memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_STATS_BANK,

    /** Incremented when an UNDEFINED memory error is detected.
     *  \chips  FM6000 */
    FM_CTR_PARITY_AREA_UNDEFINED,

    /**************************************************
     * Parity Error severity
     **************************************************/

    /** Incremented when a TRANSIENT parity error is detected.
     *  \chips  FM4000, FM6000 */
    FM_CTR_PARITY_SEVERITY_TRANSIENT,

    /** Incremented when a CUMULATIVE parity error is detected.
     *  \chips  FM4000 */
    FM_CTR_PARITY_SEVERITY_CUMULATIVE,

    /** Incremented when a FATAL parity error is detected.
     *  \chips  FM4000, FM6000 */
    FM_CTR_PARITY_SEVERITY_FATAL,

    /**************************************************
     * Parity Error status
     **************************************************/

    /** Incremented when a parity error is fixed in software.
     *  \chips  FM6000 */
    FM_CTR_PARITY_STATUS_FIXED,

    /** Incremented when a parity error is corrected in hardware.
     *  \chips  FM6000 */
    FM_CTR_PARITY_STATUS_CORRECTED,

    /**************************************************
     * Parity Error events
     **************************************************/

    /** Incremented when a corrected SRAM error interrupt occurs.
     *  \chips  FM6000 */
    FM_CTR_SRAM_C_ERR_INTERRUPT,

    /** Incremented when an uncorrectable SRAM error interrupt occurs.
     *  \chips  FM6000 */
    FM_CTR_SRAM_U_ERR_INTERRUPT,

    /** Incremented when a parity error notification cannot be sent to the
     *  event handler.
     *  \chips  FM4000, FM6000 */
    FM_CTR_PARITY_EVENT_LOST,

    /* ----  Add new entries above this line.  ---- */

    /** Used internally as the length of the switch counter array. */
    FM_SWITCH_CTR_MAX

} fm_trackingCounterIndex;


/** \ingroup macroSynonym
 *  Legacy synonym for ''FM_CTR_PARITY_ERR_TRANSIENT''. */
#define FM_CTR_PARITY_ERR_TRANSIENT     FM_CTR_PARITY_SEVERITY_TRANSIENT

/** \ingroup macroSynonym
 *  Legacy synonym for ''FM_CTR_PARITY_ERR_CUMULATIVE''. */
#define FM_CTR_PARITY_ERR_CUMULATIVE    FM_CTR_PARITY_SEVERITY_CUMULATIVE

/** \ingroup macroSynonym
 *  Legacy synonym for ''FM_CTR_PARITY_ERR_FATAL''. */
#define FM_CTR_PARITY_ERR_FATAL         FM_CTR_PARITY_SEVERITY_FATAL



/*****************************************************************************/
/** \ingroup typeEnum
 * A global set of diagnostic counters are kept for:
 *  * Tracking events
 *  * Buffer allocations and frees
 * The counters are stored in an array.  These enumerated values serve as the
 * indexes into the counter array for each type of event being counted.
 *****************************************************************************/
typedef enum
{
    /** Number of frame chunks/buffers allocated. */
    FM_GLOBAL_CTR_BUFFER_ALLOCS = 0,

    /** Number of frame chunks/buffers freed. */
    FM_GLOBAL_CTR_BUFFER_FREES,

    /** Incremented when a buffer is requested but
     * there are no free buffers remaining.
     */
    FM_GLOBAL_CTR_OUT_OF_BUFFERS,

    /** Incremented when a buffer is used for packet TX. */
    FM_GLOBAL_CTR_TX_BUFFER_ALLOCS,

    /** Incremented when a buffer used for TX is freed. */
    FM_GLOBAL_CTR_TX_BUFFER_FREES,

    /** Incremented when a buffer is used for packet RX. */
    FM_GLOBAL_CTR_RX_BUFFER_ALLOCS,

    /** Incremented when a buffer used for RX is freed. */
    FM_GLOBAL_CTR_RX_BUFFER_FREES,

    /** Incremented when a buffer is requested for packet RX but the
     * remaining buffers are reserved for TX.
     */
    FM_GLOBAL_CTR_NO_BUFFERS_FOR_RX,

    /** Incremented when a buffer is requested for packet RX but
     * there are no free buffers remaining.
     */
    FM_GLOBAL_CTR_RX_OUT_OF_BUFFERS,

    /** Incremented when a request for an event could not be satisfied. */
    FM_GLOBAL_CTR_NO_EVENTS_AVAILABLE,

    /* ----  Add new entries above this line.  ---- */

    /** Used internally as the length of the global counter array. */
    FM_GLOBAL_CTR_MAX

} fm_globalDiagCounter;


/* Diagnostics */
typedef struct
{
    fm_uint64 counters[FM_SWITCH_CTR_MAX];

} fm_switchDiagnostics;


typedef struct
{
    fm_uint64 counters[FM_GLOBAL_CTR_MAX];

} fm_globalDiagnostics;


/**************************************************/
/** \ingroup typeStruct
 *  A single Ethernet port eye diagram sample, used
 *  as an argument in ''fmDbgGetEyeDiagramSampleFirst'',
 *  ''fmDbgGetEyeDiagramSampleNext'' and
 *  ''fmDbgGetEyeDiagramSampleList''.
 **************************************************/
typedef struct _fm_eyeDiagramSample
{
    /** Generic ID for this sample. */
    fm_int sampleId;

    /** Phase point. */
    fm_int   phase;

    /** Voltage offset. */
    fm_int   offset;

    /** Bit error rate. */
    fm_float ber;

    /** Absolute number of errors. */
    fm_uint64 errors;

} fm_eyeDiagramSample;


/**************************************************/
/** \ingroup typeScalar
 * A debug print call-back function, provided by
 * the application and called by the API when
 * generating console output. This function
 * returns void and takes as arguments:
 *                                                                      \lb\lb
 *  fm_text buf - A buffer pointer that points to the
 *                string to be output.
 *                                                                      \lb\lb
 *  void *cookie1 - Arbitrary data that the application
 *                  registers in a call to
 *                  ''fmDbgSetDbgPrintContext''.
 *                                                                      \lb\lb
 *  void *cookie2 - Arbitrary data that the application
 *                  registers in a call to
 *                  ''fmDbgSetDbgPrintContext''.
 **************************************************/
typedef void (*fm_dbgPrintCallBack)(fm_text buf, void *cookie1, void *cookie2);

/***************************************************
 * All public debug functions.
 **************************************************/

/* Initializes the debug sub-system. */
fm_status fmDbgInitialize(void);

/* Switch self-test */
fm_status fmDbgSwitchSelfTest(fm_int sw, fm_bool ctrlState);
fm_status fmDbgPolicerTest(fm_int  sw,
                           fm_bool ctrlState,
                           fm_int *portList,
                           fm_int  portCnt,
                           fm_bool mrlLimiter);

/* Trace buffer management functions */
fm_status fmDbgTraceDump(fm_int start, fm_int end, fm_int stop);
fm_status fmDbgTracePost(fm_int eventCode,
                         fm_uint32 data1,
                         fm_uint32 data2,
                         fm_uint32 data3);
fm_status fmDbgTraceClear(void);
fm_status fmDbgTraceMode(fm_int mode, fm_int tail);
fm_status fmDbgTraceTrigger(fm_int eventCode, fm_int addOrDelete);
fm_status fmDbgTraceExclude(fm_int eventCode, fm_int addOrDelete);
void fmDbgTraceHelp(void);
void fmDbgTraceStatus(void);

/* Logical port management helpers */
void fmDbgDumpPortMap(int sw);
fm_status fmDbgDumpPortMapV2(fm_int sw, fm_int port, fm_int portType);
fm_status fmDbgMapLogicalPortToPhysical(fm_int  sw,
                                               fm_int  logPort,
                                               fm_int *physPort);

fm_int fmDbgMapPhysicalPortToLogical(fm_int switchNum, fm_int physPort);

/* Table update stats */
void fmDbgTableUpdateStatsDump(void);
void fmDbgTableUpdateStatsClear(void);

/* Register monitoring facility */
fm_status fmDbgMonitorRegister(fm_int    sw,
                                      fm_uint32 regOffset,
                                      fm_bool   monitor);
void fmDbgRegisterUpdate(fm_int    sw,
                                fm_uint32 regOffset,
                                fm_uint32 regValue);

/* Memory and buffer management */
fm_status fmDbgBfrDump(fm_int sw);
fm_status fmDbgDumpDeviceMemoryStats(int sw);


/* Register dump functionality */
void fmDbgDumpRegister(fm_int sw, fm_int port, char *regname);
fm_status fmDbgDumpRegisterV2(fm_int  sw,
                              fm_int  indexA,
                              fm_int  indexB,
                              fm_text regname);
fm_status fmDbgDumpRegisterV3(fm_int  sw,
                              fm_int  indexA,
                              fm_int  indexB,
                              fm_int  indexC,
                              fm_text regname);
void fmDbgListRegisters(fm_int  sw,
                        fm_bool showGlobals,
                        fm_bool showPorts);
void fmDbgGetRegisterName(fm_int   sw,
                          fm_int   regId,
                          fm_uint  regAddress,
                          fm_text  regName,
                          fm_uint  regNameLength,
                          fm_bool *isPort,
                          fm_int * index0Ptr,
                          fm_int * index1Ptr,
                          fm_int * index2Ptr,
                          fm_bool  logicalPorts,
                          fm_bool  partialLongRegs);
void fmDbgReadRegister(fm_int  sw,
                       fm_int  firstIndex,
                       fm_int  secondIndex,
                       fm_text registerName,
                       void *  values);
void fmDbgWriteRegister(fm_int  sw,
                        fm_int  port,
                        fm_text regname,
                        fm_int  value);
fm_status fmDbgWriteRegisterV2(fm_int    sw,
                               fm_int    wordOffset,
                               fm_int    indexA,
                               fm_int    indexB,
                               fm_text   regName,
                               fm_uint32 value);
fm_status fmDbgWriteRegisterV3(fm_int    sw,
                               fm_int    wordOffset,
                               fm_int    indexA,
                               fm_int    indexB,
                               fm_int    indexC,
                               fm_text   regName,
                               fm_uint32 value);
fm_status fmDbgWriteRegisterField(fm_int    sw,
                                  fm_int    indexA,
                                  fm_int    indexB,
                                  fm_int    indexC,
                                  fm_text   regName,
                                  fm_text   fieldName,
                                  fm_uint64 value);
void fmDbgWriteRegisterBits(fm_int    sw,
                            fm_uint   reg,
                            fm_uint32 mask,
                            fm_uint32 value);
fm_status fmDbgVerifyRegisterCache(fm_int sw);

/* Packet helpers */
void fmPrettyPrintPacket(fm_buffer *pkt);
void fmDbgPacketSizeDistAdd(int ps);

/* Chip snapshot debugging functions */
void fmDbgTakeChipSnapshot(fm_int sw, fm_int snapshot);
void fmDbgDeleteChipSnapshot(fm_int snapshot);
void fmDbgPrintChipSnapshot(fm_int snapshot, fm_bool showZeroValues);
void fmDbgCompareChipSnapshots(fm_uint snapshotMask);

/* Timer management */
void fmDbgTimerDump(void);
void fmDbgTimerReset(int index);
void fmDbgTimerBeginSample(int index);
void fmDbgTimerEndSample(int index);

/***************************************************
 * Module specific debug functions
 **************************************************/

fm_status fmDbgDumpLinkDebounceInfo(fm_int sw);
fm_status fmDbgDumpPortMasks(int sw);
fm_status fmDbgDumpLag(int sw);
fm_status fmDbgDumpLinkUpMask(fm_int sw);
void fmDbgDumpThreads(void);
void fmDbgDumpPort(fm_int sw, fm_int port);
void fmDbgDumpMACTable(fm_int sw, fm_int numEntries);
void fmDbgDumpMACCache(fm_int sw, fm_int numEntries);
void fmDbgDumpMACTableEntry(fm_int sw, fm_uint16 vlan, char *addressStr);
void fmDbgTraceMACAddress(fm_macaddr macAddr);
fm_status fmDbgDumpVid(fm_int sw);
fm_status fmDbgDumpMirror(fm_int sw);
void fmDbgDumpMapper(fm_int sw);
void fmDbgDumpFFU(fm_int sw, fm_bool validSlicesOnly, fm_bool validRulesOnly);
void fmDbgDumpBstTable(fm_int sw);
void fmDbgDumpSBusRegister ( fm_int  sw, 
                             fm_int  sbusDevID, 
                             fm_int  devRegID,
                             fm_bool writeReg );
fm_status fmDbgReadSBusRegister ( fm_int     sw, 
                                  fm_int     sbusDevID, 
                                  fm_int     devRegID, 
                                  fm_bool    writeReg,
                                  fm_uint32 *value );
fm_status fmDbgWriteSBusRegister ( fm_int     sw, 
                                   fm_int     sbusDevID, 
                                   fm_int     devRegID, 
                                   fm_uint32  value );
void fmDbgDumpEthSerDesRegister ( fm_int  sw, 
                                  fm_int  port, 
                                  fm_int  devRegID,
                                  fm_bool writeReg );
fm_status fmDbgReadEthSerDesRegister  ( fm_int     sw, 
                                        fm_int     port, 
                                        fm_int     devRegID, 
                                        fm_bool    writeReg,
                                        fm_uint32 *value );
fm_status fmDbgWriteEthSerDesRegister ( fm_int     sw, 
                                        fm_int     port, 
                                        fm_int     devRegID, 
                                        fm_uint32  value );
fm_status fmDbgInterruptSpico( fm_int      sw,
                               fm_int      cmd,
                               fm_int      arg,
                               fm_int      timeout,
                               fm_uint32 * result );
void fmDbgDumpL2LSweepers(fm_int sw, fm_bool regs);
void fmDbgDumpSAFTable(fm_int sw);

void fmDbgInitSwitchRegisterTable(fm_int sw);

void fmDbgDumpVersion(void);
char *fmDbgGetVersion(void);

fm_status fmDbgDumpTriggers(fm_int sw);

fm_status fmDbgDumpTriggerUsage(fm_int sw);

fm_status fmDbgDumpMulticastTables(fm_int sw);

fm_status fmDbgDumpGlortTable(fm_int sw);

fm_status fmDbgDumpGlortDestTable(fm_int sw, fm_bool raw);

void fmDbgConvertMacAddressToString(fm_macaddr macAddr, fm_char *textOut);
fm_bool fmDbgConvertStringToMacAddress(const char *addrStr, 
                                       fm_macaddr *addrValue); 

void fmDbgDumpACLsAsC(fm_int sw, const char *fileName);


/* Diagnostic counters API.
 * Switch counters:
 */
fm_status fmDbgDiagCountDump(fm_int sw);
fm_status fmDbgDiagCountClear(fm_int sw, fm_trackingCounterIndex counter);
fm_status fmDbgDiagCountClearAll(fm_int sw);
fm_status fmDbgDiagCountGet(fm_int sw, fm_trackingCounterIndex counter, fm_uint64 *outValue);
fm_status fmDbgDiagCountSet(fm_int sw, fm_trackingCounterIndex counter, fm_uint64 value);
fm_status fmDbgDiagCountIncr(fm_int                  sw,
                                    fm_trackingCounterIndex counter,
                                    fm_uint64               amount);

fm_status fmDbgDumpDriverCounts(fm_int sw);
fm_status fmDbgDumpMATableCounts(fm_int sw);
fm_status fmDbgDumpParitySweeperCounts(fm_int sw);


/* Global diagnostic counters
 */
fm_status fmDbgGlobalDiagCountDump(void);
fm_status fmDbgGlobalDiagCountClear(fm_globalDiagCounter counter);
fm_status fmDbgGlobalDiagCountClearAll(void);
fm_status fmDbgGlobalDiagCountGet(fm_globalDiagCounter counter, fm_uint64 *outValue);
fm_status fmDbgGlobalDiagCountSet(fm_globalDiagCounter counter, fm_uint64 value);
fm_status fmDbgGlobalDiagCountIncr(fm_globalDiagCounter counter, fm_uint64 amount);


/* Event queue debug API
 */
void fmDbgEventQueueCreated(fm_eventQueue *inQueue);
void fmDbgEventQueueDestroyed(fm_eventQueue *inQueue);
void fmDbgEventQueueEventPopped(fm_eventQueue *inQueue, fm_event *event);
void fmDbgEventQueueDump(void);
void fmDbgSetDbgPrintContext(fm_dbgPrintCallBack cb, void *cookie1, void *cookie2);


/* Switch insertion/removal debug API
 */
void fmDbgGenerateSwitchInsertionEvent(fm_int model, fm_int slot);
void fmDbgGenerateSwitchRemovalEvent(fm_int slot);


/* some helper macros */
#define FM_THREAD_ERR_CHECK(e)                       \
    if ( (e) != FM_OK )                              \
    {                                                \
        FM_LOG_ERROR( FM_LOG_CAT_ALOS_THREAD,        \
                     "%s: %s: line %d: error: %s\n", \
                     thread->name,                   \
                     __FILE__,                       \
                     __LINE__,                       \
                     fmErrorMsg( (e) ) );            \
    }

fm_status fmDbgDumpPortMax(fm_int sw, fm_int port);
fm_status fmDbgDumpQOS(fm_int sw, fm_int port);
fm_status fmDbgDumpMemoryUsage(fm_int sw);
fm_status fmDbgDumpMemoryUsageV2(fm_int  sw, 
                                 fm_int  rxPort, 
                                 fm_int  txPort,
                                 fm_int  rxmp, 
                                 fm_int  txmp, 
                                 fm_int  bsg,
                                 fm_bool useSegments);
fm_status fmDbgDumpWatermarks(fm_int sw);
fm_status fmDbgDumpWatermarksV2(fm_int sw, fm_int rxPort, fm_int txPort,
                                fm_int rxmp, fm_int txmp, fm_int islPri);
fm_status fmDbgDumpSwpriMap(fm_int sw, fm_int attr);
fm_status fmDbgDumpPortIdxMap(fm_int sw, fm_int port, fm_int attr);
fm_status fmDbgDumpPolicers(fm_int sw);
void fmDbgDumpStatChanges(fm_int sw, fm_bool resetCopy);

/* this is defined outside the conditional below because it is called from the test 
   enviroment using an expert call. The function will be empty if FM_API_FUNCTION_TRACKING is 
   not define.
*/
void fmDbgDumpFuncExecTable();


#ifdef FM_API_FUNCTION_TRACKING
#define FM_DBG_TRACK_FUNC()  fmDbgFuncExec(__func__, __LINE__);
#define FM_DBG_NEED_TRACK_FUNC
#define FM_DBG_TRACK_FUNC_DUMP()  fmDbgDumpFuncExecTable();
/* function to track the number of times a function has executed */
void fmDbgFuncExec(const char *funcName, int lineNum);

#else
#define FM_DBG_TRACK_FUNC()
#define FM_DBG_TRACK_FUNC_DUMP()
#ifdef FM_DBG_NEED_TRACK_FUNC
#undef FM_DBG_NEED_TRACK_FUNC
#endif
#endif


/** \ingroup macroSynonym
 * @{ */

/** A legacy synonym for ''fmDbgDumpRegisterV2''. */
#define fmDbgDumpRegisterExt(sw, indexA, indexB, regname) \
    fmDbgDumpRegisterV2( (sw), (indexA), (indexB), (regname) )

/** A legacy synonym for ''fmDbgWriteRegisterV2''. */
#define fmDbgWriteRegisterExt(sw, wordOffset, indexA, indexB, regName, value) \
    fmDbgWriteRegisterV2( (sw), (wordOffset), (indexA), (indexB), (regName), (value) )

/** @} (end of Doxygen group) */


void fmDbgDrvEventClear(void);
void fmDbgDrvEventDump(void);
void fmDbgDrvEventCount(fm_eventID eventID, int alloc);

fm_status fmDbgPlotEyeDiagram           ( fm_int eyeDiagramId );
                                        
fm_status fmDbgTakeEyeDiagram           ( fm_int sw, 
                                          fm_int port, 
                                          fm_int mac, 
                                          fm_int lane, 
                                          fm_int eyeDiagramId );

fm_status fmDbgDeleteEyeDiagram         ( fm_int eyeDiagramId );

fm_status fmDbgGetEyeDiagramSampleFirst ( fm_int eyeDiagramId, 
                                          fm_eyeDiagramSample *sample );

fm_status fmDbgGetEyeDiagramSampleList  ( fm_int  eyeDiagramId,
                                          fm_int *count,
                                          fm_eyeDiagramSample **sample );

fm_status fmDbgGetEyeDiagramSampleNext  ( fm_int eyeDiagramId, 
                                          fm_eyeDiagramSample *sample );

fm_status fmDbgGetEyeDiagramSampleCount ( fm_int eyeDiagramId, 
                                          fm_int *count );

fm_status fmDbgSetPacketDefaultSourcePort(fm_int sw, fm_int sourcePort);

#endif /* __FM_FM_DEBUG_H */
