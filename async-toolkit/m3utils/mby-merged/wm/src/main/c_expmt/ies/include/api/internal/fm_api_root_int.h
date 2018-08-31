/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_api_root_int.h
 * Creation Date:  June 21, 2007
 * Description:    Structure containing API's global variables
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_ROOT_INT_H
#define __FM_FM_API_ROOT_INT_H


#ifndef FM_GLOBAL_DECL 
#define FM_GLOBAL_DECL 
#endif

typedef enum
{
    TIB_INITIAL = 0,
    TIB_BCAST_ON,
    TIB_BCAST_OFF,
    TIB_DISABLE,

    /* Add new states above this line */
    TIB_MAX_STATE

} fm_TIBState;

typedef struct _fm_rootApi
{
    /**************************************************
     * fm_api_init.c
     **************************************************/
     
    /* global storage for mac address that is to be traced for debugging */
    /* Due to the way this feature was implemented, it is not possible to trace
     *  MAC Address 00:00:00:00:00:00 */
    fm_macaddr            testTraceMacAddress;

    /* this thread receives all non MA table related events */
    fm_thread             eventThread;

    /* Interrupt-Processing Thread */
    fm_thread             interruptTask;

    /* semaphore for interrupt signaling */
    fm_semaphore          intrAvail;

    /* Interrupt-Processing Thread for remote switches */
    fm_thread             fibmSlaveInterruptTask;

    /* semaphore for interrupt signaling for remote switches */
    fm_semaphore          fibmSlaveIntrAvail;

    /* semaphore to trigger packet transmission on NIC */
    fm_semaphore          nicPacketTxSemaphore;

    /* flags to indicate remote switch */
    fm_bool               isSwitchFibmSlave[FM_MAX_NUM_SWITCHES];

    /* MAC Table Maintenance Thread */
    fm_thread             maintenanceTask;

    /* Fast Maintenance Thread */
    fm_thread             fastMaintenanceTask;

    /* Link-State Debounce Thread */
    fm_thread             debounceTask;

    /* Thread for handling packet reception */
    fm_thread             packetReceiveTask;

    /* Sweeper Thread */
    fm_thread             paritySweeperTask;

    /* semaphore to trigger packet reception */
    fm_semaphore          packetReceiveSemaphore;

    /**************************************************
     * fm_api_glob.c
     **************************************************/
    /* list of switch state pointers */
    fm_switch *           fmSwitchStateTable[FM_MAX_NUM_SWITCHES];

    /* list of switch pointer read/write locks */
    fm_rwLock *           fmSwitchLockTable[FM_MAX_NUM_SWITCHES];

    /* list of pointers for registers cache */
    void *                fmSwRegCache[FM_MAX_NUM_SWITCHES];

    /**************************************************
     * fm_api_event_mgmt.c
     **************************************************/
    /* the free event queue */
    fm_eventQueue         fmEventFreeQueue;

    /* the semaphore used for throttling low priority events */
    fm_semaphore          fmLowPriorityEventSem;

    /**************************************************
     * fm_api_event_mac_maint.c
     **************************************************/
    /* semaphore to wakeup table maintenance thread when there is work to do */
    fm_semaphore          macTableMaintSemaphore;

    /* Diagnostics */
    fm_table_update_stats tableUpdateStats;
    fm_int                macTableMaintMaxTasks[FM_MAX_NUM_SWITCHES];

    /**************************************************
     * fm2000_api_port.c
     **************************************************/
    fm_int                executeFlag;

    /**************************************************
     * fm2000_api_pkt_rx.c
     **************************************************/
    fm_timestamp          lastTime[FM_MAX_NUM_SWITCHES];
    fm_uint64             lastBcastCount[FM_MAX_NUM_SWITCHES];
    fm_TIBState           state[FM_MAX_NUM_SWITCHES];

    /**************************************************
     * fm2000_api_attr.c
     **************************************************/
    //fm_macAddressEntry    fm2000MacTable[FM2000_MAX_ADDR];

    /**************************************************
     * fm_api_event_handler.c
     **************************************************/
    /* list of fm_localDelivery, one for each process */
    fm_dlist              localDeliveryThreads;

    /* number of threads in the above list */
    fm_uint               localDeliveryCount;

    /* lock for the above list and count */
    fm_lock               localDeliveryLock;

    /* semaphore to start the global event handler thread */
    fm_semaphore          startGlobalEventHandler;

    /* Pointer to MAC hashing table that must be shared between processes */
    fm_int               *l2lHashTable;

} fm_rootApi;

extern FM_GLOBAL_DECL fm_rootApi *fmRootApi;

#endif /* __FM_FM_API_ROOT_INT_H */
