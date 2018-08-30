/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_addr_int.h
 * Creation Date:   2005
 * Description:     Contains structure definitions and constants related to
 *                  the address table
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_API_ADDR_INT_H
#define __FM_FM_API_ADDR_INT_H

#define FM_FORMAT_ADDR      "%012" FM_FORMAT_64 "X"


#define FM_BPDU_DEST_ADDRESS                 FM_LITERAL_64(0x0180C2000000)
#define FM_LACP_DEST_ADDRESS                 FM_LITERAL_64(0x0180C2000002)
#define FM_DOT1X_DEST_ADDRESS                FM_LITERAL_64(0x0180C2000003)

/* Macro to determine whether we're tracing the specified MAC address. */

#define FM_IS_TEST_TRACE_ADDRESS(macAddr)               \
    ((fmRootApi->testTraceMacAddress != 0) &&           \
     (fmRootApi->testTraceMacAddress == (macAddr)))

/* MA Table Entry State */
typedef enum
{
    FM_MAC_ENTRY_STATE_INVALID,
    FM_MAC_ENTRY_STATE_OLD,
    FM_MAC_ENTRY_STATE_YOUNG,
    FM_MAC_ENTRY_STATE_LOCKED,
    FM_MAC_ENTRY_STATE_MOVED,                 /* FM6000 only, read-only */
    FM_MAC_ENTRY_STATE_PROVISIONAL_YOUNG,     /* FM6000 only, read-only */
    FM_MAC_ENTRY_STATE_PROVISIONAL_OLD        /* FM6000 only, read-only */

} fm_macEntryState;


/* MA Table entry type */
typedef enum
{
    FM_MAC_ENTRY_TYPE_UNKNOWN,
    FM_MAC_ENTRY_TYPE_CACHE,            /* entry is in software cache */
    FM_MAC_ENTRY_TYPE_OFFLOAD,          /* entry is in TCAM address table */
    FM_MAC_ENTRY_TYPE_HW_MATABLE,       /* entry is in the hardware MA Table */

} fm_macEntryType;


/* MA Table Entry Dirty state */
enum
{
    FM_MAC_ENTRY_WAITING,
    FM_MAC_ENTRY_DIRTY,
    FM_MAC_ENTRY_POSTED,
    FM_MAC_ENTRY_RECORDED

};



/* structure to hold MA table entry */
typedef struct _fm_internalMacAddrEntry
{
    /* Logical port mask (FM2000 and FM4000 only) */
    fm_uint32  destMask;

    /* Logical port (valid only if destMask is FM_DESTMASK_UNUSED) */
    fm_int     port;

    /* entry state (INVALID, OLD, YOUNG, LOCKED, PROVISIONAL) */
    fm_int     state;

    /* On FM2000 and FM4000, this is a parity error. On FM6000, this is
     * an unrecoverable ECC error and is read-only. */
    fm_bool    memError;

    /* Type of address entry (FM_MAC_ENTRY_TYPE_CACHE, etc.).
     * At this present, this field is only reliable following a call to
     * fmGetAddressInternal. */
    fm_byte    entryType;

    /* trigger # to fire for match (FM2000 and FM4000 only) */
    fm_uint32  trigger;

    /* lower 12 bits of chosen VID */
    fm_uint16  vlanID;

    /* lower 12 bits of chosen VID2 (FM6000 only) */
    fm_uint16  vlanID2;

    /* MAC Address */
    fm_macaddr macAddress;

    /* Aging counter, used for soft aging */
    fm_uint64  agingCounter;
    
    /* Dirty bits (WAITING, DIRTY, POSTED, RECORDED - FM6000 only) */
    fm_int     dirty;
    
    /* Remote ID filled with TAG[11:8] | DATA (FM6000 only) */
    fm_uint16  remoteID;

    /* Flag to let the pipeline know that reaching this MAC will be
     * done using TRILL enabled ports. TAG[7] (FM6000 only) */
    fm_bool    remoteMac;

    /* TAG secure bit (FM6000 only) */
    fm_bool    secure;

} fm_internalMacAddrEntry;


/* This alias is provided for some legacy regression tests. It should not
 * be used anymore. All references should be to fm_internalMacAddrEntry. */
typedef fm_internalMacAddrEntry fm_internal_mac_addr_entry;

extern fm_status fmAllocateAddressTableDataStructures(fm_switch *switchPtr);
extern fm_status fmInitAddressTable(fm_switch *switchPtr);
extern fm_status fmFreeAddressTableDataStructures(fm_switch *switchPtr);


/* core helpers */
extern fm_status fmAddAddressInternal(fm_int              sw,
                                      fm_macAddressEntry *entry,
                                      fm_uint32           trigger,
                                      fm_bool             updateHw,
                                      fm_int              bank,
                                      fm_uint32*          numUpdates,
                                      fm_event**          outEvent);

extern fm_status fmAddAddressToTable(fm_int              sw,
                                     fm_macAddressEntry *entry,
                                     fm_uint32           trigger,
                                     fm_bool             updateHw,
                                     fm_int              bank);

extern fm_status fmDeleteAddressFromTable(fm_int              sw,
                                          fm_macAddressEntry *entry,
                                          fm_bool             overrideMspt,
                                          fm_bool             updateHw,
                                          fm_int              bank);

extern fm_int fmCompareInternalMacAddressEntries(const void *key1, 
                                                 const void *key2);


/* returns the internal entry used by a mac address */
extern fm_status fmGetAddressInternal(fm_int                    sw,
                                      fm_macaddr                address,
                                      fm_int                    vlanID,
                                      fm_internalMacAddrEntry **entry,
                                      fm_uint32 *               addrIndex);

extern fm_status fmGetAddressIndex(fm_int     sw,
                                   fm_macaddr macAddress,
                                   fm_int     vlanID,
                                   fm_int     vlanID2,
                                   fm_int *   index,
                                   fm_int *   bank);

/* Returns text for an MA Table entry state. */
extern fm_text fmEntryStateToText(fm_int entryState);

extern fm_status fmReadEntryAtIndex(fm_int                   sw,
                                    fm_uint32                index,
                                    fm_internalMacAddrEntry *entry);

extern fm_status fmWriteEntryAtIndex(fm_int                   sw,
                                     fm_uint32                index,
                                     fm_internalMacAddrEntry *entry);

extern fm_status fmValidateAddressPort(fm_int sw, fm_int logicalPort);

extern fm_status fmCommonAssignTableEntry(fm_int              sw,
                                          fm_macAddressEntry *entry,
                                          fm_int              targetBank,
                                          fm_uint32           trigger,
                                          fm_bool             updateHw,
                                          fm_uint32*          numUpdates,
                                          fm_event**          outEvent);
extern fm_status fmCommonGetAddressTable(fm_int              sw,
                                         fm_int *            nEntries,
                                         fm_macAddressEntry *entries,
                                         fm_int              maxEntries);
extern fm_status fmCommonFindAndInvalidateAddr(fm_int     sw, 
                                               fm_macaddr macAddress, 
                                               fm_uint16  vlanID,
                                               fm_uint16  vlanID2,
                                               fm_int     bank,
                                               fm_uint16 *indexes,
                                               fm_bool    updateHw);
extern fm_status fmCommonAllocAddrTableCache(fm_switch *switchPtr);
extern fm_status fmCommonFreeAddrTableCache(fm_switch *switchPtr);
extern fm_status fmCommonInitAddrTableCache(fm_switch *switchPtr);
extern fm_status fmCommonDeleteAddress(fm_int sw, fm_macAddressEntry *entry);
extern fm_status fmCommonDeleteAddressesCached(fm_int sw, fm_bool dynamicOnly);



#endif /* __FM_FM_API_ADDR_INT_H */
