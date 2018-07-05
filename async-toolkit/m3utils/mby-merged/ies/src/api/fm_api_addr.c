/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_addr.c
 * Creation Date:   2005
 * Description:     Structures and functions for dealing with MA table
 *                  configuration
 *
 * INTEL CONFIDENTIAL
 * Copyright 2005 - 2012 Intel Corporation. All Rights Reserved. 
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


#include <fm_sdk_int.h>


/*****************************************************************************
 * Macros, Constants & Types
 *****************************************************************************/


/*****************************************************************************
 * Global Variables
 *****************************************************************************/


/*****************************************************************************
 * Local Variables
 *****************************************************************************/

/*****************************************************************************
 * Local function prototypes.
 *****************************************************************************/


/*****************************************************************************
 * Local Functions
 *****************************************************************************/

/*****************************************************************************/
/** RemoveDuplicateEntries
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Scans the hardware MA table and the cache, and removes any
 *                  duplicate entries.
 *                                                                      \lb\lb
 *                  This function is called after an entry has been added 
 *                  to the MA table.
 *                  
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the entry structure that we have just added.
 *
 * \param[in]       targetIndex is the index of the entry in the MA table to
 *                  which the address was assigned.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status RemoveDuplicateEntries(fm_int              sw,
                                        fm_macAddressEntry* entry,
                                        fm_int              targetIndex)
{
    fm_switch *              switchPtr;
    fm_internalMacAddrEntry *cachePtr;
    fm_internalMacAddrEntry  hwEntry;
    fm_uint16                binNumber;
    fm_int                   entryIndex;
    fm_int                   bankId;
    fm_status                err = FM_OK;

    switchPtr = GET_SWITCH_PTR(sw);
    
    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    /* Extract the bin number to which the address was assigned. */
    binNumber = targetIndex % switchPtr->macTableBankSize;

    /* Process each entry in the bin. */
    for (bankId = 0 ; bankId < switchPtr->macTableBankCount ; ++bankId)
    {
        entryIndex = (bankId * switchPtr->macTableBankSize) + binNumber;
        cachePtr = &switchPtr->maTable[entryIndex];

        /* Read the entry from the hardware MA table. */
        err = fmReadEntryAtIndex(sw, entryIndex, &hwEntry);
        if (err != FM_OK)
        {
            goto ABORT;
        }

        if (entryIndex != targetIndex)
        {
            /* Check for duplicate hardware entry. */
            if (hwEntry.state != FM_MAC_ENTRY_STATE_INVALID &&
                hwEntry.macAddress == entry->macAddress &&
                hwEntry.vlanID == entry->vlanID)
            {
                FM_LOG_DEBUG(FM_LOG_CAT_ADDR,
                             "removing duplicate MA table entry, "
                             "index=%d, "
                             "mac=" FM_FORMAT_ADDR ", "
                             "vlan=%u\n",
                             entryIndex,
                             entry->macAddress,
                             entry->vlanID);

                /* Invalidate the hardware entry. */
                hwEntry.state = FM_MAC_ENTRY_STATE_INVALID;
                err = fmWriteEntryAtIndex(sw, entryIndex, &hwEntry);
                if (err != FM_OK)
                {
                    goto ABORT;
                }
            }

            /* Check for duplicate cache entry. */
            if (cachePtr->state != FM_MAC_ENTRY_STATE_INVALID &&
                cachePtr->macAddress == entry->macAddress &&
                cachePtr->vlanID == entry->vlanID)
            {
                FM_LOG_DEBUG(FM_LOG_CAT_ADDR,
                             "removing duplicate cache entry, "
                             "index=%d, "
                             "mac=" FM_FORMAT_ADDR ", "
                             "vlan=%u\n",
                             entryIndex,
                             entry->macAddress,
                             entry->vlanID);

                /* Invalidate the cache entry. */
                cachePtr->state = FM_MAC_ENTRY_STATE_INVALID;
            }

        }
        else    /* (entryIndex == targetIndex) */
        {
            /* Check for correct hardware entry. */
            if (hwEntry.state == FM_MAC_ENTRY_STATE_INVALID ||
                hwEntry.macAddress != entry->macAddress ||
                hwEntry.vlanID != entry->vlanID)
            {
                /* This isn't necessarily an error, but we want to know 
                 * about it anyway. */
                FM_LOG_WARNING(FM_LOG_CAT_ADDR,
                               "incorrect MA table entry at index %d\n",
                               entryIndex);
            }

            /* Check for correct cache entry. */
            if (cachePtr->state == FM_MAC_ENTRY_STATE_INVALID ||
                cachePtr->macAddress != entry->macAddress ||
                cachePtr->vlanID != entry->vlanID)
            {
                /* This should never happen. */
                FM_LOG_ERROR(FM_LOG_CAT_ADDR,
                             "incorrect cache entry at index %d\n",
                             entryIndex);
            }
        }

    }   /* end for (bankId = 0 ; bankId < switchPtr->macTableBankCount ; ...) */

ABORT:
    return err;

}   /* end RemoveDuplicateEntries */




/*****************************************************************************/
/** StoreTableEntry
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Stores an address in the MA table and cache.
 *
 * \note            This function assumes the L2_LOCK is already held on entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the entry structure that we have just added.
 *
 * \param[in]       trigger is the trigger to write to the entry, or -1 to use
 *                  the default.
 *
 * \param[in]       updateHw should be TRUE to update the hardware table 
 *                  registers.
 *
 * \param[in]       targetIndex is the index of the entry in the MA table to
 *                  which the address has been assigned.
 *
 * \param[in,out]   numUpdates points to the current number of events in 
 *                  outEvent and will be updated if another event is added to 
 *                  the event buffer.
 *
 * \param[in,out]   outEvent points to the event buffer that will be filled
 *                  with learning events. If the buffer is filled, it will
 *                  be sent to the event handler and a new buffer allocated.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
static fm_status StoreTableEntry(fm_int                 sw,
                                 fm_macAddressEntry*    entry,
                                 fm_uint32              trigger,
                                 fm_bool                updateHw,
                                 fm_int                 targetIndex,
                                 fm_uint32*             numUpdates,
                                 fm_event**             outEvent)
{
    fm_switch *              switchPtr;
    fm_internalMacAddrEntry *cachePtr;
    fm_internalMacAddrEntry  hwEntry;
    fm_internalMacAddrEntry  oldEntry;
    fm_internalMacAddrEntry  tmpEntry;
    fm_status                err = FM_OK;
    fm_int                   reason;
    fm_bool                  l2Locked = TRUE;


    switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    /* Pointer to cache entry we are going to store. */
    cachePtr = &switchPtr->maTable[targetIndex];

    if (entry->destMask == FM_DESTMASK_UNUSED)
    {
        hwEntry.destMask = FM_DESTMASK_UNUSED;
        hwEntry.port     = entry->port;
    }
    else
    {
        hwEntry.destMask = entry->destMask;
        hwEntry.port     = -1;
    }

    hwEntry.vlanID     = entry->vlanID;
    hwEntry.vlanID2    = entry->vlanID2;
    hwEntry.macAddress = entry->macAddress;

    if (entry->type == FM_ADDRESS_STATIC)
    {
        hwEntry.state = FM_MAC_ENTRY_STATE_LOCKED;
    }
    else
    {
        hwEntry.state = FM_MAC_ENTRY_STATE_YOUNG;
    }

    if (trigger != (fm_uint32) -1)
    {
        hwEntry.trigger = trigger;
    }
    else
    {
        hwEntry.trigger = switchPtr->macTableDefaultTrigID;
    }

    hwEntry.agingCounter = 0;
    hwEntry.memError = FALSE;
    hwEntry.entryType = FM_MAC_ENTRY_TYPE_CACHE;

    /* Store address in cache. */
    oldEntry  = *cachePtr;
    *cachePtr = hwEntry;

    if (updateHw)
    {
        /* Update the hardware. */
        err = fmWriteEntryAtIndex(sw, targetIndex, &hwEntry);

        tmpEntry = hwEntry;

        /* Are we overwriting an existing entry? */
        switch ( oldEntry.state ) 
        {
            case FM_MAC_ENTRY_STATE_INVALID:
                /* No a new entry is used. See whether the API should send an
                   LEARNED event to the application. */
                if ( (hwEntry.state == FM_MAC_ENTRY_STATE_LOCKED &&
                      switchPtr->generateEventOnStaticAddr) ||
                     (hwEntry.state != FM_MAC_ENTRY_STATE_LOCKED &&
                      switchPtr->generateEventOnDynamicAddr) )
                {
                    /* Relinquish lock before generating updates. */
                    FM_DROP_L2_LOCK(sw);
                    l2Locked = FALSE;

                    fmGenerateUpdateForEvent(sw,
                                             &fmRootApi->eventThread,
                                             FM_EVENT_ENTRY_LEARNED,
                                             FM_MAC_REASON_API_LEARNED,
                                             targetIndex,
                                             &hwEntry,
                                             numUpdates,
                                             outEvent);
    
                    fmDbgDiagCountIncr(sw, FM_CTR_MAC_API_LEARNED, 1);
                }
                break;

            case FM_MAC_ENTRY_STATE_YOUNG:
            case FM_MAC_ENTRY_STATE_OLD:
            case FM_MAC_ENTRY_STATE_LOCKED:
                /* Yes we are overwriting an existing entry */
                if (hwEntry.vlanID == oldEntry.vlanID &&
                    hwEntry.vlanID2 == oldEntry.vlanID2 &&
                    hwEntry.macAddress == oldEntry.macAddress)                
                {
                    /* The cache is already in sync meaning that this update
                    *  is useless. */
                    if (hwEntry.destMask == oldEntry.destMask &&
                        hwEntry.port     == oldEntry.port     &&
                        hwEntry.state    == oldEntry.state    &&
                        hwEntry.trigger  == oldEntry.trigger)
                    {
                        break;
                    }
                    reason = FM_MAC_REASON_API_LEARN_CHANGED;
                }
                else 
                {
                    reason = FM_MAC_REASON_API_LEARN_REPLACED;
                }

                /**************************************************
                 * If we are overwriting an existing dynamic table
                 * entry, we need to generate an AGE event for it.
                 * it would be nice to just let the maintenance task
                 * take care of this, but if we are updating a dynamic
                 * entry and keeping it dynamic, that task won't know
                 * to not generate a LEARN event for the updated entry,
                 * so we have to generate the AGE event here. 
                 *  
                 * An AGE event is also generated for a static entry 
                 * if the application requested it through the API 
                 * attribute. 
                 **************************************************/

                if ( oldEntry.state != FM_MAC_ENTRY_STATE_LOCKED ||
                     switchPtr->generateEventOnStaticAddr )
                {
                    /* Relinquish lock before generating updates. */
                    FM_DROP_L2_LOCK(sw);
                    l2Locked = FALSE;

                    fmGenerateUpdateForEvent(sw,
                                             &fmRootApi->eventThread,
                                             FM_EVENT_ENTRY_AGED,
                                             reason,
                                             targetIndex,
                                             &oldEntry,
                                             numUpdates,
                                             outEvent);
    
                    fmDbgDiagCountIncr(sw, FM_CTR_MAC_API_AGED, 1);
                }

                /* See whether the API should send an LEARNED event to appl. */
                if ( (tmpEntry.state == FM_MAC_ENTRY_STATE_LOCKED &&
                      switchPtr->generateEventOnStaticAddr) ||
                     (tmpEntry.state != FM_MAC_ENTRY_STATE_LOCKED &&
                      switchPtr->generateEventOnDynamicAddr) )
                {
                    /* Relinquish lock before generating updates. */
                    if ( l2Locked )
                    {
                        FM_DROP_L2_LOCK(sw);
                        l2Locked = FALSE;
                    }
                    fmGenerateUpdateForEvent(sw,
                                             &fmRootApi->eventThread,
                                             FM_EVENT_ENTRY_LEARNED,
                                             reason,
                                             targetIndex,
                                             &tmpEntry,
                                             numUpdates,
                                             outEvent);
    
                   fmDbgDiagCountIncr(sw, FM_CTR_MAC_API_LEARNED, 1);
                }
               break;
        }

    }   /* end if (updateHw) */

ABORT:
    if ( !l2Locked ) 
    {
        FM_TAKE_L2_LOCK(sw);
    }
       
    return err;

}   /* end StoreTableEntry */




/*****************************************************************************
 * Public Functions
 *****************************************************************************/

/*****************************************************************************/
/** fmEntryStateToText
 * \ingroup intAddr
 *
 * \desc            Returns a textual representation of an MA Table state.
 *
 * \param[in]       entryState is the MA Table state.
 *
 * \return          Pointer to a string representation of entryState.
 *
 *****************************************************************************/
fm_text fmEntryStateToText(fm_int entryState)
{

    switch (entryState)
    {
        case FM_MAC_ENTRY_STATE_INVALID:
            return "Invalid";

        case FM_MAC_ENTRY_STATE_YOUNG:
            return "Young";

        case FM_MAC_ENTRY_STATE_OLD:
            return "Old";

        case FM_MAC_ENTRY_STATE_MOVED:
            return "Moved";

        case FM_MAC_ENTRY_STATE_LOCKED:
            return "Locked";

        default:
            return "Unknown";

    }   /* end switch (entryState) */

}   /* end fmEntryStateToText */




/*****************************************************************************/
/** fmReadEntryAtIndex
 * \ingroup intAddr
 *
 * \desc            Reads a MAC address table entry from the hardware registers
 *                  and converts it into an internal MAC address data
 *                  structure.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       index is the MAC address table index of the entry that is
 *                  to be read.
 *
 * \param[out]      entry points to an internal MAC address data structure in
 *                  which this function will store the information retrieved
 *                  from the hardware registers.
 *
 * \return          FM_OK is successful
 *
 *****************************************************************************/
fm_status fmReadEntryAtIndex(fm_int                   sw,
                             fm_uint32                index,
                             fm_internalMacAddrEntry *entry)
{
    fm_switch *switchPtr;
    fm_status  status;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_ADDR,
                         "sw=%d index=%u entry=%p\n",
                         sw,
                         index,
                         (void *) entry);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(status, switchPtr->ReadEntryAtIndex, sw, index, entry);

    if (status == FM_OK)
    {
        if (FM_IS_TEST_TRACE_ADDRESS(entry->macAddress))
        {
            FM_LOG_PRINT("Read: sw=%d "
                         "index=%u "
                         "MAC=" FM_FORMAT_ADDR " "
                         "VLAN=%hu/%hu "
                         "state=%d (%s) "
                         "dMask=%08x "
                         "port=%d "
                         "trig=%u\n",
                         sw,
                         index,
                         entry->macAddress,
                         entry->vlanID,
                         entry->vlanID2,
                         entry->state,
                         fmEntryStateToText(entry->state),
                         entry->destMask,
                         entry->port,
                         entry->trigger);
        }

    }   /* end if (status == FM_OK) */

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ADDR, status);

}   /* end fmReadEntryAtIndex */




/*****************************************************************************/
/** fmWriteEntryAtIndex
 * \ingroup intAddr
 *
 * \desc            Utility function to convert an MA Table entry structure to
 *                  register values and write it to the hardware registers.
 *
 * \param[in]       sw is the switch number.
 *
 * \param[in]       index is the MA Table index at which the entry is to be
 *                  written.
 *
 * \param[in]       entry points to the MA Table entry structure to be
 *                  converted and written to the hardware.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmWriteEntryAtIndex(fm_int                   sw,
                              fm_uint32                index,
                              fm_internalMacAddrEntry *entry)
{
    fm_switch *switchPtr;
    fm_status  status;

    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_ADDR,
                         "sw=%d index=%u entry=%p\n",
                         sw,
                         index,
                         (void *) entry);

    switchPtr = GET_SWITCH_PTR(sw);

    if (FM_IS_TEST_TRACE_ADDRESS(entry->macAddress))
    {
        FM_LOG_PRINT("Write: sw=%d "
                     "index=%u "
                     "MAC=" FM_FORMAT_ADDR " "
                     "VLAN=%hu/%hu "
                     "state=%d (%s) "
                     "dMask=%08x "
                     "port=%d "
                     "trig=%u "
                     "secure=%d\n",
                     sw,
                     index,
                     entry->macAddress,
                     entry->vlanID,
                     entry->vlanID2,
                     entry->state,
                     fmEntryStateToText(entry->state),
                     entry->destMask,
                     entry->port,
                     entry->trigger,
                     entry->secure);
    }

    FM_API_CALL_FAMILY(status, switchPtr->WriteEntryAtIndex, sw, index, entry);

    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ADDR, status);

}   /* end fmWriteEntryAtIndex */




/*****************************************************************************/
/** fmAllocateAddressTableDataStructures
 * \ingroup intAddr
 *
 * \desc            Allocates the internal storage used for managing the MAC 
 *                  address table.
 *
 * \param[in]       switchPtr points to the switch's state structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmAllocateAddressTableDataStructures(fm_switch *switchPtr)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR,
                 "switchPtr=%p<sw=%d>\n",
                 (void *) switchPtr,
                 switchPtr->switchNumber);

    FM_API_CALL_FAMILY(status, switchPtr->AllocAddrTableData, switchPtr);
    
    if (status == FM_ERR_UNSUPPORTED)
    {
        status = FM_OK;
    }
    
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end fmAllocateAddressTableDataStructures */




/*****************************************************************************/
/** fmFreeAddressTableDataStructures
 * \ingroup intAddr
 *
 * \desc            Frees the internal storage used for managing the MAC 
 *                  address table.
 *
 * \note            This function assumes that the relevant pointer is 
 *                  already protected by some mechanism.
 *
 * \param[in]       switchPtr points to the switch's state structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmFreeAddressTableDataStructures(fm_switch *switchPtr)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR,
                 "switchPtr=%p<sw=%d>\n",
                 (void *) switchPtr,
                 switchPtr ? switchPtr->switchNumber : -1);

    if (switchPtr == NULL)
    {
        status = FM_ERR_INVALID_ARGUMENT;
        goto ABORT;
    }
    
    FM_API_CALL_FAMILY(status, switchPtr->FreeAddrTableData, switchPtr);
    
    if (status == FM_ERR_UNSUPPORTED)
    {
        status = FM_OK;
    }
    
ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end fmFreeAddressTableDataStructures */




/*****************************************************************************/
/** fmInitAddressTable
 * \ingroup intAddr
 *
 * \desc            Initialize the local copy of the MA Table.
 *
 * \param[in]       switchPtr points to the switch's state structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmInitAddressTable(fm_switch *switchPtr)
{
    fm_status status;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR,
                 "switchPtr=%p<sw=%d>\n",
                 (void *) switchPtr,
                 switchPtr->switchNumber);

    FM_API_CALL_FAMILY(status, switchPtr->InitAddressTable, switchPtr);
    
    if (status == FM_ERR_UNSUPPORTED)
    {
        status = FM_OK;
    }
    
    FM_LOG_EXIT(FM_LOG_CAT_SWITCH, status);

}   /* end fmInitAddressTable */




/*****************************************************************************/
/** fmAddAddress
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Adds an entry to the MA Table.  Note that the VLAN field
 *                  of the entry structure is overridden if the
 *                  current vlan mode is shared.  Also, the valid member
 *                  in the entry structure is ignored; the entry written is
 *                  always marked as valid.
 *                                                                      \lb\lb
 *                  Addition of MAC addresses to multicast group logical ports
 *                  is not permitted using this function.  The multicast
 *                  group functions must be used to add a multicast address
 *                  to a multicast group.
 *
 * \note            If the address to be added already exists in the MA Table,
 *                  it will be overwritten by the new entry, unless the new
 *                  entry is dynamic and the existing entry is static. To
 *                  overwrite a static entry with a dynamic entry, first
 *                  delete the existing static entry with a call to
 *                  ''fmDeleteAddress''.
 *
 * \note            On FM3000 and FM4000 devices only, if the entry type is
 *                  ''FM_ADDRESS_DYNAMIC'', the destMask member of the
 *                  ''fm_macAddressEntry'' structure may specify only a single
 *                  port, otherwise it must be set to FM_DESTMASK_UNUSED and
 *                  the destination specified with the port structure member.
 *
 * \note            On FM6000 devices only, the destMask member of 
 *                  ''fm_macAddressEntry'' will be ignored. Only the "port"
 *                  structure member may be used.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to an ''fm_macAddressEntry'' structure that
 *                  describes the MA Table entry to be added.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if an attempt was made to use
 *                  a multi-port destMask for a dynamic entry on a non-FM2000
 *                  device, or if the entry type is incorrect, or if entry
 *                  is not valid.
 * \return          FM_ERR_ADDR_BANK_FULL if no room in MA Table for this
 *                  address.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if an attempt was made to
 *                  add a MAC address to a multicast group.
 *
 *****************************************************************************/
fm_status fmAddAddress(fm_int sw, fm_macAddressEntry *entry)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ADDR,
                      "sw=%d "
                      "macAddress=" FM_FORMAT_ADDR " "
                      "vlanID=%d/%d "
                      "destMask=0x%8x "
                      "port=%d "
                      "type=%d "
                      "remoteID=%d "
                      "remoteMac=%d\n ",
                      sw,
                      (entry != NULL) ? entry->macAddress : 0L,
                      (entry != NULL) ? entry->vlanID : 0,
                      (entry != NULL) ? entry->vlanID2 : 0,
                      (entry != NULL) ? entry->destMask : 0,
                      (entry != NULL) ? entry->port : -1, 
                      (entry != NULL) ? entry->type : -1,
                      (entry != NULL) ? entry->remoteID : 0,
                      (entry != NULL) ? entry->remoteMac : 0  );


    VALIDATE_AND_PROTECT_SWITCH(sw);
    
    if (entry == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        goto ABORT;
    }

    switchPtr = GET_SWITCH_PTR(sw);
    FM_API_CALL_FAMILY(err, switchPtr->AddAddress, sw, entry);

ABORT:
    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmAddAddress */




/*****************************************************************************/
/** fmAddAddressInternal
 * \ingroup intAddr
 *
 * \desc            Adds an entry to the MA table. Note that the VLAN field
 *                  of the entry structure is overridden to zero if the
 *                  current vlan mode is shared. This override is reflected
 *                  in the argument structure as well as the internal cache.
 *                  Also, the 'valid' member of the specified entry structure 
 *                  is ignored. The entry written is marked as valid.
 *
 *                  Will return an FM_ERR_ADDR_BANK_FULL error status and will
 *                  not add the address to the table if the request specifies
 *                  a static address and the bin is full of static addresses.
 *                  If the bin is full but does contain some dynamic
 *                  addresses, then the API will remove a dynamic address
 *                  and add the new static address. The API will return the
 *                  status FM_OK in this case.
 *
 *                  Will return an FM_ERR_ADDR_BANK_FULL error status and will
 *                  not add the address to the table if the request specifies
 *                  a dynamic address and the bin is full, regardless of
 *                  whether the bin is full of static addresses, dynamic 
 *                  addresses, or a mixture of both.
 *
 *                  Note that a new static address whose MAC and VLAN match
 *                  an existing static address will override the existing
 *                  static address.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the entry structure to add (will be cloned)
 *
 * \param[in]       trigger is the trigger to write to the entry, or -1 to use
 *                  the default. Ignored on FM6000.
 *
 * \param[in]       updateHw should be TRUE to update the hardware table 
 *                  registers. Ignored on FM6000 as the hardware is always
 *                  updated.
 *
 * \param[in]       bank should be set to -1 to find any available bank, 
 *                  otherwise explicitly set bank. Ignored on FM6000 as all 
 *                  callers always specify -1 anyway.
 *
 * \param[in,out]   numUpdates points to a variable containing the number of
 *                  updates stored in the event buffer. Will be updated if
 *                  an event is added to the buffer.
 *
 * \param[in,out]   outEvent points to a variable containing a pointer to
 *                  the buffer to which the learning and aging events
 *                  should be added. May be NULL, in which case an event
 *                  buffer will be allocated if one is needed. Will be
 *                  updated to point to the new event buffer.
 *
 * \return          FM_OK if successful, or error code (see fm_errno.h)
 * \return          FM_ERR_ADDR_BANK_FULL if there is no room in the MA table
 *                  for the specified address.
 *
 *****************************************************************************/
fm_status fmAddAddressInternal(fm_int                      sw,
                               fm_macAddressEntry*         entry,
                               fm_uint32                   trigger,
                               fm_bool                     updateHw,
                               fm_int                      bank,
                               fm_uint32*                  numUpdates,
                               fm_event**                  outEvent)
{
    fm_status  err;
    fm_switch *switchPtr;
    fm_bool    complete;

    FM_LOG_ENTRY( FM_LOG_CAT_ADDR,
                 "sw=%d entry=%p trigger=0x%x updateHw=%d bank=%d, "
                 "macAddress=" FM_FORMAT_ADDR ", vlan=%u/%u, type=%u, "
                 "destMask=0x%x, port=%d, type=%d, remoteID=%d remoteMac=%d\n",
                 sw,
                 (void *) entry,
                 trigger,
                 updateHw,
                 bank,
                 (entry != NULL) ? entry->macAddress : 0,
                 (entry != NULL) ? entry->vlanID : 0,
                 (entry != NULL) ? entry->vlanID2 : 0,
                 (entry != NULL) ? entry->type : 0,
                 (entry != NULL) ? entry->destMask : 0,
                 (entry != NULL) ? entry->port : -1,
                 (entry != NULL) ? entry->type : -1,
                 (entry != NULL) ? entry->remoteID : -1,
                 (entry != NULL) ? entry->remoteMac : -1);

    if (entry == NULL)
    {
        FM_LOG_EXIT(FM_LOG_CAT_ADDR, FM_ERR_INVALID_ARGUMENT);
    }

    switchPtr = GET_SWITCH_PTR(sw);

    /**************************************************
     * Do pre-processing. For SWAG, this will be the
     * only processing. The complete flag indicates
     * to do no further processing.
     **************************************************/
    
    FM_API_CALL_FAMILY(err,
                       switchPtr->AddAddressToTablePre,
                       sw, 
                       entry, 
                       trigger, 
                       updateHw, 
                       bank, 
                       &complete);
    
    if (err == FM_OK && complete)
    {
        goto EXIT;
    }
    
    if ( (err != FM_OK) && (err != FM_ERR_UNSUPPORTED) )
    {
        goto EXIT;
    }

    if (FM_IS_TEST_TRACE_ADDRESS(entry->macAddress))
    {
        FM_LOG_PRINT("fmAddAddressInternal(%d): " 
                     "adding mac address " FM_FORMAT_ADDR ":\n"
                     "    vlan=%d/%d, type=%s, MAC secure=%s, "
                     "destMask=%08X, port=%d, age=%d, "
                     "trig=%d, updateHw=%d, bank=%d\n",
                     sw, entry->macAddress, entry->vlanID, entry->vlanID2,
                     (entry->type == FM_ADDRESS_STATIC) ? "static" : "dynamic",
                     (entry->type == FM_ADDRESS_SECURE_STATIC) ? "secured" : "unsecured",
                      entry->destMask, entry->port, entry->age,
                     trigger, updateHw, bank);
    }

    switch (entry->type)
    {
        case FM_ADDRESS_STATIC:
        case FM_ADDRESS_DYNAMIC:
        case FM_ADDRESS_SECURE_STATIC:
        case FM_ADDRESS_PROVISIONAL:
            break;

        default:
            err = FM_ERR_INVALID_ENTRYTYPE;
            goto EXIT;

    }   /* end switch (entry->type) */
    

    /**************************************************
     * Get the learning FID. This will either be the
     * VLAN's unique FID or the shared learning FID.
     **************************************************/
        
    FM_API_CALL_FAMILY(err, 
                       switchPtr->GetLearningFID, 
                       sw, 
                       entry->vlanID, 
                       &entry->vlanID);
    
    if (err != FM_OK)
    {
        goto EXIT;
    }

    /**************************************************
     * The TCAM offload logic may get called, and if it
     * does, it will try to take the LAG lock. However,
     * the LAG lock must be taken before the L2_LOCK,
     * so take it now. 
     **************************************************/
    
    if (switchPtr->AddAddressOffload)
    {
        TAKE_LAG_LOCK(sw);
    }
    
    FM_TAKE_L2_LOCK(sw);

    /* Check the address offload table for potential updates. It will return 
     * FM_OK if it processed successfully. In that case, our work is done. If
     * it returns something like NOT_FOUND, we should continue. */
    FM_API_CALL_FAMILY(err,
                       switchPtr->CheckAddressOffload,
                       sw,
                       entry,
                       trigger,
                       updateHw,
                       bank,
                       numUpdates,
                       outEvent);
    
    if (err == FM_OK)
    {
        goto ABORT;
    }
        
    /* Figure out where to store the address and store it. */
    FM_API_CALL_FAMILY(err,
                       switchPtr->AssignTableEntry,
                       sw, 
                       entry, 
                       bank,
                       trigger,
                       updateHw,
                       numUpdates,
                       outEvent);
    
    if (err == FM_ERR_UNSUPPORTED)
    {
        goto ABORT;
    }

    /* If there isn't room for the address in the the MA_TABLE, see if we can
     * add it to the address offload table instead. */
    if (err == FM_ERR_ADDR_BANK_FULL && switchPtr->AddAddressOffload)
    {
        err = (switchPtr->AddAddressOffload)(sw,
                                             entry,
                                             trigger,
                                             updateHw,
                                             bank,
                                             numUpdates,
                                             outEvent);
    }

ABORT:
    FM_DROP_L2_LOCK(sw);

    if (switchPtr->AddAddressOffload)
    {
        DROP_LAG_LOCK(sw);
    }
    
EXIT:
    FM_LOG_EXIT(FM_LOG_CAT_ADDR, err);

}   /* end fmAddAddressInternal */




/*****************************************************************************/
/** fmAddAddressToTable
 * \ingroup intAddr
 *
 * \desc            Adds an entry to the MA table. Note that the VLAN field
 *                  of the entry structure is overridden to zero if the
 *                  current vlan mode is shared. This override is reflected
 *                  in the argument structure as well as the internal cache.
 *                  Also, the 'valid' member of the specified entry structure 
 *                  is ignored. The entry written is marked as valid.
 *
 *                  Will return an FM_ERR_ADDR_BANK_FULL error status and will
 *                  not add the address to the table if the request specifies
 *                  a static address and the bin is full of static addresses.
 *                  If the bin is full but does contain some dynamic
 *                  addresses, then the API will remove a dynamic address
 *                  and add the new static address. The API will return the
 *                  status FM_OK in this case.
 *
 *                  Will return an FM_ERR_ADDR_BANK_FULL error status and will
 *                  not add the address to the table if the request specifies
 *                  a dynamic address and the bin is full, regardless of
 *                  whether the bin is full of static addresses, dynamic 
 *                  addresses, or a mixture of both.
 *
 *                  Note that a new static address whose MAC and VLAN match
 *                  an existing static address will override the existing
 *                  static address.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the entry structure to add (will be cloned)
 *
 * \param[in]       trigger is the trigger to write to the entry, or -1 to use
 *                  the default.
 *
 * \param[in]       updateHw should be TRUE to update the hardware table 
 *                  registers.
 *
 * \param[in]       bank should be set to -1 to find any available bank, 
 *                  otherwise explicitly set bank.
 *
 * \return          FM_OK if successful, or error code (see fm_errno.h)
 * \return          FM_ERR_ADDR_BANK_FULL if there is no room in the MA table
 *                  for the specified address.
 *
 *****************************************************************************/
fm_status fmAddAddressToTable(fm_int                sw,
                              fm_macAddressEntry*   entry,
                              fm_uint32             trigger,
                              fm_bool               updateHw,
                              fm_int                bank)
{
    fm_event*   event = NULL;
    fm_uint32   numUpdates = 0;
    fm_status   err;

    err = fmAddAddressInternal(sw,
                               entry,
                               trigger,
                               updateHw,
                               bank,
                               &numUpdates,
                               &event);

    if (numUpdates != 0)
    {
        fmSendMacUpdateEvent(sw,
                             &fmRootApi->eventThread,
                             &numUpdates,
                             &event,
                             FALSE);
    }

    if (event != NULL)
    {
        fmReleaseEvent(event);
    }

    return err;

}   /* end fmAddAddressToTable */




/*****************************************************************************/
/** fmGetAddress
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Returns an ''fm_macAddressEntry'' structure for the MA 
 *                  Table entry that matches the specified MAC address and 
 *                  VLAN ID. In shared spanning tree mode, the supplied VLAN 
 *                  ID is ignored.
 *                                                                      \lb\lb
 *                  See also ''fmGetAddressV2''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       address is the MAC address to look up.
 *
 * \param[in]       vlanID is the VLAN number to look up.
 *
 * \param[out]      entry points to a caller-allocated ''fm_macAddressEntry'' 
 *                  structure to be filled in by this function.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if no memory available.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_ADDR_NOT_FOUND if address/vlan combination not
 *                  found in the MA Table.
 *
 *****************************************************************************/
fm_status fmGetAddress(fm_int              sw,
                       fm_macaddr          address,
                       fm_int              vlanID,
                       fm_macAddressEntry *entry)
{
    fm_status err;

    err = fmGetAddressV2(sw, address, vlanID, 0, entry);

    return err;

}   /* end fmGetAddress */




/*****************************************************************************/
/** fmGetAddressV2
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Returns an ''fm_macAddressEntry'' structure for the MA 
 *                  Table entry that matches the specified MAC address and 
 *                  VLAN IDs. In shared spanning tree mode, the supplied VLAN 
 *                  IDs are ignored.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       address is the MAC address to look up.
 *
 * \param[in]       vlanID is the VLAN number to look up.
 * 
 * \param[in]       vlanID2 is the second VLAN number to look up (ignored on
 *                  FM2000, FM3000 and FM4000 devices).
 *
 * \param[out]      entry points to a caller-allocated ''fm_macAddressEntry'' 
 *                  structure to be filled in by this function.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_NO_MEM if no memory available.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_ADDR_NOT_FOUND if address/vlan combination not
 *                  found in the MA Table.
 *
 *****************************************************************************/
fm_status fmGetAddressV2(fm_int              sw,
                         fm_macaddr          address,
                         fm_int              vlanID,
                         fm_int              vlanID2,
                         fm_macAddressEntry *entry)
{
    fm_status               err;
    fm_status               retCode;
    fm_uint32               addr;
    fm_uint16 *             indexes = NULL;
    fm_int                  bank;
    fm_internalMacAddrEntry tblentry;
    fm_switch *             switchPtr;
    fm_uint16               learningFID;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR,
                     "sw=%d address=" FM_FORMAT_ADDR " vlanID=%d\n",
                     sw,
                     address,
                     vlanID);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    retCode = FM_ERR_ADDR_NOT_FOUND;

    switchPtr = GET_SWITCH_PTR(sw);

    if (switchPtr->GetAddress != NULL)
    {
        retCode = switchPtr->GetAddress(sw, address, vlanID, vlanID2, entry);
        goto ABORT;
    }

    FM_API_CALL_FAMILY(err, switchPtr->GetLearningFID, sw, vlanID, &learningFID);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ADDR, err);
    vlanID = learningFID;
    
    indexes = (fm_uint16 *) fmAlloc( sizeof(fm_uint16) * switchPtr->macTableBankCount );
    
    if (indexes == NULL)
    {
        retCode = FM_ERR_NO_MEM;
        goto ABORT;
    }

    FM_API_CALL_FAMILY(err, 
                       switchPtr->ComputeAddressIndex, 
                       sw, 
                       address, 
                       vlanID, 
                       vlanID2,
                       indexes);

    if (err != FM_OK)
    {
        retCode = err;
        goto ABORT;
    }

    for (bank = switchPtr->macTableBankCount - 1 ; bank >= 0 ; bank--)
    {
        addr = indexes[bank];

        err = fmReadEntryAtIndex(sw, addr, &tblentry);
    
        if (err != FM_OK)
        {
            retCode = err;
            break;
        }

        if ( (tblentry.state != FM_MAC_ENTRY_STATE_INVALID) &&
             (tblentry.macAddress == address) &&
             (tblentry.vlanID == vlanID) )
        {
            /* Found the requested entry. */
            FM_API_CALL_FAMILY(err,
                               switchPtr->FillInUserEntryFromTable,
                               sw,
                               &tblentry,
                               entry);
            retCode = FM_OK;
            break;
        }
    }

ABORT:
    if (indexes != NULL)
    {
        fmFree(indexes);
    }
    
    if (retCode == FM_ERR_NOT_FOUND && switchPtr->GetAddressOffload)
    {
        retCode = switchPtr->GetAddressOffload(sw, address, vlanID, entry);
    }

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, retCode);

}   /* end fmGetAddressV2 */




/*****************************************************************************/
/** fmGetAddressInternal
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Returns a pointer to the internal address table entry
 *                  which matches the MAC address and VLAN ID supplied in the
 *                  arguments.  In shared spanning tree mode, the supplied
 *                  VLAN ID is ignored (0 is used instead).
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       address is the MAC address to look up.
 *
 * \param[in]       vlanID is the VLAN number to look up.
 *
 * \param[out]      entry points to storage where the pointer to the internal
 *                  MA Table entry structure is to be filled in by this
 *                  function.
 *
 * \param[out]      addrIndex points to storage where the maTable index will
 *                  be placed.  A NULL pointer will cause the index to not
 *                  be returned.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_ADDR_NOT_FOUND if address/vlan combination not
 *                  found in the MA Table.
 *
 *****************************************************************************/
fm_status fmGetAddressInternal(fm_int                    sw,
                               fm_macaddr                address,
                               fm_int                    vlanID,
                               fm_internalMacAddrEntry **entry,
                               fm_uint32 *               addrIndex)
{
    fm_status                err = FM_OK;
    fm_uint32                addr;
    fm_uint16 *              indexes = NULL;
    fm_int                   bank;
    fm_internalMacAddrEntry *tblentry;
    fm_switch *              switchPtr;
    fm_uint16                learningFID;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    FM_API_CALL_FAMILY(err, switchPtr->GetLearningFID, sw, vlanID, &learningFID);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ADDR, err);
    vlanID = learningFID;

    indexes = (fm_uint16 *) fmAlloc( sizeof(fm_uint16) * switchPtr->macTableBankCount );
    
    if (indexes == NULL)
    {
        err = FM_ERR_NO_MEM;
        goto ABORT;
    }

    FM_API_CALL_FAMILY(err, 
                       switchPtr->ComputeAddressIndex, 
                       sw, 
                       address, 
                       vlanID, 
                       0,
                       indexes);


    if (err != FM_OK)
    {
        goto ABORT;
    }

    err = FM_ERR_ADDR_NOT_FOUND;

    FM_TAKE_L2_LOCK(sw);

    for (bank = switchPtr->macTableBankCount - 1 ; bank >= 0 ; bank--)
    {
        addr     = indexes[bank];
        tblentry = &switchPtr->maTable[addr];

        if ((tblentry->state != FM_MAC_ENTRY_STATE_INVALID) &&
            (tblentry->macAddress == address) &&
            (tblentry->vlanID == vlanID))
        {
            tblentry->entryType = FM_MAC_ENTRY_TYPE_CACHE;
            *entry = tblentry;

            if (addrIndex != NULL)
            {
                *addrIndex = addr;
            }

            err = FM_OK;
            break;
        }
    }

    FM_DROP_L2_LOCK(sw);

ABORT:
    if (indexes != NULL)
    {
        fmFree(indexes);
    }
    
    if (err == FM_ERR_ADDR_NOT_FOUND && switchPtr->GetAddressOffloadInternal)
    {
        err = switchPtr->GetAddressOffloadInternal(sw,
                                                   address,
                                                   vlanID,
                                                   entry,
                                                   addrIndex);
    }

    UNPROTECT_SWITCH(sw);

    return err;

}   /* end fmGetAddressInternal */




/*****************************************************************************/
/** fmDeleteAddress
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Delete an address from the MA Table.
 * \note            Deletion of MAC addresses from multicast group logical
 *                  ports is not permitted using this function.
 *                  To delete an address from a multicast group use the
 *                  multicast group subsystem.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the MA Table entry user structure
 *                  containing the address and vlan combination to be
 *                  deleted from the MA Table (other members of the structure
 *                  are ignored).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT if entry is not valid.
 * \return          FM_ERR_ADDR_NOT_FOUND if address/vlan combination not
 *                  found in the MA Table.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if an attempt is made to
 *                  delete a MAC address from a multicast group.
 *
 *****************************************************************************/
fm_status fmDeleteAddress(fm_int sw, fm_macAddressEntry *entry)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY_API( FM_LOG_CAT_ADDR,
                      "sw=%d "
                      "entry->macAddress=" FM_FORMAT_ADDR " "
                      "entry->vlanID=%d/%d\n",
                      sw,
                      (entry != NULL) ? entry->macAddress : 0L,
                      (entry != NULL) ? entry->vlanID : 0,
                      (entry != NULL) ? entry->vlanID2 : 0 );

    VALIDATE_AND_PROTECT_SWITCH(sw);

    if (entry == NULL)
    {
        err = FM_ERR_INVALID_ARGUMENT;
        goto ABORT;
    }

    switchPtr = GET_SWITCH_PTR(sw);
    FM_API_CALL_FAMILY(err, switchPtr->DeleteAddress, sw, entry);
    
    if ( (err == FM_OK) || (err == FM_ERR_UNSUPPORTED) )
    {
        err = fmDeleteAddressFromTable(sw, entry, FALSE, TRUE, -1);
    }
    
ABORT:
    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmDeleteAddress */




/*****************************************************************************/
/** fmDeleteAddressFromTable
 * \ingroup intAddr
 *
 * \desc            Resets the valid bit for the given entry.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the user structure entry to delete (fields
 *                  used for calculating hash into the cache of the MA table).
 *
 * \param[in]       overrideMspt indicates whether to ignore shared VLAN mode
 *                  and unconditionally use the specified vlanID.
 *
 * \param[in]       updateHw should be TRUE to update the hardware registers.
 *
 * \param[in]       bank should be set to -1 to find any available bank,
 *                  otherwise explicitly set bank.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_ADDR_NOT_FOUND if address/vlan combination not
 *                  found in the MA Table.
 *
 *****************************************************************************/
fm_status fmDeleteAddressFromTable(fm_int              sw,
                                   fm_macAddressEntry *entry,
                                   fm_bool             overrideMspt,
                                   fm_bool             updateHw,
                                   fm_int              bank)
{
    fm_status  err;
    fm_uint16 *indexes = NULL;
    fm_uint16  vlanID;
    fm_switch *switchPtr;

    switchPtr = GET_SWITCH_PTR(sw);

    if (FM_IS_TEST_TRACE_ADDRESS(entry->macAddress))
    {
        FM_LOG_PRINT("fmDeleteAddressFromTable(%d): " 
                     "deleting mac address " FM_FORMAT_ADDR ":\n"
                     "    vlan=%d/%d, type=%s, "
                     "destMask=%08X, port=%d, age=%d, overrideMspt=%d\n"
                     "    updateHw=%d, bank=%d\n",
                     sw,
                     entry->macAddress,
                     entry->vlanID,
                     entry->vlanID2,
                     (entry->type == FM_ADDRESS_STATIC) ? "static" : "dynamic",
                     entry->destMask,
                     entry->port,
                     entry->age,
                     overrideMspt,
                     updateHw,
                     bank);
    }

    if (switchPtr->DeleteAddressFromTable != NULL)
    {
        err = switchPtr->DeleteAddressFromTable(sw,
                                                entry,
                                                overrideMspt,
                                                updateHw,
                                                bank);
        goto ABORT;
    }

    /* It's possible that in shared VLAN learning mode, we will want
     * to search for an entry with vlanID equal to 0 regardless of what the
     * user specified in the entry.  However, if the user just switched
     * to shared VLAN learning mode, maybe they actually want to delete
     * entries with non-zero VLAN IDs.  Of course, in switching to shared
     * VLAN mode, we should probably be re-writing the MA Table and VLAN
     * table, so this may be irrelevant, but the caller can specify
     * the intended behavior with the overrideMspt argument.
     */
    vlanID = entry->vlanID;
    
    if (!overrideMspt)
    {
        FM_API_CALL_FAMILY(err, switchPtr->GetLearningFID, sw, vlanID, &vlanID);
        FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ADDR, err);
    }

    indexes = (fm_uint16 *) fmAlloc( sizeof(fm_uint16) * switchPtr->macTableBankCount );
    
    if (indexes == NULL)
    {
        err = FM_ERR_NO_MEM;
        goto ABORT;
    }

    FM_API_CALL_FAMILY(err, 
                       switchPtr->ComputeAddressIndex, 
                       sw, 
                       entry->macAddress, 
                       vlanID, 
                       entry->vlanID2,
                       indexes);


    if (err != FM_OK)
    {
        goto ABORT;
    }

    FM_API_CALL_FAMILY(err, 
                       switchPtr->FindAndInvalidateAddress, 
                       sw, 
                       entry->macAddress, 
                       vlanID, 
                       entry->vlanID2,
                       bank,
                       indexes,
                       updateHw);

    if (err != FM_OK)
    {
        FM_LOG_DEBUG2(FM_LOG_CAT_ADDR,
                      "ERROR: could not find address " FM_FORMAT_ADDR
                      ", index=0%x\n",
                      entry->macAddress, 
                      indexes[0]);
    }

ABORT:
    if (indexes != NULL)
    {
        fmFree(indexes);
    }
    
    if ((err == FM_ERR_ADDR_NOT_FOUND) && (switchPtr->DeleteAddressOffload))
    {
        err = (switchPtr->DeleteAddressOffload)(sw, 
                                                entry, 
                                                overrideMspt, 
                                                updateHw, 
                                                bank);
    }

    return err;

}   /* end fmDeleteAddressFromTable */




/*****************************************************************************/
/** fmGetAddressTable
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Deprecated in favor of ''fmGetAddressTableExt''.
 *                                                                      \lb\lb
 *                  Retrieve a copy of the entire MA Table.
 *
 * \note            May not be used when TCAM MAC Table offload is enabled
 *                  with the ''api.FM4000.MATable.offloadEnable'' API 
 *                  attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      nEntries points to caller-allocated storage where this
 *                  function is to store the number of MA Table entries
 *                  retrieved.
 *
 * \param[out]      entries points to an array of ''fm_macAddressEntry''
 *                  structures that will be filled in by this function with
 *                  MA Table entries.  The array must be large enough to
 *                  hold the maximum number of possible MA Table entries
 *                  (''FM2000_MAX_ADDR'', ''FM4000_MAX_ADDR'' or 
 *                  ''FM6000_MAX_ADDR'', ).
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if an attempt is made to use this
 *                  function when TCAM MAC Table offload is enabled. In that
 *                  case, use ''fmGetAddressTableExt'' instead.
 *
 *****************************************************************************/
fm_status fmGetAddressTable(fm_int              sw,
                            fm_int *            nEntries,
                            fm_macAddressEntry *entries)
{
    fm_switch *    switchPtr;
    fm_status      err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR,
                     "sw=%d nEntries=%p entries=%p\n",
                     sw,
                     (void *) nEntries,
                     (void *) entries);

    VALIDATE_AND_PROTECT_SWITCH(sw);
    switchPtr = GET_SWITCH_PTR(sw);
    
    if (!switchPtr->tcamLearning)
    {
        err = fmGetAddressTableExt(sw, nEntries, entries, switchPtr->macTableSize);
    }
    else
    {
        /**************************************************
         * When TCAM learning is enabled, we need the caller
         * to specify the size of entries by calling
         * ''fmGetAddressTableExt'' explicitly as it is likely
         * the buffer would be overrun.
         **************************************************/
         
        err = FM_ERR_UNSUPPORTED;
        
    }

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmGetAddressTable */




/*****************************************************************************/
/** fmGetAddressTableExt
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieve a copy of the entire MA Table.
 * 
 *                  Note that calling this function with entries equal to NULL
 *                  will only return the number of MA Table entries used.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      nEntries points to caller-allocated storage where this
 *                  function is to store the number of MA Table entries
 *                  retrieved.
 *
 * \param[out]      entries points to an array of ''fm_macAddressEntry''
 *                  structures that will be filled in by this function with
 *                  MA Table entries.  The array must be large enough to
 *                  hold maxEntries number of MA Table entries. entries may
 *                  be set to NULL, in which case only nEntries will be
 *                  filled in by this function.
 *
 * \param[in]       maxEntries is the size of entries, being the maximum
 *                  number of addresses that entries can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * 
 *****************************************************************************/
fm_status fmGetAddressTableExt(fm_int              sw,
                               fm_int *            nEntries,
                               fm_macAddressEntry *entries,
                               fm_int              maxEntries)
{
    fm_switch *switchPtr;
    fm_status  err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR,
                     "sw=%d nEntries=%p entries=%p\n",
                     sw,
                     (void *) nEntries,
                     (void *) entries);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetAddressTable,
                       sw,
                       nEntries,
                       entries,
                       maxEntries);

    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmGetAddressTableExt */




/*****************************************************************************/
/** fmDeleteAllAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Delete all addresses from the MA Table before
 *                  returning to the caller, whether they are dynamic or
 *                  static. Age events are not reported.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *****************************************************************************/
fm_status fmDeleteAllAddresses(fm_int sw)
{
    fm_switch *              switchPtr;
    fm_status                status;
    fm_status                err = FM_OK;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR, "sw=%d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    fmDbgDiagCountIncr(sw, FM_CTR_ALL_DEL, 1);

    /* Clear MA Table */
    FM_API_CALL_FAMILY(err, switchPtr->DeleteAddresses, sw, FALSE);
    
    /* Clear TCAM offload */
    FM_API_CALL_FAMILY(status, switchPtr->DeleteAllAddressessOffload, sw);

    if (status != FM_OK && status != FM_ERR_UNSUPPORTED)
    {
        err = err == FM_OK ? status : err;
    }
    
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmDeleteAllAddresses */




/*****************************************************************************/
/** fmDeleteAllDynamicAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Delete all dynamic addresses from the MA Table before
 *                  returning to the caller. Static entries are left untouched. 
 *                  Age events are not reported. (See also 
 *                  ''fmFlushAllDynamicAddresses''.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *****************************************************************************/
fm_status fmDeleteAllDynamicAddresses(fm_int sw)
{
    fm_switch *switchPtr;
    fm_status  err;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR, "sw=%d\n", sw);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    fmDbgDiagCountIncr(sw, FM_CTR_DYN_DEL, 1);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err, switchPtr->DeleteAddresses, sw, TRUE);

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmDeleteAllDynamicAddresses */




/*****************************************************************************/
/** fmFlushAllDynamicAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Dispatch an independent thread to delete all dynamic
 *                  addresses from the MA Table (this function returns
 *                  immediately, before the MA Table entries have actually been
 *                  deleted). Static entries are left untouched. An age event
 *                  is reported to the application for each MA Table entry
 *                  deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if function not supported.
 * \return          FM_ERR_EVENT_QUEUE_FULL if the independent thread's event
 *                  queue is full.
 *
 *****************************************************************************/
fm_status fmFlushAllDynamicAddresses(fm_int sw)
{
    fm_status      err;
    fm_flushParams params;

    err = fmFlushAddresses(sw, FM_FLUSH_MODE_ALL_DYNAMIC, params);

    return err;

}   /* end fmFlushAllDynamicAddresses */




/*****************************************************************************/
/** fmFlushPortAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Dispatch an independent thread to delete all dynamic MA 
 *                  Table entries associated with the specified port (this 
 *                  function returns immediately, before the MA Table entries 
 *                  have actually been deleted). An age event is reported to 
 *                  the application for each MA Table entry deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port number to filter on.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if function not supported.
 * \return          FM_ERR_EVENT_QUEUE_FULL if the independent thread's event
 *                  queue is full.
 *
 *****************************************************************************/
fm_status fmFlushPortAddresses(fm_int sw, fm_uint port)
{
    fm_status  err;
    fm_flushParams params;

    memset(&params, 0, sizeof(params));
    params.port = port;

    err = fmFlushAddresses(sw, FM_FLUSH_MODE_PORT, params);

    return err;

}   /* end fmFlushPortAddresses */




/*****************************************************************************/
/** fmFlushVlanAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Dispatch an independent thread to delete all dynamic MA 
 *                  Table entries associated with the specified VLAN (this 
 *                  function returns immediately, before the MA Table entries 
 *                  have actually been deleted). An age event is reported to 
 *                  the application for each MA Table entry deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       vlan is the VLAN number to filter on.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if function not supported.
 * \return          FM_ERR_EVENT_QUEUE_FULL if the independent thread's event
 *                  queue is full.
 *
 *****************************************************************************/
fm_status fmFlushVlanAddresses(fm_int sw, fm_uint vlan)
{
    fm_status  err;
    fm_flushParams params;

    memset(&params, 0, sizeof(params));
    params.vid1 = vlan;

    err = fmFlushAddresses(sw, FM_FLUSH_MODE_VLAN, params);

    return err;

}   /* end fmFlushVlanAddresses */




/*****************************************************************************/
/** fmFlushPortVlanAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Dispatch an independent thread to delete all dynamic MA 
 *                  Table entries associated with the specified port and VLAN 
 *                  (this function returns immediately, before the MA Table 
 *                  entries have actually been deleted). An age event is 
 *                  reported to the application for each MA Table entry deleted. 
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       port is the port number to filter on.
 *
 * \param[in]       vlan is the VLAN number to filter on.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if function not supported.
 * \return          FM_ERR_EVENT_QUEUE_FULL if the independent thread's event
 *                  queue is full.
 *
 *****************************************************************************/
fm_status fmFlushPortVlanAddresses(fm_int sw, fm_uint port, fm_int vlan)
{
    fm_status  err;
    fm_flushParams params;

    memset(&params, 0, sizeof(params));
    params.port = port;
    params.vid1 = vlan;

    err = fmFlushAddresses(sw, FM_FLUSH_MODE_PORT_VLAN, params);

    return err;

}   /* end fmFlushPortVlanAddresses */




/*****************************************************************************/
/** fmFlushAddresses
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Dispatch an independent thread to delete all dynamic MA 
 *                  Table entries associated with the specified mode (this 
 *                  function returns immediately, before the MA Table entries 
 *                  have actually been deleted). An age event is reported to 
 *                  the application for each MA Table entry deleted.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       mode indicates the set of parameters used to filter
 *                  the MA Table entries to be flushed. See ''fm_flushMode'' 
 *                  for possible values.
 * 
 * \param[in]       params contains the set of parameters indicated by mode.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_UNSUPPORTED if mode is not supported by the switch.
 * \return          FM_ERR_INVALID_ARGUMENT if mode is not recognized.
 * \return          FM_ERR_EVENT_QUEUE_FULL if the independent thread's event
 *                  queue is full.
 *
 *****************************************************************************/
fm_status fmFlushAddresses(fm_int sw, fm_flushMode mode, fm_flushParams params)
{
    fm_status         err;
    fm_maWorkType     workType;
    fm_maWorkTypeData data;
    fm_bool           automaticFlush;

    VALIDATE_AND_PROTECT_SWITCH(sw);

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR, "sw=%d mode=%d, port=%d, "
                                      "vid1=%d, vid2=%d, remoteId=%d\n", 
                                      sw, 
                                      mode, 
                                      params.port,
                                      params.vid1,
                                      params.vid2,
                                      params.remoteId);

    memset(&data, 0, sizeof(data));

    automaticFlush = fmGetBoolApiAttribute(FM_AAK_API_MA_FLUSH_ON_VLAN_CHANGE, 
                                           FM_AAD_API_MA_FLUSH_ON_VLAN_CHANGE); 

    switch (mode)
    {
        case FM_FLUSH_MODE_ALL_DYNAMIC:
            workType = FM_UPD_FLUSH_DYN_ADDRESSES;
            fmDbgDiagCountIncr(sw, FM_CTR_DYN_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_PORT:
            VALIDATE_LOGICAL_PORT(sw,
                                  (fm_int) params.port,
                                  ALLOW_CPU | ALLOW_LAG | ALLOW_REMOTE);
            workType = FM_UPD_FLUSH_PORT_ADDRESSES;
            data.port = params.port;
            fmDbgDiagCountIncr(sw, FM_CTR_PORT_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_PORT_VLAN:
            VALIDATE_LOGICAL_PORT(sw, 
                                  (fm_int) params.port, 
                                  ALLOW_CPU | ALLOW_LAG | ALLOW_REMOTE);
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid1);
            }
            workType = FM_UPD_FLUSH_PORT_VLAN_ADDRESSES;
            data.port = params.port;
            data.vid1 = params.vid1;
            fmDbgDiagCountIncr(sw, FM_CTR_VLAN_PORT_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_VLAN:
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid1);
            }
            workType = FM_UPD_FLUSH_VLAN_ADDRESSES;
            data.vid1 = params.vid1;
            fmDbgDiagCountIncr(sw, FM_CTR_VLAN_FLUSH, 1);
            break;
            
        case FM_FLUSH_MODE_VID1_VID2:
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid1);
                VALIDATE_VLAN_ID(sw, params.vid2);
            }
            workType = FM_UPD_FLUSH_VID1_VID2_ADDRESSES;
            data.vid1 = params.vid1;
            data.vid2 = params.vid2;
            fmDbgDiagCountIncr(sw, FM_CTR_VID1_VID2_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_PORT_VID1_VID2:
            VALIDATE_LOGICAL_PORT(sw, 
                                  (fm_int) params.port, 
                                  ALLOW_CPU | ALLOW_LAG | ALLOW_REMOTE);
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid1);
                VALIDATE_VLAN_ID(sw, params.vid2);
            }
            workType = FM_UPD_FLUSH_PORT_VID1_VID2_ADDRESSES;
            data.port = params.port;
            data.vid1 = params.vid1;
            data.vid2 = params.vid2;
            fmDbgDiagCountIncr(sw, FM_CTR_PORT_VID1_VID2_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_PORT_VID2:
            VALIDATE_LOGICAL_PORT(sw, 
                                  (fm_int) params.port, 
                                  ALLOW_CPU | ALLOW_LAG | ALLOW_REMOTE);
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid2);
            }
            workType = FM_UPD_FLUSH_PORT_VID2_ADDRESSES;
            data.port = params.port;
            data.vid2 = params.vid2;
            fmDbgDiagCountIncr(sw, FM_CTR_PORT_VID2_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_VID2:
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid2);
            }
            workType = FM_UPD_FLUSH_VID2_ADDRESSES;
            data.vid2 = params.vid2;
            fmDbgDiagCountIncr(sw, FM_CTR_VID2_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_PORT_VID1_REMOTEID:
            VALIDATE_LOGICAL_PORT(sw, 
                                  (fm_int) params.port, 
                                  ALLOW_CPU | ALLOW_LAG | ALLOW_REMOTE);
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid1);
            }
            workType = FM_UPD_FLUSH_PORT_VID1_REMOTEID_ADDRESSES;
            data.port      = params.port;
            data.vid1      = params.vid1;
            data.remoteId  = (params.remoteId & 0xFFF) + ((params.remoteMac & 0x1) << 12); 
            fmDbgDiagCountIncr(sw, FM_CTR_PORT_VID1_REMOTEID_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_VID1_REMOTEID:
            if (automaticFlush)
            {
                VALIDATE_VLAN_ID(sw, params.vid1);
            }
            workType = FM_UPD_FLUSH_VID1_REMOTEID_ADDRESSES;
            data.vid1      = params.vid1;
            data.remoteId  = (params.remoteId & 0xFFF) + ((params.remoteMac & 0x1) << 12); 
            fmDbgDiagCountIncr(sw, FM_CTR_VID1_REMOTEID_FLUSH, 1);
            break;

        case FM_FLUSH_MODE_REMOTEID:
            workType = FM_UPD_FLUSH_REMOTEID_ADDRESSES;
            data.remoteId  = (params.remoteId & 0xFFF) + ((params.remoteMac & 0x1) << 12); 
            fmDbgDiagCountIncr(sw, FM_CTR_REMOTEID_FLUSH, 1);
            break;

        default:
            err = FM_ERR_INVALID_ARGUMENT;
            FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ADDR, err);
    }

    err = fmUpdateMATable(sw, workType, data, NULL, NULL);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ADDR, err);

ABORT:
    UNPROTECT_SWITCH(sw);

    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);
}    /* end fmFlushAddresses */




/*****************************************************************************/
/** fmGetAddressTableAttribute
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Retrieves the value of an MA table related attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       attr is the MA Table attribute to retrieve
 *                  (see 'MA Table Attributes').
 *
 * \param[out]      value points to caller-allocated storage where this
 *                  function should place the attribute value.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ATTRIB unrecognized attr.
 *
 *****************************************************************************/
fm_status fmGetAddressTableAttribute(fm_int sw, fm_int attr, void *value)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR,
                     "sw=%d attr=%d, value=%p\n",
                     sw,
                     attr,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->GetAddressTableAttribute,
                       sw,
                       attr,
                       value);

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmGetAddressTableAttribute */




/*****************************************************************************/
/** fmSetAddressTableAttribute
 * \ingroup addr
 *
 * \chips           FM2000, FM3000, FM4000, FM6000
 *
 * \desc            Set the value of an MA table related attribute.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       attr is the MA Table attribute to retrieve
 *                  (see 'MA Table Attributes').
 *
 * \param[in]       value points to the attribute value to set.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_INVALID_ARGUMENT attr is read-only or value is
 *                  invalid.
 * \return          FM_ERR_INVALID_ATTRIB unrecognized attr.
 *
 *****************************************************************************/
fm_status fmSetAddressTableAttribute(fm_int sw, fm_int attr, void *value)
{
    fm_status  err;
    fm_switch *switchPtr;

    FM_LOG_ENTRY_API(FM_LOG_CAT_ADDR,
                     "sw=%d attr=%d, value=%p\n",
                     sw,
                     attr,
                     (void *) value);

    VALIDATE_AND_PROTECT_SWITCH(sw);

    switchPtr = GET_SWITCH_PTR(sw);

    FM_API_CALL_FAMILY(err,
                       switchPtr->SetAddressTableAttribute,
                       sw,
                       attr,
                       value);

    UNPROTECT_SWITCH(sw);
    FM_LOG_EXIT_API(FM_LOG_CAT_ADDR, err);

}   /* end fmSetAddressTableAttribute */




/*****************************************************************************/
/** fmCompareInternalMacAddressEntries
 * \ingroup intAddr
 *
 * \desc            Compare two MAC address entries for sorting purposes.
 *
 * \param[in]       key1 points to the first record's key.
 *
 * \param[in]       key2 points to the second record's key.
 *
 * \return          -1 if key1 comes before key2.
 * \return           0 if the keys are identical.
 * \return           1 if key1 comes after key2.
 *
 *****************************************************************************/
fm_int fmCompareInternalMacAddressEntries(const void *key1, const void *key2)
{
    fm_internalMacAddrEntry *entry1;
    fm_internalMacAddrEntry *entry2;

    entry1 = (fm_internalMacAddrEntry *) key1;
    entry2 = (fm_internalMacAddrEntry *) key2;

    if (entry1->macAddress < entry2->macAddress)
    {
        return -1;
    }

    if (entry1->macAddress > entry2->macAddress)
    {
        return 1;
    }

    if (entry1->vlanID < entry2->vlanID)
    {
        return -1;
    }

    if (entry1->vlanID > entry2->vlanID)
    {
        return 1;
    }

    if (entry1->vlanID2 < entry2->vlanID2)
    {
        return -1;
    }

    if (entry1->vlanID2 > entry2->vlanID2)
    {
        return 1;
    }

    return 0;

}   /* end fmCompareInternalMacAddressEntries */




/*****************************************************************************/
/** fmValidateAddressPort
 * \ingroup intAddr
 *
 * \desc            Called by chip-specific MAC address management services,
 *                  this function validates the logical port number associated
 *                  with a MAC address and ensures that these management
 *                  services are not used to for multicast groups.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       logicalPort is the logical port associated with the MAC
 *                  address.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_PORT if logicalPort is not valid.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if logicalPort identifies a
 *                  multicast group.
 *
 *****************************************************************************/
fm_status fmValidateAddressPort(fm_int sw, fm_int logicalPort)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR, "sw=%d logicalPort=%d\n", sw, logicalPort);

    /* Validate the logical port number */
    if (!fmIsValidPort(sw, logicalPort, ALLOW_ALL))
    {
        FM_LOG_EXIT(FM_LOG_CAT_ADDR, FM_ERR_INVALID_PORT);
    }

    /* Does the logical port belong to a multicast group?
     * If so, reject the request.
     * Note that multicast groups are assigned a separate, unique
     * logical port. */
    if (fmFindMcastGroupByPort(sw, logicalPort) != NULL)
    {
        /* Addresses may not be attached to Multicast Groups using this
         * interface function - the multicast group functions should
         * be used instead. */
        err = FM_ERR_USE_MCAST_FUNCTIONS;
    }

    FM_LOG_EXIT(FM_LOG_CAT_ADDR, err);

}   /* end fmValidateAddressPort */




/*****************************************************************************/
/** fmCommonAssignTableEntry
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Assigns an address to an entry in the MA table and stores 
 *                  it there.
 *                                                                      \lb\lb
 *                  There is no guarantee that the hardware MA table and the
 *                  software cache are in sync. We might not yet have serviced
 *                  an event in the TCN FIFO, we could be in the middle of a
 *                  hardware purge, or we might even have missed a learning 
 *                  or aging event. To protect against duplicate entries in 
 *                  the address table, we need to inspect the hardware table 
 *                  as well as the cache.
 *                                                                      \lb\lb
 *                  This function does the following:
 *                                                                      \lb\lb
 *                  - Scans the software cache and the hardware MA table for
 *                    matching entries.
 *                                                                      \lb\lb
 *                  - Identifies an entry in the MA table to which the address
 *                    may be written.
 *                                                                      \lb\lb
 *                  - Identifies at most one entry in the software cache to be
 *                    invalidated (without being aged).
 *                                                                      \lb\lb
 *                  - Determines whether the caller may overwrite a static
 *                    (locked) address.
 *                                                                      \lb\lb
 *                  - Updates the cache and the hardware.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the entry structure to add.
 *
 * \param[in]       targetBank is the bank in the address table to which the
 *                  entry should be assigned. Will be -1 if any available 
 *                  bank may be used.
 *
 * \param[in]       trigger is the trigger to write to the entry, or -1 to use
 *                  the default.
 *
 * \param[in]       updateHw should be TRUE to update the hardware table 
 *                  registers.
 *
 * \param[in,out]   numUpdates points to a variable containing the number of
 *                  updates stored in the event buffer. Will be updated if
 *                  an event is added to the buffer.
 *
 * \param[in,out]   outEvent points to a variable containing a pointer to
 *                  the buffer to which the learning and aging events
 *                  should be added. May be NULL, in which case an event
 *                  buffer will be allocated if one is needed. Will be
 *                  updated to point to the new event buffer.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_ADDR_BANK_FULL if there is no room in the MA table
 *                  for the specified address.
 *
 *****************************************************************************/
fm_status fmCommonAssignTableEntry(fm_int                sw,
                                   fm_macAddressEntry *  entry,
                                   fm_int                targetBank,
                                   fm_uint32             trigger,
                                   fm_bool               updateHw,
                                   fm_uint32*            numUpdates,
                                   fm_event**            outEvent)
{
    fm_switch *              switchPtr;
    fm_internalMacAddrEntry* cachePtr;
    fm_internalMacAddrEntry  hwEntry;
    fm_int                   entryIndex;
    fm_int                   dynamicCacheBank        = -1;
    fm_int                   unusedCacheBank         = -1;
    fm_int                   matchingCacheBank       = -1;
    fm_int                   matchingHardwareBank    = -1;
    fm_int                   unusedHardwareBank      = -1;
    fm_int                   dynamicHardwareBank     = -1;
    fm_int                   unusedCommonBank        = -1;
    fm_int                   dynamicCommonBank       = -1;
    fm_int                   invalidIndex            = -1;
    fm_int                   targetIndex             = -1;
    fm_bool                  overwriteStatic         = FALSE;
    fm_uint16 *              indexes                 = NULL;
    fm_status                err = FM_OK;

    switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    indexes = (fm_uint16 *) fmAlloc( sizeof(fm_uint16) * switchPtr->macTableBankCount );
    
    if (indexes == NULL)
    {
        err = FM_ERR_NO_MEM;
        goto ABORT;
    }

    /* Determine the bin to which the entry will be assigned. */
    FM_API_CALL_FAMILY(err, 
                       switchPtr->ComputeAddressIndex, 
                       sw, 
                       entry->macAddress, 
                       entry->vlanID, 
                       entry->vlanID2, 
                       indexes);
    
    if (err != FM_OK)
    {
        goto ABORT;
    }
    
    /*
     * Scan the bin for matching entries and available locations.
     */
    if (targetBank == -1)
    {
        fm_int bankId;

        /* Search the bin for a matching entry, an unused entry,
         * or a dynamic entry. */
        for (bankId = switchPtr->macTableBankCount - 1 ; 
             bankId >= 0 ; 
             bankId--)
        {
            entryIndex = indexes[bankId];
            cachePtr = &switchPtr->maTable[entryIndex];

            /* If it's a static entry, just check the cache. */
            if (cachePtr->state == FM_MAC_ENTRY_STATE_LOCKED)
            {
                if (cachePtr->macAddress == entry->macAddress &&
                    cachePtr->vlanID == entry->vlanID &&
                    cachePtr->vlanID2 == entry->vlanID2)
                {
                    matchingCacheBank = bankId;
                    targetBank = bankId;
                    break;
                }
                continue;
            }

            /* Read the entry from the hardware MA table. */
            err = fmReadEntryAtIndex(sw, entryIndex, &hwEntry);
            if (err != FM_OK)
            {
                goto ABORT;
            }

            /*
             * Check for a matching hardware entry. 
             *  
             * Keep track of the lowest-numbered unused bank and the 
             * lowest-numbered bank that contains a dynamic address.
             */
            if (hwEntry.state != FM_MAC_ENTRY_STATE_INVALID)
            {
                if (hwEntry.macAddress == entry->macAddress &&
                    hwEntry.vlanID == entry->vlanID &&
                    hwEntry.vlanID2 == entry->vlanID2)
                {
                    if (matchingHardwareBank != -1)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_ADDR,
                                     "(" FM_FORMAT_ADDR ", %d/%d) "
                                     "occurs more than once in the MA table\n",
                                     hwEntry.macAddress,
                                     hwEntry.vlanID,
                                     hwEntry.vlanID2);
                    }
                    matchingHardwareBank = bankId;
                }
                else
                {
                    /* Lowest-numberd dynamic bank. */
                    dynamicHardwareBank = bankId;
                }
            }
            else
            {
                /* Save lowest-numbered unused bank. */
                unusedHardwareBank = bankId;
            }

            /*
             * Check for a matching entry in the software cache.
             *  
             * Keep track of the lowest-numbered unused bank and the 
             * lowest-numbered bank that contains a dynamic address
             */
            if (cachePtr->state != FM_MAC_ENTRY_STATE_INVALID)
            {
                if (cachePtr->macAddress == entry->macAddress &&
                    cachePtr->vlanID == entry->vlanID &&
                    cachePtr->vlanID2 == entry->vlanID2)
                {
                    if (matchingCacheBank != -1)
                    {
                        FM_LOG_ERROR(FM_LOG_CAT_ADDR,
                                     "(" FM_FORMAT_ADDR ", %d/%d) "
                                     "occurs more than once in the cache\n",
                                     cachePtr->macAddress,
                                     cachePtr->vlanID,
                                     cachePtr->vlanID2);
                    }
                    matchingCacheBank = bankId;
                }
                else
                {
                    /* Lowest-numbered dynamic bank. */
                    dynamicCacheBank = bankId;
                }
            }
            else
            {
                /* Lowest-numbered unused bank. */
                unusedCacheBank = bankId;
            }

            /*
             * Keep track of the lowest-numbered unused bank and dynamic 
             * bank the hardware and software have in common. 
             */
            if (unusedCacheBank == unusedHardwareBank)
            {
                unusedCommonBank = unusedCacheBank;
            }

            if (dynamicCacheBank == dynamicHardwareBank)
            {
                dynamicCommonBank = dynamicCacheBank;
            }

        }   /* end for (bankId = switchPtr->macTableBankCount - 1 ; ...) */

    }   /* end if (targetBank == -1) */

    /*
     * Use the results of the scan to determine the bank to use.
     */
    if (entry->type == FM_ADDRESS_STATIC)
    {
        /* If adding a static entry... */
        if (targetBank != -1)
        {
            /* Use the selected target bank. */
            overwriteStatic = TRUE;
        }
        else if (matchingHardwareBank != -1)
        {
            /* ...and there is a matching hardware entry, reuse it. */
            targetBank = matchingHardwareBank;
            overwriteStatic = TRUE;
        }
        else if (matchingCacheBank != -1)
        {
            /* ...or there is a matching cache entry, reuse it. */
            targetBank = matchingCacheBank;
            overwriteStatic = TRUE;
        }
        else if (unusedCommonBank != -1)
        {
            /* ...or if there is a common unused entry, use it. */
            targetBank = unusedCommonBank;
        }
        else if (unusedHardwareBank != -1)
        {
            /* ...or if there is room for a new entry, use it. */
            targetBank = unusedHardwareBank;
        }
        else if (dynamicCommonBank != -1)
        {
            /* ...or if there is a common dynamic entry, overwrite it. */
            targetBank = dynamicCommonBank;
        }
        else if (dynamicHardwareBank != -1)
        {
            /* ...or if there is a dynamic entry, overwrite it. */
            targetBank = dynamicHardwareBank;
        }
        else
        {
            /* ...else no room for a new entry. */
            err = FM_ERR_ADDR_BANK_FULL;
            goto ABORT;
        }

    }
    else
    {
        /*
         * Note that we do NOT test for a matching cache bank.
         *  
         * 1. If the cache entry is not stale, then matchingHardwareBank 
         *    will be set. This is our first choice after targetBank.
         *  
         * 2. If the cache entry is stale, there is no guarantee that the 
         *    corresponding hardware bank is unused. The API is not
         *    supposed to overwrite a dynamic entry with another dynamic
         *    entry.
         *  
         * 3. Even if the corresponding hardware bank is unused, we would 
         *    rather assign a different hardware bank. This is because
         *    there may be an unserviced AGE event for the hardware entry
         *    in the TCN FIFO. If we restore the hardware entry before the
         *    AGE event is processed, then we might age out the new entry
         *    prematurely. This is not catastrophic -- we don't guarantee
         *    the longevity of user-defined dynamic addresses -- but the
         *    situation is worth avoiding if it doesn't cost us too much.
         *  
         * 4. If the corresponding hardware bank is the only unused bank, 
         *    then unusedHardwareBank will be set.
         */
        if (targetBank != -1)
        {
            /* Use the selected target bank. */
        }
        else if (matchingHardwareBank != -1)
        {
            /*
             * Our first choice is to assign the entry to a matching 
             * hardware bank. 
             */
            targetBank = matchingHardwareBank;
        }
        else if (unusedCommonBank != -1)
        {
            /*
             * Our second choice is a bank that is unused in both hardware 
             * and software. 
             */
            targetBank = unusedCommonBank;
        }
        else if (unusedHardwareBank != -1)
        {
            /*
             * Our third choice is to assign the entry to a bank that is
             * unused in hardware.
             */
            targetBank = unusedHardwareBank;
        }
        else
        {
            /* ...else no room for a new entry. */
            err = FM_ERR_ADDR_BANK_FULL;
            goto ABORT;
        }

    }

    /* ASSERT: (targetBank != -1) */
    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           targetBank != -1,
                           err = FM_ERR_ASSERTION_FAILED,
                           "targetBank == -1\n");


    /* Index of the cache entry we are going to write. */
    entryIndex = indexes[targetBank];

    /* Make sure we're allowed to overwrite this entry. */
    if (switchPtr->maTable[entryIndex].state != FM_MAC_ENTRY_STATE_LOCKED ||
        overwriteStatic)
    {
        targetIndex = entryIndex;
    }

    /* If we have a matching cache entry that is stale (it doesn't match
     * the hardware entry), and we're not planning to use it as the target 
     * entry (in which case we would need to age it out now), then instruct
     * the caller to invalidate the cache entry. */
    if (matchingCacheBank != matchingHardwareBank &&
        matchingCacheBank != targetBank &&
        matchingCacheBank != -1)
    {
        invalidIndex = indexes[matchingCacheBank];
    }

    /* Invalidate a stale cache entry. */
    if (invalidIndex != -1)
    {
        /* Pointer to cache entry we are going to invalidate. */
        cachePtr = &switchPtr->maTable[invalidIndex];

        FM_LOG_DEBUG(FM_LOG_CAT_EVENT_MAC_MAINT,
                     "invalidating cache entry, "
                     "index=%d, "
                     "mac=" FM_FORMAT_ADDR ", "
                     "vlan=%d/%d\n",
                     invalidIndex,
                     cachePtr->macAddress,
                     cachePtr->vlanID,
                     cachePtr->vlanID2);

        cachePtr->state = FM_MAC_ENTRY_STATE_INVALID;
    }

    /* Make sure we are allowed to overwrite this entry. */
    if (targetIndex != -1)
    {
        /* Update MA table and cache. */
        err = StoreTableEntry(sw,
                              entry,
                              trigger,
                              updateHw,
                              targetIndex,
                              numUpdates,
                              outEvent);

        if (err != FM_OK)
        {
            goto ABORT;
        }

        if (updateHw)
        {
            /* Make sure this address is not duplicated. */
            err = RemoveDuplicateEntries(sw, entry, targetIndex);
        }
    }

ABORT:
    if (indexes != NULL)
    {
        fmFree(indexes);
    }
    
    return err;

}   /* end fmCommonAssignTableEntry */


/*****************************************************************************/
/** fmCommonFindAndInvalidateAddr
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Scan the MA Table bin for the specified address/VLAN and
 *                  invalidate the entry.
 *
 * \note            The switch lock is assumed to be taken on entry. 
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       macAddress is the MAC address to search for.
 *
 * \param[in]       vlanID is the VLAN ID to search for.
 * 
 * \param[in]       vlanID2 is the second VLAN ID to search for.
 *
 * \param[in]       bank is the bank within the bin to operate on, or -1
 *                  if the bin should be searched.
 *
 * \param[in]       indexes is an array of the MA Table indexes comprising
 *                  the hash bin for the macAddress and vlanID. The length 
 *                  of the array is equal to switchPtr->macTableBankSize.
 *
 * \param[in]       updateHw should be TRUE to update the hardware registers.
 *
 * \return          FM_OK if entry successfully deleted.
 * \return          FM_ERR_ADDR_NOT_FOUND if macAddress/vlanID could not be
 *                  found at any of the MA Table indexes in indexes.
 *
 *****************************************************************************/
fm_status fmCommonFindAndInvalidateAddr(fm_int     sw, 
                                        fm_macaddr macAddress, 
                                        fm_uint16  vlanID,
                                        fm_uint16  vlanID2,
                                        fm_int     bank,
                                        fm_uint16 *indexes,
                                        fm_bool    updateHw)
{
    fm_uint32                addr;
    fm_internalMacAddrEntry *tblentry;
    fm_internalMacAddrEntry  tmpEntry;
    fm_switch *              switchPtr;
    fm_status                err       = FM_OK;
    fm_event *               event = NULL;
    fm_uint32                numUpdates = 0;
    fm_bool                  l2Locked=FALSE;
    
    FM_LOG_ENTRY_VERBOSE(FM_LOG_CAT_ADDR, 
                         "sw=%d, macAddress=" FM_FORMAT_ADDR " "
                         "entry->vlanID=%d, bank=%d, indexes=%p, updateHw=%d\n",
                         sw,
                         macAddress,
                         vlanID,
                         bank,
                         (void *) indexes,
                         updateHw);

    switchPtr = GET_SWITCH_PTR(sw);
    
    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    FM_TAKE_L2_LOCK(sw);
    l2Locked = TRUE;

    if (bank == -1)
    {
        for (bank = switchPtr->macTableBankCount - 1 ; bank >= 0 ; bank--)
        {
            addr     = indexes[bank];
            tblentry = &switchPtr->maTable[addr];

            if ( (tblentry->state != FM_MAC_ENTRY_STATE_INVALID) &&
                (tblentry->macAddress == macAddress) &&
                (tblentry->vlanID == vlanID) &&
                (tblentry->vlanID2 == vlanID2) )
            {
                break;
            }
        }
    }

    if (bank != -1)
    {
        addr     = indexes[bank];
        tblentry = &switchPtr->maTable[addr];

        if ( (tblentry->state != FM_MAC_ENTRY_STATE_INVALID) &&
            (tblentry->macAddress == macAddress) &&
            (tblentry->vlanID == vlanID) &&
            (tblentry->vlanID2 == vlanID2) )
        {
            tmpEntry = *tblentry;
            tmpEntry.state = FM_MAC_ENTRY_STATE_INVALID;

            if (updateHw)
            {
                err = fmWriteEntryAtIndex(sw, addr, &tmpEntry);

                if ( (tblentry->state == FM_MAC_ENTRY_STATE_LOCKED &&
                      switchPtr->generateEventOnStaticAddr) ||
                     (tblentry->state != FM_MAC_ENTRY_STATE_LOCKED &&
                      switchPtr->generateEventOnDynamicAddr) )
                {
                    /* Relinquish lock before generating updates. */
                    FM_DROP_L2_LOCK(sw);
                    l2Locked = FALSE;

                    fmGenerateUpdateForEvent(sw,
                                             &fmRootApi->eventThread,
                                             FM_EVENT_ENTRY_AGED,
                                             FM_MAC_REASON_API_AGED,
                                             addr,
                                             tblentry,
                                             &numUpdates,
                                             &event);
                    
                    fmDbgDiagCountIncr(sw, FM_CTR_MAC_API_AGED, 1);
                }
            }

            tblentry->state = FM_MAC_ENTRY_STATE_INVALID;

            FM_LOG_DEBUG2(FM_LOG_CAT_ADDR,
                          "index=0x%x/%d mac=" FM_FORMAT_ADDR 
                          "/0x%x/0x%x aged (api)\n",
                          addr, 
                          bank, 
                          tblentry->macAddress, 
                          tblentry->vlanID,
                          tblentry->vlanID2);
        }
    }
    else
    {
        err = FM_ERR_ADDR_NOT_FOUND;
    }
    
    if (l2Locked)
    {
        FM_DROP_L2_LOCK(sw);
    }

    if (numUpdates != 0)
    {
        fmSendMacUpdateEvent(sw,
                             &fmRootApi->eventThread,
                             &numUpdates,
                             &event,
                             FALSE);
    }

    if (event != NULL)
    {
        fmReleaseEvent(event);
    }
    
ABORT:
    FM_LOG_EXIT_VERBOSE(FM_LOG_CAT_ADDR, err);

}   /* end  fmCommonFindAndInvalidateAddr */



/*****************************************************************************/
/** fmCommonAllocAddrTableCache
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Performs the allocation for the internal storage that
 *                  contains the address table.
 *
 * \param[in]       switchPtr points to the switch's state structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCommonAllocAddrTableCache(fm_switch *switchPtr)
{
    fm_status err = FM_OK;
    fm_uint   size;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH,
                 "switchPtr=%p, sw=%d\n",
                 (void *) switchPtr,
                 switchPtr->switchNumber);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    size = (fm_uint) sizeof(fm_internalMacAddrEntry);
    size *= (fm_uint) switchPtr->macTableSize;

    switchPtr->maTable = (fm_internalMacAddrEntry *) fmAlloc(size);

    if (switchPtr->maTable == NULL)
    {
        err = FM_ERR_NO_MEM;
    }
    else
    {
        memset((void *) switchPtr->maTable, 0, (size_t) size);
    }

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH, err);

}   /* end fmCommonAllocAddrTableCache */



/*****************************************************************************/
/** fmCommonFreeAddrTableCache
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Frees the internal storage that contains the address table.
 *
 * \param[in]       switchPtr points to the switch's state structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCommonFreeAddrTableCache(fm_switch *switchPtr)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH,
                 "switchPtr=%p, sw=%d\n",
                 (void *) switchPtr,
                 switchPtr->switchNumber);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    if (switchPtr->maTable)
    {
        fmFree(switchPtr->maTable);
        switchPtr->maTable = NULL;
    }
    
ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH, err);

}   /* end fmCommonFreeAddrTableCache */



/*****************************************************************************/
/** fmCommonInitAddrTableCache
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Initialize the local copy of the MA Table.
 *
 * \param[in]       switchPtr points to the switch's state structure.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCommonInitAddrTableCache(fm_switch *switchPtr)
{
    fm_status err = FM_OK;
    fm_int    i;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH,
                 "switchPtr=%p, sw=%d\n",
                 (void *) switchPtr,
                 switchPtr->switchNumber);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH,
                           switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000,
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    for (i = 0 ; i < switchPtr->macTableSize ; i++)
    {
        switchPtr->maTable[i].state = FM_MAC_ENTRY_STATE_INVALID;
    }
    
ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_ADDR | FM_LOG_CAT_SWITCH, err);

}   /* end fmCommonInitAddrTableCache */




/*****************************************************************************/
/** fmCommonGetAddressTable
 * \ingroup intAddr
 *
 * \desc            Retrieve a copy of the entire MA Table. For FM2000,
 *                  FM4000 and FM6000.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[out]      nEntries points to caller-allocated storage where this
 *                  function is to store the number of MA Table entries
 *                  retrieved.
 *
 * \param[out]      entries points to an array of fm_macAddressEntry
 *                  structures that will be filled in by this function with
 *                  MA Table entries.  The array must be large enough to
 *                  hold maxEntries number of MA Table entries.
 *
 * \param[in]       maxEntries is the size of entries, being the maximum
 *                  number of addresses that entries can hold.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 *
 *****************************************************************************/
fm_status fmCommonGetAddressTable(fm_int              sw,
                                  fm_int *            nEntries,
                                  fm_macAddressEntry *entries,
                                  fm_int              maxEntries)
{
    fm_switch *             switchPtr;
    fm_status               result = FM_OK;
    fm_status               status;
    fm_int                  i;
    fm_internalMacAddrEntry hwEntry;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR,
                 "sw=%d nEntries=%p entries=%p maxEntries=%d\n",
                 sw,
                 (void *) nEntries,
                 (void *) entries,
                 maxEntries);

    switchPtr = GET_SWITCH_PTR(sw);

    *nEntries = 0;

    /***************************************************
     * The MAC address table is read from hardware rather
     * than the cache. On the FM2000 and FM4000 this is
     * for debugging purposes. On the FM6000 this is a
     * necessity since there is no cache.
     *
     * Whenever an error is encountered, either because 
     * the entry could not be retrieved or due to a 
     * conversion error, the entry in question is skipped. 
     * If multiple errors occur, the user is only notified
     * of the reason for the first error.
     **************************************************/
    for (i = 0 ; i < switchPtr->macTableSize ; i++)
    {
        /* Skip any non applicable entries */
        if (switchPtr->IsIndexValid != NULL && !switchPtr->IsIndexValid(sw, i))
        {
            continue;
        }

        status = fmReadEntryAtIndex(sw, (fm_uint32) i, &hwEntry);

        if (status != FM_OK)
        {
            result = result == FM_OK ? status : result;

            continue;
        }

        if (hwEntry.state != FM_MAC_ENTRY_STATE_INVALID)
        {
            if (*nEntries >= maxEntries)
            {
                result = FM_ERR_BUFFER_FULL;

                goto ABORT;
            }

            /* entries == NULL is used to count the number of MAC entry */
            if (entries != NULL)
            {
                FM_API_CALL_FAMILY(status,
                                   switchPtr->FillInUserEntryFromTable,
                                   sw,
                                   &hwEntry,
                                   &(entries[*nEntries]));

                if (status != FM_OK)
                {
                    result = result == FM_OK ? status : result;

                    continue;
                }
            }

            (*nEntries)++;
        }
    }

    FM_API_CALL_FAMILY(status,
                       switchPtr->GetTcamAddressTable,
                       sw, 
                       nEntries, 
                       entries, 
                       maxEntries);

    if (status != FM_OK && status != FM_ERR_UNSUPPORTED)
    {
        result = result == FM_OK ? status : result;
    }
    
ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_ADDR, result);

}   /* end fmCommonGetAddressTable */





/*****************************************************************************/
/** fmCommonDeleteAddress
 * \ingroup intAddr
 *
 * \desc            For FM2000 and FM4000 only.                             \lb
 *                  Verifies that we are not trying to delete a MAC address
 *                  for a multicast group using the address management API.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       entry points to the MAC address table entry to be deleted.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_USE_MCAST_FUNCTIONS if logicalPort identifies a
 *                  multicast group.
 *
 *****************************************************************************/
fm_status fmCommonDeleteAddress(fm_int sw, fm_macAddressEntry *entry)
{
    fm_status err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_ADDR,
                 "sw=%d "
                 "macAddress=" FM_FORMAT_ADDR " "
                 "vlanID=%d/%d "
                 "destMask=0x%8x "
                 "port=%d\n",
                 sw,
                 entry->macAddress,
                 entry->vlanID,
                 entry->vlanID2,
                 entry->destMask,
                 entry->port);

    
    if (entry->destMask == FM_DESTMASK_UNUSED)
    {
        /* Does the logical port belong to a multicast group?
         * If so, reject the request.
         * Note that multicast groups are assigned a separate, unique
         * logical port. */
        if (fmFindMcastGroupByPort(sw, entry->port) != NULL)
        {
            /* Addresses may not be detached from Multicast Groups using
             * this interface function - the multicast group functions
             * should be used instead.
             */
             err = FM_ERR_USE_MCAST_FUNCTIONS;
        }
    }

    FM_LOG_EXIT(FM_LOG_CAT_ADDR, err);

}   /* end fmCommonDeleteAddress */



/*****************************************************************************/
/** fmCommonDeleteAddressesCached
 * \ingroup intAddr
 *
 * \desc            Delete addresses from the MA Table without generating
 *                  AGE events.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       dynamicOnly should be set to TRUE to leave static entries
 *                  in the table.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status fmCommonDeleteAddressesCached(fm_int sw, fm_bool dynamicOnly)
{
    fm_switch *              switchPtr;
    fm_status                status;
    fm_int                   i;
    fm_internalMacAddrEntry *cacheEntry;
    fm_status                err = FM_OK;

    FM_LOG_ENTRY(FM_LOG_CAT_EVENT_MAC_MAINT, 
                 "sw=%d, dynamicOnly=%s\n", 
                 sw,
                 dynamicOnly ? "TRUE" : "FALSE");

    switchPtr = GET_SWITCH_PTR(sw);

    FM_LOG_ABORT_ON_ASSERT(FM_LOG_CAT_ADDR,
                           (switchPtr->switchFamily != FM_SWITCH_FAMILY_FM6000) &&
                           (switchPtr->switchFamily != FM_SWITCH_FAMILY_REMOTE_FM6000),
                           err = FM_ERR_ASSERTION_FAILED,
                           "FM2000/FM4000-only function called for FM6000\n");

    FM_BEGIN_FIBM_BATCH(sw, err, FM_LOG_CAT_ADDR);
    
    for (i = 0 ; i < switchPtr->macTableSize ; i++)
    {
        /***************************************************
         * Take and release the MAC address table lock for
         * each individual entry, since deleting all entries
         * could be a relatively long operation.
         **************************************************/
         
        FM_TAKE_L2_LOCK(sw);

        cacheEntry = &switchPtr->maTable[i];

        if ( !dynamicOnly ||
             ( (cacheEntry->state != FM_MAC_ENTRY_STATE_INVALID) &&
               (cacheEntry->state != FM_MAC_ENTRY_STATE_LOCKED) ) )
        {
            cacheEntry->state = FM_MAC_ENTRY_STATE_INVALID;

            fmDbgDiagCountIncr(sw, FM_CTR_MAC_CACHE_DELETED, 1);

            status = fmWriteEntryAtIndex(sw, (fm_uint32) i, cacheEntry); 

            if (status != FM_OK)
            {
                fmDbgDiagCountIncr(sw, FM_CTR_MAC_WRITE_ERR, 1);
                err = err == FM_OK ? status : err;
            }
            
        }

        FM_DROP_L2_LOCK(sw);
        
    }   /* end for (i = 0 ; i < switchPtr->macTableSize ; i++) */

    FM_END_FIBM_BATCH(sw, status);

    if (err == FM_OK)
    {
        err = status;
    }
    
ABORT:    
    FM_LOG_EXIT(FM_LOG_CAT_EVENT_MAC_MAINT, err);

}   /* end fmCommonDeleteAddressesCached */



/*****************************************************************************/
/** fmGetAddressIndex
 * \ingroup intAddr
 *
 * \desc            Provided for the benefit of TestPoint, returns the
 *                  index and bank of the MAC Table entry matching the
 *                  specified MAC address and VLAN ID. In shared spanning 
 *                  tree mode, the supplied VLAN ID is ignored and the
 *                  default is used instead.
 *
 * \param[in]       sw is the switch on which to operate.
 *
 * \param[in]       macAddress is the MAC address to look up.
 *
 * \param[in]       vlanID is the VLAN number to look up.
 * 
 * \param[in]       vlanID2 is the second VLAN number to look up.
 *
 * \param[out]      index points to caller-allocated storage where the 
 *                  this function should place the located address's table
 *                  index.
 *
 * \param[out]      bank points to caller-allocated storage where the 
 *                  this function should place the located address's table
 *                  bank.
 *
 * \return          FM_OK if successful.
 * \return          FM_ERR_INVALID_SWITCH if sw is invalid.
 * \return          FM_ERR_ADDR_NOT_FOUND if address/vlan combination not
 *                  found in the MA Table.
 *
 *****************************************************************************/
fm_status fmGetAddressIndex(fm_int     sw,
                            fm_macaddr macAddress,
                            fm_int     vlanID,
                            fm_int     vlanID2,
                            fm_int *   index,
                            fm_int *   bank)
{
    fm_status  err;
    fm_switch *switchPtr;
    fm_uint16  learningFID;
    
    FM_LOG_ENTRY(FM_LOG_CAT_ADDR,
                 "sw=%d "
                 "macAddress=" FM_FORMAT_ADDR " "
                 "vlanID=%d "
                 "index=%p "
                 "bank=%p\n",
                 sw,
                 macAddress,
                 vlanID,
                 (void *) index,
                 (void *) bank);
    
    VALIDATE_AND_PROTECT_SWITCH(sw);
    switchPtr = GET_SWITCH_PTR(sw);
    
    FM_API_CALL_FAMILY(err, switchPtr->GetLearningFID, sw, vlanID, &learningFID);
    FM_LOG_ABORT_ON_ERR(FM_LOG_CAT_ADDR, err);
    vlanID = learningFID;

    FM_API_CALL_FAMILY(err, 
                       switchPtr->GetAddressIndex, 
                       sw,
                       macAddress,
                       vlanID,
                       vlanID2,
                       index,
                       bank);

ABORT:
    FM_LOG_EXIT(FM_LOG_CAT_ADDR, err);

}   /* end fmGetAddressIndex */



