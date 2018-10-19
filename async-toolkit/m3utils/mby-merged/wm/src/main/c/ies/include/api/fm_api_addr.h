/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_addr.h
 * Creation Date:   May 2, 2005
 * Description:     Structures and functions for dealing with MA table
 *                  configuration
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

#ifndef __FM_FM_API_ADDR_H
#define __FM_FM_API_ADDR_H


/****************************************************************************/
/** \ingroup constSystem
 *
 * Used to indicate when a destmask field is not used and a corresponding
 * logical port field is used, for example, in the ''fm_eventTableUpdate''
 * structure.
 ****************************************************************************/
#define FM_DESTMASK_UNUSED  0xffffffffL


/****************************************************************************/
/** \ingroup constSystem
 *
 *  Used to indicate when the port on which a MAC address is learned is not
 *  specified, used for example in the ''fm_eventTableUpdate'' structure.
 *  The port is unspecified in the AGE event for a station move on FM6000
 *  devices.
 ****************************************************************************/
#define FM_MAC_ADDR_PORT_UNSPECIFIED  -1


/****************************************************************************/
/* Other macros
 ****************************************************************************/

#define fmIsMulticastMacAddress(addr)  ( ( addr & \
                                           FM_LITERAL_64(0x010000000000) ) ? \
                                          TRUE : FALSE )

/*
 * Evaluates to TRUE if the argument has a single bit set. 
 * 'x' must be unsigned and non-zero. 
 */
#define FM_IS_ONEHOT_BITMASK(x)     ( ( (x) & ((x) - 1) ) == 0 )

/**************************************************/
/** \ingroup typeEnum
 *  Set of modes used as an argument to 
 *  ''fmFlushAddresses''.
 **************************************************/
typedef enum
{
    /** Delete all dynamic addresses from the MA Table.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_FLUSH_MODE_ALL_DYNAMIC = 0,

    /** Delete all addresses from the MA Table associated with the specified 
     *  port.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_FLUSH_MODE_PORT,

    /** Delete all addresses from the MA Table associated with the specified 
     *  port and VLAN (VLAN1).
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_FLUSH_MODE_PORT_VLAN,

    /** Delete all addresses from the MA Table associated with the specified 
     *  VLAN (VLAN1).
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_FLUSH_MODE_VLAN,

    /** Delete all addresses from the MA Table associated with the specified 
     *  VLAN1 and VLAN2.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_VID1_VID2,

    /** Delete all addresses from the MA Table associated with the specified 
     *  port, VLAN1 and VLAN2.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_PORT_VID1_VID2,

    /** Delete all addresses from the MA Table associated with the specified 
     *  port and VLAN2.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_PORT_VID2,

    /** Delete all addresses from the MA Table associated with the specified 
     *  VLAN2.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_VID2,

    /** Delete all addresses from the MA Table associated with the specified 
     *  port, VLAN1 and remoteID.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_PORT_VID1_REMOTEID,

    /** Delete all addresses from the MA Table associated with the specified 
     *  VLAN1 and remoteID.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_VID1_REMOTEID,

    /** Delete all addresses from the MA Table associated with the specified 
     *  remoteID.
     *
     *  \chips  FM6000 */
    FM_FLUSH_MODE_REMOTEID,

    /* ----  Add new entries above this line.  ---- */

    /** For internal use only. */
    FM_FLUSH_MODE_MAX,

} fm_flushMode;


/****************************************************************************/
/** \ingroup constAddrTypes
 *  MA Table Entry Types, as used in the ''fm_macAddressEntry'' data 
 *  structure.
 ****************************************************************************/
enum _fm_addressTypes
{
    /** MA Table entry is static and will not be subject to aging. 
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_ADDRESS_STATIC = 0,

    /** MA Table entry is dynamic and will be subject to aging. 
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    FM_ADDRESS_DYNAMIC,

    /** Secure static MA Table entry. The frames 
     *  received on an  unsecure port with a SMAC matching this entry will 
     *  be dropped. This entry will not subject to aging. 
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_ADDRESS_SECURE_STATIC, 

    /** Secure dynamic MA Table entry. The frames 
     *  received on an unsecure port with a SMAC matching this entry will be 
     *  dropped. This entry will subject to aging. 
     *
     *  \chips  FM3000, FM4000, FM6000 */
    FM_ADDRESS_SECURE_DYNAMIC,

    /** MA Table entry is provisional and will be subject to aging.
     *
     *  \chips  FM6000 */
    FM_ADDRESS_PROVISIONAL

};


/**************************************************/
/** \ingroup typeStruct
 *  Used as an argument to ''fmAddAddress'', 
 *  ''fmDeleteAddress'', ''fmGetAddress'' and others
 *  to hold a MAC Address Table entry.
 **************************************************/
typedef struct _fm_macAddressEntry
{
    /** The MAC address.
     *  
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_macaddr macAddress;

    /** VLAN ID (VLAN1).
     *  
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_uint16  vlanID;

    /** VLAN ID2 (VLAN2). Set this field to zero, in conjunction with the VLAN
     *  attribute ''FM_VLAN_FID2_IVL'' to FM_DISABLED (the default), to
     *  prevent FID2 from being considered during the MAC address table lookup.
     *  
     *  \chips  FM6000 */
    fm_uint16  vlanID2;

    /** See 'MA Table Entry Types'.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_uint16  type;

    /** Mask of port numbers to send to when destination is macAddress.
     *  If set to ''FM_DESTMASK_UNUSED'', then port is used, otherwise the 
     *  specified mask is used. The usage of mask is deprecated in favor of 
     *  port and may not be used for switch aggregates.
     *                                                                  \lb\lb
     *  On FM3000 and FM4000 devices, when adding a dynamic MA Table entry 
     *  for more than one destination port, destMask must be set to 
     *  ''FM_DESTMASK_UNUSED'' and port used to specify a multicast group.
     *                                                                  \lb\lb
     *  This field is not supported on FM6000 devices and should
     *  always be set to ''FM_DESTMASK_UNUSED'' for clarity, but if any
     *  other value is used, it will be ignored.
     *
     *  \chips  FM2000, FM3000, FM4000 */
    fm_uint32  destMask;

    /** Destination logical port. On FM2000, FM3000 and FM4000 devices, this
     *  field is only used when destMask is ''FM_DESTMASK_UNUSED''. On
     *  FM6000 devices, this field is used unconditionally.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_int     port;

    /** New entries will have age set to 1.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_int     age;

    /** Remote ID. Depending of the application, this field could
     *  represent a VLAN ID store in the MAC entry that can be used
     *  in an egress VLAN TAG when a DMAC lookup occurs. Used as a
     *  tunnel ID for TRILL, VxLAN, GRE, etc.
     *  
     *  \chips  FM6000 */
    fm_uint16  remoteID;

    /** Flag to let the pipeline know that reaching this
     *  MAC will be done using TRILL enabled ports.
     *  
     *  \chips  FM6000 */
    fm_bool    remoteMac;


} fm_macAddressEntry;


/**************************************************/
/** \ingroup typeStruct
 *  Used as an argument to ''fmFlushAddresses'',
 *  specified the parameters of a MA Table flush
 *  operation. 
 **************************************************/
typedef struct _fm_flushParams
{
    /** The port associated with each MA Table entry to be flushed.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_uint    port;

    /** The VLAN (VLAN1) associated with each MA Table entry to be flushed.
     *
     *  \chips  FM2000, FM3000, FM4000, FM6000 */
    fm_uint16  vid1;

    /** The VLAN2 associated with each MA Table entry to be flushed.
     *
     *  \chips  FM6000 */
    fm_uint16  vid2;

    /** The remoteID associated with each MA Table entry to be
     *  flushed.
     *
     *  \chips  FM6000 */
    fm_uint16  remoteId;

    /** The remoteMac associated with each remoteId to be flushed.
     *
     *  \chips  FM6000 */
    fm_bool  remoteMac;

} fm_flushParams;


/* adds a new entry to the MA table */
extern fm_status fmAddAddress(fm_int sw, fm_macAddressEntry *entry);

extern fm_status fmGetAddress(fm_int              sw,
                              fm_macaddr          address,
                              fm_int              vlanID,
                              fm_macAddressEntry *entry);

extern fm_status fmGetAddressV2(fm_int              sw,
                                fm_macaddr          address,
                                fm_int              vlanID,
                                fm_int              vlanID2,
                                fm_macAddressEntry *entry);

extern fm_status fmGetAddressTableExt(fm_int              sw,
                                      fm_int *            nEntries,
                                      fm_macAddressEntry *entries,
                                      fm_int              maxEntries);


/* deletes an entry from the MA table */
extern fm_status fmDeleteAddress(fm_int sw, fm_macAddressEntry *entry);

extern fm_status fmGetAddressTable(fm_int              sw,
                                   fm_int *            nEntries,
                                   fm_macAddressEntry *entries);

extern fm_status fmDeleteAllAddresses(fm_int sw);

extern fm_status fmDeleteAllDynamicAddresses(fm_int sw);
extern fm_status fmFlushAllDynamicAddresses(fm_int sw);
extern fm_status fmFlushPortAddresses(fm_int sw, fm_uint port);
extern fm_status fmFlushVlanAddresses(fm_int sw, fm_uint vlan);
extern fm_status fmFlushPortVlanAddresses(fm_int sw, fm_uint port, fm_int vlan);
extern fm_status fmFlushAddresses(fm_int sw, fm_flushMode mode, fm_flushParams params);


extern fm_status fmGetAddressTableAttribute(fm_int sw, fm_int attr, void *value);
extern fm_status fmSetAddressTableAttribute(fm_int sw, fm_int attr, void *value);

extern fm_status fmDbgDumpMACTablePurgeStats(fm_int sw);
extern fm_status fmDbgResetMACTablePurgeStats(fm_int sw);


#endif /* __FM_FM_API_ADDR_H */
