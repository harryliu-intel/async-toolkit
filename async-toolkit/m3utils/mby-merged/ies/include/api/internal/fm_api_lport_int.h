/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_lport_int.h
 * Creation Date:   2005
 * Description:     Contains constants related to logical ports.
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

#ifndef __FM_FM_API_LPORT_INT_H
#define __FM_FM_API_LPORT_INT_H

/****************************************************************************/
/** \ingroup inttypeEnum
 *
 * Identifies the type of port destination that a logical port maps to.
 ****************************************************************************/
typedef enum
{
    /** Allocate an entry for a single physical port on the local switch. */
    FM_PORT_TYPE_PHYSICAL = 0,

    /** Allocate a block of entries for a LAG. */
    FM_PORT_TYPE_LAG,

    /** Allocate an entry for multicasting. */
    FM_PORT_TYPE_MULTICAST,

    /** Allocate an entry for load balancing */
    FM_PORT_TYPE_LBG,

    /** Allocate an entry for the CPU port. */
    FM_PORT_TYPE_CPU,

    /** Allocate an entry for the CPU Mgmt (FIBM) port. */
    FM_PORT_TYPE_CPU_MGMT,

    /** Allocate an entry for a custom port type */
    FM_PORT_TYPE_SPECIAL,

    /** Allocate an entry for a remote port type */
    FM_PORT_TYPE_REMOTE,

    /* ----  Add new entries above this line.  ---- */

    /** For internal use only. */
    FM_PORT_TYPE_MAX

} fm_portType;


/**************************************************/
/** \ingroup intPort
 * Logical port specific attributes, set by calling
 * fmSetLogicalPortAttribute(...).
 **************************************************/
typedef enum
{
    /* The destination mask this logical port maps to. */
    FM_LPORT_DEST_MASK = 0,

    /* The multicast index associated with this logical port. (FM4000/FM6000) */
    FM_LPORT_MULTICAST_INDEX,

#if 0
    /* The multicast destination mask associated with this logical port.
     * (FM6000 only) */
    FM_LPORT_MULTICAST_DEST_MASK,
#endif

    /* Add new constants above this line */
    FM_LPORT_ATTR_MAX

} fm_logicalPortAttribute;


/**************************************************/
/** \ingroup intPort
 *
 * Specifies whether to update glort CAM, glort RAM,
 * or both CAM and RAM.
 **************************************************/
typedef enum
{
    FM_UPDATE_CAM_ONLY      = 1,
    FM_UPDATE_RAM_ONLY      = 2,
    FM_UPDATE_CAM_AND_RAM   = 3,

} fm_camUpdateMode;


/***************************************************
 * Constants representing boundaries and sizes for
 * the glort space.
 **************************************************/

/*
 * 0x0000 - 0x00ff  : Physical ports. 
 * The glort port size (number of destination table 
 * entries) is define t the switch level 
 */
#define FM_GLORT_PORT_BASE(base)        (base)

/*
 * 0x0100 - 0x03ff  : Link aggregate groups.
 */
#define FM_GLORTS_PER_LAG               32
#define FM_GLORT_LAG_BASE_OFFSET        0x100 
#define FM_GLORT_LAG_BASE(base)         ((base) + FM_GLORT_LAG_BASE_OFFSET)
#define FM_GLORT_LAG_SIZE               (FM_GLORTS_PER_LAG * FM_MAX_NUM_LAGS)

/*
 * 0x0400 - 0x07ff  : Multicast groups. 
 * 
 * This defines enough glorts for up to FM_MAX_NUM_MULTICAST_GROUP multicast
 * groups, not offset from LAG.
 */
#define FM_GLORT_MULTICAST_BASE(base)   (FM_GLORT_LAG_BASE(base) + FM_GLORT_LAG_SIZE)
#define FM_GLORT_MULTICAST_SIZE         FM_MAX_NUM_MULTICAST_GROUP

/**
 * 0x0800 - 0x17ff  : Load-balancing groups.
 */
#define FM_GLORT_LBG_BASE(base)         (FM_GLORT_MULTICAST_BASE(base)+ FM_GLORT_MULTICAST_SIZE)
#define FM_GLORT_LBG_SIZE               0x1000

/*
 * 0xff00 - 0xffff  : CPU glorts. 
 *  
 * All the switches will have the same CPU glort range. 
 * The number of CPU ports (number of destination table 
 * entries) is define at the switch level.
 */ 
#define FM_GLORT_CPU_BASE(base)         0xFF00

/* Custom glorts go at the end of CPU glort space. */
#define FM_GLORT_SPECIAL_BASE           0xFFF8
#define FM_GLORT_SPECIAL_MASK           0xFFF8
#define FM_GLORT_SPECIAL_SIZE           0x0008
#define FM_GLORT_SPECIAL_A_LENGTH       3

/* The maximum glort value. */
#define FM_MAX_GLORT                    0xFFFF


/***************************************************
 * Constants representing field values for the
 * GLORT_RAM table.
 **************************************************/

#define FM_GLORT_ENTRY_TYPE_STRICT      3
#define FM_GLORT_ENTRY_TYPE_HASHED      2
#define FM_GLORT_ENTRY_TYPE_ISL         0

#define FM_GLORT_ENTRY_HASH_A           0
#define FM_GLORT_ENTRY_HASH_B           1


/***************************************************
 * Describes a block of glorts as defined by the
 * RAM/CAM entry that defines it. (FM4000/FM6000)
 **************************************************/
typedef struct _fm_glortCamEntry
{
    /** Stores the index in CAM/RAM that holds this entry. */
    fm_uint32   camIndex;

    /** Indicates the mask and key to be used for searching the CAM */
    fm_uint32   camKey;
    fm_uint32   camMask;

    /** Indicates if the destination mask is hashed across the range */
    fm_uint32   strict;

    /** The first glort index in the block */
    fm_uint32   destIndex;

    /** The number of glorts in the block */
    fm_uint32   destCount;

    /** Defines a bit range in the glort number */
    fm_uint32   rangeAOffset;
    fm_uint32   rangeALength;

    /** (For strict only) defines a bit range in the glort number */
    fm_uint32   rangeBOffset;
    fm_uint32   rangeBLength;

    /** The hash rotation used for the hashing */
    fm_uint32   hashRotation;

    /** (FM6000 only) 1-bit tag associated with DGLORT. */
    fm_uint32   dglortTag;

    /** Indicates whether this entry is used */
    fm_int      useCount;

} fm_glortCamEntry;


/***************************************************
 * Describes a glort destination mask entry. (FM4000/FM6000)
 **************************************************/
typedef struct
{
    /** The actual destination index in the dest table in use. */
    fm_uint32           destIndex;

    /** The destination mask associated with this entry. */
    fm_portmask         destMask;

    /** The IP multicast index associated with this entry. */
    fm_uint32           multicastIndex;

    /** The cam entry associated with this dest entry. */
    fm_glortCamEntry *  owner;

    /** Indicates whether this entry is used, and by which port type. */
    fm_byte             usedBy;

} fm_glortDestEntry;


/**
 * Port parameters structure. 
 *  
 * Used during logical port allocation to keep track of the resources that 
 * have been assigned to the port. 
 */
typedef struct
{
    /** Type of logical port being allocated. */
    fm_portType portType;

    /** Number of logical ports to allocate. */
    fm_int      numPorts;

    /** First logical port number. */
    fm_int      basePort;

    /** First glort number. */
    fm_int      baseGlort;

    /** Index of the glort cam entry. */
    fm_uint32   camIndex;

    /** Index of the first entry in the dmask table. */
    fm_int      baseDestIndex;

    /** Number of entries in the dmask table. */
    fm_int      numDestEntries;

    /** Handle specifying a specific allocated resources.
        This is mainly used in stacking configuration. */
    fm_int      useHandle;

    /* Number of logical ports to delete when port is freed. */
    fm_int      freeCount;

} fm_portParms;

/***************************************************
 * Multicast resource definitions.
 **************************************************/
/* max number of cam entries per allocated resource */
#define FM_RESV_MCAST_CAM_MAX           64

/* max entries per cam as a power of 2 */
#define FM_MCAST_MAX_ENTRY_PER_CAM      8
#define FM_MCG_MAX_ENTRIES_PER_GLORT    (1 << FM_MCAST_MAX_ENTRY_PER_CAM)

/* max number of allocated muticast groups */
#define FM_MCG_ALLOC_TABLE_SIZE         8


/***************************************************
 * Keeps track of the resources that have been allocated
 * for multicast groups (MCGs).
 **************************************************/
typedef struct 
{
    fm_uint     baseGlort;
    /* If glortSize == 0, then this entry is invalid */
    fm_uint     glortSize;
    fm_int      baseHandle;
    fm_int      numHandles;

    /* CAM indexes assigned to this block of glorts. */
    fm_uint32   mcgCamIndex[FM_RESV_MCAST_CAM_MAX];

    /* Destination indexes assigned to this block of glorts. */
    fm_uint32   mcgDestIndex[FM_RESV_MCAST_CAM_MAX];

    /* Manages glort dest table entry usage. */
    fm_bitArray      dstInUse[FM_RESV_MCAST_CAM_MAX];

} fm_mcgAllocEntry;


/***************************************************
 * Keeps track of resources that have been allocated
 * for link aggregation groups (LAGs).
 **************************************************/
typedef struct 
{
    fm_uint     baseGlort;
    /* If glortSize == 0, then this entry is invalid */
    fm_uint     glortSize;
    fm_int      baseHandle;
    fm_int      numHandles;
    fm_int      numPorts;

} fm_lagAllocEntry;

/* max number of allocated LAGs */
#define FM_LAG_ALLOC_TABLE_SIZE         FM_ALLOC_LAGS_MAX


/***************************************************
 * Keeps track of resources that have been allocated
 * for load-balancing groups (LBGs).
 **************************************************/
typedef struct 
{
    fm_uint     baseGlort;
    /* If glortSize == 0, then this entry is invalid */
    fm_uint     glortSize;
    fm_int      baseHandle;
    fm_int      numHandles;
    fm_int      numPorts;

} fm_lbgAllocEntry;

/* max number of allocated LBGs */
#define FM_LBG_ALLOC_TABLE_SIZE         16


/***************************************************
 * State structure to keep track of the free space
 * list in the glort RAM/CAM.
 **************************************************/
typedef struct
{
    /***************************************************
     * Manages the glort CAM itself (and the associated
     * RAM).
     **************************************************/
    fm_glortCamEntry *  camEntries;
    fm_int              numCamEntries;

    /***************************************************
     * Manages the glort dest table.
     **************************************************/
    fm_glortDestEntry * destEntries;
    fm_int              numDestEntries;

    /***************************************************
     * Array to indicate whether a glort is free or
     * reserved for a particular use. We need this
     * because the glort space may be bigger than 
     * the physical glort space available.
     **************************************************/
    fm_byte             glortState[FM_MAX_GLORT+1];

    /***************************************************
     * Array to indicate whether a logical port is
     * free or reserved for a particular use.
     **************************************************/
    fm_byte             lportState[FM_MAX_LOGICAL_PORT+1];

    fm_uint32           physicalPortCamIndex;
    fm_uint32           specialPortCamIndex;
    fm_uint32           cpuPortCamIndex;
    fm_uint32           cpuMgmtPortCamIndex;
    fm_uint32           lagPortCamIndex;

    /* special logical ports */
    fm_int              floodPort;
    fm_int              floodCpuPort;
    fm_int              bcastPort;
    fm_int              dropPort;
    fm_int              rpfFailurePort;
    fm_int              cpuPort;
    fm_int              cpuMgmtPort; /* For fibm when cpuPort is redirected */

    /* Resources allocated for multicast groups (MCGs). */
    fm_mcgAllocEntry    mcgAllocTable[FM_MCG_ALLOC_TABLE_SIZE];

    /* Resources allocated for link aggregate groups (LAGs). */
    fm_lagAllocEntry    lagAllocTable[FM_LAG_ALLOC_TABLE_SIZE];

    /* Resources allocated for load-balancing groups (LBGs). */
    fm_lbgAllocEntry    lbgAllocTable[FM_LBG_ALLOC_TABLE_SIZE];

} fm_logicalPortInfo;


/*******************************************************************
 * State bits for the glortState array.
 *******************************************************************/

#define FM_GLORT_STATE_IN_USE       0x01    /* In use */
#define FM_GLORT_STATE_RESV_MCG     0x02    /* Reserved for MCG */
#define FM_GLORT_STATE_RESV_LAG     0x04    /* Reserved for LAG */
#define FM_GLORT_STATE_RESV_LBG     0x08    /* Reserved for LBG */
#define FM_GLORT_STATE_FREE_PEND    0x10    /* Free pending */

#define FM_GLORT_STATE_USED_BITS    \
        (FM_GLORT_STATE_IN_USE | FM_GLORT_STATE_FREE_PEND)


/*******************************************************************
 * Macros to manipulate the glortState array to check whether a glort
 * is reserved or in use.
 *******************************************************************/

#define FM_RELEASE_GLORT(info, glort)       \
        (info)->glortState[glort] = 0

#define FM_SET_GLORT_FREE(info, glort)      \
        (info)->glortState[glort] &= ~FM_GLORT_STATE_USED_BITS

#define FM_SET_GLORT_IN_USE(info, glort)    \
        (info)->glortState[glort] |= FM_GLORT_STATE_IN_USE

#define FM_RESERVE_GLORT_MCG(info, glort)   \
        (info)->glortState[glort] |= FM_GLORT_STATE_RESV_MCG

#define FM_RESERVE_GLORT_LAG(info, glort)   \
        (info)->glortState[glort] |= FM_GLORT_STATE_RESV_LAG

#define FM_RESERVE_GLORT_LBG(info, glort)   \
        (info)->glortState[glort] |= FM_GLORT_STATE_RESV_LBG

/* Either used or reserved */
#define FM_IS_GLORT_TAKEN(info, glort)      \
        ((info)->glortState[glort] != 0)

/* Check for both free and reserved */
#define FM_IS_GLORT_MCG_FREE(info, glort)   \
        ((info)->glortState[glort] == FM_GLORT_STATE_RESV_MCG)

#define FM_IS_GLORT_LAG_FREE(info, glort)   \
        ((info)->glortState[glort] == FM_GLORT_STATE_RESV_LAG)

#define FM_IS_GLORT_LBG_FREE(info, glort)   \
        ((info)->glortState[glort] == FM_GLORT_STATE_RESV_LBG)

/* Due to the problem of another thread is freeing the LAG, thus preventing
 * the code to free the allocated LAGs right after deleting all the LAGs
 * in the allocated groups, so we will need this so we can free the allocated
 * groups
 */
#define FM_SET_GLORT_FREE_PEND(info, glort) \
        (info)->glortState[glort] |= FM_GLORT_STATE_FREE_PEND

#define FM_IS_GLORT_FREE_PEND(info, glort)  \
        (((info)->glortState[glort] & FM_GLORT_STATE_FREE_PEND) != 0)


/*******************************************************************
 * State bits for the lportState array.
 *******************************************************************/

#define FM_LPORT_STATE_IN_USE       0x01    /* In use */
#define FM_LPORT_STATE_RESV_MCG     0x02    /* Reserved for MCG */
#define FM_LPORT_STATE_RESV_LAG     0x04    /* Reserved for LAG */
#define FM_LPORT_STATE_RESV_LBG     0x08    /* Reserved for LBG */
#define FM_LPORT_STATE_FREE_PEND    0x10    /* Free pending */


/*******************************************************************
 * Macros to manipulate the lportState array to check whether a logical
 * port is reserved or in use.
 *******************************************************************/

#define FM_RELEASE_LPORT(info, port)        \
        (info)->lportState[port] = 0

#define FM_SET_LPORT_FREE(info, port)       \
        (info)->lportState[port] &= ~(FM_LPORT_STATE_IN_USE | \
                                      FM_LPORT_STATE_FREE_PEND)

#define FM_SET_LPORT_IN_USE(info, port)     \
        (info)->lportState[port] |= FM_LPORT_STATE_IN_USE

#define FM_RESERVE_LPORT_MCG(info, port)    \
        (info)->lportState[port] |= FM_LPORT_STATE_RESV_MCG

#define FM_RESERVE_LPORT_LAG(info, port)    \
        (info)->lportState[port] |= FM_LPORT_STATE_RESV_LAG

#define FM_RESERVE_LPORT_LBG(info, port)    \
        (info)->lportState[port] |= FM_LPORT_STATE_RESV_LBG

/* Either used or reserved */
#define FM_IS_LPORT_TAKEN(info, port)       \
        ((info)->lportState[port] != 0)

/* Check for both free and reserved */
#define FM_IS_LPORT_MCG_FREE(info, port)    \
        ((info)->lportState[port] == FM_LPORT_STATE_RESV_MCG)

#define FM_IS_LPORT_LAG_FREE(info, port)    \
        ((info)->lportState[port] == FM_LPORT_STATE_RESV_LAG)

#define FM_IS_LPORT_LBG_FREE(info, port)    \
        ((info)->lportState[port] == FM_LPORT_STATE_RESV_LBG)

/* Due to the problem of another thread is freeing the LAG, thus preventing
 * the code to free the allocated LAGs right after deleting all the LAGs
 * in the allocated groups, so we will need this so we can free the allocated
 * groups
 */
#define FM_SET_LPORT_FREE_PEND(info, port)  \
        (info)->lportState[port] |= FM_LPORT_STATE_FREE_PEND

#define FM_IS_LPORT_FREE_PEND(info, port)   \
        (((info)->lportState[port] & FM_LPORT_STATE_FREE_PEND) != 0)

#define FM_GET_GLORT_NUMBER(swptr, p)       ( (swptr)->portTable[p]->glort )

#define FM_USED_BY_TYPE(type)   (type | 0x80)


/***************************************************
 * Function prototypes.
 **************************************************/

fm_status fmAllocLogicalPort(fm_int      sw,
                             fm_portType type,
                             fm_int      numPorts,
                             fm_int *    firstPortNumber);

fm_status fmFreeLogicalPort(fm_int sw, fm_int port);

fm_status fmGetLogicalPortAttribute(fm_int sw,
                                    fm_int port,
                                    fm_int attr,
                                    void * value);

fm_status fmSetLogicalPortAttribute(fm_int sw,
                                    fm_int port,
                                    fm_int attr,
                                    void * value);

fm_status fmCreateLogicalPortForGlort(fm_int    sw,
                                      fm_uint32 glort,
                                      fm_int *  logicalPort);

fm_status fmGetLogicalPortGlort(fm_int sw, fm_int logicalPort, fm_uint32 *glort);
fm_status fmGetGlortLogicalPort(fm_int sw, fm_uint32 glort, fm_int *logicalPort);

fm_bool fmIsLagPort(fm_int sw, fm_int port);
fm_bool fmIsRemotePort(fm_int sw, fm_int logicalPort);
fm_bool fmIsInternalPort(fm_int sw, fm_int logicalPort);
fm_bool fmIsMgmtPort(fm_int sw, fm_int logicalPort);
fm_bool fmIsPortLinkUp(fm_int sw, fm_int logicalPort);
fm_bool fmIsValidPort(fm_int sw, fm_int port, fm_int mode);

fm_status fmSortPortByGlort(fm_int sw,
                            fm_int *ports,
                            fm_int nPorts,
                            fm_int *sortedPorts);

fm_status fmAllocDestEntries(fm_int              sw,
                             fm_int              numDestEntries,
                             fm_glortCamEntry *  camEntry,
                             fm_glortDestEntry** destEntry,
                             fm_portType         type);

fm_status fmCreateGlortCamEntry(fm_int      sw,
                                fm_uint32   camMask,
                                fm_uint32   camKey,
                                fm_uint32   strict,
                                fm_uint32   baseIndex,
                                fm_uint32   destCount,
                                fm_uint32   rangeALength,
                                fm_uint32   rangeAOffset,
                                fm_uint32   rangeBLength,
                                fm_uint32   rangeBOffset,
                                fm_uint32   hashRotation,
                                fm_uint32   dglortTag,
                                fm_uint32 * camIndexPtr);

fm_int fmFindFreeLagEntry(fm_int sw);
fm_int fmFindFreeLbgEntry(fm_int sw);
fm_int fmFindFreeMcgEntry(fm_int sw);

fm_lagAllocEntry * fmFindLagEntryByHandle(fm_int sw, fm_int handle);
fm_lbgAllocEntry * fmFindLbgEntryByHandle(fm_int sw, fm_int handle);
fm_mcgAllocEntry * fmFindMcgEntryByHandle(fm_int sw, fm_int handle);

fm_int fmFindUnusedCamEntry(fm_int sw);

fm_int fmFindUnusedDestEntries(fm_int   sw,
                               fm_int   numEntries,
                               fm_int   first);

fm_int fmFindUnusedGlorts(fm_int            sw,
                          fm_int            numGlorts,
                          fm_int            first,
                          fm_glortCamEntry* camEntry);

fm_int fmFindUnusedLogicalPorts(fm_int sw, fm_int numPorts);

fm_status fmFindUnusedLagGlorts(fm_int      sw,
                                fm_int      numPorts,
                                fm_int      useHandle,
                                fm_int      glortsPerLag,
                                fm_int *    logicalPort,
                                fm_int *    firstGlort);

fm_status fmFindUnusedLbgGlorts(fm_int      sw,
                                fm_int      numPorts,
                                fm_int      useHandle,
                                fm_int *    logicalPort,
                                fm_int *    firstGlort);

fm_status fmFindUnusedMcgGlorts(fm_int              sw,
                                fm_int              numPorts,
                                fm_int              useHandle,
                                fm_mcgAllocEntry**  allocEntryPtr,
                                fm_uint *           offBasePtr);

const char* fmPortTypeToText(fm_portType type);

fm_status fmRemoveGlortCamEntry(fm_int sw, fm_uint32 camIndex);

void fmResetLogicalPortInfo(fm_logicalPortInfo* lportInfo);

fm_status fmVerifyGlortRange(fm_uint32 glort, fm_int size);

fm_status fmFreeMcastLogicalPort(fm_int sw, fm_int port);

fm_status fmInitializeLogicalPorts(fm_int sw);

fm_status fmAllocateLogicalPortDataStructures(fm_int sw,
                                              fm_int numCamEntries,
                                              fm_int numDestEntries);

fm_status fmFreeLogicalPortDataStructures(fm_switch* switchPtr);

fm_status fmFreeLogicalPortResources(fm_int sw);

fm_status fmCommonAllocLogicalPort(fm_int      sw,
                                   fm_portType portType,
                                   fm_int      numPorts,
                                   fm_int *    firstPort,
                                   fm_int      useHandle);

fm_status fmCommonFreeLogicalPort(fm_int sw, fm_int logicalPort);

fm_status fmAllocateMcastHandles(fm_int     sw,
                                 fm_uint    startGlort,
                                 fm_int     glortSize,
                                 fm_int    *baseMcastGroupHandle,
                                 fm_int    *numMcastGroups,
                                 fm_int    *step);

fm_status fmFreeMcastHandles(fm_int sw, fm_int handle);

fm_status fmAllocateLagHandles(fm_int   sw,
                               fm_uint  startGlort,
                               fm_int   glortSize,
                               fm_int   glortsPerLag,
                               fm_int   lagMaskSize,
                               fm_int * baseLagHandle,
                               fm_int * numLags,
                               fm_int * step);

fm_status fmFreeLagHandles(fm_int sw, fm_int handle);

fm_status fmAllocateLbgHandles(fm_int   sw,
                               fm_uint  startGlort,
                               fm_int   glortSize,
                               fm_int * baseLbgHandle,
                               fm_int * numLbgs,
                               fm_int * step);

fm_status fmFreeLbgHandles(fm_int sw, fm_int handle);



#endif /* __FM_FM_API_LPORT_INT_H */
