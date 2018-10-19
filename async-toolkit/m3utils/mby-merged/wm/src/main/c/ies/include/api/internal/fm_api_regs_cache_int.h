/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:            fm_api_regs_cache_int.h
 * Creation Date:   April 12, 2010
 * Description:     Header containing all declarations related to the
 *                  Register Cache implementation
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
#ifndef __FM_FM_API_REGS_CACHE_INT_H
#define __FM_FM_API_REGS_CACHE_INT_H

/** Definitions related to indexing into the cached registers */
#define FM_REGS_CACHE_NO_INDEX_USED          0
#define FM_REGS_CACHE_ONE_INDEX_USED         1
#define FM_REGS_CACHE_TWO_INDICES_USED       2
#define FM_REGS_CACHE_THREE_INDICES_USED     3
#define FM_REGS_CACHE_MAX_INDICES            FM_REGS_CACHE_THREE_INDICES_USED
#define FM_REGS_CACHE_HIGHEST_INDEX          FM_REGS_CACHE_MAX_INDICES - 1
#define FM_REGS_CACHE_INDEX_UNUSED           0


/***********************************************************/ 
/** definition of a set of methods the pointer to manage
 *  the cache for a given set of registers 
 ***********************************************************/
typedef struct _fm_regCacheMethods
{
    /** returns a pointer to the actual local cache
     *  NULL if the register is not cached */
    fm_uint32   *(*data) (fm_int physicalSw);

    /** returns a pointer to a bit array of valid bits, one per entry 
     *  NULL if the register does not need a cache of valid bits */
    fm_bitArray *(*valid)(fm_int physicalSw);

    /** returns the default 32-bit value of content of a register
     *  at a given address */
    fm_uint32    (*defaults) (fm_uint32 addr);

} fm_regCacheMethods;

/************************************************************/
/** Definition of the cached register descriptor structure. 
 *  Each cached register is listed in an compile-time,
 *  chipset-specific array named fmx000CachedRegisterList,
 *  where fmx0000 is the name of the chip 
 ************************************************************/ 
typedef struct _fm_cachedRegs
{
    /** Pointer to a function returning the location of the cache
     *  for this register set  */
    fm_regCacheMethods getCache;

    /** Base address of the register set */
    fm_uint32          baseAddr;

    /** width of the register in terms of number of words */
    fm_byte            nWords;

    /** number of indices needed to access a register in the set */
    fm_byte            nIndices;

    /** is this register to be treated atomically?   */
    fm_bool            isAtomic;

    /** incremental step in the addres space in each dimension */
    fm_uint32          stride[FM_REGS_CACHE_MAX_INDICES];

    /**  number of elements in the set in each dimension */
    fm_uint32          nElements[FM_REGS_CACHE_MAX_INDICES];

} fm_cachedRegs;



/******************************************************************/
/** Definition of the high-level scatter-gather structure 
 *  It is used rvery time an attempt is made to access one or more 
 *  cached registers in a register set. Its purpose is to combine 
 *  the access to multiple contiguous registers into a 
 *  single read/write operation 
 ******************************************************************/
typedef struct _fm_registerSGListEntry
{
    /** pointer to the register set descriptor */
    const fm_cachedRegs *registerSet;

    /** pointer to the buffer to read from/write to */
    fm_uint32 *          data;

    /** number of registers (not words) to be read/written */
    fm_uint32            count; 

    /** index of the register in its set */
    fm_uint32            idx[FM_REGS_CACHE_MAX_INDICES];

    /***************************************************/
    /** This boolean must be set to TRUE if this SG list
     *  entry is writing to a location which has already
     *  been written to by a previous entry in this same
     *  SG list.  This is because we skip writes which
     *  are writing the value which is already in the cache,
     *  but this comparison is done before any of the SG
     *  list entries are executed.  So, if we have an
     *  SG list which has one entry that sets a register
     *  to a new value, and a later entry that sets
     *  the register back to the old value, the later
     *  entry will be suppressed, but the earlier entry
     *  won't, thus leaving the register with the wrong
     *  value in the end.  (Bug 11560)  So, this flag
     *  indicates this is the second write to the same
     *  location (used for "live" updates) and we should
     *  perform the write regardless of what appears to
     *  currently be in the cache.
     **************************************************/
    fm_bool rewriting;              

} fm_registerSGListEntry;


/******************************************************************/
/** This enum is used by all methods that allow to read/write the 
 *  keyValid local cache bit array. Each value  represents one of 
 *  the possible combinations of the reference bit for key and
 *  keyInvert. The reference bit is the one chosen to manage the
 *  valid status of the CAM entry. Currently that is Bit[0:0] 
 *  That is valid for chipsets using the key/keyInvert approach (Alta
 *  and presumably later chipsets). 
 ******************************************************************/
typedef enum
{
    /** Bit0 of key and keyInvert are '0', i.e. the key was never
     *  used */
    FM_REGS_CACHE_KEY_INVALID = 0, 

    /** Bit0 is '1' for Key and '0' for KeyInvert when the key is
     *  valid */
    FM_REGS_CACHE_KEY_IS_1,

    /** Bit0 is '0' for Key and '1' for KeyInvert when the key is
     *  valid */
    FM_REGS_CACHE_KEYINVERT_IS_1,

    /** Bit0 of both Key and KeyInvert are '1', i.e. Bit[0:0] is
     * not used for lookups when the key is valid */ 
    FM_REGS_CACHE_KEY_AND_KEYINVERT_BOTH_1

} fm_regsCacheKeyValid;

/**************************************************
 * Definition of macros that allow to fill out the 
 * cached registers scatter-gather list
 **************************************************/

/**
 * Macro to fill out the registerSet field in a 
 * scatter-gather list for a given register set
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       regs is the pointer to the register set
 *  
 */
#define FM_REGS_CACHE_FILL_SGLIST_REGS(sglist, regs) \
        (sglist)->registerSet = (regs)


/**
 * Macro to fill out the count field in a scatter-gather list 
 * for a given register set 
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       cnt is the number of registers to be read or 
 *                  written
 *  
 */
#define FM_REGS_CACHE_FILL_SGLIST_COUNT(sglist, cnt) \
        (sglist)->count = (cnt)


/**
 * Macro to fill out the idx[0] field in a scatter-gather list 
 * for a given register set 
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       idx0 is the value of index idx[0] 
 *                  written
 */
#define FM_REGS_CACHE_FILL_SGLIST_IDX0(sglist, idx0) \
        (sglist)->idx[0]      = (idx0)


/**
 * Macro to fill out the idx[1] field in a scatter-gather list 
 * for a given register set 
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       idx1 is the value of index idx[1] 
 *                  written
 */
#define FM_REGS_CACHE_FILL_SGLIST_IDX1(sglist, idx1) \
        (sglist)->idx[1]      = (idx1)


/**
 * Macro to fill out the idx[2] field in a scatter-gather list 
 * for a given register set 
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       idx2 is the value of index idx[2] 
 *                  written
 */
#define FM_REGS_CACHE_FILL_SGLIST_IDX2(sglist, idx2) \
        (sglist)->idx[2]      = (idx2)


/**
 * Macro to fill out the data field in a scatter-gather list for
 * a given register set 
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       dataPtr is a pointer to the user provided data 
 *                  buffer to be written to or to read from
 */
#define FM_REGS_CACHE_FILL_SGLIST_DATA(sglist, dataPtr) \
        (sglist)->data        = (dataPtr)


/**
 * Macro to fill out the data field in a scatter-gather list for
 * a given register set 
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       rewrite is a boolean value of the rewriting 
 *                  field in the scatter-gather structure
  */
#define FM_REGS_CACHE_FILL_SGLIST_REWRITING(sglist, rewrite) \
        (sglist)->rewriting   = (rewrite)


/**
 * Macro to fill out the scatter-gather list for a given 
 * register set
 *  
 * \param[out]      sglist is a pointer to the scatter-gather list 
 *                  to be filled out
 *  
 * \param[in]       regs is the pointer to the register set
 *  
 * \param[in]       cnt is the number of registers to be read or 
 *                  written
 *  
 * \param[in]       i0 is the value of index idx[0] 
 *  
 * \param[in]       i1 is the value of index idx[1]
 *  
 * \param[in]       i2 is the value of index idx[2] 
 *  
 * \param[in]       data is a pointer to the user provided data 
 *                  buffer to be written to or to read from
 *  
 * \param[in]       rewriting is a boolean value of the rewriting 
 *                  field in the scatter-gather structure
 *  
 */

#define FM_REGS_CACHE_FILL_SGLIST(sglist, regs, cnt, i0, i1, i2, data, rewriting) \
{                                                            \
    FM_REGS_CACHE_FILL_SGLIST_REGS((sglist), (regs));      \
    FM_REGS_CACHE_FILL_SGLIST_COUNT((sglist), (cnt));       \
    FM_REGS_CACHE_FILL_SGLIST_IDX0((sglist), (i0));        \
    FM_REGS_CACHE_FILL_SGLIST_IDX1((sglist), (i1));        \
    FM_REGS_CACHE_FILL_SGLIST_IDX2((sglist), (i2));        \
    FM_REGS_CACHE_FILL_SGLIST_DATA((sglist), (data));      \
    FM_REGS_CACHE_FILL_SGLIST_REWRITING((sglist), (rewriting)); \
}


/* Declaration of the prototypes of all Register Cache API functions */
extern fm_status fmInitRegisterCache(fm_int physicalSw);
extern fm_status fmFreeRegisterCache(fm_int physicalSw);

extern fm_status fmRegCacheRead( fm_int                        physicalSw,
                                 fm_int                        nEntries,
                                 const fm_registerSGListEntry *sgList,
                                 fm_bool                       useCache);

extern fm_status fmRegCacheWrite( fm_int                        physicalSw,
                                  fm_int                        nEntries,
                                  const fm_registerSGListEntry *sgList,
                                  fm_bool                       useCache);

extern fm_status fmRegCacheReadSingle1D( fm_int               physicalSw,
                                         const fm_cachedRegs *regSet,
                                         fm_uint32           *data,
                                         fm_uint32            idx,
                                         fm_bool              useCache);

extern fm_status fmRegCacheWriteSingle1D( fm_int               physicalSw,
                                          const fm_cachedRegs *regSet,
                                          const fm_uint32     *data,
                                          fm_uint32            idx,
                                          fm_bool              useCache);

extern fm_status fmRegCacheUpdateSingle1D ( fm_int               sw,
                                            const fm_cachedRegs *regSet,     
                                            fm_uint32            data, 
                                            fm_uint32            idx );


extern fm_status fmRegCacheReadKeyValid ( fm_int physicalSw,
                                          const fm_cachedRegs  *regSet,
                                          fm_uint32            *idx,
                                          fm_regsCacheKeyValid *valid );

extern fm_status fmRegCacheWriteKeyValid ( fm_int physicalSw,             
                                           const fm_cachedRegs *regSet,   
                                           fm_uint32           *idx,     
                                           fm_regsCacheKeyValid valid ); 

extern fm_status fmRegCacheRestoreKeyValid( fm_int physicalSw,
                                            const fm_cachedRegs  *regSet,
                                            fm_uint32            *idx,
                                            fm_uint32            *key,
                                            fm_uint32            *keyInvert );

extern fm_status fmRegCacheCopyKeyValid ( fm_int physicalSw,
                                          const fm_cachedRegs  *regSet,
                                          fm_uint32            *fromIdx,
                                          fm_uint32            *toIdx );

extern fm_status fmRegCacheIsAddrRangeCached ( fm_int sw, 
                                               fm_uint32 lowAddr, 
                                               fm_uint32 hiAddr,
                                               fm_bool   *cached );
#endif /* __FM_FM_API_REGS_CACHE_INT_H */ 

