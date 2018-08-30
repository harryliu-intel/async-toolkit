/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:           fm_alos_alloc.h
 * Creation Date:  June 04, 2007
 * Description:    Platform-independent memory allocation.
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

#ifndef __FM_FM_ALOS_ALLOC_H
#define __FM_FM_ALOS_ALLOC_H

/**************************************************/
/** \ingroup typeScalar
 * Since global variables are local to a specific
 * process, all global state needs to be located in
 * structures instantiated in shared memory.
 * The ''fmGetRoot'' API service allows one process
 * to discover the global structures created by another
 * process.  ''fmGetRoot'' takes a function pointer
 * of type fm_getDataRootHandler as an argument. The
 * indicated function is expected to create the root if
 * it does not already exist when ''fmGetRoot'' is
 * called.
 *                                                                      \lb\lb
 * This function returns an fm_status (See ''Status Codes'')
 * and takes no arguments.
 **************************************************/
typedef fm_status (*fm_getDataRootHandler)(void);


/** A legacy synonym for ''fmDbgDumpAllocStats''. 
 *  \ingroup macroSynonym */
#define fmPrintAllocationStatistics fmDbgDumpAllocStats


/* Public functions */
void *fmAllocWrap(fm_uint size, const char *file, const int line);
void fmFreeWrap(void *obj, const char *file, const int line);

#define fmAlloc(z) fmAllocWrap((z), __FILE__, __LINE__)
#define fmFree(z)  fmFreeWrap ((z), __FILE__, __LINE__)

void *fmAllocP(fm_uint size); /* use when you need a function pointer */
void fmFreeP(void *obj);     /* use when you need a function pointer */

fm_status fmGetRoot(const char *          rootName,
                    void **               rootPtr,
                    fm_getDataRootHandler rootFunc);
fm_status fmGetAvailableSharedVirtualBaseAddress(void **ptr);
fm_status fmIsMasterProcess(fm_bool *isMaster);


/***************************************************
 * Private functions
 **************************************************/
fm_status fmMemInitialize(void);


/***************************************************
 * Debug functions
 **************************************************/
void fmPrintAllocationStatistics(void);
void fmGetAllocatedMemorySize(fm_uint32 *allocMemory);
void fmDbgDumpAllocCallStacks(fm_uint bufSize);
void fmDebugAccountingDump(void);

#endif /* __FM_FM_ALOS_ALLOC_H */
