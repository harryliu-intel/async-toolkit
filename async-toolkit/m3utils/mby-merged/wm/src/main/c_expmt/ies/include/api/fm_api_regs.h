/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_regs.h
 * Creation Date:   April 20, 2005
 * Description:     Basic register access functions
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

#ifndef __FM_FM_API_REGS_H
#define __FM_FM_API_REGS_H

/**************************************************/
/** \ingroup typeStruct
 * fm_scatterGatherListEntry defines a single
 * contiguous range of register space to read or
 * write.  An array of these structures can be
 * passed to the platform-defined functions pointed
 * to by the ''ReadScatterGather'' or 
 * ''WriteScatterGather'' function pointers to read 
 * or write multiple non-contiguous regions
 * in a single function call.  Use of these functions
 * can greatly improve the performance of access to 
 * FIBM-controlled remote switches.
 **************************************************/
typedef struct _fm_scatterGatherListEntry
{
    /** Starting register address to read/write. */
    fm_uint32  addr;

    /** Number of words to read/write. */
    fm_uint32  count;

    /** Array of count words, that the read data is written into
     *  or the write data is taken from. The array must be
     *  at least count words long. */
    fm_uint32 *data;

} fm_scatterGatherListEntry;



/* reads a single 32-bit integer */
fm_status fmReadUINT32(fm_int sw, fm_uint reg, fm_uint32 *ptr);


/* reads a single un-cached 32-bit integer */
fm_status fmReadUncachedUINT32(fm_int sw, fm_uint reg, fm_uint32 *ptr);


/* reads multiple consecutive 32-bit integers */
fm_status fmReadUINT32Mult(fm_int sw, fm_uint reg, fm_int wordCount, fm_uint32 *ptr);


/* reads multiple un-cached consecutive 32 bit integers */
fm_status fmReadUncachedUINT32Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint32 *ptr);


/* reads a single 64-bit integer */
fm_status fmReadUINT64(fm_int sw, fm_uint reg, fm_uint64 *ptr);


/* reads a un-cached 64-bit integer */
fm_status fmReadUncachedUINT64(fm_int sw, fm_uint reg, fm_uint64 *ptr);


/* reads multiple consecutive 64-bit integers */
fm_status fmReadUINT64Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint64 *ptr);


/* reads multiple consecutive un-cached 64-bit integers */
fm_status fmReadUncachedUINT64Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint64 *ptr);


/* read multiple discontiguous regions */
fm_status fmReadScatterGather(fm_int                           sw,
                              fm_int                           nEntries,
                              fm_scatterGatherListEntry *      sgList);


/* writes a single 32-bit integer */
fm_status fmWriteUINT32(fm_int sw, fm_uint reg, fm_uint32 value);


/* writes a field in a single 32-bit register */
fm_status fmWriteUINT32Field(fm_int sw,
                             fm_int addr,
                             fm_int startingBit,
                             fm_int length,
                             fm_int value);


/* writes multiple consecutive 32-bit integers */
fm_status fmWriteUINT32Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint32 *ptr);


/* Sets or clears a bit mask in a 32-bit integer. */
fm_status fmMaskUINT32(fm_int sw, fm_uint reg, fm_uint32 mask, fm_bool on);


/* writes a single 64-bit integer */
fm_status fmWriteUINT64(fm_int sw, fm_uint reg, fm_uint64 value);


/* writes multiple consecutive 64-bit integers */
fm_status fmWriteUINT64Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint64 *ptr);


/* write multiple discontiguous regions */
fm_status fmWriteScatterGather(fm_int                           sw,
                               fm_int                           nEntries,
                               fm_scatterGatherListEntry *      sgList);


/* writes a single 32-bit integer through I2C */
fm_status fmI2cWriteUINT32(fm_int sw, fm_uint reg, fm_uint32 value);


/* reads a single 32-bit integer through I2C */
fm_status fmI2cReadUINT32(fm_int sw, fm_uint reg, fm_uint32 *value);


/* writes multiple consecutive 32-bit integers through I2C */
fm_status fmI2cWriteUINT32Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint32 *ptr);


/* reads multiple consecutive 32-bit integers through I2C */
fm_status fmI2cReadUINT32Mult(fm_int sw, fm_uint reg, fm_int n, fm_uint32 *value);


#endif /* __FM_FM_API_REGS_H */
