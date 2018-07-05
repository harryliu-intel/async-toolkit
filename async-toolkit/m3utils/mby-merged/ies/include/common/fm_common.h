/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_common.h
 * Creation Date:   2005
 * Description:     Wrapper to include all common header files
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

#ifndef __FM_FM_COMMON_H
#define __FM_FM_COMMON_H


#define FM_DISABLED  FALSE
#define FM_ENABLED   TRUE


/**********************************************************************
 * This file is the primary include file for the externally-exposed
 * "common" portions of the Fulcrum ControlPoint SDK.
 *
 * Internal dependencies inside this subsystem should be documented here.
 * The only external requirements allowed here are the architectural
 * definitions.
 * Any order-dependencies amongst these files must be clearly documented
 * here.
 * As of 04/17/2007, there were no order-dependencies amongst the files
 * referenced here.
 * As of 11/13/2007, fm_dlist.h depends on fmFreeFunc, which is defined
 * in fm_tree.h.
 **********************************************************************/

#include <common/fm_errno.h>
#include <common/fm_tree.h>
#include <common/fm_dlist.h>
#include <common/fm_bitarray.h>
#include <common/fm_bitfield.h>
#include <common/fm_crc32.h>
#include <common/fm_plusargs.h>
#include <common/fm_attr.h>
#include <common/fm_string.h>
#include <common/fm_md5.h>
#include <common/fm_lock_prec.h>
#include <common/fm_array.h>
#include <common/fm_graycode.h>
#include <common/fm_c11_annex_k.h>


/** \ingroup macroSystem
 *  Macro used to suppress compiler warnings when a function parameter is
 *  deliberately not used in a function. */
#define FM_NOT_USED(a) (void) (a)

/** \ingroup macroSystem
 *  Macro used to zero an array or structure. */
#define FM_CLEAR(object) \
    FM_MEMSET_S( &(object), sizeof(object), 0, sizeof(object) )

/* The absolute difference between two values. */
#define FM_DELTA(a, b)  ( (a) > (b) ? (a) - (b) : (b) - (a) )

/* Combines two error codes. */
#define FM_ERR_COMBINE(oldErr, newErr) \
    if ((oldErr) == FM_OK)             \
    {                                  \
        (oldErr) = (newErr);           \
    }

/* The number of entries of a statically allocated array. */
#define FM_NENTRIES(x)  ( sizeof((x)) / sizeof((x)[0]) )

/* A typedef'd union to allow portable conversion of integers to pointers,
 * etc., designed to work in both 32-bit and 64-bit systems. */
typedef union
{
    fm_int    int32[2];
    fm_uint32 uint32[2];
    fm_int64  int64;
    fm_uint64 uint64;
    void *    ptr;

} fm_sizeConverter;

#ifdef __LITTLE_ENDIAN__
#define FM_SIZECONV_PUT_INT32(sc, i) { FM_CLEAR(sc); sc.int32[0] = i; }
#define FM_SIZECONV_GET_INT32(sc) sc.int32[0]

#define FM_SIZECONV_PUT_UINT32(sc, u) { FM_CLEAR(sc); sc.uint32[0] = u; }
#define FM_SIZECONV_GET_UINT32(sc) sc.uint32[0]
#else
#define FM_SIZECONV_PUT_INT32(sc, i) { FM_CLEAR(sc); sc.int32[1] = i; }
#define FM_SIZECONV_GET_INT32(sc) sc.int32[1]

#define FM_SIZECONV_PUT_UINT32(sc, u) { FM_CLEAR(sc); sc.uint32[1] = u; }
#define FM_SIZECONV_GET_UINT32(sc) sc.uint32[1]
#endif

#define FM_SIZECONV_PUT_INT64(sc, i) { FM_CLEAR(sc); sc.int64 = i; }
#define FM_SIZECONV_GET_INT64(sc) sc.int64

#define FM_SIZECONV_PUT_UINT64(sc, u) { FM_CLEAR(sc); sc.uint64 = u; }
#define FM_SIZECONV_GET_UINT64(sc) sc.uint64

#define FM_SIZECONV_PUT_PTR(sc, p) { FM_CLEAR(sc); sc.ptr = p; }
#define FM_SIZECONV_GET_PTR(sc) sc.ptr

#endif /* __FM_FM_COMMON_H */
