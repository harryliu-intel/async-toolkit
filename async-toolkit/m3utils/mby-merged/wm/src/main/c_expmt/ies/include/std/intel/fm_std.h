/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_std.h
 * Creation Date:   2005
 * Description:     Architecture-specific definitions for Intel processors.
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

#ifndef __FM_FM_STD_H
#define __FM_FM_STD_H


/*****************************************************************************
 * Basic type definitions
 *
 * Maintenance note: When adding new types, update @stdTypes array in
 * doc/build_doc.pl with two entries, one for the new type and one for
 * a pointer to the new type.
 *****************************************************************************/

/* Doxygen control: Group the typedefs below. */
/** \ingroup typeStd
 * @{ */

/** Unsigned 8-bit character. */
typedef unsigned char         fm_byte;

/** The standard signed integer type of the platform.
 *  Intended to be the most efficient word size available
 *  on the platform without regard to the range of values
 *  it can hold. */
typedef int                   fm_int;

/** The equivalent intptr_t for c99 extension.
 *  Intended to be the same size as pointer type
 *  so casting between pointer and integer can be
 *  done without warnings. */
typedef long                   fm_intptr;

/** Signed 16-bit word. */
typedef short                 fm_int16;

/** Signed 32-bit word. */
typedef int                   fm_int32;

/** Signed 64-bit word. */
typedef long long             fm_int64;

/** The standard unsigned integer type of the platform.
 *  Intended to be the most efficient word size available
 *  on the platform without regard to the range of values
 *  it can hold. */
typedef unsigned int          fm_uint;

/** The equivalent uintptr_t for c99 extension.
 *  Intended to be the same size as pointer type
 *  so casting between pointer and integer can be
 *  done without warnings. */
#ifdef _FM_ARCH_x86_64
typedef unsigned long long    fm_uintptr;
#else
typedef unsigned long         fm_uintptr;
#endif

/** Unsigned 16-bit word. */
typedef unsigned short        fm_uint16;

/** Unsigned 32-bit word. */
typedef unsigned int          fm_uint32;

/** Unsigned 64-bit word. */
typedef unsigned long long    fm_uint64;

/** MAC address - a 64-bit word with only the low order
 *  48 bits actually used. */
typedef unsigned long long    fm_macaddr;

/** Boolean */
typedef unsigned char         fm_bool;

/** A text character */
typedef char                  fm_char;

/** A pointer to a string of text characters. */
typedef char                 *fm_text;

/** A floating point value */
typedef double                fm_float;

/** A void pointer */
typedef void                 *fm_voidptr;

/** @} (end of Doxygen group) */


/** \ingroup macroSystem
 *  Macro to ensure portability of 64-bit literal quantities, typically
 *  used with fm_int64. */
#define FM_LITERAL_64(x)        (x ## LL)

/** \ingroup macroSystem
 *  Macro to ensure portability of 64-bit literal quantities, typically
 *  used with fm_uint64. */
#define FM_LITERAL_U64(x)       (x ## ULL)

/*  Macro to ensure portability of 64-bit literal quantities, typically
 *  used in printf format strings. */
#define FM_FORMAT_64         "ll"


/* define true/false if needed */
#ifndef TRUE
#define TRUE                 1
#endif
#ifndef FALSE
#define FALSE                0
#endif

#define FM_TICKS_PER_SECOND  1000

/* helper macros */

#define FM_READ_BIG16(ptr)      * ( (fm_uint16 *) (ptr) )
#define FM_READ_BIG32(ptr)      * ( (fm_uint32 *) (ptr) )
#define FM_READ_BIG64(ptr)      * ( (fm_uint64 *) (ptr) )

#define FM_WRITE_BIG16(ptr, v)  * ( (fm_uint16 *) (ptr) ) = (v)
#define FM_WRITE_BIG32(ptr, v)  * ( (fm_uint32 *) (ptr) ) = (v)
#define FM_WRITE_BIG64(ptr, v)  * ( (fm_uint64 *) (ptr) ) = (v)

#define FM_READ_BIGMAC(ptr) \
    (*( (fm_uint64 *) (ptr) ) >> 16)

#define FM_WRITE_BIGMAC(ptr, v) \
    (*( (fm_uint64 *) (ptr) ) = ( ( (v) & 0xffffffffffffL ) << 16 )


#endif /* __FM_FM_STD_H */
