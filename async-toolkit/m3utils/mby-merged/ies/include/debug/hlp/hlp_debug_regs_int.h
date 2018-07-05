/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            hlp_debug_regs_int.h
 * Creation Date:   April 27, 2013
 * Description:     Header file for auto-generated debug register data.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2009 - 2013 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_HLP_DEBUG_REGS_INT_H
#define __FM_HLP_DEBUG_REGS_INT_H


/**************************************************
 * Register table entry
 **************************************************/

typedef struct
{
    fm_text     regname;
    fm_int      accessMethod;
    fm_uint32   regAddr;
    fm_byte     flags;
    fm_byte     statGroup;
    fm_int      wordcount;
    fm_int      indexMin0;
    fm_int      indexMin1;
    fm_int      indexMin2;
    fm_int      indexMax0;
    fm_int      indexMax1;
    fm_int      indexMax2;
    fm_int      indexStep0;
    fm_int      indexStep1;
    fm_int      indexStep2;

} hlpDbgFulcrumRegister;


/**************************************************
 * Register access methods.
 **************************************************/

enum
{
    /* Scalar register */
    SCALAR = 0,

    /* Multi-word register. */
    MULTIWRD,

    /* Indexed register. */
    INDEXED,

    /* Multi-word indexed register. */
    MWINDEX,

    /* Doubly indexed register. */
    DBLINDEX,

    /* Multi-word doubly indexed register. */
    MWDBLIDX,

    /* Triply indexed register. */
    TPLINDEX,

    /* Multi-word triply indexed register. */
    MWTPLIDX,

    /* Composite: all registers for port. */
    ALL4PORT,

    /* Group Registers */
    GROUPREG,

    /* All configuration registers */
    ALLCONFG,

    /* Special debug pseudo-registers (not taken from the design) */
    SPECIAL

};


/**************************************************
 * Flag bit masks.
 **************************************************/
 
#define REG_FLAG_INDEX0_PER_PORT        0x01
#define REG_FLAG_INDEX1_PER_PORT        0x02
#define REG_FLAG_INDEX0_PER_EPL         0x04
#define REG_FLAG_INDEX0_CHAN_1_EPL      0x08
#define REG_FLAG_PER_PORT               (REG_FLAG_INDEX0_PER_PORT | \
                                         REG_FLAG_INDEX1_PER_PORT | \
                                         REG_FLAG_INDEX0_CHAN_1_EPL)
#define REG_FLAG_STATISTIC              0x10
#define REG_FLAG_END_OF_REGS            0x80


/**********************************************************************
 * Register table
 **********************************************************************/

extern const hlpDbgFulcrumRegister hlpRegisterTable[];

extern const fm_int hlpRegisterTableSize;


/**************************************************
 * Register field entry.
 **************************************************/

typedef struct
{
    fm_text     name;  /* field name */
    fm_int      start; /* position in bits starting from 0-indexed LSB */
    fm_int      size;  /* length in bits */
} fmRegisterField;


/**********************************************************************
 * Register field lookup
 **********************************************************************/

const fmRegisterField * hlpDbgGetRegisterFields(fm_text registerName);


#endif  /* __FM_HLP_DEBUG_REGS_INT_H */

