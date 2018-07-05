/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_stat_int.h
 * Creation Date:   2005
 * Description:     Structures and functions for dealing with counters
 *                  (statistics)
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

#ifndef __FM_FM_API_STAT_INT_H
#define __FM_FM_API_STAT_INT_H


#define FM_UNALLOCATED_VLAN_COUNTER  -1

/** Reads the specified VLAN counter register and assigns the value to var
 * in the 'counters' structure.
 *
 * Depends on the existence of the variables:
 * sw - switch number.
 * vcid - counter set ID
 * ci - fm_counterInfo pointer.
 */
#define FM_ASSIGN_VLAN_COUNTER_REG(reg, var)                                     \
    if ( ( err = switchPtr->ReadUINT64(sw,                                       \
                                       reg(vcid, 0),                             \
                                       &ci->lastReadVlan[vcid].var) ) != FM_OK ) \
    {                                                                            \
     goto ABORT;                                                                 \
    }                                                                            \
    counters->var = ci->lastReadVlan[vcid].var - ci->subtractVlan[vcid].var;


/** Add the read to the scatter gather list
 * As 64 bit is more common than 32 bit this lacks a bit size suffix.
 *
 * Depends on the existence of the variables:
 * lastReadPort [in] - fm_portCounters 
 * ci - fm_counterInfo pointer.
 * sgList [out] - scatter gather array to hold the reads
 * sgListCnt [out] - number of entry in the sgList
 */
#define FM_GET_PORT_STAT(reg, var)  \
    sgList[sgListCnt].addr = reg; \
    sgList[sgListCnt].data = (fm_uint32*)&lastReadPort.var; \
    sgList[sgListCnt].count = 2; \
    sgListCnt++;

/** Add the read to the scatter gather list for 32-bit read
 *
 * Depends on the existence of the variables:
 * lastReadPort [in] - fm_portCounters 
 * ci - fm_counterInfo pointer.
 * sgList [out] - scatter gather array to hold the reads
 * sgListCnt [out] - number of entry in the sgList
 */
#define FM_GET_PORT_STAT32(reg, var)  \
    sgList[sgListCnt].addr = reg; \
    sgList[sgListCnt].data = (fm_uint32*)&lastReadPort.var; \
    sgList[sgListCnt].count = 1; \
    sgListCnt++;


/** Swap the result, if enabled, and assigns the value to var in
 * the 'counters' structure.
 * As 64 bit is more common than 32 bit this lacks a bit size suffix.
 *
 * Depends on the existence of the variables:
 * lastReadPort [in] - fm_portCounters 
 * ci - fm_counterInfo pointer.
 * cpi [in] - cardinal port index.
 * swap64 [in] - if swapping is needed
 */
#define FM_UPDATE_PORT_STAT(reg, var)                                       \
    if (swap64)                                                             \
    {                                                                       \
        fm_uint32 temp32;                                                   \
        temp32 = *(fm_uint32*)&lastReadPort.var;                            \
        ci->lastReadPort[cpi].var = *(((fm_uint32*)&lastReadPort.var)+1);   \
        ci->lastReadPort[cpi].var <<= 32;                                   \
        ci->lastReadPort[cpi].var |= temp32;                                \
    }                                                                       \
    else                                                                    \
    {                                                                       \
        ci->lastReadPort[cpi].var = lastReadPort.var;                       \
    }                                                                       \
    FM_SET_PORT_STAT(var)

/** Assigns the value to var in the 'counters' structure.
 *
 * Depends on the existence of the variables:
 * lastReadPort [in] - fm_portCounters 
 * ci [out] - fm_counterInfo pointer. 
 * cpi [in] - cardinal port index.
 */
#define FM_UPDATE_PORT_STAT32(reg, var)     \
    ci->lastReadPort[cpi].var = *(fm_uint32*)&lastReadPort.var;    \
    FM_SET_PORT_STAT(var)

/**
 * Assigns the specified value to a 64-bit stat counter.
 *
 * @param var   Stats field to be updated.
 * @param val   Value to be assigned to the stats field.
 *
 * The 'lastReadPort' structure contains the current values of the stats
 * registers. The 'subtractPort' structure contains the values of the stat 
 * registers the last time fmResetPortCounters was called. We must 
 * maintain this relationship without changing 'subtractPort'. 
 *  
 * To achieve this result:
 *      lastReadPort - subtractPort = C
 *  
 * We must establish this precondition:
 *      lastReadPort = C + subtractPort 
 *
 * Depends on the existence of the variables:
 *  counters [out] - fm_portCounters pointer.
 *  ci [out] - fm_counterInfo pointer.
 *  cpi [in] - cardinal port index.
 */
#define FM_FORCE_PORT_STAT(var, val)    \
    ci->lastReadPort[cpi].var = (val) + ci->subtractPort[cpi].var; \
    FM_SET_PORT_STAT(var)

/**
 * Sets the specified field in the counters structure to the value of the
 * underlying stat register.
 *
 * @param var   Stats field to be updated.
 *
 * The 'lastReadPort' structure contains the current values of the stats
 * registers.
 *
 * The 'subtractPort' structure contains the values of the stat registers
 * the last time fmResetPortCounters was called.
 *
 * The difference between corresponding fields in the two structures is
 * the number of counts since the last reset.
 *
 * This macro depends on the existence of the variables:
 *  counters [out] - fm_portCounters pointer.
 *  ci [in] - fm_counterInfo pointer.
 *  cpi [in] - cardinal port index.
 */
#define FM_SET_PORT_STAT(var) \
    counters->var = ci->lastReadPort[cpi].var - ci->subtractPort[cpi].var

/* holds shared state about counters */
typedef struct
{
    /* Number of port counters - 1. */
    fm_int            portCount;
    fm_int            vlanCount;

    /* The state of the counters when they were last read.
     * Referenced by logical port number (FM2000) or cardinal port index. */
    fm_portCounters * lastReadPort;

    /* The state of the counters when they were last reset.
     * Referenced by logical port number (FM2000) or cardinal port index. */
    fm_portCounters * subtractPort;

    fm_switchCounters lastReadSwitch;
    fm_switchCounters subtractSwitch;

    fm_vlanCounters * lastReadVlan;
    fm_vlanCounters * subtractVlan;

    fm_int *          vlanAssignedToCounter;

} fm_counterInfo;


fm_status fmAllocateCounterDataStructures(fm_switch *swState);
fm_status fmInitCounters(fm_counterInfo *ci);
fm_status fmFreeCounterDataStructures(fm_switch *swState);


#endif /* __FM_FM_API_STAT_INT_H */
