/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces) */
/*****************************************************************************
 * File:            hlp_api_debug_int.h
 * Creation Date:   October 8, 2008
 * Description:     Prototypes for internal HLP debug functions.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2008 - 2012 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_HLP_API_DEBUG_INT_H
#define __FM_HLP_API_DEBUG_INT_H

void hlpDbgDumpRegister(fm_int sw, fm_int port, fm_text regname);

fm_status hlpDbgDumpRegisterV2(fm_int  sw,
                                  fm_int  indexA,
                                  fm_int  indexB,
                                  fm_text regname);

fm_status hlpDbgDumpRegisterV3(fm_int  sw,
                                  fm_int  indexA,
                                  fm_int  indexB,
                                  fm_int  indexC,
                                  fm_text regname);

void hlpDbgGetRegisterName(fm_int   sw,
                              fm_int   regId,
                              fm_uint  regAddress,
                              fm_text  regName,
                              fm_uint  regNameLength,
                              fm_bool *isPort,
                              fm_int * index0Ptr,
                              fm_int * index1Ptr,
                              fm_int * index2Ptr,
                              fm_bool  logicalPorts,
                              fm_bool  partialLongRegs);

void hlpDbgWriteRegister(fm_int  sw,
                            fm_int  port,
                            fm_text regName,
                            fm_int  val);

fm_status hlpDbgWriteRegisterV2(fm_int    sw,
                                   fm_int    wordOffset,
                                   fm_int    indexA,
                                   fm_int    indexB,
                                   fm_text   regName,
                                   fm_uint32 value);
fm_status hlpDbgWriteRegisterV3(fm_int    sw,
                                   fm_int    wordOffset,
                                   fm_int    indexA,
                                   fm_int    indexB,
                                   fm_int    indexC,
                                   fm_text   regName,
                                   fm_uint32 value);

fm_status hlpDbgWriteRegisterField(fm_int    sw, 
                                      fm_int    indexA, 
                                      fm_int    indexB, 
                                      fm_int    indexC, 
                                      fm_text   regName, 
                                      fm_text   fieldName, 
                                      fm_uint64 value);

void hlpDbgListRegisters(fm_int sw, fm_bool showGlobals, fm_bool showPorts);

void hlpDbgTakeChipSnapshot(fm_int                sw,
                               fmDbgFulcrumSnapshot *pSnapshot,
                               fm_regDumpCallback    callback);

fm_status hlpDbgGetRegInfo(fm_text    registerName,
                              fm_uint32 *registerAddr,
                              fm_int *   wordCnt,
                              fm_int *   idxMax0,
                              fm_int *   idxMax1,
                              fm_int *   idxMax2,
                              fm_int *   idxStep0,
                              fm_int *   idxStep1,
                              fm_int *   idxStep2);

fm_status hlpDbgSetRegField(fm_text registerName, fm_text fieldName,
                               fm_uint32 *regValue, fm_uint64 fieldValue);

fm_status hlpDbgDumpRegField(fm_text registerName, fm_uint32 *value);

fm_status hlpDbgDumpPortMap(fm_int sw, fm_int port, fm_int portType);

#endif  /* __FM_HLP_API_DEBUG_INT_H */
