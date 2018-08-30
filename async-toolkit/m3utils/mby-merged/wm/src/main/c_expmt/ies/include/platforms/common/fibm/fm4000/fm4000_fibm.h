/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm4000_fibm.h
 * Creation Date:   May 22, 2008
 * Description:     Internal definitions related to FIBM
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

#ifndef __FM_FM4000_FIBM_H
#define __FM_FM4000_FIBM_H


fm_status fm4000FibmEnableMasterSwitch(fm_int sw, fm_bool enable);
fm_status fm4000FibmEnableSlaveSwitch(fm_int slaveSw, fm_bool enable);

fm_status fm4000FibmAddSlaveSwitch(fm_int sw);
fm_status fm4000FibmRemoveSlaveSwitch(fm_int sw);

fm_status fm4000FibmCreateFwdRuleMasterSwitch(fm_int masterSw,
                                              fm_int fibmPort,
                                              fm_int slaveSw,
                                              fm_int slaveGlortBase,
                                              fm_int slaveGlortMask,
                                              fm_int *fwdRuleId,
                                              fm_int *fwdRuleCnt);
fm_status fm4000FibmCreateFwdRuleSlaveSwitch(fm_int slaveSw,
                                             fm_int slaveFibmPort,
                                             fm_int masterSw,
                                             fm_int masterGlortBase,
                                             fm_int masterGlortMask,
                                             fm_int *fwdRuleId,
                                             fm_int *fwdRuleCnt);

fm_int fm4000GetMaxFibmCmdLength(fm_int addr);


/* Only for bootstrap use */
fm_status fm4000FibmBootstrapSw(fm_int sw, fm_int fibmPort, fm_int portSpeed,
                                fm_uint32 fibmGlort, fm_uint32 fibmDstGlort,
                                fm_bool postIntr);

fm_status fm4000FibmDumpHwPort(fm_int sw, fm_int logPort);

fm_status fm4000SetFibmSlaveEepromConfig(fm_int sw, 
                                         fm_int fibmPort, 
                                         fm_uint32 fibmGlort, 
                                         fm_uint32 fibmDstGlort);
fm_status fm4000SetFibmSlaveEepromPath(fm_int localSw, 
                                       fm_int offset, 
                                       fm_uint32 fibmGlort, 
                                       fm_int forwardingPort);
fm_status fm4000FibmApplyNewConfig(fm_int sw);

#endif /* __FM_FM4000_FIBM_H */
