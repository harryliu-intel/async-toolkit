/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_qos.h
 * Creation Date:   May 26, 2005
 * Description:     Contains functions dealing with the QOS settings,
 *                  i.e. watermarks, priority maps, etc.
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

#ifndef __FM_FM_API_QOS_H
#define __FM_FM_API_QOS_H


/* functions to set and get port specific QOS parameters */
fm_status fmSetPortQOS(fm_int sw,
                       fm_int port,
                       fm_int attr,
                       fm_int index,
                       void * value);
fm_status fmGetPortQOS(fm_int sw,
                       fm_int port,
                       fm_int attr,
                       fm_int index,
                       void * value);


/* functions to set and get global switch level QOS parameters */
fm_status fmSetSwitchQOS(fm_int sw, fm_int attr, fm_int index, void *value);
fm_status fmGetSwitchQOS(fm_int sw, fm_int attr, fm_int index, void *value);
fm_status fmGetMemoryUsage(fm_int  sw,
                           fm_int  port,
                           fm_int  partition,
                           fm_int *globalUsage,
                           fm_int *partUsage,
                           fm_int *rxUsage,
                           fm_int *rxPartUsage,
                           fm_int *txUsage,
                           fm_int *txPartUsage,
                           fm_int *txPerClassUsage);
fm_status fmAddQDM(fm_int sw, fm_int port, fm_int tc, fm_int weight, fm_int cnt);
fm_status fmDelQDM(fm_int sw, fm_int port, fm_int tc);
fm_status fmResetQDM(fm_int sw, fm_int port, fm_int tc);
fm_status fmGetQDM(fm_int sw, fm_int port, fm_int tc, fm_int *delay);


#endif /* __FM_FM_API_QOS_H */
