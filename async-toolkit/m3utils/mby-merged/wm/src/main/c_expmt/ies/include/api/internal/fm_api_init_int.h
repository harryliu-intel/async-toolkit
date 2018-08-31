/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_init_int.h
 * Creation Date:   2005
 * Description:     Contains non-exposed functions for initializing and
 *                  booting the chip
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

#ifndef __FM_FM_API_INIT_INT_H
#define __FM_FM_API_INIT_INT_H

/* write one of the shadow fusebox entries */
fm_status fmSetShadowFuseboxEntry(fm_int sw, fm_int entry, fm_uint32 val);

fm_status fmLockSwitch(fm_int sw);
fm_status fmHandleSwitchInserted(fm_int sw, fm_eventSwitchInserted *insertEvent);
fm_status fmHandleSwitchRemoved(fm_int sw, fm_eventSwitchRemoved *removeEvent);
fm_status fmGetSwitchInfoInternal(fm_int sw, fm_switchInfo *info);


#endif /* __FM_FM_API_INIT_INT_H */
