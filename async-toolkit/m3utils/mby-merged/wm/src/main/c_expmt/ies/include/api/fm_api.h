/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api.h
 * Creation Date:   April 20, 2005
 * Description:     Wrapper to include all api files
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

#ifndef __FM_FM_API_H
#define __FM_FM_API_H


#include <api/fm_api_common.h>
#include <api/fm_api_init.h>
#include <common/fm_errno.h>
#include <api/fm_api_regs.h>
#include <api/fm_api_buffer.h>
#include <api/fm_api_packet.h>
#include <api/fm_api_routing.h>
#include <api/fm_api_event_mac_maint.h>
#include <api/fm_api_event_types.h>
#include <api/fm_api_event_mgmt.h>
#include <api/fm_api_attr.h>

#ifdef FM_SUPPORT_FM6000
/* microcode generated public attributes */
#include <api/fm_api_uc_attr.h>
#endif

#include <api/fm_api_vlan.h>
#include <api/fm_api_stp.h>
#include <api/fm_api_addr.h>
#include <api/fm_api_lag.h>
#include <api/fm_api_multicast.h>
#include <api/fm_api_storm.h>
#include <api/fm_api_acl.h>
#include <api/fm_api_mapper.h>
#include <api/fm_api_stats.h>
#include <api/fm_api_port.h>
#include <api/fm_api_mirror.h>
#include <api/fm_api_qos.h>
#include <api/fm_api_ffu.h>
#include <api/fm_api_pkt.h>
#include <api/fm_api_policer.h>
#include <api/fm_api_lbg.h>
#include <api/fm_api_swag.h>
#include <api/fm_api_stacking.h>
#include <api/fm_api_sflow.h>
#include <api/fm_api_trigger.h>
#include <api/fm_api_crm.h>
#include <api/fm_api_flow.h>
#include <api/fm_api_replication.h>
#include <api/fm_api_rbridge.h>
#include <api/fm_api_vn.h>


#endif /* __FM_FM_API_H */
