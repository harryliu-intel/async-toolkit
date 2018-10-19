/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_sdk_fm4000_int.h
 * Creation Date:   April 17, 2007
 * Description:     Wrapper file to include all needed FM4000 internal files.
 *                  For FM4000-specific SDK internal use only.
 *
 * INTEL CONFIDENTIAL
 * Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
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

#ifndef __FM_FM_SDK_FM4000_INT_H
#define __FM_FM_SDK_FM4000_INT_H

#include <fm_sdk_int.h>
#include <api/internal/fm4000/fm4000_api_hw_int.h>
#include <api/internal/fm4000/fm4000_api_regs_int.h>
#include <api/internal/fm4000/fm4000_api_lport_int.h>
#include <api/internal/fm4000/fm4000_api_routing_int.h>
#include <api/internal/fm4000/fm4000_api_mtable_int.h>
#include <api/internal/fm4000/fm4000_api_ffu_int.h>
#include <api/internal/fm4000/fm4000_api_acl_int.h>
#include <api/internal/fm4000/fm4000_api_addr_int.h>
#include <api/internal/fm4000/fm4000_api_lbg_int.h>
#include <api/internal/fm4000/fm4000_api_lag_int.h>
#include <api/internal/fm4000/fm4000_api_trigger_int.h>
#include <api/internal/fm4000/fm4000_api_stp_int.h>
#include <api/internal/fm4000/fm4000_api_addr_offload_int.h>
#include <api/internal/fm4000/fm4000_api_event_mac_maint_int.h>
#include <api/internal/fm4000/fm4000_api_qos_int.h>
#include <api/internal/fm4000/fm4000_api_regs_cache_int.h>
#include <api/internal/fm4000/fm4000_api_port_int.h>
#include <api/internal/fm4000/fm4000_api_storm_int.h>
#include <api/internal/fm4000/fm4000_api_multicast_int.h>
#include <api/internal/fm4000/fm4000_api_stacking_int.h>
#include <api/internal/fm4000/fm4000_api_sflow_int.h>
#include <api/internal/fm4000/fm4000_api_cvlan_int.h>
#include <api/internal/fm4000/fm4000_api_flow_int.h>
#include <api/internal/fm4000/fm4000_api_switch_int.h>
#include <api/internal/fm4000/fm4000_api_crm_int.h>
#include <api/internal/fm4000/fm4000_api_vlan_int.h>

#include <platforms/common/packet/generic-packet/fm_generic_packet.h>

/* fake variable to remove precompiled header warning that file doesn't
 * contain any code.
 */
extern int fake_variable_1;

#endif /* __FM_FM_SDK_FM4000_INT_H */
