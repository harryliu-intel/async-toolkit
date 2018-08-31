/* vim:ts=4:sw=4:expandtab
 * (No tabs, indent level is 4 spaces)  */
/*****************************************************************************
 * File:            fm_api_int.h
 * Creation Date:   2005
 * Description:     Contains structure definitions for various table entries
 *                  and switch state information
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

#ifndef __FM_FM_API_INT_H
#define __FM_FM_API_INT_H


/* Common definitions */
#include <api/internal/fm_api_common_int.h>
#include <api/internal/fm_api_portmask.h>
#include <api/internal/fm_api_lag_int.h>
#include <api/internal/fm_api_lport_int.h>

/* Internal definitions required by fm_api_switch_int.h */
#include <api/internal/fm_api_addr_int.h>
#include <api/internal/fm_api_cardinal_int.h>
#include <api/internal/fm_api_event_mac_maint_int.h>
#include <api/internal/fm_api_vlan_int.h>
#include <api/internal/fm_api_stp_int.h>
#include <api/internal/fm_api_routing_int.h>
#include <api/internal/fm_api_mcast_groups_int.h>
#include <api/internal/fm_api_lbg_int.h>
#include <api/internal/fm_api_events_int.h>
#include <api/internal/fm_api_stacking_int.h>
#include <api/internal/fm_api_fibm_int.h>
#include <api/internal/fm_api_acl_int.h>
#include <api/internal/fm_api_mirror_int.h>
#include <api/internal/fm_api_stat_int.h>
#include <api/internal/fm_api_vn_int.h>

/* Switch and port Generic Definitions */
#include <api/internal/fm_api_switch_int.h>
#include <api/internal/fm_api_port_int.h>

/* All other Internal Definitions */
#include <api/internal/fm_api_init_int.h>
#include <api/internal/fm_api_port_int.h>
#include <api/internal/fm_api_qos_int.h>

#ifdef FM_SUPPORT_SWAG
#include <api/internal/fm_api_swag_int.h>
#endif

#include <api/internal/fm_api_regs_cache_int.h>
#include <api/internal/fm_api_root_int.h>

#endif /* __FM_FM_API_INT_H */
