//-----------------------------------------------------------------------------
// Title         : Madison Bay GCM Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gcm_bfm_defines.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the GCM_BFM definitions
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
`ifndef __MBY_GCM_BFM_PKG__
`error "Attempt to include file outside of mby_gcm_bfm_pkg."
`endif
`ifndef __MBY_GCM_BFM_TYPES__
`define __MBY_GCM_BFM_TYPES__
// -------------------------------------------------------------------------
// Main struct type definitions for GCM BFM
// -------------------------------------------------------------------------
// This is the dequeue type defined in the gmm_pkg.
typedef mby_unicast_deque_t mby_gcm_bfm_deque_t;
// For queue the GCM snoops the tag interface and extracts src/dst info from
// there.
typedef mby_tag_ring_t mby_gcm_bfm_queue_t;
// These are the watermark types for the GCM (tx/rx) watermarks
typedef mby_cm_rx_wm_t mby_gcm_bfm_rx_wm_t;
typedef mby_cm_tx_wm_t mby_gcm_bfm_tx_wm_t;
// These are the shared watermark types for the GCM.
typedef mby_cm_shared_mem_rx_wm_t mby_gcm_bfm_sm_rx_wm_t;
typedef mby_cm_shared_mem_tx_wm_t mby_gcm_bfm_sm_tx_wm_t;
// These are the modes of operation of the GCM BFM, variable to be included in
// the configuration object.
typedef enum bit {
   GCM_BFM_IGR_MODE,
   GCM_BFM_EGR_MODE
} mby_gcm_bfm_mode_t;

// -------------------------------------------------------------------------
// Main class & VIF type definitions for GCM BFM
// -------------------------------------------------------------------------
// Creating a virtual interface types for the GCM.
typedef virtual mby_gcm_bfm_if mby_gcm_bfm_queue_vif;
typedef virtual mby_gcm_bfm_if mby_gcm_bfm_deque_vif;
typedef virtual mby_gcm_bfm_if mby_gcm_bfm_rx_wmark_vif;
typedef virtual mby_gcm_bfm_if mby_gcm_bfm_tx_wmark_vif;
typedef virtual mby_gcm_bfm_if mby_gcm_bfm_smem_rx_wmark_vif;
typedef virtual mby_gcm_bfm_if mby_gcm_bfm_smem_tx_wmark_vif;

// Forward declaration of the transaction class.
typedef class mby_gcm_bfm_xaction;

// Defining the GCM agents as parameterized base agent classes.
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_gcm_bfm_xaction),
   .T_vif(mby_gcm_bfm_queue_vif))         gcm_queue_bfm_agent;

typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_gcm_bfm_xaction),
   .T_vif(mby_gcm_bfm_deque_vif))         gcm_deque_bfm_agent;

typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_gcm_bfm_xaction),
   .T_vif(mby_gcm_bfm_tx_wmark_vif))      gcm_tx_wm_bfm_agent;

typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_gcm_bfm_xaction),
   .T_vif(mby_gcm_bfm_rx_wmark_vif))      gcm_rx_wm_bfm_agent;

typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_gcm_bfm_xaction),
   .T_vif(mby_gcm_bfm_smem_tx_wmark_vif)) gcm_tx_smem_wm_bfm_agent;

typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_gcm_bfm_xaction),
   .T_vif(mby_gcm_bfm_smem_rx_wmark_vif)) gcm_rx_smem_wm_bfm_agent;

`endif

