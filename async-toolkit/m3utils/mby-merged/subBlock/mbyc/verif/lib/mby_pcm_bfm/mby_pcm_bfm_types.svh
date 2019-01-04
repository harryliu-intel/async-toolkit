//-----------------------------------------------------------------------------
// Title         : Madison Bay PCM Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pcm_bfm_types.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the PCM_BFM definitions
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
`ifndef __MBY_PCM_BFM_PKG__
`error "Attempt to include file outside of mby_pcm_bfm_pkg."
`endif
`ifndef __MBY_PCM_BFM_TYPES__
`define __MBY_PCM_BFM_TYPES__
// -------------------------------------------------------------------------
// Main struct type definitions for PCM BFM
// -------------------------------------------------------------------------
// This is the dequeue type defined in the gmm_pkg.
typedef mby_unicast_deque_t mby_pcm_bfm_deque_t;
// For queue the PCM snoops the tag interface and extracts src/dst info from
// there.
typedef mby_tag_ring_t mby_pcm_bfm_queue_t;
// These are the watermark types for the PCM (tx/rx) watermarks
typedef mby_cm_rx_wm_t mby_pcm_bfm_rx_wm_t;
typedef mby_cm_tx_wm_t mby_pcm_bfm_tx_wm_t; // TODO: is this needed?
// These are the shared watermark types for the PCM.
typedef mby_cm_shared_mem_rx_wm_t mby_pcm_bfm_sm_rx_wm_t;
typedef mby_cm_shared_mem_tx_wm_t mby_pcm_bfm_sm_tx_wm_t;
// This is the policer type for the PCM. //TODO:   is this the correct type?  
typedef mby_gpol_state_bcast_t mby_pcm_bfm_plcr_t;
// Simple logic for debug for now
typedef logic mby_pcm_bfm_debg_t;

// These are the modes of operation of the PCM BFM, variable to be included in
// the configuration object.
typedef enum bit {
   PCM_BFM_IGR_MODE,
   PCM_BFM_EGR_MODE
} mby_pcm_bfm_mode_t;

// -------------------------------------------------------------------------
// Main class & VIF type definitions for PCM BFM
// -------------------------------------------------------------------------
// Creating a virtual interface types for the PCM.
typedef virtual mby_pcm_bfm_queue_if    mby_pcm_bfm_queue_vif;
typedef virtual mby_pcm_bfm_deque_if    mby_pcm_bfm_deque_vif;
typedef virtual mby_pcm_bfm_rx_wm_if    mby_pcm_bfm_rx_wmark_vif;
typedef virtual mby_pcm_bfm_sm_rx_wm_if mby_pcm_bfm_smem_rx_wmark_vif;
typedef virtual mby_pcm_bfm_sm_tx_wm_if mby_pcm_bfm_smem_tx_wmark_vif;
typedef virtual mby_pcm_bfm_plcr_if     mby_pcm_bfm_plcr_vif;

// Forward declaration of the transaction classes.
typedef class mby_pcm_bfm_queue_xaction;
typedef class mby_pcm_bfm_deque_xaction;
typedef class mby_pcm_bfm_sm_wm_xaction;
typedef class mby_pcm_bfm_wm_xaction;
typedef class mby_pcm_bfm_plcr_xaction;

// Defining the PCM agents as parameterized base agent classes.
// (1) The PCM queue agent, used in IGR mode
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_pcm_bfm_queue_xaction),
   .T_vif(mby_pcm_bfm_queue_vif))         pcm_queue_bfm_agent;
// (2) The PCM dequeue agent, used in EGR mode
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_pcm_bfm_deque_xaction),
   .T_vif(mby_pcm_bfm_deque_vif))         pcm_deque_bfm_agent;
// (3) The PCM rx watermark agent, used in IGR mode
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_pcm_bfm_wm_xaction),
   .T_vif(mby_pcm_bfm_rx_wmark_vif))      pcm_rx_wm_bfm_agent;
// (4) The PCM rx shared memory watermark agent used in IGR mode
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_pcm_bfm_sm_wm_xaction),
   .T_vif(mby_pcm_bfm_smem_rx_wmark_vif)) pcm_rx_smem_wm_bfm_agent;
// (5) The PCM tx shared memory watermark agent used in EGR mode
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_pcm_bfm_sm_wm_xaction),
   .T_vif(mby_pcm_bfm_smem_tx_wmark_vif)) pcm_tx_smem_wm_bfm_agent;
// (6) The PCM policer agent used in IGR mode
typedef mby_base_pkg::mby_base_agent#(
   .T_req(mby_pcm_bfm_plcr_xaction),
   .T_vif(mby_pcm_bfm_plcr_vif)) pcm_plcr_bfm_agent;

`endif

