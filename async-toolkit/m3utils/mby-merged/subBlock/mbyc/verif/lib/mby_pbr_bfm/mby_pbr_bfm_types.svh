//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_defines.sv
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.19.2018
//-----------------------------------------------------------------------------
// Description :
// These are the PBR_BFM definitions
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
`ifndef __MBY_PBR_BFM_PKG__
`error "Attempt to include file outside of mby_pbr_bfm_pkg."
`endif
`ifndef __MBY_PBR_BFM_TYPES__
`define __MBY_PBR_BFM_TYPES__

// -------------------------------------------------------------------------
// Main struct type definitions for PBR BFM
// -------------------------------------------------------------------------

// These are the modes of operation of the PBR BFM, variable to be included in
// the configuration object.
typedef enum bit {
    PBR_BFM_IGR_MODE,
    PBR_BFM_EGR_MODE
} mby_pbr_bfm_mode_t;

// -------------------------------------------------------------------------
// Main class & VIF type definitions for TAG BFM
// -------------------------------------------------------------------------
// Creating the virtual interface types for agents.
typedef virtual mby_pbr_bfm_cptr_if mby_pbr_bfm_cptr_vif;
typedef virtual mby_pbr_bfm_dptr_if mby_pbr_bfm_dptr_vif;
// Forward declaration of the transaction classes (in the bfm_pkg this
// file (bfm_types) is compiled before the transaction items.
typedef class mby_pbr_bfm_cptr_xaction;
typedef class mby_pbr_bfm_dptr_xaction;


typedef shdv_base_io_policy_param#(
   .T_req(mby_pbr_bfm_cptr_xaction),
   .T_vif(mby_pbr_bfm_cptr_vif)) mby_ptr_bfm_cptr_io;

typedef shdv_base_io_policy_param#(
   .T_req(mby_pbr_bfm_dptr_xaction),
   .T_vif(mby_pbr_bfm_dptr_vif)) mby_ptr_bfm_dptr_io;
// Defining the flow control policy for the tag bfm

typedef shdv_base_empty_fc_policy mby_ptr_bfm_cptr_fc;
typedef shdv_base_empty_fc_policy mby_ptr_bfm_dptr_fc;



// Defining the dpb agent as a parameterized base agent.
typedef mby_base_pkg::mby_base_agent#(
    .T_req(mby_pbr_bfm_dptr_xaction),
    .T_iop(mby_ptr_bfm_dptr_io),
    .T_fcp(mby_ptr_bfm_dptr_fc)) mby_pbr_bfm_dpb_agent;
// Defining the dpm agent as a parameterized base agent.
typedef mby_base_pkg::mby_base_agent#(
    .T_req(mby_pbr_bfm_dptr_xaction),
    .T_iop(mby_ptr_bfm_dptr_io),
    .T_fcp(mby_ptr_bfm_dptr_fc)) mby_pbr_bfm_dpm_agent;
// Defining the csp agent as a parameterized base agent.
typedef mby_base_pkg::mby_base_agent#(
    .T_req(mby_pbr_bfm_cptr_xaction),
    .T_fcp(mby_ptr_bfm_cptr_fc),
    .T_iop(mby_ptr_bfm_cptr_io)) mby_pbr_bfm_csp_agent;
// Defining the cpb agent as a parameterized base agent.
typedef mby_base_pkg::mby_base_agent#(
    .T_req(mby_pbr_bfm_cptr_xaction),
    .T_fcp(mby_ptr_bfm_cptr_fc),
    .T_iop(mby_ptr_bfm_cptr_io)) mby_pbr_bfm_cpb_agent;
`endif
