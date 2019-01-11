//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_defines.sv
// Author        : Roman Bernal <r.bernal@intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the SMM_BFM definitions
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
`ifndef __MBY_SMM_BFM_PKG__
`error "Attempt to include file outside of mby_smm_bfm_pkg."
`endif
`ifndef __MBY_SMM_BFM_TYPES__
`define __MBY_SMM_BFM_TYPES__

// -------------------------------------------------------------------------
// Main class & VIF type definitions for TAG BFM
// -------------------------------------------------------------------------
// Creating a virtual interface types for wr_req & rd_req/rd_rsp
typedef virtual mby_smm_bfm_row_wr_req_if mby_smm_bfm_row_wr_req_vif;
typedef virtual mby_smm_bfm_row_rd_req_if mby_smm_bfm_row_rd_req_vif;

// Forward declaration of the transaction classes (in the smm_bfm_pkg
// these file are compiled before the transaction item.
typedef class mby_smm_bfm_row_wr_req_xaction;
typedef class mby_smm_bfm_row_rd_req_xaction;

// Defining the wr_req & rd_req/rd_rsp agents as a parameterized base agent.
typedef mby_base_pkg::mby_base_agent#(.T_req(mby_smm_bfm_row_wr_req_xaction), .T_vif(mby_smm_bfm_row_wr_req_vif)) smm_bfm_row_wr_req_agent;
typedef mby_base_pkg::mby_base_agent#(.T_req(mby_smm_bfm_row_rd_req_xaction), .T_vif(mby_smm_bfm_row_rd_req_vif)) smm_bfm_row_rd_req_agent;

typedef class mby_smm_bfm_mwr_req;
typedef class mby_smm_bfm_mrd_req;
typedef class mby_smm_bfm_mem_node;
typedef class mby_smm_bfm_rdrsp_seq;
typedef mby_smm_bfm_rdrsp_seq#(.T_req(mby_smm_bfm_row_rd_req_xaction)) smm_bfm_rdrsp_seq;
typedef mby_smm_bfm_mwr_req#(.T_req(mby_smm_bfm_row_wr_req_xaction)) smm_bfm_mwr_req;
typedef mby_smm_bfm_mrd_req#(.T_req(mby_smm_bfm_row_rd_req_xaction)) smm_bfm_mrd_req;
typedef mby_smm_bfm_mem_node#(.ADDR_WIDTH(MSH_NODE_ADDR_WIDTH),.DATA_WIDTH(MSH_DATA_WIDTH)) smm_bfm_mem_node;

typedef enum logic [3:0] {
   NO_CONGESTION = 4'h0,
   LOW_CONGESTION = 4'h1,
   MEDIUM_CONGESTION = 4'h2,
   HIGH_CONGESTION = 4'h3,
   INSANE_CONGESTION = 4'h4
} mesh_status_type_t;

typedef enum logic [3:0] {
   IDEAL = 4'h0,
   LOW_DELAY = 4'h1,
   MEDIUM_DELAY = 4'h2,
   HIGH_DELAY = 4'h3,
   INSANE_DELAY = 4'h4
} delay_type_t;

`endif
