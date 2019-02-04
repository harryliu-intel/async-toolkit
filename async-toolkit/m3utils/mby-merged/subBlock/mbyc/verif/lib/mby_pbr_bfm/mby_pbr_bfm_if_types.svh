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
//`ifndef __MBY_PBR_BFM_PKG__
//`error "Attempt to include file outside of mby_pbr_bfm_pkg."
//`endif
`ifndef __MBY_PBR_BFM_IF_TYPES__
`define __MBY_PBR_BFM_IF_TYPES__

// -------------------------------------------------------------------------
// Main struct type definitions for PBR BFM
// -------------------------------------------------------------------------
//localparams copyed from mby_gmm_pkg.sv
localparam MBY_SEG_PTR_WIDTH         = 20;       // 20-bit segment pointer
localparam MBY_SEG_PTR_MSB           = (MBY_SEG_PTR_WIDTH - 1);

localparam MBY_POD_64B_WORD_WIDTH    = 24;
localparam MBY_POD_64B_WORD_MSB      = (MBY_POD_64B_WORD_WIDTH - 1);

//----------------------------------------------------------------------------------
// mby_pbr_bfm_cptr_data_t
//----------------------------------------------------------------------------------
typedef struct packed { // pod_fetch prefix in diagram
   logic  data_valid; //FIXME: [2:0]
   logic [MBY_SEG_PTR_MSB:0] data_clean_ptr; //[2:0]
//   logic [1:0] data_word; //[2:0]
//   logic  data_semaph; //[2:0]
   logic       id_ack_valid;
   logic [4:0] id_ack_id;
   logic       fifo_ack;

   logic       req_valid;
   logic [4:0] req_id;
   logic [MBY_POD_64B_WORD_MSB:0] req_seg_ptr;
} mby_pbr_bfm_cptr_local_t;

//----------------------------------------------------------------------------------
// mby_pbr_bfm_dptr_local_t
//----------------------------------------------------------------------------------
typedef struct packed{
   logic [MBY_SEG_PTR_MSB:0] data_dirty_ptr; // 2x or 3x seg_ptr
   logic       pod_put_req;
   logic       pod_put_type;
   logic       pod_put_ack;
   logic       schedule_stall;
} mby_pbr_bfm_dptr_local_t;

// Defining the cptr data type to be mby_pbr_bfm_cptr_local_t(local struct) for now.
typedef mby_pbr_bfm_cptr_local_t mby_pbr_bfm_cptr_data_t;
// FIXME: change logics to structs (e.g. mby_pod_ptr_ring_t)
// Defining the ptr data type to be mby_egr_dpm_t(local struct) for now.
typedef mby_pbr_bfm_dptr_local_t mby_pbr_bfm_dptr_data_t;
// Defining the ptr debug type to be a simple logic for now.
typedef logic mby_pbr_bfm_cptr_debg_t;
// Defining the ptr debug type to be a simple logic for now.
typedef logic mby_pbr_bfm_dptr_debg_t;

`endif
