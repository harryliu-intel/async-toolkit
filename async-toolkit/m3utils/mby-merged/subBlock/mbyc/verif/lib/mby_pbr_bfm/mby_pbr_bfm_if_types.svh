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
// pod_fetch_data
//----------------------------------------------------------------------------------
typedef struct {
   logic [2:0] valid;
   logic [2:0] clean_ptr[MBY_SEG_PTR_MSB:0];
   logic [2:0] word[1:0];
   logic [2:0] semaph;
} mby_pod_fetch_data_t;
//----------------------------------------------------------------------------------
// pod_fetch_ID_ack
//----------------------------------------------------------------------------------
typedef struct {
   logic valid;
   logic id[4:0];
} mby_pod_fetch_id_ack_t;
//----------------------------------------------------------------------------------
// pod_fetch_fifo_ack
//----------------------------------------------------------------------------------
typedef struct {
   logic valid;
   logic id[4:0];
} mby_pod_fetch_fifo_ack_t;
//----------------------------------------------------------------------------------
// pod_fetch_req
//----------------------------------------------------------------------------------
typedef struct {
   logic valid;
   logic id[4:0];
   logic seg_ptr[MBY_POD_64B_WORD_MSB:0];
} mby_pod_fetch_req_t;

//----------------------------------------------------------------------------------
// mby_pbr_bfm_cptr_data_t
//----------------------------------------------------------------------------------
typedef struct {
   mby_pod_fetch_data_t     data;
   mby_pod_fetch_id_ack_t   id_ack;
   mby_pod_fetch_fifo_ack_t fifo_ack;
   mby_pod_fetch_req_t      req;
} mby_pod_fetch_t;

//******************
//----------------------------------------------------------------------------------
// pod_fetch_data
//----------------------------------------------------------------------------------
typedef struct {
   logic [2:0] valid;
   logic [2:0] dirty_ptr[MBY_SEG_PTR_MSB:0]; //6x seg_ptr?  2x or 3x
   
   logic       pod_put_req;
   logic       put_type;
    
   logic       pod_put_ack;
   logic       schedule_stall;
} mby_egr_dpm_t;

// Defining the cptr data type to be mby_pod_fetch_t(local struct) for now.
typedef mby_pod_fetch_t mby_pbr_bfm_cptr_data_t;
// FIXME: change logics to structs (e.g. mby_pod_ptr_ring_t)
// Defining the ptr data type to be mby_egr_dpm_t(local struct) for now.
typedef mby_egr_dpm_t mby_pbr_bfm_dptr_data_t;
// Defining the ptr debug type to be a simple logic for now.
typedef logic mby_pbr_bfm_cptr_debg_t;
// Defining the ptr debug type to be a simple logic for now.
typedef logic mby_pbr_bfm_dptr_debg_t;

`endif
