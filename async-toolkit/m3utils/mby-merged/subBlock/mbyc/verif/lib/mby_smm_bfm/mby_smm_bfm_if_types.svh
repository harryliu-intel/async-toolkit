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
//`ifndef __MBY_SMM_BFM_PKG__
//`error "Attempt to include file outside of mby_smm_bfm_pkg."
//`endif
`ifndef __MBY_SMM_BFM_IF_TYPES__
`define __MBY_SMM_BFM_IF_TYPES__

// -------------------------------------------------------------------------
// Main struct type definitions for SMM BFM
// -------------------------------------------------------------------------
// Re-using the defined types from the RTL pkg libraries: subBlock/mbyc/src/shared/rtl/mby_msh_pkg.sv  
// and taking local definitions from SMM BFM defines: subBlock/mbyc/verif/lib/mby_smm_bfm/mby_smm_bfm_defines.svh
// The struct format defined here is a copy from the Mesh West Side Interface from: subBlock/mbyc/src/msh/rtl/mby_msh.sv 

// TODO : types defined for SMM BFM aren't included yet at this point, need to later replace these global
//        parameters with the local ones
typedef struct packed {  // Copy of: subBlock/mbyc/src/shared/interfaces/mim_wr_if.sv
  logic                       mim_wreq_valid;
  logic [W_SEG_PTR-1:0]       mim_wr_seg_ptr; //[19:0]
  logic [W_SEMA-1:0]          mim_wr_sema;    //[ 3:0]
  logic [W_WD_SEL-1:0]        mim_wr_wd_sel;  //[ 2:0]
  logic [W_REQ_ID-1:0]        mim_wreq_id;    //[12:0]
  logic [W_WORD_BITS-1:0]     mim_wr_data;    // 64*8

  logic [W_XACT_CREDITS-1:0]  mim_wreq_credits; // temp value
} mby_smm_bfm_row_wr_req_t;

typedef struct packed {   // Copy of: subBlock/mbyc/src/shared/interfaces/mim_rd_if.sv
  logic                       mim_rreq_valid;
  logic [W_SEG_PTR-1:0]       mim_seg_ptr;      //[19:0]
  logic [W_SEMA-1:0]          mim_sema;         //[ 3:0]
  logic [W_WD_SEL-1:0]        mim_wd_sel;       //[ 2:0]
  logic [W_REQ_ID-1:0]        mim_req_id;       //[12:0]

  logic [W_XACT_CREDITS-1:0]   mim_rreq_credits; // temp value

  logic                         mim_rrsp_valid;
  logic [W_RRSP_DEST_BLOCK-1:0] mim_rrsp_dest_block;  //[2:0]
  logic [W_REQ_ID-1:0]          mim_rrsp_req_id;  //[12:0]
  logic [W_WORD_BITS-1:0]       mim_rd_data;      //64 x 8
} mby_smm_bfm_row_rd_req_t;

// Defining the debug types to be simple logic for now.
typedef logic mby_smm_bfm_row_wr_req_debg_t;
typedef logic mby_smm_bfm_row_rd_req_debg_t;

`endif
