//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model Defines
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_defines.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the SMM BFM definitions
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
`ifndef __MBY_SMM_BFM_DEFINES__
`define __MBY_SMM_BFM_DEFINES__
// Definitions & local parameters
parameter  NUM_PLANES            = 2;
parameter  NUM_MSH_ROWS          = 3;      // number of mesh rows //TODO : update based on plusargs
parameter  NUM_MSH_COLS          = 3;      // number of mesh columns 
parameter  W_SEG_PTR             = 20;     // Segment Pointer Size
parameter  W_SEMA                = 4;      // Semaphore Bits Size
parameter  W_WD_SEL              = 2;      // Segment Word Selector Size
parameter  W_REQ_ID              = 16;     // Write/Read Request ID Width TODO: verify it
parameter  MSH_DATA_WIDTH        = 64 * 8; // width of mesh data
parameter  NUM_MSH_ROW_PORTS     = 3;      // number of mesh ports per row on each side (east and west)
parameter  NUM_MSH_COL_PORTS_WR  = 3;      // number of mesh ports per row on each side (east and west)
parameter  NUM_MSH_COL_PORTS_R   = 4;      // number of mesh ports per row on each side (east and west)
parameter  W_RRSP_DEST_BLOCK     = 3;      // Read Response Destination Block Width
parameter  ADDR_WIDTH		 = 14;	   // Address width

`endif
