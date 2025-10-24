// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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

// Definitions & local parameters as per https://securewiki.ith.intel.com/display/25T/MBY+FS+Memory+Mesh
parameter  SMM_BFM_NUM_PLANES          = 2;     // Number of plane instances           (Section: Interface)
parameter  SMM_BFM_NUM_MSH_ROWS        = 4;     // Size in bits of mesh row index      (Table 2: op structure )
parameter  SMM_BFM_NUM_MSH_COLS        = 3;     // Size in bits of mesh column index   (Table 2: op structure )
parameter  SMM_BFM_W_SEG_PTR           = 20;    // Segment Pointer Size                (Table 0: port_op structure )
parameter  SMM_BFM_W_SEMA              = 4;     // Semaphore Bits Size                 (Table 0: port_op structure ) FIXME ; Table 0 states the size in bits to 1
parameter  SMM_BFM_W_WD_SEL            = 2;     // Segment Word Selector Size          (Table 0: port_op structure )
parameter  SMM_BFM_W_REQ_ID            = 16;    // Write/Read Request ID Width         (Table 2: op structure )
parameter  SMM_BFM_DATA_WIDTH          = 512;   // Width of mesh data                  (Section: Data)
parameter  SMM_BFM_W_RRSP_DEST_BLOCK   = 3;     // Read Response Destination Block Width  TODO : Add reference to spec
parameter  SMM_BFM_ADDR_WIDTH          = 14;    // Address width                       (Table 2: op structure )

// TODO : These aren't used in SMM_BFM (yet), not sure where they come from, probably from Table 2: op structure 
//parameter  NUM_MSH_ROW_PORTS     = 3;      // number of mesh ports per row on each side (east and west)
//parameter  NUM_MSH_COL_PORTS_WR  = 3;      // number of mesh ports per row on each side (east and west)
//parameter  NUM_MSH_COL_PORTS_R   = 4;      // number of mesh ports per row on each side (east and west)

`endif
