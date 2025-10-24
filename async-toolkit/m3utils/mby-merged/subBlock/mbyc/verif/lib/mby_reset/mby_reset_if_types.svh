//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_reset_if_types.svh
// Author        : Jesus Alfonso Lopez Chin <jesus.a.lopez.chin@intel.com>
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

`ifndef __MBY_RESET_TYPES__
`define __MBY_RESET_TYPES__

// -------------------------------------------------------------------------
// Main class & VIF type definitions for TAG BFM
// -------------------------------------------------------------------------
typedef struct packed {   // Copy of: subBlock/mbyc/src/shared/interfaces/mim_rd_if.sv
   logic power_good_reset;
   logic reset;
   //logic clock;
   logic egress_int_wire;
} mby_reset_t;

typedef enum  {
   RESET_IGR_MODE,
   RESET_EGR_MODE,
   RESET_NON_CTE_MODE   //This mode of operation is expected to be used out of EGR/ING models/CTE's
} mby_reset_modes;

//typedef class mby_reset_xaction;
//typedef class mby_reset_monitor;
typedef virtual mby_egr_env_if mby_egr_env_if_h;
// Defining the wr_req & rd_req/rd_rsp agents as a parameterized base agent.
//typedef mby_base_pkg::mby_base_agent#(.T_req(mby_reset_xaction), .T_vif(mby_egr_env_if_h), .T_mon(mby_reset_monitor)) mby_reset_agent;
//typedef mby_base_pkg::mby_base_agent#(.T_req(mby_reset_xaction), .T_vif(mby_egr_env_if_h)) mby_reset_agent;

`endif
