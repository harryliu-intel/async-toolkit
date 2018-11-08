//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_defines.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the TAG BFM definitions
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
`ifndef __MBY_TAG_BFM_PKG__
`error "Attempt to include file outside of mby_tag_bfm_pkg."
`endif
`ifndef __MBY_TAG_BFM_TYPES__
`define __MBY_TAG_BFM_TYPES__

// -------------------------------------------------------------------------
// Main struct type definitions for TAG BFM
// -------------------------------------------------------------------------
// Re-using the low-latency tag format from the RTL libraries, this will be
// the main data type of the transaction item used by the tag bfm.
typedef lltformat_t mby_tag_bfm_data_t;
// Defining the debug type to be a simple logic for now.
typedef logic mby_tag_bfm_debg_t;
// These are the modes of operation of the Tag BFM, variable to be included in
// the configuration object.
typedef enum bit {
   TAG_BFM_IGR_MODE,
   TAG_BFM_EGR_MODE
} mby_tag_bfm_mode_t;

// -------------------------------------------------------------------------
// Main class & VIF type definitions for TAG BFM
// -------------------------------------------------------------------------
// Creating a virtual interface type.
typedef virtual mby_tag_bfm_if mby_tag_bfm_vif;
// Forward declaration of the transaction class (in the tag_bfm_pkg this file
// is compiled before the transaction item.
typedef class mby_tag_bfm_xaction;
// Defining the tag agent as a parameterized base agent.
typedef mby_base_agent#(.T_req(mby_tag_bfm_xaction), .T_vif(mby_tag_bfm_vif)) mby_tag_bfm_agent;

`endif
