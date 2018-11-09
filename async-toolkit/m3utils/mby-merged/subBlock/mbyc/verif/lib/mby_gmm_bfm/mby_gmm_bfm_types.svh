//-----------------------------------------------------------------------------
// Title         : Madison Bay GMM Bus Functional Model Types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gmm_bfm_defines.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the GMM_BFM definitions
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
`ifndef __MBY_GMM_BFM_PKG__
`error "Attempt to include file outside of mby_gmm_bfm_pkg."
`endif
`ifndef __MBY_GMM_BFM_TYPES__
`define __MBY_GMM_BFM_TYPES__

// Type definitions
typedef virtual mby_gmm_bfm_if mby_gmm_bfm_vif;
typedef mby_base_pkg::mby_base_agent#(.T_req(mby_gmm_bfm_xaction), .T_vif(mby_gmm_bfm_vif)) gmm_bfm_agent;
   

`endif
