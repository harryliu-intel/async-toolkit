
// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
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
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

`timescale 1ps/1fs
//  Module:    mby_mc_tb_top
//
//  MBY MC No Processor, No PHY Testbench Top module.
//  This file only contains instantiation/configuration which are
//  specific to mplex_np_nphy model.


module mby_mc_tb_top ();

    `include "mby_mc_tb_top_common.svh"
    `include "mby_mc_np_conn.svh"
    

    //-----------------------------------------------------------------------------
    // Verification Test Island
    //-----------------------------------------------------------------------------
    mby_mc_ti #(
        .TOPOLOGY(mby_mc_env_pkg::mby_mc_defines::MPLEX_NP_NPHY)
    ) mc_ti(
        .mby_mc_tb_if               (mc_tb_if),
	.ahb_if                     (ahb_if),
	.ahb_reset_if               (ahb_reset_if),
        .axi_if                     (axi_if),
        .axi_reset_if               (axi_reset_if),
        .shdv_intf                  (shdv_intf)

    );

    initial begin

    end

endmodule: mby_mc_tb_top

