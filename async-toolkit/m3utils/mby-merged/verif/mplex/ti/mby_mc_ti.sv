
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
//  Author        : Akshay Kotian
//  Project       : Madison Bay
//  Description   : Mplex Test Island.This module will hold all the shared TB content between the IP and
//                  the integration level.
//------------------------------------------------------------------------------


module mby_mc_ti #( parameter string   RTL_TOP_PATH = "",             // The RTL path to the top level EC IP RTL Block
        parameter string   TB_ENV_PATH = "uvm_test_top.env",          // The hierarchy path to the environment class
        parameter mby_mc_env_pkg::mby_mc_defines::mc_topology_e TOPOLOGY =  mby_mc_env_pkg::mby_mc_defines::MPLEX_NP_NPHY
    )
    (
        mby_mc_tb_if  mby_mc_tb_if,

        svt_ahb_if    ahb_if,
     
        ahb_reset_if  ahb_reset_if,

        svt_axi_if    axi_if,

        axi_reset_if  axi_reset_if,

        shdv_base_tb_intf shdv_intf
    );

    import uvm_pkg::*;
    import sla_pkg::*;

    initial begin
        // Set MC TI Path in the database
        uvm_config_db#(string)::set(null, TB_ENV_PATH, "TI_PATH", $sformatf("%m"));

        // Set MC RTL_TOP Path in the database
        uvm_config_db#(string)::set(null, TB_ENV_PATH, "RTL_TOP_PATH", RTL_TOP_PATH);

        //Set MC TB Topology
        uvm_config_db#(int)::set(null, TB_ENV_PATH, "TOPOLOGY", TOPOLOGY);

        // Set the MC_TB_IF in the database
        uvm_config_db#(virtual mby_mc_tb_if)::set(uvm_root::get(), TB_ENV_PATH , "mby_mc_tb_if", mby_mc_tb_if);

        // Set the SHDV tb_intf in the database
        slu_resource_db#(virtual shdv_base_tb_intf)::add({"env", ".IP_intf"}, shdv_intf, `__FILE__, `__LINE__);

        uvm_config_db#(virtual svt_axi_if)::set(null, $sformatf("%s.axi_bfm",TB_ENV_PATH), "axi_vif", axi_if);

        uvm_config_db#(virtual axi_reset_if.axi_reset_modport)::set(uvm_root::get(), $sformatf("%s.axi_bfm*",TB_ENV_PATH), "reset_mp", axi_reset_if.axi_reset_modport);

        //Set SVT AHB interface
        uvm_config_db#(virtual svt_ahb_if)::set(uvm_root::get(), $sformatf("%s",TB_ENV_PATH), "ahb_if", ahb_if);

        //Set AVT AHB reset interface modport
        uvm_config_db#(virtual ahb_reset_if.ahb_reset_modport)::set(uvm_root::get(), $sformatf("%s.ahb_bfm.sequencer", TB_ENV_PATH), "reset_mp", ahb_reset_if.ahb_reset_modport);

    end

endmodule
