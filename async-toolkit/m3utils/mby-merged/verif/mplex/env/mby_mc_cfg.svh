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
//   Description   : This is the configuration object to control the Top MPlex env and
//                   its sub components.
//------------------------------------------------------------------------------
//
//   This Class contain all the switches to control the ENV setting.
//
//   By default it contain Saola config object:
//
//   checkers_enabled  - default 1
//
//   monitors_enabled  - default 1
//
//   trackers_enabled  - default 1
//
//   coverage_enabled  - default 1
//
//   Created by Upper env/cfg/test.

`ifndef __MBY_MC_CFG_GUARD
`define __MBY_MC_CFG_GUARD

`ifndef __INSIDE_MBY_MC_ENV_PKG
`error "Attempt to include file outside of mby_mc_env_pkg."
`endif

class mby_mc_cfg extends shdv_base_config;

    // Topology/Test Island items
    mby_mc_defines::mc_topology_e topology = mby_mc_defines::MPLEX_NP_NPHY;

    // Definition of the RAL type
    string                           ral_type = "mby_mc_env_pkg::mby_mc_ral_env";

    // Definition from the Test Island, of TI Path
    string                           ti_path;

    // Definition from the Test Island, of RTL_TOP Path
    string                           rtl_top_path;

    // Definition of the RESET type
    reset_type_e                     reset_type ;

    `uvm_object_utils_begin(mby_mc_cfg)
        `uvm_field_string(ti_path,                                                      UVM_DEFAULT)
        `uvm_field_string(rtl_top_path,                                                 UVM_DEFAULT)
        `uvm_field_string(ral_type,                                                     UVM_DEFAULT)
        `uvm_field_enum  (reset_type_e,                  reset_type,                    UVM_DEFAULT)
        `uvm_field_enum  (mby_mc_defines::mc_topology_e, topology,                      UVM_DEFAULT)
    `uvm_object_utils_end


    function new( string name = "mby_mc_cfg");
        super.new(name);

        if(!uvm_config_db#(int)::get(null, "uvm_test_top", "TOPOLOGY", topology)) begin
            `uvm_fatal(get_name(),$sformatf("Unable to acquire valid topology value!!! value = %0d",topology))
        end

        if (topology == mby_mc_defines::UNK_TOPO) begin
            `uvm_fatal(get_name(), "Topology value is Unknown!!!");
        end

    endfunction: new

    // Collect Plusargs here, then push down cfg changes to any bfm/IP
    function void pre_randomize();
        super.pre_randomize();
    endfunction: pre_randomize

    function void post_randomize();
        super.post_randomize();

        push_down_knobs();
    endfunction: post_randomize

    function void push_down_knobs();

    endfunction: push_down_knobs

    function string get_ral_type();
        return ral_type;
    endfunction: get_ral_type
    
    function mby_mc_defines::mc_topology_e get_tb_topology();
        return topology;
    endfunction : get_tb_topology

endclass: mby_mc_cfg

`endif // __MBY_MC_CFG_GUARD
