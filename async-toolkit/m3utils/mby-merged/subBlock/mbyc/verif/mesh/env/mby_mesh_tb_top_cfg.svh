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
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

// Class: mby_mesh_tb_top_cfg
//
//   This is the Top configuration object to control the Mesh env and
//   its sub components.
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


`ifndef __MBY_MESH_TB_TOP_CFG_GUARD
`define __MBY_MESH_TB_TOP_CFG_GUARD

`ifndef __INSIDE_MBY_MESH_ENV_PKG
`error "Attempt to include file outside of mby_mesh_env_pkg."
`endif

class mby_mesh_tb_top_cfg extends shdv_base_config;

    // Variable: ti_path
    // Definition from the Test Island, of TI Path
    string                           ti_path;

    // Variable: rtl_top_path
    // Definition from the Test Island, of RTL_TOP Path
    string                           rtl_top_path;

    // Variable: reset_type
    // Definition of the RESET type
    reset_type_e                     reset_type ;

    // Variable: dut_cfg
    // Mesh DUT cfg object
    rand mby_mesh_dut_cfg              dut_cfg ;

    // Variable: env_cfg
    // Mesh TB env cfg object
    rand mby_mesh_env_cfg              env_cfg ;


    `uvm_object_utils_begin(mby_mesh_tb_top_cfg)
        `uvm_field_string(ti_path,                                                      UVM_DEFAULT)
        `uvm_field_string(rtl_top_path,                                                 UVM_DEFAULT)
        `uvm_field_enum  (reset_type_e,                  reset_type,                    UVM_DEFAULT)
        `uvm_field_object(dut_cfg,                                                      UVM_DEFAULT)
        `uvm_field_object(env_cfg,                                                      UVM_DEFAULT)
    `uvm_object_utils_end


    //---------------------------------------------------------------------------
    // Constructor: new
    //
    // Constructor.
    //
    // Arguments:
    //    string name - mby_mesh_tb_top_cfg object name
    //---------------------------------------------------------------------------
    function new( string name = "mby_mesh_tb_top_cfg");
        super.new(name);

        dut_cfg = mby_mesh_dut_cfg::type_id::create("dut_cfg");
        env_cfg = mby_mesh_env_cfg::type_id::create("env_cfg");

    endfunction: new

    //---------------------------------------------------------------------------
    // Function: pre_randomize
    //---------------------------------------------------------------------------
    function void pre_randomize();
        super.pre_randomize();
    endfunction: pre_randomize

    //---------------------------------------------------------------------------
    // Function: post_randomize
    // Collect Plusargs here, then push down cfg changes to any bfm/IP
    //---------------------------------------------------------------------------
    function void post_randomize();
        super.post_randomize();

        push_down_knobs();
    endfunction: post_randomize

    //---------------------------------------------------------------------------
    // Function: push_down_knobs
    //---------------------------------------------------------------------------
    function void push_down_knobs();
    endfunction: push_down_knobs



endclass: mby_mesh_tb_top_cfg

`endif // __MBY_MESH_TB_TOP_CFG_GUARD
