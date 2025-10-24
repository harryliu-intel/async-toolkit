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

// Class: mby_mc_env_cfg
//
//   This is the configuration object to control the MPlex env and
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


`ifndef __MBY_MC_ENV_CFG_GUARD
`define __MBY_MC_ENV_CFG_GUARD

`ifndef __INSIDE_MBY_MC_ENV_PKG
`error "Attempt to include file outside of mby_mc_env_pkg."
`endif

class mby_mc_env_cfg extends shdv_base_config;

    // Variable: axi_bfm_cfg
    // SVT AXI BFM config object.
    svt_axi_bfm_pkg::cust_svt_axi_system_configuration axi_bfm_cfg;

    // Variable: axi_num_masters
    // Number of AXI Masters .
    int axi_num_masters;

    // Variable: axi_num_slaves
    // Number of AXI Slaves .
    int axi_num_slaves;

    // Variable: axi_mstr_is_active
    // is_active bit for AXI Master .
    bit axi_mstr_is_active;

    // Variable: axi_slv_is_active
    // is_active bit for AXI Slave .
    bit axi_slv_is_active;

    // Variable: ahb_bfm_cfg
    // SVT AHB BFM config object.
    svt_ahb_bfm_pkg::cust_svt_ahb_system_configuration  ahb_bfm_cfg;

    // SVT AHB bfm cfg variables.
    // Variable: ahb_num_mst
    // Number of AHB Masters .
    int ahb_num_mst;

    // SVT AHB bfm cfg variables.
    // Variable: ahb_num_slv
    // Number of AHB slaves.
    int ahb_num_slv;
    
    // Variable: ahb_mst_is_active
    // is_active bit for AHB Master .
    bit ahb_mst_is_active;

    // Variable: ahb_slv_is_active
    // is_active bit for AHB Slave .
    bit ahb_slv_is_active;

    `uvm_object_utils_begin(mby_mc_env_cfg)
        `uvm_field_object(axi_bfm_cfg,                                               UVM_DEFAULT)
        `uvm_field_int(axi_num_masters,                                              UVM_DEFAULT)
        `uvm_field_int(axi_num_slaves,                                               UVM_DEFAULT)
        `uvm_field_int(axi_mstr_is_active,                                           UVM_DEFAULT)
        `uvm_field_int(axi_slv_is_active,                                            UVM_DEFAULT)
        `uvm_field_int(ahb_num_mst,                                                  UVM_DEFAULT)
        `uvm_field_int(ahb_num_slv,                                                  UVM_DEFAULT)
        `uvm_field_int(ahb_mst_is_active ,                                           UVM_DEFAULT)
        `uvm_field_int(ahb_slv_is_active,                                            UVM_DEFAULT)
    `uvm_object_utils_end


    //---------------------------------------------------------------------------
    // Constructor: new
    //
    // Constructor.
    //
    // Arguments:
    //    string name - mby_mc_env_cfg object name
    //---------------------------------------------------------------------------
    function new( string name = "mby_mc_env_cfg");
        super.new(name);
        axi_bfm_cfg = svt_axi_bfm_pkg::cust_svt_axi_system_configuration::type_id::create("axi_bfm_cfg");
        ahb_bfm_cfg = svt_ahb_bfm_pkg::cust_svt_ahb_system_configuration::type_id::create("ahb_bfm_cfg");

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



endclass: mby_mc_env_cfg

`endif // __MBY_MC_ENV_CFG_GUARD
