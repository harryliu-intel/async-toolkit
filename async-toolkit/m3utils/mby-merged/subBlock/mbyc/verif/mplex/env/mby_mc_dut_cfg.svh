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

// Class: mby_mc_dut_cfg
//
// This is the configuration object to control Mplex DUT configuration.
//

`ifndef __MBY_MC_DUT_CFG_GUARD
`define __MBY_MC_DUT_CFG_GUARD

`ifndef __INSIDE_MBY_MC_ENV_PKG
`error "Attempt to include file outside of mby_mc_env_pkg."
`endif

class mby_mc_dut_cfg extends shdv_base_config;

    // Variable: reset_type
    // Definition of the RESET type
    reset_type_e                     reset_type ;


    `uvm_object_utils_begin(mby_mc_dut_cfg)
        `uvm_field_enum  (reset_type_e,                  reset_type,                    UVM_DEFAULT)
    `uvm_object_utils_end


    //---------------------------------------------------------------------------
    // Constructor: new
    //
    // Constructor.
    //
    // Arguments:
    //    string name - mby_mc_dut_cfg object name
    //---------------------------------------------------------------------------
    function new( string name = "mby_mc_dut_cfg");
        super.new(name);
       
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

    endfunction: post_randomize


endclass: mby_mc_dut_cfg

`endif // __MBY_MC_DUT_CFG_GUARD
