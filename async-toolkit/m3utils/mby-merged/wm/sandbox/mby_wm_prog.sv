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
//   Author        : Akshay Kotian
//   Project       : MBY
//   Description   : Sandbox testbench for MBY White model. 
//------------------------------------------------------------------------------

`include "uvm_macros.svh"
import uvm_pkg::*;

import mby_wm_dpi_pkg::* ;

import mby_wm_test_pkg::*;

program mby_wm_prog();
       
    initial begin
        
        string server_path;
        
        if ($value$plusargs("server_path=%s", server_path))
            `uvm_info("wm_prog", $sformatf(" WM Server path: %0s",server_path), UVM_HIGH)
        else
            `uvm_fatal("wm_prog", "WM server path not specified. Use server_path plusarg to specify path")

        if(wm_server_start(server_path)) begin
            `uvm_fatal("wm_prog", "Error while connecting to the WM")
        end
        else
            `uvm_info("wm_prog", "Connected to model_server", UVM_HIGH)

        run_test();

        wm_server_stop();

        `uvm_info("wm_prog",  "Disconnected from model_server", UVM_HIGH)
    end

endprogram

// vim: noai : tw=80 : ts=3 : sw=3 : expandtab : ft=systemverilog
