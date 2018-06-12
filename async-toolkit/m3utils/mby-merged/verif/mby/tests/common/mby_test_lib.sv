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
//   Project       : MBY
//   Description   : MBY Test Library
//------------------------------------------------------------------------------

program  mby_test_lib;

`ifdef XVM
    import ovm_pkg::*;
    import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

    import sla_pkg::*;
    import uvm_pkg::*;
    import mby_env_pkg::*;

    `include "uvm_macros.svh"
    `include "slu_macros.svh"

    import mby_wm_dpi_pkg::* ;

    `include "mby_base_test.svh"
    `include "mby_alive_test.svh"
    `include "mby_wm_reg_rw_test.svh"


    // UVM Start test
    initial begin
        string testname;

        if ($test$plusargs("WHITE_MODEL_EN")) begin
            //Currently we connect to M3 White model server which requires the path of WM
            //Server passed in as an argument to "wm_server_start" function. This function
            //call implementation might change when we switch to Scala White model server.
            string model_server;
            `define M3_WM wm/src/main/m3/model_server/AMD64_LINUX/modelserver
            `define create_wm_str(base, wm_path) "``base``/``wm_path``"
            
            model_server = `create_wm_str(`MODEL_ROOT, `M3_WM);
            `uvm_info(get_full_name(), $sformatf(" WM Server path: %0s",model_server), UVM_HIGH)
            //Start White Model server
            if(wm_server_start(model_server)) begin
                `uvm_error(get_full_name(), "Error while connecting to the WM")
            end
        end

        if ($value$plusargs("UVM_TESTNAME=%s", testname  )) begin
`ifndef XVM
            $display ("MBY_tb Started Running %s in UVM mode!\n",testname);
        end
        uvm_pkg::run_test(testname);
`else
        $display ("MBY_tb Started Running %s in XVM mode!\n",testname);
    end
    xvm_pkg::run_test("", testname,   xvm::EOP_UVM);
`endif

end

final begin

    //Stop the White model server if it was started at the beginning of the test.
    if ($test$plusargs("WHITE_MODEL_EN")) begin
        wm_server_stop();
    end
end

endprogram
