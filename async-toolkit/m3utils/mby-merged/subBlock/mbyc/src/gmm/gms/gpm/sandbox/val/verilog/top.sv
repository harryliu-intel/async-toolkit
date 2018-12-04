///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : ??? 
// -- Description  : The top-level-module in this tesbench 
//
// -- Instantiation Hierarchy:                                  definition file
//                                                              ---------------
//          module  top                                         top.sv
//              gpm_dut_if dut_if                               gpm_dut_if.sv
//              gpm        dut                                  $(CLONE_ROOT)/src/gmm/gms/gpm/rtl/mby_gpm_top.sv
//                  ...                                         (see mby_gpm_top.sv)
//              testcase    test                                tests/<testcase>.sv
//                  env     env                                 env.sv
//                      system_driver   sys_drvr                system_driver.sv
//                      configuration   cfg                     configuration.sv
//                      id_generator    id_gen                  ig_generator.sv
//                      inp_driver      inp_driver              inp_driver.sv
//                          stimulus        stim                stimulus.sv
//                      monitor         mntr                    monitor.sv
//                      scoreboard      sb                      scoreboard.sv
//                  `include "test_loop_include"                test_loop_include.sv
//
//          bind tmpl_arb                                       $(CLONE_ROOT)/src/rtl/template/sva/template_binds.svh
//              tmpl_arb_assert a__tmpl_arb_assert              $(CLONE_ROOT)/src/rtl/template/sva/tmpl_arb_asert.sv
//
//  note:  verilog/gpm_sim_pkg.sv is included on the compilation command line so that it can be used widely
//         without being included everywhere.
// ---------------------------------------------------------------------------------------------------------------------
`ifndef TOP_SV
`define TOP_SV

// A module is a top-level-module if is defined but never instantiated
module top
    import mby_gmm_pkg::*;
//    import mby_gms_pkg::*;
();

    logic clk = 0;                              // declare clock


    // define clock
    initial begin : clk_gen
        forever
            #(10/2)
                clk = ~clk;
    end : clk_gen

//FIXME
    // instantiate DUT interface
    gpm_dut_if dut_if (
        // pass clock into this interface to make it part of the interface
        .clk(clk)
    );

    // start firing reset immediately to protect reset guarded assertions
    initial dut_if.reset_n = 1'b0;

    // instantiate DUT (Design Under Test)
    mby_gpm_top dut (
        .cclk       (dut_if.clk           ),
        .reset_n    (dut_if.reset_n       ),

        // pod pointer ring interface
        .i_pod_ring_left   (dut_if.i_pod_ring_left  ),
        .i_pod_ring_right  (dut_if.i_pod_ring_right ),
        .o_pod_ring_left   (dut_if.o_pod_ring_left  ),
        .o_pod_ring_right  (dut_if.o_pod_ring_right ),

        // Signal from GPM to egress to stall egress from injecting a new dirty
        // pod
        .o_pod_ring_stall_left   (dut_if.o_pod_ring_stall_left  ),
        .o_pod_ring_stall_right  (dut_if.o_pod_ring_stall_right )
    );

    // instantiate testcase
    testcase test (
        // pass interface dut_if into testcase
        dut_if
    );

endmodule : top

`endif // TOP_SV
