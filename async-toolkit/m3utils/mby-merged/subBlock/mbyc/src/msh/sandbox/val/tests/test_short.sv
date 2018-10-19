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
// -- Description  : An example testcase that part of a design template for starting new RTL designs and
// --                for demonstrating good design practices.
// ---------------------------------------------------------------------------------------------------------------------

`include "env.sv"                   // defines most of the testbench

program testcase (                  // a program is a system verilog testbench entry point
    msh_dut_if dut_if               // mesh DUT interface 
);     


    // declare testcase variables
    integer loops;
    integer num_reqs;

    // instantiate testbench environment 
    
    env env = new(

        .dut_if                         (dut_if   )       // pass in the interface

    );

    // testcase execution

    initial begin                   // an "initial" procedure is executed at the beginning of a simulation

        loops    = 1;               // number of times to run the test loop 
        num_reqs = 10;              // number of requests to be sent to each port for each iteration 



        // Run test
        `include "test_loop_include.sv"     // the test loop (see sandbox/verilog/test_loop_include.sv)

        $display("Simulation PASSED");      // test exits immediately upon errors, so it passes if this line is reached
        $finish();                          // ca  the final procedure (if it exists) and exits the simulation

    end // initial

endprogram // testcase
