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
    msh_node_dut_if dut_if,         // mesh DUT interface 

    mby_msh_mem_if mem_if_0,        // defined in shared/interfaces
    mby_msh_mem_if mem_if_1,        // under subBlock/mbyc/src
    mby_msh_mem_if mem_if_2,
    mby_msh_mem_if mem_if_3
);     


    // declare testcase variables
    integer loops;
    integer num_reqs;

    // instantiate testbench environment 
    
    env env = new(

        .dut_if         (dut_if  ) ,      // pass in the interface
        .mem_if_0	(mem_if_0),	// -hz: 12/7/2018
        .mem_if_1	(mem_if_1),
        .mem_if_2	(mem_if_2),
        .mem_if_3	(mem_if_3),

	// num of input request
//	.knob_inp_req_num (25),	// number of input request for request loop
	.knob_inp_req_num (1),

        .knob_dut_row ($urandom_range(0, 15)), // random dut node row num
        .knob_dut_col ($urandom_range(0, 7)),   // random dut node col num
//      .knob_dut_row (7),   
//      .knob_dut_col (3),

//      .knob_plane($urandom_range(0,2)), // 0:plane0, 1:plane1, other:random
        .knob_plane(2),

//	.knob_drv_toward($urandom_range(0,4)),	// 0:EB, 1:WB, 2:NB; 3:SB; other:random
	.knob_drv_toward(4),	

	.knob_legal_only(1),	// when 'legal_only' is 1, 
				// req_row, req_col, port_row and port_side are
				// generated under constraints to make sure test legal

        .knob_req_row ($urandom_range(0, 16)), // [0,15]=target node row, 16=random
        .knob_req_col ($urandom_range(0, 8)),  // [0,7]=target node col, 8=random
//      .knob_req_row (16),   
//      .knob_req_col (8),

        .knob_rreq_port_row ($urandom_range(0, 16)), // [0,15]=assigned port_row, 16=random
        .knob_rreq_port_side ($urandom_range(0, 4)), // [0,3]=assigned side: 0=n,1=s,2=e,3=w, and 4=random
//      .knob_rreq_port_row (16),   
//      .knob_rreq_port_side (4),

        .knob_rsp_port_row ($urandom_range(0, 16)), // [0,15]=assigned port_row, 16=random
        .knob_rsp_port_side ($urandom_range(0, 4))  // [0,3]=assigned side: 0=n,1=s,2=e,3=w, and 4=random
//      .knob_rsp_port_row (16),   
//      .knob_rsp_port_side (4)

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
