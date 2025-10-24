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

program testcase (                   // a program is a system verilog testbench entry point
    gcm_dut_if dut_if                    // testbench DUT interface 
);     

    // declare testcase variables
    integer loops;
    integer num_reqs;
    integer len;

    // instantiate testbench environment 
    env env = new(
        .dut_if                         (dut_if   )      // pass in the interface

        // input bubble rate between 1/100 and 25/100 
        //.knob_inp_bubble_numerator_min  (1      ),
        //.knob_inp_bubble_numerator_max  (25     ),
        //.knob_inp_bubble_denominator    (100    )
    );

    // testcase execution

    initial begin                  // an "initial" procedure is executed at the beginning of a simulation

        loops    = 1;              // number of times to run the test loop 
        num_reqs = 3;              // number of requests to be sent to each port for each iteration 
        
        //env.cfg.randomize();        // randomize configuration
        `ifdef INTEL_SIMONLY
            `ifdef MBY_GCM_BEHAVE_MEMS //FIXME: What is the best method for doing this?
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_0.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem");
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_1.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_2.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_3.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_4.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_5.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_6.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_7.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_8.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_9.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_10.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_11.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_12.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_13.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_14.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
                   top.dut.gcm_gen_mem_inst.gcm_sram_mems.gcm_wrap_mem_rx_uc_wm_shell_128x68_15.memory_init("/nfs/sc/disks/sc_mby_00023/abisaira/mby/work_root/mby-mby-x0/subBlock/mbyc/src/gmm/gms/gcm/sandbox/val/tests/rxuc_wm_0.mem" );
            `endif
        `endif //  `ifdef INTEL_SIMONLY


        // Run test
        `include "test_loop_include.sv"     // the test loop (see sandbox/verilog/test_loop_include.sv)

        $display("Simulation PASSED");      // test exits immediately upon errors, so it passes if this line is reached
        $finish();                          // ca  the final procedure (if it exists) and exits the simulation

    end // initial

endprogram // testcase
