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
// -- Description  : This file defines the testbench non-test specific validation environment. 
// ---------------------------------------------------------------------------------------------------------------------

`ifndef ENV_SV
`define ENV_SV

//`include "id_generator.sv"
//`include "configuration.sv"
`include "inp_driver.sv"
//`include "scoreboard.sv"
//`include "monitor.sv"
`include "system_driver.sv"

// An env object is instantiated in a testcase
class env;


    virtual msh_node_dut_if dut_if;     // interface used in objects have to be declared as "virtual" 
                                    //  - this may be because interfaces may default to being created at compile time 
                                    //  - and objects and their variables are created at run time.  

    // Declaration of variables that hold handles to env sub-objects.  
    system_driver       sys_drvr;
    inp_driver          inp_driver;

    // declaration of other env variables
    integer         done_cnt;
    string          name; 
    integer         drove_rd_req;


    function new
    (
   
        virtual msh_node_dut_if     dut_if        // testbench interface
    );

        name = "env.sv";
        $display("(time: %0d) %s: **Creating Testbench**", $time, name);

        this.dut_if   = dut_if;     // Copy virtual interface handle from the dut_if paramemter to dut_if class variable
                                    // "this.<variable>" explicitly references a class variable when the need arises 
                                    // to distinguish from another variable with the same name.

        sys_drvr    = new(dut_if);
        inp_driver  = new(dut_if);

        connect();

    endfunction

    task connect();
        $display("(time: %0d) %s: **Connecting Testbench and DUT**", $time, name);
        // Forking these sub-processes allows them to execute concurrently
        // with each other and (because of the join_none) to the parent process.
        // These sub-processes need to execute concurrently because they are driving,
        // monitoring, and responding to various concurrent events in the simulation.
        fork
//            mntr.connect_to_DUT();      // connect the monitor
              inp_driver.reset();
              inp_driver.connect_to_DUT_inputs();
//            foreach (inp_drvrs[i]) begin
//                // To avoid a race condition (i changing before it is doing being used), 
//                // each iteration gets it's own j to hold that iteration number.
//                automatic int j = i;
//                // connect the input drivers
//                fork inp_drvrs[j].connect_to_DUT_inputs(); join_none
//            end // foreach inp_drvrs[i]
        join_none
    endtask

    // reset the env object
    task reset();
        // This outer fork makes it clear which processes are the parent and child processes 
        fork begin
            $display("(time: %0d) %s: **Resetting**", $time, name);
            fork sys_drvr.reset();  join_none
            wait fork;                              // wait for all the processes forked by parent process 
        end join  
    endtask


    task wait_delay(integer delay);
        $display("(time: %0d) %s: **Waiting For %0d Clocks**", $time, name, delay);
        done_cnt = 0;
        while (done_cnt < delay) begin
            repeat (1) @ (posedge dut_if.mclk);
            done_cnt = done_cnt + 1;
        end
    endtask

    task drive_stimulus();
        $display("(time: %0d) %s: **Driving Stimulus**", $time, name);
        fork inp_driver.drive_reqs(); join_none
    endtask

    // wait for <delay> cycles after the monitor reports everythign is done
    task wait_done(integer delay);
        $display("(time: %0d) %s: **Begin Waiting For Done plus %0d Clocks**", $time, name, delay);
        done_cnt = 0;
        while (done_cnt < delay) begin
            repeat (1) @ (posedge dut_if.mclk);
            done_cnt = (inp_driver.drv_done) ? done_cnt + 1 : '0;
//            done_cnt = (mntr.all_done) ? done_cnt + 1 : '0;
        end
        $display("(time: %0d) %s: **End Waiting For Done plus %0d Clocks**", $time, name, delay);
    endtask

endclass

`endif // ENV_SV
