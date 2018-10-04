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
//`include "inp_driver.sv"
//`include "scoreboard.sv"
//`include "monitor.sv"
`include "system_driver.sv"

// An env object is instantiated in a testcase
class env;


    virtual msh_dut_if dut_if;     // interface used in objects have to be declared as "virtual" 
                                    //  - this may be because interfaces may default to being created at compile time 
                                    //  - and objects and their variables are created at run time.  

    // Declaration of variables that hold handles to env sub-objects.  
    system_driver       sys_drvr;

    // declaration of other env variables
    integer done_cnt;
    string name; 


    function new
    (
   
        virtual msh_dut_if     dut_if        // testbench interface

    );

        name = "env.sv";
        $display("(time: %0d) %s: **Creating Testbench**", $time, name);

        this.dut_if   = dut_if;     // Copy virtual interface handle from the dut_if paramemter to dut_if class variable
                                    // "this.<variable>" explicitly references a class variable when the need arises 
                                    // to distinguish from another variable with the same name.

        sys_drvr    = new(dut_if);

    endfunction

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
    
endclass

`endif // ENV_SV
