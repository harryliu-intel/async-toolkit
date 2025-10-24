// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Top level module for FC HVL TB environment
// -----------------------------------------------------------------------------

`include "force_if.sv"

module fc_hvl_top #(
);
    
    import uvm_pkg::*;

    import sla_pkg::*;
    `include "uvm_macros.svh"
    `include "slu_macros.svh"

    import fc_env_pkg::*;
    import fc_test_pkg::*;

    `include "hvl_ti_conns.svh"
    //`include "fc_pull_up_down.sv"

    // -------------------------------------------------------------------------
    // Interface & Test Island
    // -------------------------------------------------------------------------
    sig_if fc_sig_if();

    // -------------------------------------------------------------------------
    // TB Clocks
    // -------------------------------------------------------------------------
    clkgen_if #(.PERIOD(10000), .DELAY(0), .DUTYCYCLE(50)) tb_clk();
    clkgen_if #(.PERIOD(833.333), .DELAY(0), .DUTYCYCLE(50)) tmp_cclk(); //1200MHz
    clkgen_if #(.PERIOD(1250), .DELAY(0), .DUTYCYCLE(50)) tmp_clk();     //800MHz
    clkgen_if #(.PERIOD(6400), .DELAY(0), .DUTYCYCLE(50)) ref_clk();     //156.25MHz
    clkgen_if #(.PERIOD(555.555), .DELAY(0), .DUTYCYCLE(50)) tmp_mclk(); //1800MHz

    assign sig_if.tb_clk        = tb_clk.clk;
    assign sig_if.tb_rst_b      = 0; // FIXME: connect this to RTC RST

    assign sig_if.tmp_cclk     = tmp_cclk.clk;
    assign sig_if.tmp_clk      = tmp_clk.clk;
    assign sig_if.ref_clk      = ref_clk.clk;
    assign sig_if.tmp_mclk     = tmp_mclk.clk;

    // ------------------------------------------------------------------------
    // TB Forces
    // ------------------------------------------------------------------------
    force_if apply_forces();

    `include "fc_forces.sv"


/*    // ------------------------------------------------------------------------
    // Assertion Control Block
    // ------------------------------------------------------------------------
    `include "acb.vh"
    `include "bug_acb.svh"
    `include "tb_acb.svh"
    //`include "tb_st_dis_ip_acb.svh"
    `ifdef NO_PWR_PINS
        `include "tb_nlp_acb.svh"
    `endif
*/
      string testname;

  initial begin

        if ($test$plusargs("FC_ASSERT_OFF")) begin
            $assertkill(0, `HVL_TOP);
            $assertkill(0, `HDL_TOP);
        end      
    end      

    // ------------------------------------------------------------------------

    event vintfInitDone;

    initial begin 

        uvm_config_db#(virtual sig_if)::set(null, "*", "sig_if", fc_sig_if); 

        FC::apply_forces = apply_forces;
        ->vintfInitDone;
    end     

    // -------------------------------------------------------------------------
    // UVM test
    // -------------------------------------------------------------------------
    initial begin: UVM_TEST
        $display($time, "%m: Running FC top Build");
        wait(vintfInitDone.triggered);
        run_test();
    end      

    initial begin: BEACON
        time beacon_period;
        beacon_period = FC::get_time_overrides(.delay(10us), .plusarg("BEACON_PERIOD"));
        forever begin
            FC::systime("BEACON");
            #beacon_period;
        end      
    end      


   //-------------------------------------------------------------------------
   //-- NLP Flow
   //-------------------------------------------------------------------------

   `ifdef NO_PWR_PINS
       //-- Power Sequence Driver 

       //-- UPF enabled filter string
       initial begin: ENABLE_UPF_SIM_FILTER 
           $display("ACE_ERR_FILTER: Enable UPF Simulation filters");
       end      
   `endif

    //------------------------------------------------------------------------
    //-- Clock Checker Binding File
    //------------------------------------------------------------------------
    //`include "bind_clock_checker.sv"

    //------------------------------------------------------------------------
    //-- MBY White Model
    // commented out per Subodh suggestion WW40.2 - 2018
    //------------------------------------------------------------------------
    // mby_wm_top mby_wm();

endmodule
