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

`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
   `include "xvm_macros.svh"
`endif

    import sla_pkg::*;
    `include "uvm_macros.svh"
    `include "slu_macros.svh"

    import fc_env_pkg::*;
    import fc_test_pkg::*;

    //-- Temporary local signal as work around for VCS bug 
    //-- with -debug_all option

    `include "hvl_ti_conns.svh"
    //`include "fc_pull_up_down.sv"

    //-- work around for VCS bug with -debug_all option

    // -------------------------------------------------------------------------
    // Interface & Test Island
    // -------------------------------------------------------------------------
    sig_if fc_sig_if();

    // -------------------------------------------------------------------------
    // TB Clocks
    // -------------------------------------------------------------------------
    clkgen_if #(.PERIOD(10000), .DELAY(0), .DUTYCYCLE(50)) tb_clk();
    clkgen_if #(.PERIOD(40000), .DELAY(0), .DUTYCYCLE(50)) xtal_25M_clk();
    assign sig_if.tb_clk        = tb_clk.clk;
    assign sig_if.tb_rst_b      = 0; // FIXME: connect this to RTC RST

    // ------------------------------------------------------------------------
    // TB Forces
    // ------------------------------------------------------------------------
    force_if apply_forces();

    //`include "fc_forces.sv"

    //Debug tracker for looking at all IP poks/masks
    //`include "pok_trk.sv"

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
    //TbUtilsPkg::VintfBundle fc_vintfBundle;
    //svlib_pkg::VintfBundle vintfBundle_sb;

    initial begin 

        //sla_pkg::sla_resource_db #(virtual sig_if)::add("sig_if", fc_sig_if, `__FILE__,`__LINE__);

        //uvm_config_db#(virtual sig_if)::set(uvm_root::get(), "*", "sig_if", fc_sig_if); 
        uvm_config_db#(virtual sig_if)::set(null, "*", "sig_if", fc_sig_if); 

       // slu_vif_container #(virtual sig_if)  FcSigIntfWrapper;
        //slu_vif_container #(virtual FcDutIf) FcDutIntfWrapper;

        //fc_vintfBundle = new("fc_vintfBundle");
       // assert(fc_vintfBundle) else uvm_report_fatal("TB", "Failed to create fc_vintfBundle");
        //ssn uvm_config_object::set(null, .inst_name("*"), .field_name(FC::VINTF_BUNDLE_NAME), .value(fc_vintfBundle), .clone(0));
       // uvm_config_object::set(null, .inst_name("*"), .field_name(FC::VINTF_BUNDLE_NAME), .value(fc_vintfBundle));

        // Create all interface wrappers
       // FcSigIntfWrapper = new("FcSigIntfWrapper");
       // FcSigIntfWrapper.set_v_if(fc_sig_if);
        //FcDutIntfWrapper = new("FcDutIntfWrapper");
        //FcDutIntfWrapper.set_v_if(fc_hdl_top.fctop_dut_if);
       // fc_vintfBundle.setData(FC::FCSIGIFNAME, FcSigIntfWrapper);
        //fc_vintfBundle.setData(FC::FCDUTIFNAME, FcDutIntfWrapper);


        FC::apply_forces = apply_forces;
        ->vintfInitDone;
    end     

    // -------------------------------------------------------------------------
    // UVM test
    // -------------------------------------------------------------------------
    initial begin: UVM_TEST
        $display($time, "%m: Running FC top Build");
        wait(vintfInitDone.triggered);

        if ($value$plusargs("UVM_TESTNAME=%s", testname  )) begin
             xvm_pkg::run_test("", testname,   xvm::EOP_UVM);
        end
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
    //------------------------------------------------------------------------
    mby_wm_top mby_wm();

endmodule
