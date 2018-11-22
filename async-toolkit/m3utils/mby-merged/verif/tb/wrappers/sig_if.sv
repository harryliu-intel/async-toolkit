// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Interface object for FC Signals 
// -----------------------------------------------------------------------------

interface sig_if;

    // -------------------------------------------------------------------------
    // Functional/Global interface signals
    // -------------------------------------------------------------------------
    logic ref_clk;
    logic tb_clk;
    logic tb_rst_b;
    logic lan_powergood_rst_b; 
    logic tmp_cclk; 
    logic tmp_clk; 
    logic tmp_mclk; 
	
    // -------------------------------------------------------------------------
    // Clock Signals )
    // -------------------------------------------------------------------------
    

    // -------------------------------------------------------------------------
    // Full Chip Clock Pin Mux Control Signals
    // -------------------------------------------------------------------------


    // -------------------------------------------------------------------------
    // Clock Mux Control Signals
    // -------------------------------------------------------------------------


    // -------------------------------------------------------------------------
    // Spy Debug Signals
    // -------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // IP enable indication, used by the config object to report statically 
    // disabled IPs
    // ------------------------------------------------------------------------


    // ------------------------------------------------------------------------
    // Interface signals must only be accessed through interface methods and
    // not directly from UVM to maintain code compatibility with sim
    // acceleration. If a call needs to be blocking then it should have
    // atleast one output argument to work in the ZEBU flow.
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // Wait for a clock edge. Replace all #delays in UVM with this call
    // -------------------------------------------------------------------------
    task automatic wait_clk (input int unsigned cycle);
        //Each cycle is 1us 
        repeat (cycle *100)@(posedge tb_clk);
    endtask

    // -------------------------------------------------------------------------
    // Wait for a clock edge. Replace all #delays in UVM with this call
    // -------------------------------------------------------------------------
    task automatic wait_clk_edge (output logic clk_level);
        //Try to not call every clock to minimize perf impact!
        //wait (side_clk_cnt == 10);
        @(tb_clk);
        if (tb_clk) clk_level = 1;
        if (!tb_clk) clk_level = 0;
    endtask

    // -------------------------------------------------------------------------
    // Watchdog timer based off a 100MHz tb_clk 
    // -------------------------------------------------------------------------
    task automatic start_WD_timer(input int unsigned val, output expired);
        int timer_val = val;
        expired = 0;

        if ($test$plusargs("DIS_PWRGD_TIMERS")) begin
           timer_val = 1000;
           $display("WD_TIMER: DIS_PWRGD_TIMERS override detected, WD timer set to 1ms %0t",$time);
        end      

        $display("WD_TIMER: %m WD timer started, val=%0d us, time %0t", timer_val, $time);
        repeat (timer_val * 100) @(posedge tb_clk);
        $fatal("WD_TIMER: %m FATAL: WD timer expired, val=%0d us, time %0t", timer_val, $time);
        expired = 1;
    endtask


    // -------------------------------------------------------------------------
    // Wait for signal transition 
    // -------------------------------------------------------------------------
    task automatic checkSigVal(input string name, ref logic sig, input bit edge_type);
        bit expired;
        if (edge_type == 1) begin
           @(sig);
           if(!sig) $fatal("RESET_CHECKER: %0s transition to unexpected value %b. Expected value is 1", name, sig);
        end       else if (edge_type == 0) begin 
           @(sig);
           if(sig) $fatal("RESET_CHECKER: %0s transition to unexpected value %b vs. Expected value is 0", name, sig);
        end      
    endtask

endinterface

