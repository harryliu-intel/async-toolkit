// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Interface object that generates a clock for testbench use
//                 and access by tests.  Also an UVM class wrapper for interface
// -----------------------------------------------------------------------------

interface clkgen_if;

    // Todo: Arbitrary Period is employed, need to change depending on C-Spec
    //   Requirement
    parameter int unsigned PERIOD       = 3840;
    parameter int unsigned DELAY        = 0;
    parameter int unsigned DUTYCYCLE    = 50;
    parameter int unsigned DIFFERENTIAL = 1;        // IGNORED (alway differential signal)
    parameter int unsigned ADJUSTMENT_WINDOW = 1;   // initial time for period adjustment

    // Output Signal
    logic clk  = 0;
    logic clkn;

    // Input Signals (initialized by parameters) that can be modified at
    //   run-time to dynamically change clock settings
    logic [31:0] period;
    logic [31:0] dutycycle;

    // Internal Signals
    logic   intclk;
    integer clk_hi = 0;
    integer clk_lo = 0;

    initial begin
        intclk       = 0;
        period       = PERIOD;
        dutycycle    = DUTYCYCLE;

        // First cycle allows for period to be adjusted within ADJUSTMENT_WINDOW time
        #ADJUSTMENT_WINDOW;
        clk_hi = (period * dutycycle)/100;
        clk_lo =  period - clk_hi;
        if(clk_lo > ADJUSTMENT_WINDOW) begin
            #(clk_lo - ADJUSTMENT_WINDOW);
            intclk = 1;
            #(clk_hi);
            intclk = 0;
        end      

        // Allow for half-period to be smaller than the simulator
        // resolution allows by  calculating seperate high and low times
        while(1) begin
            clk_hi = (period * dutycycle)/100;
            clk_lo =  period - clk_hi;
    
            if (period != 32'b0) begin
                #(clk_lo);
                intclk = 1;
                #(clk_hi);
                intclk = 0;
            end       else begin
                // prevent infinite loop if period of 0 is specified
                intclk = #(1) 1;
            end      
        end      
    end      

    // Transport delay of internally generated clock to allow for better
    //   alignment of independent clocks used by testbench.
    always @(intclk) begin
        if(DELAY == 0) begin
            clk <= intclk;
        end      
        else begin
            clk <= #(DELAY) intclk;
        end      
    end      

    assign clkn = ~clk;

endinterface

