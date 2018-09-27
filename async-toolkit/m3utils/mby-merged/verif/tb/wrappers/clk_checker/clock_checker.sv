// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Ravindra Ganti
// Created On   :  01/05/2017
// Description:
//   This Clock Checker Interface checks for 
//    * clkreq/clkack protocol handshake validation
//    * correct fixed and switched frequencies
//    * clock being killed unexpectedly
//=========================================================

`ifndef CLOCK_CHECKER__SV
`define CLOCK_CHECKER__SV


interface clock_checker #(parameter SCALE_FACTOR = 1000000000, //-- Default value set to 10 power 9, assuming the timescale is 1ns.
                          parameter real OFFSET = 1000.0, //-- Calibration of Frequency to report in case of error
                          parameter string CLK_NAME = "", //-- Interface Clock Name for debug
                          parameter string CLKREQ_NAME = "", //-- Clock Req Name for debug
                          parameter string CLKACK_NAME = "", //-- Clock Ack Name for debug
                          parameter string MSG = "VIOLATION",
                          parameter string UNITS = "MHz",
                          parameter real GUARD_BAND = 0.005) //-- Guard Band in ns.
                        (input logic clk, //-- Testbench Clock for triggering assertions
                         input logic rst, //-- Reset 
                         input logic clk_sig, //-- Interface Clock signal to be monitored
                         input logic clk_switch, //-- Signal to inform if dynamic clock swithing occurs
                         input logic [31:0] exp_clk_freq, //-- Expected fixed clock frequency
                         input logic [31:0] exp_sw_clk_freq, //-- Expected switched clock frequency
                         input logic clkreq, //-- clkreq signal to be monitored
                         input logic clkack //-- clkack signal to be monitored
                        );
    timeunit 1ns;
    timeprecision 100ps;
    realtime t_start, t_end; //-- start and end       time to measure clock frequency
    real clk_freq;
    logic window_en; //-- Enable clock frequency measuring window
    logic clk_alive; //-- flag to check if clock is alive
    logic start_window; //-- assertion check window of clkreq and clkack handshake protocol
    logic end_window; //-- de-assertion window of clkreq and/or clkack
    logic clk_sig_d;
    logic clk_switch_d;
    logic random_pulse; //-- random sampling pulse
    logic dis_clk_idle_chk;

    parameter CLK_IDLE_TIMER = 100us; //-- Clock Idle Timer

    assign start_window = (clkreq & clkack);  //-- Clock Ungated
    assign end_window = (!clkreq & !clkack);  //-- Clock Gated
    assign clk_alive = (clk_sig_d ^ clk_sig);

    //-----------------------------------
    //-- Default Clocking for assertions
    //-----------------------------------
    default 
        clocking @(posedge clk); 
    endclocking

    //-----------------------------------
    //-- Disable Assertions
    //-----------------------------------
    initial begin: disable_clock_checker_assertions
        if (!$test$plusargs("EN_CLK_CHKR")) begin
            $assertoff(0, clk_idle_timer.clk_idle_check_a);
            $assertoff(0, fixed_clk_freq_check_a);
            $assertoff(0, dynamic_clk_freq_check_a);
            $assertoff(0, clkreq_reassert_check_a);
            $assertoff(0, clkreq_deassert_check_a);
            $assertoff(0, clkack_deassert_check_a);
            $assertoff(0, clkack_reassert_check_a);
            $assertoff(0, clkack_deassert_as_clkreq_ack_check_a);
            $assertoff(0, clkack_reassert_after_clkreq_reassert_check_a);
        end      
    end      

    //-- Initialiazation
    initial begin
        t_start = 0.0;
        t_end = 0.0;
        window_en = 1'b0;
        clk_freq = 0.0;
        random_pulse = 1'b1;
        dis_clk_idle_chk = 1'b1;
    end      

    //-- Clock-in interface Clock Sig 
    always @(posedge clk or rst) begin
        if (rst) begin
            clk_sig_d <= 1'b0;
        end      
        else begin
            clk_sig_d <= clk_sig;  
        end      
    end      

    //-- Compute frequency of the clk_sig 
    always @(posedge clk_sig or start_window or rst or clk_switch) begin
        if (!start_window || rst || (clk_switch ^ clk_switch_d)) begin
            window_en = 0;
        end      
        else if (!window_en & start_window) begin
            t_start = $realtime();
            window_en = 1;
            //-- Use this random pulse to enable clock frequency checks at discretely
            //-- to avoid simulation performance hit.
            random_pulse = $random();
        end      
        else if (start_window) begin
            t_end= $realtime();
            window_en = 0;
            clk_freq = ((SCALE_FACTOR*1.0)/(t_end-t_start));
        end      
        clk_switch_d = clk_switch;
    end      
 
    //==========================================
    //-- Clock Frequency Check
    //-- Check Static Clock Frequency with 
    //-- expected static frequency
    //-- Note: ##1 clk_switch in the consequent
    //--       expr is to remove the noise 
    //--       due to clock instability as soon
    //--       as the switching activity occurs.
    //==========================================
    property fixed_clk_freq_check;
        realtime current_time;
        @(posedge clk_sig) disable iff (rst) 
        (((start_window && !clk_switch && $stable(clk_switch) && random_pulse), current_time=$realtime()) |=> 
        (((((SCALE_FACTOR*1.0)/exp_clk_freq) == ($realtime()-current_time)) || 
        (((((SCALE_FACTOR*1.0)/exp_clk_freq)-($realtime()-current_time))>0.0) && 
        ((((SCALE_FACTOR*1.0)/exp_clk_freq)-($realtime()-current_time))<=GUARD_BAND)) || 
        (((((SCALE_FACTOR*1.0)/exp_clk_freq)-($realtime()-current_time))<0.0) && 
        ((($realtime()-current_time)-((SCALE_FACTOR*1.0)/exp_clk_freq))<= GUARD_BAND)) || 
        !start_window) or (##1 clk_switch)));
    endproperty: fixed_clk_freq_check


    //==========================================
    //-- Clock Frequency Check
    //-- Check Dynamic Clock Frequency with 
    //-- expected dynamic frequency
    //-- Note: !clk_switch in the consequent
    //--       expr is to remove the noise 
    //--       due to clock instability as soon
    //--       as the switching activity occurs.
    //==========================================
    property dynamic_clk_freq_check;
        realtime current_time;
        @(posedge clk_sig) disable iff (rst) 
        (((start_window && $stable(clk_switch) && clk_switch && random_pulse), current_time = $realtime()) |=> 
        (((((SCALE_FACTOR*1.0)/exp_sw_clk_freq) == ($realtime()-current_time)) || 
        (((((SCALE_FACTOR*1.0)/exp_sw_clk_freq)-($realtime()-current_time))>0.0) && 
        ((((SCALE_FACTOR*1.0)/exp_sw_clk_freq) -($realtime()-current_time)) <= GUARD_BAND)) || 
        (((((SCALE_FACTOR*1.0)/exp_sw_clk_freq)-($realtime-current_time))<0.0) && 
        ((($realtime-current_time)-((SCALE_FACTOR*1.0)/exp_sw_clk_freq)) <= GUARD_BAND)) || 
        !clk_switch || !start_window) or (##1 !clk_switch)));
    endproperty: dynamic_clk_freq_check


    //==========================================
    //-- Clock Livenss Check
    //-- Check if interface clock is alive 
    //==========================================
    initial begin
        forever begin
            fork
                begin: clk_liveness_monitor
                    @(posedge clk_sig);
                    @(negedge clk_sig);
                    if (dis_clk_idle_chk) begin
                        dis_clk_idle_chk = 1'b0;
                    end      
                end      
                begin: clk_idle_timer
                    if (rst) begin
                        @(posedge clk);
                        dis_clk_idle_chk = 1'b1; 
                    end      
                    else begin
                        #(CLK_IDLE_TIMER);
                        if (!dis_clk_idle_chk && !rst) begin
                            clk_idle_check_a: 
                                assert (!start_window)
                                else $error("[CLK-%s] Interface Clock '%s' died unexpectedly without being Clock Gated!!!", MSG, CLK_NAME);
                            //-- Disable check once fired to avoid simulation performance hit. 
                            dis_clk_idle_chk = 1'b1;
                        end      
                    end      
                end      
            join_any
            disable fork;
        end      
    end      


    //==========================================
    //-- Rule 10. After de-asserting, clkreq can
    //-- only assert if clkack is de-asserted.
    //-- Reference:
    //--   IOSF Spec V1.2: Section 4.1.2.4 
    //--   Rules for Interface Clock Gating 
    //==========================================

    always @(clkreq) begin
        if (clkreq && !rst) begin
            clkreq_reassert_check_a: 
                assert (!clkack)
                else $error("[PROTOCOL-%s] '%s' shall only be re-asserted after '%s' is de-asserted!!! (Reference: IOSF Spec V1.2: Section 4.1.2.4)", MSG, CLKREQ_NAME, CLKACK_NAME);
        end      
    end      

    //==========================================
    //-- Rule 11. After asserting, clkreq can
    //-- only de-assert if clkack is asserted.
    //-- Reference:
    //--   IOSF Spec V1.2: Section 4.1.2.4 
    //--   Rules for Interface Clock Gating 
    //==========================================
    always @(clkreq) begin
        if (!clkreq && !rst) begin
            clkreq_deassert_check_a: 
                assert (clkack)
                else $error("[PROTOCOL-%s] '%s' shall only be de-asserted after '%s' is asserted!!! (Reference: IOSF Spec V1.2: Section 4.1.2.4)", MSG, CLKREQ_NAME, CLKACK_NAME);
        end      
    end      

    //==========================================
    //-- Rule 12. If clkack is asserted, then 
    //-- clkack can deassert only if clkreq is 
    //-- de-asserted.
    //-- Reference:
    //--   IOSF Spec V1.2: Section 4.1.2.4 
    //--   Rules for Interface Clock Gating 
    //==========================================
    always @(clkack) begin
        if (!clkack && !rst) begin
            clkack_deassert_check_a: 
                assert (!clkreq)
                else $error("[PROTOCOL-%s] '%s' shall only be de-asserted after '%s' is de-asserted!!! (Reference: IOSF Spec V1.2: Section 4.1.2.4)", MSG, CLKACK_NAME, CLKREQ_NAME);
        end      
    end      

    //==========================================
    //-- Rule 13. If clkack is de-asserted, then 
    //-- clkack can assert only if clkreq is 
    //-- asserted.
    //-- Reference:
    //--   IOSF Spec V1.2: Section 4.1.2.4 
    //--   Rules for Interface Clock Gating 
    //==========================================
    always @(clkack) begin
        if (clkack && !rst) begin
            clkack_reassert_check_a: 
                assert (clkreq)
                else $error("[PROTOCOL-%s] '%s' shall only be re-asserted after '%s' is asserted!!! (Reference: IOSF Spec V1.2: Section 4.1.2.4)", MSG, CLKACK_NAME, CLKREQ_NAME);
        end      
    end      

    //==========================================
    //-- Rule 14. The clkack signal must
    //-- de-assert after clkreq de-asserts as an
    //-- acknowledgement of the de-assertion of
    //-- clkreq.
    //-- Reference:
    //--   IOSF Spec V1.2: Section 4.1.2.4 
    //--   Rules for Interface Clock Gating 
    //==========================================
    property clkack_deassert_as_clkreq_ack_check;
        disable iff (rst) 
        $fell(clkreq) |-> (clkack[*0:$] ##1 !clkack);
    endproperty: clkack_deassert_as_clkreq_ack_check


    //==========================================
    //-- Rule 15. The clkack signal must
    //-- re-assert after clkreq re-asserts after 
    //-- ensuring that the interface clock is 
    //-- valid.
    //-- Reference:
    //--   IOSF Spec V1.2: Section 4.1.2.4 
    //--   Rules for Interface Clock Gating 
    //==========================================
    property clkack_reassert_after_clkreq_reassert_check;
        disable iff (rst) 
        ($rose(clkreq) and (!clk_alive[*0:$] ##1 clk_alive)) |-> (!clkack[*0:$] ##1 clkack);
    endproperty: clkack_reassert_after_clkreq_reassert_check


    //==========================================
    //-- Assertion Directives
    //==========================================

    //=================================================
    //-- Clock Fixed Frequency Check
    //=================================================
    fixed_clk_freq_check_a: 
        assert property(fixed_clk_freq_check)
        else $error("[FREQ-%s] Interface Clock '%s' Fixed Frequency did not match the Expected Fixed Clock Frequency!!! Actual Fixed Frequency: %0.3f%s, Expected Fixed Frequency: %0.3f%s", MSG, CLK_NAME, (clk_freq*OFFSET)/SCALE_FACTOR, UNITS, (exp_clk_freq*OFFSET)/SCALE_FACTOR, UNITS);

    //=================================================
    //-- Clock Dynamic Frequency Check
    //=================================================
    dynamic_clk_freq_check_a: 
        assert property(dynamic_clk_freq_check)
        else $error("[FREQ-%s] Interface Clock '%s' Switched Frequency did not match the Expected Switched Clock Frequency!!! Actual Switched Frequency: %0.3f%s, Expected Switched Frequency: %0.3f%s", MSG, CLK_NAME, (clk_freq*OFFSET)/SCALE_FACTOR, UNITS, (exp_sw_clk_freq*OFFSET)/SCALE_FACTOR, UNITS);

    //=============================================================
    //-- clkack de-assert Check to acknowledge clkreq de-assertion 
    //=============================================================
    clkack_deassert_as_clkreq_ack_check_a: 
        assert property(clkack_deassert_as_clkreq_ack_check)
        else $error("[PROTOCOL-%s] '%s' shall be de-asserted when '%s' is de-asserted to acknowledge '%s' de-assertion!!! (Reference: IOSF Spec V1.2: Section 4.1.2.4)", MSG, CLKACK_NAME, CLKREQ_NAME, CLKREQ_NAME);

    //====================================================
    //-- clkack re-assert Check after clkreq re-assertion 
    //====================================================
    clkack_reassert_after_clkreq_reassert_check_a: 
        assert property(clkack_reassert_after_clkreq_reassert_check)
        else $error("[PROTOCOL-%s] '%s' shall re-assert after '%s' re-asserts after ensuring that the interface clock is valid!!! (Reference: IOSF Spec V1.2: Section 4.1.2.4)", MSG, CLKACK_NAME, CLKREQ_NAME);


endinterface: clock_checker

`endif //-- CLOCK_CHECKER__SV
