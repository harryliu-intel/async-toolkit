/**
 * INTEL TOP SECRET
 * Copyright 2004 - 2013 Intel Corporation. All Rights Reserved.
 * $Id$
 */

//*** Slack-(n+1/2) output buffer *****************************************

module output_timing_buffer$
    #(parameter bit_width = 1,
                slack = 1, 
                forward_latency = 0,
                cycle_time     = 18,
                cycle_time_in  = 18,
                cycle_time_out = 18,
                bf_latency = 2)
    (input _RESET,
     input signed [bit_width:0] L$data,
     output reg L$enable,
     output reg signed [bit_width:0] R$data,
     input R$enable);

`PRS2VERILOG_TIMESCALE

// circular buffer holds data values
reg signed [bit_width:0] x[slack-1:0];
integer num_in=0, num_out=0;

// latency timing
real backward_latency = (slack*cycle_time)-forward_latency;
reg ready_back[slack-1:0], ready_forw[slack-1:0], ready_out;

// temp variable
integer i;


always @(negedge _RESET) 
begin
    disable main;
    disable main2;
//    $display("bit_width: %d", bit_width);
//    $display("slack: %d", slack);
//    $display("forward_latency: %d", forward_latency);
//    $display("cycle_time: %d", cycle_time);
//    $display("cycle_time_in: %d", cycle_time_in);
//    $display("cycle_time_out: %d", cycle_time_out);
//    $display("bf_latency: %d", bf_latency);
    num_in    = 0;
    num_out   = 0;
    ready_out = 1;
    L$enable  = 0;
    R$data    = -1;
    for (i=0 ; i<slack ; i=i+1)
        begin
            ready_back[i] = 1;
            ready_forw[i] = 0;
        end

    if (backward_latency < 0)
    begin
        $display("WARNING: negative backward latency in %m, modeling as 0\n");
        backward_latency = 0;
    end
end

always @(posedge _RESET) 
begin : main
   while (1)
        begin
            wait(ready_back[num_in%slack]);

            // L?x[num_in]
            L$enable = 1;
            wait(L$data >= 0);
            x[num_in%slack] = L$data;
            L$enable = 0;
            wait(L$data < 0);

            ready_back[num_in%slack] = 0;
            ready_forw[num_in%slack] <=
                #(forward_latency * `PRS2VERILOG_TAU) 1;
            num_in = num_in+1;

            #(cycle_time_in * `PRS2VERILOG_TAU);
        end
end

always @(posedge _RESET) 
begin : main2
   while (1)
        begin
            // start R!x[num_out]
            wait(R$enable);
            #(bf_latency * `PRS2VERILOG_TAU);

            // wait for timing
            wait(ready_forw[num_out%slack] & ready_out);
            ready_forw[num_out%slack] = 0;
            ready_back[num_out%slack] <=
                #(backward_latency * `PRS2VERILOG_TAU) 1;
            ready_out = 0;
            ready_out <= #(cycle_time_out * `PRS2VERILOG_TAU) 1;

            // finish R!x[num_out]
            R$data = x[num_out%slack];
            num_out = num_out+1;
            wait(!R$enable);
            #(bf_latency * `PRS2VERILOG_TAU);
            R$data = -1;
        end
end
endmodule 


//*** Slack-(n+1/2) input buffer ******************************************

module input_timing_buffer$
    #(parameter bit_width = 1,
                slack = 1, 
                forward_latency = 0,
                cycle_time     = 18,
                cycle_time_in  = 18,
                cycle_time_out = 18,
                fb_neutral = 7.25,
                fb_valid = 6.75)
    (input _RESET,
     input signed [bit_width:0] L$data,
     output reg L$enable,
     output reg signed [bit_width:0] R$data,
     input R$enable);

`PRS2VERILOG_TIMESCALE

// circular buffer holds data values
reg signed [bit_width:0] x[slack-1:0];
integer num_in=0, num_out=0;

// latency timing
real backward_latency = (slack*cycle_time)-forward_latency;
reg ready_back[slack-1:0], ready_forw[slack-1:0], ready_in;

// temp variable
integer i;


always @(negedge _RESET) 
begin
    disable main;
    disable main2;
//    $display("bit_width: %d", bit_width);
//    $display("slack: %d", slack);
//    $display("forward_latency: %d", forward_latency);
//    $display("cycle_time: %d", cycle_time);
//    $display("cycle_time_in: %d", cycle_time_in);
//    $display("cycle_time_out: %d", cycle_time_out);
//    $display("fb_neutral: %f", fb_neutral);
//    $display("fb_valid: %f", fb_valid);
    num_in   = 0;
    num_out  = 0;
    ready_in = 1;
    L$enable = 0;
    R$data = -1;
    for (i=0 ; i<slack ; i=i+1)
        begin
            ready_back[i] = 1;
            ready_forw[i] = 0;
        end

    if (backward_latency < 0)
    begin
        $display("WARNING: negative backward latency in %m, modeling as 0\n");
        backward_latency = 0;
    end
end

always @(posedge _RESET) 
begin : main
   while (1)
        begin
            wait(ready_back[num_in%slack] & ready_in);

            // HSE: *[ le+; [li]; le-; [~li] ]

            // start L?x[num_in]
            L$enable = 1;
            // wait for timing
            wait(L$data >= 0);
            x[num_in%slack] = L$data;

            ready_back[num_in%slack] = 0;
            ready_forw[num_in%slack] <=
                #(forward_latency * `PRS2VERILOG_TAU) 1;
            ready_in = 0;
            ready_in <= #(cycle_time_in * `PRS2VERILOG_TAU) 1;
            num_in = num_in+1;

            // finish L?x[num_in]
            #(fb_valid * `PRS2VERILOG_TAU);
            L$enable = 0;
            wait(L$data < 0);
            #(fb_neutral * `PRS2VERILOG_TAU);
        end
end

always @(posedge _RESET) 
begin : main2
   while (1)
        begin
            wait(ready_forw[num_out%slack]);

            // R!x[num_out]
            wait(R$enable);
            R$data = x[num_out%slack];
            wait(!R$enable);
            R$data = -1;

            ready_forw[num_out%slack] = 0;
            ready_back[num_out%slack] <=
                #(backward_latency * `PRS2VERILOG_TAU) 1;
            num_out = num_out+1;

            #(cycle_time_out * `PRS2VERILOG_TAU);
        end
end
endmodule 


//*** Slack-1/2 output buffer *********************************************

module output_timing_buffer$s0
    #(parameter bit_width = 1,
                cycle_time = 18,
                bf_latency = 2)
    (input _RESET,
     input signed [bit_width:0] L$data,
     output reg L$enable,
     output reg signed [bit_width:0] R$data,
     input R$enable);

`PRS2VERILOG_TIMESCALE

reg signed [bit_width:0] x;
reg ready;
integer num_toks = 0;

always @(negedge _RESET) 
begin
    disable main;
//    $display("bit_width: %d", bit_width);
//    $display("cycle_time: %d", cycle_time);
//    $display("bf_latency: %d", bf_latency);

    L$enable = 0;
    R$data = -1;
    ready = 1;
end

always @(posedge _RESET) 
begin : main
   while (1)
        begin
            // start R!x
            wait(R$enable);
            #(bf_latency * `PRS2VERILOG_TAU);

            wait(ready);

            // L?x
            L$enable = 1;
            wait(L$data >= 0);
            x = L$data;
            L$enable = 0;
            wait(L$data < 0);

            ready = 0;
            ready <= #(cycle_time * `PRS2VERILOG_TAU) 1;
            num_toks = num_toks+1;

            // finish R!x
            R$data = x;
            wait(!R$enable);
            #(bf_latency * `PRS2VERILOG_TAU);
            R$data = -1;
        end
end
endmodule 


//*** Slack-1/2 input buffer **********************************************

module input_timing_buffer$s0
    #(parameter bit_width = 1,
                cycle_time = 18,
                fb_neutral = 7.25,
                fb_valid = 6.75)
    (input _RESET,
     input signed [bit_width:0] L$data,
     output reg L$enable,
     output reg signed [bit_width:0] R$data,
     input R$enable);

`PRS2VERILOG_TIMESCALE

reg signed [bit_width:0] x;
reg ready;
integer num_toks = 0;

always @(negedge _RESET) 
begin
    disable main;
//    $display("bit_width: %d", bit_width);
//    $display("cycle_time: %d", cycle_time);
//    $display("fb_neutral: %f", fb_neutral);
//    $display("fb_valid: %f", fb_valid);

    L$enable = 0;
    R$data = -1;
    ready = 1;
end

always @(posedge _RESET) 
begin : main
   while (1)
        begin
            // start L?x
            L$enable = 1;
            wait(L$data >= 0);
            x = L$data;

            wait(ready);

            // R!x
            wait(R$enable);
            R$data = x;
            wait(!R$enable);
            R$data = -1;

            ready = 0;
            ready <= #(cycle_time * `PRS2VERILOG_TAU) 1;
            num_toks = num_toks+1;

            // finish L?x
            #(fb_valid * `PRS2VERILOG_TAU);
            L$enable = 0;
            wait(L$data < 0);
            #(fb_neutral * `PRS2VERILOG_TAU);
        end
end
endmodule 
