// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/**
 * Copyright 2004 - 2013 Intel Corporation. All Rights Reserved.
 * $Id: //hw/tsmc28-dev/verilog/standard/new-timingbuffer.v#6 $
 */

//*** Slack-(n+1/2) output buffer *****************************************

module bd_output_timing_buffer$
    #(parameter bit_width = 1,
                slack = 1, 
                forward_latency = 0,
                cycle_time     = 10,
                cycle_time_in  = 10,
                cycle_time_out = 10,
                bf_latency = 2)
    (input _RESET,
     input signed [bit_width:0] L$data,
     input L$req,
     output reg L$ack,
     output reg signed [bit_width:0] R$data,
     output reg R$req,
     input R$ack);
`PRS2VERILOG_TIMESCALE
localparam fromData = 2;
localparam toData = 2;

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
    L$ack     = 0;
    R$req     = 0;
    R$data    = 0;
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
            wait(L$req != L$ack);
            x[num_in%slack] = L$data;
//$display("%t %m sent %x", $time, L$data);
            L$ack = ~L$ack;

            ready_back[num_in%slack] = 0;
            ready_forw[num_in%slack] <=
                `CAST2VERILOG_WAIT(100.0*forward_latency) 1;
            num_in = num_in+1;

            `CAST2VERILOG_WAIT(100.0*cycle_time_in);
        end
end

always @(posedge _RESET) 
begin : main2
   while (1)
        begin
            // start R!x[num_out]
            wait(R$req == R$ack);

            // wait for timing
            wait(ready_forw[num_out%slack] & ready_out);

            // finish R!x[num_out]
            `CAST2VERILOG_WAIT(100.0*toData);
            R$data = x[num_out%slack];

            ready_forw[num_out%slack] = 0;
            ready_back[num_out%slack] <=
                `CAST2VERILOG_WAIT(100.0*backward_latency) 1;
            ready_out = 0;
            ready_out <= `CAST2VERILOG_WAIT(100.0*cycle_time_in) 1;

            num_out = num_out+1;

            `CAST2VERILOG_WAIT(100.0*fromData);
            R$req = ~R$req;
        end
end

// *** error/sanity checking *** //

//Check to make sure Req/Ack are not X coming out of RESET
always @(posedge _RESET)
begin
    if ( L$req  === 1'bx) $display("%m: WARNING - L.req is X coming out of reset at %0t", $time);
    if ( R$ack  === 1'bx) $display("%m: WARNING - R.ack is X coming out of reset at %0t", $time);
end

//Assign Idle signals
logic Lidle, Ridle;
always_comb begin
    Lidle = L$req == L$ack;
    Ridle = R$req == R$ack;
end

//Collect stats
time t_Lq, t_La, t_Rq, t_Ra;
integer n_Lq, n_La, n_Rq, n_Ra;
always @( L$req ) begin
    t_Lq = (_RESET==1'b0) ? '0 : $time;
    n_Lq = (_RESET==1'b0) ? '0 : n_Lq+1;
end
always @( L$ack ) begin
    t_La = (_RESET==1'b0) ? '0 : $time;
    n_La = (_RESET==1'b0) ? '0 : n_La+1;
end
always @( R$req ) begin
    t_Rq = (_RESET==1'b0) ? '0 : $time;
    n_Rq = (_RESET==1'b0) ? '0 : n_Rq+1;
end
always @( R$ack ) begin
    t_Ra = (_RESET==1'b0) ? 1'b0 : $time;
    n_Ra = (_RESET==1'b0) ? '0 : n_Ra+1;
end

//Display information at the end of the simulation
final begin
    //$timeformat(-12,0,"ps");
    if (Lidle == 1'b0 && Ridle == 1'b0) $display("%m: Both L&R channels are not idle, last activity at %0t", (t_Lq > t_Ra) ? t_Lq : t_Ra);
    else begin
        if (Lidle == 1'b0) $display("%m: L channel not idle, last activity at %0t", (t_Lq > t_La) ? t_Lq : t_La);
        if (Ridle == 1'b0) $display("%m: R channel not idle, last activity at %0t", (t_Rq > t_Ra) ? t_Rq : t_Ra);
    end
end

endmodule 


//*** Slack-(n+1/2) input buffer ******************************************

module bd_input_timing_buffer$
    #(parameter bit_width = 1,
                slack = 1, 
                forward_latency = 0,
                cycle_time     = 10,
                cycle_time_in  = 10,
                cycle_time_out = 10,
                fb_neutral = 7.25,
                fb_valid = 6.75)
    (input _RESET,
     input signed [bit_width:0] L$data,
     input L$req,
     output reg L$ack,
     output reg signed [bit_width:0] R$data,
     output reg R$req,
     input R$ack);
`PRS2VERILOG_TIMESCALE

localparam fromData = 2;
localparam toData = 2;

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
    L$ack    = 0;
    R$req    = 0;
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
            wait(L$req != L$ack);

            `CAST2VERILOG_WAIT(100.0*toData);
            x[num_in%slack] = L$data;

            ready_back[num_in%slack] = 0;
            ready_forw[num_in%slack] <=
                `CAST2VERILOG_WAIT(100.0*forward_latency) 1;
            ready_in = 0;
            ready_in <= `CAST2VERILOG_WAIT(100.0*cycle_time_in) 1;

            num_in = num_in+1;

            // finish L?x[num_in]
            `CAST2VERILOG_WAIT(100.0*fromData);
            L$ack = ~L$ack;
        end
end

always @(posedge _RESET) 
begin : main2
   while (1)
        begin
            wait(ready_forw[num_out%slack]);

            // R!x[num_out]
            wait(R$req == R$ack);
            R$data = x[num_out%slack];
//$display("%t %m received %x", $time, R$data);
            R$req = ~R$req;

            ready_forw[num_out%slack] = 0;
            ready_back[num_out%slack] <=
                `CAST2VERILOG_WAIT(100.0*backward_latency) 1;
            num_out = num_out+1;

            `CAST2VERILOG_WAIT(100.0*cycle_time_out);
        end
end

// *** error/sanity checking *** //

//Check to make sure Req/Ack are not X coming out of RESET
always @(posedge _RESET)
begin
    if ( L$req  === 1'bx) $display("%m: WARNING - L.req is X coming out of reset at %0t", $time);
    if ( R$ack  === 1'bx) $display("%m: WARNING - R.ack is X coming out of reset at %0t", $time);
end

//Assign Idle signals
logic Lidle, Ridle;
always_comb begin
    Lidle = L$req == L$ack;
    Ridle = R$req == R$ack;
end

//Collect stats
time t_Lq, t_La, t_Rq, t_Ra;
integer n_Lq, n_La, n_Rq, n_Ra;
always @( L$req ) begin
    t_Lq = (_RESET==1'b0) ? '0 : $time;
    n_Lq = (_RESET==1'b0) ? '0 : n_Lq+1;
end
always @( L$ack ) begin
    t_La = (_RESET==1'b0) ? '0 : $time;
    n_La = (_RESET==1'b0) ? '0 : n_La+1;
end
always @( R$req ) begin
    t_Rq = (_RESET==1'b0) ? '0 : $time;
    n_Rq = (_RESET==1'b0) ? '0 : n_Rq+1;
end
always @( R$ack ) begin
    t_Ra = (_RESET==1'b0) ? 1'b0 : $time;
    n_Ra = (_RESET==1'b0) ? '0 : n_Ra+1;
end

//Display information at the end of the simulation
final begin
    //$timeformat(-12,0,"ps");
    if (Lidle == 1'b0 && Ridle == 1'b0) $display("%m: Both L&R channels are not idle, last activity at %0t", (t_Lq > t_Ra) ? t_Lq : t_Ra);
    else begin
        if (Lidle == 1'b0) $display("%m: L channel not idle, last activity at %0t", (t_Lq > t_La) ? t_Lq : t_La);
        if (Ridle == 1'b0) $display("%m: R channel not idle, last activity at %0t", (t_Rq > t_Ra) ? t_Rq : t_Ra);
    end
end

endmodule 


//*** Slack-1/2 output buffer *********************************************

module bd_output_timing_buffer$s0
    #(parameter bit_width = 1,
                cycle_time = 10,
                bf_latency = 2)
    (input _RESET,
     input signed [bit_width:0] L$data,
     input L$req,
     output reg L$ack,
     output reg signed [bit_width:0] R$data,
     output reg R$req,
     input R$ack);
`PRS2VERILOG_TIMESCALE
localparam fromData = 2; //6;
localparam toData = 2; //0;

reg ready;
integer num_toks = 0;

always @(negedge _RESET) 
begin
    disable main;
//    $display("bit_width: %d", bit_width);
//    $display("cycle_time: %d", cycle_time);
//    $display("bf_latency: %d", bf_latency);

    L$ack = 0;
    R$req = 0;
    R$data= 0;
    ready = 1;
end

always @(posedge _RESET) 
begin : main
   while (1)
        begin
            wait(ready);

            wait(R$req == R$ack && L$req != L$ack);
            `CAST2VERILOG_WAIT(100.0*toData);
            R$data = L$data;
//$display("%t %m sent %x", $time, L$data);
            L$ack = ~L$ack;

            ready = 0;
            ready <= `CAST2VERILOG_WAIT(100.0*cycle_time) 1;
            num_toks = num_toks+1;

            `CAST2VERILOG_WAIT(100.0*fromData);
            R$req = ~R$req;
        end
end

// *** error/sanity checking *** //

//Check to make sure Req/Ack are not X coming out of RESET
always @(posedge _RESET)
begin
    if ( L$req  === 1'bx) $display("%m: WARNING - L.req is X coming out of reset at %0t", $time);
    if ( R$ack  === 1'bx) $display("%m: WARNING - R.ack is X coming out of reset at %0t", $time);
end

//Assign Idle signals
logic Lidle, Ridle;
always_comb begin
    Lidle = L$req == L$ack;
    Ridle = R$req == R$ack;
end

//Collect stats
time t_Lq, t_La, t_Rq, t_Ra;
integer n_Lq, n_La, n_Rq, n_Ra;
always @( L$req ) begin
    t_Lq = (_RESET==1'b0) ? '0 : $time;
    n_Lq = (_RESET==1'b0) ? '0 : n_Lq+1;
end
always @( L$ack ) begin
    t_La = (_RESET==1'b0) ? '0 : $time;
    n_La = (_RESET==1'b0) ? '0 : n_La+1;
end
always @( R$req ) begin
    t_Rq = (_RESET==1'b0) ? '0 : $time;
    n_Rq = (_RESET==1'b0) ? '0 : n_Rq+1;
end
always @( R$ack ) begin
    t_Ra = (_RESET==1'b0) ? 1'b0 : $time;
    n_Ra = (_RESET==1'b0) ? '0 : n_Ra+1;
end

//Display information at the end of the simulation
final begin
    //$timeformat(-12,0,"ps");
    if (Lidle == 1'b0 && Ridle == 1'b0) $display("%m: Both L&R channels are not idle, last activity at %0t", (t_Lq > t_Ra) ? t_Lq : t_Ra);
    else begin
        if (Lidle == 1'b0) $display("%m: L channel not idle, last activity at %0t", (t_Lq > t_La) ? t_Lq : t_La);
        if (Ridle == 1'b0) $display("%m: R channel not idle, last activity at %0t", (t_Rq > t_Ra) ? t_Rq : t_Ra);
    end
end

endmodule 


//*** Slack-1/2 input buffer **********************************************

module bd_input_timing_buffer$s0
    #(parameter bit_width = 1,
                cycle_time = 10,
                fb_neutral = 7.25,
                fb_valid = 6.75)
    (input _RESET,
     input signed [bit_width:0] L$data,
     input L$req,
     output reg L$ack,
     output reg signed [bit_width:0] R$data,
     output reg R$req,
     input R$ack);
`PRS2VERILOG_TIMESCALE

localparam fromData = 2; //2;
localparam toData = 2; //0;

reg ready;
integer num_toks = 0;

always @(negedge _RESET) 
begin
    disable main;
//    $display("bit_width: %d", bit_width);
//    $display("cycle_time: %d", cycle_time);
//    $display("fb_neutral: %f", fb_neutral);
//    $display("fb_valid: %f", fb_valid);

    L$ack = 0;
    R$req = 0;
    ready = 1;
end

always @(posedge _RESET) 
begin : main
   while (1)
        begin
            wait(ready);

            wait(R$req == R$ack && L$req != L$ack);
            `CAST2VERILOG_WAIT(100.0*toData);
            R$data = L$data;
//$display("%t %m received %x", $time, R$data);
            R$req = ~R$req;

            ready = 0;
            ready <= `CAST2VERILOG_WAIT(100.0*cycle_time) 1;
            num_toks = num_toks+1;

            `CAST2VERILOG_WAIT(100.0*fromData);
            L$ack = ~L$ack;
        end
end

// *** error/sanity checking *** //

//Check to make sure Req/Ack are not X coming out of RESET
always @(posedge _RESET)
begin
    if ( L$req  === 1'bx) $display("%m: WARNING - L.req is X coming out of reset at %0t", $time);
    if ( R$ack  === 1'bx) $display("%m: WARNING - R.ack is X coming out of reset at %0t", $time);
end

//Assign Idle signals
logic Lidle, Ridle;
always_comb begin
    Lidle = L$req == L$ack;
    Ridle = R$req == R$ack;
end

//Collect stats
time t_Lq, t_La, t_Rq, t_Ra;
integer n_Lq, n_La, n_Rq, n_Ra;
always @( L$req ) begin
    t_Lq = (_RESET==1'b0) ? '0 : $time;
    n_Lq = (_RESET==1'b0) ? '0 : n_Lq+1;
end
always @( L$ack ) begin
    t_La = (_RESET==1'b0) ? '0 : $time;
    n_La = (_RESET==1'b0) ? '0 : n_La+1;
end
always @( R$req ) begin
    t_Rq = (_RESET==1'b0) ? '0 : $time;
    n_Rq = (_RESET==1'b0) ? '0 : n_Rq+1;
end
always @( R$ack ) begin
    t_Ra = (_RESET==1'b0) ? 1'b0 : $time;
    n_Ra = (_RESET==1'b0) ? '0 : n_Ra+1;
end

//Display information at the end of the simulation
final begin
    //$timeformat(-12,0,"ps");
    if (Lidle == 1'b0 && Ridle == 1'b0) $display("%m: Both L&R channels are not idle, last activity at %0t", (t_Lq > t_Ra) ? t_Lq : t_Ra);
    else begin
        if (Lidle == 1'b0) $display("%m: L channel not idle, last activity at %0t", (t_Lq > t_La) ? t_Lq : t_La);
        if (Ridle == 1'b0) $display("%m: R channel not idle, last activity at %0t", (t_Rq > t_Ra) ? t_Rq : t_Ra);
    end
end

endmodule

