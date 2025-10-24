// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// state machine for oscillator clock generator
// mika.nystroem@intel.com
// August, 2024

`include "incdec.sv"
`include "oscdecode.sv"

module osc_statemachine_regs
  #(parameter W=8)
   (input  logic            clk,
    input  logic            rst_n,
    input  logic [W - 1:0]  i_rstval,
    input  logic [W - 1:0]  i_nxt,
    output logic [W - 1:0]  o_cur);
   

    always_ff @(posedge clk or negedge rst_n) begin
       o_cur <= rst_n ? i_nxt : i_rstval;
    end

endmodule // osc_statemachine_regs

module osc_statemachine
  #(parameter NSETTINGS=4,
    parameter NINTERP  =8,
    parameter NSTAGES  =6,
    parameter NTAPS    = NSTAGES - 1,
    parameter NSETTINGS_PER_TAP = NINTERP * NSETTINGS,
    parameter NTOTAL_SPEEDS = NTAPS * NSETTINGS_PER_TAP + 1,
    parameter W=$clog2(NTOTAL_SPEEDS)
    )
   (input  logic                                          clk,
    input  logic                                          rst_n,
    input  logic                                          i_up_down, 
    input  logic [ W - 1 : 0 ]                            i_rstval,
    output logic [ NSTAGES - 1 : 0]                       o_stage_en,
    output logic [ NINTERP - 1 : 0][ NSETTINGS - 1 : 0 ]  o_interp_ctrl
    );

   logic [W - 1:0]                                        cur, nxt;

   logic                                                  decrement;

   assign decrement = ~i_up_down;

   incdec    u_incdec(.i_cur(cur), 
                      .i_decrement(decrement),
                      .o_nxt(nxt));
   

   osc_statemachine_regs u_osc_statemachine_regs(.clk,
                                                 .rst_n,
                                                 .i_rstval,
                                                 .i_nxt(nxt),
                                                 .o_cur(cur));
   
   oscdecode u_oscdecode(.i_speed(cur),
                         .o_stage_en,
                         .o_interp_ctrl);

endmodule
