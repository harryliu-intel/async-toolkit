// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`timescale 1ps/1fs

`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/base_ulvt/verilog/lib783_i0s_160h_50pp_base_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/ldrseq_ulvt/verilog/lib783_i0s_160h_50pp_ldrseq_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/ldrbase_ulvt/verilog/lib783_i0s_160h_50pp_ldrbase_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/supbase_ulvt/verilog/lib783_i0s_160h_50pp_supbase_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/ldrdsibase_ulvt/verilog/lib783_i0s_160h_50pp_ldrdsibase_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsibase_ulvt/verilog/lib783_i0s_160h_50pp_dsibase_ulvt.v"

`include "osc_statemachine.pg.v"

module osc_statemachine__pg_tb #() ();

   localparam NSETTINGS=4;
   
   localparam NINTERP  =8;
   
   localparam NSTAGES  =6;
   
   localparam NTAPS    = NSTAGES - 1;
   
   localparam NSETTINGS_PER_TAP = NINTERP * NSETTINGS;
   
   localparam NTOTAL_SPEEDS = NTAPS * NSETTINGS_PER_TAP + 1;
   
   localparam W        = $clog2(NTOTAL_SPEEDS);

logic  clk ;
logic  rst_n ;
logic  i_up_down ;
logic  [7:0]  i_rstval ;
logic  [5:0]  o_stage_en ;
logic  [31:0] o_interp_ctrl ;
logic  VSS ;
logic  VDD ;

   osc_statemachine #() u_dut (.*);

   initial begin
      VSS = '0;
      VDD = '1;
      
      i_rstval = '0;
     
   end

   // clock
   
   initial begin
      clk = '0;

      while(1)
        begin
           #500;
           clk = ~clk;

        end
   end

   // reset
   
   initial begin
      rst_n = '0;
      @(posedge clk);
      @(posedge clk);
      @(posedge clk);
      @(posedge clk);
      @(posedge clk);
      rst_n = '1;
   end

   initial begin
      for (int ud=0; ud < 1 + 1; ++ud)
        for (int i=0; i <NTOTAL_SPEEDS + 2; ++i) begin

           @(negedge clk);
           
           i_up_down = ud;

           @(posedge clk);
           
           $display("i_up_down = %b, o_stage_en=%b, o_interp_ctrl=%b",
                    i_up_down, o_stage_en, o_interp_ctrl);
           
        
        end
   end

endmodule

