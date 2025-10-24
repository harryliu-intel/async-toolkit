// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`timescale 1ps/1fs

`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/base_ulvt/verilog/lib783_i0s_160h_50pp_base_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/clk_ulvt/verilog/lib783_i0s_160h_50pp_clk_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/spcl_ulvt/verilog/lib783_i0s_160h_50pp_spcl_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsibase_ulvt/verilog/lib783_i0s_160h_50pp_dsibase_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/ldrbase_ulvt/verilog/lib783_i0s_160h_50pp_ldrbase_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/ldrdsibase_ulvt/verilog/lib783_i0s_160h_50pp_ldrdsibase_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/seq_ulvt/verilog/lib783_i0s_160h_50pp_seq_ulvt.v"
//`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsicore_ulvt/verilog/lib783_i0s_160h_50pp_dsicore_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsispcl_ulvt/verilog/lib783_i0s_160h_50pp_dsispcl_ulvt.v"
`include "/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk090_r7v1p0_fv/dsiseq_ulvt/verilog/lib783_i0s_160h_50pp_dsiseq_ulvt.v"

`include "/nfs/site/disks/zsc9_fwr_lib_char_008/mnystroe/p4/hw-dev18a/verilog/i0s/i0sltny00aa1n02x1.v"
`include "/nfs/site/disks/zsc9_fwr_lib_char_008/mnystroe/p4/hw-dev18a/verilog/i0s/i0skpgcmbaa1n02x5.v"
`include "/nfs/site/disks/zsc9_fwr_lib_char_008/mnystroe/p4/hw-dev18a/verilog/i0s/i0skpgencaa1n02x5.v"
`include "/nfs/site/disks/zsc9_fwr_lib_char_008/mnystroe/p4/hw-dev18a/verilog/i0s/i0skpginvaa1n01x1.v"

//`include "/nfs/site/disks/zsc9_fwr_ip/ENGCKGEN/rp1/engckgen.v"
`include "engckgen_standin.v"

`include "lib.comb.add.kpg.KPG_ADD_32-Lfalse-R.1000.v"
`include "add_wrap.sv"

`include "testenv.sv"

// some of the cells from engckgen that this PDK doesn't have

module i0scinv00aa1n08x5
  #()
   (input logic clk,
    output logic clkout,
    wire  vcc,
    wire  vssx);

   i0scinv00aa1n12x5 u_wrapped(.*);
endmodule // i0scinv00aa1n08x5

module i0scilma5aa1n06x5
   #()
   (input logic clk,
    output logic clkout,
    input logic  en,
    input logic  te,
    wire  vcc,
    wire  vssx);

   i0scilb05aa1n02x5 u_wrapped(.*);
endmodule // i0scilma5aa1n06x5

module i0smxn022aa1n09x5( input logic a, input logic b, output logic o1, input logic sa , wire vcc, wire vssx );

   // cant include dsicore verilog.. errors from other modules
   
  assign  o1 =  sa ? ~a : ~b;
endmodule
   

module testenv_tb #() ();

   
   logic             i_clk2x_hi;         // nominal "2X" (~1 GHz) clock
   logic             i_rst_n_hi; // active-lo async reset
                     
   logic [ 63 : 0 ]  i_x_hi;
   logic [ 63 : 0 ]  o_y_hi; // registered output
   
   // config bits for clock recovery
   logic [ 7 : 0]    i_rstval_hi; // reset value of recovery FSM
   logic             i_refc_o_dly_s0_hi; // delay setting of recovery
   logic             i_refc_o_dly_s1_hi; // delay setting of recovery
   logic             i_sel_odd_sync_hi; // odd sync of recovery FSM
   
   // config bits for engckgen
   logic [ 1 : 0 ]   i_sync_hi; // multiphase clock phasing
   logic             i_te_hi; // clock enable  (s.b. 1)
   
   // clock-phase rotation
   logic [ 1 : 0 ]   i_ck_rot_hi;

   // debug output
   logic [ 3 : 0 ] o_ck_hi;

   wire             vcchi; // nominal (~650 mV)
   wire             vcc; // low-voltage (~300mV)
   wire             vssx;
   
   testenv #() u_dut (.*);

   assign vcc = 1;

   assign vcchi = 1;

   assign vssx = 0;

   
   initial begin
      i_rst_n_hi = 0;
      @(posedge i_clk2x_hi);
      @(posedge i_clk2x_hi);
      @(posedge i_clk2x_hi);
      @(posedge i_clk2x_hi);
      @(posedge i_clk2x_hi); 
      i_rst_n_hi = 1;
   end
      
      
   initial begin
      i_clk2x_hi = '0;


      while(1)
        begin
           #500;
           i_clk2x_hi = ~i_clk2x_hi;
        end
   end

   initial begin
      i_rstval_hi = '0;
      i_refc_o_dly_s0_hi = '0;
      i_refc_o_dly_s1_hi = '0;
      i_sel_odd_sync_hi = '0;
      i_sync_hi = '0;
      i_te_hi = '1;
      i_ck_rot_hi = '0;
   end
      
   initial begin
      i_x_hi = 'hc0edbabe_c0edbabe;

      while(1)
        begin
           @(negedge o_ck_hi[0]);
           ++i_x_hi;
        end
   end

   initial begin
      while(1)
        begin
           @(negedge o_ck_hi[0]);

           $display("o_y_hi = %x", o_y_hi);
        end
   end
   
endmodule // testenv_tb

