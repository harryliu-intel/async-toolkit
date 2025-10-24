// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`include "cdp_lamb_1r1w1c_128d_144b.sv"

module lamb_tb #() ();

   localparam DEPTH=128;
   localparam DWIDTH=144;
   localparam AWIDTH=$clog2(DEPTH);
   
   logic clk;
   logic wen;
   logic ren;
   logic [AWIDTH-1:0] radr;
   logic [AWIDTH-1:0] wadr;
   logic [DWIDTH-1:0] wdata;
   logic [DWIDTH-1:0] dout;
   
   logic              test__scan_en;
   logic [1:0]        dft__core_si;
   logic              icg_force_on;
   logic              dft_read_bypass;
   logic              dft__mem_wr_disable;
   logic [1:0]        dft__core_so;
   
   cdp_lamb_1r1w1c_128d_144b #(DEPTH, DWIDTH, AWIDTH) dut(.*);

endmodule // lamb_tb
