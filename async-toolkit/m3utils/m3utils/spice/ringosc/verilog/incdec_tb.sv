// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0


`timescale 1ps/1fs

`include "incdec.sv"

module incdec_tb #() ();

   localparam W=8;

   localparam MAXVAL=160;
   
   logic [W-1:0]  i_cur;
   
   logic          i_decrement;
   
   logic [W-1:0]  o_nxt;

   incdec #() u_dut (.*);
   
   
   initial begin
      for (int d=0; d <=1; ++d) begin
         i_decrement = d;
         
         for (int i=0; i <= MAXVAL; ++i) begin
            i_cur = i;
            #1
              $display("i_cur=%d i_decrement=%b o_nxt=%d", i_cur, i_decrement, o_nxt);
         end
      end
   end
endmodule
