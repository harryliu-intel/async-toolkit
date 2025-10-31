// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// Copyright 2013 Intel Corporation. All Rights Reserved.

// Allows sram writes to bypass the memory core and show up on the read
// output. Consequently can be used in read-modify-write loops to hide
// the sram latency. Decomposition diagrams are located here:
//   securewiki.ith.intel.com/display/SRDredrock/Design+-+sram_bypass+Diagrams

module sram_bypass #(parameter WA=6, WD=32, READ_LATENCY=2, UNFLOPPED_BYPASS=0)
 (input  logic          clk,
  input  logic          rst_n,
  input  logic [WA-1:0] i_sra,
  input  logic [WD-1:0] i_srd,
  input  logic [WA-1:0] i_swa,
  input  logic          i_swen,
  input  logic [WD-1:0] i_swd,
  output logic [WD-1:0] o_rd,
  input  logic          i_suerr,
  input  logic          i_scerr,
  output logic          o_uerr,
  output logic          o_cerr
  );

  // Local channels
  logic [WA-1:0] cache_ra[READ_LATENCY:0];
  logic [WD-1:0] cache_wd[READ_LATENCY:0];
  logic          match0; // lintra s-70036 "not used when UNFLOPPED_BYPASS==0"
  logic [READ_LATENCY:1] match, match_dly;

  // Incoming write data (wd) and read address (ra) get put in the first cache
  // position
  assign cache_ra[0] = i_sra;
  assign cache_wd[0] = i_swd;

  // Compare the write address to all of the cached read addresses
  // Delay match result until corresponding read is ready
  generate
  for (genvar i=1; i<=READ_LATENCY; i++) begin:addr_cmp
    assign match[i] = i_swen & (i_swa==cache_ra[READ_LATENCY-i]);
    delay #(.WIDTH(1), .STAGES(i)) u_delay
      (.clk(clk), .rst_n(rst_n), .i_in(match[i]), .o_out(match_dly[i]));
  end
  endgenerate
  assign   match0 = i_swen & (i_swa==cache_ra[READ_LATENCY]);

  // Store the write addr/data for READ_LATENCY cycles
  always_ff @(posedge clk) begin
    if (~rst_n)
      for (int i=0; i<READ_LATENCY; i=i+1) begin
        cache_ra[i+1]  <= '0;
        cache_wd[i+1]  <= '0;
      end
    else
      for (int i=0; i<READ_LATENCY; i=i+1) begin
        cache_ra[i+1]  <= cache_ra[i];
        cache_wd[i+1]  <= cache_wd[i];
      end
  end

  // Set output. Do bypass if there was a match.
  //   By default, send the sram read data through to the output then check
  //   for address matches. If UNFLOPPED_BYPASS==1, then the i_swd input can
  //   be passed directly through to the output. This is necessary in some
  //   cases, but is more difficult for timing and consequently is turned off
  //   by default. 
  //   If there are multiple bypass matches, newer writes take precedence
  always_comb begin
    o_rd   = i_srd;                               // by default use sram output
    o_uerr = i_suerr;                             // pass the err bits 
    o_cerr = i_scerr;                             // through unchanged

    for (int i=READ_LATENCY; i>=1; i--) begin     // exclude cache_wd[0]
      if (match_dly[i])
        begin
          o_rd   = cache_wd[i];
          o_uerr = 1'b0;                          // suppress errs when using
          o_cerr = 1'b0;                          // the bypass
        end
    end
    if (match0 & (UNFLOPPED_BYPASS==1))           // include cache_wd[0]
      begin
        o_rd   = cache_wd[0];
        o_uerr = 1'b0;                            // suppress errs when using
        o_cerr = 1'b0;                            // the bypass
      end
  end

endmodule
