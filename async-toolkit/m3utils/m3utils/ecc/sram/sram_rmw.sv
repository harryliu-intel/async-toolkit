// INTEL TOP SECRET
// Copyright 2013 Intel Corporation. All Rights Reserved.

// Used to implement a read-modify-write sram. Assumes that the modification
// of the o_rd data to i_wd data happens in one combination block outside of
// this unit. Exposes the wi/wa ports (so that mgmt write can be injected),
// but still does the delay of these channels internally. Consequently it
// assumes that i_wi/i_wa arrive READ_LATENCY cycles before the associated
// i_wd.

module sram_rmw #(parameter WA=6,           // Sram address width
                  parameter WD=32,          // Sram datapath width
                  parameter READ_LATENCY=2, // Sram read latency
                  parameter UNFLOPPED_BYPASS=0)
  (input   logic           clk,
   input   logic           rst_n,
   // Inputs
   input   logic           i_ri,     // read enable
   input   logic           i_wi,     // write enable
   input   logic [WA-1:0]  i_ra,     // read address
   input   logic [WA-1:0]  i_wa,     // write address
   input   logic [WD-1:0]  i_wd,     // write data (that has been modified)
   input   logic [WD-1:0]  i_srd,    // read data input from sram
   input   logic           i_suerr,  // uncorrectable error bit from sram
   input   logic           i_scerr,  // correctable error bit from sram
   // Outputs                        
   output  logic [WD-1:0]  o_rd,     // read data output (to be modified)
   output  logic           o_sri,    // sram read enable
   output  logic           o_swi,    // sram write enable
   output  logic [WA-1:0]  o_sra,    // sram read address
   output  logic [WA-1:0]  o_swa,    // sram write address
   output  logic [WD-1:0]  o_swd,    // sram write data
   output  logic           o_uerr,   // error suppressed when sram rd not used
   output  logic           o_cerr    // error suppressed when sram rd not used
   );
  
  // Sram read control and write data drive the sram directly
  assign   o_sri = i_ri;
  assign   o_sra = i_ra;
  assign   o_swd = i_wd;  

  // Store the write address and enable until i_wd comes back around
  delay_v #(.WIDTH(WA), .STAGES(READ_LATENCY)) u_delay_v
    (.clk(clk), .rst_n(rst_n), .i_in(i_wa),   .i_in_v(i_wi),
                               .o_out(o_swa), .o_out_v(o_swi));

  // Instantiate bypass to hide the read latency
  sram_bypass #(.WA               (WA),            // Address width
                .WD               (WD),            // Datapath width
                .READ_LATENCY     (READ_LATENCY),  // Read latency of sram
                .UNFLOPPED_BYPASS (UNFLOPPED_BYPASS)// Always flop bypass data
                ) u_sram_bypass
    (.clk     (clk),
     .rst_n   (rst_n),
     .i_sra   (o_sra),
     .i_srd   (i_srd),
     .i_swa   (o_swa),
     .i_swen  (o_swi),
     .i_swd   (o_swd),
     .o_rd    (o_rd),
     .i_suerr (i_suerr),
     .i_scerr (i_scerr),
     .o_uerr  (o_uerr),
     .o_cerr  (o_cerr));

endmodule
