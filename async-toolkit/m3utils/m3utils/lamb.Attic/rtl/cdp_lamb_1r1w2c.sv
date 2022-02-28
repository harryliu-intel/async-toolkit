//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2021 - 2021 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related
// to the source code ("Material") are owned by Intel Corporation or its
// suppliers or licensors. Title to the Material remains with Intel
// Corporation or its suppliers and licensors. The Material contains trade
// secrets and proprietary and confidential information of Intel or its
// suppliers and licensors. The Material is protected by worldwide copyright
// and trade secret laws and treaty provisions. No part of the Material may
// be used, copied, reproduced, modified, published, uploaded, posted,
// transmitted, distributed, or disclosed in any way without Intel's prior
// express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or
// delivery of the Materials, either expressly, by implication, inducement,
// estoppel or otherwise. Any license under such intellectual property rights
// must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------

`resetall
`default_nettype none

// synthesis translate_off

module cdp_lamb_1r1w2c_depthd_widthb
 #(
 parameter DEPTH=depth,
 parameter DWIDTH=width,
 parameter AWIDTH=$clog2(DEPTH),
 parameter X_ON_NOT_REN=1, // produce X on dout on non-reading cycles
 parameter X_ON_CONFLICT=1 // produce X on dout when wadr and radr match
 )
(
  input  logic wclk,
  input  logic rclk,
  input  logic wen,                 // sampled @(posedge wclk)
  input  logic ren,                 // sampled @(posedge rclk)
  input  logic [AWIDTH-1:0] radr,   // sampled @(posedge rclk)
  input  logic [AWIDTH-1:0] wadr,   // sampled @(posedge wclk)
  input  logic [DWIDTH-1:0] wdata,  // sampled @(posedge wclk)
  output logic [DWIDTH-1:0] dout,   // produced @(posedge rclk)

  input  logic test__scan_en,
  input  logic [1:0] dft__core_si,
  input  logic icg_force_on,
  input  logic dft_read_bypass,
  input  logic dft__mem_wr_disable,
  output logic [1:0] dft__core_so
);
  logic [DEPTH-1:0][DWIDTH-1:0] mem_array;

  // The following state is used to check that the LAMB is not being
  // used illegally with overlapping read and write ops.
  // A read and a write are conflicting 
  // iff
  // the read and write are to the same address and the read and write
  // cycles overlap in (real) time
  logic [AWIDTH-1:0]            active_radr, active_wadr;
  logic                         ractive, wactive;
  logic                         rw_conflict;
  logic                         do_x;
   
  // write
  always_ff @(posedge wclk) begin
    if (wen)
      mem_array[wadr] <= wdata;

    // update conflict state
    if (wen) begin 
       wactive <= '1;
       active_wadr <= wadr;
    end
    else
      wactive <= '0;
     
  end

  assign rw_conflict = (wactive && ractive && (active_radr == active_wadr));

  always_comb
    assert (rw_conflict !== '1); // on reset, rw_conflict is X and thats OK

  logic [AWIDTH-1:0] radr_int;
  always_ff @(posedge rclk or posedge rw_conflict) begin

     //
     // note the careful, pessimistic coding below.
     // ren can change before the next posedge rclk, therefore
     // once rw_conflict has been raised, we want to ensure that do_x
     // stays true for the entire cycle (even if ren has meanwhile fallen)
     //
     // if we didn't check for rw_conflict below, if we have a conflict
     // that appears in the second half of the cycle, and ren has by that
     // time been cleared, then the conflict would appear to go away.
     //
     if (ren || rw_conflict) begin
        do_x <= (rw_conflict && X_ON_CONFLICT);
     end
     else
       do_x <= X_ON_NOT_REN;
  end
   
  always_ff @(posedge rclk) begin
     if (ren) begin
        radr_int <= radr;
     end

     // update conflict state
     if (ren) begin
        ractive <= '1;
        active_radr <= radr;
     end
     else
       ractive <= '0;
     
  end
  assign dout = do_x ? {DWIDTH{1'bx}} : mem_array[radr_int];

endmodule
// synthesis translate_on

`default_nettype wire
