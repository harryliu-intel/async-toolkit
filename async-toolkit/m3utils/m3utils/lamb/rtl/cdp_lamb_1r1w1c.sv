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

module cdp_lamb_1r1w1c_depthd_widthb
 #(
 parameter DEPTH=depth,
 parameter DWIDTH=width,
 parameter AWIDTH=$clog2(DEPTH),
 parameter X_ON_NOT_REN=1
 )
(
  input  logic clk,
  input  logic wen,
  input  logic ren,
  input  logic [AWIDTH-1:0] radr,
  input  logic [AWIDTH-1:0] wadr,
  input  logic [DWIDTH-1:0] wdata,
  output logic [DWIDTH-1:0] dout,

  input  logic test__scan_en,
  input  logic [1:0] dft__core_si,
  input  logic icg_force_on,
  input  logic dft_read_bypass,
  input  logic dft__mem_wr_disable,
  output logic [1:0] dft__core_so
);
  logic [DEPTH-1:0][DWIDTH-1:0] mem_array;
  logic                         do_x;

   
  // write
  always_ff @(posedge clk) begin
    if (wen)
      mem_array[wadr] <= wdata;
  end

  logic [AWIDTH-1:0] radr_int;
  always_ff @(posedge clk) begin
     if (ren) begin
       radr_int <= radr;
       do_x     <= '0;
     end
     else
       do_x <= X_ON_NOT_REN;
  end
  assign dout = do_x ? {DWIDTH{1'bx}} : mem_array[radr_int];

endmodule

// synthesis translate_on

`default_nettype wire
