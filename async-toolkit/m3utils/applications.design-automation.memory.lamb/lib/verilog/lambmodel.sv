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
//
// Structural model of LAMBs
//
// Author : mika.nystroem@intel.com
// April, 2022
//

`resetall
`default_nettype none

`ifndef INTEL_DC

module lambmodel
  #(
    parameter DEPTH=4,
    parameter DWIDTH=10,
    parameter AWIDTH=$clog2(DEPTH)
    ,parameter X_ON_NOT_REN=1  // what does this mean here?
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
    input  logic dft__core_si,
    input  logic icg_force_on,
    input  logic dft_read_bypass,
    input  logic dft__mem_wr_disable,
    output logic dft__core_so
    );

   parameter MAXBLKDEPTH    = 16;
   parameter NBLKS          = (DEPTH - 1) / MAXBLKDEPTH + 1;
   parameter LASTBLKDEPTH   = DEPTH - (NBLKS - 1) * MAXBLKDEPTH;
   parameter MAXSECTORWIDTH = 72;
   parameter NSIDES         = (DWIDTH - 1) / MAXSECTORWIDTH + 1;

   initial assert (DWIDTH <= MAXSECTORWIDTH * 2);

   logic [AWIDTH-1:0] radr_reg;

   always_ff @ (posedge clk) 
     begin
        radr_reg <= radr;
     end

   logic [ DEPTH - 1  : 0 ]                      ck, ckb, rwl, rwlb;
   logic [ DWIDTH - 1 : 0 ] [ NBLKS - 1 : 0 ]    y;
   logic                    [ NBLKS - 1 : 0 ]    z;
   
   
                         
   generate
      for (genvar s = 0; s < NSIDES; ++s)
        begin : gen_latches_loop_0
           for (genvar b = 0; b < NBLKS; ++b)
             begin : gen_latches_loop_1
                localparam BASE  = b * MAXBLKDEPTH;
                localparam DEPTH = (b == NBLKS - 1)? LASTBLKDEPTH : MAXBLKDEPTH;
                
                                   
                latchblk #(DEPTH)
                  m_latchblk (.ck  (),
                              .ckb (),
                              .rwl (),
                              .rwlb(),
                              .dx  (),
                              .z   (z[b]),
                              .q   (),
                              .y   ()
                              );
             end
        end
   endgenerate

endmodule // lambmodel

`endif

`default_nettype wire
