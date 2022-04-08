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

module decoder
  #(
    parameter DEPTH  = 4,
    parameter AWIDTH = $clog2(DEPTH)
    )
   (
    input  logic [ AWIDTH - 1 : 0 ]  addr,
    output logic [ DEPTH - 1  : 0 ]  onehot
    );
   
   assign onehot = (1 << addr);
   
endmodule
    
module lambmodel // not flowthrough version
  #(
    parameter DEPTH         =             4,
    parameter DWIDTH        =            10,
    parameter AWIDTH        = $clog2(DEPTH),
    parameter X_ON_NOT_REN  =             1  // what does this mean here?
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
     if (ren)
       radr_reg <= radr;
   
   logic [AWIDTH-1:0] wadr_reg;
   
   always_ff @ (posedge clk) 
     if (wen)
       wadr_reg <= wadr;
   
   logic [ DEPTH - 1 : 0 ] rdec, wdec;
   
   decoder #(DEPTH) m_r_decoder (radr_reg, rdec);
   
   decoder #(DEPTH) m_w_decoder (wadr_reg, wdec);

   logic [DWIDTH-1:0]      wdata_reg;
   
   always_ff @ (posedge clk) 
     if (wen)
       wdata_reg <= wdata;
                             

   logic [ DEPTH - 1  : 0 ]                      ck, ckb, rwl, rwlb;
   logic [ DWIDTH - 1 : 0 ] [ NBLKS - 1 : 0 ]    y;
   logic                    [ NBLKS - 1 : 0 ]    z;

   assign ck   =  wdec;
   assign ckb  = ~wdec;

   assign rwl  =  rdec;
   assign rwlb = ~rdec;

   generate
      for (genvar bb = 0; bb < NBLKS; ++bb)
        begin : gen_z_loop
           localparam BASE  = bb * MAXBLKDEPTH;
           localparam BDEPTH = (bb == NBLKS - 1) ? 
                               LASTBLKDEPTH : MAXBLKDEPTH;

           // z is true if none of the corresponding read strobes is active
           assign z[bb] = ~(|rdec[ BASE + BDEPTH - 1 : BASE ]);
        end
   endgenerate
                
   logic [DWIDTH-1:0] db;

   assign db = ~wdata_reg;

   generate
      for (genvar i = 0; i < DWIDTH; ++i)
        begin : gen_latches_loop_0
           for (genvar b = 0; b < NBLKS; ++b)
             begin : gen_latches_loop_1
                localparam BASE        = b * MAXBLKDEPTH;
                localparam BDEPTH      = (b == NBLKS - 1) ? 
                                         LASTBLKDEPTH : MAXBLKDEPTH;
                localparam BDEPTHOVER2 = (BDEPTH - 1) / 2 + 1;

                latchblk #(BDEPTH)
                  m_latchblk (.ck  (ck   [ BASE + BDEPTH - 1 : BASE ]),
                              .ckb (ckb  [ BASE + BDEPTH - 1 : BASE ]),
                              .rwl (rwl  [ BASE + BDEPTH - 1 : BASE ]),
                              .rwlb(rwlb [ BASE + BDEPTH - 1 : BASE ]),
                              .dx  ({ BDEPTHOVER2 { db[i] } }),
                              .z   (z[b]),
                              .q   ( ),
                              .y   (y[i][b])
                              );
             end
        end
   endgenerate

   generate
      for (genvar ii = 0; ii < DWIDTH; ++ii)
        begin : gen_output_loop
           assign dout[ii] = |(y[ii][NBLKS-1 : 0]);
        end
   endgenerate
           
endmodule // lambmodel

`endif

`default_nettype wire
