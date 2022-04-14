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
// Structural model of latch blocks (LATCHBLKS) used in LAMBs
//
// Author : mika.nystroem@intel.com
// April, 2022
//

`resetall
`default_nettype none

`ifndef _LATCHBLK_SV
`define _LATCHBLK_SV

`include "latch.sv"

module latchblk
  #(
    parameter DEPTH      = 4,
    parameter DEPTHOVER2 = (DEPTH - 1) / 2 + 1
    )
   (
    // inputs
    // in the following actH means active high; actL means active low
    
    input logic [ DEPTH - 1 : 0 ]     ck , 
    // write strobe          actH

    input logic [ DEPTH - 1 : 0 ]     ckb , 
    // write strobe inverted actL
    
    input logic [ DEPTH - 1 : 0 ]     rwl , 
    // read  strobe          actH
    
    input logic [ DEPTH - 1 : 0 ]     rwlb, 
    // read  strobe inverted actL
    
    input logic [ DEPTHOVER2 - 1 : 0] dx , 
    // D input inverted
    
    input logic                       z , 
    // force shared output low actH
    
    //output logic  [ DEPTH - 1 : 0 ]     q, // port removed in N3

    output logic                      y     
    // shared output
    );

   logic    [ DEPTHOVER2 - 1 : 0 ]     d     ;
   logic    [ DEPTH - 1      : 0 ]     cken  ;
   logic    [ DEPTH - 1      : 0 ]     rwlen ;
   logic    [ DEPTH - 1      : 0 ]     dout  ;
   logic                               y1    ;
   logic [ DEPTH - 1 : 0 ]             q     ; // replacing port

   always_comb d    = ~dx;
   assign cken = ~ckb & ck;
   
   generate
      for (genvar i=0; i < DEPTH; ++i)
        LATCH m_latch (q[i], cken[i], d[i/2]);
   endgenerate

   assign rwlen = ~rwlb & rwl;
   assign dout  = rwlen & q;
   assign y1 = |dout;
   assign y  = ~z & y1;

   //synopsys translate_off
   generate
      
      for (genvar ai=0; ai < DEPTH; ++ai)
        always_comb begin : assert_strobe_blk

           AssertWritesComplementary:
             // the !== '0 syntax allows us to handle the X case on reset
             assert final ((~ck[ai] & ckb[ai]) | (ck[ai] & ~ckb[ai]) !== '0);

           AssertReadsComplementary:
             // the !== '0 syntax allows us to handle the X case on reset
             assert final ((~rwl[ai] & rwlb[ai]) | (rwl[ai] & ~rwlb[ai]) !== '0);
        end
      
   endgenerate
      
   always_comb begin : assert_z_blk
      AssertZ:
        // the !== '0 syntax allows us to handle the X case on reset
        assert final ((~z & |(rwl)) | (z & ~|(rwl)) !== '0);
                     
   end  
   //synopsys translate_on
endmodule // latchblk

`endif // !_LATCHBLK_SV

`default_nettype wire
