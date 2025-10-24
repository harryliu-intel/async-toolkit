// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
// Test bench for naked latch
//
// Author : mika.nystroem@intel.com
// April, 2022
//

`resetall
`default_nettype none

`include "nlatch.sv"

module nlatch_tb #() ();

   localparam CYCLETIME = 10; // 1000 ps cycle
   
   localparam verbose = 0;
//   localparam verbose = 1;
   localparam ITERS = 10000;

   // TB vars follow
   logic first;
   logic wen;
   logic clk;

   // interface vars follow
   logic ck;
   logic ckb;
   logic dx;
   logic q;
   
   //////////////////////////////////////////////////////////////////////
   //
   // D.U.T.
   //
   nlatch dut(.*);

   //////////////////////////////////////////////////////////////////////
   //
   // model state
   //
   logic mem;     // mem model

   initial begin : init_init
      mem = '0;
   end

   //////////////////////////////////////////////////////////////////////
   //
   // clock generator
   //
   initial begin : gen_clk
      clk = '0;

      while (1) begin
         #(CYCLETIME/2.0);

         clk = ~clk;
      end
   end

   //////////////////////////////////////////////////////////////////////
   //
   // input generation
   //
   initial begin : gen_inputs
      first = '1;
      
      repeat (ITERS) begin
         @(negedge clk);
 
         ////////////////////////////////////////////////////////////
         //
         // check previous iteration
         //
         
         ck  = 0;
         ckb = 1;
         
         if (!first) 
            assert(q == mem);


         ////////////////////////////////////////////////////////////
         //
         // update inputs for next step
         //
         
         dx    = $urandom();
         first = '0;
         wen   = $urandom();
                 
         @(posedge clk);
         ck   = wen;
         ckb  = ~wen;

         ////////////////////////////////////////////////////////////
         //
         // update model of hardware
         //

         if (wen)
           mem  = ~dx;
                    
      end

      repeat(10) @(posedge clk);

      $stop();
   end
   
endmodule // lamb_tb


`default_nettype wire
