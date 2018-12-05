//
//  Copyright 2018 - 2028 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// -- Author       : John Lo          
// -- Project Name : Madison Bay (MBY)
// -- Description  : 
//
// EPL uses a:d this corresponds to 0:3 in EGR.
//------------------------------------------------------------------------------



module egr_tcu_fifo_gen #(parameter width = 32, addr_bits = 3)(
   output logic [width-1:0]             dout
  ,output logic [addr_bits:0]           empty_space
  ,output logic                         empty
  ,output logic                         aempty
  ,output logic                         full
  ,output logic                         rd_err
  ,output logic                         wr_err
  ,input  logic                         ren        // NOT Qualified yet!!!
  ,input  logic                         wen        // NOT Qualified yet!!!
  ,input  logic [width-1:0]             din
  ,input  logic				reset_n
  ,input  logic				clk
  );

  parameter depth = 1<<addr_bits; // number of entries
  logic [depth-1:0][width-1:0]  mem; 
  logic [addr_bits:0]           rp,wp;
  logic                         wen_QA,ren_QA;

/* --------------- start wp Management ------------------- */
// overflow/underflow should never happen.    
  always_comb begin
    empty_space = (wp[addr_bits] == rp[addr_bits]) ?
       depth - ({1'b0,wp[addr_bits-1:0]} - {1'b0,rp[addr_bits-1:0]}):
                    ({1'b0,rp[addr_bits-1:0]} - {1'b0,wp[addr_bits-1:0]});
    full  = (wp[addr_bits]==(!rp[addr_bits])) &&
            (wp[addr_bits-1:0]==rp[addr_bits-1:0]);
    empty = (wp[addr_bits:0]==rp[addr_bits:0]);
    aempty = empty_space >= depth-1;
  end

  assign  wr_err = wen &  full;
  assign  rd_err = ren &  empty;
  assign  ren_QA = ren & ~empty;
  assign  wen_QA = wen & ~full;

  //rp
  always_ff @ (posedge clk)
    if (!reset_n)
      rp <= '0;
    else if (ren_QA)
      rp <= rp+1;

  // wp
  always_ff @ (posedge clk) 
    if (!reset_n)
      wp <= '0;
    else if (wen_QA)
      wp <= wp+1;
 
/* --------------- memory instantiation ---------------------- */
  always_ff @ (posedge clk) 
    if (!reset_n)
      mem     <= '0;
    else if (wen_QA)
      mem[wp[addr_bits-1:0]] <= din;
    
  always_comb   dout =  mem[rp[addr_bits-1:0]];
 

endmodule : egr_tcu_fifo_gen

