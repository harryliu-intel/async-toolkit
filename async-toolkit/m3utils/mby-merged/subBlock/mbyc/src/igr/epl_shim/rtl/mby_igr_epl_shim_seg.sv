//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
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
// -- Author       : Edward C. Ross
// -- Project Name : Madison Bay
// -- Description  : This block is a 64B segments of aligned EPL data that will be written to the  
//                   Packet Buffer(PB).
// -- Assumptions  :
//------------------------------------------------------------------------------



module mby_igr_epl_shim_seg 
  import mby_igr_pkg::*;
(
  input logic cclk,
  input logic rst,
  input data64_w_ecc_t [0:7]    i_rx_data,
  input epl_ts_t                  i_rx_ts,
  input epl_md_t                 i_seg_md,
  input logic                 i_seg_sop_e,
  input shimfsel_t              i_seg_sel,
  input logic [7:0]              i_seg_we,
  input logic                    i_seg_e,
  output shim_ts_md_t          o_seg_ts_md,
  output data64_w_ecc_t [0:7]  o_seg_data
);

  data64_w_ecc_t [0:7]    rx_data_seg;
  data64_w_ecc_t [0:7]  s4q_rx_data_seg;
  data64_w_ecc_t             data_pad;
  shim_ts_md_t             s4q_seg_ts_md;
  
  logic       seg_sop;
  logic [1:0] seg_sop_err;
  logic [2:0] seg_sop_tc;
  epl_ts_t    seg_sop_ts;
  
  assign o_seg_data = s4q_rx_data_seg;  
  assign o_seg_ts_md = s4q_seg_ts_md;
  assign data_pad = {8'h06, 64'h0};  //when sel=8 then write data_pad;
  
  
  
  assign rx_data_seg[0] = (i_seg_sel.s0 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s0 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s0 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s0 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s0 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s0 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s0 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s0 == 3'h7)? i_rx_data[7]: data_pad;
  assign rx_data_seg[1] = (i_seg_sel.s1 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s1 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s1 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s1 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s1 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s1 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s1 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s1 == 3'h7)? i_rx_data[7]: data_pad;
  assign rx_data_seg[2] = (i_seg_sel.s2 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s2 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s2 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s2 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s2 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s2 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s2 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s2 == 3'h7)? i_rx_data[7]: data_pad;
  assign rx_data_seg[3] = (i_seg_sel.s3 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s3 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s3 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s3 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s3 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s3 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s3 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s3 == 3'h7)? i_rx_data[7]: data_pad;                          
  assign rx_data_seg[4] = (i_seg_sel.s4 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s4 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s4 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s4 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s4 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s4 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s4 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s4 == 3'h7)? i_rx_data[7]: data_pad;                          
  assign rx_data_seg[5] = (i_seg_sel.s5 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s5 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s5 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s5 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s5 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s5 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s5 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s5 == 3'h7)? i_rx_data[7]: data_pad;                          
  assign rx_data_seg[6] = (i_seg_sel.s6 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s6 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s6 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s6 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s6 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s6 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s6 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s6 == 3'h7)? i_rx_data[7]: data_pad;                          
  assign rx_data_seg[7] = (i_seg_sel.s6 == 3'h0)? i_rx_data[0]:
                          (i_seg_sel.s6 == 3'h1)? i_rx_data[1]: 
                          (i_seg_sel.s6 == 3'h2)? i_rx_data[2]:
                          (i_seg_sel.s6 == 3'h3)? i_rx_data[3]:
                          (i_seg_sel.s6 == 3'h4)? i_rx_data[4]:
                          (i_seg_sel.s6 == 3'h5)? i_rx_data[5]:
                          (i_seg_sel.s6 == 3'h6)? i_rx_data[6]:
                          (i_seg_sel.s6 == 3'h7)? i_rx_data[7]: data_pad;

  generate
  genvar g_i;
    for(g_i=0; g_i<8; g_i++) begin: gen_shim_seg_q
      always_ff @(posedge cclk)
        s4q_rx_data_seg[g_i] <= (rst)? data_pad: (i_seg_we[g_i])? rx_data_seg[g_i]: s4q_rx_data_seg[g_i]; 
    end //for
  endgenerate
  
  
//metadata
//Unaligned segments can have eop sop in the same unaligned segment
//certain md fields are only valid with sop
//seg_sop_e will capture those fields when a sop follows an eop 
//in that case the sop put into another segment and that will be a partial segment
//until the next valid unaligned data segment arrives to complete the partial sop segment

  always_ff @(posedge cclk) begin
    if(rst || i_seg_e) begin
      seg_sop     <= '0;
      seg_sop_err <= '0;
      seg_sop_tc  <= '0;
      seg_sop_ts  <= '0;
    end 
    else if(i_seg_sop_e) begin
      seg_sop     <= i_seg_md.sop;
      seg_sop_err <= i_seg_md.error;
      seg_sop_tc  <= i_seg_md.tc;
      seg_sop_ts  <= i_rx_ts;
    end
  end
  
  always_ff @(posedge cclk) begin
    if(rst) s4q_seg_ts_md <= '0;
    else if(i_seg_e) begin
       s4q_seg_ts_md.md.multi      <= i_seg_md.multi;
       s4q_seg_ts_md.md.fast       <= i_seg_md.fast;
       s4q_seg_ts_md.md.fcs_hint   <= i_seg_md.fcs_hint;
       s4q_seg_ts_md.md.dei        <= i_seg_md.dei;
       s4q_seg_ts_md.md.error[1:0] <= seg_sop_err[1:0] | i_seg_md.error[1:0];
       s4q_seg_ts_md.md.eop        <= i_seg_md.eop;
       s4q_seg_ts_md.md.eop_pos    <= i_seg_md.eop_pos;
       s4q_seg_ts_md.md.byte_pos   <= i_seg_md.byte_pos;
       s4q_seg_ts_md.md.sop        <= (seg_sop)? seg_sop: i_seg_md.sop;
       s4q_seg_ts_md.md.sop_pos    <= i_seg_md.sop_pos;  //should be 0
       s4q_seg_ts_md.md.tc         <= (seg_sop)? seg_sop_tc: i_seg_md.tc;       
       s4q_seg_ts_md.ts            <= (seg_sop)? seg_sop_ts: i_rx_ts;    
    end
  end
  
    

endmodule