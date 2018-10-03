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



module igr_epl_shim_seg 
  import mby_igr_pkg::*;
(
  input logic cclk,
  input logic rst,
  input data64_w_ecc_t [0:7]  i_rx_data,
  input epl_md_t               i_seg_md,
  input shimfsel_t            i_seg_sel,
  input logic [7:0]            i_seg_we,
  output data64_w_ecc_t [0:7]  o_seg_data
);

  data64_w_ecc_t [0:7]    rx_data_seg;
  data64_w_ecc_t [0:7]  q_rx_data_seg;
  data64_w_ecc_t             data_pad;
  
  assign o_seg_data = q_rx_data_seg;  
  
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
        q_rx_data_seg[g_i] <= (rst)? data_pad: (i_seg_we[g_i])? rx_data_seg[g_i]: q_rx_data_seg[g_i]; 
    end //for
  endgenerate
  
    

endmodule