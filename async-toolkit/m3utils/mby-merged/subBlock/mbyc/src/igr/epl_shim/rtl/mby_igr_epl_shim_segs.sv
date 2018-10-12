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
// -- Description  : This block is 3 64B segments of aligned EPL data that will be written to the  
//                   Packet Buffer(PB).
// -- Assumptions  :
//------------------------------------------------------------------------------



module mby_igr_epl_shim_segs 
  import mby_igr_pkg::*;
(
  input logic cclk,
  input logic rst,  //taken from HLP active low for MBY?
  input data64_w_ecc_t [0:7]   i_rx_data,
  input epl_ts_t                 i_rx_ts,
  input epl_md_t               i_seg0_md,
  input epl_md_t               i_seg1_md,
  input epl_md_t               i_seg2_md,
  input logic               i_seg0_sop_e,
  input logic               i_seg1_sop_e,
  input logic               i_seg2_sop_e,
  input shimfsel_t            i_seg0_sel,
  input shimfsel_t            i_seg1_sel,
  input shimfsel_t            i_seg2_sel,
  input logic [7:0]            i_seg0_we,
  input logic [7:0]            i_seg1_we,
  input logic [7:0]            i_seg2_we,
  input logic [2:0]              i_seg_e,  //q3 dly
  output shim_pb_data_t   o_shim_pb_data,
  output shim_pb_md_t       o_shim_pb_md,
  output logic [2:0]         o_shim_pb_v
);


  data64_w_ecc_t [0:7] seg0_data;
  data64_w_ecc_t [0:7] seg1_data;
  data64_w_ecc_t [0:7] seg2_data;
  shim_ts_md_t         seg_ts_md0;
  shim_ts_md_t         seg_ts_md1;
  shim_ts_md_t         seg_ts_md2;

// 2 segemts may have to write at the same time but only one segment write to PB per cycle
// counter will accumulate extra writes and drain accumulated writes until zero.
// Never 3 segment writes so segments will not be over run

  logic [2:0]       s4q_seg_pb_e;
  
//outputs begin 
  assign o_seg_pb_e    = s4q_seg_pb_e;
  assign o_shim_pb_data.seg2 = seg2_data;
  assign o_shim_pb_data.seg1 = seg1_data;
  assign o_shim_pb_data.seg0 = seg0_data;
  assign o_shim_pb_md.md0    = seg_ts_md0;
  assign o_shim_pb_md.md1    = seg_ts_md1;
  assign o_shim_pb_md.md2    = seg_ts_md2;

  assign o_shim_pb_v         = s4q_seg_pb_e;

//outputs end
 
  always_ff @(posedge cclk) s4q_seg_pb_e <= i_seg_e;  

  
  mby_igr_epl_shim_seg seg0(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(i_rx_data),
    .i_rx_ts(i_rx_ts),
    .i_seg_md(i_seg0_md),
    .i_seg_sop_e(i_seg0_sop_e),
    .i_seg_sel(i_seg0_sel),
    .i_seg_we(i_seg0_we),
    .i_seg_e(i_seg_e[0]),
    .o_seg_ts_md(seg_ts_md0),
    .o_seg_data(seg0_data)  
  );

  mby_igr_epl_shim_seg seg1(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(i_rx_data),
    .i_rx_ts(i_rx_ts),
    .i_seg_md(i_seg1_md),
    .i_seg_sop_e(i_seg1_sop_e),    
    .i_seg_sel(i_seg1_sel),
    .i_seg_we(i_seg1_we),
    .i_seg_e(i_seg_e[1]),
    .o_seg_ts_md(seg_ts_md1),
    .o_seg_data(seg1_data)    
  );  

  mby_igr_epl_shim_seg seg2(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(i_rx_data),
    .i_seg_md(i_seg2_md),
    .i_rx_ts(i_rx_ts),
    .i_seg_sop_e(i_seg2_sop_e),
    .i_seg_sel(i_seg2_sel),
    .i_seg_we(i_seg2_we),
    .i_seg_e(i_seg_e[2]),
    .o_seg_ts_md(seg_ts_md2),
    .o_seg_data(seg2_data)    
  );  


endmodule