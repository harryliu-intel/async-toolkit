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
// -- Description  : This block is the epl shim.  
//		     It connects to an EPL generates aligned 64B segments on frame boundary from unaligned 64B EPL data and metadata.
//                   It interfaces with EPL, Packet Buffer(PB).
//------------------------------------------------------------------------------



module igr_epl_shim 
  import mby_igr_pkg::*;
(

  input logic cclk,
  input logic rst_n,  //taken from HLP active low for MBY?
  
// EPL I/O from MBY FS Dataplane Interface signals.
  input logic [7:0]                       rx_ecc,
  input logic [1:0]                  rx_port_num,  
  input logic [7:0]                    rx_data_v,
  input data64_w_ecc_t [0:7]             rx_data,  
  input epl_md_t                           rx_md,
  input epl_ts_t                           rx_ts,  
  input logic                        rx_pfc_xoff,  
  input logic [2:0]           rx_flow_control_tc
  
  
);

 logic rst;
 assign rst = ~rst_n;
//q1 ..qn are stages thru the shim.  Ctrl block is q2->q3

  logic [7:0]                       q1_rx_ecc;
  logic [1:0]                  q1_rx_port_num;
  logic [3:0]                   q1q2_port_num;  //onehot ddecode q1_rx_port_num
  logic [3:0]                         q2_port;  //onehot ddecode q1_rx_port_num
  logic [7:0]                    q1_rx_data_v;
  logic                       q1_or_rx_data_v;
  logic                       q2_or_rx_data_v;    
  epl_md_t                           q1_rx_md;
  epl_ts_t                           q1_rx_ts;  
  data64_w_ecc_t [0:7]             q1_rx_data;
  data64_w_ecc_t [0:7]             q2_rx_data;
  data64_w_ecc_t [0:7]             q3_p0_rx_data;
  data64_w_ecc_t [0:7]             q3_p1_rx_data;
  data64_w_ecc_t [0:7]             q3_p2_rx_data;
  data64_w_ecc_t [0:7]             q3_p3_rx_data;
  data64_w_ecc_t [0:7]         rx_data_align;
  logic                        q1_rx_pfc_xoff;  
  logic [2:0]           q1_rx_flow_control_tc;
  shimfsel_t                        shimfsel_p0;
  shimfsel_t                        shimfsel_p1;
  shimfsel_t                        shimfsel_p2;
  shimfsel_t                        shimfsel_p3;
  logic [2:0]                          seg_p0_e;
  logic [2:0]                          seg_p1_e;
  logic [2:0]                          seg_p2_e;
  logic [2:0]                          seg_p3_e;

  logic [23:0]                        seg_p0_we;
  logic [23:0]                        seg_p1_we;
  logic [23:0]                        seg_p2_we;
  logic [23:0]                        seg_p3_we;
      
   epl_md_t                           seg0_p0_md;  //FIXME need copy for each port?
   epl_md_t                           seg1_p0_md;
   epl_md_t                           seg2_p0_md;
   epl_md_t                           seg0_p1_md;  //FIXME need copy for each port?
   epl_md_t                           seg1_p1_md;
   epl_md_t                           seg2_p1_md;
   epl_md_t                           seg0_p2_md;  //FIXME need copy for each port?
   epl_md_t                           seg1_p2_md;
   epl_md_t                           seg2_p2_md;
   epl_md_t                           seg0_p3_md;  //FIXME need copy for each port?
   epl_md_t                           seg1_p3_md;
   epl_md_t                           seg2_p3_md;

   
//EPL input buffers interface to partition so no clk gating
  always_ff @(posedge cclk) q1_rx_ecc             <= rx_ecc;
  always_ff @(posedge cclk) q1_rx_port_num        <= rx_port_num;
  always_ff @(posedge cclk) q1_rx_data_v          <= rx_data_v;
  always_ff @(posedge cclk) q1_rx_md              <= rx_md;
  always_ff @(posedge cclk) q1_rx_ts              <= rx_ts;
  always_ff @(posedge cclk) q1_rx_pfc_xoff        <= rx_pfc_xoff;
  always_ff @(posedge cclk) q1_rx_flow_control_tc <= rx_flow_control_tc;

  assign q1q2_port_num = {(q1_rx_port_num == 2'b11),
                          (q1_rx_port_num == 2'b10),
                          (q1_rx_port_num == 2'b01),
                          (q1_rx_port_num == 2'b00)};
  always_ff @(posedge cclk) q2_port <= q1q2_port_num;
  
//rx datapath delay stages begin  
  assign q1_or_rx_data_v = |q1_rx_data_v;
  always_ff @(posedge cclk) q2_or_rx_data_v <= q1_rx_data_v;
  always_ff @(posedge cclk) begin
    if(rst)                  q2_rx_data <= '0;
    else if(q1_or_rx_data_v) q2_rx_data <= q1_rx_data;    
  end
//logical port0  
  always_ff @(posedge cclk) begin
    if(rst) q3_p0_rx_data <= '0;
    else if(q2_or_rx_data_v && q2_port[0]) q3_p0_rx_data <= q2_rx_data;
  end
//logical port1  
  always_ff @(posedge cclk) begin
    if(rst)                                q3_p1_rx_data <= '0;
    else if(q2_or_rx_data_v && q2_port[1]) q3_p1_rx_data <= q2_rx_data;
  end
//logical port2  
  always_ff @(posedge cclk) begin
    if(rst)                                q3_p2_rx_data <= '0;
    else if(q2_or_rx_data_v && q2_port[2]) q3_p2_rx_data <= q2_rx_data;
  end
//logical port3  
  always_ff @(posedge cclk) begin
    if(rst)                                q3_p3_rx_data <= '0;
    else if(q2_or_rx_data_v && q2_port[3]) q3_p3_rx_data <= q2_rx_data;
  end    
//rx datapath delay stages end  
  
  assign q1q2_port_num = {(q1_rx_port_num == 2'b11),
                          (q1_rx_port_num == 2'b10),
                          (q1_rx_port_num == 2'b01),
                          (q1_rx_port_num == 2'b00)};
  always_ff @(posedge cclk) q2_port <= q1q2_port_num;
  
  igr_epl_shim_ctrl ctrl0(
    .cclk(cclk),
    .rst_n(rst_n),
    .rx_port_e(q2_port[0]),
    .rx_data_v(q1_rx_data_v),  //q2 stage inside ctrl block
    .rx_md(q1_rx_md),          //q2 stage inside ctrl block
    .o_shimfsel(shimfsel_p0),
    .o_seg_we(seg_p0_we),
    .o_seg_e(seg_p0_e),
    .o_seg0_md(seg0_p0_md),
    .o_seg1_md(seg1_p0_md),
    .o_seg2_md(seg2_p0_md)
  );
  
    igr_epl_shim_ctrl ctrl1(
    .cclk(cclk),
    .rst_n(rst_n),
    .rx_port_e(q2_port[1]),
    .rx_data_v(q1_rx_data_v),  //q2 stage inside ctrl block
    .rx_md(q1_rx_md),          //q2 stage inside ctrl block
    .o_shimfsel(shimfsel_p1),
    .o_seg_we(seg_p1_we),
    .o_seg_e(seg_p1_e),
    .o_seg0_md(seg0_p1_md),
    .o_seg1_md(seg1_p1_md),
    .o_seg2_md(seg2_p1_md)
  );
  
    igr_epl_shim_ctrl ctrl2(
    .cclk(cclk),
    .rst_n(rst_n),
    .rx_port_e(q2_port[2]),
    .rx_data_v(q1_rx_data_v),  //q2 stage inside ctrl block
    .rx_md(q1_rx_md),          //q2 stage inside ctrl block
    .o_shimfsel(shimfsel_p2),
    .o_seg_we(seg_p2_we),
    .o_seg_e(seg_p2_e),
    .o_seg0_md(seg0_p2_md),
    .o_seg1_md(seg1_p2_md),
    .o_seg2_md(seg2_p2_md)
  );
  
    igr_epl_shim_ctrl ctrl3(
    .cclk(cclk),
    .rst_n(rst_n),
    .rx_port_e(q2_port[3]),
    .rx_data_v(q1_rx_data_v),  //q2 stage inside ctrl block
    .rx_md(q1_rx_md),          //q2 stage inside ctrl block
    .o_shimfsel(shimfsel_p3),
    .o_seg_we(seg_p3_we),
    .o_seg_e(seg_p3_e),
    .o_seg0_md(seg0_p3_md),
    .o_seg1_md(seg1_p3_md),
    .o_seg2_md(seg2_p3_md)
  );
  
  

endmodule
