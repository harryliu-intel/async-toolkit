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
  input logic rst,
  
// EPL I/O from MBY FS Dataplane Interface signals.
  input logic [7:0]                       rx_ecc,
  input logic [1:0]                  rx_port_num,  
  input logic [7:0]                    rx_data_v,
  input data64_w_ecc_t [0:7]             rx_data,  
  input epl_md_t                           rx_md,
  input epl_ts_t                           rx_ts,  
  input logic                        rx_pfc_xoff,  
  input logic [2:0]           rx_flow_control_tc,
  output shim_pb_data_t            o_shim_pb_data_p0,
  output shim_pb_data_t            o_shim_pb_data_p1,
  output shim_pb_data_t            o_shim_pb_data_p2,
  output shim_pb_data_t            o_shim_pb_data_p3,
  output logic [2:0]                  o_shim_pb_v_p0,  
  output logic [2:0]                  o_shim_pb_v_p1,  
  output logic [2:0]                  o_shim_pb_v_p2,  
  output logic [2:0]                  o_shim_pb_v_p3    
);

//s1 ..sn are stages thru the shim.  Ctrl block is s2->s3

  logic [7:0]                       s1q_rx_ecc;
  logic [1:0]                  s1q_rx_port_num;
  logic [3:0]                      s1_port_num;  //onehot ddecode q1_rx_port_num
  logic [3:0]                         s2q_port;  //onehot ddecode q1_rx_port_num
  logic [7:0]                    s1q_rx_data_v;
  logic                        s1_or_rx_data_v;
  logic                       s2q_or_rx_data_v;    
  epl_md_t                           s1q_rx_md;
  epl_ts_t                           s1q_rx_ts;  
  data64_w_ecc_t [0:7]             s1q_rx_data;
  data64_w_ecc_t [0:7]             s2q_rx_data;
  data64_w_ecc_t [0:7]          s3q_p0_rx_data;
  data64_w_ecc_t [0:7]          s3q_p1_rx_data;
  data64_w_ecc_t [0:7]          s3q_p2_rx_data;
  data64_w_ecc_t [0:7]          s3q_p3_rx_data;
  data64_w_ecc_t [0:7]           rx_data_align;
  logic                        s1q_rx_pfc_xoff;  
  logic [2:0]           s1q_rx_flow_control_tc;
  shimfsel_t                        seg0_sel_p0;
  shimfsel_t                        seg1_sel_p0;
  shimfsel_t                        seg2_sel_p0;
  shimfsel_t                        seg0_sel_p1;
  shimfsel_t                        seg1_sel_p1;
  shimfsel_t                        seg2_sel_p1;  
  shimfsel_t                        seg0_sel_p2;
  shimfsel_t                        seg1_sel_p2;
  shimfsel_t                        seg2_sel_p2;
  shimfsel_t                        seg0_sel_p3;
  shimfsel_t                        seg1_sel_p3;
  shimfsel_t                        seg2_sel_p3;
  logic [2:0]                          seg_e_p0;
  logic [2:0]                          seg_e_p1;
  logic [2:0]                          seg_e_p2;
  logic [2:0]                          seg_e_p3;

  logic [7:0]                        seg0_we_p0;
  logic [7:0]                        seg1_we_p0;
  logic [7:0]                        seg2_we_p0;
  logic [7:0]                        seg0_we_p1;
  logic [7:0]                        seg1_we_p1;
  logic [7:0]                        seg2_we_p1;
  logic [7:0]                        seg0_we_p2;
  logic [7:0]                        seg1_we_p2;
  logic [7:0]                        seg2_we_p2;
  logic [7:0]                        seg0_we_p3;
  logic [7:0]                        seg1_we_p3;
  logic [7:0]                        seg2_we_p3;
      
   epl_md_t                           seg0_md_p0;  //FIXME need copy for each port?
   epl_md_t                           seg1_md_p0;
   epl_md_t                           seg2_md_p0;
   epl_md_t                           seg0_md_p1;  //FIXME need copy for each port?
   epl_md_t                           seg1_md_p1;
   epl_md_t                           seg2_md_p1;
   epl_md_t                           seg0_md_p2;  //FIXME need copy for each port?
   epl_md_t                           seg1_md_p2;
   epl_md_t                           seg2_md_p2;
   epl_md_t                           seg0_md_p3;  //FIXME need copy for each port?
   epl_md_t                           seg1_md_p3;
   epl_md_t                           seg2_md_p3;

   
//EPL input buffers interface to partition so no clk gating
  always_ff @(posedge cclk) s1q_rx_ecc             <= rx_ecc;
  always_ff @(posedge cclk) s1q_rx_port_num        <= rx_port_num;
  always_ff @(posedge cclk) s1q_rx_data_v          <= rx_data_v;
  always_ff @(posedge cclk) s1q_rx_md              <= rx_md;
  always_ff @(posedge cclk) s1q_rx_ts              <= rx_ts;
  always_ff @(posedge cclk) s1q_rx_pfc_xoff        <= rx_pfc_xoff;
  always_ff @(posedge cclk) s1q_rx_flow_control_tc <= rx_flow_control_tc;

  assign s1_port_num = {(s1q_rx_port_num == 2'b11),
                          (s1q_rx_port_num == 2'b10),
                          (s1q_rx_port_num == 2'b01),
                          (s1q_rx_port_num == 2'b00)};
  always_ff @(posedge cclk) s2q_port <= s1_port_num;
  
//rx datapath delay stages begin  
  assign s1_or_rx_data_v = |s1q_rx_data_v;
  always_ff @(posedge cclk) s2q_or_rx_data_v <= s1_or_rx_data_v;
  always_ff @(posedge cclk) s1q_rx_data <= rx_data;
  always_ff @(posedge cclk) s2q_rx_data <= (rst)? '0: (s1_or_rx_data_v)? s1q_rx_data: s2q_rx_data;
  
//logical port0  
  always_ff @(posedge cclk) begin
    if(rst)                                  s3q_p0_rx_data <= '0;
    else if(s2q_or_rx_data_v && s2q_port[0]) s3q_p0_rx_data <= s2q_rx_data;
  end
//logical port1  
  always_ff @(posedge cclk) begin
    if(rst)                                  s3q_p1_rx_data <= '0;
    else if(s2q_or_rx_data_v && s2q_port[1]) s3q_p1_rx_data <= s2q_rx_data;
  end
//logical port2  
  always_ff @(posedge cclk) begin
    if(rst)                                  s3q_p2_rx_data <= '0;
    else if(s2q_or_rx_data_v && s2q_port[2]) s3q_p2_rx_data <= s2q_rx_data;
  end
//logical port3  
  always_ff @(posedge cclk) begin
    if(rst)                                  s3q_p3_rx_data <= '0;
    else if(s2q_or_rx_data_v && s2q_port[3]) s3q_p3_rx_data <= s2q_rx_data;
  end    
//rx datapath delay stages end  
  
  assign s1_port_num = {(s1q_rx_port_num == 2'b11),
                        (s1q_rx_port_num == 2'b10),
                        (s1q_rx_port_num == 2'b01),
                        (s1q_rx_port_num == 2'b00)};
  always_ff @(posedge cclk) s2q_port <= s1_port_num;
  
  igr_epl_shim_ctrl ctrl0(
    .cclk(cclk),
    .rst(rst),
    .rx_port_e(s2q_port[0]),
    .rx_data_v(s1q_rx_data_v),  //s2 stage inside ctrl block
    .rx_md(s1q_rx_md),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p0),
    .o_seg1_md(seg1_md_p0),
    .o_seg2_md(seg2_md_p0),
    .o_seg0_sel(seg0_sel_p0),
    .o_seg1_sel(seg1_sel_p0),
    .o_seg2_sel(seg2_sel_p0),
    .o_seg0_we(seg0_we_p0),
    .o_seg1_we(seg1_we_p0),
    .o_seg2_we(seg2_we_p0),
    .o_seg_e(seg_e_p0)
  );
  
  igr_epl_shim_segs segs0(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(s3q_p0_rx_data),
    .i_seg0_md(seg0_md_p0),
    .i_seg1_md(seg1_md_p0),
    .i_seg2_md(seg2_md_p0),
    .i_seg0_sel(seg0_sel_p0),
    .i_seg1_sel(seg1_sel_p0),
    .i_seg2_sel(seg2_sel_p0),
    .i_seg0_we(seg0_we_p0),
    .i_seg1_we(seg1_we_p0),
    .i_seg2_we(seg2_we_p0),
    .i_seg_e(seg_e_p0),
    .o_shim_pb_data(o_shim_pb_data_p0),
    .o_shim_pb_v(o_shim_pb_v_p0)
  );
  
  igr_epl_shim_ctrl ctrl1(
    .cclk(cclk),
    .rst(rst),
    .rx_port_e(s2q_port[1]),
    .rx_data_v(s1q_rx_data_v),  //s2 stage inside ctrl block
    .rx_md(s1q_rx_md),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p1),
    .o_seg1_md(seg1_md_p1),
    .o_seg2_md(seg2_md_p1),
    .o_seg0_sel(seg0_sel_p1),
    .o_seg1_sel(seg1_sel_p1),
    .o_seg2_sel(seg2_sel_p1),
    .o_seg0_we(seg0_we_p1),
    .o_seg1_we(seg1_we_p1),
    .o_seg2_we(seg2_we_p1),
    .o_seg_e(seg_e_p1)
  );
  
  igr_epl_shim_segs segs1(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(s3q_p1_rx_data),
    .i_seg0_md(seg0_md_p1),
    .i_seg1_md(seg1_md_p1),
    .i_seg2_md(seg2_md_p1),
    .i_seg0_sel(seg0_sel_p1),
    .i_seg1_sel(seg1_sel_p1),
    .i_seg2_sel(seg2_sel_p1),
    .i_seg0_we(seg0_we_p1),
    .i_seg1_we(seg1_we_p1),
    .i_seg2_we(seg2_we_p1),
    .i_seg_e(seg_e_p1),
    .o_shim_pb_data(o_shim_pb_data_p1),
    .o_shim_pb_v(o_shim_pb_v_p1)

  );
  
  igr_epl_shim_ctrl ctrl2(
    .cclk(cclk),
    .rst(rst),
    .rx_port_e(s2q_port[2]),
    .rx_data_v(s1q_rx_data_v),  //s2 stage inside ctrl block
    .rx_md(s1q_rx_md),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p2),
    .o_seg1_md(seg1_md_p2),
    .o_seg2_md(seg2_md_p2),
    .o_seg0_sel(seg0_sel_p2),
    .o_seg1_sel(seg1_sel_p2),
    .o_seg2_sel(seg2_sel_p2),
    .o_seg0_we(seg0_we_p2),
    .o_seg1_we(seg1_we_p2),
    .o_seg2_we(seg2_we_p2),
    .o_seg_e(seg_e_p2)
  );
  
  igr_epl_shim_segs segs2(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(s3q_p1_rx_data),
    .i_seg0_md(seg0_md_p2),
    .i_seg1_md(seg1_md_p2),
    .i_seg2_md(seg2_md_p2),
    .i_seg0_sel(seg0_sel_p2),
    .i_seg1_sel(seg1_sel_p2),
    .i_seg2_sel(seg2_sel_p2),
    .i_seg0_we(seg0_we_p2),
    .i_seg1_we(seg1_we_p2),
    .i_seg2_we(seg2_we_p2),
    .i_seg_e(seg_e_p2),
    .o_shim_pb_data(o_shim_pb_data_p2),
    .o_shim_pb_v(o_shim_pb_v_p2)
  );
 
  igr_epl_shim_ctrl ctrl3(
    .cclk(cclk),
    .rst(rst),
    .rx_port_e(s2q_port[3]),
    .rx_data_v(s1q_rx_data_v),  //s2 stage inside ctrl block
    .rx_md(s1q_rx_md),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p3),
    .o_seg1_md(seg1_md_p3),
    .o_seg2_md(seg2_md_p3),
    .o_seg0_sel(seg0_sel_p3),
    .o_seg1_sel(seg1_sel_p3),
    .o_seg2_sel(seg2_sel_p3),
    .o_seg0_we(seg0_we_p3),
    .o_seg1_we(seg1_we_p3),
    .o_seg2_we(seg2_we_p3),
    .o_seg_e(seg_e_p3)
  );
  
  igr_epl_shim_segs segs3(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(s3q_p1_rx_data),
    .i_seg0_md(seg0_md_p3),
    .i_seg1_md(seg1_md_p3),
    .i_seg2_md(seg2_md_p3),
    .i_seg0_sel(seg0_sel_p3),
    .i_seg1_sel(seg1_sel_p3),
    .i_seg2_sel(seg2_sel_p3),
    .i_seg0_we(seg0_we_p3),
    .i_seg1_we(seg1_we_p3),
    .i_seg2_we(seg2_we_p3),
    .i_seg_e(seg_e_p3),
    .o_shim_pb_data(o_shim_pb_data_p3),
    .o_shim_pb_v(o_shim_pb_v_p3)
  );
  

endmodule
