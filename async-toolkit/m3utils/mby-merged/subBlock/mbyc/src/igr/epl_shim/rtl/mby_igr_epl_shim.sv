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



module mby_igr_epl_shim 
  import mby_igr_pkg::*;
(

  input logic cclk,
  input logic rst,
  
// EPL I/O from MBY FS Dataplane Interface signals.
//  input  logic [1:0]                     rx_port_num,  
//  input  logic [7:0]                       rx_data_v,
//  input  data64_w_ecc_t [0:7]                rx_data,
  input  dpc_pb_t       [PB_BANKS-1:0]    i_pb_shim,
  output shim_pb_data_t            o_shim_pb_data_p0,
  output shim_pb_data_t            o_shim_pb_data_p1,
  output shim_pb_data_t            o_shim_pb_data_p2,
  output shim_pb_data_t            o_shim_pb_data_p3,
  output shim_pb_md_t                o_shim_pb_md_p0,
  output shim_pb_md_t                o_shim_pb_md_p1,
  output shim_pb_md_t                o_shim_pb_md_p2,
  output shim_pb_md_t                o_shim_pb_md_p3,
  output logic [2:0]                  o_shim_pb_v_p0,  
  output logic [2:0]                  o_shim_pb_v_p1,  
  output logic [2:0]                  o_shim_pb_v_p2,  
  output logic [2:0]                  o_shim_pb_v_p3    
);

//s1 ..sn are stages thru the shim.  Ctrl block is s2->s3

  logic [3:0]                          s1_port;  //onehot ddecode q1_rx_port_num
  logic [3:0]                         qs1_port;  //onehot ddecode q1_rx_port_num
  dpc_ts_md_t                      qs1_tsmd_p0;
  dpc_ts_md_t                      qs1_tsmd_p1;
  dpc_ts_md_t                      qs1_tsmd_p2;
  dpc_ts_md_t                      qs1_tsmd_p3;
  data64_w_ecc_t [0:7]          qs1_data_p0;
  data64_w_ecc_t [0:7]          qs1_data_p1;
  data64_w_ecc_t [0:7]          qs1_data_p2;
  data64_w_ecc_t [0:7]          qs1_data_p3;
  data64_w_ecc_t [0:7]          qs2_data_p0;
  data64_w_ecc_t [0:7]          qs2_data_p1;
  data64_w_ecc_t [0:7]          qs2_data_p2;
  data64_w_ecc_t [0:7]          qs2_data_p3;
  data64_w_ecc_t [0:7]          qs3_data_p0;
  data64_w_ecc_t [0:7]          qs3_data_p1;
  data64_w_ecc_t [0:7]          qs3_data_p2;
  data64_w_ecc_t [0:7]          qs3_data_p3;

  data64_w_ecc_t [0:7]           rx_data_align;
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
      
   shim_md_t                           seg0_md_p0;  //FIXME need copy for each port?
   shim_md_t                           seg1_md_p0;
   shim_md_t                           seg2_md_p0;
   shim_md_t                           seg0_md_p1;  //FIXME need copy for each port?
   shim_md_t                           seg1_md_p1;
   shim_md_t                           seg2_md_p1;
   shim_md_t                           seg0_md_p2;  //FIXME need copy for each port?
   shim_md_t                           seg1_md_p2;
   shim_md_t                           seg2_md_p2;
   shim_md_t                           seg0_md_p3;  //FIXME need copy for each port?
   shim_md_t                           seg1_md_p3;
   shim_md_t                           seg2_md_p3;
   epl_ts_t                           seg0_ts_p0;  //FIXME need copy for each port?
   epl_ts_t                           seg1_ts_p0;
   epl_ts_t                           seg2_ts_p0;
   epl_ts_t                           seg0_ts_p1;  //FIXME need copy for each port?
   epl_ts_t                           seg1_ts_p1;
   epl_ts_t                           seg2_ts_p1;
   epl_ts_t                           seg0_ts_p2;  //FIXME need copy for each port?
   epl_ts_t                           seg1_ts_p2;
   epl_ts_t                           seg2_ts_p2;
   epl_ts_t                           seg0_ts_p3;  //FIXME need copy for each port?
   epl_ts_t                           seg1_ts_p3;
   epl_ts_t                           seg2_ts_p3;

  logic                               seg0_sop_e_p0;
  logic                               seg1_sop_e_p0;
  logic                               seg2_sop_e_p0;
  logic                               seg0_sop_e_p1;
  logic                               seg1_sop_e_p1;
  logic                               seg2_sop_e_p1;
  logic                               seg0_sop_e_p2;
  logic                               seg1_sop_e_p2;
  logic                               seg2_sop_e_p2;
  logic                               seg0_sop_e_p3;
  logic                               seg1_sop_e_p3;
  logic                               seg2_sop_e_p3;
  

  assign s1_port = {i_pb_shim[3].v,
                    i_pb_shim[2].v,
                    i_pb_shim[1].v,
                    i_pb_shim[0].v};
  always_ff @(posedge cclk) qs1_port <= s1_port;
  

//logical port0  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs1_tsmd_p0 <= '0;
      qs1_data_p0 <= '0;
    end    
    else if(i_pb_shim[0].v) begin
      qs1_tsmd_p0 <= i_pb_shim[0].tsmd;
      qs1_data_p0 <= i_pb_shim[0].d;
    end
  end
 always_ff @(posedge cclk) qs2_data_p0 <= qs1_data_p0;  //FIXME clk gate
 always_ff @(posedge cclk) qs3_data_p0 <= qs2_data_p0;

//logical port1  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs1_tsmd_p1 <= '0;
      qs1_data_p1 <= '0;
    end    
    else if(i_pb_shim[1].v) begin
      qs1_tsmd_p1 <= i_pb_shim[1].tsmd;
      qs1_data_p1 <= i_pb_shim[1].d;
    end
  end
 always_ff @(posedge cclk) qs2_data_p1 <= qs1_data_p1;  //FIXME clk gate
 always_ff @(posedge cclk) qs3_data_p1 <= qs2_data_p1;

//logical port2  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs1_tsmd_p2 <= '0;
      qs1_data_p2 <= '0;
    end    
    else if(i_pb_shim[2].v) begin
      qs1_tsmd_p2 <= i_pb_shim[2].tsmd;
      qs1_data_p2 <= i_pb_shim[2].d;
    end
  end
 always_ff @(posedge cclk) qs2_data_p2 <= qs1_data_p2;  //FIXME clk gate
 always_ff @(posedge cclk) qs3_data_p2 <= qs2_data_p2;

//logical port3  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs1_tsmd_p3 <= '0;
      qs1_data_p3 <= '0;
    end    
    else if(i_pb_shim[3].v) begin
      qs1_tsmd_p3 <= i_pb_shim[3].tsmd;
      qs1_data_p3 <= i_pb_shim[3].d;
    end
  end
 always_ff @(posedge cclk) qs2_data_p3 <= qs1_data_p3;  //FIXME clk gate
 always_ff @(posedge cclk) qs3_data_p3 <= qs2_data_p3;

//rx datapath delay stages end  
  
  mby_igr_epl_shim_ctrl ctrl0(
    .cclk(cclk),
    .rst(rst),
    .i_port_e(qs1_port[0]),
    .i_tsmd(qs1_tsmd_p0),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p0),
    .o_seg1_md(seg1_md_p0),
    .o_seg2_md(seg2_md_p0),
    .o_seg0_ts(seg0_ts_p0),
    .o_seg1_ts(seg1_ts_p0),
    .o_seg2_ts(seg2_ts_p0),
    .o_seg0_sop_e(seg0_sop_e_p0),
    .o_seg1_sop_e(seg1_sop_e_p0),
    .o_seg2_sop_e(seg2_sop_e_p0),
    .o_seg0_sel(seg0_sel_p0),
    .o_seg1_sel(seg1_sel_p0),
    .o_seg2_sel(seg2_sel_p0),
    .o_seg0_we(seg0_we_p0),
    .o_seg1_we(seg1_we_p0),
    .o_seg2_we(seg2_we_p0),
    .o_seg_e(seg_e_p0)
  );
  
  mby_igr_epl_shim_segs segs0(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(qs3_data_p0),
    .i_seg0_ts(seg0_ts_p0),
    .i_seg1_ts(seg1_ts_p0),
    .i_seg2_ts(seg2_ts_p0),
    .i_seg0_md(seg0_md_p0),
    .i_seg1_md(seg1_md_p0),
    .i_seg2_md(seg2_md_p0),
    .i_seg0_sop_e(seg0_sop_e_p0),
    .i_seg1_sop_e(seg1_sop_e_p0),
    .i_seg2_sop_e(seg2_sop_e_p0),
    .i_seg0_sel(seg0_sel_p0),
    .i_seg1_sel(seg1_sel_p0),
    .i_seg2_sel(seg2_sel_p0),
    .i_seg0_we(seg0_we_p0),
    .i_seg1_we(seg1_we_p0),
    .i_seg2_we(seg2_we_p0),
    .i_seg_e(seg_e_p0),
    .o_shim_pb_data(o_shim_pb_data_p0),
    .o_shim_pb_md(o_shim_pb_md_p0),
    .o_shim_pb_v(o_shim_pb_v_p0)
  );
  
  mby_igr_epl_shim_ctrl ctrl1(
    .cclk(cclk),
    .rst(rst),
    .i_port_e(qs1_port[1]),
    .i_tsmd(qs1_tsmd_p1),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p1),
    .o_seg1_md(seg1_md_p1),
    .o_seg2_md(seg2_md_p1),
    .o_seg0_ts(seg0_ts_p1),
    .o_seg1_ts(seg1_ts_p1),
    .o_seg2_ts(seg2_ts_p1),
    .o_seg0_sop_e(seg0_sop_e_p1),
    .o_seg1_sop_e(seg1_sop_e_p1),
    .o_seg2_sop_e(seg2_sop_e_p1),
    .o_seg0_sel(seg0_sel_p1),
    .o_seg1_sel(seg1_sel_p1),
    .o_seg2_sel(seg2_sel_p1),
    .o_seg0_we(seg0_we_p1),
    .o_seg1_we(seg1_we_p1),
    .o_seg2_we(seg2_we_p1),
    .o_seg_e(seg_e_p1)
  );
  
  mby_igr_epl_shim_segs segs1(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(qs3_data_p1),
    .i_seg0_ts(seg0_ts_p1),
    .i_seg1_ts(seg1_ts_p1),
    .i_seg2_ts(seg2_ts_p1),
    .i_seg0_md(seg0_md_p1),
    .i_seg1_md(seg1_md_p1),
    .i_seg2_md(seg2_md_p1),
    .i_seg0_sop_e(seg0_sop_e_p1),
    .i_seg1_sop_e(seg1_sop_e_p1),
    .i_seg2_sop_e(seg2_sop_e_p1),
    .i_seg0_sel(seg0_sel_p1),
    .i_seg1_sel(seg1_sel_p1),
    .i_seg2_sel(seg2_sel_p1),
    .i_seg0_we(seg0_we_p1),
    .i_seg1_we(seg1_we_p1),
    .i_seg2_we(seg2_we_p1),
    .i_seg_e(seg_e_p1),
    .o_shim_pb_data(o_shim_pb_data_p1),
    .o_shim_pb_md(o_shim_pb_md_p1),
    .o_shim_pb_v(o_shim_pb_v_p1)

  );
  
  mby_igr_epl_shim_ctrl ctrl2(
    .cclk(cclk),
    .rst(rst),
    .i_port_e(qs1_port[2]),
    .i_tsmd(qs1_tsmd_p2),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p2),
    .o_seg1_md(seg1_md_p2),
    .o_seg2_md(seg2_md_p2),
    .o_seg0_ts(seg0_ts_p2),
    .o_seg1_ts(seg1_ts_p2),
    .o_seg2_ts(seg2_ts_p2),
    .o_seg0_sop_e(seg0_sop_e_p2),
    .o_seg1_sop_e(seg1_sop_e_p2),
    .o_seg2_sop_e(seg2_sop_e_p2),
    .o_seg0_sel(seg0_sel_p2),
    .o_seg1_sel(seg1_sel_p2),
    .o_seg2_sel(seg2_sel_p2),
    .o_seg0_we(seg0_we_p2),
    .o_seg1_we(seg1_we_p2),
    .o_seg2_we(seg2_we_p2),
    .o_seg_e(seg_e_p2)
  );
  
  mby_igr_epl_shim_segs segs2(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(qs3_data_p2),
    .i_seg0_ts(seg0_ts_p2),
    .i_seg1_ts(seg1_ts_p2),
    .i_seg2_ts(seg2_ts_p2),
    .i_seg0_md(seg0_md_p2),
    .i_seg1_md(seg1_md_p2),
    .i_seg2_md(seg2_md_p2),
    .i_seg0_sop_e(seg0_sop_e_p2),
    .i_seg1_sop_e(seg1_sop_e_p2),
    .i_seg2_sop_e(seg2_sop_e_p2),
    .i_seg0_sel(seg0_sel_p2),
    .i_seg1_sel(seg1_sel_p2),
    .i_seg2_sel(seg2_sel_p2),
    .i_seg0_we(seg0_we_p2),
    .i_seg1_we(seg1_we_p2),
    .i_seg2_we(seg2_we_p2),
    .i_seg_e(seg_e_p2),
    .o_shim_pb_data(o_shim_pb_data_p2),
    .o_shim_pb_md(o_shim_pb_md_p2),
    .o_shim_pb_v(o_shim_pb_v_p2)
  );
 
  mby_igr_epl_shim_ctrl ctrl3(
    .cclk(cclk),
    .rst(rst),
    .i_port_e(qs1_port[3]),
    .i_tsmd(qs1_tsmd_p3),          //s2 stage inside ctrl block
    .o_seg0_md(seg0_md_p3),
    .o_seg1_md(seg1_md_p3),
    .o_seg2_md(seg2_md_p3),
    .o_seg0_ts(seg0_ts_p3),
    .o_seg1_ts(seg1_ts_p3),
    .o_seg2_ts(seg2_ts_p3),
    .o_seg0_sop_e(seg0_sop_e_p3),
    .o_seg1_sop_e(seg1_sop_e_p3),
    .o_seg2_sop_e(seg2_sop_e_p3),
    .o_seg0_sel(seg0_sel_p3),
    .o_seg1_sel(seg1_sel_p3),
    .o_seg2_sel(seg2_sel_p3),
    .o_seg0_we(seg0_we_p3),
    .o_seg1_we(seg1_we_p3),
    .o_seg2_we(seg2_we_p3),
    .o_seg_e(seg_e_p3)
  );
  
  mby_igr_epl_shim_segs segs3(
    .cclk(cclk),
    .rst(rst),
    .i_rx_data(qs3_data_p3),
    .i_seg0_ts(seg0_ts_p3),
    .i_seg1_ts(seg1_ts_p3),
    .i_seg2_ts(seg2_ts_p3),
    .i_seg0_md(seg0_md_p3),
    .i_seg1_md(seg1_md_p3),
    .i_seg2_md(seg2_md_p3),
    .i_seg0_sop_e(seg0_sop_e_p3),
    .i_seg1_sop_e(seg1_sop_e_p3),
    .i_seg2_sop_e(seg2_sop_e_p3),
    .i_seg0_sel(seg0_sel_p3),
    .i_seg1_sel(seg1_sel_p3),
    .i_seg2_sel(seg2_sel_p3),
    .i_seg0_we(seg0_we_p3),
    .i_seg1_we(seg1_we_p3),
    .i_seg2_we(seg2_we_p3),
    .i_seg_e(seg_e_p3),
    .o_shim_pb_data(o_shim_pb_data_p3),
    .o_shim_pb_md(o_shim_pb_md_p3),
    .o_shim_pb_v(o_shim_pb_v_p3)
  );
  

endmodule
