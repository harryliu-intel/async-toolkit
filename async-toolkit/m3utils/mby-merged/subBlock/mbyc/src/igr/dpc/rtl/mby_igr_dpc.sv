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
// -- Description  : This block is the IGR interface to EPL, controls packet Drops and packet Pause Control.  
//                   It interfaces with EPL, Packet Buffer(PB).
//------------------------------------------------------------------------------



module mby_igr_dpc 
  import mby_igr_pkg::*;
(

  input logic cclk,
  input logic rst,
    
// EPL I/O from MBY FS Dataplane Interface signals.
  input  logic [7:0]                          rx_ecc,
  input  logic [1:0]                     rx_port_num,  
  input  logic [7:0]                       rx_data_v,
  input  logic [63:0][0:7]                   rx_data,  
  input  epl_md_t                              rx_md,
  input  epl_ts_t                              rx_ts,  
  output logic [3:0]                     tx_pfc_xoff, //[3]=port3,[2]=port2,[1]=port1,[0]=port0
  output logic                        tx_pfc_tc_sync, //tc0 sync pulse to tx mac
  output dpc_pb_t                     o_dpc_pb_p0,
  output dpc_pb_t                     o_dpc_pb_p1,    
  output dpc_pb_t                     o_dpc_pb_p2,    
  output dpc_pb_t                     o_dpc_pb_p3    
);

  logic                                  q_rst;
  logic [7:0]                       qs1_rx_ecc;
  logic [1:0]                  qs1_rx_port_num;
  logic [3:0]                      s1_port_num;  //onehot ddecode q1_rx_port_num
  logic [3:0]                      s1_cnt_ones;
  logic [3:0]                       qs2_port_v;  //onehot ddecode q1_rx_port_num
  logic [7:0]                    qs1_rx_data_v;
  logic                        s1_or_rx_data_v;
  logic                       qs2_or_rx_data_v;    
  epl_md_t                           qs1_rx_md;
  dpc_md_t                           s1_dpc_md;
  dpc_md_t                       qs2_dpc_md_p0;
  dpc_md_t                       qs2_dpc_md_p1;
  dpc_md_t                       qs2_dpc_md_p2;
  dpc_md_t                       qs2_dpc_md_p3;
  epl_ts_t                           qs1_rx_ts;
  epl_ts_t                       qs2_dpc_ts_p0;
  epl_ts_t                       qs2_dpc_ts_p1;
  epl_ts_t                       qs2_dpc_ts_p2;
  epl_ts_t                       qs2_dpc_ts_p3;
  logic [63:0][0:7]             qs1_rx_data;
  logic [63:0][0:7]          qs2_rx_data_p0;
  logic [63:0][0:7]          qs2_rx_data_p1;
  logic [63:0][0:7]          qs2_rx_data_p2;
  logic [63:0][0:7]          qs2_rx_data_p3;
  logic [63:0][0:7]           rx_data_align;
  logic [7:0]                  qs1_pfc_tc_sync;  // free-running shift register. [0] is sync pulse every eighth clk to tx mac for tc[0].
  logic                       qs2_pfc_tc_sync0;
  logic                                tc_sync;
//outputs start here
  assign o_dpc_pb_p0.v     = qs2_port_v[0];
  assign o_dpc_pb_p0.tsmd  = {qs2_dpc_ts_p0, qs2_dpc_md_p0};
  assign o_dpc_pb_p0.d     = qs2_rx_data_p0;
  assign o_dpc_pb_p1.v     = qs2_port_v[1];
  assign o_dpc_pb_p1.tsmd  = {qs2_dpc_ts_p1, qs2_dpc_md_p1};
  assign o_dpc_pb_p1.d     = qs2_rx_data_p1;
  assign o_dpc_pb_p2.v     = qs2_port_v[2];
  assign o_dpc_pb_p2.tsmd  = {qs2_dpc_ts_p2, qs2_dpc_md_p2};
  assign o_dpc_pb_p2.d     = qs2_rx_data_p2;
  assign o_dpc_pb_p3.v     = qs2_port_v[3];
  assign o_dpc_pb_p3.tsmd  = {qs2_dpc_ts_p3, qs2_dpc_md_p3};
  assign o_dpc_pb_p3.d     = qs2_rx_data_p3;

  assign tx_pfc_tc_sync    = qs2_pfc_tc_sync0;
// port strobe for pfc xoff, this must be sync'd with tx_pfc_tc_sync. exapmle tx_pfc_xoff[0]=1 when tx_pfc_tc_sync=1 then port0:tc0 pfc to tx mac
// tx_pfc_xoff[0]=1 1 clk after tx_pfc_tc_sync=1 then port0:tc1 pfc to tx mac, tx_pfc_xoff[0]=1  when tx_pfc_tc_sync=1 and 1 clk after tx_pfc_tc_sync=1 then port0:tc0,tc1 pfc to tx mac
//see HSD 2007752976
  assign tx_pfc_xoff       = 4'h0; //FIXME drive this with per port TC strobes 
//outputs finish here

  always_ff @(posedge cclk) q_rst <= rst;
  
//EPL input buffers interface to partition so no clk gating
  always_ff @(posedge cclk) qs1_rx_ecc             <= rx_ecc;
  always_ff @(posedge cclk) qs1_rx_port_num        <= rx_port_num;
  always_ff @(posedge cclk) qs1_rx_data_v          <= rx_data_v;
  always_ff @(posedge cclk) qs1_rx_md              <= rx_md;
  always_ff @(posedge cclk) qs1_rx_ts              <= rx_ts;
  
//pfc tc sync
  assign tc_sync = ((~ rst) & q_rst) | qs1_pfc_tc_sync[7]; 
  always_ff @(posedge cclk) begin
    if(rst) qs1_pfc_tc_sync <= '0;
    else    qs1_pfc_tc_sync <= {qs1_pfc_tc_sync[6:0], tc_sync};
  end
  always_ff @(posedge cclk) qs2_pfc_tc_sync0 <= qs1_pfc_tc_sync[0];
  
//desigmware count_ones
  parameter width = 8;  //used by function
  logic [3:0] nc_cnt_ones;  //noconnect only need cnt_ones 0to8
  `include "DW_dp_count_ones_function.inc"
  assign {nc_cnt_ones, s1_cnt_ones} = DWF_dp_count_ones(qs1_rx_data_v);
  assign s1_dpc_md = {s1_cnt_ones,
                      qs1_rx_md.multi,
                      qs1_rx_md.fast,
                      qs1_rx_md.fcs_hint,
                      qs1_rx_md.dei,
                      qs1_rx_md.error,
                      qs1_rx_md.eop,
                      qs1_rx_md.eop_pos,
                      qs1_rx_md.byte_pos,
                      qs1_rx_md.sop,
                      qs1_rx_md.sop_pos,
                      qs1_rx_md.tc};
                      
  assign s1_port_num = {(qs1_rx_port_num == 2'b11),
                        (qs1_rx_port_num == 2'b10),
                        (qs1_rx_port_num == 2'b01),
                        (qs1_rx_port_num == 2'b00)};
  
//rx datapath delay stages begin  
  assign s1_or_rx_data_v = |qs1_rx_data_v;
  
  always_ff @(posedge cclk) qs2_port_v <= s1_port_num & {4{s1_or_rx_data_v}};
  
  always_ff @(posedge cclk) qs2_or_rx_data_v <= s1_or_rx_data_v;
  always_ff @(posedge cclk) qs1_rx_data      <= rx_data;

//timestamp
  always_ff @(posedge cclk) begin
    if(rst)                                    qs2_dpc_ts_p0 <= '0;
    else if(s1_or_rx_data_v && s1_port_num[0]) qs2_dpc_ts_p0 <= qs1_rx_ts;
  end
    always_ff @(posedge cclk) begin
    if(rst)                                    qs2_dpc_ts_p1 <= '0;
    else if(s1_or_rx_data_v && s1_port_num[1]) qs2_dpc_ts_p1 <= qs1_rx_ts;
  end
    always_ff @(posedge cclk) begin
    if(rst)                                    qs2_dpc_ts_p2 <= '0;
    else if(s1_or_rx_data_v && s1_port_num[2]) qs2_dpc_ts_p2 <= qs1_rx_ts;
  end
  always_ff @(posedge cclk) begin
    if(rst)                                    qs2_dpc_ts_p3 <= '0;
    else if(s1_or_rx_data_v && s1_port_num[3]) qs2_dpc_ts_p3 <= qs1_rx_ts;
  end
 
//logical port0  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs2_dpc_md_p0  <= '0;
      qs2_rx_data_p0 <= '0;
    end
    else if(s1_or_rx_data_v && s1_port_num[0]) begin
      qs2_dpc_md_p0  <= s1_dpc_md;
      qs2_rx_data_p0 <= qs1_rx_data;
    end
  end
//logical port1  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs2_dpc_md_p1  <= '0;
      qs2_rx_data_p1 <= '0;
    end
    else if(s1_or_rx_data_v && s1_port_num[1]) begin
      qs2_dpc_md_p1  <= s1_dpc_md;
      qs2_rx_data_p1 <= qs1_rx_data;
    end
  end
//logical port2  
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs2_dpc_md_p2  <= '0;
      qs2_rx_data_p2 <= '0;
    end
    else if(s1_or_rx_data_v && s1_port_num[2]) begin
      qs2_dpc_md_p2  <= s1_dpc_md;
      qs2_rx_data_p2 <= qs1_rx_data;
    end
  end
//logical port3
  always_ff @(posedge cclk) begin
    if(rst) begin
      qs2_dpc_md_p3  <= '0;
      qs2_rx_data_p3 <= '0;
    end
    else if(s1_or_rx_data_v && s1_port_num[3]) begin
      qs2_dpc_md_p3  <= s1_dpc_md;
      qs2_rx_data_p3 <= qs1_rx_data;
    end
  end  
//rx datapath delay stages end  
  

 
endmodule
