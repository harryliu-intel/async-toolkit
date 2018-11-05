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


module mby_igr_dpc_wrap 
  import mby_igr_pkg::*;
(

  input logic cclk,
  input logic rst,
  
  input logic [7:0]            grp_a_rx_ecc, //src:EPL
  input logic [7:0]            grp_b_rx_ecc, //src:EPL
  input logic [7:0]            grp_c_rx_ecc, //src:EPL
  input logic [7:0]            grp_d_rx_ecc, //src:EPL

  input logic [1:0]            grp_a_rx_port_num, //src:EPL
  input logic [1:0]            grp_b_rx_port_num, //src:EPL
  input logic [1:0]            grp_c_rx_port_num, //src:EPL
  input logic [1:0]            grp_d_rx_port_num, //src:EPL
  
  input logic [7:0]            grp_a_rx_data_valid, //src:EPL
  input logic [7:0]            grp_b_rx_data_valid, //src:EPL
  input logic [7:0]            grp_c_rx_data_valid, //src:EPL
  input logic [7:0]            grp_d_rx_data_valid, //src:EPL
  
  input epl_md_t               grp_a_rx_metadata, //src:EPL 
  input epl_md_t               grp_b_rx_metadata, //src:EPL
  input epl_md_t               grp_c_rx_metadata, //src:EPL
  input epl_md_t               grp_d_rx_metadata, //src:EPL

  input epl_ts_t              grp_a_rx_time_stamp, //src:EPL
  input epl_ts_t              grp_b_rx_time_stamp, //src:EPL
  input epl_ts_t              grp_c_rx_time_stamp, //src:EPL
  input epl_ts_t              grp_d_rx_time_stamp, //src:EPL
   
  input data64_w_ecc_t [0:7]  grp_a_rx_data_w_ecc, //src:EPL
  input data64_w_ecc_t [0:7]  grp_b_rx_data_w_ecc, //src:EPL
  input data64_w_ecc_t [0:7]  grp_c_rx_data_w_ecc, //src:EPL
  input data64_w_ecc_t [0:7]  grp_d_rx_data_w_ecc, //src:EPL

  input logic                 grp_a_rx_pfc_xoff, //src:EPL
  input logic                 grp_b_rx_pfc_xoff, //src:EPL
  input logic                 grp_c_rx_pfc_xoff, //src:EPL
  input logic                 grp_d_rx_pfc_xoff, //src:EPL

  input logic [2:0]           grp_a_rx_flow_control_tc, //src:EPL
  input logic [2:0]           grp_b_rx_flow_control_tc, //src:EPL
  input logic [2:0]           grp_c_rx_flow_control_tc, //src:EPL
  input logic [2:0]           grp_d_rx_flow_control_tc, //src:EPL

  
// EPL I/O from MBY FS Dataplane Interface signals.

  output dpc_pb_t             o_dpc_pb0_p0, //dst:PB
  output dpc_pb_t             o_dpc_pb0_p1, //dst:PB
  output dpc_pb_t             o_dpc_pb0_p2, //dst:PB 
  output dpc_pb_t             o_dpc_pb0_p3, //dst:PB

  output dpc_pb_t             o_dpc_pb1_p0, //dst:PB
  output dpc_pb_t             o_dpc_pb1_p1, //dst:PB
  output dpc_pb_t             o_dpc_pb1_p2, //dst:PB 
  output dpc_pb_t             o_dpc_pb1_p3, //dst:PB

  output dpc_pb_t             o_dpc_pb2_p0, //dst:PB
  output dpc_pb_t             o_dpc_pb2_p1, //dst:PB
  output dpc_pb_t             o_dpc_pb2_p2, //dst:PB 
  output dpc_pb_t             o_dpc_pb2_p3, //dst:PB
  
  output dpc_pb_t             o_dpc_pb3_p0, //dst:PB
  output dpc_pb_t             o_dpc_pb3_p1, //dst:PB
  output dpc_pb_t             o_dpc_pb3_p2, //dst:PB 
  output dpc_pb_t             o_dpc_pb3_p3  //dst:PB
  
);


  mby_igr_dpc dpc_0(  
    .cclk(cclk),
    .rst(rst),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_a_rx_ecc),
    .rx_port_num(grp_a_rx_port_num),  
    .rx_data_v(grp_a_rx_data_valid),  
    .rx_md(grp_a_rx_metadata),
    .rx_ts(grp_a_rx_time_stamp),  
    .rx_data(grp_a_rx_data_w_ecc),
    .rx_pfc_xoff(grp_a_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_a_rx_flow_control_tc),
    .o_dpc_pb_p0(o_dpc_pb0_p0),
    .o_dpc_pb_p1(o_dpc_pb0_p1),
    .o_dpc_pb_p2(o_dpc_pb0_p2),
    .o_dpc_pb_p3(o_dpc_pb0_p3)
  );
  
  mby_igr_dpc dpc_1(  
    .cclk(cclk),
    .rst(rst),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_b_rx_ecc),
    .rx_port_num(grp_b_rx_port_num),  
    .rx_data_v(grp_b_rx_data_valid),  
    .rx_md(grp_b_rx_metadata),
    .rx_ts(grp_b_rx_time_stamp),  
    .rx_data(grp_b_rx_data_w_ecc),
    .rx_pfc_xoff(grp_b_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_b_rx_flow_control_tc),
    .o_dpc_pb_p0(o_dpc_pb1_p0),
    .o_dpc_pb_p1(o_dpc_pb1_p1),
    .o_dpc_pb_p2(o_dpc_pb1_p2),
    .o_dpc_pb_p3(o_dpc_pb1_p3)
  );

    mby_igr_dpc dpc_2(  
    .cclk(cclk),
    .rst(rst),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_c_rx_ecc),
    .rx_port_num(grp_c_rx_port_num),  
    .rx_data_v(grp_c_rx_data_valid),  
    .rx_md(grp_c_rx_metadata),
    .rx_ts(grp_c_rx_time_stamp),  
    .rx_data(grp_c_rx_data_w_ecc),
    .rx_pfc_xoff(grp_c_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_c_rx_flow_control_tc),
    .o_dpc_pb_p0(o_dpc_pb2_p0),
    .o_dpc_pb_p1(o_dpc_pb2_p1),
    .o_dpc_pb_p2(o_dpc_pb2_p2),
    .o_dpc_pb_p3(o_dpc_pb2_p3)
  );

    mby_igr_dpc dpc_3(  
    .cclk(cclk),
    .rst(rst),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_d_rx_ecc),
    .rx_port_num(grp_d_rx_port_num),  
    .rx_data_v(grp_d_rx_data_valid),  
    .rx_md(grp_d_rx_metadata),
    .rx_ts(grp_d_rx_time_stamp),  
    .rx_data(grp_d_rx_data_w_ecc),
    .rx_pfc_xoff(grp_d_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_d_rx_flow_control_tc),
    .o_dpc_pb_p0(o_dpc_pb3_p0),
    .o_dpc_pb_p1(o_dpc_pb3_p1),
    .o_dpc_pb_p2(o_dpc_pb3_p2),
    .o_dpc_pb_p3(o_dpc_pb3_p3)
  );





  

  

endmodule
