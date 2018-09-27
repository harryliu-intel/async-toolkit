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
// -- Description  : This is the top level of the Ingress(IGR) partition.
//                   It interfaces with EPL,PPE,XBAR.
//------------------------------------------------------------------------------



module igr_top 
  import mby_igr_pkg::*, shared_pkg::*;
(

  input logic clk,
  input logic rst_n,  //taken from HLP active low for MBY?
  
// EPL I/O from MBY FS Dataplane Interface signals.
  input logic [7:0]                       grp_A_rx_ecc,
  input logic [7:0]                       grp_B_rx_ecc,
  input logic [7:0]                       grp_C_rx_ecc,
  input logic [7:0]                       grp_D_rx_ecc,

  input logic [1:0]                  grp_A_rx_port_num,
  input logic [1:0]                  grp_B_rx_port_num,
  input logic [1:0]                  grp_C_rx_port_num,
  input logic [1:0]                  grp_D_rx_port_num,
  
  input logic [7:0]                grp_A_rx_data_valid,
  input logic [7:0]                grp_B_rx_data_valid,
  input logic [7:0]                grp_C_rx_data_valid,
  input logic [7:0]                grp_D_rx_data_valid,
  
  input epl_md_t                     grp_A_rx_metadata,
  input epl_md_t                     grp_B_rx_metadata,
  input epl_md_t                     grp_C_rx_metadata,
  input epl_md_t                     grp_D_rx_metadata,

  input epl_ts_t                   grp_A_rx_time_stamp,
  input epl_ts_t                   grp_B_rx_time_stamp,
  input epl_ts_t                   grp_C_rx_time_stamp,
  input epl_ts_t                   grp_D_rx_time_stamp,
   
  input data64_w_ecc_t [0:7]       grp_A_rx_data_w_ecc,
  input data64_w_ecc_t [0:7]       grp_B_rx_data_w_ecc,
  input data64_w_ecc_t [0:7]       grp_C_rx_data_w_ecc,
  input data64_w_ecc_t [0:7]       grp_D_rx_data_w_ecc,

  input logic                        grp_A_rx_pfc_xoff,
  input logic                        grp_B_rx_pfc_xoff,
  input logic                        grp_C_rx_pfc_xoff,
  input logic                        grp_D_rx_pfc_xoff,

  input logic [2:0]           grp_A_rx_flow_control_tc,
  input logic [2:0]           grp_B_rx_flow_control_tc,
  input logic [2:0]           grp_C_rx_flow_control_tc,
  input logic [2:0]           grp_D_rx_flow_control_tc,
  
  
// EPL I/O end

//Virtual Port I/O start
  input  logic [7:0]                       vp_rx_ecc,
  input  logic [1:0]                  vp_rx_port_num,
  input  logic [7:0]                vp_rx_data_valid,  
  input  epl_md_t                     vp_rx_metadata,
  input  epl_ts_t                   vp_rx_time_stamp,
  input  logic [19:0]             vp_cpp_rx_metadata,   
  input  data64_w_ecc_t [0:7]       vp_rx_data_w_ecc,
  input  logic [2:0]           vp_rx_flow_control_tc,
  output logic                        vp_tx_pfc_xoff, //FIXME should this be [1:0] for 2 VP 

//Virtual Port I/O end

// IGR/RX_PPE connections
  rx_ppe_igr_if.igr                      rx_ppe_igr,
  igr_rx_ppe_if.igr                      igr_rx_ppe   

  
);

  logic [7:0]                       q_vp_rx_ecc;
  logic [1:0]                  q_vp_rx_port_num;
  logic [7:0]                q_vp_rx_data_valid;  
  epl_md_t                     q_vp_rx_metadata;
  epl_ts_t                   q_vp_rx_time_stamp;
  logic [19:0]             q_vp_cpp_rx_metadata;   
  data64_w_ecc_t [0:7]       q_vp_rx_data_w_ecc;
  logic [2:0]           q_vp_rx_flow_control_tc;



//edr TEMP connections for rx_ppe until Scott G. decides how to connect
//igr_rx_ppe
  igr_rx_ppe_tail_t intf0_tail;
  igr_rx_ppe_head_t intf0_head;
  logic             intf0_ack;
  igr_rx_ppe_tail_t intf1_tail;
  igr_rx_ppe_head_t intf1_head;
  logic             intf1_ack;
  rx_ppe_igr_t      intf0;
  rx_ppe_igr_t      intf1;
  
  assign intf0_tail = '0;
  assign intf0_head = '0; 
  assign intf1_tail = '0; 
  assign intf1_head = '0; 

  `MBY_MSFF(igr_rx_ppe.intf0_tail, intf0_tail, clk) //out 
  `MBY_MSFF(igr_rx_ppe.intf0_head, intf0_head, clk) //out  
  `MBY_MSFF(igr_rx_ppe.intf1_tail, intf1_tail, clk) //out  
  `MBY_MSFF(igr_rx_ppe.intf1_tail, intf1_head, clk) //out 
  `MBY_MSFF(intf0_ack, igr_rx_ppe.intf0_ack, clk)   //in  
  `MBY_MSFF(intf1_ack, igr_rx_ppe.intf1_ack, clk)   //in
  
  `MBY_MSFF(intf0, rx_ppe_igr.intf0, clk)   //in  
  `MBY_MSFF(intf1, rx_ppe_igr.intf1, clk)   //in 


//edr TEMP connections for rx_ppe until Scott G. decides how to connect end

//edr VP ffs begin
  `MBY_MSFF(q_vp_rx_ecc, vp_rx_ecc, clk)
  `MBY_MSFF(q_vp_rx_port_num, vp_rx_port_num, clk) 
  `MBY_MSFF(q_vp_rx_data_valid, vp_rx_data_valid, clk)  
  `MBY_MSFF(q_vp_rx_metadata, vp_rx_metadata, clk) 
  `MBY_MSFF(q_vp_rx_time_stamp, vp_rx_time_stamp, clk)  
  `MBY_MSFF(q_vp_cpp_rx_metadata, vp_cpp_rx_metadata, clk)
  `MBY_MSFF(q_vp_rx_data_w_ecc, vp_rx_data_w_ecc, clk)  
  `MBY_MSFF(q_vp_rx_flow_control_tc, vp_rx_flow_control_tc, clk)
  `MBY_MSFF(vp_tx_pfc_xoff, '0, clk)  //output

//edr VP ffs end


//EPL shims

  igr_epl_shim epl_shim_A(  
    .clk(clk),
    .rst_n(rst_n),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_A_rx_ecc),
    .rx_port_num(grp_A_rx_port_num),  
    .rx_data_v(grp_A_rx_data_valid),  
    .rx_md(grp_A_rx_metadata),
    .rx_ts(grp_A_rx_time_stamp),  
    .rx_data(grp_A_rx_data_w_ecc),
    .rx_pfc_xoff(grp_A_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_A_rx_flow_control_tc)  
  );
  
  igr_epl_shim epl_shim_B(  
    .clk(clk),
    .rst_n(rst_n),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_B_rx_ecc),
    .rx_port_num(grp_B_rx_port_num),  
    .rx_data_v(grp_B_rx_data_valid),  
    .rx_md(grp_B_rx_metadata),
    .rx_ts(grp_B_rx_time_stamp),  
    .rx_data(grp_B_rx_data_w_ecc),
    .rx_pfc_xoff(grp_B_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_B_rx_flow_control_tc)  
  );
  
  igr_epl_shim epl_shim_C(  
    .clk(clk),
    .rst_n(rst_n),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_C_rx_ecc),
    .rx_port_num(grp_C_rx_port_num),  
    .rx_data_v(grp_C_rx_data_valid),  
    .rx_md(grp_C_rx_metadata),
    .rx_ts(grp_C_rx_time_stamp),  
    .rx_data(grp_C_rx_data_w_ecc),
    .rx_pfc_xoff(grp_C_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_C_rx_flow_control_tc)  
  );
  
  igr_epl_shim epl_shim_D(  
    .clk(clk),
    .rst_n(rst_n),
// EPL I/O from MBY FS Dataplane Interface signals.
    .rx_ecc(grp_D_rx_ecc),
    .rx_port_num(grp_D_rx_port_num),  
    .rx_data_v(grp_D_rx_data_valid),  
    .rx_md(grp_D_rx_metadata),
    .rx_ts(grp_D_rx_time_stamp),  
    .rx_data(grp_D_rx_data_w_ecc),
    .rx_pfc_xoff(grp_D_rx_pfc_xoff),  
    .rx_flow_control_tc(grp_D_rx_flow_control_tc)  
  );




endmodule
