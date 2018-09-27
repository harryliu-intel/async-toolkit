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

  input logic clk,
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

  logic [7:0]                       q_rx_ecc;
  logic [1:0]                  q_rx_port_num;
  logic [7:0]                    q_rx_data_v;  
  epl_md_t                           q_rx_md;
  epl_ts_t                           q_rx_ts;  
  data64_w_ecc_t [0:7]             q_rx_data;
  data64_w_ecc_t [0:7]         rx_data_align;
  logic                        q_rx_pfc_xoff;  
  logic [2:0]           q_rx_flow_control_tc;



//EPL input buffers
  `MBY_MSFF(q_rx_ecc, rx_ecc, clk)  
  `MBY_MSFF(q_rx_port_num, rx_port_num, clk)
  `MBY_MSFF(q_rx_data_v, rx_data_v, clk)
  `MBY_MSFF(q_rx_md, rx_md, clk)
  `MBY_MSFF(q_rx_ts, rx_ts, clk)  
  `MBY_MSFF(q_rx_data, rx_data, clk)
  `MBY_MSFF(q_rx_pfc_xoff, rx_pfc_xoff, clk)
  `MBY_MSFF(q_rx_flow_control_tc, rx_flow_control_tc, clk)
  

endmodule
