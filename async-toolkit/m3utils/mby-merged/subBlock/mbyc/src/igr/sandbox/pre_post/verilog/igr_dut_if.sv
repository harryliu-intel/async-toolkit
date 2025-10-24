// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Scott Greenfield
// -- Project Name : Madison Bay (MBY)
// -- Description  : pre_ppe / post_ppe DUT interface 
// ---------------------------------------------------------------------------------------------------------------------

// This is a connectivity only interface and thus is just a named bundle of nets that can be passed around as a group 
// to save typing.  Anywhere an interface is defined, individual nets in the interface can be referenced as follows:    
//
//      <interface name>.<net name>
//

interface igr_dut_if
import mby_igr_pkg::*, mby_rx_metadata_pkg::*, shared_pkg::*;
(
   input clk                                        // clk is passed in a parameter and becomes part of the interface
);


// local paramters


// DUT inputs  (direction not specified in this interface)
logic               rst;                                // reset


 shim_pb_data_t [3:0]       i_shim_pb_data_p0;
 shim_pb_data_t [3:0]       i_shim_pb_data_p1;
 shim_pb_data_t [3:0]       i_shim_pb_data_p2;
 shim_pb_data_t [3:0]       i_shim_pb_data_p3;
 shim_pb_md_t   [3:0]       i_shim_pb_md_p0;
 shim_pb_md_t   [3:0]       i_shim_pb_md_p1;
 shim_pb_md_t   [3:0]       i_shim_pb_md_p2;
 shim_pb_md_t   [3:0]       i_shim_pb_md_p3;
 logic [3:0][2:0]           i_shim_pb_v_p0;  
 logic [3:0][2:0]           i_shim_pb_v_p1;  
 logic [3:0][2:0]           i_shim_pb_v_p2;  
 logic [3:0][2:0]           i_shim_pb_v_p3;
 logic [4:0]                i_free_ptr_valid;
 logic [4:0]                o_free_ptr_req;
 seg_ptr_t [4:0]            i_free_seg_ptr;
 sema_t    [4:0]            i_free_sema;

 igr_pkt_id_t [1:0]         i_return_id;
 logic [1:0]                i_return_id_valid;

//Interface List
 igr_rx_ppe_tail_t          igr_rx_ppe_intf0_tail; // Blasted Interface igr_rx_ppe_if.igr igr_rx_ppe
 igr_rx_ppe_head_t          igr_rx_ppe_intf0_head; 
 logic                      igr_rx_ppe_intf0_ack; 
 igr_rx_ppe_tail_t          igr_rx_ppe_intf1_tail; 
 igr_rx_ppe_head_t          igr_rx_ppe_intf1_head; 
 logic                      igr_rx_ppe_intf1_ack; 

 rx_ppe_igr_t               rx_ppe_igr_intf0; // Blasted Interface rx_ppe_igr_if.igr rx_ppe_igr
 rx_ppe_igr_t               rx_ppe_igr_intf1; 


 logic                      egr_igr_wreq_mim_wreq_valid; // Blasted Interface mim_wr_if.receive egr_igr_wreq
 logic [20-1:0]             egr_igr_wreq_mim_wr_seg_ptr; 
 logic [4-1:0]              egr_igr_wreq_mim_wr_sema; 
 logic [2-1:0]              egr_igr_wreq_mim_wr_wd_sel; 
 logic [13-1:0]             egr_igr_wreq_mim_wreq_id; 
 logic [(8*64)-1:0]         egr_igr_wreq_mim_wr_data; 
 logic [1-1:0]              egr_igr_wreq_mim_wreq_credits; 

 logic                      mim_wreq_0_mim_wreq_valid; // Blasted Interface mim_wr_if.request mim_wreq_0
 logic [20-1:0]             mim_wreq_0_mim_wr_seg_ptr; 
 logic [4-1:0]              mim_wreq_0_mim_wr_sema; 
 logic [2-1:0]              mim_wreq_0_mim_wr_wd_sel; 
 logic [13-1:0]             mim_wreq_0_mim_wreq_id; 
 logic [(8*64)-1:0]         mim_wreq_0_mim_wr_data; 
 logic [1-1:0]              mim_wreq_0_mim_wreq_credits; 

 logic                      mim_wreq_1_mim_wreq_valid; // Blasted Interface mim_wr_if.request mim_wreq_1
 logic [20-1:0]             mim_wreq_1_mim_wr_seg_ptr; 
 logic [4-1:0]              mim_wreq_1_mim_wr_sema; 
 logic [2-1:0]              mim_wreq_1_mim_wr_wd_sel; 
 logic [13-1:0]             mim_wreq_1_mim_wreq_id; 
 logic [(8*64)-1:0]         mim_wreq_1_mim_wr_data; 
 logic [1-1:0]              mim_wreq_1_mim_wreq_credits; 

 logic                      mim_wreq_2_mim_wreq_valid; // Blasted Interface mim_wr_if.request mim_wreq_2
 logic [20-1:0]             mim_wreq_2_mim_wr_seg_ptr; 
 logic [4-1:0]              mim_wreq_2_mim_wr_sema; 
 logic [2-1:0]              mim_wreq_2_mim_wr_wd_sel; 
 logic [13-1:0]             mim_wreq_2_mim_wreq_id; 
 logic [(8*64)-1:0]         mim_wreq_2_mim_wr_data; 
 logic [1-1:0]              mim_wreq_2_mim_wreq_credits; 

 logic                      mim_wreq_3_mim_wreq_valid; // Blasted Interface mim_wr_if.request mim_wreq_3
 logic [20-1:0]             mim_wreq_3_mim_wr_seg_ptr; 
 logic [4-1:0]              mim_wreq_3_mim_wr_sema; 
 logic [2-1:0]              mim_wreq_3_mim_wr_wd_sel; 
 logic [13-1:0]             mim_wreq_3_mim_wreq_id; 
 logic [(8*64)-1:0]         mim_wreq_3_mim_wr_data; 
 logic [1-1:0]              mim_wreq_3_mim_wreq_credits; 

 logic                      mim_wreq_4_mim_wreq_valid; // Blasted Interface mim_wr_if.request mim_wreq_4
 logic [20-1:0]             mim_wreq_4_mim_wr_seg_ptr; 
 logic [4-1:0]              mim_wreq_4_mim_wr_sema; 
 logic [2-1:0]              mim_wreq_4_mim_wr_wd_sel; 
 logic [13-1:0]             mim_wreq_4_mim_wreq_id; 
 logic [(8*64)-1:0]         mim_wreq_4_mim_wr_data; 
 logic [1-1:0]              mim_wreq_4_mim_wreq_credits; 

 logic                      mim_wreq_5_mim_wreq_valid; // Blasted Interface mim_wr_if.request mim_wreq_5
 logic [20-1:0]             mim_wreq_5_mim_wr_seg_ptr; 
 logic [4-1:0]              mim_wreq_5_mim_wr_sema; 
 logic [2-1:0]              mim_wreq_5_mim_wr_wd_sel; 
 logic [13-1:0]             mim_wreq_5_mim_wreq_id; 
 logic [(8*64)-1:0]         mim_wreq_5_mim_wr_data; 
 logic [1-1:0]              mim_wreq_5_mim_wreq_credits; 



 logic [W_SEG_PTR-1:0][1:0]       o_drop_seg_ptr; //[19:0] 
 logic [1:0]                      o_drop_seg_valid; 
 logic [W_SEMA-1:0][1:0]          o_drop_sema; 
 logic [7:0]                      o_port_id; // FIXME -- what is this for?  ring_tag_t also has src & dst ports 
 mby_tag_ring_t                   o_post_ppe_tag_at_rate0; 
 mby_tag_ring_t                   o_post_ppe_tag_at_rate1; 
 mby_tag_ring_t                   o_post_ppe_tag_set_aside0; 
 mby_tag_ring_t                   o_post_ppe_tag_set_aside1;
                



endinterface // igr_dut_if
