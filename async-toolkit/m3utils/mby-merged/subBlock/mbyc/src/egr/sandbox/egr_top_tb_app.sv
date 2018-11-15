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
//-----------------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
//-----------------------------------------------------------------------------
// -- Author:  John M. Greth (john.greth@intel.com)
// -- Project Name: Madison Bay
// -- FUB:  MBY_EGR
// -- Module: egr_top_tb_app
// -- Description: generates stimulus and performs application-level checks for the egr sandbox test environment.
// --
//-----------------------------------------------------------------------------

module egr_top_tb_app 
  import mby_egr_pkg::*, shared_pkg::*, mby_gmm_pkg::*;
(
//TODO change width of interfaces to parameters
  input logic                                  clk,
  input logic                               arst_n, //Asynchronous negedge reset   

  egr_epl_if.epl                           epl_if0, //EGR-EPL 0 Interface
  egr_epl_if.epl                           epl_if1, //EGR-EPL 1 Interface
  egr_epl_if.epl                           epl_if2, //EGR-EPL 2 Interface
  egr_epl_if.epl                           epl_if3  //EGR-EPL 3 Interface
);

logic quiesce;

int mgp;
always_comb mgp = 6;

// Use each EPL for single port with no back-pressure.
always_comb epl_if0.tx_enable_port_num = 2'b0;
always_comb epl_if0.tx_enable = 1'b1;
always_comb epl_if1.tx_enable_port_num = 2'b0;
always_comb epl_if1.tx_enable = 1'b1;
always_comb epl_if2.tx_enable_port_num = 2'b0;
always_comb epl_if2.tx_enable = 1'b1;
always_comb epl_if3.tx_enable_port_num = 2'b0;
always_comb epl_if3.tx_enable = 1'b1;

// Send a single tag
mby_tag_ring_t tag;
initial begin
    quiesce = 0;
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    while (!arst_n) @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);

    // TODO: set missing fields
    //
    // typedef struct packed {
    //    logic [MBY_TAG_RING_PARITY_MSB:0]    parity;         // 2-bit parity
    //    logic [4:0]                          rsvrd;          // Spare (5-bit)
    //    logic [MBY_TAG_TX_TC_MSB:0]          dst_tc;         // TX TC (16-bits)
    //    logic [MBY_TAG_TX_PORT_MSB:0]        dst_port_id;    // TX port ID (9-bits)
    //    logic [MBY_TAG_NEXT_LEN_MSB:0]       next_len;      
    //    logic [MBY_TAG_SLL_MSB:0]            sll;            // Switch Lifetime (3-bit)
    //    logic [MBY_TAG_RX_TC_MSB:0]          src_tc;		// RX TC (3-bit)
    //    logic [MBY_PORTID_IN_MGP_MSB:0]      src_port_id;    // Rx PORT [3:0] (3-bits)
    //    logic                                at_egr_rate;    // Packet is eligible for the cut-through (1-bit)
    //    logic                                multi_copy;     // packet to be mirrored (1-bit)
    //    logic                                err_xmd;        // Error if EOP and 64B meta-dta if SOP (1-bit)
    //    logic                                eop;            // End-of_packet
    //    logic [MBY_TAG_LENGTH_MSB:0]         length;         // length of packets data in this segment in bytes  (8-bits)
    //    logic [MBY_SEG_PTR_TOGGLE_MSB:0]     ptr_toggle;     // Semaphore for 4 64B words (4-bits)
    //    logic [MBY_SEG_PTR_MSB:0]            ptr;            // Segment Pointer (20-bits)
    //    logic                                valid;          // Valid (1-bit)
    // } mby_tag_ring_t;   

    tag = 0;
    tag.dst_tc = 0;
    tag.dst_port_id = {1'b0, mgp[3:0], 4'b0};
    tag.next_len = 0; // ?
    tag.sll = 0; // ?
    tag.eop = 1'b1;
    tag.length = 64;
    tag.ptr_toggle = 4'b1; // ?
    tag.ptr = $random();
    tag.valid = 1'b1;
    tagring_bfm.send_tag(0, tag);
    quiesce = 1;
end

endmodule : egr_top_tb_app
