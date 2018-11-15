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
// -- Module: egr_top_tb_tagring
// -- Description: Models the tagring for the erg sandbox test environment.
// --
//-----------------------------------------------------------------------------

module egr_top_tb_tagring
  import mby_egr_pkg::*, shared_pkg::*, mby_gmm_pkg::*; 
(
  input logic                           clk,
  input logic                        arst_n, //Asynchronous negedge reset

  egr_tagring_if.tagring          tagring_if0_0, //EGR-Tag Ring Interface MGP  0 0
  egr_tagring_if.tagring          tagring_if0_1, //EGR-Tag Ring Interface MGP  0 1
  egr_tagring_if.tagring          tagring_if1_0, //EGR-Tag Ring Interface MGP  1 0
  egr_tagring_if.tagring          tagring_if1_1, //EGR-Tag Ring Interface MGP  1 1
  egr_tagring_if.tagring          tagring_if2_0, //EGR-Tag Ring Interface MGP  2 0
  egr_tagring_if.tagring          tagring_if2_1, //EGR-Tag Ring Interface MGP  2 1
  egr_tagring_if.tagring          tagring_if3_0, //EGR-Tag Ring Interface MGP  3 0
  egr_tagring_if.tagring          tagring_if3_1, //EGR-Tag Ring Interface MGP  3 1
  egr_tagring_if.tagring          tagring_if4_0, //EGR-Tag Ring Interface MGP  4 0
  egr_tagring_if.tagring          tagring_if4_1, //EGR-Tag Ring Interface MGP  4 1
  egr_tagring_if.tagring          tagring_if5_0, //EGR-Tag Ring Interface MGP  5 0
  egr_tagring_if.tagring          tagring_if5_1, //EGR-Tag Ring Interface MGP  5 1
  egr_tagring_if.tagring          tagring_if6_0, //EGR-Tag Ring Interface MGP  6 0
  egr_tagring_if.tagring          tagring_if6_1, //EGR-Tag Ring Interface MGP  6 1
  egr_tagring_if.tagring          tagring_if7_0, //EGR-Tag Ring Interface MGP  7 0
  egr_tagring_if.tagring          tagring_if7_1, //EGR-Tag Ring Interface MGP  7 1
  egr_tagring_if.tagring          tagring_if8_0, //EGR-Tag Ring Interface MGP  8 0
  egr_tagring_if.tagring          tagring_if8_1, //EGR-Tag Ring Interface MGP  8 1
  egr_tagring_if.tagring          tagring_if9_0, //EGR-Tag Ring Interface MGP  9 0
  egr_tagring_if.tagring          tagring_if9_1, //EGR-Tag Ring Interface MGP  9 1
  egr_tagring_if.tagring         tagring_if10_0, //EGR-Tag Ring Interface MGP 10 0
  egr_tagring_if.tagring         tagring_if10_1, //EGR-Tag Ring Interface MGP 10 1
  egr_tagring_if.tagring         tagring_if11_0, //EGR-Tag Ring Interface MGP 11 0
  egr_tagring_if.tagring         tagring_if11_1, //EGR-Tag Ring Interface MGP 11 1
  egr_tagring_if.tagring         tagring_if12_0, //EGR-Tag Ring Interface MGP 12 0
  egr_tagring_if.tagring         tagring_if12_1, //EGR-Tag Ring Interface MGP 12 1
  egr_tagring_if.tagring         tagring_if13_0, //EGR-Tag Ring Interface MGP 13 0
  egr_tagring_if.tagring         tagring_if13_1, //EGR-Tag Ring Interface MGP 13 1
  egr_tagring_if.tagring         tagring_if14_0, //EGR-Tag Ring Interface MGP 14 0
  egr_tagring_if.tagring         tagring_if14_1, //EGR-Tag Ring Interface MGP 14 1
  egr_tagring_if.tagring         tagring_if15_0, //EGR-Tag Ring Interface MGP 15 0
  egr_tagring_if.tagring         tagring_if15_1  //EGR-Tag Ring Interface MGP 15 1
);

logic quiesce;

int ring_pos;
always_comb ring_pos = app_bfm.mgp; // which MGP this is

// Single stop on ring
mby_tag_ring_t [15:0][1:0] ring_if;

// each stop
mby_tag_ring_t [15:0][15:0][1:0] full_ring;

class egr_tb_igr;
    mby_tag_ring_t tag_queue[15:0][15:0][$];
endclass

egr_tb_igr s = new;

///////////////////////////////////////////////////////////////////////////////
// Drive interface
///////////////////////////////////////////////////////////////////////////////
always_comb tagring_if0_0.mby_tag_ring  = ring_if[ 0][0];
always_comb tagring_if0_1.mby_tag_ring  = ring_if[ 0][1];
always_comb tagring_if1_0.mby_tag_ring  = ring_if[ 1][0];
always_comb tagring_if1_1.mby_tag_ring  = ring_if[ 1][1];
always_comb tagring_if2_0.mby_tag_ring  = ring_if[ 2][0];
always_comb tagring_if2_1.mby_tag_ring  = ring_if[ 2][1];
always_comb tagring_if3_0.mby_tag_ring  = ring_if[ 3][0];
always_comb tagring_if3_1.mby_tag_ring  = ring_if[ 3][1];
always_comb tagring_if4_0.mby_tag_ring  = ring_if[ 4][0];
always_comb tagring_if4_1.mby_tag_ring  = ring_if[ 4][1];
always_comb tagring_if5_0.mby_tag_ring  = ring_if[ 5][0];
always_comb tagring_if5_1.mby_tag_ring  = ring_if[ 5][1];
always_comb tagring_if6_0.mby_tag_ring  = ring_if[ 6][0];
always_comb tagring_if6_1.mby_tag_ring  = ring_if[ 6][1];
always_comb tagring_if7_0.mby_tag_ring  = ring_if[ 7][0];
always_comb tagring_if7_1.mby_tag_ring  = ring_if[ 7][1];
always_comb tagring_if8_0.mby_tag_ring  = ring_if[ 8][0];
always_comb tagring_if8_1.mby_tag_ring  = ring_if[ 8][1];
always_comb tagring_if9_0.mby_tag_ring  = ring_if[ 9][0];
always_comb tagring_if9_1.mby_tag_ring  = ring_if[ 9][1];
always_comb tagring_if10_0.mby_tag_ring = ring_if[10][0];
always_comb tagring_if10_1.mby_tag_ring = ring_if[10][1];
always_comb tagring_if11_0.mby_tag_ring = ring_if[11][0];
always_comb tagring_if11_1.mby_tag_ring = ring_if[11][1];
always_comb tagring_if12_0.mby_tag_ring = ring_if[12][0];
always_comb tagring_if12_1.mby_tag_ring = ring_if[12][1];
always_comb tagring_if13_0.mby_tag_ring = ring_if[13][0];
always_comb tagring_if13_1.mby_tag_ring = ring_if[13][1];
always_comb tagring_if14_0.mby_tag_ring = ring_if[14][0];
always_comb tagring_if14_1.mby_tag_ring = ring_if[14][1];
always_comb tagring_if15_0.mby_tag_ring = ring_if[15][0];
always_comb tagring_if15_1.mby_tag_ring = ring_if[15][1];

always_ff @(posedge clk)
    for (int i = 0; i < 16; i++) begin
        ring_if[i][0] <= full_ring[ring_pos][i][0];
        ring_if[i][1] <= full_ring[ring_pos][i][1];
    end

///////////////////////////////////////////////////////////////////////////////
// Drive ring
///////////////////////////////////////////////////////////////////////////////

// loop so that traffic from a shared port are evenly spaced.
logic [3:0] ring_ct;
always_ff @(posedge clk)
    if (!arst_n) ring_ct <= 4'b0;
    else {ring_ct[0], ring_ct[1], ring_ct[3:2]} <= {ring_ct[0], ring_ct[1], ring_ct[3:2]} + 4'b1;

logic [15:0][15:0][3:0] link_to_src;
initial begin
    for (int i = 0; i < 16; i++)
        for (int j = 0; j < 16; j++)
            link_to_src[i][j] = j;
end

always_ff @(posedge clk) begin
    quiesce <= 1'b1;
    for (int i = 0; i < 16; i++) for (int j = 0; j < 16; j++) begin
        if (full_ring[i][j][0].valid) quiesce <= 1'b0;
        if (full_ring[i][j][1].valid) quiesce <= 1'b0;
        if (!s.tag_queue[i][j].empty()) quiesce <= 1'b0;
    end
end

always_ff @(posedge clk) begin
    full_ring <= {full_ring[14:0], full_ring[15]}; // advance ring
    // drive ring
    for (int i = 0; i < 16; i++) begin
        if (!arst_n || s.tag_queue[i][link_to_src[i][ring_ct]].empty()) begin
            full_ring[i][i][0].valid <= 1'b0;
            full_ring[i][i][1].valid <= 1'b0;
        end else if (s.tag_queue[i][link_to_src[i][ring_ct]].size() > 1) begin
            full_ring[i][i][0] <= s.tag_queue[i][link_to_src[i][ring_ct]][0];
            full_ring[i][i][1] <= s.tag_queue[i][link_to_src[i][ring_ct]][1];
            s.tag_queue[i][link_to_src[i][ring_ct]].pop_front();
            s.tag_queue[i][link_to_src[i][ring_ct]].pop_front();
        end else begin
            full_ring[i][i][0] <= s.tag_queue[i][link_to_src[i][ring_ct]][0];
            full_ring[i][i][1].valid <= 1'b0;
            s.tag_queue[i][link_to_src[i][ring_ct]].pop_front();
        end
    end
end

function merge_src(logic [3:0] src_mgp, logic [3:0] src_port, logic [3:0] dst_port);
    link_to_src[src_mgp][src_port] = dst_port;
endfunction

function send_tag(logic [3:0] src_mgp, mby_tag_ring_t tag);
    s.tag_queue[src_mgp][link_to_src[src_mgp][tag.src_port_id]].push_back(tag);
endfunction

endmodule : egr_top_tb_tagring


