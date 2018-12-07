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
// -- Author       : Jeff Hoelscher
// -- Project Name : Madison Bay
// -- Description  : This is the top level of the Egress(EGR) Tag Manager
//------------------------------------------------------------------------------

//`include "rtlgen_include_mby_egr_tag_csrs.vh"
//`include "rtlgen_pkg_mby_egr_tag_csrs.vh"
//`include "mby_egr_tag_csrs_pkg.vh"
//`include "shrtl_lmemc_pkg.vh"

module tmu
import shared_pkg::*;
(
    input logic                       clk,
    input logic                     rst_n,

    // Internal inferfaces
    egr_dp_if.requestor            dpb_if, // TMU requests from DPB
    egr_pfs_tmu_if.tmu             pfs_if, // PFS requests from TMU
    egr_prc_tmu_if.tmu             prc_if, // PRC requests from TMU

    egr_rrq_if.requestor       rrq_mri_if, // TMU requests from MRI
    egr_rrs_if.requestor       rrs_mri_if, // TMU receives responses from MRI

    // External interfaces
    egr_tagring_if.egr         tagring_if, //[15:0][1:0]
    egr_mce_tagring_if.egr mce_tagring_if  //[3:0]
);

logic qvalid;

//This only works for a single 64B packet
always_ff @(posedge clk) qvalid <= tagring_if.mby_tag_ring[0][0].valid;//Add reset

//input pfs_if.pop     [EPL_PER_MGP-1:0]
//input pfs_if.pop_port[EPL_PER_MGP-1:0][$clog2(PORTS_PER_EPL)-1:0]
//input pfs_if.pop_tc  [EPL_PER_MGP-1:0][$clog2(RX_TC_COUNT)-1:0]
//input pfs_if.pop_mgp [EPL_PER_MGP-1:0][$clog2(MGP_COUNT)-1:0] 

always_ff @(posedge clk) begin 
//pfs_if.queue_valid = [EPL_PER_MGP-1:0][PORTS_PER_EPL-1:0][RX_TC_COUNT-1:0][MGP_COUNT-1:0]
  pfs_if.queue_valid[3:1] <= 1536'h0;
  pfs_if.queue_valid[0][3:1] <= 384'h0;
  pfs_if.queue_valid[0][0][7:1] <= 112'h0;
  pfs_if.queue_valid[0][0][0][15:1] <= 15'h0;
  //Array Initialization required to make VCS happy

  if (!tagring_if.mby_tag_ring[0][0].valid)//In place of reset
      pfs_if.queue_valid[0][0][0][0] <= 1'b0;
  else if (tagring_if.mby_tag_ring[0][0].valid && !qvalid)//Set only on 1st packet
      pfs_if.queue_valid[0][0][0][0] <= 1'b1;
  else if (pfs_if.pop[0])
      pfs_if.queue_valid[0][0][0][0] <= 1'b0;
end
assign pfs_if.update = 4'h0;//[EPL_PER_MGP-1:0]
assign pfs_if.update_port = '0;//[EPL_PER_MGP-1:0][$clog2(PORTS_PER_EPL)-1:0]
assign pfs_if.update_tc = '0;//[EPL_PER_MGP-1:0][$clog2(RX_TC_COUNT)-1:0]
assign pfs_if.update_mgp = '0;//[EPL_PER_MGP-1:0][$clog2(MGP_COUNT)-1:0]
assign pfs_if.update_length = '0;//[EPL_PER_MGP-1:0][6:0](in 64B incs)

always_ff @(posedge clk) begin 
  if (prc_if.qsel[0][0][0]) begin
      prc_if.tag[0] <= tagring_if.mby_tag_ring[0][0];
      prc_if.tag[0].valid <= 1'b1;
  end else 
      prc_if.tag[0].valid <= '0;
end
assign prc_if.tag[1] = '0;
//input prc_if.qsel[MGP_COUNT-1:0][MGP_PORT_CNT-1:0][15:0]


//logic [15:0][15:0][15:0] eop_in_buf;
//Segment counters per VOQ to Egress Scheduler
logic [15:0][7:0][19:0] seg_uc_count0; //From IGR MGP0
//Fewer gates to add 4 100G counters together for 400G, or have four extra 7b conters?  
//Along with eight extra 6b counters for 200G?
logic [15:0][7:0][4:0] eop_uc_count0; //From IGR MGP0
//16 intances of tag_buf
//    -instance tail + head buffer in each
//instance tag_collate
/*
logic reset_n;

always_ff @(posedge cclk) reset_n <= ~rst;

mby_mem_igr_tag_aside_ram_shell_1024x104  mby_mem_igr_tag_aside_ram_shell_1024x104_0(
        .rd_clk(cclk),
        .wr_clk(cclk),
        .rd_reset_n(reset_n),
        .wr_reset_n(reset_n),
        .rd_adr(igr_tag_aside_ram_0_rd_adr),
        .wr_adr(igr_tag_aside_ram_0_wr_adr),
        .rd_en(igr_tag_aside_ram_0_rd_en),
        .wr_en(igr_tag_aside_ram_0_wr_en),
        .wr_data(igr_tag_aside_ram_0_wr_data),
        .mem_ls_enter(igr_tag_aside_ram_0_mem_ls_enter),
        .igr_igr_tag_aside_ram_from_mem(igr_igr_tag_aside_ram_0_from_mem),
        .igr_igr_tag_aside_ram_from_ctl(igr_igr_tag_aside_ram_0_from_ctl),

        .igr_igr_tag_aside_ram_to_mem(igr_igr_tag_aside_ram_0_to_mem),
        .igr_igr_tag_aside_ram_to_ctl(igr_igr_tag_aside_ram_0_to_ctl),
        .rd_data(igr_tag_aside_ram_0_rd_data),
        .rd_valid(igr_tag_aside_ram_0_rd_valid),
        .init_done(igr_tag_aside_ram_0_init_done),
        .ecc_uncor_err(igr_tag_aside_ram_0_ecc_uncor_err)
);

//Tags awaiting Mesh
mby_mem_igr_tag_mesh_ram_shell_768x600  mby_mem_igr_tag_mesh_ram_shell_768x600_0(
        .rd_clk(cclk),
        .wr_clk(cclk),
        .rd_reset_n(reset_n),
        .wr_reset_n(reset_n),
        .rd_adr(igr_tag_mesh_ram_0_rd_adr),
        .wr_adr(igr_tag_mesh_ram_0_wr_adr),
        .rd_en(igr_tag_mesh_ram_0_rd_en),
        .wr_en(igr_tag_mesh_ram_0_wr_en),
        .wr_data(igr_tag_mesh_ram_0_wr_data),
        .mem_ls_enter(igr_tag_mesh_ram_0_mem_ls_enter),
        .igr_igr_tag_mesh_ram_from_mem(igr_igr_tag_mesh_ram_0_from_mem),
        .igr_igr_tag_mesh_ram_from_ctl(igr_igr_tag_mesh_ram_0_from_ctl),

        .igr_igr_tag_mesh_ram_to_mem(igr_igr_tag_mesh_ram_0_to_mem),
        .igr_igr_tag_mesh_ram_to_ctl(igr_igr_tag_mesh_ram_0_to_ctl),
        .rd_data(igr_tag_mesh_ram_0_rd_data),
        .rd_valid(igr_tag_mesh_ram_0_rd_valid),
        .init_done(igr_tag_mesh_ram_0_init_done),
        .ecc_uncor_err(igr_tag_mesh_ram_0_ecc_uncor_err)
);

*/
endmodule : tmu
