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
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : Mesh Read Interface
//------------------------------------------------------------------------------

module mri
    import mby_egr_pkg::*, egr_int_pkg::*;
(
    input logic       clk,
    input logic     rst_n, 

    //EGR Internal Interfaces    
    egr_rrq_if.mri rrq_cpb_if, //Read Request  Interface. Provides to Clean Pointer Broker
    egr_rrs_if.mri rrs_cpb_if, //Read Response Interface. Provides to Clean Pointer Broker
    
    egr_rrq_if.mri rrq_tmu_if, //Read Request  Interface. Provides to Tag Management Unit 
    egr_rrs_if.mri rrs_tmu_if, //Read Response Interface. Provides to Tag Management Unit 
    
    egr_rrq_if.mri rrq_prc_if0, //Read Request  Interface. Provides to Packet Read Controller EPL 0,1
    egr_rrq_if.mri rrq_prc_if1, //Read Request  Interface. Provides to Packet Read Controller EPL 2,3
    egr_rrs_id_if.mri rrs_id_prc_if0, //Read Response ID Interface 0. Gives service to EPL0,1
    egr_rrs_id_if.mri rrs_id_prc_if1, //Read Response ID Interface 1. Gives service to EPL2,3
    egr_rrs_if.mri rrs_tqu_if0, //Read Response Interface. Provides to Transmit Queuing Unit EPL 0,1
    egr_rrs_if.mri rrs_tqu_if1, //Read Response Interface. Provides to Transmit Queuing Unit EPL 2,3

    //EGR External Interfaces
    mim_rd_if.request    mim_rd_if0_0, //MRI-MIM Read Interface Row 0 Line 0
    mim_rd_if.request    mim_rd_if0_1, //MRI-MIM Read Interface Row 0 Line 1
    mim_rd_if.request    mim_rd_if0_2, //MRI-MIM Read Interface Row 0 Line 2

    mim_rd_if.request    mim_rd_if1_0, //MRI-MIM Read Interface Row 1 Line 0
    mim_rd_if.request    mim_rd_if1_1, //MRI-MIM Read Interface Row 1 Line 1
    mim_rd_if.request    mim_rd_if1_2  //MRI-MIM Read Interface Row 1 Line 2
    
);


////////////////////////////////////////////
////////////////////////////////////////////
////////// FIRST PACKET LOGIC //////////////
////////////////////////////////////////////


////////////////////////////////////////////
////////// READ RESPONSES //////////////////
////////////////////////////////////////////
localparam N_RRSP_FIFOS        = 6;
localparam N_RRSP_FIFO_ENTRIES = 4;
localparam W_RRSP_ADD_L        = $clog2(N_RRSP_FIFO_ENTRIES);
localparam W_RRSP_FIFO_DATA_WORD = W_WORD_BITS;



egr_int_pkg::data_word_t fifo_out;
logic                  fifo_valid;
logic                    fifo_pop;
logic                fifo_pop_reg;
logic                  fifo_empty;
logic              fifo_empty_reg;
logic                   mri_valid;
egr_int_pkg::req_id_t fifo_req_id;

always_ff @(posedge clk, negedge rst_n) begin
    if(!rst_n) begin
        fifo_pop_reg <= '0;
        fifo_empty_reg <= '0;
    end
    else begin
        fifo_pop_reg <= fifo_pop;
        fifo_empty_reg <= fifo_empty;
    end
end

assign fifo_pop = (fifo_empty_reg==0)&&(fifo_empty==0);
assign mri_valid                    = fifo_pop;
assign rrs_tqu_if0.rrsp_wd_valid[0] = mri_valid;
assign rrs_tqu_if0.rrsp_wd_valid[1] = '0;
assign rrs_tqu_if0.rrsp_wd_valid[2] = '0;
assign rrs_tqu_if0.rrsp_wd_id[0]    = fifo_req_id;
assign rrs_tqu_if0.rrsp_wd_id[1]    = '0;
assign rrs_tqu_if0.rrsp_wd_id[2]    = '0;
assign rrs_tqu_if0.rrsp_wd[1]       = '0;
assign rrs_tqu_if0.rrsp_wd[2]       = '0;

assign rrs_tqu_if1.rrsp_wd_id       = '0;
assign rrs_tqu_if1.rrsp_wd          = '0;
assign rrs_tqu_if1.rrsp_wd_valid    = '0;

assign rrs_id_prc_mri_if0.rrsp_wd_valid[0] = mri_valid;
assign rrs_id_prc_mri_if0.rrsp_wd_valid[1] = '0;
assign rrs_id_prc_mri_if0.rrsp_wd_valid[2] = '0;
assign rrs_id_prc_mri_if0.rrsp_wd_id[0] = fifo_req_id;
assign rrs_id_prc_mri_if0.rrsp_wd_id[1] = '0;
assign rrs_id_prc_mri_if0.rrsp_wd_id[2] = '0;

assign rrs_id_prc_mri_if1.rrsp_wd_valid = '0;
assign rrs_id_prc_mri_if1.rrsp_wd_id    = '0;



mby_mgm_fifo #(.WIDTH(W_RRSP_FIFO_DATA_WORD),
               .ADD_L(W_RRSP_ADD_L)
)
rrsp_fifo_data0_0
(
    .clk       (clk),
    .rst_n     (rst_n),
    .d_out     (rrs_tqu_if0.rrsp_wd[0]),
    .d_in      (mim_rd_if0_0.mim_rd_data),
    .rd        (fifo_pop),
    .wr        (mim_rd_if0_0.mim_rrsp_valid),
    .empty     (fifo_empty),
    .full      (),
    .state_cnt ()
);

mby_mgm_fifo #(.WIDTH(W_REQ_ID),
               .ADD_L(W_RRSP_ADD_L)
)
rrsp_fifo_req_id0_0
(
    .clk       (clk),
    .rst_n     (rst_n),
    .d_out     (fifo_req_id),
    .d_in      (mim_rd_if0_0.mim_rrsp_req_id),
    .rd        (fifo_pop),
    .wr        (mim_rd_if0_0.mim_rrsp_valid),
    .empty     (fifo_empty),
    .full      (),
    .state_cnt ()
);


endmodule : mri
