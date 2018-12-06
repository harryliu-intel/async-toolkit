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
//------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : MRI Testbench for testing basic functionality
//------------------------------------------------------------------------------


`include "shared_pkg.sv"
`include "mby_gmm_pkg.sv"
`include "mby_egr_pkg.sv"
`include "egr_int_pkg.sv"

module mri_tb
    import mby_egr_pkg::*, shared_pkg::*, egr_int_pkg::*
    `ifndef NO_SLA 
    ,sla_pkg::*
    `endif
    ; 
();

logic clk;
logic arst_n;


////////////////////////////////
//// External Interfaces ///////
////////////////////////////////
    mim_rd_if mim_rd_if0_0(); //MIM Read Request Row0 Plane 0
    mim_rd_if mim_rd_if0_1(); //MIM Read Request Row0 Plane 1
    mim_rd_if mim_rd_if0_2(); //MIM Read Request Row0 Plane 2
    mim_rd_if mim_rd_if1_0(); //MIM Read Request Row1 Plane 0
    mim_rd_if mim_rd_if1_1(); //MIM Read Request Row1 Plane 1
    mim_rd_if mim_rd_if1_2(); //MIM Read Request Row1 Plane 2


////////////////////////////////
//// MRI Interfaces ////////////
////////////////////////////////
    egr_rrq_if    #(.N_RREQS(1))     rrq_cpb_mri_if();
    egr_rrq_if    #(.N_RREQS(1))     rrq_tmu_mri_if();
    egr_rrq_if    #(.N_RREQS(3))    rrq_prc_mri_if0();
    egr_rrq_if    #(.N_RREQS(3))    rrq_prc_mri_if1();
    egr_rrs_id_if #(.N_RRSPS(3)) rrs_id_prc_mri_if0();
    egr_rrs_id_if #(.N_RRSPS(3)) rrs_id_prc_mri_if1();
    egr_rrs_if    #(.N_RRSPS(3))     rrs_cpb_mri_if();
    egr_rrs_if    #(.N_RRSPS(3))     rrs_tmu_mri_if();
    egr_rrs_if    #(.N_RRSPS(3))    rrs_tqu_mri_if0();
    egr_rrs_if    #(.N_RRSPS(3))    rrs_tqu_mri_if1();

////////////////////////////////
//// MRI DUT ///////////////////
////////////////////////////////
mri dut(
    .clk(clk),
    .rst_n(arst_n), 
    .rrq_cpb_if(rrq_cpb_mri_if), //Read Request  Interface. Provides to Clean Pointer Broker
    .rrq_tmu_if(rrq_tmu_mri_if), //Read Request  Interface. Provides to Tag Management Unit 
    .rrq_prc_if0(rrq_prc_mri_if0), //Read Request Interface 0. Provides to Packet Read Controller EPL 0,1
    .rrq_prc_if1(rrq_prc_mri_if1), //Read Request Interface 1. Provides to Packet Read Controller EPL 2,3
    .rrs_id_prc_if0(rrs_id_prc_mri_if0), //Read Response ID Interface 0. Gives service to EPL0,1
    .rrs_id_prc_if1(rrs_id_prc_mri_if1), //Read Response ID Interface 1. Gives service to EPL2,3
    .rrs_cpb_if(rrs_cpb_mri_if), //Read Response Interface. Provides to Clean Pointer Broker
    .rrs_tmu_if(rrs_tmu_mri_if), //Read Response Interface. Provides to Tag Management Unit 
    .rrs_tqu_if0(rrs_tqu_mri_if0), //Read Response Interface. Provides to Transmit Queuing Unit EPL 0,1
    .rrs_tqu_if1(rrs_tqu_mri_if1), //Read Response Interface. Provides to Transmit Queuing Unit EPL 2,3
    .mim_rd_if0_0(mim_rd_if0_0), //MRI-MIM Read Interface Row 0 Line 0
    .mim_rd_if0_1(mim_rd_if0_1), //MRI-MIM Read Interface Row 0 Line 1
    .mim_rd_if0_2(mim_rd_if0_2), //MRI-MIM Read Interface Row 0 Line 2
    .mim_rd_if1_0(mim_rd_if1_0), //MRI-MIM Read Interface Row 1 Line 0
    .mim_rd_if1_1(mim_rd_if1_1), //MRI-MIM Read Interface Row 1 Line 1
    .mim_rd_if1_2(mim_rd_if1_2)  //MRI-MIM Read Interface Row 1 Line 2
);


////////////////////////////////
//// SANDBOX RUNTIME ///////////
////////////////////////////////
mby_sandbox_runtime runtime(.clk(clk), .rst_n(arst_n));


////////////////////////////////
//// TESTBENCH RUNTIME /////////
////////////////////////////////
egr_int_pkg::req_id_t req_id;
initial
begin
    req_id = 0;
    mim_rd_if0_0.mim_rrsp_valid = 0;
    mim_rd_if0_0.mim_rrsp_dest_block = 0;
    mim_rd_if0_0.mim_rrsp_req_id = req_id;
    mim_rd_if0_0.mim_rd_data = 0;
    @(posedge arst_n);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    req_id.client_id = CLIENT_PRC_TQU0_0;
    req_id.data_wd_id.word_type = WORD_TYPE_CTRL;
    req_id.data_wd_id.epl = 0;
    req_id.data_wd_id.tx_lp = 0;
    req_id.data_wd_id.tx_tc = 0;
    mim_rd_if0_0.mim_rrsp_valid = 1;
    mim_rd_if0_0.mim_rrsp_dest_block = 0;
    mim_rd_if0_0.mim_rrsp_req_id = req_id;
    mim_rd_if0_0.mim_rd_data = {'0,32'h1234_5678};
    @(posedge clk);
    mim_rd_if0_0.mim_rrsp_valid = 0;
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    req_id.data_wd_id.word_type = WORD_TYPE_DATA;
    mim_rd_if0_0.mim_rrsp_req_id = req_id;
    mim_rd_if0_0.mim_rd_data = {'0,32'haabb_ccdd};
    mim_rd_if0_0.mim_rrsp_valid = 1;
    @(posedge clk);
    mim_rd_if0_0.mim_rrsp_valid = 0;
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
end




//initial
//begin
//    rrs_tqu_mri_if0.rrsp_wd_id    = '0;
//    rrs_tqu_mri_if0.rrsp_wd       = '0;
//    rrs_tqu_mri_if0.rrsp_wd_valid = '0;
//    rrs_tqu_mri_if1.rrsp_wd_id    = '0;
//    rrs_tqu_mri_if1.rrsp_wd       = '0;
//    rrs_tqu_mri_if1.rrsp_wd_valid = '0;
//    @(posedge clk);
//    @(posedge clk);
//    @(posedge clk);
//    @(posedge arst_n);
//    @(posedge clk);
//    @(posedge clk);
//    rrs_tqu_mri_if0.rrsp_wd_id[0].client_id = CLIENT_PRC_TQU0_0;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.word_type = WORD_TYPE_DATA;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.epl   = 0;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.tx_lp = 0;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.tx_tc = 0;
//    rrs_tqu_mri_if0.rrsp_wd[0] = {'0,32'h12345678};
//    rrs_tqu_mri_if0.rrsp_wd_valid[0] = '1;
//    @(posedge clk);
//    rrs_tqu_mri_if0.rrsp_wd_valid[0] = '0;
//    @(posedge clk);
//    @(posedge clk);
//    @(posedge clk);
//    rrs_tqu_mri_if0.rrsp_wd_id[0].client_id = CLIENT_PRC_TQU0_0;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.word_type = WORD_TYPE_CTRL;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.epl   = 0;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.tx_lp = 0;
//    rrs_tqu_mri_if0.rrsp_wd_id[0].data_wd_id.tx_tc = 0;
//    rrs_tqu_mri_if0.rrsp_wd[0] = {'0,32'haabbccdd};
//    rrs_tqu_mri_if0.rrsp_wd_valid[0] = '1;
//    @(posedge clk);
//    rrs_tqu_mri_if0.rrsp_wd_valid[0] = '0;
//    @(posedge clk);
//end
//
//initial 
//begin
//    tcu_tqu_if.dtq_ctrl_pull = '0;
//    @(posedge arst_n);
//    do
//        @(posedge clk);
//    while(tcu_tqu_if.dtq_ctrl_ready[0][0][0]=='0);
//    @(posedge clk);
//    @(posedge clk);
//    tcu_tqu_if.dtq_ctrl_pull[0].dtq_sel  = '0;
//    tcu_tqu_if.dtq_ctrl_pull[0].peek_pop = '0;
//    tcu_tqu_if.dtq_ctrl_pull[0].req      = '1;
//    wait(tcu_tqu_if.ctrl_word_valid[0]==1); #2;
//    $display("Ctrl Data: %x",tcu_tqu_if.ctrl_word);
//end
//
//initial 
//begin
//    tcu_tqu_if.dtq_data_pull = '0;
//    @(posedge arst_n);
//    do
//        @(posedge clk);
//    while(tcu_tqu_if.dtq_data_ready[0][0][0]=='0);
//    @(posedge clk);
//    @(posedge clk);
//    tcu_tqu_if.dtq_data_pull[0].dtq_sel  = '0;
//    tcu_tqu_if.dtq_data_pull[0].peek_pop = '0;
//    tcu_tqu_if.dtq_data_pull[0].req      = '1;
//    wait(tcu_tqu_if.data_word_valid[0]==1); #2;
//    $display("Data Word: %x",tcu_tqu_if.pkt_word);
//end

endmodule : mri_tb
