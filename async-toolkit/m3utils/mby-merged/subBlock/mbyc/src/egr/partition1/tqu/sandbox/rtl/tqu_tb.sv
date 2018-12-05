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
// -- Description  : TQU Testbench for testing basic functionality
//------------------------------------------------------------------------------


`include "shared_pkg.sv"
`include "mby_gmm_pkg.sv"
`include "mby_egr_pkg.sv"
`include "egr_int_pkg.sv"

module tqu_tb
  import mby_egr_pkg::*, shared_pkg::*, egr_int_pkg::*
  `ifndef NO_SLA 
  ,sla_pkg::*
  `endif
  ; 
();

logic clk;
logic arst_n;


////////////////////////////////
//// TQU Interfaces ////////////
////////////////////////////////
egr_prc_tqu_if                       prc_tqu_if();
egr_rrs_if    #(.N_RRSPS(3))    rrs_tqu_mri_if0();
egr_rrs_if    #(.N_RRSPS(3))    rrs_tqu_mri_if1();
egr_tcu_tqu_if                       tcu_tqu_if();

////////////////////////////////
//// TQU Signals ///////////////
////////////////////////////////
pkt_buf_addr_t               pkt_buf_ctrl_addr;
logic                       pkt_buf_ctrl_rd_en;
logic                       pkt_buf_ctrl_wr_en;
egr_int_pkg::data_word_t pkt_buf_ctrl_word_out;
egr_int_pkg::data_word_t  pkt_buf_ctrl_word_in;
pkt_buf_addr_t               pkt_buf_data_addr;
logic                       pkt_buf_data_rd_en;
logic                       pkt_buf_data_wr_en;
egr_int_pkg::data_word_t pkt_buf_data_word_out;
egr_int_pkg::data_word_t  pkt_buf_data_word_in;

////////////////////////////////
//// TQU DUT ///////////////////
////////////////////////////////
tqu dut(
    .clk(clk),
    .rst_n(arst_n), 
    .prc_if(prc_tqu_if),        //Packet Read Controller   - Transmit Queuing Unit Interface
    .tcu_if(tcu_tqu_if),        //Transmit Controller Unit - Transmit Queuing Unit Interface
    .mri_if0(rrs_tqu_mri_if0),  //Read Response Interface. Gives service to EPL 0,1
    .mri_if1(rrs_tqu_mri_if1),  //Read Response Interface. Gives service to EPL 2,3
    //Data Buffer
    //Control Memory Bank
    .pkt_buf_ctrl_addr     (    pkt_buf_ctrl_addr),//o
    .pkt_buf_ctrl_rd_en    (   pkt_buf_ctrl_rd_en),//o
    .pkt_buf_ctrl_wr_en    (   pkt_buf_ctrl_wr_en),//o
    .pkt_buf_ctrl_word_out (pkt_buf_ctrl_word_out),//o
    .pkt_buf_ctrl_word_in  ( pkt_buf_ctrl_word_in),//i
                                                   
    //Data Memory Bank                             
    .pkt_buf_data_addr     (    pkt_buf_data_addr),//o
    .pkt_buf_data_rd_en    (   pkt_buf_data_rd_en),//o
    .pkt_buf_data_wr_en    (   pkt_buf_data_wr_en),//o
    .pkt_buf_data_word_out (pkt_buf_data_word_out),//o
    .pkt_buf_data_word_in  ( pkt_buf_data_word_in) //i
);

////////////////////////////////
//// MEMORIES //////////////////
////////////////////////////////
mby_mgm_1rw_behave #( .MEM_WIDTH(W_WORD_BITS),
                      .MEM_DEPTH(TQU_BUF_DEPTH)
)
tqu_ctrl_buf
(
    .clk     (                   clk),
    .address (     pkt_buf_ctrl_addr),
    .rd_en   (    pkt_buf_ctrl_rd_en),
    .wr_en   (    pkt_buf_ctrl_wr_en),
    .data_in ( pkt_buf_ctrl_word_out),
    .data_out(  pkt_buf_ctrl_word_in)
);

mby_mgm_1rw_behave #( .MEM_WIDTH(W_WORD_BITS),
                      .MEM_DEPTH(TQU_BUF_DEPTH)
)
tqu_data_buf
(
    .clk     (                   clk),
    .address (     pkt_buf_data_addr),
    .rd_en   (    pkt_buf_data_rd_en),
    .wr_en   (    pkt_buf_data_wr_en),
    .data_in ( pkt_buf_data_word_out),
    .data_out(  pkt_buf_data_word_in)
);

////////////////////////////////
//// SANDBOX RUNTIME ///////////
////////////////////////////////
mby_sandbox_runtime runtime(.clk(clk), .rst_n(arst_n));


////////////////////////////////
//// TESTBENCH RUNTIME /////////
////////////////////////////////
initial
begin
    @(posedge clk);
    @(posedge clk);
    @(posedge clk);
    @(posedge rst_n);
    @(posedge clk);

end

endmodule : tqu_tb
