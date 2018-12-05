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
// -- Description  : Tag Queuing Unit
//------------------------------------------------------------------------------

module tqu 
    import egr_int_pkg::*;
    #(parameter N_RRSPS = 3)
(
    input logic             clk,
    input logic           rst_n, 

    //EGR Internal Interfaces 
    egr_prc_tqu_if.tqu   prc_if,  // Packet Read Controller   - Transmit Queuing Unit Interface
    egr_tcu_tqu_if.tqu   tcu_if,  // Transmit Controller Unit - Transmit Queunig Unit Interface

    egr_rrs_if.requestor mri_if0, // Read Response Interface. Gives service to EPL 0,1
    egr_rrs_if.requestor mri_if1,  // Read Response Interface. Gives service to EPL 2,3

    //Data Buffer
    //Control Memory Bank
    output pkt_buf_addr_t     pkt_buf_ctrl_addr, //o
    output logic             pkt_buf_ctrl_rd_en, //o
    output logic             pkt_buf_ctrl_wr_en, //o
    output data_word_t    pkt_buf_ctrl_word_out, //o
    input  data_word_t     pkt_buf_ctrl_word_in, //i
                                                 
    //Data Memory Bank                           
    output pkt_buf_addr_t     pkt_buf_data_addr, //o
    output logic             pkt_buf_data_rd_en, //o
    output logic             pkt_buf_data_wr_en, //o
    output data_word_t    pkt_buf_data_word_out, //o
    input  data_word_t     pkt_buf_data_word_in  //i
);
//        input   logic                           .clk     (clk)                     ,
//        input   logic   [MEM_ADR_WIDTH-1:0]     .address (                ,
//        input   logic                           .rd_en   (                ,
//        input   logic   [MEM_WR_EN_WIDTH-1:0]   .wr_en   (                ,
//        input   logic   [      MEM_WIDTH-1:0]   .data_in (                ,
//        output  logic   [      MEM_WIDTH-1:0]   .data_out(


//req_id_t    [N_RRSPS-1:0]    rrsp_wd_id; // Read Response Word ID
//data_word_t [N_RRSPS-1:0]       rrsp_wd; // Read Response Word
//logic       [N_RRSPS-1:0] rrsp_wd_valid; // Read Response Word Valid
//logic       [N_RRSPS-1:0] rrsp_wd_stall; // Read Response Word Stall
//input     rrsp_wd_id,
//input        rrsp_wd,
//input  rrsp_wd_valid,
//output rrsp_wd_stall

//FIRST PACKET LOGIC
localparam FST_PKT_CTRL_LEN = 1;  //One word packet metadata for first packet
localparam FST_PKT_DATA_LEN = 1;  //One word packet data for first packet
localparam FST_PKT_LEN   = FST_PKT_CTRL_LEN + FST_PKT_DATA_LEN; //1 Ctrl + 1 Data words for first packet

//RRS data registers
data_wd_id_t     rrsp_data_wd_id;
data_wd_id_t rrsp_data_wd_id_reg;
data_word_t       rrsp_data_word;
data_word_t   rrsp_data_word_reg;

always_ff @(posedge clk, negedge rst_n)
    if(!rst_n) rrsp_data_wd_id_reg <= '0;
    else begin
        rrsp_data_wd_id_reg <= rrsp_data_wd_id;
    end

assign rrsp_data_wd_id = mri_if0.rrsp_wd_valid[0] ? 
                         mri_if0.rrsp_wd_id[0].data_wd_id : 
                         rrsp_data_wd_id_reg;

always_ff @(posedge clk, negedge rst_n)
    if(!rst_n) rrsp_data_word_reg <= '0;
    else begin
        rrsp_data_word_reg <= rrsp_data_word;
    end

assign rrsp_data_word = mri_if0.rrsp_wd_valid[0] ? 
                         mri_if0.rrsp_wd[0] : 
                         rrsp_data_word_reg;

//RRS to Packet Buffer
logic rrsp_ctrl_ready;
logic rrsp_ctrl_ready_reg;
logic rrsp_data_ready;
logic rrsp_data_ready_reg;
logic [$clog2(FST_PKT_LEN):0] word_counter;
logic [$clog2(FST_PKT_LEN):0] word_counter_reg;

always_ff @(posedge clk, negedge rst_n)
    if(!rst_n) begin
        rrsp_ctrl_ready_reg <= '0;
        rrsp_data_ready_reg <= '0;
        word_counter_reg    <= '0;
    end
    else begin
        rrsp_ctrl_ready_reg <= rrsp_ctrl_ready;
        rrsp_data_ready_reg <= rrsp_data_ready;
        word_counter_reg    <= word_counter;
    end

assign rrsp_ctrl_ready = ((mri_if0.rrsp_wd_valid[0]) && 
                            (mri_if0.rrsp_wd_id[0].data_wd_id.word_type==WORD_TYPE_CTRL)) ? 
                         '1 : 
                         rrsp_ctrl_ready_reg;

assign rrsp_data_ready = ((mri_if0.rrsp_wd_valid[0]) && 
                            (mri_if0.rrsp_wd_id[0].data_wd_id.word_type==WORD_TYPE_DATA)) ? 
                         '1 : 
                         rrsp_data_ready_reg;

assign word_counter = mri_if0.rrsp_wd_valid[0] ?
                      word_counter_reg + 1     :
                      word_counter_reg;

//Stall outputs
logic rrsp_data_stall;

assign rrsp_data_stall          = word_counter_reg==FST_PKT_LEN;
assign mri_if0.rrsp_wd_stall[0] = rrsp_data_stall;
assign mri_if0.rrsp_wd_stall[1] = '1;
assign mri_if0.rrsp_wd_stall[2] = '1;
assign mri_if1.rrsp_wd_stall[0] = '1;
assign mri_if1.rrsp_wd_stall[1] = '1;
assign mri_if1.rrsp_wd_stall[2] = '1;


  

//logic       wd_valid [N_EPP_PER_MGP][N_EPL_PER_EPP];
//data_word_t     word [N_EPP_PER_MGP][N_EPL_PER_EPP];
//dtq_sel_t    dtq_sel [N_EPP_PER_MGP][N_EPL_PER_EPP]; 
//
//
//tqu_rrsp_rcv tqu_rrsp_rcv0(
//    .clk      (     clk   ),
//    .rst_n    (   rst_n   ), 
//    .mri_if   ( mri_if0   ), 
//    .wd_valid (wd_valid[0]), 
//    .word     (    word[0]),
//    .dtq_sel  ( dtq_sel[0])
//);
//
//tqu_rrsp_rcv tqu_rrsp_rcv1(
//    .clk      (     clk   ),
//    .rst_n    (   rst_n   ), 
//    .mri_if   ( mri_if1   ), 
//    .wd_valid (wd_valid[1]), 
//    .word     (    word[1]),
//    .dtq_sel  ( dtq_sel[1])
//);
//
//// 4 Packet Buffers
////tcu_if
//    ////Buffer interface
//    //Service dtq_ctrl_pull
//    dtq_sel_t        buf_dtq_ctrl_sel,
//    logic           buf_dtq_ctrl_read,
//    data_word_t     buf_dtq_ctrl_word,
//    dtq_ready_t    buf_dtq_ctrl_ready,
//    //Service dtq_data_pull
//    dtq_sel_t          dtq_data_sel,
//
endmodule : tqu
