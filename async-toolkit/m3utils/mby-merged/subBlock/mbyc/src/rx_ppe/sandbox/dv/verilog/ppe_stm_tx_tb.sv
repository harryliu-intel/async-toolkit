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
// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Jon Bagge <jon.bagge@intel.com>
// -- Project Name : Madison Bay
// -- Description : PPE Shared table memory top level verification file
// --
// -------------------------------------------------------------------
`timescale 1ps/1ps

module ppe_stm_tx_tb
();

localparam CCLK_PERIOD = 816ps;

logic           cclk;
int             cclk_cnt;
logic           reset;
logic   [31:0]  start_dumping;

//Clock Generator
initial begin //{
    cclk = 0;
    cclk_cnt = 0;
    #(4 * CCLK_PERIOD);
    forever cclk = #(CCLK_PERIOD/2) ~cclk;
end //}

initial $value$plusargs("start_dumping=%d",start_dumping);

always @(posedge cclk) begin //{
    cclk_cnt <= cclk_cnt + 1;
end //}

always_ff @(posedge cclk) begin //{
    if(cclk_cnt == start_dumping) begin //{
        $vcdpluson();
        $vcdplusmemon();
    end //}

    if(cclk_cnt == 2000) begin //{
        $display("Simulation PASSED\n");
        $finish;
    end //}
end //}

tx_ppe_mod_if       tx_ppe_mod_if0();
tx_ppe_mod_if       tx_ppe_mod_if1();
tx_ppe_mod_if       tx_ppe_mod_if2();
tx_ppe_mod_if       tx_ppe_mod_if3();
tx_ppe_negh_if      tx_ppe_negh_if0();
tx_ppe_negh_if      tx_ppe_negh_if1();
tx_ppe_negh_if      tx_ppe_negh_if2();
tx_ppe_negh_if      tx_ppe_negh_if3();
egr_mc_table_if     mc_table_if0_0();
egr_mc_table_if     mc_table_if0_1();
egr_mc_table_if     mc_table_if1_0();
egr_mc_table_if     mc_table_if1_1();

//Reset
always @(posedge cclk) begin //{
    if(cclk_cnt == 5) reset <= 1'b0;
    else if(cclk_cnt == 10) reset <= 1'b1;
    else if(cclk_cnt == 15) reset <= 1'b0;
end //}

logic   [3:0]           q_mod_tbl_cur_bank;
logic   [9:0] [13:0]    q_mod_tbl_waddr;
logic   [9:0] [71:0]    q_mod_tbl_wdata;
//Temporary for driving write data
always @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        for(int i=0; i<10; i++) begin //{
            q_mod_tbl_waddr[i] <= 14'h0;
            q_mod_tbl_wdata[i] <= 72'h0;
        end //}
        q_mod_tbl_cur_bank <= 4'h0;
    end //}
    else if((cclk_cnt > 49) && (cclk_cnt < 1000)) begin //{
        force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[17:14] = q_mod_tbl_cur_bank;
        if(q_mod_tbl_cur_bank == 9) q_mod_tbl_cur_bank <= 4'b0;
        else q_mod_tbl_cur_bank <= q_mod_tbl_cur_bank + 1;
        if((cclk_cnt % 9) == 0) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h1; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h2; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h4; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h8; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h10; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h20; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h40; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h80; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 1;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
        end //}
        else if((cclk_cnt % 9) == 1) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h3; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h6; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hc; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h18; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h30; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h60; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hc0; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h81; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 2;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
        end //}
        else if((cclk_cnt % 9) == 2) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h7; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'he; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h1c; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h38; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h70; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'he0; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hc1; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h83; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 3;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
        end //}
        else if((cclk_cnt % 9) == 3) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h1e; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h3c; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h78; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf0; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'he1; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hc3; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h87; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 4;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
        end //}
        else if((cclk_cnt % 9) == 4) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h1f; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h3e; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h7c; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf8; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf1; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'he3; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hc7; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h8f; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 5;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
        end //}
        else if((cclk_cnt % 9) == 5) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h3f; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h7e; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hfc; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf9; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf3; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'he7; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hcf; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h9f; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 6;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
        end //}
        else if((cclk_cnt % 9) == 6) begin //{
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h7f; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = 72'hffffffffffffffffff;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hfe; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hfd; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hfb; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hf7; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hef; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hdf; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hbf; 
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = 72'hffffffffffffffffff;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 7;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
        end //}
        else if((cclk_cnt % 9) == 6) begin //{
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'hff;
            case(q_mod_tbl_waddr[q_mod_tbl_cur_bank][2:0]) //{
                3'h0: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                end //}
                3'h1: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                end //}
                3'h2: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                end //}
                3'h3: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                end //}
                3'h4: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                end //}
                3'h5: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                end //}
                3'h6: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[0] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 1;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[1] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 2;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[2] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 3;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[3] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 4;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[4] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 5;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[5] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 6;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[6] = q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 7;
                    force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wdata[7] = q_mod_tbl_wdata[q_mod_tbl_cur_bank];
                end //}
            endcase //}
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_waddr[13:0] = q_mod_tbl_waddr[q_mod_tbl_cur_bank];
            q_mod_tbl_waddr[q_mod_tbl_cur_bank] <= q_mod_tbl_waddr[q_mod_tbl_cur_bank] + 8;
            q_mod_tbl_wdata[q_mod_tbl_cur_bank] <= q_mod_tbl_wdata[q_mod_tbl_cur_bank] + 8;
        end //}
        else begin //{
            force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h0;
        end //}
    end //}
    else if(cclk_cnt == 1000) force ppe_stm_tx_top.ppe_stm_tx_logic.q_mod_tbl_wen = 8'h0; 
end //}

logic   [13:0]    q_negh_tbl_waddr;
logic   [81:0]    q_negh_tbl_wdata;
//Temporary for driving write data
always @(negedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        q_negh_tbl_waddr <= 14'h3fff;
        q_negh_tbl_wdata <= 82'h3ffffffffffffffffffff;
    end //}
    else if((cclk_cnt > 49) && (cclk_cnt < 1000)) begin //{
        force ppe_stm_tx_top.ppe_stm_tx_logic.q_negh_tbl_waddr[0] = q_negh_tbl_waddr;
        force ppe_stm_tx_top.ppe_stm_tx_logic.q_negh_tbl_waddr[1] = q_negh_tbl_waddr;
        force ppe_stm_tx_top.ppe_stm_tx_logic.q_negh_tbl_wen = 2'h3; 
        force ppe_stm_tx_top.ppe_stm_tx_logic.q_negh_tbl_wdata[0] = q_negh_tbl_wdata;
        force ppe_stm_tx_top.ppe_stm_tx_logic.q_negh_tbl_wdata[1] = q_negh_tbl_wdata + 1;
        q_negh_tbl_waddr <= q_negh_tbl_waddr + 1;
        q_negh_tbl_wdata <= q_negh_tbl_wdata + 1;
    end //}
    else if(cclk_cnt == 1000) force ppe_stm_tx_top.ppe_stm_tx_logic.q_negh_tbl_wen = 2'h0; 
end //}

tx_ppe_dr tx_ppe (
    .cclk               (cclk),
    .cclk_cnt           (cclk_cnt),

    .tx_ppe_mod_if0     (tx_ppe_mod_if0),
    .tx_ppe_mod_if1     (tx_ppe_mod_if1),
    .tx_ppe_mod_if2     (tx_ppe_mod_if2),
    .tx_ppe_mod_if3     (tx_ppe_mod_if3),

    .tx_ppe_negh_if0    (tx_ppe_negh_if0),
    .tx_ppe_negh_if1    (tx_ppe_negh_if1),
    .tx_ppe_negh_if2    (tx_ppe_negh_if2),
    .tx_ppe_negh_if3    (tx_ppe_negh_if3)
);

ppe_stm_tx_top  ppe_stm_tx_top (
    .cclk               (cclk),
    .reset              (reset),

    .i_ibus_ctrl        (99'b0),
    .o_ibus_resp        (),

    .tx_ppe_mod_if0     (tx_ppe_mod_if0),
    .tx_ppe_mod_if1     (tx_ppe_mod_if1),
    .tx_ppe_mod_if2     (tx_ppe_mod_if2),
    .tx_ppe_mod_if3     (tx_ppe_mod_if3),

    .tx_ppe_negh_if0    (tx_ppe_negh_if0),
    .tx_ppe_negh_if1    (tx_ppe_negh_if1),
    .tx_ppe_negh_if2    (tx_ppe_negh_if2),
    .tx_ppe_negh_if3    (tx_ppe_negh_if3),

    .mc_table_if0_0     (mc_table_if0_0),
    .mc_table_if0_1     (mc_table_if0_1),
    .mc_table_if1_0     (mc_table_if1_0),
    .mc_table_if1_1     (mc_table_if1_1)
);

endmodule
