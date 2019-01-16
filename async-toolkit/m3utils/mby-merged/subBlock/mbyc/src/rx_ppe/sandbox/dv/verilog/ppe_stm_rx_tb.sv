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

module ppe_stm_rx_tb
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

rx_ppe_ppe_stm0_if  rx_ppe_ppe_stm0_if0();
rx_ppe_ppe_stm1_if  rx_ppe_ppe_stm1_if0();
rx_ppe_ppe_stm0_if  rx_ppe_ppe_stm0_if1();
rx_ppe_ppe_stm1_if  rx_ppe_ppe_stm1_if1();

//Reset
always @(posedge cclk) begin //{
    if(cclk_cnt == 5) reset <= 1'b0;
    else if(cclk_cnt == 10) reset <= 1'b1;
    else if(cclk_cnt == 15) reset <= 1'b0;
end //}

logic   [5:0]           q_fwd_tbl0_cur_bank;
logic   [47:0] [13:0]   q_fwd_tbl0_waddr;
logic   [47:0] [71:0]   q_fwd_tbl0_wdata;
//Temporary for driving write data
always @(negedge cclk) begin //{
    if((cclk_cnt > 47) && (cclk_cnt < 1000)) begin //{
        force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_waddr[19:14] = q_fwd_tbl0_cur_bank;
        if((cclk_cnt % 5) == 0) begin //{
            case(q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h1; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h2; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h4; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = 72'hffffffffffffffffff;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h8; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_waddr[13:0] = q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank];
        end //}
        else if((cclk_cnt % 5) == 1) begin //{
            case(q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h3; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h6; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'hc; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h9; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_waddr[13:0] = q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank];
        end //}
        else if((cclk_cnt % 5) == 2) begin //{
            case(q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h7; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'he; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'hd; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'hb; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_waddr[13:0] = q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank];
        end //}
        else if((cclk_cnt % 5) == 3) begin //{
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'hf;
            case(q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 3;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 3;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 3;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[0] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[1] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[2] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 3;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wdata[3] = q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_waddr[13:0] = q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank];
        end //}
        else begin //{
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h0;
        end //}
    end //}
    else if(cclk_cnt == 1000) force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl0_wen = 4'h0; 
end //}

always @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        for(int i=0; i<48; i++) begin //{
            q_fwd_tbl0_waddr[i] <= 14'h0;
            q_fwd_tbl0_wdata[i] <= 72'h0;
        end //}
        q_fwd_tbl0_cur_bank <= 6'h0;
    end //}
    else if((cclk_cnt > 48) && (cclk_cnt < 1000)) begin //{
        if(q_fwd_tbl0_cur_bank == 47) q_fwd_tbl0_cur_bank <= 6'b0;
        else q_fwd_tbl0_cur_bank <= q_fwd_tbl0_cur_bank + 1;
        if((cclk_cnt % 5) == 0) begin //{
            q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] + 1;
            q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 1;
        end //}
        else if((cclk_cnt % 5) == 1) begin //{
            q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] + 2;
            q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 2;
        end //}
        else if((cclk_cnt % 5) == 2) begin //{
            q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] + 3;
            q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 3;
        end //}
        else if((cclk_cnt % 5) == 3) begin //{
            q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_waddr[q_fwd_tbl0_cur_bank] + 4;
            q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] <= q_fwd_tbl0_wdata[q_fwd_tbl0_cur_bank] + 4;
        end //}
    end //}
end //}

logic   [3:0]           q_fwd_tbl1_cur_bank;
logic   [15:0] [12:0]   q_fwd_tbl1_waddr;
logic   [15:0] [71:0]   q_fwd_tbl1_wdata;
//Temporary for driving write data
always @(negedge cclk) begin //{
    if((cclk_cnt > 47) && (cclk_cnt < 1000)) begin //{
        force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_waddr[16:13] = q_fwd_tbl1_cur_bank;
        if((cclk_cnt % 5) == 0) begin //{
            case(q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h1; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h2; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h4; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = 72'hffffffffffffffffff;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h8; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_waddr[12:0] = q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank];
        end //}
        else if((cclk_cnt % 5) == 1) begin //{
            case(q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h3; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h6; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'hc; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h9; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_waddr[12:0] = q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank];
        end //}
        else if((cclk_cnt % 5) == 2) begin //{
            case(q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h7; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = 72'hffffffffffffffffff;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'he; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'hd; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'hb; 
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = 72'hffffffffffffffffff;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_waddr[12:0] = q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank];
        end //}
        else if((cclk_cnt % 5) == 3) begin //{
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'hf;
            case(q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank][1:0]) //{
                2'h0: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 3;
                end //}
                2'h1: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 3;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                end //}
                2'h2: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 3;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                end //}
                default: begin //{
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[0] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[1] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[2] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 3;
                    force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wdata[3] = q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank];
                end //}
            endcase //}
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_waddr[12:0] = q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank];
        end //}
        else begin //{
            force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h0;
        end //}
    end //}
    else if(cclk_cnt == 1000) force ppe_stm_rx_top.ppe_stm_rx_logic.q_fwd_tbl1_wen = 4'h0; 
end //}

always @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        for(int i=0; i<16; i++) begin //{
            q_fwd_tbl1_waddr[i] <= 13'h0;
            q_fwd_tbl1_wdata[i] <= 72'h0;
        end //}
        q_fwd_tbl1_cur_bank <= 4'h0;
    end //}
    else if((cclk_cnt > 48) && (cclk_cnt < 1000)) begin //{
        q_fwd_tbl1_cur_bank <= q_fwd_tbl1_cur_bank + 1;
        if((cclk_cnt % 5) == 0) begin //{
            q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] + 1;
            q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 1;
        end //}
        else if((cclk_cnt % 5) == 1) begin //{
            q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] + 2;
            q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 2;
        end //}
        else if((cclk_cnt % 5) == 2) begin //{
            q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] + 3;
            q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 3;
        end //}
        else if((cclk_cnt % 5) == 3) begin //{
            q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_waddr[q_fwd_tbl1_cur_bank] + 4;
            q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] <= q_fwd_tbl1_wdata[q_fwd_tbl1_cur_bank] + 4;
        end //}
    end //}
end //}

rx_ppe_dr rx_ppe (
    .cclk                   (cclk),
    .cclk_cnt               (cclk_cnt),
    .rx_ppe_ppe_stm0_if0    (rx_ppe_ppe_stm0_if0),
    .rx_ppe_ppe_stm1_if0    (rx_ppe_ppe_stm1_if0),
    .rx_ppe_ppe_stm0_if1    (rx_ppe_ppe_stm0_if1),
    .rx_ppe_ppe_stm1_if1    (rx_ppe_ppe_stm1_if1)
);

ppe_stm_rx_top  ppe_stm_rx_top (
    .cclk                   (cclk),
    .reset                  (reset),

    .i_ibus_ctrl            (99'b0),
    .o_ibus_resp            (),

    .rx_ppe_ppe_stm0_if0    (rx_ppe_ppe_stm0_if0),
    .rx_ppe_ppe_stm1_if0    (rx_ppe_ppe_stm1_if0),
    .rx_ppe_ppe_stm0_if1    (rx_ppe_ppe_stm0_if1),
    .rx_ppe_ppe_stm1_if1    (rx_ppe_ppe_stm1_if1)
);

endmodule
