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
// -- Description : RX PPE Driver for ppe_stm testbed
// --
// -------------------------------------------------------------------
`timescale 1ps/1ps

module rx_ppe_dr
(
input   logic           cclk,
input   int             cclk_cnt,
rx_ppe_ppe_stm0_if.ppe  rx_ppe_ppe_stm0_if0,
rx_ppe_ppe_stm1_if.ppe  rx_ppe_ppe_stm1_if0,
rx_ppe_ppe_stm0_if.ppe  rx_ppe_ppe_stm0_if1,
rx_ppe_ppe_stm1_if.ppe  rx_ppe_ppe_stm1_if1
);

logic                   q_stm0_0_lpm_rvalid;
logic   [143:0]         q_stm0_0_lpm_rdata;
logic   [1:0]           q_stm0_0_em_rvalid;
logic   [1:0] [287:0]   q_stm0_0_em_rdata;
logic                   q_stm0_1_lpm_rvalid;
logic   [143:0]         q_stm0_1_lpm_rdata;
logic   [1:0]           q_stm0_1_em_rvalid;
logic   [1:0] [287:0]   q_stm0_1_em_rdata;
logic   [5:0] [13:0]    q_stm0_raddr;
logic   [5:0] [5:0]     q_stm0_cur_bank;
logic   [1:0] [143:0]   q_lpm_exp_rdata;
logic   [3:0] [287:0]   q_stm0_em_exp_rdata;
logic   [5:0] [5:0]     q_stm0_exp_bank;

logic   [1:0]           q_stm1_0_em_rvalid;
logic   [1:0] [287:0]   q_stm1_0_em_rdata;
logic   [1:0]           q_stm1_1_em_rvalid;
logic   [1:0] [287:0]   q_stm1_1_em_rdata;
logic   [3:0] [12:0]    q_stm1_raddr;
logic   [3:0] [3:0]     q_stm1_cur_bank;
logic   [3:0] [287:0]   q_stm1_em_exp_rdata;
logic   [3:0] [3:0]     q_stm1_exp_bank;

always_ff @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        rx_ppe_ppe_stm0_if0.ren[0]      <= 4'b0;
        rx_ppe_ppe_stm0_if0.ren[1]      <= 4'b0;
        rx_ppe_ppe_stm0_if0.ren[2]      <= 4'b0;
        rx_ppe_ppe_stm0_if0.raddr[0]    <= 20'h0;
        rx_ppe_ppe_stm0_if0.raddr[1]    <= 20'h0;
        rx_ppe_ppe_stm0_if0.raddr[2]    <= 20'h0;
        rx_ppe_ppe_stm0_if1.ren[0]      <= 4'b0;
        rx_ppe_ppe_stm0_if1.ren[1]      <= 4'b0;
        rx_ppe_ppe_stm0_if1.ren[2]      <= 4'b0;
        rx_ppe_ppe_stm0_if1.raddr[0]    <= 20'h0;
        rx_ppe_ppe_stm0_if1.raddr[1]    <= 20'h0;
        rx_ppe_ppe_stm0_if1.raddr[2]    <= 20'h0;
        for(int i=0; i<6; i++) begin //{
            q_stm0_cur_bank[i]   <= i;
            q_stm0_raddr[i]      <= 14'b0;
            q_stm0_exp_bank[i]   <= i;
        end //}
        q_lpm_exp_rdata[0] <= 144'h1000000000000000000;
        q_lpm_exp_rdata[1] <= 144'h1000000000000000000;
        q_stm0_em_exp_rdata[0] <= 288'h3000000000000000002000000000000000001000000000000000000;
        q_stm0_em_exp_rdata[1] <= 288'h3000000000000000002000000000000000001000000000000000000;
        q_stm0_em_exp_rdata[2] <= 288'h3000000000000000002000000000000000001000000000000000000;
        q_stm0_em_exp_rdata[3] <= 288'h3000000000000000002000000000000000001000000000000000000;
    end //}
    else if(cclk_cnt > 1000) begin //{
        case(q_stm0_raddr[0][1:0]) //{
            2'h0: rx_ppe_ppe_stm0_if0.ren[0] <= 4'h3;
            2'h1: rx_ppe_ppe_stm0_if0.ren[0] <= 4'h6;
            2'h2: rx_ppe_ppe_stm0_if0.ren[0] <= 4'hc;
            default: rx_ppe_ppe_stm0_if0.ren[0] <= 4'h9;
        endcase //}
        rx_ppe_ppe_stm0_if0.raddr[0][19:14] <= q_stm0_cur_bank[0];
        rx_ppe_ppe_stm0_if0.raddr[0][13:0]  <= q_stm0_raddr[0];
        if(q_stm0_cur_bank[0] == 47) begin //{
            q_stm0_raddr[0] <= q_stm0_raddr[0] + 1;
            q_stm0_cur_bank[0] <= 6'b0;
        end //}
        else q_stm0_cur_bank[0] <= q_stm0_cur_bank[0] + 1;

        rx_ppe_ppe_stm0_if0.ren[1] <= 4'hf;
        rx_ppe_ppe_stm0_if0.raddr[1][19:14] <= q_stm0_cur_bank[1];
        rx_ppe_ppe_stm0_if0.raddr[1][13:0]  <= q_stm0_raddr[1];
        if(q_stm0_cur_bank[1] == 47) begin //{
            q_stm0_raddr[1] <= q_stm0_raddr[1] + 1;
            q_stm0_cur_bank[1] <= 6'b0;
        end //}
        else q_stm0_cur_bank[1] <= q_stm0_cur_bank[1] + 1;

        rx_ppe_ppe_stm0_if0.ren[2] <= 4'hf;
        rx_ppe_ppe_stm0_if0.raddr[2][19:14] <= q_stm0_cur_bank[2];
        rx_ppe_ppe_stm0_if0.raddr[2][13:0]  <= q_stm0_raddr[2];
        if(q_stm0_cur_bank[2] == 47) begin //{
            q_stm0_raddr[2] <= q_stm0_raddr[2] + 1;
            q_stm0_cur_bank[2] <= 6'b0;
        end //}
        else q_stm0_cur_bank[2] <= q_stm0_cur_bank[2] + 1;

        case(q_stm0_raddr[3][1:0]) //{
            2'h0: rx_ppe_ppe_stm0_if1.ren[0] <= 4'h3;
            2'h1: rx_ppe_ppe_stm0_if1.ren[0] <= 4'h6;
            2'h2: rx_ppe_ppe_stm0_if1.ren[0] <= 4'hc;
            default: rx_ppe_ppe_stm0_if1.ren[0] <= 4'h9;
        endcase //}
        rx_ppe_ppe_stm0_if1.raddr[0][19:14] <= q_stm0_cur_bank[3];
        rx_ppe_ppe_stm0_if1.raddr[0][13:0]  <= q_stm0_raddr[3];
        if(q_stm0_cur_bank[3] == 47) begin //{
            q_stm0_raddr[3] <= q_stm0_raddr[3] + 1;
            q_stm0_cur_bank[3] <= 6'b0;
        end //}
        else q_stm0_cur_bank[3] <= q_stm0_cur_bank[3] + 1;

        rx_ppe_ppe_stm0_if1.ren[1] <= 4'hf;
        rx_ppe_ppe_stm0_if1.raddr[1][19:14] <= q_stm0_cur_bank[4];
        rx_ppe_ppe_stm0_if1.raddr[1][13:0]  <= q_stm0_raddr[4];
        if(q_stm0_cur_bank[4] == 47) begin //{
            q_stm0_raddr[4] <= q_stm0_raddr[4] + 1;
            q_stm0_cur_bank[4] <= 6'b0;
        end //}
        else q_stm0_cur_bank[4] <= q_stm0_cur_bank[4] + 1;

        rx_ppe_ppe_stm0_if1.ren[2] <= 4'hf;
        rx_ppe_ppe_stm0_if1.raddr[2][19:14] <= q_stm0_cur_bank[5];
        rx_ppe_ppe_stm0_if1.raddr[2][13:0]  <= q_stm0_raddr[5];
        if(q_stm0_cur_bank[5] == 47) begin //{
            q_stm0_raddr[5] <= q_stm0_raddr[5] + 1;
            q_stm0_cur_bank[5] <= 6'b0;
        end //}
        else q_stm0_cur_bank[5] <= q_stm0_cur_bank[5] + 1;
    end //}

    q_stm0_0_lpm_rvalid <= rx_ppe_ppe_stm0_if0.lpm_rvalid;
    q_stm0_0_lpm_rdata  <= rx_ppe_ppe_stm0_if0.lpm_rdata;
    q_stm0_0_em_rvalid  <= rx_ppe_ppe_stm0_if0.em_rvalid;
    q_stm0_0_em_rdata   <= rx_ppe_ppe_stm0_if0.em_rdata;

    q_stm0_1_lpm_rvalid <= rx_ppe_ppe_stm0_if1.lpm_rvalid;
    q_stm0_1_lpm_rdata  <= rx_ppe_ppe_stm0_if1.lpm_rdata;
    q_stm0_1_em_rvalid  <= rx_ppe_ppe_stm0_if1.em_rvalid;
    q_stm0_1_em_rdata   <= rx_ppe_ppe_stm0_if1.em_rdata;

    if(cclk_cnt > 1008) begin //{
        if(q_stm0_0_lpm_rvalid !== 1'b1) begin //{
            $display("ERROR, Shared Table0 Inteface 0 LPM read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm0_0_lpm_rdata !== q_lpm_exp_rdata[0]) begin //{
            $display("ERROR, Shared Table0 Interface 0 LPM read data miscompare\n actual   = %h\n expected = %h\n", q_stm0_0_lpm_rdata, q_lpm_exp_rdata[0]);
            $finish;
        end //}
        if(q_stm0_exp_bank[0] == 47) begin //{
            q_lpm_exp_rdata[0] <= q_lpm_exp_rdata[0] + 144'h1000000000000000001;
            q_stm0_exp_bank[0] = 6'b0;
        end //}
        else q_stm0_exp_bank[0] <= q_stm0_exp_bank[0] + 1;

        if(q_stm0_0_em_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Shared Table0 Inteface 0 EM 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm0_0_em_rdata[0] !== q_stm0_em_exp_rdata[0]) begin //{
            $display("ERROR, Shared Table0 Interface 0 EM 0 read data miscompare\n actual   = %h\n expected = %h\n", q_stm0_0_em_rdata[0], q_stm0_em_exp_rdata[0]);
            $finish;
        end //}
        if(q_stm0_exp_bank[1] == 47) begin //{
            q_stm0_em_exp_rdata[0] <= q_stm0_em_exp_rdata[0] + 288'h1000000000000000001000000000000000001000000000000000001;
            q_stm0_exp_bank[1] = 6'b0;
        end //}
        else q_stm0_exp_bank[1] <= q_stm0_exp_bank[1] + 1;

        if(q_stm0_0_em_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Shared Table0 Inteface 0 EM 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm0_0_em_rdata[1] !== q_stm0_em_exp_rdata[1]) begin //{
            $display("ERROR, Shared Table0 Interface 0 EM 1 read data miscompare\n actual   = %h\n expected = %h\n", q_stm0_0_em_rdata[1], q_stm0_em_exp_rdata[1]);
            $finish;
        end //}
        if(q_stm0_exp_bank[2] == 47) begin //{
            q_stm0_em_exp_rdata[1] <= q_stm0_em_exp_rdata[1] + 288'h1000000000000000001000000000000000001000000000000000001;
            q_stm0_exp_bank[2] = 6'b0;
        end //}
        else q_stm0_exp_bank[2] <= q_stm0_exp_bank[2] + 1;

        if(q_stm0_1_lpm_rvalid !== 1'b1) begin //{
            $display("ERROR, Shared Table0 Inteface 1 LPM read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm0_1_lpm_rdata !== q_lpm_exp_rdata[1]) begin //{
            $display("ERROR, Shared Table0 Interface 1 LPM read data miscompare\n actual   = %h\n expected = %h\n", q_stm0_1_lpm_rdata, q_lpm_exp_rdata[1]);
            $finish;
        end //}
        if(q_stm0_exp_bank[3] == 47) begin //{
            q_lpm_exp_rdata[1] <= q_lpm_exp_rdata[1] + 144'h1000000000000000001;
            q_stm0_exp_bank[3] = 6'b0;
        end //}
        else q_stm0_exp_bank[3] <= q_stm0_exp_bank[3] + 1;

        if(q_stm0_1_em_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Shared Table0 Inteface 1 EM 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm0_1_em_rdata[0] !== q_stm0_em_exp_rdata[2]) begin //{
            $display("ERROR, Shared Table0 Interface 1 EM 0 read data miscompare\n actual   = %h\n expected = %h\n", q_stm0_1_em_rdata[0], q_stm0_em_exp_rdata[2]);
            $finish;
        end //}
        if(q_stm0_exp_bank[4] == 47) begin //{
            q_stm0_em_exp_rdata[2] <= q_stm0_em_exp_rdata[2] + 288'h1000000000000000001000000000000000001000000000000000001;
            q_stm0_exp_bank[4] = 6'b0;
        end //}
        else q_stm0_exp_bank[4] <= q_stm0_exp_bank[4] + 1;

        if(q_stm0_1_em_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Shared Table0 Inteface 1 EM 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm0_1_em_rdata[1] !== q_stm0_em_exp_rdata[3]) begin //{
            $display("ERROR, Shared Table0 Interface 1 EM 1 read data miscompare\n actual   = %h\n expected = %h\n", q_stm0_1_em_rdata[1], q_stm0_em_exp_rdata[3]);
            $finish;
        end //}
        if(q_stm0_exp_bank[5] == 47) begin //{
            q_stm0_em_exp_rdata[3] <= q_stm0_em_exp_rdata[3] + 288'h1000000000000000001000000000000000001000000000000000001;
            q_stm0_exp_bank[5] = 6'b0;
        end //}
        else q_stm0_exp_bank[5] <= q_stm0_exp_bank[5] + 1;
    end //}
end //}

always_ff @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        rx_ppe_ppe_stm1_if0.ren[0]      <= 4'b0;
        rx_ppe_ppe_stm1_if0.ren[1]      <= 4'b0;
        rx_ppe_ppe_stm1_if0.raddr[0]    <= 17'h0;
        rx_ppe_ppe_stm1_if0.raddr[1]    <= 17'h0;
        rx_ppe_ppe_stm1_if1.ren[0]      <= 4'b0;
        rx_ppe_ppe_stm1_if1.ren[1]      <= 4'b0;
        rx_ppe_ppe_stm1_if1.raddr[0]    <= 17'h0;
        rx_ppe_ppe_stm1_if1.raddr[1]    <= 17'h0;
        for(int i=0; i<4; i++) begin //{
            q_stm1_cur_bank[i]   <= i;
            q_stm1_raddr[i]      <= 13'b0;
            q_stm1_exp_bank[i]   <= i;
        end //}
        q_stm1_em_exp_rdata[0] <= 288'h3000000000000000002000000000000000001000000000000000000;
        q_stm1_em_exp_rdata[1] <= 288'h3000000000000000002000000000000000001000000000000000000;
        q_stm1_em_exp_rdata[2] <= 288'h3000000000000000002000000000000000001000000000000000000;
        q_stm1_em_exp_rdata[3] <= 288'h3000000000000000002000000000000000001000000000000000000;
    end //}
    else if(cclk_cnt > 1000) begin //{
        rx_ppe_ppe_stm1_if0.ren[0] <= 4'hf;
        rx_ppe_ppe_stm1_if0.raddr[0][16:13] <= q_stm1_cur_bank[0];
        rx_ppe_ppe_stm1_if0.raddr[0][12:0]  <= q_stm1_raddr[0];
        if(q_stm1_cur_bank[0] == 15) q_stm1_raddr[0] <= q_stm1_raddr[0] + 1;
        q_stm1_cur_bank[0] <= q_stm1_cur_bank[0] + 1;

        rx_ppe_ppe_stm1_if0.ren[1] <= 4'hf;
        rx_ppe_ppe_stm1_if0.raddr[1][16:13] <= q_stm1_cur_bank[1];
        rx_ppe_ppe_stm1_if0.raddr[1][12:0]  <= q_stm1_raddr[1];
        if(q_stm1_cur_bank[1] == 15) q_stm1_raddr[1] <= q_stm1_raddr[1] + 1;
        q_stm1_cur_bank[1] <= q_stm1_cur_bank[1] + 1;

        rx_ppe_ppe_stm1_if1.ren[0] <= 4'hf;
        rx_ppe_ppe_stm1_if1.raddr[0][16:13] <= q_stm1_cur_bank[2];
        rx_ppe_ppe_stm1_if1.raddr[0][12:0]  <= q_stm1_raddr[2];
        if(q_stm1_cur_bank[2] == 15) q_stm1_raddr[2] <= q_stm1_raddr[2] + 1;
        q_stm1_cur_bank[2] <= q_stm1_cur_bank[2] + 1;

        rx_ppe_ppe_stm1_if1.ren[1] <= 4'hf;
        rx_ppe_ppe_stm1_if1.raddr[1][16:13] <= q_stm1_cur_bank[3];
        rx_ppe_ppe_stm1_if1.raddr[1][12:0]  <= q_stm1_raddr[3];
        if(q_stm1_cur_bank[3] == 15) q_stm1_raddr[3] <= q_stm1_raddr[3] + 1;
        q_stm1_cur_bank[3] <= q_stm1_cur_bank[3] + 1;
    end //}

    q_stm1_0_em_rvalid  <= rx_ppe_ppe_stm1_if0.em_rvalid;
    q_stm1_0_em_rdata   <= rx_ppe_ppe_stm1_if0.em_rdata;

    q_stm1_1_em_rvalid  <= rx_ppe_ppe_stm1_if1.em_rvalid;
    q_stm1_1_em_rdata   <= rx_ppe_ppe_stm1_if1.em_rdata;

    if(cclk_cnt > 1008) begin //{
        if(q_stm1_0_em_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Shared Table1 Inteface 0 EM 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm1_0_em_rdata[0] !== q_stm1_em_exp_rdata[0]) begin //{
            $display("ERROR, Shared Table1 Interface 0 EM 0 read data miscompare\n actual   = %h\n expected = %h\n", q_stm1_0_em_rdata[0], q_stm1_em_exp_rdata[0]);
            $finish;
        end //}
        if(q_stm1_exp_bank[0] == 15) q_stm1_em_exp_rdata[0] <= q_stm1_em_exp_rdata[0] + 288'h1000000000000000001000000000000000001000000000000000001;
        q_stm1_exp_bank[0] <= q_stm1_exp_bank[0] + 1;

        if(q_stm1_0_em_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Shared Table1 Inteface 0 EM 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm1_0_em_rdata[1] !== q_stm1_em_exp_rdata[1]) begin //{
            $display("ERROR, Shared Table0 Interface 1 EM 1 read data miscompare\n actual   = %h\n expected = %h\n", q_stm1_0_em_rdata[1], q_stm1_em_exp_rdata[1]);
            $finish;
        end //}
        if(q_stm1_exp_bank[1] == 15) q_stm1_em_exp_rdata[1] <= q_stm1_em_exp_rdata[1] + 288'h1000000000000000001000000000000000001000000000000000001;
        q_stm1_exp_bank[1] <= q_stm1_exp_bank[1] + 1;

        if(q_stm1_1_em_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Shared Table1 Inteface 1 EM 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm1_1_em_rdata[0] !== q_stm1_em_exp_rdata[2]) begin //{
            $display("ERROR, Shared Table1 Interface 1 EM 0 read data miscompare\n actual   = %h\n expected = %h\n", q_stm1_1_em_rdata[0], q_stm1_em_exp_rdata[2]);
            $finish;
        end //}
        if(q_stm1_exp_bank[2] == 15) q_stm1_em_exp_rdata[2] <= q_stm1_em_exp_rdata[2] + 288'h1000000000000000001000000000000000001000000000000000001;
        q_stm1_exp_bank[2] <= q_stm1_exp_bank[2] + 1;

        if(q_stm1_1_em_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Shared Table1 Inteface 1 EM 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_stm1_1_em_rdata[1] !== q_stm1_em_exp_rdata[3]) begin //{
            $display("ERROR, Shared Table1 Interface 1 EM 1 read data miscompare\n actual   = %h\n expected = %h\n", q_stm1_1_em_rdata[1], q_stm1_em_exp_rdata[3]);
            $finish;
        end //}
        if(q_stm1_exp_bank[3] == 15) q_stm1_em_exp_rdata[3] <= q_stm1_em_exp_rdata[3] + 288'h1000000000000000001000000000000000001000000000000000001;
        q_stm1_exp_bank[3] <= q_stm1_exp_bank[3] + 1;
    end //}
end //}

endmodule
