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

module tx_ppe_dr
(
input   logic           cclk,
input   int             cclk_cnt,

egr_ppe_stm_if.egr      egr_ppe_stm_if0,
egr_ppe_stm_if.egr      egr_ppe_stm_if1
);

logic   [1:0]           q_mod_0_rvalid;
logic   [1:0] [575:0]   q_mod_0_rdata;
logic   [1:0]           q_mod_1_rvalid;
logic   [1:0] [575:0]   q_mod_1_rdata;
logic   [3:0] [13:0]    q_mod_raddr;
logic   [3:0] [3:0]     q_mod_cur_bank;
logic   [3:0] [575:0]   q_mod_exp_rdata;
logic   [3:0] [3:0]     q_mod_exp_bank;

always_ff @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        egr_ppe_stm_if0.ren[0]      <= 8'b0;
        egr_ppe_stm_if0.ren[1]      <= 8'b0;
        egr_ppe_stm_if0.raddr[0]    <= 18'h0;
        egr_ppe_stm_if0.raddr[1]    <= 18'h0;
        egr_ppe_stm_if1.ren[0]      <= 8'b0;
        egr_ppe_stm_if1.ren[1]      <= 8'b0;
        egr_ppe_stm_if1.raddr[0]    <= 18'h0;
        egr_ppe_stm_if1.raddr[1]    <= 18'h0;
        for(int i=0; i<4; i++) begin //{
            q_mod_cur_bank[i]   <= i;
            q_mod_raddr[i]      <= 14'b0;
            q_mod_exp_bank[i]   <= i;
        end //}
        q_mod_exp_rdata[0] <= 576'h7000000000000000006000000000000000005000000000000000004000000000000000003000000000000000002000000000000000001000000000000000000;
        q_mod_exp_rdata[1] <= 576'h7000000000000000006000000000000000005000000000000000004000000000000000003000000000000000002000000000000000001000000000000000000;
        q_mod_exp_rdata[2] <= 576'h7000000000000000006000000000000000005000000000000000004000000000000000003000000000000000002000000000000000001000000000000000000;
        q_mod_exp_rdata[3] <= 576'h7000000000000000006000000000000000005000000000000000004000000000000000003000000000000000002000000000000000001000000000000000000;
    end //}
    else if(cclk_cnt > 1000) begin //{
        egr_ppe_stm_if0.ren[0] <= 8'hff;
        egr_ppe_stm_if0.raddr[0][17:14] <= q_mod_cur_bank[0];
        egr_ppe_stm_if0.raddr[0][13:0]  <= q_mod_raddr[0];
        if(q_mod_cur_bank[0] == 9) begin //{
            q_mod_raddr[0] <= q_mod_raddr[0] + 1;
            q_mod_cur_bank[0] <= 4'b0;
        end //}
        else q_mod_cur_bank[0] <= q_mod_cur_bank[0] + 1;

        egr_ppe_stm_if0.ren[1] <= 8'hff;
        egr_ppe_stm_if0.raddr[1][17:14] <= q_mod_cur_bank[1];
        egr_ppe_stm_if0.raddr[1][13:0]  <= q_mod_raddr[1];
        if(q_mod_cur_bank[1] == 9) begin //{
            q_mod_raddr[1] <= q_mod_raddr[1] + 1;
            q_mod_cur_bank[1] <= 4'b0;
        end //}
        else q_mod_cur_bank[1] <= q_mod_cur_bank[1] + 1;

        egr_ppe_stm_if1.ren[0] <= 8'hff;
        egr_ppe_stm_if1.raddr[0][17:14] <= q_mod_cur_bank[2];
        egr_ppe_stm_if1.raddr[0][13:0]  <= q_mod_raddr[2];
        if(q_mod_cur_bank[2] == 9) begin //{
            q_mod_raddr[2] <= q_mod_raddr[2] + 1;
            q_mod_cur_bank[2] <= 4'b0;
        end //}
        else q_mod_cur_bank[2] <= q_mod_cur_bank[2] + 1;

        egr_ppe_stm_if1.ren[1] <= 8'hff;
        egr_ppe_stm_if1.raddr[1][17:14] <= q_mod_cur_bank[3];
        egr_ppe_stm_if1.raddr[1][13:0]  <= q_mod_raddr[3];
        if(q_mod_cur_bank[3] == 9) begin //{
            q_mod_raddr[3] <= q_mod_raddr[3] + 1;
            q_mod_cur_bank[3] <= 4'b0;
        end //}
        else q_mod_cur_bank[3] <= q_mod_cur_bank[3] + 1;
    end //}

    q_mod_0_rvalid  <= egr_ppe_stm_if0.rvalid;
    q_mod_0_rdata   <= egr_ppe_stm_if0.rdata;

    q_mod_1_rvalid  <= egr_ppe_stm_if1.rvalid;
    q_mod_1_rdata   <= egr_ppe_stm_if1.rdata;

    if(cclk_cnt > 1008) begin //{
        if(q_mod_0_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 0 port 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_0_rdata[0] !== q_mod_exp_rdata[0]) begin //{
            $display("ERROR, Mod Table Interface 0 port 0 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_0_rdata[0], q_mod_exp_rdata[0]);
            $finish;
        end //}
        if(q_mod_exp_bank[0] == 9) begin //{
            q_mod_exp_rdata[0] <= q_mod_exp_rdata[0] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[0] = 4'b0;
        end //}
        else q_mod_exp_bank[0] <= q_mod_exp_bank[0] + 1;

        if(q_mod_0_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 0 port 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_0_rdata[1] !== q_mod_exp_rdata[1]) begin //{
            $display("ERROR, Mod Table Interface 0 port 1 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_0_rdata[1], q_mod_exp_rdata[1]);
            $finish;
        end //}
        if(q_mod_exp_bank[1] == 9) begin //{
            q_mod_exp_rdata[1] <= q_mod_exp_rdata[1] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[1] = 4'b0;
        end //}
        else q_mod_exp_bank[1] <= q_mod_exp_bank[1] + 1;

        if(q_mod_1_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 1 port 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_1_rdata[0] !== q_mod_exp_rdata[2]) begin //{
            $display("ERROR, Mod Table Interface 1 port 0 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_1_rdata[0], q_mod_exp_rdata[2]);
            $finish;
        end //}
        if(q_mod_exp_bank[2] == 9) begin //{
            q_mod_exp_rdata[2] <= q_mod_exp_rdata[2] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[2] = 4'b0;
        end //}
        else q_mod_exp_bank[2] <= q_mod_exp_bank[2] + 1;

        if(q_mod_1_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 1 port 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_1_rdata[1] !== q_mod_exp_rdata[3]) begin //{
            $display("ERROR, Mod Table Interface 1 port 1 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_1_rdata[1], q_mod_exp_rdata[3]);
            $finish;
        end //}
        if(q_mod_exp_bank[3] == 9) begin //{
            q_mod_exp_rdata[3] <= q_mod_exp_rdata[3] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[3] = 4'b0;
        end //}
        else q_mod_exp_bank[3] <= q_mod_exp_bank[3] + 1;
    end //}
end //}

endmodule
