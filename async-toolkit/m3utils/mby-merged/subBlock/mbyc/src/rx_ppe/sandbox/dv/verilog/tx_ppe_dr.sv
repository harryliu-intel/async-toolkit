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
// -- Description : TX PPE Driver for ppe_stm testbed
// --
// -------------------------------------------------------------------
`timescale 1ps/1ps

module tx_ppe_dr
(
input   logic       cclk,
input   int         cclk_cnt,

tx_ppe_mod_if.ppe   tx_ppe_mod_if0,
tx_ppe_mod_if.ppe   tx_ppe_mod_if1,
tx_ppe_mod_if.ppe   tx_ppe_mod_if2,
tx_ppe_mod_if.ppe   tx_ppe_mod_if3,

tx_ppe_negh_if.ppe  tx_ppe_negh_if0,
tx_ppe_negh_if.ppe  tx_ppe_negh_if1,
tx_ppe_negh_if.ppe  tx_ppe_negh_if2,
tx_ppe_negh_if.ppe  tx_ppe_negh_if3
);

logic   [3:0]           q_mod_rvalid;
logic   [3:0] [575:0]   q_mod_rdata;
logic   [3:0] [13:0]    q_mod_raddr;
logic   [3:0] [3:0]     q_mod_cur_bank;
logic   [3:0] [575:0]   q_mod_exp_rdata;
logic   [3:0] [3:0]     q_mod_exp_bank;

always_ff @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        tx_ppe_mod_if0.ren      <= 8'b0;
        tx_ppe_mod_if1.ren      <= 8'b0;
        tx_ppe_mod_if2.ren      <= 8'b0;
        tx_ppe_mod_if3.ren      <= 8'b0;
        tx_ppe_mod_if0.raddr    <= 18'h0;
        tx_ppe_mod_if1.raddr    <= 18'h0;
        tx_ppe_mod_if2.raddr    <= 18'h0;
        tx_ppe_mod_if3.raddr    <= 18'h0;
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
        tx_ppe_mod_if0.ren          <= 8'hff;
        tx_ppe_mod_if0.raddr[17:14] <= q_mod_cur_bank[0];
        tx_ppe_mod_if0.raddr[13:0]  <= q_mod_raddr[0];
        if(q_mod_cur_bank[0] == 9) begin //{
            q_mod_raddr[0] <= q_mod_raddr[0] + 1;
            q_mod_cur_bank[0] <= 4'b0;
        end //}
        else q_mod_cur_bank[0] <= q_mod_cur_bank[0] + 1;

        tx_ppe_mod_if1.ren          <= 8'hff;
        tx_ppe_mod_if1.raddr[17:14] <= q_mod_cur_bank[1];
        tx_ppe_mod_if1.raddr[13:0]  <= q_mod_raddr[1];
        if(q_mod_cur_bank[1] == 9) begin //{
            q_mod_raddr[1] <= q_mod_raddr[1] + 1;
            q_mod_cur_bank[1] <= 4'b0;
        end //}
        else q_mod_cur_bank[1] <= q_mod_cur_bank[1] + 1;

        tx_ppe_mod_if2.ren          <= 8'hff;
        tx_ppe_mod_if2.raddr[17:14] <= q_mod_cur_bank[2];
        tx_ppe_mod_if2.raddr[13:0]  <= q_mod_raddr[2];
        if(q_mod_cur_bank[2] == 9) begin //{
            q_mod_raddr[2] <= q_mod_raddr[2] + 1;
            q_mod_cur_bank[2] <= 4'b0;
        end //}
        else q_mod_cur_bank[2] <= q_mod_cur_bank[2] + 1;

        tx_ppe_mod_if3.ren          <= 8'hff;
        tx_ppe_mod_if3.raddr[17:14] <= q_mod_cur_bank[3];
        tx_ppe_mod_if3.raddr[13:0]  <= q_mod_raddr[3];
        if(q_mod_cur_bank[3] == 9) begin //{
            q_mod_raddr[3] <= q_mod_raddr[3] + 1;
            q_mod_cur_bank[3] <= 4'b0;
        end //}
        else q_mod_cur_bank[3] <= q_mod_cur_bank[3] + 1;
    end //}

    q_mod_rvalid[0] <= tx_ppe_mod_if0.rvalid;
    q_mod_rdata[0]  <= tx_ppe_mod_if0.rdata;
    q_mod_rvalid[1] <= tx_ppe_mod_if1.rvalid;
    q_mod_rdata[1]  <= tx_ppe_mod_if1.rdata;
    q_mod_rvalid[2] <= tx_ppe_mod_if2.rvalid;
    q_mod_rdata[2]  <= tx_ppe_mod_if2.rdata;
    q_mod_rvalid[3] <= tx_ppe_mod_if3.rvalid;
    q_mod_rdata[3]  <= tx_ppe_mod_if3.rdata;

    if(cclk_cnt > 1008) begin //{
        if(q_mod_rvalid[0] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 0 port 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_rdata[0] !== q_mod_exp_rdata[0]) begin //{
            $display("ERROR, Mod Table Interface 0 port 0 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_rdata[0], q_mod_exp_rdata[0]);
            $finish;
        end //}
        if(q_mod_exp_bank[0] == 9) begin //{
            q_mod_exp_rdata[0] <= q_mod_exp_rdata[0] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[0] = 4'b0;
        end //}
        else q_mod_exp_bank[0] <= q_mod_exp_bank[0] + 1;

        if(q_mod_rvalid[1] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 0 port 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_rdata[1] !== q_mod_exp_rdata[1]) begin //{
            $display("ERROR, Mod Table Interface 0 port 1 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_rdata[1], q_mod_exp_rdata[1]);
            $finish;
        end //}
        if(q_mod_exp_bank[1] == 9) begin //{
            q_mod_exp_rdata[1] <= q_mod_exp_rdata[1] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[1] = 4'b0;
        end //}
        else q_mod_exp_bank[1] <= q_mod_exp_bank[1] + 1;

        if(q_mod_rvalid[2] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 1 port 0 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_rdata[2] !== q_mod_exp_rdata[2]) begin //{
            $display("ERROR, Mod Table Interface 1 port 0 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_rdata[2], q_mod_exp_rdata[2]);
            $finish;
        end //}
        if(q_mod_exp_bank[2] == 9) begin //{
            q_mod_exp_rdata[2] <= q_mod_exp_rdata[2] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[2] = 4'b0;
        end //}
        else q_mod_exp_bank[2] <= q_mod_exp_bank[2] + 1;

        if(q_mod_rvalid[3] !== 1'b1) begin //{
            $display("ERROR, Mod Table Inteface 1 port 1 read is not valid when it was expected to be\n");
            $finish;
        end //}
        if(q_mod_rdata[3] !== q_mod_exp_rdata[3]) begin //{
            $display("ERROR, Mod Table Interface 1 port 1 read data miscompare\n actual   = %h\n expected = %h\n", q_mod_rdata[3], q_mod_exp_rdata[3]);
            $finish;
        end //}
        if(q_mod_exp_bank[3] == 9) begin //{
            q_mod_exp_rdata[3] <= q_mod_exp_rdata[3] + 576'h1000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001000000000000000001;
            q_mod_exp_bank[3] = 4'b0;
        end //}
        else q_mod_exp_bank[3] <= q_mod_exp_bank[3] + 1;
    end //}
end //}

logic   [3:0]           q_negh_rvalid;
logic   [3:0] [81:0]    q_negh_rdata;
logic   [1:0] [13:0]    q_negh_raddr;
logic   [1:0] [81:0]    q_negh_exp_rdata;

always_ff @(posedge cclk) begin //{
    if(cclk_cnt == 1) begin //{
        tx_ppe_negh_if0.ren     <= 1'b0;
        tx_ppe_negh_if1.ren     <= 1'b0;
        tx_ppe_negh_if2.ren     <= 1'b0;
        tx_ppe_negh_if3.ren     <= 1'b0;
        tx_ppe_negh_if0.raddr   <= 14'h0;
        tx_ppe_negh_if1.raddr   <= 14'h0;
        tx_ppe_negh_if2.raddr   <= 14'h0;
        tx_ppe_negh_if3.raddr   <= 14'h0;
        q_negh_raddr[0]         <= 14'b0;
        q_negh_raddr[1]         <= 14'b0;
        q_negh_exp_rdata[0]     <= 82'b0;
        q_negh_exp_rdata[1]     <= 82'h1;
    end //}
    else if(cclk_cnt > 1000) begin //{
        if((cclk_cnt % 2) == 1) begin //{
            tx_ppe_negh_if0.ren     <= 1'b0;
            tx_ppe_negh_if1.ren     <= 1'b1;
            tx_ppe_negh_if1.raddr   <= q_negh_raddr[1];
            tx_ppe_negh_if2.ren     <= 1'b0;
            tx_ppe_negh_if3.ren     <= 1'b1;
            tx_ppe_negh_if3.raddr   <= q_negh_raddr[1];
            q_negh_raddr[1]         <= q_negh_raddr[1] + 1;
        end //}
        else begin //{
            tx_ppe_negh_if0.ren     <= 1'b1;
            tx_ppe_negh_if0.raddr   <= q_negh_raddr[0];
            tx_ppe_negh_if1.ren     <= 1'b0;
            tx_ppe_negh_if2.ren     <= 1'b1;
            tx_ppe_negh_if2.raddr   <= q_negh_raddr[0];
            tx_ppe_negh_if3.ren     <= 1'b0;
            q_negh_raddr[0]         <= q_negh_raddr[0] + 1;
        end //}
    end //}

    q_negh_rvalid[0]    <= tx_ppe_negh_if0.rvalid;
    q_negh_rdata[0]     <= tx_ppe_negh_if0.rdata;
    q_negh_rvalid[1]    <= tx_ppe_negh_if1.rvalid;
    q_negh_rdata[1]     <= tx_ppe_negh_if1.rdata;
    q_negh_rvalid[2]    <= tx_ppe_negh_if2.rvalid;
    q_negh_rdata[2]     <= tx_ppe_negh_if2.rdata;
    q_negh_rvalid[3]    <= tx_ppe_negh_if3.rvalid;
    q_negh_rdata[3]     <= tx_ppe_negh_if3.rdata;

    if(cclk_cnt > 1008) begin //{
        if((cclk_cnt % 2) == 1) begin //{
            if(q_negh_rvalid[0] !== 1'b0) begin //{
                $display("ERROR, Neighbor Table Inteface 0 read is valid when it wasn't expected to be\n");
                $finish;
            end //}
            if(q_negh_rvalid[1] !== 1'b1) begin //{
                $display("ERROR, Neighbor Table Inteface 1 read is not valid when it was expected to be\n");
                $finish;
            end //}
            if(q_negh_rdata[1] !== q_negh_exp_rdata[0]) begin //{
                $display("ERROR, Neighbor Table Interface 1 read data miscompare\n actual   = %h\n expected = %h\n", q_negh_rdata[1], q_negh_exp_rdata[0]);
                $finish;
            end //}
            if(q_negh_rvalid[2] !== 1'b0) begin //{
                $display("ERROR, Neighbor Table Inteface 2 read is valid when it wasn't expected to be\n");
                $finish;
            end //}
            if(q_negh_rvalid[3] !== 1'b1) begin //{
                $display("ERROR, Neighbor Table Inteface 3 read is not valid when it was expected to be\n");
                $finish;
            end //}
            if(q_negh_rdata[3] !== q_negh_exp_rdata[1]) begin //{
                $display("ERROR, Neighbor Table Interface 3 read data miscompare\n actual   = %h\n expected = %h\n", q_negh_rdata[3], q_negh_exp_rdata[1]);
                $finish;
            end //}
        end //}
        else begin //{
            if(q_negh_rvalid[0] !== 1'b1) begin //{
                $display("ERROR, Neighbor Table Inteface 0 read is not valid when it was expected to be\n");
                $finish;
            end //}
            if(q_negh_rdata[0] !== q_negh_exp_rdata[0]) begin //{
                $display("ERROR, Neighbor Table Interface 0 read data miscompare\n actual   = %h\n expected = %h\n", q_negh_rdata[0], q_negh_exp_rdata[0]);
                $finish;
            end //}
            if(q_negh_rvalid[1] !== 1'b0) begin //{
                $display("ERROR, Neighbor Table Inteface 1 read is valid when it wasn't expected to be\n");
                $finish;
            end //}
            if(q_negh_rvalid[2] !== 1'b1) begin //{
                $display("ERROR, Neighbor Table Inteface 2 read is not valid when it was expected to be\n");
                $finish;
            end //}
            if(q_negh_rdata[2] !== q_negh_exp_rdata[1]) begin //{
                $display("ERROR, Neighbor Table Interface 2 read data miscompare\n actual   = %h\n expected = %h\n", q_negh_rdata[2], q_negh_exp_rdata[1]);
                $finish;
            end //}
            if(q_negh_rvalid[3] !== 1'b0) begin //{
                $display("ERROR, Neighbor Table Inteface 3 read is valid when it wasn't expected to be\n");
                $finish;
            end //}
            q_negh_exp_rdata[0] <= q_negh_exp_rdata[0] + 1;
            q_negh_exp_rdata[1] <= q_negh_exp_rdata[1] + 1;
        end //}
    end //}
end //}

endmodule
