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

module igr_dr
(
input   logic       cclk,
input   int         cclk_cnt,
igr_rx_ppe_if.igr   igr_rx_ppe_if,
rx_ppe_igr_if.igr   rx_ppe_igr_if
);

logic   [31:0]  q_random;
logic   [8:0]   q_pkt_id0;
logic   [8:0]   q_pkt_id1;
logic   [84:0]  q_val_dly0;
logic   [84:0]  q_val_dly1;
logic   [8:0]   q_exp_pkt_id0;
logic   [8:0]   q_exp_pkt_id1;

generate //{
    for(genvar g_i=0; g_i<8; g_i++) begin:gen_data_ecc //{
        shrtl_ecc_gen #(
            .DATA(120),
            .CHK(8)
        ) gen_data_ecc_0 (
            .i_data (igr_rx_ppe_if.intf0_head.data[(g_i*120):((g_i*120)+119)]),
            .o_chk  (igr_rx_ppe_if.intf0_head.ecc[(g_i*8):((g_i*8)+7)])
        );

        shrtl_ecc_gen #(
            .DATA(120),
            .CHK(8)
        ) gen_data_ecc_1 (
            .i_data (igr_rx_ppe_if.intf1_head.data[(g_i*120):((g_i*120)+119)]),
            .o_chk  (igr_rx_ppe_if.intf1_head.ecc[(g_i*8):((g_i*8)+7)])
        );
    end //}
endgenerate //}

shrtl_ecc_gen #(
    .DATA(64),
    .CHK(8)
) gen_data_ecc_0_8 (
    .i_data (igr_rx_ppe_if.intf0_head.data[960:1023]),
    .o_chk  (igr_rx_ppe_if.intf0_head.ecc[64:71])
);

shrtl_ecc_gen #(
    .DATA(64),
    .CHK(8)
) gen_data_ecc_1_8 (
    .i_data (igr_rx_ppe_if.intf1_head.data[960:1023]),
    .o_chk  (igr_rx_ppe_if.intf1_head.ecc[64:71])
);

always_ff @(posedge cclk) begin //{
    q_random <= $urandom;

    if(cclk_cnt == 1) begin //{
        igr_rx_ppe_if.intf0_head.valid          <= 1'b0;
        igr_rx_ppe_if.intf0_head.md.cpp_md      <= 23'b0;
        igr_rx_ppe_if.intf0_head.md.ts          <= 64'b0;
        igr_rx_ppe_if.intf0_head.md.tc          <= 4'b0;
        igr_rx_ppe_if.intf0_head.md.port        <= 5'b0;
        igr_rx_ppe_if.intf0_head.md.eop         <= 1'b0;
        igr_rx_ppe_if.intf0_head.md.len         <= 8'b0;
        igr_rx_ppe_if.intf0_head.md.next_len    <= 2'b0;
        igr_rx_ppe_if.intf1_head.valid          <= 1'b0;
        igr_rx_ppe_if.intf1_head.md.cpp_md      <= 23'b0;
        igr_rx_ppe_if.intf1_head.md.ts          <= 64'b0;
        igr_rx_ppe_if.intf1_head.md.tc          <= 4'b0;
        igr_rx_ppe_if.intf1_head.md.port        <= 5'b0;
        igr_rx_ppe_if.intf1_head.md.eop         <= 1'b0;
        igr_rx_ppe_if.intf1_head.md.len         <= 8'b0;
        igr_rx_ppe_if.intf1_head.md.next_len    <= 2'b0;
        q_pkt_id0 <= 0;
        q_pkt_id1 <= 0;
        q_exp_pkt_id0 <= 0;
        q_exp_pkt_id1 <= 0;
        q_val_dly0 <= 0;
        q_val_dly1 <= 0;
    end //}
    else if(cclk_cnt > 25) begin //{
        igr_rx_ppe_if.intf0_head.valid  <= q_random[0];
        if(q_random[0]) begin //{
            igr_rx_ppe_if.intf0_head.data   <= {$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,
                                                $urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,
                                                $urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,
                                                $urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom};
        end //}
        igr_rx_ppe_if.intf0_head.md.id  <= q_pkt_id0;
        if(q_random[0]) q_pkt_id0 <= q_pkt_id0 + 1;

        igr_rx_ppe_if.intf1_head.valid  <= q_random[0] | q_random[1];
        if(q_random[0] || q_random[1]) begin //{
            igr_rx_ppe_if.intf1_head.data   <= {$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,
                                                $urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,
                                                $urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,
                                                $urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom,$urandom};
        end //}
        igr_rx_ppe_if.intf1_head.md.id  <= q_pkt_id1;
        if(q_random[0] || q_random[1]) q_pkt_id1 <= q_pkt_id1 + 1;

        q_val_dly0 <= {q_val_dly0[83:0],q_random[0]};
        q_val_dly1 <= {q_val_dly1[83:0],(q_random[0] | q_random[1])};
    end //}

    if(cclk_cnt > 25) begin //{
        if(q_val_dly0[43]) begin //{
            if(igr_rx_ppe_if.intf0_ack !== 1'b1) begin //{
                $display("ERROR, Interface 0 acknowledge is cleared when it should be set\n");
                $finish;
            end //}
        end //}
        else begin //{
            if(igr_rx_ppe_if.intf0_ack !== 1'b0) begin //{
                $display("ERROR, Interface 0 acknowledge is set when it should be cleared\n");
                $finish;
            end //}
        end //}

        if(q_val_dly1[43]) begin //{
            if(igr_rx_ppe_if.intf1_ack !== 1'b1) begin //{
                $display("ERROR, Interface 1 acknowledge is cleared when it should be set\n");
                $finish;
            end //}
        end //}
        else begin //{
            if(igr_rx_ppe_if.intf1_ack !== 1'b0) begin //{
                $display("ERROR, Interface 1 acknowledge is set when it should be cleared\n");
                $finish;
            end //}
        end //}

        if(q_val_dly0[84]) begin //{
            if(rx_ppe_igr_if.intf0.valid !== 1'b1) begin //{
                $display("ERROR, Interface 0 valid is cleared when it should be set\n");
                $finish;
            end //}
            if(rx_ppe_igr_if.intf0.id !== q_exp_pkt_id0) begin //{
                $display("ERROR, Interface 0 packet ID is incorrect\n actual   = %h\nexpected = %h\n", rx_ppe_igr_if.intf0.id, q_exp_pkt_id0);
                $finish;
            end //}
            q_exp_pkt_id0 <= q_exp_pkt_id0 + 1;
        end //}
        else begin //{
            if(rx_ppe_igr_if.intf0.valid !== 1'b0) begin //{
                $display("ERROR, Interface 0 valid is set when it should be cleared\n");
                $finish;
            end //}
        end //}

        if(q_val_dly1[84]) begin //{
            if(rx_ppe_igr_if.intf1.valid !== 1'b1) begin //{
                $display("ERROR, Interface 1 valid is cleared when it should be set\n");
                $finish;
            end //}
            if(rx_ppe_igr_if.intf1.id !== q_exp_pkt_id1) begin //{
                $display("ERROR, Interface 1 packet ID is incorrect\n, actual = %h\nexpected = %h\n", rx_ppe_igr_if.intf1.id, q_exp_pkt_id1);
                $finish;
            end //}
            q_exp_pkt_id1 <= q_exp_pkt_id1 + 1;
        end //}
        else begin //{
            if(rx_ppe_igr_if.intf1.valid !== 1'b0) begin //{
                $display("ERROR, Interface 1 valid is set when it should be cleared\n");
                $finish;
            end //}
        end //}
    end //}
end //}

endmodule
