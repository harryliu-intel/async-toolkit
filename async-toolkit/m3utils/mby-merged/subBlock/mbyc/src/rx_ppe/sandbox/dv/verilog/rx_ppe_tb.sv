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
// -- Description : RX PPE top level verification file
// --
// -------------------------------------------------------------------
`timescale 1ps/1ps

module rx_ppe_tb
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

//delay assertion until reset propagates
initial $assertoff();

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

igr_rx_ppe_if       igr_rx_ppe_if();
rx_ppe_igr_if       rx_ppe_igr_if();
rx_ppe_ppe_stm0_if  rx_ppe_ppe_stm0_if();
rx_ppe_ppe_stm1_if  rx_ppe_ppe_stm1_if();

//Reset
always @(posedge cclk) begin //{
    if(cclk_cnt == 5) reset <= 1'b0;
    else if(cclk_cnt == 10) reset <= 1'b1;
    else if(cclk_cnt == 15) begin //{
        reset <= 1'b0;
        $asserton();
    end //}

//Temporary forces for TCAM errors
    force rx_ppe.parser_top.reset_n = 1'b0;
    force rx_ppe.class_gpa_top.reset_n = 1'b0;
    force rx_ppe.action_top.reset_n = 1'b0;
end //}

always @(posedge cclk) begin //{
    if(cclk_cnt > 15) begin //{
        if(rx_ppe.parser_top.parser_func_logic.q_input_fifo_rd_en_sg0[0]) begin //{
            if((|rx_ppe.parser_top.parser_func_logic.input_data_fifo_sbe[0]) || (|rx_ppe.parser_top.parser_func_logic.input_data_fifo_mbe[0])) begin //{
                $display("ERROR, Read data FIFO0 SECDED error\n");
                $finish;
            end //}
            if(rx_ppe.parser_top.parser_func_logic.input_md_fifo_sbe[0] || rx_ppe.parser_top.parser_func_logic.input_md_fifo_mbe[0]) begin //{
                $display("ERROR, Read MD FIFO0 SECDED error\n");
                $finish;
            end //}
        end //}
        if(rx_ppe.parser_top.parser_func_logic.q_input_fifo_rd_en_sg0[1]) begin //{
            if((|rx_ppe.parser_top.parser_func_logic.input_data_fifo_sbe[1]) || (|rx_ppe.parser_top.parser_func_logic.input_data_fifo_mbe[1])) begin //{
                $display("ERROR, Read data FIFO1 SECDED error\n");
                $finish;
            end //}
            if(rx_ppe.parser_top.parser_func_logic.input_md_fifo_sbe[1] || rx_ppe.parser_top.parser_func_logic.input_md_fifo_mbe[1]) begin //{
                $display("ERROR, Read MD FIFO1 SECDED error\n");
                $finish;
            end //}
        end //}
    end //}
end //}

igr_dr igr_dr (
    .cclk           (cclk),
    .cclk_cnt       (cclk_cnt),
    .igr_rx_ppe_if  (igr_rx_ppe_if),
    .rx_ppe_igr_if  (rx_ppe_igr_if)
);

rx_ppe  rx_ppe (
    .cclk               (cclk),
    .reset              (reset),
    .i_ibus_ctrl        (99'b0),
    .o_ibus_resp        (),
    .igr_rx_ppe_if      (igr_rx_ppe_if),
    .rx_ppe_igr_if      (rx_ppe_igr_if),
    .rx_ppe_ppe_stm0_if (rx_ppe_ppe_stm0_if),
    .rx_ppe_ppe_stm1_if (rx_ppe_ppe_stm1_if)
);

endmodule
