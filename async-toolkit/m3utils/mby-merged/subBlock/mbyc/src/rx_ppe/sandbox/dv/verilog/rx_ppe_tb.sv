// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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

//Set configuration CSRs
generate //{
    for(genvar g_i=0; g_i < 17; g_i++) begin //{
        always @(negedge cclk) begin //{
            if(cclk_cnt == 25) begin //{
                force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.u_pa_port_cfg.c_port_cfg_q[g_i] = {$urandom,$urandom} & 64'h0f0f0f0fffff0000;
            end //}
        end //}
    end //}
endgenerate //}

generate //{
    for(genvar g_i=0; g_i < 32; g_i++) begin //{
        for(genvar g_j=0; g_j < 16; g_j++) begin //{
            always @(negedge cclk) begin //{
                if(cclk_cnt == 25) begin //{
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_ana_w[g_j] = $urandom & 32'h0f0f0f0f;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_ana_s[g_j] = {$urandom,$urandom} & 64'h0000ffffffff0000;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_w[g_j].w0_mask = $urandom & 16'h003f;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_w[g_j].w1_mask = $urandom & 16'h0fc0;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_s[g_j].state_mask = $urandom & 16'hf000;
                end //}
                if(cclk_cnt == 26) begin //{
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_w[g_j].w0_value = $urandom & rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_w[g_j].w0_mask;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_w[g_j].w1_value = $urandom & rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_w[g_j].w1_mask;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_s[g_j].state_value = $urandom & rx_ppe.parser_top.parser_func_logic.hlp_pa_core.ana_stage[g_i].u_pa_ana.c_pa_key_s[g_j].state_mask;
                end //}
            end //}
        end //}
    end //}
endgenerate //}

generate //{
    for(genvar g_i=0; g_i < 32; g_i++) begin //{
        for(genvar g_j=0; g_j < 16; g_j++) begin //{
            always @(negedge cclk) begin //{
                if(cclk_cnt == 25) begin //{
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.u_pa_xal.stages[g_i].u_pa_xal_stage.c_pa_exc[g_j] = {($urandom & 8'h07),(($urandom & 32'hff) == 8'h0)};
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.u_pa_xal.stages[g_i].u_pa_xal_stage.c_pa_ext[g_j] = $urandom & 26'h3fc0fff;
                    force rx_ppe.parser_top.parser_func_logic.hlp_pa_core.u_pa_xal.stages[g_i].u_pa_xal_stage.c_pa_ext[g_j+16] = $urandom & 26'h3fc0fff;
                end //}
            end //}
        end //}
    end //}
endgenerate //}

generate //{
    for(genvar g_i=0; g_i < 64; g_i++) begin //{
        always @(negedge cclk) begin //{
            if(cclk_cnt == 25) begin //{
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_ptype_ram_shell_64x14.behave_mem.sram[g_i] = $urandom;
                force rx_ppe.parser_top.parser_func_logic.parser_extract.q_ptype_tcam[g_i][31:0] = $urandom & $urandom & $urandom;
            end //}
            if(cclk_cnt == 26) begin //{
                force rx_ppe.parser_top.parser_func_logic.parser_extract.q_ptype_tcam[g_i][63:32] = $urandom & $urandom & $urandom & ~rx_ppe.parser_top.parser_func_logic.parser_extract.q_ptype_tcam[g_i][31:0];
            end //}
        end //}
    end //}
endgenerate //}

generate //{
    for(genvar g_i=0; g_i < 16; g_i++) begin //{
        always @(negedge cclk) begin //{
            if(cclk_cnt == 25) begin //{
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_0.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_1.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_2.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_3.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_4.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_5.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_6.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_7.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_8.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_9.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_10.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_11.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_12.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_13.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_14.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_15.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_16.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_17.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_18.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_19.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_20.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_21.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_22.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_23.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_24.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_25.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_26.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_27.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_28.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_29.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_30.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_31.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_32.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_33.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_34.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_35.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_36.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_37.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_38.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_39.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_40.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_41.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_42.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_43.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_44.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_45.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_46.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_47.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_48.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_49.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_50.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_51.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_52.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_53.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_54.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_55.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_56.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_57.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_58.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_59.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_60.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_61.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_62.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_63.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_64.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_65.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_66.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_67.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_68.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_69.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_70.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_71.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_72.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_73.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_74.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_75.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_76.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_77.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_78.behave_mem.sram[g_i] = $urandom & 16'h3ff;
                force rx_ppe.parser_top.parser_ff_mems.parser_wrap_mem_parser_extract_cfg_shell_16x16_79.behave_mem.sram[g_i] = $urandom & 16'h3ff;
            end //}
        end //}
    end //}
endgenerate //}

//Check input FIFO SECDED
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
