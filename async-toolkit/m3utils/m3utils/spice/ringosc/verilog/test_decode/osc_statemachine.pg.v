// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// Fusion Compiler Version U-2022.12-SP5 Verilog Writer
// Generated on 8/22/2024 at 14:52:21
// Library Name: osc_statemachine.ndm
// Block Name: osc_statemachine
// User Label: 
// Write Command: write_verilog -compress gzip -exclude { empty_modules scalar_wire_declarations leaf_module_declarations unconnected_ports all_physical_cells } outputs/compile_final_opto/osc_statemachine.pg.v
module osc_statemachine ( clk , rst_n , i_up_down , i_rstval , o_stage_en , 
    o_interp_ctrl , VSS , VDD ) ;
input  clk ;
input  rst_n ;
input  i_up_down ;
input  [7:0] i_rstval ;
output [5:0] o_stage_en ;
output [31:0] o_interp_ctrl ;
input  VSS ;
input  VDD ;

supply0 VSS ;
supply1 VDD ;

i0saoi012aa1n02x5 ctmi_252 ( .b ( ctmn_236 ) , .c ( ctmn_247 ) , 
    .a ( ctmn_255 ) , .o1 ( nxt_7 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_6 ( .d ( nxt_6 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_89 ) , .s ( ASYNC_NET_12 ) , 
    .o ( cur_6 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d02x2 u_osc_statemachine_regs_o_cur_reg_5 ( .d ( nxt_5 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_90 ) , .s ( ASYNC_NET_19 ) , 
    .o ( cur_5 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_4 ( .d ( nxt_4 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_91 ) , .s ( ASYNC_NET_26 ) , 
    .o ( cur_4 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_291 ( .a ( rst_n ) , .b ( i_rstval[0] ) , 
    .o ( SEQMAP_NET_95 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_3 ( .d ( nxt_3 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_92 ) , .s ( ASYNC_NET_33 ) , 
    .o ( cur_3 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_2 ( .d ( nxt_2 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_93 ) , .s ( ASYNC_NET_40 ) , 
    .o ( cur_2 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_1 ( .d ( nxt_1 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_94 ) , .s ( ASYNC_NET_47 ) , 
    .o ( cur_1 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_0 ( .d ( nxt_0 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_95 ) , .s ( ASYNC_NET_54 ) , 
    .o ( cur_0 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soaoi15aa1n02x5 ctmi_280 ( .d ( ctmn_237 ) , .e ( N56 ) , .c ( ctmn_245 ) , 
    .b ( ctmn_241 ) , .a ( ctmn_262 ) , .o1 ( nxt_4 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0soaih22aa1n01x1 ctmi_277 ( .a ( phfnn_76 ) , .b ( ctmn_259 ) , 
    .c ( ctmn_241 ) , .d ( ctmn_260 ) , .o1 ( nxt_5 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 phfnr_buf_133 ( .a ( i_up_down ) , .o1 ( phfnn_76 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 ctmi_273 ( .b ( ctmn_256 ) , .c ( ctmn_258 ) , 
    .a ( phfnn_80 ) , .o1 ( nxt_6 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0smdn022aa1n01x1 ctmi_305 ( .b ( ctmn_241 ) , .a ( ctmn_258 ) , .sa ( N57 ) , 
    .o1 ( nxt_1 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saoi012aa1n02x5 ctmi_283 ( .b ( ctmn_241 ) , .c ( ctmn_258 ) , 
    .a ( cur_0 ) , .o1 ( nxt_0 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_284 ( .a ( rst_n ) , .b ( i_rstval[7] ) , 
    .o ( SEQMAP_NET_88 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai112aa1n02x5 ctmi_281 ( .c ( cur_4 ) , .d ( ctmn_250 ) , 
    .a ( phfnn_79 ) , .b ( ctmn_251 ) , .o1 ( ctmn_262 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0saoi012aa1n02x5 ctmi_279 ( .b ( ctmn_245 ) , .c ( cur_5 ) , 
    .a ( ctmn_246 ) , .o1 ( ctmn_260 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sxnrna2aa1n02x5 ctmi_274 ( .a ( cur_6 ) , .c ( cur_5 ) , .b ( phfnn_78 ) , 
    .out0 ( ctmn_256 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 ctmi_275 ( .b ( ctmn_257 ) , .c ( ctmn_245 ) , 
    .a ( i_up_down ) , .o1 ( ctmn_258 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand23aa1n01x1 ctmi_276 ( .a ( o_stage_en[5] ) , .b ( cur_5 ) , 
    .c ( ctmn_236 ) , .o1 ( ctmn_257 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 phfnr_buf_136 ( .a ( ctmn_258 ) , .o1 ( phfnn_79 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_285 ( .a ( rst_n ) , .b ( i_rstval[6] ) , 
    .o ( SEQMAP_NET_89 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_286 ( .a ( rst_n ) , .b ( i_rstval[5] ) , 
    .o ( SEQMAP_NET_90 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_287 ( .a ( rst_n ) , .b ( i_rstval[4] ) , 
    .o ( SEQMAP_NET_91 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_288 ( .a ( rst_n ) , .b ( i_rstval[3] ) , 
    .o ( SEQMAP_NET_92 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_289 ( .a ( rst_n ) , .b ( i_rstval[2] ) , 
    .o ( SEQMAP_NET_93 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 ctmi_290 ( .a ( rst_n ) , .b ( i_rstval[1] ) , 
    .o ( SEQMAP_NET_94 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sfvn00baa1d01x2 u_osc_statemachine_regs_o_cur_reg_7 ( .d ( nxt_7 ) , 
    .clk ( clk ) , .rb ( SEQMAP_NET_88 ) , .s ( ASYNC_NET_5 ) , 
    .o ( o_stage_en[5] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_292 ( .a ( i_rstval[6] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_12 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_293 ( .a ( i_rstval[5] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_19 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_294 ( .a ( i_rstval[4] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_26 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C645_phfnr_buf_145 ( .a ( cur_0 ) , .o1 ( C645_phfnn_88 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_295 ( .a ( i_rstval[3] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_33 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_296 ( .a ( i_rstval[2] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_40 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_297 ( .a ( i_rstval[1] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_47 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_298 ( .a ( i_rstval[0] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_54 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 phfnr_buf_134 ( .a ( ctmn_242 ) , .o1 ( phfnn_77 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 phfnr_buf_135 ( .a ( ctmn_251 ) , .o1 ( phfnn_78 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0smdn022aa1n01x1 ctmi_301 ( .b ( phfnn_77 ) , .a ( ctmn_249 ) , 
    .sa ( ctmn_241 ) , .o1 ( ctmn_264 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sxnr002aa1n02x5 ctmi_302 ( .a ( cur_3 ) , .b ( ctmn_266 ) , 
    .out0 ( nxt_3 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saoi023aa1n02x5 ctmi_303 ( .c ( ctmn_258 ) , .d ( ctmn_242 ) , 
    .e ( nxt_2 ) , .a ( phfnn_79 ) , .b ( ctmn_265 ) , .o1 ( ctmn_266 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 phfnr_buf_137 ( .a ( ctmn_247 ) , .o1 ( phfnn_80 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snona32aa1n02x5 ctmi_83 ( .b ( cur_4 ) , .c ( o_stage_en[5] ) , 
    .d ( cur_1 ) , .a ( ctmn_238 ) , .out0 ( ctmn_239 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sand002aa1n02x5 A80 ( .a ( cur_6 ) , .b ( ctmn_256 ) , .o ( N58 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 ctmi_253 ( .a ( cur_6 ) , .o1 ( ctmn_236 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_251 ( .a ( i_rstval[7] ) , .b ( rst_n ) , 
    .out0 ( ASYNC_NET_5 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sxo2no2aa1n02x5 ctmi_254 ( .c ( ctmn_241 ) , .a ( ctmn_236 ) , 
    .b ( ctmn_246 ) , .out0 ( ctmn_247 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai013aa1n02x4 ctmi_255 ( .b ( ctmn_239 ) , .c ( cur_5 ) , .d ( cur_6 ) , 
    .a ( phfnn_76 ) , .o1 ( ctmn_241 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 C644_ctmi_128 ( .a ( cur_6 ) , .b ( o_stage_en[5] ) , 
    .out0 ( o_stage_en[3] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 ctmi_257 ( .a ( cur_4 ) , .o1 ( ctmn_237 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 ctmi_258 ( .a ( cur_2 ) , .b ( cur_3 ) , .o1 ( ctmn_238 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_129 ( .a ( cur_6 ) , .b ( o_stage_en[5] ) , 
    .o1 ( o_stage_en[1] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 ctmi_260 ( .a ( ctmn_245 ) , .b ( cur_5 ) , 
    .o1 ( ctmn_246 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sand002aa1n02x5 C644_ctmi_130 ( .a ( C644_phfnn_81 ) , 
    .b ( o_stage_en[1] ) , .o ( o_stage_en[0] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snanb02aa1n02x5 ctmi_84 ( .a ( cur_4 ) , .b ( N56 ) , .out0 ( ctmn_245 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 ctmi_263 ( .a ( cur_1 ) , .b ( cur_0 ) , .o1 ( ctmn_242 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_138 ( .a ( cur_5 ) , .o1 ( C644_phfnn_81 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sxnrna2aa1n02x5 ctmi_265 ( .a ( o_stage_en[5] ) , .c ( phfnn_80 ) , 
    .b ( N58 ) , .out0 ( ctmn_255 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_132 ( .a ( o_stage_en[4] ) , 
    .b ( o_stage_en[0] ) , .o1 ( o_stage_en[2] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sxor002aa1n02x5 ctmi_85 ( .a ( ctmn_251 ) , .b ( cur_5 ) , 
    .out0 ( ctmn_259 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_86 ( .a ( ctmn_249 ) , .b ( ctmn_242 ) , 
    .out0 ( N57 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 ctmi_269 ( .a ( cur_4 ) , .b ( ctmn_250 ) , 
    .o1 ( ctmn_251 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snano22aa1n02x5 ctmi_270 ( .b ( cur_2 ) , .c ( cur_3 ) , .a ( ctmn_249 ) , 
    .out0 ( ctmn_250 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 ctmi_271 ( .a ( cur_1 ) , .b ( cur_0 ) , .o1 ( ctmn_249 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_133 ( .b ( C644_ctmn_106 ) , 
    .c ( C644_phfnn_82 ) , .a ( C644_ctmn_109 ) , .o1 ( o_interp_ctrl[31] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sand002aa1n02x5 A81 ( .a ( ctmn_242 ) , .b ( ctmn_238 ) , .o ( N56 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_134 ( .a ( C644_ctmn_104 ) , 
    .b ( C644_ctmn_105 ) , .o1 ( C644_ctmn_106 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sxor002aa1n02x5 ctmi_87 ( .a ( cur_2 ) , .b ( ctmn_264 ) , .out0 ( nxt_2 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorb02aa1n02x5 ctmi_88 ( .a ( cur_2 ) , .b ( ctmn_249 ) , 
    .out0 ( ctmn_265 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saoi013aa1n02x4 C644_ctmi_174 ( .b ( C644_phfnn_81 ) , 
    .c ( C644_ctmn_107 ) , .d ( C644_ctmn_104 ) , .a ( C644_ctmn_130 ) , 
    .o1 ( o_interp_ctrl[16] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sao0012aa1n02x5 C644_ctmi_177 ( .b ( C644_phfnn_85 ) , .c ( cur_0 ) , 
    .a ( o_interp_ctrl[15] ) , .o ( o_interp_ctrl[14] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0soai013aa1n02x4 C644_ctmi_164 ( .b ( cur_2 ) , .c ( C644_phfnn_82 ) , 
    .d ( C644_phfnn_83 ) , .a ( C644_phfnn_87 ) , .o1 ( o_interp_ctrl[23] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_140 ( .b ( C644_ctmn_115 ) , 
    .c ( C644_ctmn_117 ) , .a ( C644_ctmn_109 ) , .o1 ( o_interp_ctrl[30] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_141 ( .a ( C644_ctmn_113 ) , 
    .b ( C644_ctmn_114 ) , .o1 ( C644_ctmn_115 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0soaih22aa1n01x1 C644_ctmi_142 ( .a ( C644_ctmn_110 ) , 
    .b ( C644_ctmn_111 ) , .c ( C644_phfnn_83 ) , .d ( cur_1 ) , 
    .o1 ( C644_ctmn_113 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soaoi13aa1n02x5 C644_ctmi_175 ( .c ( C644_phfnn_81 ) , 
    .d ( o_stage_en[5] ) , .b ( cur_4 ) , .a ( C644_ctmn_105 ) , 
    .o1 ( C644_ctmn_130 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saob012aa1n01x2 C644_ctmi_176 ( .b ( C644_ctmn_107 ) , 
    .c ( C644_ctmn_119 ) , .a ( C644_ctmn_130 ) , 
    .out0 ( o_interp_ctrl[15] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_142 ( .a ( C644_ctmn_120 ) , 
    .o1 ( C644_phfnn_85 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_179 ( .a ( C644_ctmn_130 ) , 
    .b ( C644_ctmn_120 ) , .o1 ( o_interp_ctrl[13] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_173 ( .a ( C644_ctmn_121 ) , 
    .b ( C644_ctmn_129 ) , .o1 ( o_interp_ctrl[17] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_139 ( .a ( C644_ctmn_107 ) , 
    .o1 ( C644_phfnn_82 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soaib12aa1n02x5 C644_ctmi_168 ( .c ( C644_ctmn_114 ) , 
    .b ( C644_ctmn_121 ) , .a ( C644_phfnn_87 ) , 
    .out0 ( o_interp_ctrl[21] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_149 ( .b ( C644_ctmn_120 ) , 
    .c ( C644_ctmn_121 ) , .a ( C644_ctmn_109 ) , .o1 ( o_interp_ctrl[29] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saoi013aa1n02x4 C644_ctmi_150 ( .b ( C644_phfnn_81 ) , .c ( cur_1 ) , 
    .d ( C644_ctmn_118 ) , .a ( C644_ctmn_119 ) , .o1 ( C644_ctmn_120 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sand002aa1n02x5 C644_ctmi_151 ( .a ( cur_2 ) , .b ( cur_3 ) , 
    .o ( C644_ctmn_118 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_154 ( .b ( C644_ctmn_107 ) , 
    .c ( C644_ctmn_122 ) , .a ( C644_ctmn_123 ) , .o1 ( o_interp_ctrl[28] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand23aa1n01x1 C644_ctmi_155 ( .a ( cur_2 ) , .b ( cur_3 ) , 
    .c ( C644_phfnn_84 ) , .o1 ( C644_ctmn_122 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sand002aa1n02x5 C644_ctmi_156 ( .a ( C644_ctmn_109 ) , 
    .b ( C644_ctmn_106 ) , .o ( C644_ctmn_123 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_165 ( .a ( C644_ctmn_117 ) , 
    .b ( C644_ctmn_109 ) , .o1 ( C644_ctmn_127 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_162 ( .b ( C644_ctmn_117 ) , 
    .c ( C644_ctmn_126 ) , .a ( C644_ctmn_123 ) , .o1 ( o_interp_ctrl[24] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai013aa1n02x4 C644_ctmi_157 ( .b ( cur_3 ) , .c ( C644_phfnn_82 ) , 
    .d ( C644_phfnn_83 ) , .a ( C644_ctmn_124 ) , .o1 ( o_interp_ctrl[27] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sand002aa1n02x5 C644_ctmi_158 ( .a ( C644_ctmn_122 ) , 
    .b ( C644_ctmn_123 ) , .o ( C644_ctmn_124 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_159 ( .b ( C644_ctmn_117 ) , 
    .c ( C644_phfnn_86 ) , .a ( C644_ctmn_124 ) , .o1 ( o_interp_ctrl[26] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_143 ( .a ( C644_ctmn_113 ) , 
    .o1 ( C644_phfnn_86 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_163 ( .a ( C644_phfnn_82 ) , .b ( cur_2 ) , 
    .o1 ( C644_ctmn_126 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C644_ctmi_161 ( .b ( C644_ctmn_117 ) , 
    .c ( C644_ctmn_121 ) , .a ( C644_ctmn_124 ) , .o1 ( o_interp_ctrl[25] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_144 ( .a ( C644_ctmn_127 ) , 
    .o1 ( C644_phfnn_87 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_167 ( .a ( C644_phfnn_87 ) , 
    .b ( C644_ctmn_115 ) , .o1 ( o_interp_ctrl[22] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sao0012aa1n02x5 C644_ctmi_169 ( .b ( C644_phfnn_82 ) , 
    .c ( C644_ctmn_114 ) , .a ( o_interp_ctrl[23] ) , 
    .o ( o_interp_ctrl[20] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_171 ( .a ( C644_ctmn_114 ) , 
    .b ( C644_ctmn_127 ) , .o1 ( C644_ctmn_129 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_140 ( .a ( C644_ctmn_105 ) , 
    .o1 ( C644_phfnn_83 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_135 ( .a ( cur_2 ) , .b ( cur_3 ) , 
    .o1 ( C644_ctmn_104 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_136 ( .a ( C644_phfnn_81 ) , .b ( cur_4 ) , 
    .o1 ( C644_ctmn_105 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_143 ( .a ( cur_0 ) , .b ( cur_1 ) , 
    .o1 ( C644_ctmn_110 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C644_phfnr_buf_141 ( .a ( C644_ctmn_111 ) , 
    .o1 ( C644_phfnn_84 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sao0012aa1n02x5 C644_ctmi_127 ( .b ( cur_5 ) , .c ( cur_6 ) , 
    .a ( o_stage_en[5] ) , .o ( o_stage_en[4] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C644_ctmi_137 ( .a ( cur_0 ) , .b ( cur_1 ) , 
    .o1 ( C644_ctmn_107 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_139 ( .a ( cur_5 ) , .b ( o_stage_en[5] ) , 
    .o1 ( C644_ctmn_109 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C644_ctmi_144 ( .a ( cur_4 ) , .b ( C644_phfnn_81 ) , 
    .o1 ( C644_ctmn_111 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0smdn022aa1n01x1 C644_ctmi_146 ( .b ( C644_phfnn_83 ) , 
    .a ( C644_ctmn_111 ) , .sa ( cur_2 ) , .o1 ( C644_ctmn_114 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saoi022aa1n02x5 C644_ctmi_153 ( .a ( C644_ctmn_110 ) , 
    .b ( C644_ctmn_105 ) , .c ( cur_1 ) , .d ( C644_phfnn_84 ) , 
    .o1 ( C644_ctmn_121 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sand003aa1n02x5 C644_ctmi_89 ( .a ( cur_5 ) , .b ( C644_ctmn_104 ) , 
    .c ( C644_ctmn_110 ) , .o ( C644_ctmn_119 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0smbn022aa1n01x1 C644_ctmi_90 ( .b ( C644_phfnn_83 ) , .a ( C644_ctmn_111 ) , 
    .sa ( cur_3 ) , .o ( C644_ctmn_117 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saob012aa1n01x2 C644_ctmi_91 ( .b ( C644_ctmn_107 ) , .c ( C644_ctmn_105 ) , 
    .a ( C644_ctmn_129 ) , .out0 ( o_interp_ctrl[19] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snanb02aa1n02x5 C644_ctmi_92 ( .a ( C644_ctmn_113 ) , .b ( C644_ctmn_129 ) , 
    .out0 ( o_interp_ctrl[18] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C645_ctmi_109 ( .a ( C645_ctmn_85 ) , .b ( C645_ctmn_80 ) , 
    .o1 ( o_interp_ctrl[2] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai013aa1n02x4 C645_ctmi_102 ( .b ( C645_ctmn_82 ) , .c ( cur_1 ) , 
    .d ( cur_2 ) , .a ( C645_ctmn_71 ) , .o1 ( o_interp_ctrl[7] ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soabi12aa1n02x5 C645_ctmi_103 ( .b ( C645_phfnn_88 ) , .c ( C645_ctmn_84 ) , 
    .a ( o_interp_ctrl[7] ) , .out0 ( o_interp_ctrl[6] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snanb02aa1n02x5 C645_ctmi_101 ( .a ( C645_ctmn_77 ) , .b ( C645_ctmn_73 ) , 
    .out0 ( o_interp_ctrl[8] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soabi12aa1n02x5 C645_ctmi_108 ( .b ( C645_ctmn_68 ) , .c ( C645_phfnn_89 ) , 
    .a ( o_interp_ctrl[4] ) , .out0 ( o_interp_ctrl[3] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0saoi022aa1n02x5 C645_ctmi_104 ( .a ( C645_ctmn_79 ) , .b ( C645_ctmn_74 ) , 
    .c ( cur_1 ) , .d ( C645_ctmn_76 ) , .o1 ( C645_ctmn_84 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C645_ctmi_105 ( .a ( C645_ctmn_84 ) , .b ( C645_ctmn_71 ) , 
    .o1 ( o_interp_ctrl[5] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C645_ctmi_93 ( .b ( C645_ctmn_68 ) , .c ( C645_ctmn_73 ) , 
    .a ( C645_ctmn_78 ) , .o1 ( o_interp_ctrl[11] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C645_ctmi_94 ( .b ( C645_ctmn_73 ) , .c ( C645_ctmn_80 ) , 
    .a ( C645_ctmn_78 ) , .o1 ( o_interp_ctrl[10] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C645_phfnr_buf_146 ( .a ( cur_2 ) , .o1 ( C645_phfnn_89 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C645_ctmi_96 ( .a ( cur_0 ) , .b ( cur_1 ) , 
    .o1 ( C645_ctmn_79 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C645_ctmi_110 ( .a ( C645_ctmn_83 ) , .b ( C645_ctmn_85 ) , 
    .o1 ( o_interp_ctrl[1] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C645_ctmi_97 ( .b ( C645_ctmn_83 ) , .c ( C645_ctmn_73 ) , 
    .a ( C645_ctmn_78 ) , .o1 ( o_interp_ctrl[9] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0saoa112aa1n02x5 C645_ctmi_98 ( .c ( cur_5 ) , .d ( cur_1 ) , 
    .b ( C645_ctmn_81 ) , .a ( C645_ctmn_82 ) , .o ( C645_ctmn_83 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C645_ctmi_99 ( .a ( cur_1 ) , .b ( cur_2 ) , 
    .o1 ( C645_ctmn_81 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saob012aa1n01x2 C645_ctmi_106 ( .b ( C645_ctmn_76 ) , .c ( C645_ctmn_68 ) , 
    .a ( C645_ctmn_85 ) , .out0 ( o_interp_ctrl[4] ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C645_ctmi_107 ( .a ( C645_phfnn_92 ) , .b ( C645_ctmn_74 ) , 
    .o1 ( C645_ctmn_85 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C645_ctmi_100 ( .a ( cur_5 ) , .b ( C645_phfnn_88 ) , 
    .o1 ( C645_ctmn_82 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C645_ctmi_77 ( .a ( C645_ctmn_73 ) , .b ( C645_ctmn_78 ) , 
    .o1 ( o_interp_ctrl[12] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soai012aa1n02x5 C645_ctmi_78 ( .b ( C645_ctmn_65 ) , .c ( C645_ctmn_68 ) , 
    .a ( C645_phfnn_92 ) , .o1 ( C645_ctmn_73 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C645_phfnr_buf_147 ( .a ( cur_3 ) , .o1 ( C645_phfnn_90 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C645_phfnr_buf_148 ( .a ( cur_5 ) , .o1 ( C645_phfnn_91 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sinv000ta1n02x5 C645_phfnr_buf_149 ( .a ( C645_ctmn_71 ) , 
    .o1 ( C645_phfnn_92 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0soaih22aa1n01x1 C645_ctmi_84 ( .a ( C645_phfnn_90 ) , .b ( C645_ctmn_70 ) , 
    .c ( cur_3 ) , .d ( C645_ctmn_65 ) , .o1 ( C645_ctmn_71 ) , .vcc ( VDD ) , 
    .vssx ( VSS ) ) ;
i0snand22aa1n01x1 C645_ctmi_86 ( .a ( cur_4 ) , .b ( cur_5 ) , 
    .o1 ( C645_ctmn_70 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0saoi122aa1n02x5 C645_ctmi_88 ( .b ( C645_ctmn_65 ) , .c ( C645_ctmn_70 ) , 
    .d ( C645_phfnn_90 ) , .e ( C645_ctmn_74 ) , .a ( C645_ctmn_77 ) , 
    .o1 ( C645_ctmn_78 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C645_ctmi_89 ( .a ( C645_phfnn_91 ) , .b ( cur_2 ) , 
    .o1 ( C645_ctmn_74 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sand002aa1n02x5 C645_ctmi_90 ( .a ( cur_3 ) , .b ( C645_ctmn_76 ) , 
    .o ( C645_ctmn_77 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0snorp02aa1n02x5 C645_ctmi_91 ( .a ( C645_phfnn_89 ) , .b ( cur_5 ) , 
    .o1 ( C645_ctmn_76 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 C645_ctmi_112 ( .a ( cur_5 ) , .b ( cur_4 ) , 
    .o ( C645_ctmn_65 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 C645_ctmi_113 ( .a ( cur_0 ) , .b ( o_interp_ctrl[1] ) , 
    .o ( o_interp_ctrl[0] ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sao0022aa1n02x5 C645_ctmi_114 ( .a ( cur_1 ) , .b ( cur_5 ) , 
    .c ( C645_phfnn_89 ) , .d ( C645_ctmn_79 ) , .o ( C645_ctmn_80 ) , 
    .vcc ( VDD ) , .vssx ( VSS ) ) ;
i0sorn002aa1n02x5 C645_ctmi_115 ( .a ( cur_0 ) , .b ( cur_1 ) , 
    .o ( C645_ctmn_68 ) , .vcc ( VDD ) , .vssx ( VSS ) ) ;
endmodule


