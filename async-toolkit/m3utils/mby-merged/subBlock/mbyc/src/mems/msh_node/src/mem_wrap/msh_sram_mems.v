//------------------------------------------------------------------------------
//  INTEL CONFIDENTIAL
//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////
//
//                      Automated Memory Wrappers Creator
//
//      Created by solson with create_memories script version 2.31 on NA
//                                      & 
// Physical file /nfs/site/disks/slx_1593/solson/mby/work_root/mby-mby-x0_WW4818/tools/mgm/mby_physical_params.csv
//
//////////////////////////////////////////////////////////////////////
`include        "msh_mem.def"
module msh_sram_mems // Parameters
// Interface
(

// Module inputs

   input                                          car_raw_lan_power_good_with_byprst      ,   
   input                                          fary_enblfloat_sram                     ,   
   input                                          fary_ensleep_sram                       ,   
   input                                 [20-1:0] fary_ffuse_data_misc_sram               ,   
   input                                    [1:0] fary_fwen_sram                          ,   
   input                                          fary_pwren_b_sram                       ,   
   input                                          fary_stm_enable                         ,   
   input                                          fary_stm_hilo                           ,   
   input                                          fary_wakeup_sram                        ,   
   input                                          fdfx_lbist_test_mode                    ,   
   input                                          fscan_byprst_b                          ,   
   input                                          fscan_mode                              ,   
   input                                 [32-1:0] fscan_ram_awt_mode                      ,   
   input                                 [32-1:0] fscan_ram_awt_ren                       ,   
   input                                 [32-1:0] fscan_ram_awt_wen                       ,   
   input                                 [32-1:0] fscan_ram_bypsel                        ,   
   input                                          fscan_ram_init_en                       ,   
   input                                          fscan_ram_init_val                      ,   
   input                                 [32-1:0] fscan_ram_odis_b                        ,   
   input                                          fscan_ram_rddis_b                       ,   
   input                                          fscan_ram_wrdis_b                       ,   
   input                                          fscan_rstbypen                          ,   
   input                                          mclk                                    ,   
   input [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_0_to_mem               ,   
   input [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_1_to_mem               ,   
   input [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_2_to_mem               ,   
   input [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_3_to_mem               ,

// Module outputs

  output                                          aary_pwren_b_sram                       ,   
  output [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_0_from_mem             ,   
  output [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_1_from_mem             ,   
  output [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_2_from_mem             ,   
  output [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_3_from_mem                 
);

// Module wires

    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_0_aary_pwren_b_sram     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fary_pwren_b_sram     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_mode     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_ren     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_wen     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_bypsel     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_odis_b     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_1_aary_pwren_b_sram     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fary_pwren_b_sram     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_mode     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_ren     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_wen     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_bypsel     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_odis_b     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_2_aary_pwren_b_sram     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fary_pwren_b_sram     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_mode     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_ren     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_wen     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_bypsel     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_odis_b     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_3_aary_pwren_b_sram     ;   
    wire                                          msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fary_pwren_b_sram     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_mode     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_ren     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_wen     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_bypsel     ;   
    wire                                  [8-1:0] msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_odis_b    ;

//BEGIN_TOP_LOGIC
//END_TOP_LOGIC


genvar iter;


// Instances

msh_wrap_mem_msh_bank_ram_shell_4096x552  #( // Parameters
    .MEM_PROT_TYPE(2),
    .MEM_PST_EBB_SAMPLE(0),
    .WRAPPER_COL_REPAIR(0),
    .MEM_WIDTH(552),
    .LL_INIT_OFFSET( ),
    .MEM_INIT_VALUE(552'h0),
    .BYPASS_CLK_MUX(1),
    .LL_IS_LAST(0),
    .MEM_PROT_RESOLUTION( ),
    .TOTAL_MEMORY_INSTANCE(8),
    .NFUSEMISC_SRAM(20),
    .FROM_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH),
    .MEM_PROT_INTERLV_LEVEL(1),
    .MEM_WIDTH_NO_SIG(552),
    .MEM_WR_RESOLUTION_NO_SIG(552),
    .MSWT_MODE(0),
    .NFUSERED_SRAM(0),
    .MEM_INIT_TYPE(0),
    .MEM_DELAY(1),
    .MEM_WR_RES_PROT_FRAGM( ),
    .MEM_DEPTH(4096),
    .MEM_WR_RESOLUTION(552),
    .BYPASS_MBIST_EN_SYNC(0),
    .TO_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH),
    .WRAPPER_REDROW_ENABLE(0)
) msh_wrap_mem_msh_bank_ram_shell_4096x552_0(
        .clk(mclk),
        .car_raw_lan_power_good(car_raw_lan_power_good_with_byprst),
        .wrap_shell_to_mem(msh_msh_bank_ram_0_to_mem),
        .wrap_shell_from_mem(msh_msh_bank_ram_0_from_mem),
        .fscan_mode(fscan_mode),
        .fscan_byprst_b(fscan_byprst_b),
        .fscan_rstbypen(fscan_rstbypen),
        .fscan_ram_init_en(fscan_ram_init_en),
        .fscan_ram_init_val(fscan_ram_init_val),
        .fdfx_lbist_test_mode(fdfx_lbist_test_mode),
        .fscan_ram_rddis_b(fscan_ram_rddis_b),
        .fscan_ram_wrdis_b(fscan_ram_wrdis_b),
        .fscan_ram_odis_b(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_odis_b),
        .fscan_ram_awt_mode(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_mode),
        .fscan_ram_awt_ren(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_ren),
        .fscan_ram_awt_wen(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_wen),
        .fscan_ram_bypsel(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_bypsel),
        .fary_stm_enable(fary_stm_enable),
        .fary_stm_hilo(fary_stm_hilo),
        .fary_wakeup_sram(fary_wakeup_sram),
        .fary_fwen_sram(fary_fwen_sram),
        .fary_enblfloat_sram(fary_enblfloat_sram),
        .fary_ensleep_sram(fary_ensleep_sram),
        .fary_ffuse_data_misc_sram(fary_ffuse_data_misc_sram),
        .fary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fary_pwren_b_sram),
        .aary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_0_aary_pwren_b_sram)
);

msh_wrap_mem_msh_bank_ram_shell_4096x552  #( // Parameters
    .MEM_PROT_TYPE(2),
    .MEM_PST_EBB_SAMPLE(0),
    .WRAPPER_COL_REPAIR(0),
    .MEM_WIDTH(552),
    .LL_INIT_OFFSET( ),
    .MEM_INIT_VALUE(552'h0),
    .BYPASS_CLK_MUX(1),
    .LL_IS_LAST(0),
    .MEM_PROT_RESOLUTION( ),
    .TOTAL_MEMORY_INSTANCE(8),
    .NFUSEMISC_SRAM(20),
    .FROM_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH),
    .MEM_PROT_INTERLV_LEVEL(1),
    .MEM_WIDTH_NO_SIG(552),
    .MEM_WR_RESOLUTION_NO_SIG(552),
    .MSWT_MODE(0),
    .NFUSERED_SRAM(0),
    .MEM_INIT_TYPE(0),
    .MEM_DELAY(1),
    .MEM_WR_RES_PROT_FRAGM( ),
    .MEM_DEPTH(4096),
    .MEM_WR_RESOLUTION(552),
    .BYPASS_MBIST_EN_SYNC(0),
    .TO_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH),
    .WRAPPER_REDROW_ENABLE(0)
) msh_wrap_mem_msh_bank_ram_shell_4096x552_1(
        .clk(mclk),
        .car_raw_lan_power_good(car_raw_lan_power_good_with_byprst),
        .wrap_shell_to_mem(msh_msh_bank_ram_1_to_mem),
        .wrap_shell_from_mem(msh_msh_bank_ram_1_from_mem),
        .fscan_mode(fscan_mode),
        .fscan_byprst_b(fscan_byprst_b),
        .fscan_rstbypen(fscan_rstbypen),
        .fscan_ram_init_en(fscan_ram_init_en),
        .fscan_ram_init_val(fscan_ram_init_val),
        .fdfx_lbist_test_mode(fdfx_lbist_test_mode),
        .fscan_ram_rddis_b(fscan_ram_rddis_b),
        .fscan_ram_wrdis_b(fscan_ram_wrdis_b),
        .fscan_ram_odis_b(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_odis_b),
        .fscan_ram_awt_mode(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_mode),
        .fscan_ram_awt_ren(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_ren),
        .fscan_ram_awt_wen(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_wen),
        .fscan_ram_bypsel(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_bypsel),
        .fary_stm_enable(fary_stm_enable),
        .fary_stm_hilo(fary_stm_hilo),
        .fary_wakeup_sram(fary_wakeup_sram),
        .fary_fwen_sram(fary_fwen_sram),
        .fary_enblfloat_sram(fary_enblfloat_sram),
        .fary_ensleep_sram(fary_ensleep_sram),
        .fary_ffuse_data_misc_sram(fary_ffuse_data_misc_sram),
        .fary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fary_pwren_b_sram),
        .aary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_1_aary_pwren_b_sram)
);

msh_wrap_mem_msh_bank_ram_shell_4096x552  #( // Parameters
    .MEM_PROT_TYPE(2),
    .MEM_PST_EBB_SAMPLE(0),
    .WRAPPER_COL_REPAIR(0),
    .MEM_WIDTH(552),
    .LL_INIT_OFFSET( ),
    .MEM_INIT_VALUE(552'h0),
    .BYPASS_CLK_MUX(1),
    .LL_IS_LAST(0),
    .MEM_PROT_RESOLUTION( ),
    .TOTAL_MEMORY_INSTANCE(8),
    .NFUSEMISC_SRAM(20),
    .FROM_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH),
    .MEM_PROT_INTERLV_LEVEL(1),
    .MEM_WIDTH_NO_SIG(552),
    .MEM_WR_RESOLUTION_NO_SIG(552),
    .MSWT_MODE(0),
    .NFUSERED_SRAM(0),
    .MEM_INIT_TYPE(0),
    .MEM_DELAY(1),
    .MEM_WR_RES_PROT_FRAGM( ),
    .MEM_DEPTH(4096),
    .MEM_WR_RESOLUTION(552),
    .BYPASS_MBIST_EN_SYNC(0),
    .TO_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH),
    .WRAPPER_REDROW_ENABLE(0)
) msh_wrap_mem_msh_bank_ram_shell_4096x552_2(
        .clk(mclk),
        .car_raw_lan_power_good(car_raw_lan_power_good_with_byprst),
        .wrap_shell_to_mem(msh_msh_bank_ram_2_to_mem),
        .wrap_shell_from_mem(msh_msh_bank_ram_2_from_mem),
        .fscan_mode(fscan_mode),
        .fscan_byprst_b(fscan_byprst_b),
        .fscan_rstbypen(fscan_rstbypen),
        .fscan_ram_init_en(fscan_ram_init_en),
        .fscan_ram_init_val(fscan_ram_init_val),
        .fdfx_lbist_test_mode(fdfx_lbist_test_mode),
        .fscan_ram_rddis_b(fscan_ram_rddis_b),
        .fscan_ram_wrdis_b(fscan_ram_wrdis_b),
        .fscan_ram_odis_b(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_odis_b),
        .fscan_ram_awt_mode(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_mode),
        .fscan_ram_awt_ren(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_ren),
        .fscan_ram_awt_wen(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_wen),
        .fscan_ram_bypsel(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_bypsel),
        .fary_stm_enable(fary_stm_enable),
        .fary_stm_hilo(fary_stm_hilo),
        .fary_wakeup_sram(fary_wakeup_sram),
        .fary_fwen_sram(fary_fwen_sram),
        .fary_enblfloat_sram(fary_enblfloat_sram),
        .fary_ensleep_sram(fary_ensleep_sram),
        .fary_ffuse_data_misc_sram(fary_ffuse_data_misc_sram),
        .fary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fary_pwren_b_sram),
        .aary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_2_aary_pwren_b_sram)
);

msh_wrap_mem_msh_bank_ram_shell_4096x552  #( // Parameters
    .MEM_PROT_TYPE(2),
    .MEM_PST_EBB_SAMPLE(0),
    .WRAPPER_COL_REPAIR(0),
    .MEM_WIDTH(552),
    .LL_INIT_OFFSET( ),
    .MEM_INIT_VALUE(552'h0),
    .BYPASS_CLK_MUX(1),
    .LL_IS_LAST(0),
    .MEM_PROT_RESOLUTION( ),
    .TOTAL_MEMORY_INSTANCE(8),
    .NFUSEMISC_SRAM(20),
    .FROM_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH),
    .MEM_PROT_INTERLV_LEVEL(1),
    .MEM_WIDTH_NO_SIG(552),
    .MEM_WR_RESOLUTION_NO_SIG(552),
    .MSWT_MODE(0),
    .NFUSERED_SRAM(0),
    .MEM_INIT_TYPE(0),
    .MEM_DELAY(1),
    .MEM_WR_RES_PROT_FRAGM( ),
    .MEM_DEPTH(4096),
    .MEM_WR_RESOLUTION(552),
    .BYPASS_MBIST_EN_SYNC(0),
    .TO_MEM_WIDTH(`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH),
    .WRAPPER_REDROW_ENABLE(0)
) msh_wrap_mem_msh_bank_ram_shell_4096x552_3(
        .clk(mclk),
        .car_raw_lan_power_good(car_raw_lan_power_good_with_byprst),
        .wrap_shell_to_mem(msh_msh_bank_ram_3_to_mem),
        .wrap_shell_from_mem(msh_msh_bank_ram_3_from_mem),
        .fscan_mode(fscan_mode),
        .fscan_byprst_b(fscan_byprst_b),
        .fscan_rstbypen(fscan_rstbypen),
        .fscan_ram_init_en(fscan_ram_init_en),
        .fscan_ram_init_val(fscan_ram_init_val),
        .fdfx_lbist_test_mode(fdfx_lbist_test_mode),
        .fscan_ram_rddis_b(fscan_ram_rddis_b),
        .fscan_ram_wrdis_b(fscan_ram_wrdis_b),
        .fscan_ram_odis_b(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_odis_b),
        .fscan_ram_awt_mode(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_mode),
        .fscan_ram_awt_ren(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_ren),
        .fscan_ram_awt_wen(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_wen),
        .fscan_ram_bypsel(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_bypsel),
        .fary_stm_enable(fary_stm_enable),
        .fary_stm_hilo(fary_stm_hilo),
        .fary_wakeup_sram(fary_wakeup_sram),
        .fary_fwen_sram(fary_fwen_sram),
        .fary_enblfloat_sram(fary_enblfloat_sram),
        .fary_ensleep_sram(fary_ensleep_sram),
        .fary_ffuse_data_misc_sram(fary_ffuse_data_misc_sram),
        .fary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fary_pwren_b_sram),
        .aary_pwren_b_sram(msh_wrap_mem_msh_bank_ram_shell_4096x552_3_aary_pwren_b_sram)
);


// BEGIN_BOTTOM_LOGIC

assign msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_odis_b = fscan_ram_odis_b[7:0];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_mode = fscan_ram_awt_mode[7:0];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_ren = fscan_ram_awt_ren[7:0];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_awt_wen = fscan_ram_awt_wen[7:0];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fscan_ram_bypsel = fscan_ram_bypsel[7:0];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_0_fary_pwren_b_sram = fary_pwren_b_sram;
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_odis_b = fscan_ram_odis_b[15:8];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_mode = fscan_ram_awt_mode[15:8];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_ren = fscan_ram_awt_ren[15:8];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_awt_wen = fscan_ram_awt_wen[15:8];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fscan_ram_bypsel = fscan_ram_bypsel[15:8];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_1_fary_pwren_b_sram = msh_wrap_mem_msh_bank_ram_shell_4096x552_0_aary_pwren_b_sram;
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_odis_b = fscan_ram_odis_b[23:16];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_mode = fscan_ram_awt_mode[23:16];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_ren = fscan_ram_awt_ren[23:16];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_awt_wen = fscan_ram_awt_wen[23:16];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fscan_ram_bypsel = fscan_ram_bypsel[23:16];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_2_fary_pwren_b_sram = msh_wrap_mem_msh_bank_ram_shell_4096x552_1_aary_pwren_b_sram;
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_odis_b = fscan_ram_odis_b[31:24];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_mode = fscan_ram_awt_mode[31:24];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_ren = fscan_ram_awt_ren[31:24];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_awt_wen = fscan_ram_awt_wen[31:24];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fscan_ram_bypsel = fscan_ram_bypsel[31:24];
assign msh_wrap_mem_msh_bank_ram_shell_4096x552_3_fary_pwren_b_sram = msh_wrap_mem_msh_bank_ram_shell_4096x552_2_aary_pwren_b_sram;
assign  aary_pwren_b_sram               = msh_wrap_mem_msh_bank_ram_shell_4096x552_3_aary_pwren_b_sram;

// END_BOTTOM_LOGIC


endmodule

