// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

module msh_shells_wrapper // Parameters
#(
  parameter MEM_CSR_RD_DATA_SAMPLE = 0,
  parameter INT_ON_CORR_ECC = 0,
  parameter INT_FROM_STATUS = 1,
  parameter MEM_DBG_DW_SEL_WIDTH = `MBY_MEM_DBG_DW_SEL_WIDTH       ,
  parameter MEM_RM_WIDTH = `MBY_MEM_RM_WIDTH                ,
  parameter MEM_DBG_RD_ADR_WIDTH = `MBY_MEM_DBG_RD_ADR_WIDTH        ,
  parameter MEM_GEN_ECC_INST_NUM = `MBY_MEM_GEN_ECC_INST_NUM       
)
// Interface
(

// Module inputs

   input                                          MSH_BANK_RAM_0_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_0_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_10_CFG_reg_sel             ,   
   input                                          MSH_BANK_RAM_10_STATUS_reg_sel          ,   
   input                                          MSH_BANK_RAM_11_CFG_reg_sel             ,   
   input                                          MSH_BANK_RAM_11_STATUS_reg_sel          ,   
   input                                          MSH_BANK_RAM_12_CFG_reg_sel             ,   
   input                                          MSH_BANK_RAM_12_STATUS_reg_sel          ,   
   input                                          MSH_BANK_RAM_13_CFG_reg_sel             ,   
   input                                          MSH_BANK_RAM_13_STATUS_reg_sel          ,   
   input                                          MSH_BANK_RAM_14_CFG_reg_sel             ,   
   input                                          MSH_BANK_RAM_14_STATUS_reg_sel          ,   
   input                                          MSH_BANK_RAM_15_CFG_reg_sel             ,   
   input                                          MSH_BANK_RAM_15_STATUS_reg_sel          ,   
   input                                          MSH_BANK_RAM_1_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_1_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_2_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_2_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_3_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_3_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_4_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_4_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_5_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_5_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_6_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_6_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_7_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_7_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_8_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_8_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_9_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_9_STATUS_reg_sel           ,   
   input                                          MSH_ECC_COR_ERR_reg_sel                 ,   
   input                                          MSH_ECC_UNCOR_ERR_reg_sel               ,   
   input                                          clk                                     ,   
   input                                 [12-1:0] msh_bank_ram_0_adr                      ,   
   input                                          msh_bank_ram_0_mem_ls_enter             ,   
   input                                          msh_bank_ram_0_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_0_wr_data                  ,   
   input                                          msh_bank_ram_0_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_10_adr                     ,   
   input                                          msh_bank_ram_10_mem_ls_enter            ,   
   input                                          msh_bank_ram_10_rd_en                   ,   
   input                                [133-1:0] msh_bank_ram_10_wr_data                 ,   
   input                                          msh_bank_ram_10_wr_en                   ,   
   input                                 [12-1:0] msh_bank_ram_11_adr                     ,   
   input                                          msh_bank_ram_11_mem_ls_enter            ,   
   input                                          msh_bank_ram_11_rd_en                   ,   
   input                                [133-1:0] msh_bank_ram_11_wr_data                 ,   
   input                                          msh_bank_ram_11_wr_en                   ,   
   input                                 [12-1:0] msh_bank_ram_12_adr                     ,   
   input                                          msh_bank_ram_12_mem_ls_enter            ,   
   input                                          msh_bank_ram_12_rd_en                   ,   
   input                                [133-1:0] msh_bank_ram_12_wr_data                 ,   
   input                                          msh_bank_ram_12_wr_en                   ,   
   input                                 [12-1:0] msh_bank_ram_13_adr                     ,   
   input                                          msh_bank_ram_13_mem_ls_enter            ,   
   input                                          msh_bank_ram_13_rd_en                   ,   
   input                                [133-1:0] msh_bank_ram_13_wr_data                 ,   
   input                                          msh_bank_ram_13_wr_en                   ,   
   input                                 [12-1:0] msh_bank_ram_14_adr                     ,   
   input                                          msh_bank_ram_14_mem_ls_enter            ,   
   input                                          msh_bank_ram_14_rd_en                   ,   
   input                                [133-1:0] msh_bank_ram_14_wr_data                 ,   
   input                                          msh_bank_ram_14_wr_en                   ,   
   input                                 [12-1:0] msh_bank_ram_15_adr                     ,   
   input                                          msh_bank_ram_15_mem_ls_enter            ,   
   input                                          msh_bank_ram_15_rd_en                   ,   
   input                                [133-1:0] msh_bank_ram_15_wr_data                 ,   
   input                                          msh_bank_ram_15_wr_en                   ,   
   input                                 [12-1:0] msh_bank_ram_1_adr                      ,   
   input                                          msh_bank_ram_1_mem_ls_enter             ,   
   input                                          msh_bank_ram_1_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_1_wr_data                  ,   
   input                                          msh_bank_ram_1_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_2_adr                      ,   
   input                                          msh_bank_ram_2_mem_ls_enter             ,   
   input                                          msh_bank_ram_2_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_2_wr_data                  ,   
   input                                          msh_bank_ram_2_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_3_adr                      ,   
   input                                          msh_bank_ram_3_mem_ls_enter             ,   
   input                                          msh_bank_ram_3_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_3_wr_data                  ,   
   input                                          msh_bank_ram_3_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_4_adr                      ,   
   input                                          msh_bank_ram_4_mem_ls_enter             ,   
   input                                          msh_bank_ram_4_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_4_wr_data                  ,   
   input                                          msh_bank_ram_4_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_5_adr                      ,   
   input                                          msh_bank_ram_5_mem_ls_enter             ,   
   input                                          msh_bank_ram_5_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_5_wr_data                  ,   
   input                                          msh_bank_ram_5_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_6_adr                      ,   
   input                                          msh_bank_ram_6_mem_ls_enter             ,   
   input                                          msh_bank_ram_6_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_6_wr_data                  ,   
   input                                          msh_bank_ram_6_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_7_adr                      ,   
   input                                          msh_bank_ram_7_mem_ls_enter             ,   
   input                                          msh_bank_ram_7_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_7_wr_data                  ,   
   input                                          msh_bank_ram_7_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_8_adr                      ,   
   input                                          msh_bank_ram_8_mem_ls_enter             ,   
   input                                          msh_bank_ram_8_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_8_wr_data                  ,   
   input                                          msh_bank_ram_8_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_9_adr                      ,   
   input                                          msh_bank_ram_9_mem_ls_enter             ,   
   input                                          msh_bank_ram_9_rd_en                    ,   
   input                                [133-1:0] msh_bank_ram_9_wr_data                  ,   
   input                                          msh_bank_ram_9_wr_en                    ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_0_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_10_from_mem            ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_11_from_mem            ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_12_from_mem            ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_13_from_mem            ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_14_from_mem            ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_15_from_mem            ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_1_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_2_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_3_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_4_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_5_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_6_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_7_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_8_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_9_from_mem             ,   
   input                                          reset_n                                 ,   
   input                                          unified_regs_rd                         ,   
   input                                   [31:0] unified_regs_wr_data                    ,

// Module outputs

  output                                          msh_bank_ram_0_ecc_uncor_err            ,   
  output                                          msh_bank_ram_0_init_done                ,   
  output                                [133-1:0] msh_bank_ram_0_rd_data                  ,   
  output                                          msh_bank_ram_0_rd_valid                 ,   
  output                                          msh_bank_ram_10_ecc_uncor_err           ,   
  output                                          msh_bank_ram_10_init_done               ,   
  output                                [133-1:0] msh_bank_ram_10_rd_data                 ,   
  output                                          msh_bank_ram_10_rd_valid                ,   
  output                                          msh_bank_ram_11_ecc_uncor_err           ,   
  output                                          msh_bank_ram_11_init_done               ,   
  output                                [133-1:0] msh_bank_ram_11_rd_data                 ,   
  output                                          msh_bank_ram_11_rd_valid                ,   
  output                                          msh_bank_ram_12_ecc_uncor_err           ,   
  output                                          msh_bank_ram_12_init_done               ,   
  output                                [133-1:0] msh_bank_ram_12_rd_data                 ,   
  output                                          msh_bank_ram_12_rd_valid                ,   
  output                                          msh_bank_ram_13_ecc_uncor_err           ,   
  output                                          msh_bank_ram_13_init_done               ,   
  output                                [133-1:0] msh_bank_ram_13_rd_data                 ,   
  output                                          msh_bank_ram_13_rd_valid                ,   
  output                                          msh_bank_ram_14_ecc_uncor_err           ,   
  output                                          msh_bank_ram_14_init_done               ,   
  output                                [133-1:0] msh_bank_ram_14_rd_data                 ,   
  output                                          msh_bank_ram_14_rd_valid                ,   
  output                                          msh_bank_ram_15_ecc_uncor_err           ,   
  output                                          msh_bank_ram_15_init_done               ,   
  output                                [133-1:0] msh_bank_ram_15_rd_data                 ,   
  output                                          msh_bank_ram_15_rd_valid                ,   
  output                                          msh_bank_ram_1_ecc_uncor_err            ,   
  output                                          msh_bank_ram_1_init_done                ,   
  output                                [133-1:0] msh_bank_ram_1_rd_data                  ,   
  output                                          msh_bank_ram_1_rd_valid                 ,   
  output                                          msh_bank_ram_2_ecc_uncor_err            ,   
  output                                          msh_bank_ram_2_init_done                ,   
  output                                [133-1:0] msh_bank_ram_2_rd_data                  ,   
  output                                          msh_bank_ram_2_rd_valid                 ,   
  output                                          msh_bank_ram_3_ecc_uncor_err            ,   
  output                                          msh_bank_ram_3_init_done                ,   
  output                                [133-1:0] msh_bank_ram_3_rd_data                  ,   
  output                                          msh_bank_ram_3_rd_valid                 ,   
  output                                          msh_bank_ram_4_ecc_uncor_err            ,   
  output                                          msh_bank_ram_4_init_done                ,   
  output                                [133-1:0] msh_bank_ram_4_rd_data                  ,   
  output                                          msh_bank_ram_4_rd_valid                 ,   
  output                                          msh_bank_ram_5_ecc_uncor_err            ,   
  output                                          msh_bank_ram_5_init_done                ,   
  output                                [133-1:0] msh_bank_ram_5_rd_data                  ,   
  output                                          msh_bank_ram_5_rd_valid                 ,   
  output                                          msh_bank_ram_6_ecc_uncor_err            ,   
  output                                          msh_bank_ram_6_init_done                ,   
  output                                [133-1:0] msh_bank_ram_6_rd_data                  ,   
  output                                          msh_bank_ram_6_rd_valid                 ,   
  output                                          msh_bank_ram_7_ecc_uncor_err            ,   
  output                                          msh_bank_ram_7_init_done                ,   
  output                                [133-1:0] msh_bank_ram_7_rd_data                  ,   
  output                                          msh_bank_ram_7_rd_valid                 ,   
  output                                          msh_bank_ram_8_ecc_uncor_err            ,   
  output                                          msh_bank_ram_8_init_done                ,   
  output                                [133-1:0] msh_bank_ram_8_rd_data                  ,   
  output                                          msh_bank_ram_8_rd_valid                 ,   
  output                                          msh_bank_ram_9_ecc_uncor_err            ,   
  output                                          msh_bank_ram_9_init_done                ,   
  output                                [133-1:0] msh_bank_ram_9_rd_data                  ,   
  output                                          msh_bank_ram_9_rd_valid                 ,   
  output                                          msh_ecc_int                             ,   
  output                                          msh_init_done                           ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_0_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_10_to_mem              ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_11_to_mem              ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_12_to_mem              ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_13_to_mem              ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_14_to_mem              ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_15_to_mem              ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_1_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_2_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_3_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_4_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_5_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_6_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_7_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_8_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_9_to_mem               ,   
  output                                          unified_regs_ack                        ,   
  output                                   [31:0] unified_regs_rd_data                          
);

// Module wires

    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_0_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_0_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_10_from_ctl            ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_10_to_ctl              ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_11_from_ctl            ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_11_to_ctl              ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_12_from_ctl            ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_12_to_ctl              ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_13_from_ctl            ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_13_to_ctl              ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_14_from_ctl            ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_14_to_ctl              ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_15_from_ctl            ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_15_to_ctl              ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_1_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_1_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_2_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_2_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_3_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_3_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_4_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_4_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_5_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_5_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_6_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_6_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_7_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_7_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_8_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_8_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_9_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_9_to_ctl              ;

genvar iter;


// Instances

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_0(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_0_adr),
        .rd_en(msh_bank_ram_0_rd_en),
        .wr_en(msh_bank_ram_0_wr_en),
        .wr_data(msh_bank_ram_0_wr_data),
        .rd_data(msh_bank_ram_0_rd_data),
        .rd_valid(msh_bank_ram_0_rd_valid),
        .init_done(msh_bank_ram_0_init_done),
        .ecc_uncor_err(msh_bank_ram_0_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_0_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_0_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_0_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_0_to_ctl),
        .mem_ls_enter(msh_bank_ram_0_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_1(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_1_adr),
        .rd_en(msh_bank_ram_1_rd_en),
        .wr_en(msh_bank_ram_1_wr_en),
        .wr_data(msh_bank_ram_1_wr_data),
        .rd_data(msh_bank_ram_1_rd_data),
        .rd_valid(msh_bank_ram_1_rd_valid),
        .init_done(msh_bank_ram_1_init_done),
        .ecc_uncor_err(msh_bank_ram_1_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_1_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_1_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_1_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_1_to_ctl),
        .mem_ls_enter(msh_bank_ram_1_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_10(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_10_adr),
        .rd_en(msh_bank_ram_10_rd_en),
        .wr_en(msh_bank_ram_10_wr_en),
        .wr_data(msh_bank_ram_10_wr_data),
        .rd_data(msh_bank_ram_10_rd_data),
        .rd_valid(msh_bank_ram_10_rd_valid),
        .init_done(msh_bank_ram_10_init_done),
        .ecc_uncor_err(msh_bank_ram_10_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_10_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_10_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_10_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_10_to_ctl),
        .mem_ls_enter(msh_bank_ram_10_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_11(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_11_adr),
        .rd_en(msh_bank_ram_11_rd_en),
        .wr_en(msh_bank_ram_11_wr_en),
        .wr_data(msh_bank_ram_11_wr_data),
        .rd_data(msh_bank_ram_11_rd_data),
        .rd_valid(msh_bank_ram_11_rd_valid),
        .init_done(msh_bank_ram_11_init_done),
        .ecc_uncor_err(msh_bank_ram_11_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_11_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_11_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_11_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_11_to_ctl),
        .mem_ls_enter(msh_bank_ram_11_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_12(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_12_adr),
        .rd_en(msh_bank_ram_12_rd_en),
        .wr_en(msh_bank_ram_12_wr_en),
        .wr_data(msh_bank_ram_12_wr_data),
        .rd_data(msh_bank_ram_12_rd_data),
        .rd_valid(msh_bank_ram_12_rd_valid),
        .init_done(msh_bank_ram_12_init_done),
        .ecc_uncor_err(msh_bank_ram_12_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_12_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_12_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_12_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_12_to_ctl),
        .mem_ls_enter(msh_bank_ram_12_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_13(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_13_adr),
        .rd_en(msh_bank_ram_13_rd_en),
        .wr_en(msh_bank_ram_13_wr_en),
        .wr_data(msh_bank_ram_13_wr_data),
        .rd_data(msh_bank_ram_13_rd_data),
        .rd_valid(msh_bank_ram_13_rd_valid),
        .init_done(msh_bank_ram_13_init_done),
        .ecc_uncor_err(msh_bank_ram_13_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_13_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_13_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_13_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_13_to_ctl),
        .mem_ls_enter(msh_bank_ram_13_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_14(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_14_adr),
        .rd_en(msh_bank_ram_14_rd_en),
        .wr_en(msh_bank_ram_14_wr_en),
        .wr_data(msh_bank_ram_14_wr_data),
        .rd_data(msh_bank_ram_14_rd_data),
        .rd_valid(msh_bank_ram_14_rd_valid),
        .init_done(msh_bank_ram_14_init_done),
        .ecc_uncor_err(msh_bank_ram_14_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_14_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_14_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_14_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_14_to_ctl),
        .mem_ls_enter(msh_bank_ram_14_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_15(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_15_adr),
        .rd_en(msh_bank_ram_15_rd_en),
        .wr_en(msh_bank_ram_15_wr_en),
        .wr_data(msh_bank_ram_15_wr_data),
        .rd_data(msh_bank_ram_15_rd_data),
        .rd_valid(msh_bank_ram_15_rd_valid),
        .init_done(msh_bank_ram_15_init_done),
        .ecc_uncor_err(msh_bank_ram_15_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_15_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_15_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_15_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_15_to_ctl),
        .mem_ls_enter(msh_bank_ram_15_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_2(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_2_adr),
        .rd_en(msh_bank_ram_2_rd_en),
        .wr_en(msh_bank_ram_2_wr_en),
        .wr_data(msh_bank_ram_2_wr_data),
        .rd_data(msh_bank_ram_2_rd_data),
        .rd_valid(msh_bank_ram_2_rd_valid),
        .init_done(msh_bank_ram_2_init_done),
        .ecc_uncor_err(msh_bank_ram_2_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_2_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_2_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_2_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_2_to_ctl),
        .mem_ls_enter(msh_bank_ram_2_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_3(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_3_adr),
        .rd_en(msh_bank_ram_3_rd_en),
        .wr_en(msh_bank_ram_3_wr_en),
        .wr_data(msh_bank_ram_3_wr_data),
        .rd_data(msh_bank_ram_3_rd_data),
        .rd_valid(msh_bank_ram_3_rd_valid),
        .init_done(msh_bank_ram_3_init_done),
        .ecc_uncor_err(msh_bank_ram_3_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_3_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_3_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_3_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_3_to_ctl),
        .mem_ls_enter(msh_bank_ram_3_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_4(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_4_adr),
        .rd_en(msh_bank_ram_4_rd_en),
        .wr_en(msh_bank_ram_4_wr_en),
        .wr_data(msh_bank_ram_4_wr_data),
        .rd_data(msh_bank_ram_4_rd_data),
        .rd_valid(msh_bank_ram_4_rd_valid),
        .init_done(msh_bank_ram_4_init_done),
        .ecc_uncor_err(msh_bank_ram_4_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_4_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_4_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_4_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_4_to_ctl),
        .mem_ls_enter(msh_bank_ram_4_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_5(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_5_adr),
        .rd_en(msh_bank_ram_5_rd_en),
        .wr_en(msh_bank_ram_5_wr_en),
        .wr_data(msh_bank_ram_5_wr_data),
        .rd_data(msh_bank_ram_5_rd_data),
        .rd_valid(msh_bank_ram_5_rd_valid),
        .init_done(msh_bank_ram_5_init_done),
        .ecc_uncor_err(msh_bank_ram_5_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_5_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_5_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_5_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_5_to_ctl),
        .mem_ls_enter(msh_bank_ram_5_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_6(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_6_adr),
        .rd_en(msh_bank_ram_6_rd_en),
        .wr_en(msh_bank_ram_6_wr_en),
        .wr_data(msh_bank_ram_6_wr_data),
        .rd_data(msh_bank_ram_6_rd_data),
        .rd_valid(msh_bank_ram_6_rd_valid),
        .init_done(msh_bank_ram_6_init_done),
        .ecc_uncor_err(msh_bank_ram_6_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_6_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_6_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_6_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_6_to_ctl),
        .mem_ls_enter(msh_bank_ram_6_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_7(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_7_adr),
        .rd_en(msh_bank_ram_7_rd_en),
        .wr_en(msh_bank_ram_7_wr_en),
        .wr_data(msh_bank_ram_7_wr_data),
        .rd_data(msh_bank_ram_7_rd_data),
        .rd_valid(msh_bank_ram_7_rd_valid),
        .init_done(msh_bank_ram_7_init_done),
        .ecc_uncor_err(msh_bank_ram_7_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_7_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_7_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_7_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_7_to_ctl),
        .mem_ls_enter(msh_bank_ram_7_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_8(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_8_adr),
        .rd_en(msh_bank_ram_8_rd_en),
        .wr_en(msh_bank_ram_8_wr_en),
        .wr_data(msh_bank_ram_8_wr_data),
        .rd_data(msh_bank_ram_8_rd_data),
        .rd_valid(msh_bank_ram_8_rd_valid),
        .init_done(msh_bank_ram_8_init_done),
        .ecc_uncor_err(msh_bank_ram_8_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_8_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_8_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_8_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_8_to_ctl),
        .mem_ls_enter(msh_bank_ram_8_mem_ls_enter)
);

mby_mem_msh_bank_ram_shell_4096x133  mby_mem_msh_bank_ram_shell_4096x133_9(
        .clk(clk),
        .reset_n(reset_n),
        .adr(msh_bank_ram_9_adr),
        .rd_en(msh_bank_ram_9_rd_en),
        .wr_en(msh_bank_ram_9_wr_en),
        .wr_data(msh_bank_ram_9_wr_data),
        .rd_data(msh_bank_ram_9_rd_data),
        .rd_valid(msh_bank_ram_9_rd_valid),
        .init_done(msh_bank_ram_9_init_done),
        .ecc_uncor_err(msh_bank_ram_9_ecc_uncor_err),
        .msh_msh_bank_ram_from_mem(msh_msh_bank_ram_9_from_mem),
        .msh_msh_bank_ram_to_mem(msh_msh_bank_ram_9_to_mem),
        .msh_msh_bank_ram_from_ctl(msh_msh_bank_ram_9_from_ctl),
        .msh_msh_bank_ram_to_ctl(msh_msh_bank_ram_9_to_ctl),
        .mem_ls_enter(msh_bank_ram_9_mem_ls_enter)
);

msh_shell_ctl  #( // Parameters
    .MEM_CSR_RD_DATA_SAMPLE(MEM_CSR_RD_DATA_SAMPLE),
    .INT_FROM_STATUS(INT_FROM_STATUS),
    .INT_ON_CORR_ECC(INT_ON_CORR_ECC)
) msh_shell_ctl(
        .clk(clk),
        .reset_n(reset_n),
        .unified_regs_rd(unified_regs_rd),
        .unified_regs_wr_data(unified_regs_wr_data),
        .unified_regs_rd_data(unified_regs_rd_data),
        .unified_regs_ack(unified_regs_ack),
        .MSH_ECC_COR_ERR_reg_sel(MSH_ECC_COR_ERR_reg_sel),
        .MSH_ECC_UNCOR_ERR_reg_sel(MSH_ECC_UNCOR_ERR_reg_sel),
        .MSH_BANK_RAM_0_CFG_reg_sel(MSH_BANK_RAM_0_CFG_reg_sel),
        .MSH_BANK_RAM_0_STATUS_reg_sel(MSH_BANK_RAM_0_STATUS_reg_sel),
        .MSH_BANK_RAM_1_CFG_reg_sel(MSH_BANK_RAM_1_CFG_reg_sel),
        .MSH_BANK_RAM_1_STATUS_reg_sel(MSH_BANK_RAM_1_STATUS_reg_sel),
        .MSH_BANK_RAM_2_CFG_reg_sel(MSH_BANK_RAM_2_CFG_reg_sel),
        .MSH_BANK_RAM_2_STATUS_reg_sel(MSH_BANK_RAM_2_STATUS_reg_sel),
        .MSH_BANK_RAM_3_CFG_reg_sel(MSH_BANK_RAM_3_CFG_reg_sel),
        .MSH_BANK_RAM_3_STATUS_reg_sel(MSH_BANK_RAM_3_STATUS_reg_sel),
        .MSH_BANK_RAM_4_CFG_reg_sel(MSH_BANK_RAM_4_CFG_reg_sel),
        .MSH_BANK_RAM_4_STATUS_reg_sel(MSH_BANK_RAM_4_STATUS_reg_sel),
        .MSH_BANK_RAM_5_CFG_reg_sel(MSH_BANK_RAM_5_CFG_reg_sel),
        .MSH_BANK_RAM_5_STATUS_reg_sel(MSH_BANK_RAM_5_STATUS_reg_sel),
        .MSH_BANK_RAM_6_CFG_reg_sel(MSH_BANK_RAM_6_CFG_reg_sel),
        .MSH_BANK_RAM_6_STATUS_reg_sel(MSH_BANK_RAM_6_STATUS_reg_sel),
        .MSH_BANK_RAM_7_CFG_reg_sel(MSH_BANK_RAM_7_CFG_reg_sel),
        .MSH_BANK_RAM_7_STATUS_reg_sel(MSH_BANK_RAM_7_STATUS_reg_sel),
        .MSH_BANK_RAM_8_CFG_reg_sel(MSH_BANK_RAM_8_CFG_reg_sel),
        .MSH_BANK_RAM_8_STATUS_reg_sel(MSH_BANK_RAM_8_STATUS_reg_sel),
        .MSH_BANK_RAM_9_CFG_reg_sel(MSH_BANK_RAM_9_CFG_reg_sel),
        .MSH_BANK_RAM_9_STATUS_reg_sel(MSH_BANK_RAM_9_STATUS_reg_sel),
        .MSH_BANK_RAM_10_CFG_reg_sel(MSH_BANK_RAM_10_CFG_reg_sel),
        .MSH_BANK_RAM_10_STATUS_reg_sel(MSH_BANK_RAM_10_STATUS_reg_sel),
        .MSH_BANK_RAM_11_CFG_reg_sel(MSH_BANK_RAM_11_CFG_reg_sel),
        .MSH_BANK_RAM_11_STATUS_reg_sel(MSH_BANK_RAM_11_STATUS_reg_sel),
        .MSH_BANK_RAM_12_CFG_reg_sel(MSH_BANK_RAM_12_CFG_reg_sel),
        .MSH_BANK_RAM_12_STATUS_reg_sel(MSH_BANK_RAM_12_STATUS_reg_sel),
        .MSH_BANK_RAM_13_CFG_reg_sel(MSH_BANK_RAM_13_CFG_reg_sel),
        .MSH_BANK_RAM_13_STATUS_reg_sel(MSH_BANK_RAM_13_STATUS_reg_sel),
        .MSH_BANK_RAM_14_CFG_reg_sel(MSH_BANK_RAM_14_CFG_reg_sel),
        .MSH_BANK_RAM_14_STATUS_reg_sel(MSH_BANK_RAM_14_STATUS_reg_sel),
        .MSH_BANK_RAM_15_CFG_reg_sel(MSH_BANK_RAM_15_CFG_reg_sel),
        .MSH_BANK_RAM_15_STATUS_reg_sel(MSH_BANK_RAM_15_STATUS_reg_sel),
        .msh_msh_bank_ram_0_to_ctl(msh_msh_bank_ram_0_to_ctl),
        .msh_msh_bank_ram_0_from_ctl(msh_msh_bank_ram_0_from_ctl),
        .msh_msh_bank_ram_1_to_ctl(msh_msh_bank_ram_1_to_ctl),
        .msh_msh_bank_ram_1_from_ctl(msh_msh_bank_ram_1_from_ctl),
        .msh_msh_bank_ram_2_to_ctl(msh_msh_bank_ram_2_to_ctl),
        .msh_msh_bank_ram_2_from_ctl(msh_msh_bank_ram_2_from_ctl),
        .msh_msh_bank_ram_3_to_ctl(msh_msh_bank_ram_3_to_ctl),
        .msh_msh_bank_ram_3_from_ctl(msh_msh_bank_ram_3_from_ctl),
        .msh_msh_bank_ram_4_to_ctl(msh_msh_bank_ram_4_to_ctl),
        .msh_msh_bank_ram_4_from_ctl(msh_msh_bank_ram_4_from_ctl),
        .msh_msh_bank_ram_5_to_ctl(msh_msh_bank_ram_5_to_ctl),
        .msh_msh_bank_ram_5_from_ctl(msh_msh_bank_ram_5_from_ctl),
        .msh_msh_bank_ram_6_to_ctl(msh_msh_bank_ram_6_to_ctl),
        .msh_msh_bank_ram_6_from_ctl(msh_msh_bank_ram_6_from_ctl),
        .msh_msh_bank_ram_7_to_ctl(msh_msh_bank_ram_7_to_ctl),
        .msh_msh_bank_ram_7_from_ctl(msh_msh_bank_ram_7_from_ctl),
        .msh_msh_bank_ram_8_to_ctl(msh_msh_bank_ram_8_to_ctl),
        .msh_msh_bank_ram_8_from_ctl(msh_msh_bank_ram_8_from_ctl),
        .msh_msh_bank_ram_9_to_ctl(msh_msh_bank_ram_9_to_ctl),
        .msh_msh_bank_ram_9_from_ctl(msh_msh_bank_ram_9_from_ctl),
        .msh_msh_bank_ram_10_to_ctl(msh_msh_bank_ram_10_to_ctl),
        .msh_msh_bank_ram_10_from_ctl(msh_msh_bank_ram_10_from_ctl),
        .msh_msh_bank_ram_11_to_ctl(msh_msh_bank_ram_11_to_ctl),
        .msh_msh_bank_ram_11_from_ctl(msh_msh_bank_ram_11_from_ctl),
        .msh_msh_bank_ram_12_to_ctl(msh_msh_bank_ram_12_to_ctl),
        .msh_msh_bank_ram_12_from_ctl(msh_msh_bank_ram_12_from_ctl),
        .msh_msh_bank_ram_13_to_ctl(msh_msh_bank_ram_13_to_ctl),
        .msh_msh_bank_ram_13_from_ctl(msh_msh_bank_ram_13_from_ctl),
        .msh_msh_bank_ram_14_to_ctl(msh_msh_bank_ram_14_to_ctl),
        .msh_msh_bank_ram_14_from_ctl(msh_msh_bank_ram_14_from_ctl),
        .msh_msh_bank_ram_15_to_ctl(msh_msh_bank_ram_15_to_ctl),
        .msh_msh_bank_ram_15_from_ctl(msh_msh_bank_ram_15_from_ctl),
        .msh_ecc_int(msh_ecc_int),
        .msh_init_done(msh_init_done)
);


endmodule

