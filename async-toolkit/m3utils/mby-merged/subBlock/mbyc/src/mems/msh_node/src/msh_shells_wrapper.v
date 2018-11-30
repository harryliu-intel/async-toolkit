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
   input                                          MSH_BANK_RAM_1_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_1_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_2_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_2_STATUS_reg_sel           ,   
   input                                          MSH_BANK_RAM_3_CFG_reg_sel              ,   
   input                                          MSH_BANK_RAM_3_STATUS_reg_sel           ,   
   input                                          MSH_ECC_COR_ERR_reg_sel                 ,   
   input                                          MSH_ECC_UNCOR_ERR_reg_sel               ,   
   input                                          clk                                     ,   
   input                                 [12-1:0] msh_bank_ram_0_adr                      ,   
   input                                          msh_bank_ram_0_mem_ls_enter             ,   
   input                                          msh_bank_ram_0_rd_en                    ,   
   input                                [552-1:0] msh_bank_ram_0_wr_data                  ,   
   input                                          msh_bank_ram_0_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_1_adr                      ,   
   input                                          msh_bank_ram_1_mem_ls_enter             ,   
   input                                          msh_bank_ram_1_rd_en                    ,   
   input                                [552-1:0] msh_bank_ram_1_wr_data                  ,   
   input                                          msh_bank_ram_1_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_2_adr                      ,   
   input                                          msh_bank_ram_2_mem_ls_enter             ,   
   input                                          msh_bank_ram_2_rd_en                    ,   
   input                                [552-1:0] msh_bank_ram_2_wr_data                  ,   
   input                                          msh_bank_ram_2_wr_en                    ,   
   input                                 [12-1:0] msh_bank_ram_3_adr                      ,   
   input                                          msh_bank_ram_3_mem_ls_enter             ,   
   input                                          msh_bank_ram_3_rd_en                    ,   
   input                                [552-1:0] msh_bank_ram_3_wr_data                  ,   
   input                                          msh_bank_ram_3_wr_en                    ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_0_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_1_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_2_from_mem             ,   
   input [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0] msh_msh_bank_ram_3_from_mem             ,   
   input                                          reset_n                                 ,   
   input                                          unified_regs_rd                         ,   
   input                                   [31:0] unified_regs_wr_data                    ,

// Module outputs

  output                                          msh_bank_ram_0_ecc_uncor_err            ,   
  output                                          msh_bank_ram_0_init_done                ,   
  output                                [552-1:0] msh_bank_ram_0_rd_data                  ,   
  output                                          msh_bank_ram_0_rd_valid                 ,   
  output                                          msh_bank_ram_1_ecc_uncor_err            ,   
  output                                          msh_bank_ram_1_init_done                ,   
  output                                [552-1:0] msh_bank_ram_1_rd_data                  ,   
  output                                          msh_bank_ram_1_rd_valid                 ,   
  output                                          msh_bank_ram_2_ecc_uncor_err            ,   
  output                                          msh_bank_ram_2_init_done                ,   
  output                                [552-1:0] msh_bank_ram_2_rd_data                  ,   
  output                                          msh_bank_ram_2_rd_valid                 ,   
  output                                          msh_bank_ram_3_ecc_uncor_err            ,   
  output                                          msh_bank_ram_3_init_done                ,   
  output                                [552-1:0] msh_bank_ram_3_rd_data                  ,   
  output                                          msh_bank_ram_3_rd_valid                 ,   
  output                                          msh_ecc_int                             ,   
  output                                          msh_init_done                           ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_0_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_1_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_2_to_mem               ,   
  output [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_3_to_mem               ,   
  output                                          unified_regs_ack                        ,   
  output                                   [31:0] unified_regs_rd_data                          
);

// Module wires

    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_0_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_0_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_1_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_1_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_2_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_2_to_ctl               ;   
    wire [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0] msh_msh_bank_ram_3_from_ctl             ;   
    wire [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0] msh_msh_bank_ram_3_to_ctl              ;

genvar iter;


// Instances

mby_mem_msh_bank_ram_shell_4096x552  mby_mem_msh_bank_ram_shell_4096x552_0(
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

mby_mem_msh_bank_ram_shell_4096x552  mby_mem_msh_bank_ram_shell_4096x552_1(
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

mby_mem_msh_bank_ram_shell_4096x552  mby_mem_msh_bank_ram_shell_4096x552_2(
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

mby_mem_msh_bank_ram_shell_4096x552  mby_mem_msh_bank_ram_shell_4096x552_3(
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
        .msh_msh_bank_ram_0_to_ctl(msh_msh_bank_ram_0_to_ctl),
        .msh_msh_bank_ram_0_from_ctl(msh_msh_bank_ram_0_from_ctl),
        .msh_msh_bank_ram_1_to_ctl(msh_msh_bank_ram_1_to_ctl),
        .msh_msh_bank_ram_1_from_ctl(msh_msh_bank_ram_1_from_ctl),
        .msh_msh_bank_ram_2_to_ctl(msh_msh_bank_ram_2_to_ctl),
        .msh_msh_bank_ram_2_from_ctl(msh_msh_bank_ram_2_from_ctl),
        .msh_msh_bank_ram_3_to_ctl(msh_msh_bank_ram_3_to_ctl),
        .msh_msh_bank_ram_3_from_ctl(msh_msh_bank_ram_3_from_ctl),
        .msh_ecc_int(msh_ecc_int),
        .msh_init_done(msh_init_done)
);


endmodule

