//------------------------------------------------------------------------------
///  INTEL TOP SECRET

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
//
//------------------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////
//
//              Automated Memory Shells Controller Creater
//
//      Created by solson with create_memories script version 2.40 on NA
//
//                          Logical File Details
//
//              
//                      Author's name   : Olson, Steve
//                      Author's email  : steve.olson@intel.com
//                      Commited on     : Tue Dec 18 08:34:10 2018 -0800
//                      Commit tag      : 
//                      Hash            : d72ff1d74c33bd8ab161f67eb939d8a0ce81dbf0
//
//////////////////////////////////////////////////////////////////////
`include        "msh_mem.def"
module  msh_shell_ctl   #(
                parameter       MEM_RM_WIDTH                    =       `MBY_MEM_RM_WIDTH                ,
                parameter       MEM_DBG_RD_ADR_WIDTH            =       `MBY_MEM_DBG_RD_ADR_WIDTH        ,
                parameter       MEM_DBG_DW_SEL_WIDTH            =       `MBY_MEM_DBG_DW_SEL_WIDTH       ,
                parameter       MEM_GEN_ECC_INST_NUM            =       `MBY_MEM_GEN_ECC_INST_NUM       ,
                parameter       MEM_CSR_RD_DATA_SAMPLE          =       0                                       ,
                parameter       INT_ON_CORR_ECC                 =       0                                       ,
                parameter       INT_FROM_STATUS                 =       1                                       ,
                parameter       MEM_GEN_ECC_INST_NUM_WIDTH      =       $clog2(MEM_GEN_ECC_INST_NUM)     
)(
        input                           clk,
        input                           reset_n,
// CSR Interface
        input                           unified_regs_rd,
        input           [31:0]          unified_regs_wr_data,
        output  logic   [31:0]          unified_regs_rd_data,
        output  logic                   unified_regs_ack,
        input                           MSH_ECC_COR_ERR_reg_sel,
        input                           MSH_ECC_UNCOR_ERR_reg_sel,
        input                           MSH_BANK_RAM_0_CFG_reg_sel,
        input                           MSH_BANK_RAM_0_STATUS_reg_sel,
        input                           MSH_BANK_RAM_1_CFG_reg_sel,
        input                           MSH_BANK_RAM_1_STATUS_reg_sel,
        input                           MSH_BANK_RAM_2_CFG_reg_sel,
        input                           MSH_BANK_RAM_2_STATUS_reg_sel,
        input                           MSH_BANK_RAM_3_CFG_reg_sel,
        input                           MSH_BANK_RAM_3_STATUS_reg_sel,
// Interface for shell: msh_bank_ram_0
        input           [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0]                        msh_msh_bank_ram_0_to_ctl,
        output  wire    [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0]                      msh_msh_bank_ram_0_from_ctl,
// Interface for shell: msh_bank_ram_1
        input           [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0]                        msh_msh_bank_ram_1_to_ctl,
        output  wire    [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0]                      msh_msh_bank_ram_1_from_ctl,
// Interface for shell: msh_bank_ram_2
        input           [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0]                        msh_msh_bank_ram_2_to_ctl,
        output  wire    [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0]                      msh_msh_bank_ram_2_from_ctl,
// Interface for shell: msh_bank_ram_3
        input           [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0]                        msh_msh_bank_ram_3_to_ctl,
        output  wire    [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0]                      msh_msh_bank_ram_3_from_ctl,
//      Global Indications
        output  reg                     msh_ecc_int,
        output  wire                    msh_init_done
);


//      MEM_CFG_CSRs


`ifndef MBY_MGM_EMU_FPGA;          
  `ifdef INTEL_FPGA            
     `define    MBY_MGM_EMU_FPGA      
  `else                        
     `ifdef INTEL_EMULATION    
        `define  MBY_MGM_EMU_FPGA   
     `endif                       
  `endif                       
`endif                         
        reg     [3:0] mem_cfg_ecc_en;
        reg     [3:0] mem_cfg_ecc_invert_1;
        reg     [3:0] mem_cfg_ecc_invert_2;
        reg     [3:0] mem_cfg_ls_force;
        reg     [3:0] mem_cfg_ls_bypass;
        reg     [3:0] mem_cfg_mask_int;
        reg     [3:0] mem_cfg_fix_cnt;
        reg     [3:0] mem_cfg_err_cnt;
        reg     [3:0] mem_cfg_rme;
        reg     [3:0] mem_cfg_tcam_check_err_dis;
        reg     [3:0] mem_cfg_tcam_update_dis;
        reg     [MEM_RM_WIDTH-1:0] mem_cfg_rm[3:0];
        reg     [MEM_GEN_ECC_INST_NUM_WIDTH-1:0] mem_cfg_gen_ecc_inst_num[3:0];
        reg     [3:0] mem_cfg_pwren_b;
        logic   [3:0] mem_cfg_reg_sel;
always_comb 
 begin 
        mem_cfg_reg_sel[0]      = MSH_BANK_RAM_0_CFG_reg_sel;
        mem_cfg_reg_sel[1]      = MSH_BANK_RAM_1_CFG_reg_sel;
        mem_cfg_reg_sel[2]      = MSH_BANK_RAM_2_CFG_reg_sel;
        mem_cfg_reg_sel[3]      = MSH_BANK_RAM_3_CFG_reg_sel;
  `ifdef INTEL_EMULATION 
      `ifndef MBY_MGM_EMU_DO_ECC_MEM_DBG 
        mem_cfg_reg_sel =  0;
      `endif
   `endif
   end
 

        always_ff @(posedge clk or negedge reset_n)
                if (!reset_n) begin
  `ifdef INTEL_EMULATION 
      `ifndef MBY_MGM_EMU_DO_ECC_MEM_DBG 
                        mem_cfg_ecc_en[3:0]             <= 0 ;
      `else
                        mem_cfg_ecc_en[3:0]             <= {4{1'b1}};
      `endif
   `else
                        mem_cfg_ecc_en[3:0]             <= {4{1'b1}};
  `endif
                        mem_cfg_ecc_invert_1[3:0]       <= {4{1'b0}};
                        mem_cfg_ecc_invert_2[3:0]       <= {4{1'b0}};
                        mem_cfg_ls_force[3:0]           <= {4{1'b0}};
                        mem_cfg_ls_bypass[3:0]          <= {4{1'b1}};
                        mem_cfg_mask_int[3:0]           <= {4{1'b0}};
                        mem_cfg_fix_cnt[3:0]            <= {4{1'b1}};
                        mem_cfg_pwren_b[3:0]            <= {4{1'b0}};
                        mem_cfg_err_cnt[3:0]            <= {4{1'b1}};
                        mem_cfg_rme[3:0]                <= {4{1'b0}};
                        mem_cfg_tcam_check_err_dis[3:0]         <= {4{1'b0}};
                        mem_cfg_tcam_update_dis[3:0]            <= {4{1'b0}};
                        for (int i=0;i<4;i++) begin
                                mem_cfg_rm[i][MEM_RM_WIDTH-1:0] <= {{(MEM_RM_WIDTH-2){1'b0}},2'h2};
                                mem_cfg_gen_ecc_inst_num[i][MEM_GEN_ECC_INST_NUM_WIDTH-1:0] <= {(MEM_GEN_ECC_INST_NUM_WIDTH){1'b0}};
                        end
                end
                else begin
                        for (int i=0;i<4;i++)
                                if (mem_cfg_reg_sel[i] && (!unified_regs_rd)) begin
  `ifdef INTEL_EMULATION 
      `ifndef MBY_MGM_EMU_DO_ECC_MEM_DBG 
                                        mem_cfg_ecc_en[i]                               <= 0 ;
      `else
                                        mem_cfg_ecc_en[i]                               <= unified_regs_wr_data[0];
      `endif
   `else
                                        mem_cfg_ecc_en[i]                               <= unified_regs_wr_data[0];
  `endif
                                        mem_cfg_ecc_invert_1[i]                         <= unified_regs_wr_data[1];
                                        mem_cfg_ecc_invert_2[i]                         <= unified_regs_wr_data[2];
                                        mem_cfg_ls_force[i]                             <= unified_regs_wr_data[3];
                                        mem_cfg_ls_bypass[i]                            <= unified_regs_wr_data[4];
                                        mem_cfg_mask_int[i]                             <= unified_regs_wr_data[5];
                                        mem_cfg_fix_cnt[i]                              <= unified_regs_wr_data[8];
                                        mem_cfg_err_cnt[i]                              <= unified_regs_wr_data[9];
                                        mem_cfg_tcam_check_err_dis[i]                           <= unified_regs_wr_data[10];
                                        mem_cfg_tcam_update_dis[i]                              <= unified_regs_wr_data[11];
                                        mem_cfg_rme[i]                                  <= unified_regs_wr_data[12];
                                        mem_cfg_pwren_b[i]                               <= unified_regs_wr_data[13];
                                        mem_cfg_rm[i][MEM_RM_WIDTH-1:0]         <= unified_regs_wr_data[16+:MEM_RM_WIDTH];
                                        mem_cfg_gen_ecc_inst_num[i][MEM_GEN_ECC_INST_NUM_WIDTH-1:0]     <= unified_regs_wr_data[31-:MEM_GEN_ECC_INST_NUM_WIDTH];
                                end
                end

//      MEM_STAT_CSRs


        wire    [3:0] mem_init_done;
        wire    [3:0] mem_ecc_uncor_err;
        wire    [3:0] mem_ecc_cor_err;
        reg     [3:0] rd_ecc_fix;
        reg     [3:0] rd_ecc_err;

        reg     [3:0] mem_status_ecc_err;
        reg     [3:0] mem_status_ecc_fix;
        reg     [3:0] mem_status_init_done;
        reg     mem_status_global_init_done;

        wire    [3:0] mem_status_reg_sel;
        assign          mem_status_reg_sel[0]   = MSH_BANK_RAM_0_STATUS_reg_sel;
        assign          mem_status_reg_sel[1]   = MSH_BANK_RAM_1_STATUS_reg_sel;
        assign          mem_status_reg_sel[2]   = MSH_BANK_RAM_2_STATUS_reg_sel;
        assign          mem_status_reg_sel[3]   = MSH_BANK_RAM_3_STATUS_reg_sel;


        always_ff  @(posedge clk or negedge reset_n)
                if (!reset_n) begin
                        mem_status_ecc_err[3:0]         <= {4{1'b0}};
                        mem_status_ecc_fix[3:0]         <= {4{1'b0}};
                        mem_status_init_done[3:0]               <= {4{1'b0}};
                        mem_status_global_init_done             <= 1'b0;
                        rd_ecc_fix[3:0]         <= {4{1'b0}};
                        rd_ecc_err[3:0]         <= {4{1'b0}};
                end
                else begin
                        for (int i=0;i<4;i++) begin
                                        mem_status_ecc_err[i]   <= (rd_ecc_err[i])?1'b1:(unified_regs_rd&&mem_status_reg_sel[i])?1'b0:mem_status_ecc_err[i];
                                        mem_status_ecc_fix[i]   <= (rd_ecc_fix[i])?1'b1:(unified_regs_rd&&mem_status_reg_sel[i])?1'b0:mem_status_ecc_fix[i];
                                        mem_status_init_done[i] <= mem_init_done[i];
                        end
                        mem_status_global_init_done             <= &mem_init_done;
                        rd_ecc_fix                              <= mem_ecc_cor_err;
                        rd_ecc_err                              <= mem_ecc_uncor_err;
                end

//      GLOBAL_ECC_COUNTERS CSRs


        wire    [3:0] rd_ecc_err_cnt; 
        wire    [3:0] rd_ecc_fix_cnt; 
        assign   rd_ecc_err_cnt = mem_cfg_err_cnt & rd_ecc_err;
        assign   rd_ecc_fix_cnt = mem_cfg_fix_cnt & rd_ecc_fix;
        reg     [12-1:0]                msh_ecc_uncor_cnt;
        reg     [12-1:0]                msh_ecc_cor_cnt;

        always_ff @(posedge clk or negedge reset_n)
                if (!reset_n)
                   begin
                      msh_ecc_uncor_cnt <= 12'h0;
                      msh_ecc_cor_cnt   <= 12'h0;
                   end
                else
                   begin
                      msh_ecc_uncor_cnt <= (msh_ecc_uncor_cnt!=12'hFFF)?msh_ecc_uncor_cnt+{11'd0,(|rd_ecc_err_cnt)}:msh_ecc_uncor_cnt;
                      msh_ecc_cor_cnt   <= (msh_ecc_cor_cnt!=12'hFFF)?msh_ecc_cor_cnt+{11'd0,(|rd_ecc_fix_cnt)}:msh_ecc_cor_cnt;
                   end

        logic   [MEM_DBG_RD_ADR_WIDTH-1:0] mem_ecc_err_adr[3:0];
        logic   [3:0] mem_is_tcam   ;

//      DEBUG_READ CSRs


        wire    [3:0] mem_dbg_rd_ctl_done_int;
        wire    [32-1:0] mem_dbg_rd_data_rd_data_int[3:0];
        reg     [3:0] mem_dbg_rd_ctl_rd_en;
        reg     [MEM_DBG_RD_ADR_WIDTH-1:0] mem_dbg_rd_ctl_adr[3:0];
        reg     [MEM_DBG_DW_SEL_WIDTH-1:0] mem_dbg_rd_ctl_dw_sel[3:0];
        reg     [32-1:0] mem_dbg_rd_data_rd_data[3:0];
        reg     [3:0] mem_dbg_rd_ctl_done;

        wire    [3:0] mem_dbg_rd_ctl_reg_sel;
        wire    [3:0] mem_dbg_rd_data_reg_sel;
        assign          mem_dbg_rd_ctl_reg_sel[0]       = 1'b0;
        assign          mem_dbg_rd_data_reg_sel[0]      = 1'b0;
        assign          mem_dbg_rd_ctl_reg_sel[1]       = 1'b0;
        assign          mem_dbg_rd_data_reg_sel[1]      = 1'b0;
        assign          mem_dbg_rd_ctl_reg_sel[2]       = 1'b0;
        assign          mem_dbg_rd_data_reg_sel[2]      = 1'b0;
        assign          mem_dbg_rd_ctl_reg_sel[3]       = 1'b0;
        assign          mem_dbg_rd_data_reg_sel[3]      = 1'b0;

        always_ff @(posedge clk or negedge reset_n)
                if (!reset_n)
                   begin
                        mem_dbg_rd_ctl_rd_en                            <= 0;
                        mem_dbg_rd_ctl_done                             <= {(4){1'b1}};
                        for (int i=0;i<4;i++)
                                begin
                                        mem_dbg_rd_ctl_adr[i][MEM_DBG_RD_ADR_WIDTH-1:0]         <= {(MEM_DBG_RD_ADR_WIDTH){1'b0}};
                                        mem_dbg_rd_ctl_dw_sel[i][MEM_DBG_DW_SEL_WIDTH-1:0]      <= {(MEM_DBG_DW_SEL_WIDTH){1'b0}};
                                end
                   end
                else
                   begin
                        for (int i=0;i<4;i++)
                                if (mem_dbg_rd_ctl_reg_sel[i] && (!unified_regs_rd) && mem_dbg_rd_ctl_done[i])
                                        begin
                                                mem_dbg_rd_ctl_rd_en[i]                                 <= unified_regs_wr_data[30];
                                                mem_dbg_rd_ctl_adr[i][MEM_DBG_RD_ADR_WIDTH-1:0]         <= unified_regs_wr_data[0+:MEM_DBG_RD_ADR_WIDTH];
                                                mem_dbg_rd_ctl_dw_sel[i][MEM_DBG_DW_SEL_WIDTH-1:0]      <= unified_regs_wr_data[MEM_DBG_RD_ADR_WIDTH+:MEM_DBG_DW_SEL_WIDTH];
                                                mem_dbg_rd_ctl_done[i]                                  <= (unified_regs_wr_data[30]) ? 1'b0 : mem_dbg_rd_ctl_done[i];
                                        end
                                else if (mem_dbg_rd_ctl_done_int[i])
                                        begin
                                                mem_dbg_rd_ctl_rd_en[i]                                 <= 1'b0;
                                                mem_dbg_rd_ctl_done[i]                                  <= 1'b1;
                                        end
                   end

        always_ff @(posedge clk or negedge reset_n)
                if (!reset_n)
                   begin
                        for (int i=0;i<4;i++)
                                mem_dbg_rd_data_rd_data[i][31:0]        <= 32'h0;
                   end
                else
                   begin
                        for (int i=0;i<4;i++)
                                if (mem_dbg_rd_ctl_done_int[i])
                                        mem_dbg_rd_data_rd_data[i][31:0]        <= mem_dbg_rd_data_rd_data_int[i][31:0];
                   end

//      GLOBAL Indicatiions


        wire    [3:0]   rd_ecc_int;
        generate
                if ((INT_ON_CORR_ECC==0) && (INT_FROM_STATUS==0)) begin: UNCOR_ERR_PULSE
                        assign                          rd_ecc_int[3:0] = (~mem_cfg_mask_int) & rd_ecc_err;
                end
                else if ((INT_ON_CORR_ECC==0) && (INT_FROM_STATUS!=0)) begin: UNCOR_ERR_LVL
                        assign                          rd_ecc_int[3:0] = (~mem_cfg_mask_int) & mem_status_ecc_err;
                end
                else if ((INT_ON_CORR_ECC!=0) && (INT_FROM_STATUS==0)) begin: COR_AND_UNCOR_ERR_PULSE
                        assign                          rd_ecc_int[3:0] = (~mem_cfg_mask_int) & (rd_ecc_err | rd_ecc_fix);
                end
                else begin: COR_AND_UNCOR_ERR_LVL
                        assign                          rd_ecc_int[3:0] = (~mem_cfg_mask_int) & (mem_status_ecc_err | mem_status_ecc_fix);
                end
        endgenerate
        assign          msh_init_done   = mem_status_global_init_done;
        always_ff  @(posedge clk or negedge reset_n)
                if (!reset_n) begin
                        msh_ecc_int             <= 1'b0;
                end
                else begin
                        msh_ecc_int             <= |rd_ecc_int;
                end

        wire    [7-1:0] rd_reg_sel;
        assign                  rd_reg_sel      = {unified_regs_rd, MSH_ECC_COR_ERR_reg_sel, MSH_ECC_UNCOR_ERR_reg_sel, |mem_dbg_rd_ctl_reg_sel, |mem_dbg_rd_data_reg_sel, |mem_status_reg_sel, |mem_cfg_reg_sel};
        logic   [32-1:0]        unified_regs_rd_data_int;
        always_comb
                begin
                        unified_regs_rd_data_int        = 32'h0;
                        case(rd_reg_sel)
                                7'b1000001:     begin   // MEM_CFG
                                                        for (int i=0;i<4;i++)
                                                                if (!mem_is_tcam[i]) begin // not tcam
                                                                        if (mem_cfg_reg_sel[i])
                                                                                unified_regs_rd_data_int        =  {
                                                                                                                mem_cfg_gen_ecc_inst_num[i],
                                                                                                                5'h0,
                                                                                                                mem_cfg_rm[i],
                                                                                                                2'h0,
                                                                                                                mem_cfg_pwren_b[i],
                                                                                                                mem_cfg_rme[i],
                                                                                                                2'h0,
                                                                                                                mem_cfg_err_cnt[i],
                                                                                                                mem_cfg_fix_cnt[i],
                                                                                                                2'h0,
                                                                                                                mem_cfg_mask_int[i],
                                                                                                                mem_cfg_ls_bypass[i],
                                                                                                                mem_cfg_ls_force[i],
                                                                                                                mem_cfg_ecc_invert_2[i],
                                                                                                                mem_cfg_ecc_invert_1[i],
                                                                                                                mem_cfg_ecc_en[i]
                                                                                    };
                                                                end
                                                                else begin
                                                                        if (mem_cfg_reg_sel[i])
                                                                                unified_regs_rd_data_int        =  {
                                                                                                                mem_cfg_gen_ecc_inst_num[i],
                                                                                                                5'h0,
                                                                                                                mem_cfg_rm[i],
                                                                                                                3'h0,
                                                                                                                mem_cfg_rme[i],
                                                                                                                mem_cfg_tcam_update_dis[i],
                                                                                                                mem_cfg_tcam_check_err_dis[i],
                                                                                                                mem_cfg_err_cnt[i],
                                                                                                                mem_cfg_fix_cnt[i],
                                                                                                                2'h0,
                                                                                                                mem_cfg_mask_int[i],
                                                                                                                mem_cfg_ls_bypass[i],
                                                                                                                mem_cfg_ls_force[i],
                                                                                                                mem_cfg_ecc_invert_2[i],
                                                                                                                mem_cfg_ecc_invert_1[i],
                                                                                                                mem_cfg_ecc_en[i]
                                                                                    };
                                                                end
                                end// MEM_CFG
                                7'b1000010:     begin   // MEM_STATUS
                                                        for (int i=0;i<4;i++)
                                                                if (mem_status_reg_sel[i])
                                                                        unified_regs_rd_data_int        =  {
                                                                                                        2'h0,
                                                                                                        mem_ecc_err_adr[i],
                                                                                                        8'h0,
                                                                                                        mem_status_global_init_done,
                                                                                                        mem_status_init_done[i],
                                                                                                        mem_status_ecc_fix[i],
                                                                                                        mem_status_ecc_err[i]
                                                                                                    };
                                end// MEM_STATUS
                                7'b1000100:     begin   // DBG_READ_DATA
                                                        for (int i=0;i<4;i++)
                                                                if (mem_dbg_rd_data_reg_sel[i])
                                                                        unified_regs_rd_data_int        =  mem_dbg_rd_data_rd_data[i];
                                end// DBG_READ_DATA
                                7'b1001000:     begin   // DBG_READ_CTL
                                                        for (int i=0;i<4;i++)
                                                                if (mem_dbg_rd_ctl_reg_sel[i])
                                                                        unified_regs_rd_data_int        =  {
                                                                                                        mem_dbg_rd_ctl_done[i],
                                                                                                        mem_dbg_rd_ctl_rd_en[i],
                                                                                                        4'h0,
                                                                                                        mem_dbg_rd_ctl_dw_sel[i],
                                                                                                        mem_dbg_rd_ctl_adr[i]
                                                                                                    };
                                end// DBG_READ_CTL
                                7'b1010000:     begin   // ECC_UNCOR_CNT
                                                                        unified_regs_rd_data_int        =  {20'h0, msh_ecc_uncor_cnt};
                                end// ECC_UNCOR_CNT
                                7'b1100000:     begin   // ECC_COR_CNT
                                                                        unified_regs_rd_data_int        =  {20'h0, msh_ecc_cor_cnt};
                                end// ECC_COR_CNT
                                default:        begin   // Default
                                        unified_regs_rd_data_int        = 32'h0;
                                end// Default
                        endcase
                end
        logic   unified_regs_ack_int;
        always_comb begin
                unified_regs_ack_int    = MSH_ECC_UNCOR_ERR_reg_sel || MSH_ECC_COR_ERR_reg_sel || |mem_dbg_rd_ctl_reg_sel || |mem_dbg_rd_data_reg_sel || |mem_status_reg_sel || |mem_cfg_reg_sel;
        end

                generate
                        if (MEM_CSR_RD_DATA_SAMPLE == 1) begin: RD_DATA_SAMPLE
                                always_ff @(posedge clk or negedge reset_n)
                                        if (!reset_n) begin
                                                unified_regs_ack        <= 1'b0                                 ;
                                                unified_regs_rd_data    <= {32{1'b0}}                           ;
                                        end
                                        else begin
                                                unified_regs_ack        <= unified_regs_ack_int                 ;
                                                if (unified_regs_ack_int) begin
                                                        unified_regs_rd_data    <= unified_regs_rd_data_int     ;
                                                end
                                        end
                        end
                        else begin: NO_RD_DATA_SAMPLE
                                always_comb begin
                                        unified_regs_ack                = unified_regs_ack_int;
                                        unified_regs_rd_data            = unified_regs_rd_data_int;
                                end
                        end
                endgenerate

//      TYPEDEF STRUCTS


        typedef struct packed{                                                          
                logic                                                   cfg_wr_ind      ;
                logic                                                   stat_rd_ind     ;
                logic                                                   ecc_en          ;
                logic                                                   ecc_invert_1    ;
                logic                                                   ecc_invert_2    ;
                logic   [           MEM_GEN_ECC_INST_NUM_WIDTH-1:0]     gen_ecc_inst_num;
                logic                                                   pwren_b         ;
                logic                                                   rm_e            ;
                logic   [                         MEM_RM_WIDTH-1:0]     rm              ;
                logic                                                   ls_bypass       ;
                logic                                                   ls_force        ;
                logic                                                   dbg_wr_ind      ;
                logic                                                   dbg_rd_en       ;
                logic   [                 MEM_DBG_RD_ADR_WIDTH-1:0]     dbg_adr         ;
                logic   [                 MEM_DBG_DW_SEL_WIDTH-1:0]     dbg_dw_sel      ;
                logic                                                   tcam_check_err_dis;
                logic                                                   tcam_update_dis ;
        } from_ctl_t;                                                                   
        from_ctl_t   [3:0]        from_ctl_bus    ;

        assign          from_ctl_bus[0].ecc_en          = mem_cfg_ecc_en[0];
        assign          from_ctl_bus[0].ecc_invert_1    = mem_cfg_ecc_invert_1[0];
        assign          from_ctl_bus[0].ecc_invert_2    = mem_cfg_ecc_invert_2[0];
        assign          from_ctl_bus[0].gen_ecc_inst_num= mem_cfg_gen_ecc_inst_num[0];
        assign          from_ctl_bus[0].tcam_check_err_dis= mem_cfg_tcam_check_err_dis[0];
        assign          from_ctl_bus[0].tcam_update_dis= mem_cfg_tcam_update_dis[0];
        assign          from_ctl_bus[0].pwren_b         = mem_cfg_pwren_b[0];
        assign          from_ctl_bus[0].rm_e            = mem_cfg_rme[0];
        assign          from_ctl_bus[0].rm              = mem_cfg_rm[0];
        assign          from_ctl_bus[0].ls_bypass       = mem_cfg_ls_bypass[0];
        assign          from_ctl_bus[0].ls_force        = mem_cfg_ls_force[0];
        assign          from_ctl_bus[0].dbg_rd_en       = mem_dbg_rd_ctl_rd_en[0];
        assign          from_ctl_bus[0].dbg_adr         = mem_dbg_rd_ctl_adr[0];
        assign          from_ctl_bus[0].dbg_dw_sel      = mem_dbg_rd_ctl_dw_sel[0];
        assign          from_ctl_bus[0].cfg_wr_ind      = !unified_regs_rd & mem_cfg_reg_sel[0];
        assign          from_ctl_bus[0].stat_rd_ind     = unified_regs_rd & mem_status_reg_sel[0];
        assign          from_ctl_bus[0].dbg_wr_ind      = !unified_regs_rd & mem_dbg_rd_ctl_reg_sel[0];

        assign          from_ctl_bus[1].ecc_en          = mem_cfg_ecc_en[1];
        assign          from_ctl_bus[1].ecc_invert_1    = mem_cfg_ecc_invert_1[1];
        assign          from_ctl_bus[1].ecc_invert_2    = mem_cfg_ecc_invert_2[1];
        assign          from_ctl_bus[1].gen_ecc_inst_num= mem_cfg_gen_ecc_inst_num[1];
        assign          from_ctl_bus[1].tcam_check_err_dis= mem_cfg_tcam_check_err_dis[1];
        assign          from_ctl_bus[1].tcam_update_dis= mem_cfg_tcam_update_dis[1];
        assign          from_ctl_bus[1].pwren_b         = mem_cfg_pwren_b[1];
        assign          from_ctl_bus[1].rm_e            = mem_cfg_rme[1];
        assign          from_ctl_bus[1].rm              = mem_cfg_rm[1];
        assign          from_ctl_bus[1].ls_bypass       = mem_cfg_ls_bypass[1];
        assign          from_ctl_bus[1].ls_force        = mem_cfg_ls_force[1];
        assign          from_ctl_bus[1].dbg_rd_en       = mem_dbg_rd_ctl_rd_en[1];
        assign          from_ctl_bus[1].dbg_adr         = mem_dbg_rd_ctl_adr[1];
        assign          from_ctl_bus[1].dbg_dw_sel      = mem_dbg_rd_ctl_dw_sel[1];
        assign          from_ctl_bus[1].cfg_wr_ind      = !unified_regs_rd & mem_cfg_reg_sel[1];
        assign          from_ctl_bus[1].stat_rd_ind     = unified_regs_rd & mem_status_reg_sel[1];
        assign          from_ctl_bus[1].dbg_wr_ind      = !unified_regs_rd & mem_dbg_rd_ctl_reg_sel[1];

        assign          from_ctl_bus[2].ecc_en          = mem_cfg_ecc_en[2];
        assign          from_ctl_bus[2].ecc_invert_1    = mem_cfg_ecc_invert_1[2];
        assign          from_ctl_bus[2].ecc_invert_2    = mem_cfg_ecc_invert_2[2];
        assign          from_ctl_bus[2].gen_ecc_inst_num= mem_cfg_gen_ecc_inst_num[2];
        assign          from_ctl_bus[2].tcam_check_err_dis= mem_cfg_tcam_check_err_dis[2];
        assign          from_ctl_bus[2].tcam_update_dis= mem_cfg_tcam_update_dis[2];
        assign          from_ctl_bus[2].pwren_b         = mem_cfg_pwren_b[2];
        assign          from_ctl_bus[2].rm_e            = mem_cfg_rme[2];
        assign          from_ctl_bus[2].rm              = mem_cfg_rm[2];
        assign          from_ctl_bus[2].ls_bypass       = mem_cfg_ls_bypass[2];
        assign          from_ctl_bus[2].ls_force        = mem_cfg_ls_force[2];
        assign          from_ctl_bus[2].dbg_rd_en       = mem_dbg_rd_ctl_rd_en[2];
        assign          from_ctl_bus[2].dbg_adr         = mem_dbg_rd_ctl_adr[2];
        assign          from_ctl_bus[2].dbg_dw_sel      = mem_dbg_rd_ctl_dw_sel[2];
        assign          from_ctl_bus[2].cfg_wr_ind      = !unified_regs_rd & mem_cfg_reg_sel[2];
        assign          from_ctl_bus[2].stat_rd_ind     = unified_regs_rd & mem_status_reg_sel[2];
        assign          from_ctl_bus[2].dbg_wr_ind      = !unified_regs_rd & mem_dbg_rd_ctl_reg_sel[2];

        assign          from_ctl_bus[3].ecc_en          = mem_cfg_ecc_en[3];
        assign          from_ctl_bus[3].ecc_invert_1    = mem_cfg_ecc_invert_1[3];
        assign          from_ctl_bus[3].ecc_invert_2    = mem_cfg_ecc_invert_2[3];
        assign          from_ctl_bus[3].gen_ecc_inst_num= mem_cfg_gen_ecc_inst_num[3];
        assign          from_ctl_bus[3].tcam_check_err_dis= mem_cfg_tcam_check_err_dis[3];
        assign          from_ctl_bus[3].tcam_update_dis= mem_cfg_tcam_update_dis[3];
        assign          from_ctl_bus[3].pwren_b         = mem_cfg_pwren_b[3];
        assign          from_ctl_bus[3].rm_e            = mem_cfg_rme[3];
        assign          from_ctl_bus[3].rm              = mem_cfg_rm[3];
        assign          from_ctl_bus[3].ls_bypass       = mem_cfg_ls_bypass[3];
        assign          from_ctl_bus[3].ls_force        = mem_cfg_ls_force[3];
        assign          from_ctl_bus[3].dbg_rd_en       = mem_dbg_rd_ctl_rd_en[3];
        assign          from_ctl_bus[3].dbg_adr         = mem_dbg_rd_ctl_adr[3];
        assign          from_ctl_bus[3].dbg_dw_sel      = mem_dbg_rd_ctl_dw_sel[3];
        assign          from_ctl_bus[3].cfg_wr_ind      = !unified_regs_rd & mem_cfg_reg_sel[3];
        assign          from_ctl_bus[3].stat_rd_ind     = unified_regs_rd & mem_status_reg_sel[3];
        assign          from_ctl_bus[3].dbg_wr_ind      = !unified_regs_rd & mem_dbg_rd_ctl_reg_sel[3];


        typedef struct packed{                                                          
                logic                                                   ecc_uncor_err   ;       
                logic                                                   ecc_cor_err     ;       
                logic                                                   init_done       ;       
                logic   [                                   32-1:0]     dbg_rd_data     ;       
                logic                                                   dbg_done        ;       
                logic   [                 MEM_DBG_RD_ADR_WIDTH-1:0]     ecc_err_adr     ;       
        } to_ctl_t;                                                                             
        to_ctl_t     [3:0]        to_ctl_bus      ;

                //msh_bank_ram_0
        assign          mem_ecc_uncor_err[0]            = to_ctl_bus[0].ecc_uncor_err;
        assign          mem_ecc_cor_err[0]              = to_ctl_bus[0].ecc_cor_err;
        assign          mem_dbg_rd_data_rd_data_int[0]  = to_ctl_bus[0].dbg_rd_data;
        assign          mem_dbg_rd_ctl_done_int[0]      = to_ctl_bus[0].dbg_done;
        assign          mem_ecc_err_adr[0]              = to_ctl_bus[0].ecc_err_adr;
        assign          mem_init_done[0]                = to_ctl_bus[0].init_done;
        assign          mem_is_tcam[0]                  = 1'b0;

                //msh_bank_ram_1
        assign          mem_ecc_uncor_err[1]            = to_ctl_bus[1].ecc_uncor_err;
        assign          mem_ecc_cor_err[1]              = to_ctl_bus[1].ecc_cor_err;
        assign          mem_dbg_rd_data_rd_data_int[1]  = to_ctl_bus[1].dbg_rd_data;
        assign          mem_dbg_rd_ctl_done_int[1]      = to_ctl_bus[1].dbg_done;
        assign          mem_ecc_err_adr[1]              = to_ctl_bus[1].ecc_err_adr;
        assign          mem_init_done[1]                = to_ctl_bus[1].init_done;
        assign          mem_is_tcam[1]                  = 1'b0;

                //msh_bank_ram_2
        assign          mem_ecc_uncor_err[2]            = to_ctl_bus[2].ecc_uncor_err;
        assign          mem_ecc_cor_err[2]              = to_ctl_bus[2].ecc_cor_err;
        assign          mem_dbg_rd_data_rd_data_int[2]  = to_ctl_bus[2].dbg_rd_data;
        assign          mem_dbg_rd_ctl_done_int[2]      = to_ctl_bus[2].dbg_done;
        assign          mem_ecc_err_adr[2]              = to_ctl_bus[2].ecc_err_adr;
        assign          mem_init_done[2]                = to_ctl_bus[2].init_done;
        assign          mem_is_tcam[2]                  = 1'b0;

                //msh_bank_ram_3
        assign          mem_ecc_uncor_err[3]            = to_ctl_bus[3].ecc_uncor_err;
        assign          mem_ecc_cor_err[3]              = to_ctl_bus[3].ecc_cor_err;
        assign          mem_dbg_rd_data_rd_data_int[3]  = to_ctl_bus[3].dbg_rd_data;
        assign          mem_dbg_rd_ctl_done_int[3]      = to_ctl_bus[3].dbg_done;
        assign          mem_ecc_err_adr[3]              = to_ctl_bus[3].ecc_err_adr;
        assign          mem_init_done[3]                = to_ctl_bus[3].init_done;
        assign          mem_is_tcam[3]                  = 1'b0;

//      To/FROM_CTL BUSES Generation


        assign          to_ctl_bus[0]           = msh_msh_bank_ram_0_to_ctl;
        assign          to_ctl_bus[1]           = msh_msh_bank_ram_1_to_ctl;
        assign          to_ctl_bus[2]           = msh_msh_bank_ram_2_to_ctl;
        assign          to_ctl_bus[3]           = msh_msh_bank_ram_3_to_ctl;
        assign          msh_msh_bank_ram_0_from_ctl     = from_ctl_bus[0];
        assign          msh_msh_bank_ram_1_from_ctl     = from_ctl_bus[1];
        assign          msh_msh_bank_ram_2_from_ctl     = from_ctl_bus[2];
        assign          msh_msh_bank_ram_3_from_ctl     = from_ctl_bus[3];


endmodule