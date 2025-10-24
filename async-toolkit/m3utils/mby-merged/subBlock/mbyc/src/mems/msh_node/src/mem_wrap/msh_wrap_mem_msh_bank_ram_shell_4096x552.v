// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
//                          Logical File Details
//
//              
//                      Author's name   : Mccormick, Jim
//                      Author's email  : jim.mccormick@intel.com
//                      Commited on     : Tue Oct 30 11:35:04 2018 -0700
//                      Commit tag      : 
//                      Hash            : 92f1a9b52c141ce961b1302be8bc68915dd3487e
//
//////////////////////////////////////////////////////////////////////
`include        "msh_mem.def"
module  msh_wrap_mem_msh_bank_ram_shell_4096x552 #(
                // Memory General Parameters
                parameter MEM_WIDTH                     = 42                                                                                                                            ,
                parameter MEM_DEPTH                     = 3086                                                                                                                          ,
                parameter MEM_WR_RESOLUTION             = MEM_WIDTH                                                                                                                     ,
                parameter MEM_WR_EN_WIDTH               = (MEM_WIDTH/MEM_WR_RESOLUTION)                                                                                                 ,
                parameter MEM_DELAY                     = 1                                                                                                                             ,
                parameter MEM_PST_EBB_SAMPLE            = 0                                                                                                                             ,
                parameter MEM_ADR_WIDTH                 = (MEM_DEPTH>1) ? $clog2(MEM_DEPTH) : 1                                                                                         ,
                parameter MEM_RM_WIDTH                  = `MBY_MEM_RM_WIDTH                                                                                                         ,
                parameter FROM_MEM_WIDTH                = MEM_WIDTH + 1                                                                                                                 ,
                parameter TO_MEM_WIDTH                  = 0+1+MEM_RM_WIDTH+1+1+MEM_WR_EN_WIDTH+MEM_ADR_WIDTH+MEM_WIDTH +1                                                                   ,                                       
                // FAST_CONFIG Parameters
                parameter MEM_INIT_TYPE                 = 1                                                                                                                             , // 1 - Const. Val. Init., 2 - LL, Other - No Init.
                parameter LL_INIT_OFFSET                = 1                                                                                                                             ,
                parameter LL_IS_LAST                    = 1                                                                                                                             , //LINA CHANGE
                parameter MEM_INIT_VALUE                = 0                                                                                                                             , 
                parameter MEM_INIT_VALUE_WIDTH          = $bits(MEM_INIT_VALUE)                                                                                                         ,
                parameter MEM_PROT_TYPE                 = 0                                                                                                                             ,
                parameter MEM_PROT_RESOLUTION           = MEM_WR_RESOLUTION                                                                                                             ,
                parameter MEM_PROT_INST_WIDTH           = (MEM_PROT_TYPE == 0) ? $clog2(MEM_PROT_RESOLUTION+$clog2(MEM_PROT_RESOLUTION)+1)+1 : 1                                        ,                                                                                                                               
                parameter MEM_INIT_PROT_INST_NUM        = MEM_INIT_VALUE_WIDTH/MEM_PROT_RESOLUTION                                                                                      ,    

                parameter       MEM_WIDTH_NO_SIG                        = MEM_INIT_VALUE_WIDTH                                                                                                  ,
                parameter       MEM_WR_RESOLUTION_NO_SIG                = MEM_WIDTH_NO_SIG                                                                                                      ,
                parameter       MEM_WR_RES_PROT_FRAGM                   = 1                                                                                                                     , // Memory Write Resolution Fragmentation for Protection.      
                parameter       MEM_WR_RESOLUTION_ZERO_PADDING          = (MEM_PROT_RESOLUTION * MEM_WR_RES_PROT_FRAGM) - MEM_WR_RESOLUTION_NO_SIG                                       , // Memory Write Resolution Zero Padding.
                parameter       MEM_PROT_INTERLV_LEVEL                  = 1                                                                                                                     , // Memory Protection Bits Interleaving Level: 1 - No Interleaving, 2 - Every 2 bits (grouping of even and odd bits), 3 - Every 3 bits and Etc.
                parameter       MEM_PROT_PER_GEN_INST                   = (MEM_PROT_RESOLUTION % MEM_PROT_INTERLV_LEVEL) ? ((MEM_PROT_RESOLUTION-(MEM_PROT_RESOLUTION % MEM_PROT_INTERLV_LEVEL))/MEM_PROT_INTERLV_LEVEL) + 1 : (MEM_PROT_RESOLUTION/MEM_PROT_INTERLV_LEVEL)     , // Memory Width per protection module.
                parameter       MEM_PROT_RESOLUTION_ZERO_PADDING        = (MEM_PROT_PER_GEN_INST * MEM_PROT_INTERLV_LEVEL) - MEM_PROT_RESOLUTION                                         , // Memory Protection Resolution Zero Padding.
                parameter       MEM_TOTAL_ZERO_PADDING                  = ((MEM_PROT_RESOLUTION_ZERO_PADDING * MEM_WR_RES_PROT_FRAGM) + MEM_WR_RESOLUTION_ZERO_PADDING) * MEM_WR_EN_WIDTH       , // Memory Total Zero Padding. 
                parameter       MEM_PROT_TOTAL_GEN_INST                 = MEM_PROT_INTERLV_LEVEL * MEM_WR_RES_PROT_FRAGM * MEM_WR_EN_WIDTH                                                      ,
                parameter       MEM_PROT_INST_WIDTH_NO_SIG              = (MEM_PROT_TYPE == 0) ? $clog2(MEM_PROT_PER_GEN_INST+$clog2(MEM_PROT_PER_GEN_INST)+1)+1 : (MEM_PROT_TYPE == 1) ? 1 : 0 , // Data Integrity signature width for a single chunk of protected data
                parameter       MEM_PROT_TOTAL_WIDTH                    = MEM_PROT_TOTAL_GEN_INST * MEM_PROT_INST_WIDTH_NO_SIG                                                                         , // Total width of the Data Integrity signatures in Memory line.

                parameter POWER_GATE_CAPABLE            = 0                                                                                             ,

                // FPGA Memory Parameters
                parameter FPGA_MEM_ZERO_PADDING         = (8 - (MEM_WR_RESOLUTION % 8)) % 8                                                                                             ,
                parameter FPGA_MEM_WR_RESOLUTION        = MEM_WR_RESOLUTION + FPGA_MEM_ZERO_PADDING                                                                                     ,
                parameter FPGA_MEM_WIDTH                = MEM_WR_EN_WIDTH*(FPGA_MEM_WR_RESOLUTION)                                                                                      ,
                parameter FPGA_MEM_WR_EN_WIDTH          = FPGA_MEM_WIDTH/8                                                                                                              
                // DFx Memory Parameters
               ,
                parameter MSWT_MODE                     = 0                                                                             ,
                parameter BYPASS_CLK_MUX                = 0                                                                             ,
                parameter BYPASS_MBIST_EN_SYNC          = 0                                                                             ,
                parameter WRAPPER_REDROW_ENABLE         = 0                                                                             ,
                parameter WRAPPER_COL_REPAIR            = 1                                                                             ,
                parameter NFUSEMISC_SRAM                = 20                                                                            ,
                parameter NFUSERED_SRAM                 = $clog2(MEM_WIDTH) + 1                                                         ,
                parameter TOTAL_MEMORY_INSTANCE         = 1                                                       
)(
        // Memory General Interface
        input                                                   clk                             ,
        input                                                   car_raw_lan_power_good          ,
        input           [  TO_MEM_WIDTH-1:0]                    wrap_shell_to_mem               ,
        output  wire    [FROM_MEM_WIDTH-1:0]                    wrap_shell_from_mem             
        // Memory DFx Interface 
       , 
        input                                                   fscan_mode                      ,
        input                                                   fscan_byprst_b                  ,
        input                                                   fscan_rstbypen                  ,
        input                                                   fscan_ram_init_en              ,
        input                                                   fscan_ram_init_val                ,       
        input                                                   fdfx_lbist_test_mode            ,
        input                                                   fscan_ram_rddis_b               ,
        input                                                   fscan_ram_wrdis_b               ,
        input           [TOTAL_MEMORY_INSTANCE-1:0]             fscan_ram_odis_b                ,
        input           [TOTAL_MEMORY_INSTANCE-1:0]             fscan_ram_awt_mode              ,
        input           [TOTAL_MEMORY_INSTANCE-1:0]             fscan_ram_awt_ren               ,
        input           [TOTAL_MEMORY_INSTANCE-1:0]             fscan_ram_awt_wen               ,
        input           [TOTAL_MEMORY_INSTANCE-1:0]             fscan_ram_bypsel                ,

        input                                                   fary_stm_enable                 ,
        input                                                   fary_stm_hilo                   , 
        input                                                   fary_wakeup_sram                ,
        input           [1:0]                                   fary_fwen_sram                  ,
        input                                                   fary_enblfloat_sram             ,
        input                                                   fary_ensleep_sram               ,

        input           [NFUSEMISC_SRAM-1:0]                    fary_ffuse_data_misc_sram       ,

                
        input                                                   fary_pwren_b_sram               ,
        output                                                  aary_pwren_b_sram               
);




        // Disassembling the to_mem bus
        

        logic                           shellctl_pwren_b; 
        generate
                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CAPABLE_SET
                        always_comb
                                begin
                                     shellctl_pwren_b= wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH+MEM_WR_EN_WIDTH+2+MEM_RM_WIDTH +1   +:1                     ]; 
                                end
                end
        endgenerate


        
        wire                            reset_n         = wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH+MEM_WR_EN_WIDTH+2+MEM_RM_WIDTH      +:1                     ];
        wire    [   MEM_RM_WIDTH-1:0]   mem_rm          = wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH+MEM_WR_EN_WIDTH+2                   +:MEM_RM_WIDTH          ];
        wire                            mem_rm_e        = wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH+MEM_WR_EN_WIDTH+1                   +:1                     ];
        wire                            rd_en           = wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH+MEM_WR_EN_WIDTH                     +:1                     ];
        wire    [MEM_WR_EN_WIDTH-1:0]   wr_en           = wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH                                     +:MEM_WR_EN_WIDTH       ];
        wire    [  MEM_ADR_WIDTH-1:0]   adr             = wrap_shell_to_mem[MEM_WIDTH                                                   +:MEM_ADR_WIDTH         ];
        wire    [      MEM_WIDTH-1:0]   wr_data         = wrap_shell_to_mem[0                                                           +:MEM_WIDTH             ];
                                                                  
        // Assembling the from_mem bus
        wire    [      MEM_WIDTH-1:0]   rd_data                                                                 ;         
        wire                            rd_valid                                                                ;                                                                       
        assign                          wrap_shell_from_mem[0]                  = rd_valid                      ;
        assign                          wrap_shell_from_mem[1+:MEM_WIDTH]       = rd_data                       ;

`ifdef  MBY_MSH_BEHAVE_MEMS_EXCLUDE
        `define MBY_MSH_BEHAVE_MEMS
`endif



`ifdef  MBY_MSH_BEHAVE_MEMS
        `define MBY_MSH_BEHAVE_MEMS_INSTANCE
`endif

`ifdef INTEL_SIMONLY
        `define MBY_MSH_BEHAVE_MEMS_INSTANCE
`endif



`ifdef MBY_MSH_BEHAVE_MEMS_INSTANCE

        wire    behave_mem_wr_en        = |wr_en;
        wire    behave_mem_sel          = rd_en || behave_mem_wr_en;
        
        wire    [MEM_WIDTH-1:0]         behave_mem_rd_data;
        logic   [MEM_WIDTH-1:0]         rd_data_behave_int;
        mby_mgm_1rw_behave #(
                .MEM_WIDTH              (MEM_WIDTH)                             ,
                .MEM_DEPTH              (MEM_DEPTH)                             ,
                .MEM_WR_RESOLUTION      (MEM_WR_RESOLUTION))
        behave_mem (
                .clk                    (clk)                                   ,
                .address                (adr)                                   ,
                .rd_en                  (rd_en)                                 ,
                .wr_en                  (wr_en)                                 ,
                .data_in                (wr_data)                               ,
                .data_out               (behave_mem_rd_data));



`endif





        `ifdef INTEL_SIMONLY
          `ifdef MBY_RTL
                generate

                        logic   [MEM_WIDTH_NO_SIG + (MEM_WR_RESOLUTION_ZERO_PADDING * MEM_WR_EN_WIDTH) - 1:0]  wr_data_for_prot_full_int                          ;
                        logic   [MEM_WIDTH_NO_SIG + MEM_TOTAL_ZERO_PADDING - 1:0]                              wr_data_for_prot_full                              ;
                        logic   [MEM_PROT_PER_GEN_INST-1:0]                                             wr_data_for_prot_interlv[MEM_PROT_TOTAL_GEN_INST-1:0]     ;
                        logic   [MEM_PROT_TOTAL_WIDTH-1:0]              signature_full;
                        logic   [MEM_WIDTH-1:0]                 prot_wr_data_out;

                        if (MEM_INIT_TYPE == 1)
                                begin:  CONST_MEM_INIT

                                        logic [MEM_WIDTH-1:0] init_word[MEM_DEPTH-1:0];
                                        logic [MEM_INIT_VALUE_WIDTH-1:0] init_value = MEM_INIT_VALUE[MEM_INIT_VALUE_WIDTH-1:0];         

                                        mby_mgm_functions mem_func();

                                        if (MEM_PROT_TYPE < 2) begin    
                                                
                                                initial begin

                                                // Zero Padding before protection
                                                        wr_data_for_prot_full_int[MEM_WIDTH_NO_SIG + (MEM_WR_RESOLUTION_ZERO_PADDING * MEM_WR_EN_WIDTH) - 1:0]                 = {(MEM_WIDTH_NO_SIG + (MEM_WR_RESOLUTION_ZERO_PADDING * MEM_WR_EN_WIDTH)){1'b0}}      ;
                                                        for (int i=0; i<MEM_WR_EN_WIDTH; i=i+1) begin
                                                                wr_data_for_prot_full_int[i*(MEM_WR_RESOLUTION_NO_SIG + MEM_WR_RESOLUTION_ZERO_PADDING)+:MEM_WR_RESOLUTION_NO_SIG]    = init_value[i*(MEM_WR_RESOLUTION_NO_SIG)+:MEM_WR_RESOLUTION_NO_SIG]                         ;
                                                        end

                                                        wr_data_for_prot_full[MEM_WIDTH_NO_SIG + MEM_TOTAL_ZERO_PADDING - 1:0]                                                 = {(MEM_WIDTH_NO_SIG + MEM_TOTAL_ZERO_PADDING){1'b0}}                                  ;
                                                        for (int i=0; i<MEM_WR_RES_PROT_FRAGM*MEM_WR_EN_WIDTH; i=i+1) begin
                                                                wr_data_for_prot_full[i*(MEM_PROT_RESOLUTION + MEM_PROT_RESOLUTION_ZERO_PADDING)+:MEM_PROT_RESOLUTION]  = wr_data_for_prot_full_int[i*(MEM_PROT_RESOLUTION)+:MEM_PROT_RESOLUTION]       ;
                                                        end

                                                // Interleaving
                                                        for (int i=0; i<MEM_WR_EN_WIDTH*MEM_WR_RES_PROT_FRAGM; i=i+1) begin
                                                                for (int j=0; j<MEM_PROT_INTERLV_LEVEL; j=j+1) begin
                                                                        for (int k=0; k<MEM_PROT_PER_GEN_INST; k=k+1) begin
                                                                                wr_data_for_prot_interlv[i*(MEM_PROT_INTERLV_LEVEL)+j][k]       = wr_data_for_prot_full[i*(MEM_PROT_RESOLUTION+MEM_PROT_RESOLUTION_ZERO_PADDING)+k*(MEM_PROT_INTERLV_LEVEL)+j]                          ;
                                                                        end
                                                                end
                                                        end

                                                // Combining all signatures into one            

                                                        for (int i=0; i<MEM_PROT_TOTAL_GEN_INST; i=i+1) begin
                                                                signature_full[i*MEM_PROT_INST_WIDTH_NO_SIG+:MEM_PROT_INST_WIDTH_NO_SIG] =mem_func.gen_ecc(wr_data_for_prot_interlv[i][MEM_PROT_PER_GEN_INST-1:0], MEM_PROT_TYPE);                

                                                        end
                                                

                                                // Combining the write data together with the signature
                                                
                                                        for (int i=0; i<MEM_WR_EN_WIDTH; i=i+1) begin
                                                                prot_wr_data_out[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]={signature_full[i*MEM_PROT_TOTAL_WIDTH/MEM_WR_EN_WIDTH+:MEM_PROT_TOTAL_WIDTH/MEM_WR_EN_WIDTH],MEM_INIT_VALUE[i*MEM_WR_RESOLUTION_NO_SIG+:MEM_WR_RESOLUTION_NO_SIG]};
                                                        end
                                                
                                    
                                                        for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
                                                                        init_word[i][MEM_WIDTH-1:0]    = prot_wr_data_out[MEM_WIDTH-1:0];
                                                        end
                                                        
                                                end
                                        end       
                                        else begin
                                        
                                                initial begin

                                                        for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
                                                                init_word[i] = MEM_INIT_VALUE;
                                                        end
                                                
                                                end
                                        
                                        end
                                        
                                        always @(posedge reset_n) 
                                                if ($test$plusargs("MBY_FAST_CONFIG")) begin
                                                                for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
                                                                        behave_mem.sram[i]      = init_word[i];
                                                                end

                                                end
                                end
                        else if (MEM_INIT_TYPE == 2)
                                begin:  LL_MEM_INIT
                                
                                        logic [MEM_WIDTH-1:0] init_word[MEM_DEPTH-1:0]    ;
                                        logic [MEM_INIT_VALUE_WIDTH-1:0] init_value       ;             

                                        mby_mgm_functions mem_func();

                                        if (MEM_PROT_TYPE < 2) begin    
                                                
                                                initial begin
                                                        for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
                                                                if (i == MEM_DEPTH-1) 
                                                                        init_value = LL_IS_LAST ? {(MEM_INIT_VALUE_WIDTH){1'b0}} : MEM_INIT_VALUE_WIDTH'(LL_INIT_OFFSET + i); 
                                                                else
                                                                        init_value = MEM_INIT_VALUE_WIDTH'(LL_INIT_OFFSET + i);         
                                                        
                                                        // Zero Padding before protection
                                                        
                                                                wr_data_for_prot_full_int[MEM_WIDTH_NO_SIG + (MEM_WR_RESOLUTION_ZERO_PADDING * MEM_WR_EN_WIDTH) - 1:0]                 = {(MEM_WIDTH_NO_SIG + (MEM_WR_RESOLUTION_ZERO_PADDING * MEM_WR_EN_WIDTH)){1'b0}}      ;
                                                                for (int i=0; i<MEM_WR_EN_WIDTH; i=i+1) begin
                                                                        wr_data_for_prot_full_int[i*(MEM_WR_RESOLUTION_NO_SIG + MEM_WR_RESOLUTION_ZERO_PADDING)+:MEM_WR_RESOLUTION_NO_SIG]    = init_value[i*(MEM_WR_RESOLUTION_NO_SIG)+:MEM_WR_RESOLUTION_NO_SIG]                         ;
                                                                end

                                                                wr_data_for_prot_full[MEM_WIDTH_NO_SIG + MEM_TOTAL_ZERO_PADDING - 1:0]                                                 = {(MEM_WIDTH_NO_SIG + MEM_TOTAL_ZERO_PADDING){1'b0}}                                  ;
                                                                for (int i=0; i<MEM_WR_RES_PROT_FRAGM*MEM_WR_EN_WIDTH; i=i+1) begin
                                                                        wr_data_for_prot_full[i*(MEM_PROT_RESOLUTION + MEM_PROT_RESOLUTION_ZERO_PADDING)+:MEM_PROT_RESOLUTION]  = wr_data_for_prot_full_int[i*(MEM_PROT_RESOLUTION)+:MEM_PROT_RESOLUTION]       ;
                                                                end

                                                        // Interleaving

                                                                for (int i=0; i<MEM_WR_EN_WIDTH*MEM_WR_RES_PROT_FRAGM; i=i+1) begin
                                                                        for (int j=0; j<MEM_PROT_INTERLV_LEVEL; j=j+1) begin
                                                                                for (int k=0; k<MEM_PROT_PER_GEN_INST; k=k+1) begin
                                                                                        wr_data_for_prot_interlv[i*(MEM_PROT_INTERLV_LEVEL)+j][k]       = wr_data_for_prot_full[i*(MEM_PROT_RESOLUTION+MEM_PROT_RESOLUTION_ZERO_PADDING)+k*(MEM_PROT_INTERLV_LEVEL)+j]                          ;
                                                                                end
                                                                        end
                                                                end

                                                        // Combining all signatures into one 
           
                                                        for (int i=0; i<MEM_PROT_TOTAL_GEN_INST; i=i+1) begin
                                                                signature_full[i*MEM_PROT_INST_WIDTH_NO_SIG+:MEM_PROT_INST_WIDTH_NO_SIG] =mem_func.gen_ecc(wr_data_for_prot_interlv[i][MEM_PROT_PER_GEN_INST-1:0], MEM_PROT_TYPE);                

                                                        end
                                                

                                                        // Combining the write data together with the signature
                                                        
                                                                for (int i=0; i<MEM_WR_EN_WIDTH; i=i+1) begin
                                                                        prot_wr_data_out[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]={signature_full[i*MEM_PROT_TOTAL_WIDTH/MEM_WR_EN_WIDTH+:MEM_PROT_TOTAL_WIDTH/MEM_WR_EN_WIDTH],init_value[i*MEM_WR_RESOLUTION_NO_SIG+:MEM_WR_RESOLUTION_NO_SIG]};
                                                                end



                                                        // put a protected word back                    
                                                                init_word[i][MEM_WIDTH-1:0]    = prot_wr_data_out[MEM_WIDTH-1:0];

                                                        end
                                                end
                                        end        
                                        else begin
                                        
                                                initial begin
                                                        init_word[MEM_DEPTH-1]= LL_IS_LAST ? {(MEM_INIT_VALUE_WIDTH){1'b0}} : MEM_INIT_VALUE_WIDTH'(LL_INIT_OFFSET + MEM_DEPTH - 1); 
                                                        for (int i = 0; i < MEM_DEPTH-1; i = i + 1) begin
                                                                init_word[i] = MEM_INIT_VALUE_WIDTH'(i + LL_INIT_OFFSET);
                                                        end
                                                
                                                end
                                        
                                        end

                                        always @(posedge reset_n) 
                                                if ($test$plusargs("MBY_FAST_CONFIG")) begin
                                                                for (int i = 0; i < MEM_DEPTH; i = i + 1)
                                                                        behave_mem.sram[i]      = init_word[i];
                                                end
                                end
                endgenerate     
          `endif
        `endif



        // Memories Implementation
`ifndef MBY_MSH_BEHAVE_MEMS
        
////////////////////////////////////////////////////////////////////////
//
//                              ASIC MEMORIES                                                                                                                   
//
////////////////////////////////////////////////////////////////////////



        generate
                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CAPABLE_SET_SHELLCTL
                        wire      shellctl_pwren_b_int        = shellctl_pwren_b     ; 
                end
        endgenerate



// TODO: Add assertion that will verify no memory accesses applied <n> (configurable) clocks before and after sleep command. 
// That would ensure Tactive and Twake  are maintained. Twake is given in lib file to insure that all the internal array power rails are fully charged up prior memory access



        // RAM Row Select

        wire    [1-1:0]                         ram_row_sel;
        assign                                  ram_row_sel[0]          = 1'b1;


        // RAM Address Decoder

        wire    [12-1:0]                ram_row_adr[1-1:0];
        assign                                  ram_row_adr[0][12-1:0]  =  ram_row_sel[0] ? adr - 12'd0 : 12'd0;


        // RAM Read Enable Decoder

        wire    [1-1:0]         ram_row_rd_en;
        assign                  ram_row_rd_en[0]                = ram_row_sel[0] ? rd_en : 1'b0;


        // Read Delay

        reg     [1-1:0] rd_en_delay[MEM_DELAY:0];
        generate
                if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_RD_EN_DELAY
                logic [1-1:0]           ram_row_rd_en_s;
                always_ff @(posedge clk) begin
                        ram_row_rd_en_s <= ram_row_rd_en;
                end
                assign  rd_en_delay[0][1-1:0] = ram_row_rd_en_s;
              end
              else begin: NO_PST_EBB_SAMPLE_RD_EN_DELAY
                assign  rd_en_delay[0][1-1:0] = ram_row_rd_en;
              end
        endgenerate
        always_ff @(posedge clk)
                        for (int i = 0; i <= MEM_DELAY-1; i = i + 1) begin
                                rd_en_delay[i+1]                <= rd_en_delay[i];
                        end
        logic   [1-1:0]         ram_row_num_rd_en_delay;
        always_ff @(posedge clk) begin
                if (|rd_en_delay[MEM_DELAY-1]) begin
                        for (int i = 0; i < 1; i = i + 1) begin
                                if (rd_en_delay[MEM_DELAY-1][i]) begin
                                        ram_row_num_rd_en_delay[1-1:0]  <= i;
                                end
                        end
                end
        end


        // Write Data

        logic   [552-1:0]       wr_data_full;
        assign                  wr_data_full    = wr_data;
        logic   [552-1:0]       ram_row_wr_data;
        always_comb begin
                for (int i = 0; i < MEM_WR_EN_WIDTH; i = i + 1) begin
                        ram_row_wr_data[i*(MEM_WR_RESOLUTION)+:MEM_WR_RESOLUTION]       = wr_data_full[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION];
                end
        end
        logic   [69-1:0]        ram_wr_data_col[8-1:0];
        always_comb begin
                for (int i = 0; i < 8; i = i + 1) begin
                        ram_wr_data_col[i][69-1:0]              = ram_row_wr_data[i*69+:69];
                end
        end


        // Write Enable

        logic   [MEM_WR_EN_WIDTH-1:0]   wr_en_full;
        assign                  wr_en_full      = wr_en;
        logic   [8-1:0] ram_row_wr_en[1-1:0];
        always_comb begin
                ram_row_wr_en[0] = {8{1'b0}};
                ram_row_wr_en[0][8-1:0] =  ram_row_sel[0] ? {{8{wr_en_full[0]}}} : 8'd0;
        end
        logic   [1-1:0]         ram_col_wr_en[1-1:0][8-1:0];
        assign          ram_col_wr_en[0][0]     = ram_row_wr_en[0][0*(1)+:1];
        assign          ram_col_wr_en[0][1]     = ram_row_wr_en[0][1*(1)+:1];
        assign          ram_col_wr_en[0][2]     = ram_row_wr_en[0][2*(1)+:1];
        assign          ram_col_wr_en[0][3]     = ram_row_wr_en[0][3*(1)+:1];
        assign          ram_col_wr_en[0][4]     = ram_row_wr_en[0][4*(1)+:1];
        assign          ram_col_wr_en[0][5]     = ram_row_wr_en[0][5*(1)+:1];
        assign          ram_col_wr_en[0][6]     = ram_row_wr_en[0][6*(1)+:1];
        assign          ram_col_wr_en[0][7]     = ram_row_wr_en[0][7*(1)+:1];


        // Read Data

        logic   [69-1:0]        ram_rd_data_col[1-1:0][8-1:0];
        logic   [552-1:0]       ram_rd_data_row;
        always_comb begin
                for (int i = 0; i < 8; i = i + 1) begin
                        ram_rd_data_row[i*(69)+:69]             = ram_rd_data_col[ram_row_num_rd_en_delay[1-1:0]][i][69-1:0];
                end
        end
        logic   [1*MEM_WIDTH-1:0]       ram_rd_data_full;
        always_comb begin
                for (int i = 0; i < 1*(MEM_WIDTH/MEM_WR_RESOLUTION); i = i + 1) begin
                        ram_rd_data_full[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]                = ram_rd_data_row[i*(MEM_WR_RESOLUTION+0)+:MEM_WR_RESOLUTION];
                end
        end
        logic   [MEM_WIDTH-1:0] rd_data_int;
        always_comb begin
                rd_data_int[MEM_WIDTH-1:0]      =       ram_rd_data_full[MEM_WIDTH-1:0];
        end
        assign          rd_data         = rd_data_int;
        assign          rd_valid        = |rd_en_delay[MEM_DELAY];


        // EBBs Instantiation

                logic           [8-1:0] ram_fary_pwren_b;
                logic           [8-1:0] ram_aary_pwren_b;
                logic           [8-1:0] ram_aary_pwren_b_last;
                assign                  ram_fary_pwren_b[0]     = fary_pwren_b_sram;
                assign                  ram_fary_pwren_b[1]     = ram_aary_pwren_b[0];
                assign                  ram_fary_pwren_b[2]     = ram_aary_pwren_b[1];
                assign                  ram_fary_pwren_b[3]     = ram_aary_pwren_b[2];
                assign                  ram_fary_pwren_b[4]     = ram_aary_pwren_b[3];
                assign                  ram_fary_pwren_b[5]     = ram_aary_pwren_b[4];
                assign                  ram_fary_pwren_b[6]     = ram_aary_pwren_b[5];
                assign                  ram_fary_pwren_b[7]     = ram_aary_pwren_b[6];
                assign                  aary_pwren_b_sram       = ram_aary_pwren_b_last[8-1];

                
                
                wire                            ram_row_0_col_0_clk = clk;
                wire    [12-1:0]               ram_row_0_col_0_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_0_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_0_wr_en       = |ram_col_wr_en[0][0];
                wire    [69-1:0]              ram_row_0_col_0_data_in     = ram_wr_data_col[0][69-1:0];
                wire    [69-1:0]              ram_row_0_col_0_data_out;
                wire    [1:0]                   ram_row_0_col_0_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_0_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_0_pwren_b_in ; 
                logic                           ram_row_0_col_0_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[0] = ram_row_0_col_0_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_0
                
                                     always_comb
                                       begin 
                
                                               if ((0 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_0_pwren_b_in     =  ram_fary_pwren_b[0] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_0_pwren_b_in     =  ram_fary_pwren_b[0] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[0]    = ram_aary_pwren_b[0] ; 
                                               else
                                                      ram_aary_pwren_b_last[0]    = ram_aary_pwren_b[0] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_0
                                        always_comb
                                                begin
                                                      ram_row_0_col_0_pwren_b_in     =  ram_fary_pwren_b[0] ; 
                                                       ram_aary_pwren_b_last[0]    = ram_aary_pwren_b[0] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_0 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_0_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_0_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_0_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_0_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_0_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_0_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_0_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_0_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_0_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[0*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_0_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[0]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[0]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_0
                                logic    [69-1:0]              ram_row_0_col_0_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_0_data_out_s <= ram_row_0_col_0_data_out;
                                end
                                assign  ram_rd_data_col[0][0]              = ram_row_0_col_0_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_0
                                assign  ram_rd_data_col[0][0]      = ram_row_0_col_0_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_1_clk = clk;
                wire    [12-1:0]               ram_row_0_col_1_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_1_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_1_wr_en       = |ram_col_wr_en[0][1];
                wire    [69-1:0]              ram_row_0_col_1_data_in     = ram_wr_data_col[1][69-1:0];
                wire    [69-1:0]              ram_row_0_col_1_data_out;
                wire    [1:0]                   ram_row_0_col_1_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_1_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_1_pwren_b_in ; 
                logic                           ram_row_0_col_1_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[1] = ram_row_0_col_1_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_1
                
                                     always_comb
                                       begin 
                
                                               if ((1 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_1_pwren_b_in     =  ram_fary_pwren_b[1] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_1_pwren_b_in     =  ram_fary_pwren_b[1] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[1]    = ram_aary_pwren_b[1] ; 
                                               else
                                                      ram_aary_pwren_b_last[1]    = ram_aary_pwren_b[1] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_1
                                        always_comb
                                                begin
                                                      ram_row_0_col_1_pwren_b_in     =  ram_fary_pwren_b[1] ; 
                                                       ram_aary_pwren_b_last[1]    = ram_aary_pwren_b[1] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_1 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_1_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_1_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_1_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_1_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_1_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_1_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_1_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_1_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_1_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[1*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_1_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[1]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[1]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_1
                                logic    [69-1:0]              ram_row_0_col_1_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_1_data_out_s <= ram_row_0_col_1_data_out;
                                end
                                assign  ram_rd_data_col[0][1]              = ram_row_0_col_1_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_1
                                assign  ram_rd_data_col[0][1]      = ram_row_0_col_1_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_2_clk = clk;
                wire    [12-1:0]               ram_row_0_col_2_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_2_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_2_wr_en       = |ram_col_wr_en[0][2];
                wire    [69-1:0]              ram_row_0_col_2_data_in     = ram_wr_data_col[2][69-1:0];
                wire    [69-1:0]              ram_row_0_col_2_data_out;
                wire    [1:0]                   ram_row_0_col_2_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_2_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_2_pwren_b_in ; 
                logic                           ram_row_0_col_2_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[2] = ram_row_0_col_2_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_2
                
                                     always_comb
                                       begin 
                
                                               if ((2 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_2_pwren_b_in     =  ram_fary_pwren_b[2] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_2_pwren_b_in     =  ram_fary_pwren_b[2] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[2]    = ram_aary_pwren_b[2] ; 
                                               else
                                                      ram_aary_pwren_b_last[2]    = ram_aary_pwren_b[2] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_2
                                        always_comb
                                                begin
                                                      ram_row_0_col_2_pwren_b_in     =  ram_fary_pwren_b[2] ; 
                                                       ram_aary_pwren_b_last[2]    = ram_aary_pwren_b[2] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_2 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_2_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_2_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_2_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_2_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_2_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_2_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_2_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_2_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_2_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[2*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_2_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[2]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[2]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_2
                                logic    [69-1:0]              ram_row_0_col_2_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_2_data_out_s <= ram_row_0_col_2_data_out;
                                end
                                assign  ram_rd_data_col[0][2]              = ram_row_0_col_2_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_2
                                assign  ram_rd_data_col[0][2]      = ram_row_0_col_2_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_3_clk = clk;
                wire    [12-1:0]               ram_row_0_col_3_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_3_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_3_wr_en       = |ram_col_wr_en[0][3];
                wire    [69-1:0]              ram_row_0_col_3_data_in     = ram_wr_data_col[3][69-1:0];
                wire    [69-1:0]              ram_row_0_col_3_data_out;
                wire    [1:0]                   ram_row_0_col_3_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_3_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_3_pwren_b_in ; 
                logic                           ram_row_0_col_3_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[3] = ram_row_0_col_3_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_3
                
                                     always_comb
                                       begin 
                
                                               if ((3 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_3_pwren_b_in     =  ram_fary_pwren_b[3] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_3_pwren_b_in     =  ram_fary_pwren_b[3] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[3]    = ram_aary_pwren_b[3] ; 
                                               else
                                                      ram_aary_pwren_b_last[3]    = ram_aary_pwren_b[3] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_3
                                        always_comb
                                                begin
                                                      ram_row_0_col_3_pwren_b_in     =  ram_fary_pwren_b[3] ; 
                                                       ram_aary_pwren_b_last[3]    = ram_aary_pwren_b[3] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_3 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_3_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_3_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_3_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_3_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_3_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_3_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_3_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_3_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_3_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[3*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_3_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[3]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[3]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_3
                                logic    [69-1:0]              ram_row_0_col_3_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_3_data_out_s <= ram_row_0_col_3_data_out;
                                end
                                assign  ram_rd_data_col[0][3]              = ram_row_0_col_3_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_3
                                assign  ram_rd_data_col[0][3]      = ram_row_0_col_3_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_4_clk = clk;
                wire    [12-1:0]               ram_row_0_col_4_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_4_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_4_wr_en       = |ram_col_wr_en[0][4];
                wire    [69-1:0]              ram_row_0_col_4_data_in     = ram_wr_data_col[4][69-1:0];
                wire    [69-1:0]              ram_row_0_col_4_data_out;
                wire    [1:0]                   ram_row_0_col_4_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_4_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_4_pwren_b_in ; 
                logic                           ram_row_0_col_4_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[4] = ram_row_0_col_4_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_4
                
                                     always_comb
                                       begin 
                
                                               if ((4 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_4_pwren_b_in     =  ram_fary_pwren_b[4] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_4_pwren_b_in     =  ram_fary_pwren_b[4] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[4]    = ram_aary_pwren_b[4] ; 
                                               else
                                                      ram_aary_pwren_b_last[4]    = ram_aary_pwren_b[4] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_4
                                        always_comb
                                                begin
                                                      ram_row_0_col_4_pwren_b_in     =  ram_fary_pwren_b[4] ; 
                                                       ram_aary_pwren_b_last[4]    = ram_aary_pwren_b[4] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_4 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_4_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_4_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_4_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_4_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_4_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_4_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_4_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_4_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_4_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[4*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_4_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[4]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[4]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_4
                                logic    [69-1:0]              ram_row_0_col_4_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_4_data_out_s <= ram_row_0_col_4_data_out;
                                end
                                assign  ram_rd_data_col[0][4]              = ram_row_0_col_4_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_4
                                assign  ram_rd_data_col[0][4]      = ram_row_0_col_4_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_5_clk = clk;
                wire    [12-1:0]               ram_row_0_col_5_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_5_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_5_wr_en       = |ram_col_wr_en[0][5];
                wire    [69-1:0]              ram_row_0_col_5_data_in     = ram_wr_data_col[5][69-1:0];
                wire    [69-1:0]              ram_row_0_col_5_data_out;
                wire    [1:0]                   ram_row_0_col_5_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_5_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_5_pwren_b_in ; 
                logic                           ram_row_0_col_5_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[5] = ram_row_0_col_5_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_5
                
                                     always_comb
                                       begin 
                
                                               if ((5 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_5_pwren_b_in     =  ram_fary_pwren_b[5] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_5_pwren_b_in     =  ram_fary_pwren_b[5] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[5]    = ram_aary_pwren_b[5] ; 
                                               else
                                                      ram_aary_pwren_b_last[5]    = ram_aary_pwren_b[5] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_5
                                        always_comb
                                                begin
                                                      ram_row_0_col_5_pwren_b_in     =  ram_fary_pwren_b[5] ; 
                                                       ram_aary_pwren_b_last[5]    = ram_aary_pwren_b[5] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_5 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_5_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_5_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_5_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_5_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_5_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_5_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_5_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_5_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_5_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[5*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_5_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[5]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[5]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_5
                                logic    [69-1:0]              ram_row_0_col_5_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_5_data_out_s <= ram_row_0_col_5_data_out;
                                end
                                assign  ram_rd_data_col[0][5]              = ram_row_0_col_5_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_5
                                assign  ram_rd_data_col[0][5]      = ram_row_0_col_5_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_6_clk = clk;
                wire    [12-1:0]               ram_row_0_col_6_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_6_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_6_wr_en       = |ram_col_wr_en[0][6];
                wire    [69-1:0]              ram_row_0_col_6_data_in     = ram_wr_data_col[6][69-1:0];
                wire    [69-1:0]              ram_row_0_col_6_data_out;
                wire    [1:0]                   ram_row_0_col_6_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_6_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_6_pwren_b_in ; 
                logic                           ram_row_0_col_6_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[6] = ram_row_0_col_6_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_6
                
                                     always_comb
                                       begin 
                
                                               if ((6 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_6_pwren_b_in     =  ram_fary_pwren_b[6] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_6_pwren_b_in     =  ram_fary_pwren_b[6] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[6]    = ram_aary_pwren_b[6] ; 
                                               else
                                                      ram_aary_pwren_b_last[6]    = ram_aary_pwren_b[6] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_6
                                        always_comb
                                                begin
                                                      ram_row_0_col_6_pwren_b_in     =  ram_fary_pwren_b[6] ; 
                                                       ram_aary_pwren_b_last[6]    = ram_aary_pwren_b[6] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_6 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_6_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_6_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_6_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_6_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_6_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_6_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_6_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_6_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_6_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[6*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_6_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[6]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[6]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_6
                                logic    [69-1:0]              ram_row_0_col_6_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_6_data_out_s <= ram_row_0_col_6_data_out;
                                end
                                assign  ram_rd_data_col[0][6]              = ram_row_0_col_6_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_6
                                assign  ram_rd_data_col[0][6]      = ram_row_0_col_6_data_out;
                        end
                endgenerate
                   

                
                
                wire                            ram_row_0_col_7_clk = clk;
                wire    [12-1:0]               ram_row_0_col_7_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_7_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_7_wr_en       = |ram_col_wr_en[0][7];
                wire    [69-1:0]              ram_row_0_col_7_data_in     = ram_wr_data_col[7][69-1:0];
                wire    [69-1:0]              ram_row_0_col_7_data_out;
                wire    [1:0]                   ram_row_0_col_7_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_7_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_7_pwren_b_in ; 
                logic                           ram_row_0_col_7_pwren_b_out; 
                
                
                  assign ram_aary_pwren_b[7] = ram_row_0_col_7_pwren_b_out; 
                
                
                
                        generate
                                if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CTL_ram_row_0_col_7
                
                                     always_comb
                                       begin 
                
                                               if ((7 != 0) | fscan_mode)
                                                  begin
                                                      ram_row_0_col_7_pwren_b_in     =  ram_fary_pwren_b[7] ; 
                                                  end
                
                                               else
                
                                                  ram_row_0_col_7_pwren_b_in     =  ram_fary_pwren_b[7] | shellctl_pwren_b ; 
                
                
                                               if ( fscan_mode | ram_fary_pwren_b[0])
                                                      ram_aary_pwren_b_last[7]    = ram_aary_pwren_b[7] ; 
                                               else
                                                      ram_aary_pwren_b_last[7]    = ram_aary_pwren_b[7] & ~(shellctl_pwren_b) ; 
                
                
                                       end
                
                
                                end
                                else begin: NO_POWER_GATE_CAPABLE_ram_row_0_col_7
                                        always_comb
                                                begin
                                                      ram_row_0_col_7_pwren_b_in     =  ram_fary_pwren_b[7] ; 
                                                       ram_aary_pwren_b_last[7]    = ram_aary_pwren_b[7] ; 
                                                end
                                end
                        endgenerate
                
                
                
                
                  
                  
                
                saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP #(        
                                .MSWT_MODE              (MSWT_MODE),                    // Ram Sequential Mode                                  
                                .BYPASS_CLK_MUX         (BYPASS_CLK_MUX),               // Glitch-free Clock Mux Used Between Func Clk and BIST Clk for Async Memories
                                .WRAPPER_COL_REPAIR     (WRAPPER_COL_REPAIR),           // Column redundancy is enabled
                                .BYPASS_MBIST_EN_SYNC   (BYPASS_MBIST_EN_SYNC),
                                .WRAPPER_REDROW_ENABLE  (WRAPPER_REDROW_ENABLE)         // Row redundancy is disabled
                        )               
                ram_row_0_col_7 (
                            //------------------------------------------------------------
                            // Functional Interfaces
                            //------------------------------------------------------------
                             .FUNC_CLK_SSA_IN                   (ram_row_0_col_7_clk),
                             .FUNC_REN_SSA                      (ram_row_0_col_7_rd_en),
                             .FUNC_WEN_SSA                      (ram_row_0_col_7_wr_en),
                             .FUNC_ADDR_SSA_IN                  (ram_row_0_col_7_adr),
                             .FUNC_DATA_SSA_IN                  (ram_row_0_col_7_data_in),
                             .DATA_SSA_OUT                      (ram_row_0_col_7_data_out),
                           //-------------------------------------------------------------
                           // Power Management Interfaces
                           //-------------------------------------------------------------           
                             .PWR_MGMT_MISC_SSA_IN              ({fary_ensleep_sram, fary_enblfloat_sram, fary_fwen_sram[1:0], fary_wakeup_sram, ram_row_0_col_7_pwren_b_in }), 
                             `ifndef INTEL_NO_PWR_PINS
                             `ifdef INTC_ADD_VSS
                                .vss                            (ram_row_0_col_7_vss),
                             `endif
                                .vccsocaon_lv                   (1'b1),
                                .vccsoc_lv                      (1'b0),
                             `endif
                             .PWR_MGMT_MISC_SSA_OUT             (ram_row_0_col_7_pwren_b_out),
                           //-------------------------------------------------------------
                           // DFx Interfaces
                           //-------------------------------------------------------------           
                           //.DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, fary_ffuse_data_red_sram[7*NFUSERED_SRAM+:NFUSERED_SRAM], 2'b0, fary_stm_hilo, fary_stm_enable}),
                             .DFX_MISC_SSA_IN                   ({car_raw_lan_power_good, 2'b0, fary_stm_hilo, fary_stm_enable}),
                             `ifdef INTC_MEM_ENABLE_BIRA_LVLIB
                             .BIST_RESET_B                      (1'b0),
                             .RROW_EN_SSA_IN                    (2'b0),
                             `endif
                             .DFX_MISC_SSA_OUT                  (),
                           //-------------------------------------------------------------
                           // Fuse Interfaces
                           //-------------------------------------------------------------           
                             .FUSE_MISC_SSA_IN                  ({fary_ffuse_data_misc_sram[NFUSEMISC_SRAM-1:2], ram_row_0_col_7_mc00b[1:0]}),
                           //-------------------------------------------------------------
                           // Bist Interfaces
                           //-------------------------------------------------------------           
                             .BIST_CLK_SSA_IN                   (1'b0),
                             .BIST_ADDR_SSA_IN                  (12'b0),
                             .BIST_WEN_SSA                      (1'b0),
                             .BIST_REN_SSA                      (1'b0),
                             .BIST_DATA_SSA_IN                  (69'b0),
                             .BIST_SSA_ENABLE                   (1'b0),
                             .UNGATE_BIST_WRTEN                 (1'b1),
                             .LBIST_TEST_MODE                   (fdfx_lbist_test_mode),
                           //-------------------------------------------------------------
                           //  ATPG AWT Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_AWT_MODE                (1'b0),
                             .FSCAN_RAM_AWT_REN                 (1'b0),
                             .FSCAN_RAM_AWT_WEN                 (1'b0),
                           //-------------------------------------------------------------
                           // Scan Interfaces
                           //-------------------------------------------------------------           
                             .FSCAN_RAM_RDDIS_B                 (fscan_ram_rddis_b),
                             .FSCAN_RAM_WRDIS_B                 (fscan_ram_wrdis_b),         
                             .FSCAN_RAM_ODIS_B                  (fscan_ram_odis_b[7]),
                             .FSCAN_RAM_BYPSEL                  (fscan_ram_bypsel[7]),
                             .FSCAN_BYPRST_B                    (fscan_byprst_b),
                             .FSCAN_RSTBYPEN                    (fscan_rstbypen),
                             .FSCAN_RAM_INIT_EN                 (fscan_ram_init_en),
                             .FSCAN_RAM_INIT_VAL                (fscan_ram_init_val)           
                );// saduls0g4l1p4096x69m4b4w0c0p1d0l2rm3sdrw01_MSWT_WRP
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_7
                                logic    [69-1:0]              ram_row_0_col_7_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_7_data_out_s <= ram_row_0_col_7_data_out;
                                end
                                assign  ram_rd_data_col[0][7]              = ram_row_0_col_7_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_7
                                assign  ram_rd_data_col[0][7]      = ram_row_0_col_7_data_out;
                        end
                endgenerate
                   

                
`elsif MBY_FPGA_MEMS

////////////////////////////////////////////////////////////////////////
//
//                              FPGA MEMORIES                                                                                                                   
//
////////////////////////////////////////////////////////////////////////

        wire                                    fpga_mem_wr_en          = |wr_en;
        
        reg     [FPGA_MEM_WIDTH-1:0]            fpga_mem_wr_data;
        reg     [FPGA_MEM_WR_EN_WIDTH-1:0]      fpga_mem_bwe;
        wire    [FPGA_MEM_WIDTH-1:0]            fpga_mem_rd_data;
        logic   [      MEM_WIDTH-1:0]           rd_data_int;


        generate
                if (FPGA_MEM_ZERO_PADDING > 0) begin: ZERO_PADDED_FPGA_MEM_WR_DATA
                        always_comb
                                begin
                                        for(int i = 0; i < MEM_WR_EN_WIDTH; i = i + 1)
                                                fpga_mem_wr_data[i*FPGA_MEM_WR_RESOLUTION+:FPGA_MEM_WR_RESOLUTION]      = {{(FPGA_MEM_ZERO_PADDING){1'b0}},wr_data[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]};
                                end
                end
                else begin: NO_ZERO_PADDED_FPGA_MEM_WR_DATA
                        always_comb
                                begin
                                        for(int i = 0; i < MEM_WR_EN_WIDTH; i = i + 1)
                                                fpga_mem_wr_data[i*FPGA_MEM_WR_RESOLUTION+:FPGA_MEM_WR_RESOLUTION]      = {wr_data[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]};
                                end
                end
        endgenerate

        always_comb
                begin
                        for(int i = 0; i < MEM_WR_EN_WIDTH; i = i + 1)
                                fpga_mem_bwe[i*(FPGA_MEM_WR_RESOLUTION/8)+:(FPGA_MEM_WR_RESOLUTION/8)]  = {(FPGA_MEM_WR_RESOLUTION/8){wr_en[i]}};
                end
        always_comb
                begin
                        for(int i = 0; i < MEM_WR_EN_WIDTH; i = i + 1)
                                rd_data_int[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]                     = fpga_mem_rd_data[i*(FPGA_MEM_WR_RESOLUTION)+:MEM_WR_RESOLUTION];
                end

        
        altsyncram #(
                .clock_enable_input_a           ("BYPASS")                      ,                       
                .clock_enable_output_a          ("BYPASS")                      ,
                .intended_device_family         ("Stratix IV")                  ,
                .lpm_hint                       ("ENABLE_RUNTIME_MOD=NO")       ,
                .lpm_type                       ("altsyncram")                  ,
                .numwords_a                     (MEM_DEPTH)                     ,
                .operation_mode                 ("SINGLE_PORT")                 ,
                .outdata_aclr_a                 ("NONE")                        ,
                .outdata_reg_a                  ("UNREGISTERED")                ,
                .power_up_uninitialized         ("FALSE")                       ,
                .read_during_write_mode_port_a  ("DONT_CARE")                   ,
                .widthad_a                      (MEM_ADR_WIDTH)                 ,
                .width_a                        (FPGA_MEM_WIDTH)                ,
                .width_byteena_a                (FPGA_MEM_WR_EN_WIDTH))
        fpga_mem (
                .address_a                      (adr)                           ,
                .clock0                         (clk)                           ,
                .data_a                         (fpga_mem_wr_data)              ,
                .wren_a                         (fpga_mem_wr_en)                ,
                .rden_a                         (1'b1)                          ,
                .q_a                            (fpga_mem_rd_data)              ,
                .aclr0                          (1'b0)                          ,
                .aclr1                          (1'b0)                          ,
                .address_b                      (1'b1)                          ,
                .addressstall_a                 (1'b0)                          ,
                .addressstall_b                 (1'b0)                          ,
                .byteena_a                      (fpga_mem_bwe)                  ,
                .byteena_b                      (1'b1)                          ,
                .clock1                         (1'b1)                          ,
                .clocken0                       (1'b1)                          ,
                .clocken1                       (1'b1)                          ,
                .clocken2                       (1'b1)                          ,
                .clocken3                       (1'b1)                          ,
                .data_b                         (1'b1)                          ,
                .eccstatus                      ()                              ,
                .q_b                            ()                              ,
                .rden_b                         (1'b1)                          ,
                .wren_b                         (1'b0));

        // Read Delay
        logic   [MEM_DELAY:0]           rd_en_delay;
        always_comb
                rd_en_delay[0]                          = rd_en;
        always_ff @(posedge clk) begin
                rd_en_delay[MEM_DELAY:1]        <= rd_en_delay[MEM_DELAY-1:0];
        end
        assign          rd_valid                        = rd_en_delay[MEM_DELAY];

        // Read Data Delay
        logic   [MEM_WIDTH-1:0]         rd_data_delay[MEM_DELAY:0];
        always_comb
                rd_data_delay[0]                        = rd_data_int;
        always_ff @(posedge clk) begin
                for (int i = 1; i <= MEM_DELAY; i = i + 1) begin
                        if (rd_en_delay[i])
                                rd_data_delay[i]        <= rd_data_delay[i-1];
                end
        end
        assign          rd_data[MEM_WIDTH-1:0]  = rd_en_delay[MEM_DELAY] ? rd_data_delay[MEM_DELAY-1][MEM_WIDTH-1:0] : rd_data_delay[MEM_DELAY][MEM_WIDTH-1:0];

        `ifdef INTEL_SIMONLY
           `ifdef MBY_RTL
                generate
                        if (MEM_INIT_TYPE == 1)
                                begin:  CONST_MEM_INIT

                                        reg [MEM_WIDTH-1:0] init_word[MEM_DEPTH-1:0];

                                        mby_mgm_functions mem_func();

                                        if (MEM_PROT_TYPE < 2) begin    
                                                
                                                initial begin
                                                
                                                        for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
                                                                for (int j = 0; j < MEM_INIT_PROT_INST_NUM; j = j + 1) begin
                                                                        init_word[i][j*(MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM) +: MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM]    = MEM_INIT_VALUE[j*(MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM)+:(MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM)];
                                                                        init_word[i][MEM_INIT_VALUE_WIDTH + j*(MEM_PROT_INST_WIDTH)  +: MEM_PROT_INST_WIDTH]                            = mem_func.gen_ecc(MEM_INIT_VALUE[j*(MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM)+:(MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM)], MEM_PROT_TYPE);
                                                                end
                                                        end
                                                        
                                                end
                                                
                                        end
                                        else begin
                                        
                                                initial begin

                                                        for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
                                                                init_word[i] = MEM_INIT_VALUE;
                                                        end
                                                
                                                end
                                        
                                        end

                                        if (FPGA_MEM_ZERO_PADDING > 0) begin
                                                always @(posedge reset_n)
                                                        if ($test$plusargs("MBY_FAST_CONFIG")) begin
                                                                for (int i = 0; i < MEM_DEPTH; i = i + 1)
                                                                        for(int j = 0; j < MEM_WR_EN_WIDTH; j = j + 1)
                                                                                fpga_mem.mem_data[i][j*FPGA_MEM_WR_RESOLUTION+:FPGA_MEM_WR_RESOLUTION]  = {{(FPGA_MEM_ZERO_PADDING){1'b0}},init_word[i][j*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]};
                                                        end
                                        end
                                        else begin
                                                always @(posedge reset_n)
                                                        if ($test$plusargs("MBY_FAST_CONFIG")) begin
                                                                for (int i = 0; i < MEM_DEPTH; i = i + 1)
                                                                        for(int j = 0; j < MEM_WR_EN_WIDTH; j = j + 1)
                                                                                fpga_mem.mem_data[i][j*FPGA_MEM_WR_RESOLUTION+:FPGA_MEM_WR_RESOLUTION]  = {init_word[i][j*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]};
                                                        end
                                        end
                                                                                                                                
                                end
                        else if (MEM_INIT_TYPE == 2)
                                begin:  LL_MEM_INIT
                                
                                        reg [MEM_WIDTH-1:0] init_word[MEM_DEPTH-1:0];

                                        mby_mgm_functions mem_func();

                                        if (MEM_PROT_TYPE < 2) begin    
                                                
                                                initial begin
                                                        for (int i = 0; i < MEM_DEPTH-1; i = i + 1) begin
                                                                for (int j = 0; j < MEM_INIT_PROT_INST_NUM; j = j + 1) begin

                                                                        if (i == MEM_DEPTH-1)
                                                                                init_value = LL_IS_LAST ? {(MEM_INIT_VALUE_WIDTH){1'b0}} : MEM_INIT_VALUE_WIDTH'(i + LL_INIT_OFFSET);
                                                                        else 
                                                                                init_value = MEM_INIT_VALUE_WIDTH'( i + LL_INIT_OFFSET );

                                                                        init_word[i][j*(MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM) +: MEM_INIT_VALUE_WIDTH/MEM_INIT_PROT_INST_NUM]    = init_value;
                                                                        init_word[i][MEM_INIT_VALUE_WIDTH + j*(MEM_PROT_INST_WIDTH)  +: MEM_PROT_INST_WIDTH]                            = mem_func.gen_ecc(init_value, MEM_PROT_TYPE);
                                                                end
                                                        end
                                                        
                                                end
                                                
                                        end
                                        else begin
                                        
                                                initial begin
                                                        init_word[MEM_DEPTH-1]= LL_IS_LAST ? {(MEM_INIT_VALUE_WIDTH){1'b0}} : MEM_INIT_VALUE_WIDTH'(LL_INIT_OFFSET + MEM_DEPTH - 1); 
                                                        for (int i = 0; i < MEM_DEPTH-1; i = i + 1) begin
                                                                init_word[i] = MEM_INIT_VALUE_WIDTH'(i + LL_INIT_OFFSET);
                                                        end
                                                
                                                end
                                        
                                        end

                                        if (FPGA_MEM_ZERO_PADDING > 0) begin
                                                always @(posedge reset_n)
                                                        if ($test$plusargs("MBY_FAST_CONFIG")) begin
                                                                for (int i = 0; i < MEM_DEPTH; i = i + 1)
                                                                        for(int j = 0; j < MEM_WR_EN_WIDTH; j = j + 1)
                                                                                fpga_mem.mem_data[i][j*FPGA_MEM_WR_RESOLUTION+:FPGA_MEM_WR_RESOLUTION]  = {{(FPGA_MEM_ZERO_PADDING){1'b0}},init_word[i][j*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]};
                                                        end
                                        end
                                        else begin
                                                always @(posedge reset_n)
                                                        if ($test$plusargs("MBY_FAST_CONFIG")) begin
                                                                for (int i = 0; i < MEM_DEPTH; i = i + 1)
                                                                        for(int j = 0; j < MEM_WR_EN_WIDTH; j = j + 1)
                                                                                fpga_mem.mem_data[i][j*FPGA_MEM_WR_RESOLUTION+:FPGA_MEM_WR_RESOLUTION]  = {init_word[i][j*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION]};
                                                        end
                                        end

                                end
                endgenerate
           `endif
        `endif

        `ASSERTS_NEVER(ASSERT_no_post_EBB_sample, MEM_PST_EBB_SAMPLE == 1, clk, 1'b1, );

`else


////////////////////////////////////////////////////////////////////////
//
//                              BEHAVE MEMORIES                                                                                                                                         
//
////////////////////////////////////////////////////////////////////////
        

        // Read Delay
        logic   [MEM_DELAY:0]           rd_en_delay;
        always_ff @(posedge clk) begin
                rd_en_delay[MEM_DELAY:1]        <= rd_en_delay[MEM_DELAY-1:0];
        end
        assign          rd_valid                        = rd_en_delay[MEM_DELAY];
        generate
                if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA
                        logic    [MEM_WIDTH-1:0]   behave_mem_rd_data_s;
                        logic                      rd_en_s;
                        always_ff @(posedge clk) begin
                                behave_mem_rd_data_s     <= behave_mem_rd_data;
                                rd_en_s                  <= rd_en;
                        end
                        assign  rd_data_behave_int    = behave_mem_rd_data_s;
                        assign  rd_en_delay[0] = rd_en_s;
                end
                else begin: NO_PST_EBB_SAMPLE_DATA
                        assign rd_data_behave_int     = behave_mem_rd_data;
                        assign rd_en_delay[0]  = rd_en;
                end
        endgenerate

        // Read Data Delay
        logic   [MEM_WIDTH-1:0]         rd_data_delay[MEM_DELAY:0];
        always_comb
                rd_data_delay[0]                        = rd_data_behave_int;
        always_ff @(posedge clk) begin
                for (int i = 1; i <= MEM_DELAY; i = i + 1) begin
                        if (rd_en_delay[i])
                                rd_data_delay[i]        <= rd_data_delay[i-1];
                end
        end
        assign          rd_data[MEM_WIDTH-1:0]  = rd_en_delay[MEM_DELAY] ? rd_data_delay[MEM_DELAY-1][MEM_WIDTH-1:0] : rd_data_delay[MEM_DELAY][MEM_WIDTH-1:0];


        
`endif
`ifdef INTEL_SIMONLY
 task automatic memory_init (input string file_name );
    begin     
`ifdef MBY_MSH_BEHAVE_MEMS
       $readmemh ( file_name,behave_mem.sram);
`endif
    end     
   endtask // automatic

`endif //  `ifdef INTEL_SIMONLY
   
endmodule
