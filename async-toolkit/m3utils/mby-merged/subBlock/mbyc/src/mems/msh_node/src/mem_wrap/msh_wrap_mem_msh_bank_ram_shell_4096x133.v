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
//------------------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////
//
//                      Automated Memory Wrappers Creator
//
//      Created by solson with create_memories script version 2.40 on NA
//                                      & 
// Physical file /nfs/site/disks/slx_1593/solson/mby/work_root/mby-mby-x0_WW0519/tools/mgm/mby_physical_params.csv
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
module  msh_wrap_mem_msh_bank_ram_shell_4096x133 #(
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
//         generate
//                 if (POWER_GATE_CAPABLE ) begin: POWER_GATE_CAPABLE_SET
//                         always_comb
//                                 begin
//                                      shellctl_pwren_b= wrap_shell_to_mem[MEM_WIDTH+MEM_ADR_WIDTH+MEM_WR_EN_WIDTH+2+MEM_RM_WIDTH +1   +:1                     ]; 
//                                 end
//                 end
//         endgenerate


        typedef struct packed{
                logic                                                   pwren_b         ; 
                logic                                                   reset_n         ;
                logic                                                   rm_e            ;
                logic   [                         MEM_RM_WIDTH-1:0]     rm              ;
                logic                                                   rd_en           ;
                logic   [                      MEM_WR_EN_WIDTH-1:0]     wr_en           ;
                logic   [                        MEM_ADR_WIDTH-1:0]     adr             ;
                logic   [      MEM_WIDTH- MEM_PROT_TOTAL_WIDTH-1:0]     wr_data_orig    ;
                logic   [                            MEM_WIDTH-1:0]     wr_data         ;
        } to_mem_t;

        typedef struct packed{
                logic   [                            MEM_WIDTH-1:0]     rd_data         ;
                logic                                                   rd_valid        ;
        } from_mem_t;


        from_mem_t      wrap_shell_from_mem_int; 
        assign          wrap_shell_from_mem = wrap_shell_from_mem_int; 
        to_mem_t        wrap_shell_to_mem_int; 
        assign          wrap_shell_to_mem_int = to_mem_t'(wrap_shell_to_mem);
        
        wire                                           reset_n                        = wrap_shell_to_mem_int.reset_n       ;
        wire    [   MEM_RM_WIDTH-1:0]                  mem_rm                         = wrap_shell_to_mem_int.rm        ;
        wire                                           mem_rm_e                       = wrap_shell_to_mem_int.rm_e      ;
        wire                                           rd_en                          = wrap_shell_to_mem_int.rd_en         ;
        wire    [MEM_WR_EN_WIDTH-1:0]                  wr_en                          = wrap_shell_to_mem_int.wr_en         ;
        wire    [  MEM_ADR_WIDTH-1:0]                  adr                            = wrap_shell_to_mem_int.adr           ;
        wire    [MEM_WIDTH-MEM_PROT_TOTAL_WIDTH-1:0]   wr_data_orig                   = wrap_shell_to_mem_int.wr_data_orig  ;
        wire    [      MEM_WIDTH-1:0]                  wr_data                        = wrap_shell_to_mem_int.wr_data       ;
                                                                  
        // Assembling the from_mem bus
        wire    [      MEM_WIDTH-1:0]   rd_data                                      ;         
        wire                            rd_valid                                     ;                                                                       
        assign                          wrap_shell_from_mem_int.rd_valid  = rd_valid ;
        assign                          wrap_shell_from_mem_int.rd_data   = rd_data  ;
        

`ifdef  MBY_MSH_BEHAVE_MEMS_EXCLUDE
        `define MBY_MSH_BEHAVE_MEMS
`endif



`ifdef INTEL_SIMONLY


        

        logic    [133  -1:0]   rd_data_orig  ;       

        mby_mgm_1rw_behave #(
                .MEM_WIDTH              (133)        ,
                .MEM_DEPTH              (MEM_DEPTH),
                .MEM_WR_RESOLUTION      (133)
                )                              
        behave_mem_acc (
                .clk                    (clk)                                ,
                .address                (adr)                                ,
                
                `ifdef MBY_RTL_MGM_DEBUG
                 .rd_en                  (rd_en)                                 ,
                 `else
                 .rd_en                  (1'b0)                                 ,
                `endif
                .wr_en                  (wr_en)                                 ,
                .data_in                (wr_data_orig)                          ,
                .data_out               (rd_data_orig)

                );

`endif
`ifdef  MBY_MSH_BEHAVE_MEMS



        
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

`endif // `ifdef        MBY_MSH_BEHAVE_MEMS


`ifdef INTEL_SIMONLY


        
        logic [MEM_WIDTH-1:0] init_word[MEM_DEPTH-1:0]    ;
        logic [MEM_INIT_VALUE_WIDTH-1:0] init_value       ;             




        generate

        if   (MEM_PROT_TYPE < 2) 
                begin: PROT_FUNC  


                  logic    [MEM_WIDTH    -1:0]   rd_data_orig_prot  ;       
                  logic                          prot_fail  ;       


                   mby_mgm_functions #(
                       .MEM_WIDTH                       (133),
                       .MEM_WR_RESOLUTION               (133),
                       .MEM_WR_RES_PROT_FRAGM           (),
                       .MEM_PROT_RESOLUTION             (),
                       .MEM_PROT_INTERLV_LEVEL          (1),
                       .MEM_PROT_TYPE                   (2)
                   )
                   mby_mgm_functions ();

// 
//                 `ifdef MBY_RTL_MGM_DEBUG
//                    mby_mgm_functions_exp #(
//                        .MEM_WIDTH                       (133),
//                        .MEM_WR_RESOLUTION               (133),
//                        .MEM_WR_RES_PROT_FRAGM           (),
//                        .MEM_PROT_RESOLUTION             (),
//                        .MEM_PROT_INTERLV_LEVEL          (1),
//                        .MEM_PROT_TYPE                   (2)
//                    )
//                    mby_mgm_functions_exp (
//                    .init_value(rd_data_orig)
//                    );
// 
//                    always_comb 
//                    begin
//                         rd_data_orig_prot  = mby_mgm_functions.calc_prot_ext(rd_data_orig);
//                         prot_fail = (rd_data_orig_prot !== behave_mem_rd_data); 
//                    end
//                  `endif

                end // PROT_FUNC
        else
          begin: PROT_FUNC 
                   mby_mgm_functions_empty #(
                       .MEM_WIDTH                       (133),
                       .MEM_WR_RESOLUTION               (133),
                       .MEM_WR_RES_PROT_FRAGM           (),
                       .MEM_PROT_RESOLUTION             (),
                       .MEM_PROT_INTERLV_LEVEL          (1),
                       .MEM_PROT_TYPE                   (MEM_PROT_TYPE)
                   )
                   mby_mgm_functions ();
          end // PROT_FUNC
                 
    endgenerate
`endif
 
////////////////////////////////////////////////////////////////////////
//
//                           Mems' Fast Config                                                                                                                 
//
////////////////////////////////////////////////////////////////////////


`ifdef INTEL_SIMONLY


        initial 
          begin
            
            
        forever  begin //delay
 
            @(posedge reset_n) 
                   if ($test$plusargs(" MBY_FAST_CONFIG")) begin
                       memory_fast_config ( ); 
                   end
 
        end 
          
          end

          
 `endif // SIMONLY



        // Memories Implementation
`ifndef MBY_MSH_BEHAVE_MEMS
        
////////////////////////////////////////////////////////////////////////
//
//                              ASIC MEMORIES                                                                                                                   
//
////////////////////////////////////////////////////////////////////////




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

        logic   [133-1:0]       wr_data_full;
        assign                  wr_data_full    = wr_data;
        logic   [133-1:0]       ram_row_wr_data;
        always_comb begin
                ram_row_wr_data = 0;     // Overcome write_data width gap
                for (int i = 0; i < MEM_WR_EN_WIDTH; i = i + 1) begin
                        ram_row_wr_data[i*(MEM_WR_RESOLUTION)+:MEM_WR_RESOLUTION]       = wr_data_full[i*MEM_WR_RESOLUTION+:MEM_WR_RESOLUTION];
                end
        end
        logic   [133-1:0]       ram_wr_data_col[1-1:0];
        always_comb begin
                for (int i = 0; i < 1; i = i + 1) begin
                        ram_wr_data_col[i][133-1:0]             = ram_row_wr_data[i*133+:133];
                end
        end


        // Write Enable

        logic   [MEM_WR_EN_WIDTH-1:0]   wr_en_full;
        assign                  wr_en_full      = wr_en;
        logic   [1-1:0] ram_row_wr_en[1-1:0];
        always_comb begin
                ram_row_wr_en[0] = {1{1'b0}};
                ram_row_wr_en[0][1-1:0] =  ram_row_sel[0] ? {{1{wr_en_full[0]}}} : 1'd0;
        end
        logic   [1-1:0]         ram_col_wr_en[1-1:0][1-1:0];
        assign          ram_col_wr_en[0][0]     = ram_row_wr_en[0][0*(1)+:1];


        // Read Data

        logic   [133-1:0]       ram_rd_data_col[1-1:0][1-1:0];
        logic   [133-1:0]       ram_rd_data_row;
        always_comb begin
                for (int i = 0; i < 1; i = i + 1) begin
                        ram_rd_data_row[i*(133)+:133]           = ram_rd_data_col[ram_row_num_rd_en_delay[1-1:0]][i][133-1:0];
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

                logic           [1-1:0] ram_fary_pwren_b;
                logic           [1-1:0] ram_aary_pwren_b;
                logic           [1-1:0] ram_aary_pwren_b_last;
                assign                  ram_fary_pwren_b[0]     = fary_pwren_b_sram;
                assign                  aary_pwren_b_sram       = ram_aary_pwren_b_last[1-1];

                wire                            ram_row_0_col_0_clk = clk;
                wire    [12-1:0]               ram_row_0_col_0_adr = ram_row_adr[0][12-1:0];
                wire                            ram_row_0_col_0_rd_en       = ram_row_rd_en[0];
                wire                            ram_row_0_col_0_wr_en       = |ram_col_wr_en[0][0];
                wire    [133-1:0]              ram_row_0_col_0_data_in     = ram_wr_data_col[0][133-1:0];
                wire    [133-1:0]              ram_row_0_col_0_data_out;
                //wire    [1:0]                   ram_row_0_col_0_mc00b       = mem_rm_e ? mem_rm[1:0] : fary_ffuse_data_misc_sram[1:0];
                wire                            ram_row_0_col_0_vss         = 1'b0;
                
                
                logic                           ram_row_0_col_0_pwren_b_in ; 
                logic                           ram_row_0_col_0_pwren_b_out; 
                
                logic                           ram_row_0_col_0_pwren_b_out_inv; 
                assign ram_row_0_col_0_pwren_b_out = 0;// ~ram_row_0_col_0_pwren_b_out_inv; 
                
                
                  assign ram_aary_pwren_b[0] = 0;// ram_row_0_col_0_pwren_b_out; 
                
                
                
                always_comb
                        begin
                              ram_row_0_col_0_pwren_b_in     =  ram_fary_pwren_b[0] ; 
                               ram_aary_pwren_b_last[0]    = ram_aary_pwren_b[0] ; 
                        end
                
                
                
                  
                //Symbol
                //
                //                              _____________________________
                //                             |                             |
                //                         ----|ADRi                         |
                //                         ----|Di                           |
                //                         ----|WE                           |
                //                         ----|ME                           |
                //                         ----|CLK                          |
                //                         ----|RSCIN                        |
                //                         ----|RSCEN                        |
                //                         ----|RSCRST                       |
                //                         ----|RSCLK                        |
                //                         ----|FISO                         |
                //                         ----|TADRi                        |
                //                         ----|TDi                          |
                //                         ----|TWE                          |
                //                         ----|TME                          |
                //                         ----|TCLK                         |
                //                         ----|BISTE                        |
                //                         ----|TEST1                        |
                //                             |                           Qi|---
                //                         ----|TEST_RNM                     |
                //                             |                       RSCOUT|---
                //                         ----|RME                          |
                //                             |                          QPi|---
                //                         ----|RMi                          |
                //                             |                       SO_Q_L|---
                //                         ----|CDi                          |
                //                             |                       SO_Q_H|---
                //                         ----|CAPT                         |
                //                             |                       SO_D_L|---
                //                         ----|PIPEME                       |
                //                             |                       SO_D_H|---
                //                         ----|TPIPEME                      |
                //                             |                      SO_CNTR|---
                //                         ----|TCLKE                        |
                //                             |                          ROP|---
                //                         ----|STICKY                       |
                //                         ----|SI_Q_L                       |
                //                         ----|SI_Q_H                       |
                //                         ----|SI_D_L                       |
                //                         ----|SI_D_H                       |
                //                         ----|SE_Q                         |
                //                         ----|SE_IN                        |
                //                         ----|SI_CNTR                      |
                //                         ----|DFTCLKEN                     |
                //                         ----|DFTMASK                      |
                //                         ----|LS                           |
                //                         ----|DS                           |
                //                         ----|SD                           |
                //                         ----|BC0                          |
                //                         ----|BC1                          |
                //                         ----|BC2                          |
                //                             |_____________________________|
                //
                //
                
                
                
                
                
                `ifndef MBY_MGM_ASIC_BEHAVE
                  
                
                    saduls0g4l1p4096x133m4b4w0c0p0d0l2rm3sdrw01 #(        
                            )               
                    ram_row_0_col_0 (
                
                
                `else 
                    mgm_1rw_asic_behave   #(
                          .MEM_WIDTH (133), 
                          .MEM_DEPTH (12)            
                            ) 
                  
                `endif
                
                          .CLK       (ram_row_0_col_0_clk), //       input   logic                                                 
                          .ADR       (ram_row_0_col_0_adr), //       input   logic   [MEM_ADR_WIDTH-1:0]                           
                          .ME        (1'b1), //       input   logic                                                 
                          .WE        (ram_row_0_col_0_wr_en), //       input   logic   [MEM_WE_WIDTH-1:0]                            
                          .D         (ram_row_0_col_0_data_in), //       input   logic   [      MEM_WIDTH-1:0]                         
                          .Q         (ram_row_0_col_0_data_out), //       output  logic   [      MEM_WIDTH-1:0]                         
                
                          
                          .RSCIN     (1'b0), //        input                                                         
                          .RSCEN     (1'b0), //        input                                                         
                          .RSCRST    (1'b0), //        input                                                         
                          .RSCLK     (1'b0), //        input                                                         
                          .FISO      (1'b0), //        input                                                         
                          .TEST1     (1'b0), //        input                                                         
                          .TEST_RNM  (1'b0), //        input                                                         
                          .RME       (1'b0), //        input                                                         
                          .RM        (4'b0), //        input [3:0]                                                        
                          .LS        (1'b0), //        input                                                         
                
                
                          .BC0       (1'b0), //        input                                                         
                          .BC1       (1'b0), //        input                                                         
                          .BC2       (1'b0), //        input  
                          .PIPEME    (1'b0), //        input 
                             .SI_D    (1'b0),  //         input
                             .SI_Q    (1'b0),  //         input
                
                
                
                
                          .SE_Q      (1'b0), //        input
                          .SE_IN     (1'b0), //        input
                          .SI_CNTR   (1'b0), //        input
                          .DFTCLKEN  (1'b0), //        input
                          .DFTMASK   (1'b0), //        input
                          .RSCOUT    (), //        output  logic                                                 
                
                
                 `ifdef MBY_MGM_ASSIST_EN
                          .RA (2'b0), 
                          .WA (3'b0), 
                          .WPULSE (3'b0), 
                
                 `endif
                           
                          .QP         (), //        output
                
                
                
                
                              .SO_D       (), //        output
                              .SO_Q       (), //        output
                          .SO_CNTR    () //        output
                
                ); 
                
                
                generate
                        if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA_0
                                logic    [133-1:0]              ram_row_0_col_0_data_out_s;
                                always_ff @(posedge clk) begin
                                        ram_row_0_col_0_data_out_s <= ram_row_0_col_0_data_out;
                                end
                                assign  ram_rd_data_col[0][0]              = ram_row_0_col_0_data_out_s;
                        end
                        else begin: NO_PST_EBB_SAMPLE_DATA_0
                                assign  ram_rd_data_col[0][0]      = ram_row_0_col_0_data_out;
                        end
                endgenerate
                   

                

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
                if (MEM_PST_EBB_SAMPLE == 1) begin: PST_EBB_SAMPLE_DATA15
                        logic    [MEM_WIDTH-1:0]   behave_mem_rd_data_s;
                        logic                      rd_en_s;
                        always_ff @(posedge clk) begin
                                behave_mem_rd_data_s     <= behave_mem_rd_data;
                                rd_en_s                  <= rd_en;
                        end
                        assign  rd_data_behave_int    = behave_mem_rd_data_s;
                        assign  rd_en_delay[0] = rd_en_s;
                end
                else begin: NO_PST_EBB_SAMPLE_DATA15
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

parameter           MEM_INIT_TYPE_T = 0 ;
parameter           LL_INIT_OFFSET_T = 0 ;
parameter           LL_IS_LAST_T  = 0 ;
parameter           MEM_INIT_VALUE_T = 133'h0 ; 
parameter       MEM_NUM_ROW = 1; 
parameter       MEM_NUM_COL = 1; 
parameter       MEM_TOTAL_W = 133; 
parameter       MEM_ROW_W = 4096 ; 
parameter       MEM_COL_W = 133; 
parameter       MEM_TOTAL_DEPTH = MEM_NUM_ROW * MEM_ROW_W; 
parameter       MEM_LOGICAL_DEPTH = 4096; 
parameter       MEM_FOLD_FACT = 1; 
parameter       MEM_IS_FLOPS = 0; 
    `ifdef INTEL_SIMONLY
       // load behave_mem_acc from file
       task automatic memory_init (input string file_name );
         $readmemh ( file_name,behave_mem_acc.sram);
         memory_init_sync(); 
       endtask
    
       // load behave_mem_acc from array
       task automatic memory_fast_config (); 
       logic [MEM_WIDTH-1:0] init_value   ;             
         begin
    
    
              `ifdef MBY_MSH_BEHAVE_MEMS
              for (int i = 0; i < MEM_LOGICAL_DEPTH; i = i + 1) begin
                     if (MEM_INIT_TYPE_T == 1)
                          behave_mem_acc.sram[i]    = MEM_INIT_VALUE_T;
                     else if (MEM_INIT_TYPE_T == 2) begin
    
                        if (i == MEM_LOGICAL_DEPTH-1) 
                            init_value = LL_IS_LAST_T ? 0 : (LL_INIT_OFFSET_T + i); 
                        else
                            init_value = LL_INIT_OFFSET_T + i;         
    
                        behave_mem_acc.sram[i]   =init_value;
                     end
                     
              end
              
               
              `else // MBY_MSH_BEHAVE_MEMS
              for (int i = 0; i < MEM_TOTAL_DEPTH; i = i + 1) begin
                 for (int k = 0; k < MEM_FOLD_FACT; k = k + 1) begin   
    
    
    
                     if (MEM_INIT_TYPE == 1)
                          behave_mem_acc.sram[i*MEM_FOLD_FACT +k]    = PROT_FUNC.mby_mgm_functions.calc_prot_ext(MEM_INIT_VALUE_T);
                     else if (MEM_INIT_TYPE_T == 2) begin
    
                        if ((i*MEM_FOLD_FACT +k) == MEM_LOGICAL_DEPTH-1) 
                            init_value = LL_IS_LAST ? 0 : (LL_INIT_OFFSET_T + i*MEM_FOLD_FACT +k); 
                        else
                            init_value = LL_INIT_OFFSET_T + i*MEM_FOLD_FACT +k;         
    
                        behave_mem_acc.sram[i*MEM_FOLD_FACT +k]     = PROT_FUNC.mby_mgm_functions.calc_prot_ext(init_value);
                      end
    
    
                 end //  for (int k = 0; k < MEM_FOLD
    
               end //for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
              `endif // MBY_MSH_BEHAVE_MEMS
    
            memory_init_sync(); 
    
    
         end
       endtask
    
    // Sync behave_mem_acc into behave/asic mem
    task automatic memory_init_sync ( );
        parameter WR_SEGMENT =  MEM_NUM_COL*MEM_COL_W/MEM_FOLD_FACT;
    
        logic    [MEM_WIDTH-1:0]   temp_behave_mem_ext_data;
        logic    [(MEM_WIDTH/MEM_COL_W +1 )*MEM_COL_W* MEM_FOLD_FACT-1:0]   temp_ph_mem_ext_data_folded;
        logic    [(MEM_WIDTH/MEM_COL_W +1 )*MEM_COL_W* MEM_FOLD_FACT-1:0]   temp_behave_mem_ext_data_folded;
        logic    [MEM_FOLD_FACT-1:0]                                        invalid_line; 
       
        begin
              `ifdef MBY_MSH_BEHAVE_MEMS
              for (int i = 0; i < MEM_LOGICAL_DEPTH; i = i + 1) begin
                 temp_behave_mem_ext_data  = behave_mem_acc.sram[i] ;         
                 if (temp_behave_mem_ext_data !== {(MEM_WIDTH){1'bx}} ) begin
                          temp_behave_mem_ext_data_folded  [MEM_TOTAL_W-1:0]  = behave_mem_acc.sram[i];
                          temp_ph_mem_ext_data_folded [0 +: MEM_TOTAL_W] = 0; 
                          temp_ph_mem_ext_data_folded [0 +: MEM_TOTAL_W] =  PROT_FUNC.mby_mgm_functions.calc_prot_ext(temp_behave_mem_ext_data_folded[0 +: MEM_TOTAL_W]) ; 
                          behave_mem.sram[i]   = temp_ph_mem_ext_data_folded[0 +: MEM_TOTAL_W];
                    //  $display("addr = %h   =  behave_mem_acc.sram[i]  %h PEOT_FUNC= %h ", i, behave_mem_acc.sram[i] ,  PROT_FUNC.mby_mgm_functions.calc_prot_ext(temp_behave_mem_ext_data_folded));
                 end //  if (temp_behave_mem_ext_data !== {(MEM_I
              `else // MBY_MSH_BEHAVE_MEMS
              for (int i = 0; i < MEM_TOTAL_DEPTH; i = i + 1) begin
                 invalid_line = 0; 
                 for (int k = 0; k < MEM_FOLD_FACT; k = k + 1) begin   
    
                   temp_behave_mem_ext_data  = behave_mem_acc.sram[i*MEM_FOLD_FACT +k] ;         
                   if (temp_behave_mem_ext_data !== {(MEM_WIDTH){1'bx}} ) begin
    temp_behave_mem_ext_data_folded[k*MEM_TOTAL_W +: MEM_TOTAL_W]  = behave_mem_acc.sram[i*MEM_FOLD_FACT +k];
                           temp_ph_mem_ext_data_folded [k*WR_SEGMENT +: WR_SEGMENT] = 0; 
                           temp_ph_mem_ext_data_folded [k*WR_SEGMENT +: MEM_TOTAL_W] =  PROT_FUNC.mby_mgm_functions.calc_prot_ext(temp_behave_mem_ext_data_folded[k*MEM_TOTAL_W +: MEM_TOTAL_W]) ; 
                          // $display("i = %d  \n k = %d \n temp_behave_mem_ext_data = %h \n temp_ph_mem_ext_data_folded = %h \n ", i,k, temp_behave_mem_ext_data , temp_ph_mem_ext_data_folded );
                       end
                   else
                       invalid_line[k] = 1;                               
               end //  for (int k = 0; k < MEM_FOLD
    
    
                 case (i/MEM_ROW_W) 
    
         0 :begin
         for (int j = 0;j < MEM_NUM_COL ; j = j+1) 
             case (j)
                                    0: begin
                                          ram_row_0_col_0.uut.mem_core_array[i%MEM_ROW_W] = temp_ph_mem_ext_data_folded[(j)*(MEM_COL_W)+:MEM_COL_W]        ;
                                    end
                                    default : begin
                                       $error("SOMTING IS wrong with parameters given for col number %d ",j);
                                     end
              endcase
                      
        end // 0 
    
    
                 endcase
    
               `endif // MBY_MSH_BEHAVE_MEMS
               end //for (int i = 0; i < MEM_DEPTH; i = i + 1) begin
    
        end // task
        
    
              
      endtask // automatic
        
    `endif //  `ifdef INTEL_SIMONLY

   
endmodule
