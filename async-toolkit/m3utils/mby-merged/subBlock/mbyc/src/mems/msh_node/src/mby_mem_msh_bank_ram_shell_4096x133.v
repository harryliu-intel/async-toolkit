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
//                      Automated Memory Shell Creator
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
module  mby_mem_msh_bank_ram_shell_4096x133(
        //------------------- clock and reset -------------------
        input                                   clk                     ,
        input                                   reset_n                 ,

        //----------------- Functional Interface ----------------
        input         [12-1:0]         adr                     ,
        input                                   rd_en                   ,
        input                                   wr_en                   ,
        input         [133-1:0]         wr_data                 ,
        output  wire  [133-1:0]         rd_data                 ,
        output  wire                            rd_valid                ,
        output  reg                             init_done               ,
        //--------------------- ECC Interface -------------------
        output  wire                            ecc_uncor_err           ,
        //----------------- Memory Wrap Interface ---------------
        input           [`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH-1:0]       msh_msh_bank_ram_from_mem   ,
        output  wire    [`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH-1:0] msh_msh_bank_ram_to_mem     ,
        //------------------- Gen CTR Interface -----------------
        input           [`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH-1:0]  msh_msh_bank_ram_from_ctl      ,
        output  wire    [`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH-1:0]        msh_msh_bank_ram_to_ctl    ,
        ////------------------ Dyn Light Sleep ------------------
        input                                   mem_ls_enter
);

        wire    ecc_cor_err;
        mby_mgm_master_1rw_shell    #(
                .MEM_WIDTH              (133),
                .MEM_DEPTH              (4096),
                .MEM_WR_RESOLUTION      (133),  
                .MEM_INIT_TYPE          (0),              // 1 - Const. Val. Init., 2 - LL, Other - No Init.
                .LL_INIT_OFFSET         (),  //LINA CHANGE
                .LL_IS_LAST             (),  //LINA CHANGE
                .MEM_INIT_VALUE         (133'h0),             //              Init. Val. - valid only when MEM_INIT_TYPE == 1.
                .MEM_WR_RES_PROT_FRAGM  (),
                .MEM_PROT_TYPE          (2),              // 0 - ECC, 1 - Parity, Other - No Protection.
                .MEM_PROT_RESOLUTION    (),
                .MEM_PROT_INTERLV_LEVEL (1),
                .MEM_RD_DEBUG           (0),
                .MEM_PST_ECC_GEN_SAMPLE (0),
                .MEM_PRE_ECC_CHK_SAMPLE (0),
                .MEM_PST_ECC_CHK_SAMPLE (0),
                .MEM_ECC_CHK_SYN_SAMPLE (0),
                .MEM_CTL_SYNC           (0),
                .FROM_CTL_WIDTH         (`MBY_MSH_MSH_BANK_RAM_FROM_CTL_WIDTH),
                .TO_CTL_WIDTH           (`MBY_MSH_MSH_BANK_RAM_TO_CTL_WIDTH),
                .FROM_MEM_WIDTH         (`MBY_MSH_MSH_BANK_RAM_FROM_MEM_WIDTH),
                .TO_MEM_WIDTH           (`MBY_MSH_MSH_BANK_RAM_TO_MEM_WIDTH)     
        )
        u_master_shell(
                //------------------- clock and reset -------------------
                .clk                    (clk)                           ,
                .reset_n                (reset_n)                       ,
                .sync_reset_n           (1'b0)          ,
                //----------------- Functional Interface ----------------
                .adr                    (adr)                           ,
                .rd_en                  (rd_en)                         ,
                .wr_en                  (wr_en)                         ,
                .wr_data                (wr_data)                       ,
                .rd_data                (rd_data)                       ,
                .rd_valid               (rd_valid)                      ,
                .init_done              (init_done)                     ,
                //--------------------- ECC Interface -------------------
                .ecc_cor_err            (ecc_cor_err)                   ,
                .ecc_uncor_err          (ecc_uncor_err)                 ,
                //----------------- Memory Wrap Interface ---------------
                .wrap_shell_from_mem    (msh_msh_bank_ram_from_mem)         ,
                .wrap_shell_to_mem      (msh_msh_bank_ram_to_mem)           ,
                //------------------- Gen CTR Interface -----------------
                .ctl_shell_to_mem       (msh_msh_bank_ram_from_ctl)            ,
                .ctl_shell_from_mem     (msh_msh_bank_ram_to_ctl)          ,
                //-------------------- Dyn Light Sleep ------------------
                .mem_ls_enter           (mem_ls_enter)
        );// u_master_1rw_shell
        
endmodule
