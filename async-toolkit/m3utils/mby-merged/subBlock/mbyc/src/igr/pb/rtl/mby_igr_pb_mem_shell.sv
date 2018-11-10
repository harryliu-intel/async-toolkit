//------------------------------------------------------------------------------
//
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
// -- Author       : Edward C. Ross
// -- Project Name : Madison Bay
// -- Description  : This is a wrapper for the mgm generated memory shells for the PB memory.
//
//------------------------------------------------------------------------------


module mby_igr_pb_mem_shell
  import mby_igr_pkg::*;
(
  input logic cclk,
  input logic rst,

  input  pb_shell_ctrl_wdata_t [PB_BANKS-1:0] i_pb_shell_ctrl_wdata, 
  output pb_shell_rdata_t      [PB_BANKS-1:0] o_pb_shell_rdata,
  
  input  [`MBY_IGR_IGR_PB0_RAM_FROM_MEM_WIDTH-1:0] i_frm_pb_mem_bus [PB_BANKS-1:0],
  output [`MBY_IGR_IGR_PB0_RAM_TO_MEM_WIDTH-1:0]   o_to_pb_mem_bus  [PB_BANKS-1:0]
);


  logic rst_mem_b;
  
// internal interfaces
logic  [PB_BANKS-1:0]       nc_bnk_init_done, nc_bnk_ecc_uerr;  // lintra s-70036  
logic   [`MBY_IGR_IGR_PB0_RAM_TO_CTL_WIDTH-1:0] nc_igr_igr_pb0_ram_to_ctl [PB_BANKS-1:0];
logic  [`MBY_IGR_IGR_PB0_RAM_TO_MEM_WIDTH-1:0]   tom_pb_mem_bus [PB_BANKS-1:0]; 
logic  [`MBY_IGR_IGR_PB0_RAM_FROM_MEM_WIDTH-1:0] frm_pb_mem_bus [PB_BANKS-1:0]; 

  assign rst_mem_b = ~rst;
  assign frm_pb_mem_bus  = i_frm_pb_mem_bus;
  assign o_to_pb_mem_bus = tom_pb_mem_bus;
generate
  genvar g_bnk;
  for(g_bnk=0; g_bnk<PB_BANKS; g_bnk++) begin: gen_mem_shell_banks
  mby_mem_igr_pb0_ram_shell_1024x644  u_mem_bank_shell_1024x644(
      //------------------- clock and reset -------------------
      .clk       ( cclk       ),
      .reset_n   ( rst_mem_b ),

      //----------------- Functional Interface ----------------
      .adr       ( i_pb_shell_ctrl_wdata[g_bnk].adr   ),
      .rd_en     ( i_pb_shell_ctrl_wdata[g_bnk].rd_en   ),
      .wr_en     ( i_pb_shell_ctrl_wdata[g_bnk].wr_en   ),
      .wr_data   ( i_pb_shell_ctrl_wdata[g_bnk].wr_data   ),

      .rd_data   (o_pb_shell_rdata[g_bnk].rd_data   ),
      .rd_valid  (o_pb_shell_rdata[g_bnk].rd_valid  ), 

      .init_done ( nc_bnk_init_done[g_bnk] ),  // not used in PB

      //--------------------- ECC Interface -------------------
      .ecc_uncor_err  ( nc_bnk_ecc_uerr[g_bnk] ),  // not used in PB

      //----------------- Memory Wrap Interface ---------------
      .igr_igr_pb0_ram_from_mem  ( frm_pb_mem_bus[g_bnk] ),
      .igr_igr_pb0_ram_to_mem    ( tom_pb_mem_bus[g_bnk] ),

      //------------------- Gen CTR Interface ( not used in PB )-----------------
      .igr_igr_pb0_ram_from_ctl  ( '0),
      .igr_igr_pb0_ram_to_ctl    ( nc_igr_igr_pb0_ram_to_ctl[g_bnk] ),

      ////------------------ Dyn Light Sleep ------------------
      .mem_ls_enter  ( 1'b0 )
    );
    end //for
endgenerate

endmodule
