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
// -- Project Name : Madison Bay(MBY)
// -- Description  : This is the package including all the structs used only in 
//                   Packet Buffer(PB) block.
//------------------------------------------------------------------------------


package mby_igr_pb_pkg;

  localparam PB_BANKS     = 4;
  localparam PB_BANK_D    = 1024;
  localparam PB_BANK_ADRS = $clog2(PB_BANK_D);
  localparam PB_SHELL_DATA_W = 644;
  localparam MAX_DPTH400  = 4095;
  localparam MAX_DPTH200  = 2047;
  localparam MAX_DPTH100  = 1023;
 
  typedef struct packed {
    logic                        rd_en;
    logic                        wr_en;
    logic  [PB_BANK_ADRS-1:0]    adr;
    logic  [PB_SHELL_DATA_W-1:0] wr_data;
  } pb_shell_ctrl_wdata_t;
  
  typedef struct packed {
    logic                        rd_valid;
    logic  [PB_SHELL_DATA_W-1:0] rd_data;
  } pb_shell_rdata_t;


/*
//  import  hlp_pkg::*;
//  import  hlp_register_constants_pkg::*;

  localparam  N_CHUNKS       = 8;                    // 8
  localparam  W_N_CHUNKS     = $clog2(N_CHUNKS);           // 3
  localparam  W_CHUNK_DATA   = 64;                         // 64b data
  localparam  W_CHUNK_ECC    = 8;                          // 8b ecc
  localparam  W_CHUNK        = W_CHUNK_DATA + W_CHUNK_ECC; // 72b
  localparam  W_CHUNK_DATA_B = (W_CHUNK_DATA+7) / 8;       // 8B
  localparam  D_SECTOR       = int'(N_SEGS / N_SECTORS);   // sector depth 2k
  localparam  W_SECT_ADDR    = $clog2(D_SECTOR);           // 11
  localparam  W_N_SECTORS    = $clog2(N_SECTORS);          // 4
  localparam  W_PM_ADDR      = $clog2(N_SEGS);             // 15 for 24k
  localparam  N_HEAD_B       = 64;
  localparam  N_HEAD_CHUNKS  = (N_HEAD_B+7) / 8;

  localparam  I_DELAY_CTRL_IN  = 3; // num of pipelines on i_pm_ctrl 
  localparam  I_DELAY_PM_WRITE = 0; // num of pipelines on i_pm_write 
  localparam  N_RPL_JUNC_PIPE  = 2; // num of added pipelines between rpl/mgmt_junc
  localparam  N_INT_PIPE       = 2; 
  localparam  SECT_PIPE_MOD = 4; // num of sectors with a pipeline inserted
  localparam  N_SECT_PIPE   = N_SECTORS / SECT_PIPE_MOD; // num of sector pipelines inserted

  typedef struct packed {
    logic                     mgmt_rd_v;
    logic  [N_CHUNKS-1:0]     en_rd;
    logic  [W_SECT_ADDR-1:0]  a_rd;
    logic  [N_CHUNKS-1:0]     en_wr;
    logic  [W_SECT_ADDR-1:0]  a_wr;
    logic  [N_SECTORS-1:0]    sect_rw_upper;
    logic  [N_SECTORS-1:0]    sect_rw_lower;
    logic  [N_SECTORS-1:0]    sect_en_upper;
    logic  [N_SECTORS-1:0]    sect_en_lower;
  } ctrl_ripple_gated_t;

  typedef struct packed {
    logic                v;
    ctrl_ripple_gated_t  g;
  } ctrl_ripple_t;

  typedef logic [W_CHUNK_DATA-1:0] chunk_data_t;
  typedef logic [W_CHUNK_ECC -1:0] chunk_ecc_t;

  typedef logic [1:0] ecc_err_t;  // 01:cerr, 11:uerr

  typedef struct packed {
    chunk_ecc_t   ecc;
    chunk_data_t  data;
  } sram_data_t;

  typedef struct packed {
    logic       [N_CHUNKS-1:0] v;
    sram_data_t [N_CHUNKS-1:0] d;    
  } sect_rw_data_bus_t;

  typedef struct packed {
    logic                       v;
    ecc_err_t  [N_CHUNKS-1:0]   err;
    logic      [W_PM_ADDR-1:0]  addr;
  } ecc_err_log_t;
*/

endpackage

