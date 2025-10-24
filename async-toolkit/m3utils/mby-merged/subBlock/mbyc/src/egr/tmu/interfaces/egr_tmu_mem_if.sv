// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

interface egr_tmu_mem_if  ();
    
localparam TAG_BUF_W_DATA = 48;
localparam TAG_BUF_W_DEEP = 72;
localparam TAG_BUF_W_INST = 512;

// Signals for rrequ_fifo
logic [$clog2(TAG_BUF_W_DEEP)-1:0] rd_adr    [TAG_BUF_W_INST-1:0] ; 
logic                              rd_en     [TAG_BUF_W_INST-1:0] ; 
logic [$clog2(TAG_BUF_W_DEEP)-1:0] wr_adr    [TAG_BUF_W_INST-1:0] ; 
logic         [TAG_BUF_W_DATA-1:0] wr_data   [TAG_BUF_W_INST-1:0] ; 
logic                              wr_en     [TAG_BUF_W_INST-1:0] ; 
logic         [TAG_BUF_W_DATA-1:0] rd_data   [TAG_BUF_W_INST-1:0] ; 
logic                              rd_valid  [TAG_BUF_W_INST-1:0] ;

// memory modport
modport mem (
             input   rd_adr  ,   
             input   rd_en   ,
             input   wr_adr  ,   
             input   wr_data ,
             input   wr_en   ,
             output  rd_data ,
             output  rd_valid
            );
// client modport
modport cln (
             output  rd_adr  ,   
             output  rd_en   ,
             output  wr_adr  ,   
             output  wr_data ,
             output  wr_en   ,
             input   rd_data ,
             input   rd_valid
            );
    
endinterface:egr_tmu_mem_if
