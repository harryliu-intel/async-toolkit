// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

interface egr_pfs_mem_if  ();
    
localparam W_DATA = 32;
localparam W_DEEP = 1024;
localparam W_INST = 4;

// Signals for rrequ_fifo
logic [$clog2(W_DEEP)-1:0] rd_adr    [W_INST-1:0] ; 
logic                      rd_en     [W_INST-1:0] ; 
logic [$clog2(W_DEEP)-1:0] wr_adr    [W_INST-1:0] ; 
logic         [W_DATA-1:0] wr_data   [W_INST-1:0] ; 
logic                      wr_en     [W_INST-1:0] ; 
logic         [W_DATA-1:0] rd_data   [W_INST-1:0] ; 
logic                      rd_valid  [W_INST-1:0] ;

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
    
endinterface:egr_pfs_mem_if
