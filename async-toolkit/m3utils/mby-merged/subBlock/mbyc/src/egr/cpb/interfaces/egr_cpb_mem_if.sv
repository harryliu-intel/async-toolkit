// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

interface egr_cpb_mem_if ();

localparam CPB_RREQ_BUF_W_DATA = 24;
localparam CPB_RREQ_BUF_W_DEEP = 3;
localparam CPB_RREQ_BUF_W_INST = 1;

localparam CPB_RRSP_BUF_W_DATA = 512;
localparam CPB_RRSP_BUF_W_DEEP = 6;
localparam CPB_RRSP_BUF_W_INST = 3;

logic [$clog2(CPB_RREQ_BUF_W_DEEP)-1:0]   rreq_buf_adr       [CPB_RREQ_BUF_W_INST-1:0] ; 
logic                                     rreq_buf_rd_en     [CPB_RREQ_BUF_W_INST-1:0] ; 
logic         [CPB_RREQ_BUF_W_DATA-1:0]   rreq_buf_wr_data   [CPB_RREQ_BUF_W_INST-1:0] ; 
logic                                     rreq_buf_wr_en     [CPB_RREQ_BUF_W_INST-1:0] ; 
logic         [CPB_RREQ_BUF_W_DATA-1:0]   rreq_buf_rd_data   [CPB_RREQ_BUF_W_INST-1:0] ; 
logic                                     rreq_buf_rd_valid  [CPB_RREQ_BUF_W_INST-1:0] ;


logic [$clog2(CPB_RRSP_BUF_W_DEEP)-1:0]   rrsp_buf_adr       [CPB_RRSP_BUF_W_INST-1:0] ; 
logic                                     rrsp_buf_rd_en     [CPB_RRSP_BUF_W_INST-1:0] ; 
logic         [CPB_RRSP_BUF_W_DATA-1:0]   rrsp_buf_wr_data   [CPB_RRSP_BUF_W_INST-1:0] ; 
logic                                     rrsp_buf_wr_en     [CPB_RRSP_BUF_W_INST-1:0] ; 
logic         [CPB_RRSP_BUF_W_DATA-1:0]   rrsp_buf_rd_data   [CPB_RRSP_BUF_W_INST-1:0] ; 
logic                                     rrsp_buf_rd_valid  [CPB_RRSP_BUF_W_INST-1:0] ;


modport mem (
             // rreq
             input   rreq_buf_adr      , 
             input   rreq_buf_rd_en    , 
             input   rreq_buf_wr_data  , 
             input   rreq_buf_wr_en    , 
             output  rreq_buf_rd_data  , 
             output  rreq_buf_rd_valid ,
             // rrsp
             input   rrsp_buf_adr      , 
             input   rrsp_buf_rd_en    , 
             input   rrsp_buf_wr_data  , 
             input   rrsp_buf_wr_en    , 
             output  rrsp_buf_rd_data  , 
             output  rrsp_buf_rd_valid 
);

modport cln (
             // rreq
             output  rreq_buf_adr     , 
             output  rreq_buf_rd_en   ,
             output  rreq_buf_wr_data ,
             output  rreq_buf_wr_en   ,
             input   rreq_buf_rd_data ,
             input   rreq_buf_rd_valid,
             // rrsp
             output  rrsp_buf_adr     , 
             output  rrsp_buf_rd_en   ,
             output  rrsp_buf_wr_data ,
             output  rrsp_buf_wr_en   ,
             input   rrsp_buf_rd_data ,
             input   rrsp_buf_rd_valid
);

endinterface:egr_cpb_mem_if

