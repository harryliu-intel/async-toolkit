// ---------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
// ---------------------------------------------------------------------------
//
// Created By: Dhivya Sankar
// Created On:  10/24/2018
// Description: MBY Mesh defines
//
// This is a place for common Constants, Typedefs, Functions, etc..
//
// ----------------------------------------------------------------------------

parameter  NUM_PLANES             = 2;
parameter  NUM_MSH_ROWS          = 3;     // number of mesh rows 
parameter  NUM_MSH_COLS          = 3;      // number of mesh columns 
parameter  W_SEG_PTR             = 20;     // Segment Pointer Size
parameter  W_SEMA                = 4;      // Semaphore Bits Size
parameter  W_WD_SEL              = 2;      // Segment Word Selector Size
parameter  W_REQ_ID              = 16;     // Write/Read Request ID Width TODO: verify it
parameter  MSH_DATA_WIDTH        = 64 * 8; // width of mesh data
parameter  NUM_MSH_ROW_PORTS     = 3;      // number of mesh ports per row on each side (east and west)
parameter  NUM_MSH_COL_PORTS_WR  = 3;      // number of mesh ports per row on each side (east and west)
parameter  NUM_MSH_COL_PORTS_R   = 4;      // number of mesh ports per row on each side (east and west)

//TODO
//class and mcast field

logic mcast, cclk;
typedef enum {ROW, COL} port_type_e; 
typedef enum {REQOP, RSPOP, DATA} bus_type_e;
typedef enum {RDREQ, WRREQ, RDRSP} req_type_e;


//interface signal MGP -> MIM
logic [W_REQ_ID-1:0]        req_id;   // Write/Read Request ID type
logic [W_SEG_PTR-1:0]       seg_ptr;  // Segment Pointer type
logic [W_WD_SEL-1:0]        wd_sel;   // Segment Word Selector type
logic [W_SEMA-1:0]          sema;     // Semaphore Bits type
logic                       valid;
logic [MSH_DATA_WIDTH-1:0]  msh_data; // Mesh Data



