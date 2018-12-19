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
parameter  NUM_MSH_ROWS          = 16;     // number of mesh rows 
parameter  NUM_MSH_COLS          = 8;      // number of mesh columns 
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

//interface signal MGP -> MIM
logic [NUM_MSH_ROW_PORTS][W_REQ_ID-1:0]        req_id[NUM_MSH_ROWS];   // Write/Read Request ID type
logic [NUM_MSH_ROW_PORTS][W_SEG_PTR-1:0]       seg_ptr[NUM_MSH_ROWS];  // Segment Pointer type
logic [NUM_MSH_ROW_PORTS][W_WD_SEL-1:0]        wd_sel[NUM_MSH_ROWS];   // Segment Word Selector type
logic [NUM_MSH_ROW_PORTS][W_SEMA-1:0]          sema[NUM_MSH_ROWS];     // Semaphore Bits type
logic [NUM_MSH_ROW_PORTS][MSH_DATA_WIDTH-1:0]  msh_data[NUM_MSH_ROWS]; // Mesh Data

//interface signal GMM -> MIG
logic [W_REQ_ID-1:0]        req_id_mig;   // Write/Read Request ID type
logic [W_SEG_PTR-1:0]       seg_ptr_mig;  // Segment Pointer type
logic [W_WD_SEL-1:0]        wd_sel_mig;   // Segment Word Selector type
logic [W_SEMA-1:0]          sema_mig;     // Semaphore Bits type
logic [MSH_DATA_WIDTH-1:0]  msh_data_mig; // Mesh Data

