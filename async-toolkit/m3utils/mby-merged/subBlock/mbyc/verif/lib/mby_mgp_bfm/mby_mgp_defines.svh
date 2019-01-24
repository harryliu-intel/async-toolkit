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

parameter  NUM_PLANES            = 2;
parameter  NUM_MSH_ROWS          = 16;      // number of mesh rows //TODO : update based on plusargs
parameter  NUM_MSH_COLS          = 8 ;      // number of mesh columns 

//TODO
//class and mcast field

logic mcast, cclk;
typedef enum {ROW, COL} port_type_e; 
typedef enum {REQOP, RSPOP}  bus_type_e;
typedef enum {RDREQ, WRREQ, RDRSP} req_type_e;
