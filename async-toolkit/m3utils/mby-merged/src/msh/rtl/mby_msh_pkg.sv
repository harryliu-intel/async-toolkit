///
///  INTEL CONFIDENTIAL
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
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Description :  Mesh package file 
// --
// ---------------------------------------------------------------------------------------------------------------------

package mby_msh_pkg;


//-----------------------------------------------------------------------------
// parameters
//-----------------------------------------------------------------------------

localparam NUM_MSH_ROWS         = 16;       // number of mesh rows 
localparam NUM_MSH_COLS         = 8;        // number of mesh columns 
localparam NUM_MSH_PLANES       = 2;        // number of mesh planes
localparam MSH_DATA_WIDTH       = 64 * 8;   // width of mesh data
localparam MSH_ECC_WIDTH        = 8 * 8;    // width of ECC associated with mesh data buses
localparam MSH_ID_WIDTH         = 16;       // width of IDs sent with rd reqs/rsps
localparam NUM_MSH_DP_CHUNKS    = 2;        // the data path is broken bitwise into this number of chunks 
localparam MSH_DBUS_WIDTH       = MSH_DATA_WIDTH + MSH_ECC_WIDTH;
localparam MSH_DP_CHUNK_WIDTH   = MSH_DBUS_WIDTH / NUM_MSH_DP_CHUNKS; // width of data path chunk


localparam MSH_NODE_MEM_BYTES        = 1024 * 1024; // each mesh node contains 1MB of memory 
localparam NUM_MSH_NODE_MEM_BANKS    = 4;           // the memory is broken into this number of banks 

//-----------------------------------------------------------------------------
// derived parameters
//-----------------------------------------------------------------------------

localparam LOG_NUM_MSH_ROWS            = $clog2(NUM_MSH_ROWS);
localparam LOG_NUM_MSH_COLS            = $clog2(NUM_MSH_COLS);
localparam MSH_NODE_ADDR_WIDTH         = $clog2(MSH_NODE_MEM_BYTES);
localparam LOG_NUM_MSH_NODE_MEM_BANKS  = $clog2(NUM_MSH_NODE_MEM_BANKS); 
localparam MSH_NODE_BANK_IDX_WIDTH     = MSH_NODE_ADDR_WIDTH - LOG_NUM_MSH_NODE_MEM_BANKS; 


//-----------------------------------------------------------------------------
// simple typedefs
//-----------------------------------------------------------------------------

typedef logic   [MSH_DATA_WIDTH-1:0]        msh_data_t;
typedef logic   [MSH_ECC_WIDTH-1:0]         msh_ecc_t;
typedef logic   [MSH_DP_CHUNK_WIDTH-1:0]    msh_dp_chunk_t;
typedef logic   [MSH_ID_WIDTH-1:0]          msh_rd_id_t;

typedef logic   [LOG_NUM_MSH_ROWS-1:0]      msh_row_t;
typedef logic   [LOG_NUM_MSH_COLS-1:0]      msh_col_t;
    
typedef logic   [NUM_MSH_COLS-1:0]          msh_row_crdts_t;

//-----------------------------------------------------------------------------
// enum typedefs
//-----------------------------------------------------------------------------

typedef enum logic {west, east}             msh_side_t; 


//-----------------------------------------------------------------------------
// struct and union typedefs
//-----------------------------------------------------------------------------
//  - struct typedefs enable fields to be specified by name (instead of bit position) in RTL and in debuggers 
//  - union typedefs enable specification of multiple different encodings for a single set of bits 

typedef struct packed {
    logic [LOG_NUM_MSH_NODE_MEM_BANKS-1:0] bank_num;
    logic [MSH_NODE_BANK_IDX_WIDTH-1:0]    bank_idx;
} msh_struct_addr_t;

typedef union packed {
    logic [MSH_NODE_ADDR_WIDTH-1:0]    addr_flat;
    msh_struct_addr_t                  addr_struct;
} mshnd_mem_addr_t;

typedef struct packed {
    msh_data_t     data;
    msh_ecc_t      ecc;
} msh_dbus_t; 

typedef struct packed {
    logic               vld;            // this message is valid
    msh_col_t           node_col;       // destination node column
    msh_row_t           node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
} msh_row_wr_req_t;

typedef struct packed {
    logic               vld;            // this message is valid
    msh_row_t           node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
} msh_col_wr_req_t;

typedef struct packed {
    logic               vld;            // this message is valid
    msh_rd_id_t         id;             // read identifier
    msh_col_t           mpg_col;        // source megaport column
    msh_col_t           node_col;       // destination node column
    msh_row_t           node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
    logic               sema_vld;       // if valid, read should wait for write semaphore
    logic               sema_val;       // the value of the write semaphore to wait for
} msh_row_rd_req_t;

typedef struct packed {
    logic               vld;            // this message is valid
    msh_rd_id_t         id;             // read identifier
    msh_row_t           mpg_row;        // source megaport row
    msh_side_t          mpg_side;       // source megaport side
    msh_row_t           node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
    logic               sema_vld;       // if valid, read should wait for write semaphore
    logic               sema_val;       // the value of the write semaphore to wait for
} msh_col_rd_req_t;

typedef struct packed {
    logic           vld;                // this message is valid
    msh_rd_id_t     id;                 // read identifier
    msh_side_t      mpg_side;           // destination megaport side
    msh_row_t       mpg_row;            // destination megaport row
} msh_col_rd_rsp_t;

typedef struct packed {
    logic           vld;                // this message is valid
    msh_rd_id_t     id;                 // read identifier
} msh_row_rd_rsp_t;


//-----------------------------------------------------------------------------
// array typedefs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// constants
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

endpackage : mby_msh_pkg
