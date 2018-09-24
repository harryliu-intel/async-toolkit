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

package mesh_pkg;


//-----------------------------------------------------------------------------
// parameters
//-----------------------------------------------------------------------------

localparam NUM_MESH_ROWS        = 16;       // number of mesh rows 
localparam NUM_MESH_COLS        = 8;        // number of mesh columns 
localparam NUM_MESH_PLANES      = 2;        // number of mesh planes
localparam MESH_DATA_WIDTH      = 64 * 8;   // width of mesh data
localparam MESH_ECC_WIDTH       = 8 * 8;    // width of ECC associated with mesh data buses
localparam MESH_ID_WIDTH        = 16;       // width of IDs sent with rd reqs/rsps
localparam NUM_MESH_DP_CHUNKS   = 4;        // the data path is broken bitwise into this number of chunks 


localparam MESH_NODE_MEM_BYTES        = 1024 * 1024; // each mesh node contains 1MB of memory 
localparam NUM_MESH_NODE_MEM_BANKS    = 4;           // the memory is broken into this number of banks 


//-----------------------------------------------------------------------------
// derived parameters
//-----------------------------------------------------------------------------

localparam LOG_NUM_MESH_ROWS            = $clog2(NUM_MESH_ROWS);
localparam LOG_NUM_MESH_COLS            = $clog2(NUM_MESH_COLS);
localparam MESH_NODE_ADDR_WIDTH         = $clog2(MESH_NODE_MEM_BYTES);
localparam LOG_NUM_MESH_NODE_MEM_BANKS  = $clog2(NUM_MESH_NODE_MEM_BANKS); 
localparam MESH_NODE_BANK_IDX_WIDTH     = MESH_NODE_ADDR_WIDTH - LOG_NUM_MESH_NODE_MEM_BANKS; 


//-----------------------------------------------------------------------------
// simple typedefs
//-----------------------------------------------------------------------------

typedef logic   [MESH_DATA_WIDTH-1:0]       mesh_data_t;
typedef logic   [MESH_ECC_WIDTH-1:0]        mesh_ecc_t;
typedef logic   [MESH_ID_WIDTH-1:0]         mesh_id_t;

typedef logic   [LOG_NUM_MESH_ROWS-1:0]     mesh_row_t;
typedef logic   [LOG_NUM_MESH_COLS-1:0]     mesh_col_t;

typedef logic   [NUM_MESH_COLS-1:0]         mesh_row_crdts_t;

//-----------------------------------------------------------------------------
// enum typedefs
//-----------------------------------------------------------------------------

typedef enum logic {west, east}             mesh_side_t; 


//-----------------------------------------------------------------------------
// struct and union typedefs
//-----------------------------------------------------------------------------
//  - struct typedefs enable fields to be specified by name (instead of bit position) in RTL and in debuggers 
//  - union typedefs enable specification of multiple different encodings for a single set of bits 

typedef struct packed {
    logic [LOG_NUM_MESH_NODE_MEM_BANKS-1:0] bank_num;
    logic [MESH_NODE_BANK_IDX_WIDTH-1:0]    bank_idx;
} mesh_struct_addr_t;

typedef union packed {
    logic [MESH_NODE_ADDR_WIDTH-1:0]    addr_flat;
    mesh_struct_addr_t                  addr_struct;
} mshnd_mem_addr_t;

typedef struct packed {
    mesh_data_t     data;
    mesh_ecc_t      ecc;
} mesh_dbus_t; 

typedef struct packed {
    logic               vld;            // this message is valid
    mesh_col_t          node_col;       // destination node column
    mesh_row_t          node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
} mesh_row_wr_req_t;

typedef struct packed {
    logic               vld;            // this message is valid
    mesh_row_t          node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
} mesh_col_wr_req_t;

typedef struct packed {
    logic               vld;            // this message is valid
    mesh_id_t           id;             // read identifier
    mesh_col_t          mpg_col;        // source megaport column
    mesh_col_t          node_col;       // destination node column
    mesh_row_t          node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
    logic               sema_vld;       // if valid, read should wait for write semaphore
    logic               sema_val;       // the value of the write semaphore to wait for
} mesh_row_rd_req_t;

typedef struct packed {
    logic               vld;            // this message is valid
    mesh_id_t           id;             // read identifier
    mesh_row_t          mpg_row;        // source megaport row
    mesh_side_t         mpg_side;       // source megaport side
    mesh_row_t          node_row;       // destination node row
    mshnd_mem_addr_t    mem_addr;       // address within mesh node memory
    logic               sema_vld;       // if valid, read should wait for write semaphore
    logic               sema_val;       // the value of the write semaphore to wait for
} mesh_col_rd_req_t;

typedef struct packed {
    logic           vld;                // this message is valid
    mesh_id_t       id;                 // read identifier
    mesh_side_t     mpg_side;           // destination megaport side
    mesh_row_t      mpg_row;            // destination megaport row
} mesh_col_rd_rsp_t;

typedef struct packed {
    logic           vld;                // this message is valid
    mesh_id_t       id;                 // read identifier
} mesh_row_rd_rsp_t;


//-----------------------------------------------------------------------------
// array typedefs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// constants
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

endpackage : mesh_pkg
