// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
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
// -- Description :  A template demonstration package file
// --
// ---------------------------------------------------------------------------------------------------------------------

package tmpl_pkg;


//-----------------------------------------------------------------------------
// parameters
//-----------------------------------------------------------------------------

localparam NUM_INPUTS       = 4;        // number of request input ports
localparam NUM_OUTPUTS      = 4;        // number of request output ports

localparam DATA_WIDTH       = 34;       // width of data

localparam FIFO_DEPTH       = 80;       // depth of data and request FIFOs 
localparam RF_PIPE          = 1;        // 1=Fifo uses Pipelined RF, 0=no pipe stage

//-----------------------------------------------------------------------------
// derived parameters
//-----------------------------------------------------------------------------

localparam LOG_NUM_INPUTS   = $clog2(NUM_INPUTS);   // log of number of inputs 
localparam LOG_NUM_OUTPUTS  = $clog2(NUM_OUTPUTS);  // log of number of outputs 
localparam LOG_FIFO_DEPTH   = $clog2(FIFO_DEPTH);   // log of FIFO depth 

//-----------------------------------------------------------------------------
// simple typedefs
//-----------------------------------------------------------------------------

typedef logic           [NUM_INPUTS-1:0]        inp_t;              // decoded input number
typedef logic           [NUM_OUTPUTS-1:0]       outp_t;             // decoded output number
typedef logic           [LOG_NUM_INPUTS-1:0]    enc_inp_t;          // encoded input number
typedef logic           [LOG_NUM_OUTPUTS-1:0]   enc_outp_t;         // encoded output number
//typedef logic           [DATA_WIDTH-1:0]        data_t;             // chunk of data 
typedef logic           [LOG_FIFO_DEPTH-1:0]    fifo_addr_t;        // FIFO address
typedef logic           [FIFO_DEPTH-1:0]        fifo_ptr_t;         // FIFO pointer (not encoded)

//-----------------------------------------------------------------------------
// enum typedefs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// struct and union typedefs
//-----------------------------------------------------------------------------
//  - struct typedefs enable fields to be specified by name (instead of bit position) in RTL and in debuggers 
//  - union typedefs enable specification of multiple different encodings for a single set of bits 

// packet type 0 encoding 

typedef struct packed {
    logic [3:0]             one_field;          // arbitrary example field
    logic [DATA_WIDTH-5:0]  two_field;          // arbitrary example field
} pkt_0_t;

// packet type 1 encoding 

typedef struct packed {
    logic [7:0]             red_field;          // arbitrary example field
    logic [DATA_WIDTH-9:0]  blue_field;         // arbitrary example field
} pkt_1_t;


// two different data formats 

typedef union packed {          
    pkt_0_t  data_t0;           // data encoded with packet type 0 encoding
    pkt_1_t  data_t1;           // data encoded with packet type 1 encoding
} data_t;  

// incoming request

typedef struct packed {
    logic       vld;
    enc_outp_t  outp;    
    data_t      data;
} req_in_t;

// outgoing request

typedef struct packed {
    logic       vld;
    enc_inp_t   inp;    
    data_t      data;
} req_out_t;

//-----------------------------------------------------------------------------
// array typedefs
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// constants
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

endpackage : tmpl_pkg
