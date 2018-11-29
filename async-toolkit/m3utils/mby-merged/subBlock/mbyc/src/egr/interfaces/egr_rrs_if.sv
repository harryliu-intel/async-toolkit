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
//------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : Read Response Interface
//                   For connecting the Mesh Read Interface to the consumers 
//                   of the responses
//------------------------------------------------------------------------------

interface egr_rrs_if #(parameter N_RRSPS = 3)();
    import mby_egr_pkg::*;
    import shared_pkg::*;

////////////////// PARAMS AND STRUCTS
//TODO Merge signals with rrq_if in package
// Service try_put_pkt_rreq
localparam W_CLIENT_ID  = 3; // Read Request client sel (CPB, 2xPRC[2], TDB) (3)
localparam W_DATA_WD_ID = 9; // Data Word ID (9)

typedef struct packed {
    logic [W_CLIENT_ID-1:0]   client_id; // [12:10] Client ID selector
    logic                         spare; // [9]     Spare bits (for 13 bit req id)
    logic [W_DATA_WD_ID-1:0] data_wd_id; // [8:0]   Data Word ID
} req_id_t;

////////////////// SIGNALS
// Service try_put_word_rreq
req_id_t    [N_RRSPS-1:0]    rrsp_wd_id; // Read Response Word ID
data_word_t [N_RRSPS-1:0]       rrsp_wd; // Read Response Word
logic       [N_RRSPS-1:0] rrsp_wd_valid; // Read Response Word Valid
logic       [N_RRSPS-1:0] rrsp_wd_stall; // Read Response Word Stall

////////////////// MODPORTS
// Requestor of mesh read services
modport requestor(
    input     rrsp_wd_id,
    input        rrsp_wd,
    input  rrsp_wd_valid,
    output rrsp_wd_stall
    );

// Provider of mesh read services
modport mri(
    output    rrsp_wd_id,
    output       rrsp_wd,
    output rrsp_wd_valid,
    input  rrsp_wd_stall
    );

endinterface : egr_rrs_if
