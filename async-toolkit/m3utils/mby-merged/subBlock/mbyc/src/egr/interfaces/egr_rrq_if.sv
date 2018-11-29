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
// -- Description  : Read Request Interface
//                   For connecting Read Requestors to the Mesh Read Interface
//------------------------------------------------------------------------------

interface egr_rrq_if #(parameter N_RREQS = 1)();
    import mby_egr_pkg::*;
    import shared_pkg::*;

////////////////// PARAMS AND STRUCTS
// Service try_put_pkt_rreq
localparam W_CLIENT_ID  = 3; // Read Request client sel (CPB, 2xPRC[2], TDB) (3)
localparam W_DATA_WD_ID = 9; // Data Word ID (9)

typedef struct packed {
    logic [W_CLIENT_ID-1:0]   client_id; // [12:10] Client ID selector
    logic                         spare; // [9]     Spare bits (for 13 bit req id)
    logic [W_DATA_WD_ID-1:0] data_wd_id; // [8:0]   Data Word ID
} req_id_t;

typedef struct packed {
    seg_handle_t seg_handle; // [40:17] Segment Pointer handle w/ semaphores
    wd_sel_t         wd_sel; // [16:15] Word select
    req_id_t         req_id; // [14:2]  Request ID w/client id and packet id
    logic     service_class; // [1]     Service class: 0: SAF, 1: VCT
    logic                mc; // [0]     Multicast bit
} wd_rreq_t;

////////////////// SIGNALS
// Service try_put_word_rreq
wd_rreq_t [N_RREQS-1:0]       wd_rreq; // Word Read Request 
logic     [N_RREQS-1:0] wd_rreq_valid; // Word Read Request Valid
logic     [N_RREQS-1:0]   wd_rreq_ack; // Word Read Request Acknowledge

////////////////// MODPORTS
// Requestor of mesh read services
modport requestor(
    output       wd_rreq,
    output wd_rreq_valid,
    input    wd_rreq_ack
    );

// Provider of mesh read services
modport mri(
    input         wd_rreq,
    input   wd_rreq_valid,
    output    wd_rreq_ack
    );

endinterface : egr_rrq_if
