// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with Ingress (IGR) for sharing clean pod read requests

interface igr_egr_cpod_if ();
  //import mby_egr_pkg::*;
    
    
  
  // Pointer cache read/rsp
 
  //Pod fetch request
  logic                    igr_egr_rreq_valid;
  logic [4:0]              igr_egr_req_id;     //[4:0]
  logic [23:0]             igr_egr_pod_ptr;    //[23:0]

  logic                    egr_igr_rreq_ack;    
  
  //Fetch ack  
  logic                    egr_igr_fetch_valid;
  logic [4:0]              egr_igr_fetch_id;
  
  
  //Fetch data
  logic                    egr_igr_cptr_valid [0:2];
  logic [19:0]             egr_igr_cptr       [0:2];
  logic [1:0]              egr_igr_cptr_word  [0:2];
  logic                    egr_igr_cptr_sema  [0:2];        


  
    
modport egr(
    input  igr_egr_rreq_valid,
    input  igr_egr_req_id,
    input  igr_egr_pod_ptr,
    output egr_igr_rreq_ack,    
    
    output egr_igr_fetch_valid,
    output egr_igr_fetch_id,
    
    output egr_igr_cptr_valid,
    output egr_igr_cptr,    
    output egr_igr_cptr_word,
    output egr_igr_cptr_sema   
    );

modport igr(
    output igr_egr_rreq_valid,
    output igr_egr_req_id,
    output igr_egr_pod_ptr,
    input  egr_igr_rreq_ack,    
    
    input  egr_igr_fetch_valid,
    input  egr_igr_fetch_id,
    
    input  egr_igr_cptr_valid,
    input  egr_igr_cptr,    
    input  egr_igr_cptr_word,
    input  egr_igr_cptr_sema   
    );

endinterface : igr_egr_cpod_if
