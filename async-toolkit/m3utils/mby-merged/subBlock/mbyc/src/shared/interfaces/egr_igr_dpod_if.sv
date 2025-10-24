// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) to Ingress (IGR) for returning dirty pods

interface egr_igr_dpod_if ();
    
    
  
  // 
 
  //Pod write request
  logic                    egr_igr_wreq_valid;
  logic                    egr_igr_dpod_type;  //0 = unicast, 1 = multicast
  logic [71:0]             egr_igr_dpod_ptr;   // Ucast = 3 x 24 bit pointers
                                               // Mcast = 2 x 36 bit pointers
  logic                    igr_egr_ack;                                             

  
    
modport egr(
    output  egr_igr_wreq_valid,
    output  egr_igr_dpod_type,
    output  egr_igr_dpod_ptr,
    input   igr_egr_ack
    );

modport igr(
    input   egr_igr_wreq_valid,
    input   egr_igr_dpod_type,
    input   egr_igr_dpod_ptr,
    output  igr_egr_ack
    );

endinterface : egr_igr_dpod_if
