///------------------------------------------------------------------------------
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
// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Steve Olson
// -- Project Name : Madison Bay
// -- Description : AHB to RX PPE register interface
// --
// -------------------------------------------------------------------

interface ahb_rx_ppe_if
();
    logic                                  ahb_sel;     // Slave select
    logic                                  ahb_wr;      // Write/Read
    logic  [shared_pkg::W_MGMT_ADDR-1:0]   ahb_addr;    // Address
    logic  [shared_pkg::W_MGMT_DATA64-1:0] ahb_wr_data; // Write Data (from AHB master to   PPE)
    logic  [shared_pkg::W_MGMT_DATA64-1:0] ahb_rd_data; // Read  Data (to   AHB master from PPE)
    logic                                  ahb_rd_ack;  // Read acknowledge

modport ppe(
    input   ahb_sel,
    input   ahb_wr,
    input   ahb_addr,
    input   ahb_wr_data,
    output  ahb_rd_data,
    output  ahb_rd_ack
);

modport ahb(
    output  ahb_sel,
    output  ahb_wr,
    output  ahb_addr,
    output  ahb_wr_data,
    input   ahb_rd_data,
    input   ahb_rd_ack
);

endinterface : ahb_rx_ppe_if
