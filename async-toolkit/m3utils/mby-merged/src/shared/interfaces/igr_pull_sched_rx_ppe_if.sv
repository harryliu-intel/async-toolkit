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
// -- Author : Jon Bagge <jon.bagge@intel.com>
// -- Project Name : Madison Bay
// -- Description : Ingress Pre PPE to RX PPE interface
// --
// -------------------------------------------------------------------

interface igr_pull_sched_rx_ppe_if
();
logic           intf0_valid;    //interface 0 read valid
logic   [14:0]  intf0_addr;     //interface 0 read address

logic           intf1_valid;    //interface 1 read valid
logic   [14:0]  intf1_addr;     //interface 1 read address

modport igr(
    output  intf0_valid,
    output  intf0_addr,
    output  intf1_valid,
    output  intf1_addr
);

modport ppe(
    input   intf0_valid,
    input   intf0_addr,
    input   intf1_valid,
    input   intf1_addr
);

endinterface: igr_pull_sched_rx_ppe_if
