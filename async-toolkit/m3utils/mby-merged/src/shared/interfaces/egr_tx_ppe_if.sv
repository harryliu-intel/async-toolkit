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
// -- Description : Egress to TX-PPE interface
// --
// -------------------------------------------------------------------

interface egr_tx_ppe_if
import shared_pkg::*;
();
logic                               valid;          //inputs are valid
logic   [EGR_PPE_DATA_WIDTH-1:0]    hdr_data;       //header segment (pre mod)
logic   [EGR_PPE_MOD_WIDTH-1:0]     mod_data;       //mod memory data
logic   [PPE_EGR_DATA_WIDTH-1:0]    mod_hdr_data;   //modified header

modport egr(
    output  valid,
    output  hdr_data,
    output  mod_data,
    input   mod_hdr_data
);

modport ppe(
    input   valid,
    input   hdr_data,
    input   mod_data,
    output  mod_hdr_data
);

endinterface: egr_tx_ppe_if
