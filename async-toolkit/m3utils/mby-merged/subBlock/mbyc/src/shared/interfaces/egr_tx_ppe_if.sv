// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
import mby_rx_metadata_pkg::*;
();
logic                                   egr_ppe_valid;  //egress to PPE valid
logic   [EGR_PPE_DATA_CNT_WIDTH-1:0]    egr_ppe_cnt;    //egress to PPE header byte count
logic   [0:EGR_PPE_DATA_WIDTH-1]        egr_ppe_hdr;    //header segment (pre mod)
ppe_meta_data_t                         egr_ppe_md;     //metadata
logic                                   ack;            //acknowledge
logic                                   ppe_egr_valid;  //PPE to egress valid
logic   [PPE_EGR_DATA_CNT_WIDTH-1:0]    ppe_egr_cnt;    //PPE to egress header byte count
logic   [0:PPE_EGR_DATA_WIDTH-1]        ppe_egr_hdr;    //modified header

modport egr(
    output  egr_ppe_valid,
    output  egr_ppe_cnt,
    output  egr_ppe_hdr,
    output  egr_ppe_md,
    input   ack,
    input   ppe_egr_valid,
    input   ppe_egr_cnt,
    input   ppe_egr_hdr
);

modport ppe(
    input   egr_ppe_valid,
    input   egr_ppe_cnt,
    input   egr_ppe_hdr,
    input   egr_ppe_md,
    output  ack,
    output  ppe_egr_valid,
    output  ppe_egr_cnt,
    output  ppe_egr_hdr
);

endinterface: egr_tx_ppe_if
