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
// -- Description : Classifier group interface
// --
// -------------------------------------------------------------------

interface class_group_if
import shared_pkg::*;
import hlp_pkg::*;
import hlp_ipp_pkg::*;
();
imn_rpl_frwd_t                      rpl_bkwd;       //Management status from downstream blocks
imn_rpl_frwd_t                      rpl_frwd;       //Managment to downstream blocks

group_data_t                        group_data;     //Classifier group data
logic   [1:0]                       group_data_v;   //Group data valid

igr_rx_ppe_md_t                     if0_md;         //Interface 0 metadata
logic   [IGR_PPE_MD_ECC_WIDTH-1:0]  if0_md_ecc;     //Interface 0 metadata ECC bits
igr_rx_ppe_md_t                     if1_md;         //Interface 1 metadata
logic   [IGR_PPE_MD_ECC_WIDTH-1:0]  if1_md_ecc;     //Interface 0 metadata ECC bits

igr_rx_ppe_tail_t [1:0]             tail_info;      //Tail info

modport class_out(
    input   rpl_bkwd,
    output  rpl_frwd,
    output  group_data,
    output  group_data_v,
    output  if0_md,
    output  if0_md_ecc,
    output  if1_md,
    output  if1_md_ecc,
    output  tail_info
);

modport class_in(
    output  rpl_bkwd,
    input   rpl_frwd,
    input   group_data,
    input   group_data_v,
    input   if0_md,
    input   if0_md_ecc,
    input   if1_md,
    input   if1_md_ecc,
    input   tail_info
);

endinterface: class_group_if
