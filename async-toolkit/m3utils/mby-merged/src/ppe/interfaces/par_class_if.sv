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
// -- Description : Parser to classifier interface
// --
// -------------------------------------------------------------------

interface par_class_if
import hlp_pkg::*, hlp_ipp_pkg::*;
();
imn_rpl_bkwd_t  rpl_bkwd;       //Management status from downstream blocks
imn_rpl_frwd_t  rpl_frwd;       //Managment to downstream blocks

parser_out_t    parser_out;     //Parser results
logic           parser_out_v;   //Parser results valid

tail_info_t     o_tail_info;    //Tail info
logic           o_tail_info_v;  //Tail info valid

modport parser(
    input   rpl_bkwd,
    output  rpl_frwd,
    output  parser_out,
    output  parser_out_v,
    output  o_tail_info,
    output  o_tail_info_v
);

modport classifier(
    output  rpl_bkwd,
    input   rpl_frwd,
    input   parser_out,
    input   parser_out_v,
    input   o_tail_info,
    input   o_tail_info_v
);

endinterface: par_class_if
