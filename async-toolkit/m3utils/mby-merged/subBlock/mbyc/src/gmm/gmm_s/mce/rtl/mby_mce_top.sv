///=======================================================================================================================================
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
///=======================================================================================================================================
//
// MBY_MCE_TOP.SV
//
// MBY Multicast/Mirror Engine
//
// HISTORY
// ----------------------------------------------------------------------------------
// 17-10-2018 intital version
//
//=======================================================================================================================================
`ifndef MBY_MCE_TOP_SV
 `define MBY_MCE_TOP_SV

// collage-pragma translate_on

module mby_mce_top
  import shared_pkg::*, mby_gmm_pkg::*;
(
   // Clock and reset
   input                              cclk,
   input                              reset_n,

   // MCE enqueue
   input logic [MBY_MAX_NUM_MGP-1:0]  mc_deep_q_wr,

   // mesh read requests for the MC deep-Q

   // mesh read responses for the MC deep-Q

   // MC tag ring
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_3,
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_2,
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_1,
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_0,
 
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_3,
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_2,
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_1,
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_0
 	  
);

    // collage-pragma translate_off

    // collage-pragma translate_on

endmodule // mby_gpm_top

`endif
     