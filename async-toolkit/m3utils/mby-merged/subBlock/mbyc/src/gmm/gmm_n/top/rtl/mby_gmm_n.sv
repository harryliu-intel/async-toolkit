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
// MBY_GMM_S.SV
//
// HISTORY
// ----------------------------------------------------------------------------------
// 17-10-2018 intital version
//
//=======================================================================================================================================

`ifndef MBY_GMM_N_SV
 `define MBY_GMM_N_SV

// collage-pragma translate_on

module mby_gmm_n
  import shared_pkg::*;
  import mby_gmm_pkg::*;
(
   input                              cclk,
   input                              reset_n,
 
   // pod pointer ring interface

   input  logic                       pod_ring_stall_in, // Signal from GPM to egress to stall egress from injecting a new dirty pod
	  
   // Tag ring interface (ingress -to- egress/GMM)
   // - 2 tags from 16 MGP's 
   input  mby_tag_ring_t              tag_ring_in_15_1 ,
   input  mby_tag_ring_t              tag_ring_in_15_0 ,
   input  mby_tag_ring_t              tag_ring_in_14_1 ,
   input  mby_tag_ring_t              tag_ring_in_14_0 ,
   input  mby_tag_ring_t              tag_ring_in_13_1 ,
   input  mby_tag_ring_t              tag_ring_in_13_0 ,
   input  mby_tag_ring_t              tag_ring_in_12_1 ,
   input  mby_tag_ring_t              tag_ring_in_12_0 ,
   input  mby_tag_ring_t              tag_ring_in_11_1 ,
   input  mby_tag_ring_t              tag_ring_in_11_0 ,
   input  mby_tag_ring_t              tag_ring_in_10_1 ,
   input  mby_tag_ring_t              tag_ring_in_10_0 ,
   input  mby_tag_ring_t              tag_ring_in_9_1  ,
   input  mby_tag_ring_t              tag_ring_in_9_0  ,
   input  mby_tag_ring_t              tag_ring_in_8_1  ,
   input  mby_tag_ring_t              tag_ring_in_8_0  ,
   input  mby_tag_ring_t              tag_ring_in_7_1  ,
   input  mby_tag_ring_t              tag_ring_in_7_0  ,
   input  mby_tag_ring_t              tag_ring_in_6_1  ,
   input  mby_tag_ring_t              tag_ring_in_6_0  ,
   input  mby_tag_ring_t              tag_ring_in_5_1  ,
   input  mby_tag_ring_t              tag_ring_in_5_0  ,
   input  mby_tag_ring_t              tag_ring_in_4_1  ,
   input  mby_tag_ring_t              tag_ring_in_4_0  ,
   input  mby_tag_ring_t              tag_ring_in_3_1  ,
   input  mby_tag_ring_t              tag_ring_in_3_0  ,
   input  mby_tag_ring_t              tag_ring_in_2_1  ,
   input  mby_tag_ring_t              tag_ring_in_2_0  ,
   input  mby_tag_ring_t              tag_ring_in_1_1  ,
   input  mby_tag_ring_t              tag_ring_in_1_0  ,
   input  mby_tag_ring_t              tag_ring_in_0_1  ,
   input  mby_tag_ring_t              tag_ring_in_0_0  ,
   
   output mby_tag_ring_t              tag_ring_out_15_1,
   output mby_tag_ring_t              tag_ring_out_15_0,
   output mby_tag_ring_t              tag_ring_out_14_1,
   output mby_tag_ring_t              tag_ring_out_14_0,
   output mby_tag_ring_t              tag_ring_out_13_1,
   output mby_tag_ring_t              tag_ring_out_13_0,
   output mby_tag_ring_t              tag_ring_out_12_1,
   output mby_tag_ring_t              tag_ring_out_12_0,
   output mby_tag_ring_t              tag_ring_out_11_1,
   output mby_tag_ring_t              tag_ring_out_11_0,
   output mby_tag_ring_t              tag_ring_out_10_1,
   output mby_tag_ring_t              tag_ring_out_10_0,
   output mby_tag_ring_t              tag_ring_out_9_1,
   output mby_tag_ring_t              tag_ring_out_9_0,
   output mby_tag_ring_t              tag_ring_out_8_1,
   output mby_tag_ring_t              tag_ring_out_8_0,
   output mby_tag_ring_t              tag_ring_out_7_1,
   output mby_tag_ring_t              tag_ring_out_7_0,
   output mby_tag_ring_t              tag_ring_out_6_1,
   output mby_tag_ring_t              tag_ring_out_6_0,
   output mby_tag_ring_t              tag_ring_out_5_1,
   output mby_tag_ring_t              tag_ring_out_5_0,
   output mby_tag_ring_t              tag_ring_out_4_1,
   output mby_tag_ring_t              tag_ring_out_4_0,
   output mby_tag_ring_t              tag_ring_out_3_1,
   output mby_tag_ring_t              tag_ring_out_3_0,
   output mby_tag_ring_t              tag_ring_out_2_1,
   output mby_tag_ring_t              tag_ring_out_2_0,
   output mby_tag_ring_t              tag_ring_out_1_1,
   output mby_tag_ring_t              tag_ring_out_1_0,
   output mby_tag_ring_t              tag_ring_out_0_1,
   output mby_tag_ring_t              tag_ring_out_0_0,
 
   // MultiCast tag ring interafce (MCE-to-egress)
   input  mby_mc_tag_ring_t           mc_tag_ring_in_3 ,
   input  mby_mc_tag_ring_t           mc_tag_ring_in_2 ,
   input  mby_mc_tag_ring_t           mc_tag_ring_in_1 ,
   input  mby_mc_tag_ring_t           mc_tag_ring_in_0 ,

   // Dequeue (Egress-to-GMM)
   output mby_deque_t                 mby_deque_from_vp
 
);
   
// collage-pragma translate_off

    //-----------------------------------------------------------------------------------------------------
   // FLOP repeaters for rings (may create its own module)
   //-----------------------------------------------------------------------------------------------------

   // Tag ring
   always_ff @(posedge cclk) begin
      tag_ring_out_15_1 <= tag_ring_in_15_1;
      tag_ring_out_15_0 <= tag_ring_in_15_0;
      tag_ring_out_14_1 <= tag_ring_in_14_1;
      tag_ring_out_14_0 <= tag_ring_in_14_0;
      tag_ring_out_13_1 <= tag_ring_in_13_1;
      tag_ring_out_13_0 <= tag_ring_in_13_0;
      tag_ring_out_12_1 <= tag_ring_in_12_1;
      tag_ring_out_12_0 <= tag_ring_in_12_0;
      tag_ring_out_11_1 <= tag_ring_in_11_1;
      tag_ring_out_11_0 <= tag_ring_in_11_0;
      tag_ring_out_10_1 <= tag_ring_in_10_1;
      tag_ring_out_10_0 <= tag_ring_in_10_0;
      tag_ring_out_9_1  <= tag_ring_in_9_1;
      tag_ring_out_9_0  <= tag_ring_in_9_0;
      tag_ring_out_8_1  <= tag_ring_in_8_1;
      tag_ring_out_8_0  <= tag_ring_in_8_0;
      tag_ring_out_7_1  <= tag_ring_in_7_1;
      tag_ring_out_7_0  <= tag_ring_in_7_0;
      tag_ring_out_6_1  <= tag_ring_in_6_1;
      tag_ring_out_6_0  <= tag_ring_in_6_0;
      tag_ring_out_5_1  <= tag_ring_in_5_1;
      tag_ring_out_5_0  <= tag_ring_in_5_0;
      tag_ring_out_4_1  <= tag_ring_in_4_1;
      tag_ring_out_4_0  <= tag_ring_in_4_0;
      tag_ring_out_3_1  <= tag_ring_in_3_1;
      tag_ring_out_3_0  <= tag_ring_in_3_0;
      tag_ring_out_2_1  <= tag_ring_in_2_1;
      tag_ring_out_2_0  <= tag_ring_in_2_0;
      tag_ring_out_1_1  <= tag_ring_in_1_1;
      tag_ring_out_1_0  <= tag_ring_in_1_0;
      tag_ring_out_0_1  <= tag_ring_in_0_1;
      tag_ring_out_0_0  <= tag_ring_in_0_0;
   end
   
// collage-pragma translate_on

endmodule // mby_gmm_s

`endif
   
