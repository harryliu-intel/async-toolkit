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
// To DO: remove slg_pkt::* once we ahve TB
//=======================================================================================================================================
module mby_gmm_s
  import sla_pkg::*, shared_pkg::*, mby_gmm_pkg::*;
(
   input                              cclk,
   input                              reset_n,
 
   // pod pointer ring interface
   input  mby_pod_ptr_ring_t          pod_ring_left_in,
   input  mby_pod_ptr_ring_t          pod_ring_right_in,
   output mby_pod_ptr_ring_t          pod_ring_left_out,
   output mby_pod_ptr_ring_t          pod_ring_right_out,

   output logic                       pod_ring_stall_left_out, // Signal from GPM to egress to stall egress from injecting a new dirty pod
   output logic                       pod_ring_stall_right_out,
	  
   // Tag ring interface (ingress -to- egress/GMM)
   input  mby_tag_ring_t              tag_ring_in [MBY_MAX_NUM_MGP-1:0][1:0],

   // Multicast deep-Q WR (ingress-to-GMM)
   input  logic [MBY_MAX_NUM_MGP-1:0] mc_deep_q_wr,
 
   // MultiCast tag ring interafce (MCE-to-egress)
   output mby_mc_tag_ring_t           mc_tag_ring_out_left  [3:0],
   output mby_mc_tag_ring_t           mc_tag_ring_out_right [3:0],

   // Dequeue (EGress-to-GMM)
   input  mby_deque_t                 mby_deque_from_egr [MBY_MAX_NUM_MGP-1:0][1:0]
 
);

endmodule // mby_gmm_s

   