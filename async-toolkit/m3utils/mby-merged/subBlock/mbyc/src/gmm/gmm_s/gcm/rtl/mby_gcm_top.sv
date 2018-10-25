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
// MBY_GCM_TOP.SV
//
// Global Congestion Management for the MBY shared memory
//
// HISTORY
// ----------------------------------------------------------------------------------
// 17-10-2018 intital version
//
//=======================================================================================================================================
`ifndef MBY_GCM_TOP_SV
 `define MBY_GCM_TOP_SV

 // collage-pragma translate_on

module mby_gcm_top
  import shared_pkg::*, mby_gmm_pkg::*;
(
   // CLock and reset
   input                              cclk,
   input                              reset_n,
 
   // Enqueue
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

   // Dequeue frome egress
   input  mby_deque_t                 mby_deque_from_egr_15_1,
   input  mby_deque_t                 mby_deque_from_egr_15_0,
   input  mby_deque_t                 mby_deque_from_egr_14_1,
   input  mby_deque_t                 mby_deque_from_egr_14_0,
   input  mby_deque_t                 mby_deque_from_egr_13_1,
   input  mby_deque_t                 mby_deque_from_egr_13_0,
   input  mby_deque_t                 mby_deque_from_egr_12_1,
   input  mby_deque_t                 mby_deque_from_egr_12_0,
   input  mby_deque_t                 mby_deque_from_egr_11_1,
   input  mby_deque_t                 mby_deque_from_egr_11_0,
   input  mby_deque_t                 mby_deque_from_egr_10_1,
   input  mby_deque_t                 mby_deque_from_egr_10_0,
   input  mby_deque_t                 mby_deque_from_egr_9_1 ,
   input  mby_deque_t                 mby_deque_from_egr_9_0 ,
   input  mby_deque_t                 mby_deque_from_egr_8_1 ,
   input  mby_deque_t                 mby_deque_from_egr_8_0 ,
   input  mby_deque_t                 mby_deque_from_egr_7_1 ,
   input  mby_deque_t                 mby_deque_from_egr_7_0 ,
   input  mby_deque_t                 mby_deque_from_egr_6_1 ,
   input  mby_deque_t                 mby_deque_from_egr_6_0 ,
   input  mby_deque_t                 mby_deque_from_egr_5_1 ,
   input  mby_deque_t                 mby_deque_from_egr_5_0 ,
   input  mby_deque_t                 mby_deque_from_egr_4_1 ,
   input  mby_deque_t                 mby_deque_from_egr_4_0 ,
   input  mby_deque_t                 mby_deque_from_egr_3_1 ,
   input  mby_deque_t                 mby_deque_from_egr_3_0 ,
   input  mby_deque_t                 mby_deque_from_egr_2_1 ,
   input  mby_deque_t                 mby_deque_from_egr_2_0 ,
   input  mby_deque_t                 mby_deque_from_egr_1_1 ,
   input  mby_deque_t                 mby_deque_from_egr_1_0 ,
   input  mby_deque_t                 mby_deque_from_egr_0_1 ,
   input  mby_deque_t                 mby_deque_from_egr_0_0 ,

   // Dequeue from VP
   input  mby_deque_t                 mby_deque_from_vp,

   // Congestion Management ring 
   output mby_cm_rx_wm_t              rx_cm_wm_out_left,
   output mby_cm_rx_wm_t              rx_cm_wm_out_right,

   output mby_cm_tx_wm_t              tx_cm_wm_out_left,
   output mby_cm_tx_wm_t              tx_cm_wm_out_right,

   output mby_cm_shared_mem_rx_wm_t   rx_cm_sm_wm_out_left,
   output mby_cm_shared_mem_rx_wm_t   rx_cm_sm_wm_out_right,

   output mby_cm_shared_mem_tx_wm_t   tx_cm_sm_wm_out_left,
   output mby_cm_shared_mem_tx_wm_t   tx_cm_sm_wm_out_right
	  
);

  // collage-pragma translate_off

   
  // collage-pragma translate_on
   
endmodule // mby_gpm_top

`endif //  `ifndef MBY_GCM_TOP_SV


   