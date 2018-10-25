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
module mby_gcm_top
  import shared_pkg::*, mby_gmm_pkg::*;
(
   // CLock and reset
   input                              cclk,
   input                              reset_n,
 
   // Enqueue
   input  mby_tag_ring_t              tag_ring_in_0  [MBY_MAX_NUM_MGP-1:0],
   input  mby_tag_ring_t              tag_ring_in_1  [MBY_MAX_NUM_MGP-1:0],

   // Dequeue
   input  mby_deque_t                 mby_deque_from_egr_left  [MBY_MAX_NUM_MGP-1:0],
   input  mby_deque_t                 mby_deque_from_egr_right [MBY_MAX_NUM_MGP-1:0],
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

endmodule // mby_gpm_top


   