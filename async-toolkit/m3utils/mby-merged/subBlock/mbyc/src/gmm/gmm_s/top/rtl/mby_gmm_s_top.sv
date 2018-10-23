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

`ifndef MBY_GMM_S_TOP_SV
 `define MBY_GMM_S_TOP_SV

// collage-pragma translate_on

module mby_gmm_s_top
  import shared_pkg::*, mby_gmm_pkg::*;
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
   input  mby_tag_ring_t              tag_ring_in_0  [MBY_MAX_NUM_MGP-1:0],
   input  mby_tag_ring_t              tag_ring_in_1  [MBY_MAX_NUM_MGP-1:0],
   output mby_tag_ring_t              tag_ring_out_0 [MBY_MAX_NUM_MGP-1:0],
   output mby_tag_ring_t              tag_ring_out_1 [MBY_MAX_NUM_MGP-1:0],
   
   input  logic [MBY_MAX_NUM_MGP-1:0] tag_ring_in_policer_idx_0, // part of the tag ring but the destination is to GPL not egress
   input  logic [MBY_MAX_NUM_MGP-1:0] tag_ring_in_policer_idx_1, // part of the tag ring but the destination is to GPL not egress
 
   // Multicast deep-Q WR (ingress -to- GMM)
   input  logic [MBY_MAX_NUM_MGP-1:0] mc_deep_q_wr,
 
   // MultiCast tag ring interafce (MCE-to-egress)
   output mby_mc_tag_ring_t           mc_tag_ring_out_left  [3:0],
   output mby_mc_tag_ring_t           mc_tag_ring_out_right [3:0],

   // Dequeue (EGress -to- GMM)
   input  mby_deque_t                 mby_deque_from_egr_left  [MBY_MAX_NUM_MGP-1:0],
   input  mby_deque_t                 mby_deque_from_egr_right [MBY_MAX_NUM_MGP-1:0],
 
   // Dequeue from Virtual Port Egress
   input  mby_deque_t                 mby_deque_from_vp,

   // Congestion Management WaterMark indication
   output mby_cm_rx_wm_t              rx_cm_wm_out_left,
   output mby_cm_rx_wm_t              rx_cm_wm_out_right,

   output mby_cm_tx_wm_t              tx_cm_wm_out_left,
   output mby_cm_tx_wm_t              tx_cm_wm_out_right,

   output mby_cm_shared_mem_wm_t      rx_cm_sm_wm_out_left,
   output mby_cm_shared_mem_wm_t      rx_cm_sm_wm_out_right,

   output mby_cm_shared_mem_wm_t      tx_cm_sm_wm_out_left,
   output mby_cm_shared_mem_wm_t      tx_cm_sm_wm_out_right
 
);

// collage-pragma translate_off
   
   //-----------------------------------------------------------------------------------------------------
   // FLOP repeaters for rings (may create its own module)
   //-----------------------------------------------------------------------------------------------------

   // Tag ring
   always_ff @(posedge cclk) begin
      for (int i = 0; i < MBY_MAX_NUM_MGP; i++) begin
	 tag_ring_out_1 <= tag_ring_in_1;
	 tag_ring_out_0 <= tag_ring_in_0;
      end
   end
   

   //-----------------------------------------------------------------------------------------------------
   // Global Pod Pointer Management Module Instantiation
   //-----------------------------------------------------------------------------------------------------
   mby_gpm_top mby_gpm_top ( .cclk                     (cclk),
                             .reset_n                  (reset_n),
			     .pod_ring_left_in         (pod_ring_left_in),
			     .pod_ring_right_in        (pod_ring_right_in),
			     .pod_ring_left_out        (pod_ring_left_out),
			     .pod_ring_right_out       (pod_ring_right_out),
			     .pod_ring_stall_left_out  (pod_ring_stall_left_out ),
			     .pod_ring_stall_right_out (pod_ring_stall_right_out)
			    );
   		     
   //-----------------------------------------------------------------------------------------------------
   // Global Congestion Management
   //-----------------------------------------------------------------------------------------------------
   mby_gcm_top mby_gcm_top ( .cclk                     (cclk),
			     .reset_n                  (reset_n),
			     .tag_ring_in_0            (tag_ring_in_0),
			     .tag_ring_in_1            (tag_ring_in_1),
			     .mby_deque_from_egr_left  (mby_deque_from_egr_left),
			     .mby_deque_from_egr_right (mby_deque_from_egr_right),
			     .mby_deque_from_vp        (mby_deque_from_vp),
			     .rx_cm_wm_out_left        (rx_cm_wm_out_left),
			     .rx_cm_wm_out_right       (rx_cm_wm_out_right),
			     .tx_cm_wm_out_left        (tx_cm_wm_out_left),
			     .tx_cm_wm_out_right       (tx_cm_wm_out_right),
			     .rx_cm_sm_wm_out_left     (rx_cm_sm_wm_out_left),
			     .rx_cm_sm_wm_out_right    (rx_cm_sm_wm_out_right),
			     .tx_cm_sm_wm_out_left     (tx_cm_sm_wm_out_left),
			     .tx_cm_sm_wm_out_right    (tx_cm_sm_wm_out_right)
			    );

   //-----------------------------------------------------------------------------------------------------
   // Multicast/Mirror engine
   //-----------------------------------------------------------------------------------------------------
   mby_mce_top mby_mce_top ( .cclk                     (cclk),
			     .reset_n                  (reset_n),
			     .mc_deep_q_wr             (mc_deep_q_wr),
                             .mc_tag_ring_out_left     (mc_tag_ring_out_left),
                             .mc_tag_ring_out_right    (mc_tag_ring_out_right)    
			    );
   
   //-----------------------------------------------------------------------------------------------------
   // Global Policier
   //-----------------------------------------------------------------------------------------------------
   mby_gpl_top mby_gpl_top ( .cclk                     (cclk),
			     .reset_n                  (reset_n)
			    );
   
// collage-pragma translate_on
   
endmodule // mby_gmm_s_top

`endif //  `ifndef MBY_GMM_S_TOP_SV

   