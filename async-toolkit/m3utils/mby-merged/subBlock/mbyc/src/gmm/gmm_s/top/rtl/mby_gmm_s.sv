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

`ifndef MBY_GMM_S_SV
 `define MBY_GMM_S_SV

// collage-pragma translate_on

module mby_gmm_s
  import shared_pkg::*;
  import mby_gmm_pkg::*;
(
   input                              cclk,
   input                              reset_n,
 
   // pod pointer ring interface
   input  mby_pod_ptr_ring_t          pod_ring_left_in,
   input  mby_pod_ptr_ring_t          pod_ring_right_in,
   output mby_pod_ptr_ring_t          pod_ring_left_out,
   output mby_pod_ptr_ring_t          pod_ring_right_out,

   output logic                       pod_ring_stall_left_out,  // Signal from GPM to egress to stall egress from injecting a new dirty pod
   output logic                       pod_ring_stall_right_out,
	  
   // Tag ring interface (ingress -to- egress/GMM)
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
   
   input  logic [MBY_MAX_NUM_MGP-1:0] tag_ring_in_policer_idx_0, // part of the tag ring but the destination is to GPL not egress
   input  logic [MBY_MAX_NUM_MGP-1:0] tag_ring_in_policer_idx_1, // part of the tag ring but the destination is to GPL not egress
 
   // Multicast deep-Q WR (ingress -to- GMM)
   input  logic [MBY_MAX_NUM_MGP-1:0] mc_deep_q_wr,
 
   // MultiCast tag ring interafce (MCE-to-egress)
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_3  ,
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_2  ,
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_1  ,
   output mby_mc_tag_ring_t           mc_tag_ring_out_left_0  ,
 
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_3 ,
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_2 ,
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_1 ,
   output mby_mc_tag_ring_t           mc_tag_ring_out_right_0 ,

   // Dequeue (Egress -to- GMM)
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
  
   // Dequeue from Virtual Port Egress
   input  mby_deque_t                 mby_deque_from_vp,

   // Congestion Management WaterMark indication
   output mby_cm_rx_wm_t              rx_cm_wm_out_left,
   output mby_cm_rx_wm_t              rx_cm_wm_out_right,

   output mby_cm_tx_wm_t              tx_cm_wm_out_left,
   output mby_cm_tx_wm_t              tx_cm_wm_out_right,

   output mby_cm_shared_mem_rx_wm_t   rx_cm_sm_wm_out_left,
   output mby_cm_shared_mem_rx_wm_t   rx_cm_sm_wm_out_right,

   output mby_cm_shared_mem_tx_wm_t   tx_cm_sm_wm_out_left,
   output mby_cm_shared_mem_tx_wm_t   tx_cm_sm_wm_out_right,

   // Global Policer update broadcast
   output mby_gpol_state_bcast_t      gpol_update_bcast_out_left,
   output mby_gpol_state_bcast_t      gpol_update_bcast_out_right
 
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
   mby_gcm_top mby_gcm_top ( .cclk                       (cclk),
			     .reset_n                    (reset_n),
			     // TAG RING inputs
			     .tag_ring_in_15_1           (tag_ring_in_15_1),
			     .tag_ring_in_15_0           (tag_ring_in_15_0),
			     .tag_ring_in_14_1           (tag_ring_in_14_1),
			     .tag_ring_in_14_0           (tag_ring_in_14_0),
			     .tag_ring_in_13_1           (tag_ring_in_13_1),
			     .tag_ring_in_13_0           (tag_ring_in_13_0),
			     .tag_ring_in_12_1           (tag_ring_in_12_1),
			     .tag_ring_in_12_0           (tag_ring_in_12_0),
			     .tag_ring_in_11_1           (tag_ring_in_11_1),
			     .tag_ring_in_11_0           (tag_ring_in_11_0),
			     .tag_ring_in_10_1           (tag_ring_in_10_1),
			     .tag_ring_in_10_0           (tag_ring_in_10_0),
			     .tag_ring_in_9_1            (tag_ring_in_9_1),
			     .tag_ring_in_9_0            (tag_ring_in_9_0),
			     .tag_ring_in_8_1            (tag_ring_in_8_1),
			     .tag_ring_in_8_0            (tag_ring_in_8_0),
			     .tag_ring_in_7_1            (tag_ring_in_7_1),
			     .tag_ring_in_7_0            (tag_ring_in_7_0),
			     .tag_ring_in_6_1            (tag_ring_in_6_1),
			     .tag_ring_in_6_0            (tag_ring_in_6_0),
			     .tag_ring_in_5_1            (tag_ring_in_5_1),
			     .tag_ring_in_5_0            (tag_ring_in_5_0),
			     .tag_ring_in_4_1            (tag_ring_in_4_1),
			     .tag_ring_in_4_0            (tag_ring_in_4_0),
			     .tag_ring_in_3_1            (tag_ring_in_3_1),
			     .tag_ring_in_3_0            (tag_ring_in_3_0),
			     .tag_ring_in_2_1            (tag_ring_in_2_1),
			     .tag_ring_in_2_0            (tag_ring_in_2_0),
			     .tag_ring_in_1_1            (tag_ring_in_1_1),
			     .tag_ring_in_1_0            (tag_ring_in_1_0),
			     .tag_ring_in_0_1            (tag_ring_in_0_1),
			     .tag_ring_in_0_0            (tag_ring_in_0_0),
			     // Dequeue 
			     .mby_deque_from_egr_15_1    (mby_deque_from_egr_15_1),
			     .mby_deque_from_egr_14_0    (mby_deque_from_egr_15_0),
			     .mby_deque_from_egr_14_1    (mby_deque_from_egr_14_1),
			     .mby_deque_from_egr_14_0    (mby_deque_from_egr_14_0),
			     .mby_deque_from_egr_13_1    (mby_deque_from_egr_13_1),
			     .mby_deque_from_egr_13_0    (mby_deque_from_egr_13_0),
			     .mby_deque_from_egr_12_1    (mby_deque_from_egr_12_1),
			     .mby_deque_from_egr_12_0    (mby_deque_from_egr_12_0),
			     .mby_deque_from_egr_11_1    (mby_deque_from_egr_11_1),
			     .mby_deque_from_egr_11_0    (mby_deque_from_egr_11_0),
			     .mby_deque_from_egr_10_1    (mby_deque_from_egr_10_1),
			     .mby_deque_from_egr_10_0    (mby_deque_from_egr_10_0),
			     .mby_deque_from_egr_9_1     (mby_deque_from_egr_9_1),
			     .mby_deque_from_egr_9_0     (mby_deque_from_egr_9_0),
			     .mby_deque_from_egr_8_1     (mby_deque_from_egr_8_1),
			     .mby_deque_from_egr_8_0     (mby_deque_from_egr_8_0),
			     .mby_deque_from_egr_7_1     (mby_deque_from_egr_7_1),
			     .mby_deque_from_egr_7_0     (mby_deque_from_egr_7_0),
			     .mby_deque_from_egr_6_1     (mby_deque_from_egr_6_1),
			     .mby_deque_from_egr_6_0     (mby_deque_from_egr_6_0),
	                     .mby_deque_from_egr_5_1     (mby_deque_from_egr_5_1),
			     .mby_deque_from_egr_5_0     (mby_deque_from_egr_5_0),
                             .mby_deque_from_egr_4_1     (mby_deque_from_egr_4_1),
			     .mby_deque_from_egr_4_0     (mby_deque_from_egr_4_0),
                             .mby_deque_from_egr_3_1     (mby_deque_from_egr_3_1),
			     .mby_deque_from_egr_3_0     (mby_deque_from_egr_3_0),
                             .mby_deque_from_egr_2_1     (mby_deque_from_egr_2_1),
			     .mby_deque_from_egr_2_0     (mby_deque_from_egr_2_0),
                             .mby_deque_from_egr_1_1     (mby_deque_from_egr_1_1),
			     .mby_deque_from_egr_1_0     (mby_deque_from_egr_1_0),
                             .mby_deque_from_egr_0_1     (mby_deque_from_egr_0_1),
			     .mby_deque_from_egr_0_0     (mby_deque_from_egr_0_0),
		             // Dequeue from VP
			     .mby_deque_from_vp          (mby_deque_from_vp),
			     // Water Mark 
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
                             .mc_tag_ring_out_left_3   (mc_tag_ring_out_left_3),
			     .mc_tag_ring_out_left_2   (mc_tag_ring_out_left_2),
			     .mc_tag_ring_out_left_1   (mc_tag_ring_out_left_1),
			     .mc_tag_ring_out_left_0   (mc_tag_ring_out_left_0),
                             .mc_tag_ring_out_right_3  (mc_tag_ring_out_right_3),
			     .mc_tag_ring_out_right_2  (mc_tag_ring_out_right_2),
			     .mc_tag_ring_out_right_1  (mc_tag_ring_out_right_1),
			     .mc_tag_ring_out_right_0  (mc_tag_ring_out_right_0)
			    );
   
   //-----------------------------------------------------------------------------------------------------
   // Global Policier
   //-----------------------------------------------------------------------------------------------------
   mby_gpl_top mby_gpl_top ( .cclk                        (cclk),
			     .reset_n                     (reset_n),
			     .gpol_update_bcast_out_left  (gpol_update_bcast_out_left ),
			     .gpol_update_bcast_out_right (gpol_update_bcast_out_right)
			    );
   
// collage-pragma translate_on
   
endmodule // mby_gmm_s

`endif //  `ifndef MBY_GMM_S_TOP_SV

   