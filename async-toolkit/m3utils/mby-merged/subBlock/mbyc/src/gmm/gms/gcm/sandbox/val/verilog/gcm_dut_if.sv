///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
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
// ----------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : MBY
// -- Description  : The DUT interface
// ----------------------------------------------------------------------------

// This is a connectivity only interface and thus is just a named bundle of
// nets that can be passed around as a group to save typing.  Anywhere an
// interface is defined, individual nets in the interface can be referenced as
// follows:
//
//      <interface name>.<net name>
//
`ifndef GCM_DUT_IF_SV
`define GCM_DUT_IF_SV

interface gcm_dut_if
    import mby_gmm_pkg::*;
    import mby_gms_pkg::*;
(
    input cclk
);

    //// local parameters
    ////localparam NUM_INPUTS  = tmpl_pkg::NUM_INPUTS;
    ////localparam NUM_OUTPUTS = tmpl_pkg::NUM_OUTPUTS;

    //// DUT inputs  (direction not specified in this interface)
    logic reset_n;

    //// pod pointer ring interface
    //mby_pod_ptr_ring_t  i_pod_ring_left;
    //mby_pod_ptr_ring_t  i_pod_ring_right;
    //mby_pod_ptr_ring_t  o_pod_ring_left;
    //mby_pod_ptr_ring_t  o_pod_ring_right;

    //// Signal from GPM to egress to stall egress from injecting a new dirty pod
    //logic               o_pod_ring_stall_left;
    //logic               o_pod_ring_stall_right;


   mby_tag_ring_to_gcm_t       i_tag_to_gcm [MBY_MAX_NUM_MGP-1:0][1:0] ;
   mby_unicast_deque_t         i_mby_deque_from_egr [MBY_MAX_NUM_MGP-1:0][1:0] ;
   mby_unicast_deque_t         i_mby_deque_from_vp;
   mc_mirror_deque_from_mce_t  i_mce_mc_mirror_dequeue [15:0]; 
   mby_cm_rx_wm_t              o_rx_cm_wm_out_left;
   mby_cm_rx_wm_t              o_rx_cm_wm_out_right;
   mby_cm_tx_wm_t              o_tx_cm_wm_out_left;
   mby_cm_tx_wm_t              o_tx_cm_wm_out_right;
   mby_cm_shared_mem_rx_wm_t   o_rx_cm_sm_wm_out_left;
   mby_cm_shared_mem_rx_wm_t   o_rx_cm_sm_wm_out_right;
   mby_cm_shared_mem_tx_wm_t   o_tx_cm_sm_wm_out_left;
   mby_cm_shared_mem_tx_wm_t   o_tx_cm_sm_wm_out_right;


endinterface : gcm_dut_if
`endif // GCM_DUT_IF_SV
