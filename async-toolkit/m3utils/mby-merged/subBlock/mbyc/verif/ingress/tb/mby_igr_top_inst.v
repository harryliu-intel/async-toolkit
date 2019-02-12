//-----------------------------------------------------------------------------
// Title         : Ingress top instance module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_top_inst.v
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

rx_ppe_igr_if    rx_ppe_igr_if_i    ();
igr_rx_ppe_if    igr_rx_ppe_if_i    ();
egr_igr_dpod_if  egr_dirtypod_if_i  ();
igr_egr_cpod_if  egr_cleanpod_if_i  ();
mim_wr_if        mim_wreq_0_if_i    ();
mim_wr_if        mim_wreq_1_if_i    ();
mim_wr_if        mim_wreq_2_if_i    ();
mim_wr_if        mim_wreq_3_if_i    ();
mim_wr_if        mim_wreq_4_if_i    ();
mim_wr_if        mim_wreq_5_if_i    ();
//igr_egr_cpod_if  igr_egr_cptr_req_i ();

mby_igr_top igr_top_i (
   .cclk                  ( ingress_clock                 ),
   .rst                   ( ingress_reset                 ),
   .grp_a_rx_port_num     ( eth_bfm_rx_intf_0.port_num    ),
   .grp_b_rx_port_num     ( eth_bfm_rx_intf_1.port_num    ),
   .grp_c_rx_port_num     ( eth_bfm_rx_intf_2.port_num    ),
   .grp_d_rx_port_num     ( eth_bfm_rx_intf_3.port_num    ),
   .grp_a_rx_data_valid   ( eth_bfm_rx_intf_0.data_valid  ),
   .grp_b_rx_data_valid   ( eth_bfm_rx_intf_1.data_valid  ),
   .grp_c_rx_data_valid   ( eth_bfm_rx_intf_2.data_valid  ),
   .grp_d_rx_data_valid   ( eth_bfm_rx_intf_3.data_valid  ),
   .grp_a_rx_metadata     ( eth_bfm_rx_intf_0.metadata    ),
   .grp_b_rx_metadata     ( eth_bfm_rx_intf_1.metadata    ),
   .grp_c_rx_metadata     ( eth_bfm_rx_intf_2.metadata    ),
   .grp_d_rx_metadata     ( eth_bfm_rx_intf_3.metadata    ),
   .grp_a_rx_time_stamp   ( '0                            ),
   .grp_b_rx_time_stamp   ( '0                            ),
   .grp_c_rx_time_stamp   ( '0                            ),
   .grp_d_rx_time_stamp   ( '0                            ),
   .grp_a_rx_data         ( eth_bfm_rx_intf_0.data        ),
   .grp_b_rx_data         ( eth_bfm_rx_intf_1.data        ),
   .grp_c_rx_data         ( eth_bfm_rx_intf_2.data        ),
   .grp_d_rx_data         ( eth_bfm_rx_intf_3.data        ),
   .grp_a_tx_pfc_xoff     ( eth_bfm_rx_intf_0.pfc_xoff    ),
   .grp_b_tx_pfc_xoff     ( eth_bfm_rx_intf_1.pfc_xoff    ),
   .grp_c_tx_pfc_xoff     ( eth_bfm_rx_intf_2.pfc_xoff    ),
   .grp_d_tx_pfc_xoff     ( eth_bfm_rx_intf_3.pfc_xoff    ),
   .grp_a_tx_pfc_tc_sync  ( eth_bfm_rx_intf_0.pfc_tc_sync ),
   .grp_b_tx_pfc_tc_sync  ( eth_bfm_rx_intf_1.pfc_tc_sync ),
   .grp_c_tx_pfc_tc_sync  ( eth_bfm_rx_intf_2.pfc_tc_sync ),
   .grp_d_tx_pfc_tc_sync  ( eth_bfm_rx_intf_3.pfc_tc_sync ),
   .vp_rx_port_num        ( eth_bfm_rx_intf_4.port_num    ),
   .vp_rx_data_valid      ( eth_bfm_rx_intf_4.data_valid  ),
   .vp_rx_metadata        ( eth_bfm_rx_intf_4.metadata    ),
   .vp_tx_pfc_xoff        ( eth_bfm_rx_intf_4.pfc_xoff    ),
   .vp_rx_time_stamp      ( '0                            ),
   .vp_cpp_rx_metadata    ( 20'b0                         ),
   .vp_rx_ecc             ( vp_rx_ecc                     ),
   .vp_rx_data_w_ecc      ( vp_rx_data_w_ecc              ),
   .vp_rx_flow_control_tc ( vp_rx_flow_control_tc         ),
   .rx_ppe_igr            ( rx_ppe_igr_if_i.igr           ),
   .igr_rx_ppe            ( igr_rx_ppe_if_i.igr           ),
   .egr_dirtypod_if       ( egr_dirtypod_if_i.igr         ),
   .egr_cleanpod_if       ( egr_cleanpod_if_i.igr         ),
   //.igr_egr_cptr_req      ( igr_egr_cptr_req_i.igr        ),
   .mim_wreq_0            ( mim_wreq_0_if_i.request       ),
   .mim_wreq_1            ( mim_wreq_1_if_i.request       ),
   .mim_wreq_2            ( mim_wreq_2_if_i.request       ),
   .mim_wreq_3            ( mim_wreq_3_if_i.request       ),
   .mim_wreq_4            ( mim_wreq_4_if_i.request       ),
   .mim_wreq_5            ( mim_wreq_5_if_i.request       ),
   .o_tag_ring_lltag0     ( tag_bfm_intf_0.intf_data_pkt  ),
   .o_tag_ring_lltag1     ( tag_bfm_intf_1.intf_data_pkt  ),
   .o_gpolring0           (                               ),
   .o_gpolring1           (                               ),
   .i_gpolring_update0    ( '0                            ),
   .i_gpolring_update1    ( '0                            ),
	.i_rx_cm_wm_out        ( i_rx_cm_wm_out                ),
	.i_rx_cm_sm_wm_out     ( i_rx_cm_sm_wm_out             )
);

rx_ppe_bfm  rx_ppe
(
   .cclk                  ( ingress_clock                 ),
   .rst                   ( ingress_reset                 ),
   .igr_rx_ppe_intf0_ack  ( igr_rx_ppe_if_i.intf0_ack     ),
   .igr_rx_ppe_intf1_ack  ( igr_rx_ppe_if_i.intf1_ack     ),
   .igr_rx_ppe_intf0_tail ( igr_rx_ppe_if_i.intf0_tail    ),
   .igr_rx_ppe_intf0_head ( igr_rx_ppe_if_i.intf0_head    ),
   .igr_rx_ppe_intf1_tail ( igr_rx_ppe_if_i.intf1_tail    ),
   .igr_rx_ppe_intf1_head ( igr_rx_ppe_if_i.intf1_head    ),
   .rx_ppe_igr_intf0      ( rx_ppe_igr_if_i.intf0         ),
   .rx_ppe_igr_intf1      ( rx_ppe_igr_if_i.intf1         )
);
