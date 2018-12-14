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

rx_ppe_igr_if rx_ppe_igr_if_i();
igr_rx_ppe_if igr_rx_ppe_if_i();           

mim_wr_if     egr_igr_wreq_if_i();
mim_rd_if     igr_egr_rreq_if_i();
mim_wr_if     mim_wreq_0_if_i();
mim_wr_if     mim_wreq_1_if_i();
mim_wr_if     mim_wreq_2_if_i();
mim_wr_if     mim_wreq_3_if_i();
mim_wr_if     mim_wreq_4_if_i();
mim_wr_if     mim_wreq_5_if_i();

always_comb rx_ppe_igr_if_i.intf0 = 0;
always_comb rx_ppe_igr_if_i.intf1 = 0;

mby_igr_top igr_top_i (
   .cclk(ingress_clock),
   .rst(ingress_reset),
   .grp_a_rx_port_num(eth_bfm_rx_intf_0.port_num),
   .grp_b_rx_port_num(eth_bfm_rx_intf_1.port_num),
   .grp_c_rx_port_num(eth_bfm_rx_intf_2.port_num),
   .grp_d_rx_port_num(eth_bfm_rx_intf_3.port_num),
   .grp_a_rx_data_valid(eth_bfm_rx_intf_0.data_valid),
   .grp_b_rx_data_valid(eth_bfm_rx_intf_1.data_valid),
   .grp_c_rx_data_valid(eth_bfm_rx_intf_2.data_valid),
   .grp_d_rx_data_valid(eth_bfm_rx_intf_3.data_valid),
   .grp_a_rx_metadata(eth_bfm_rx_intf_0.metadata),
   .grp_b_rx_metadata(eth_bfm_rx_intf_1.metadata),
   .grp_c_rx_metadata(eth_bfm_rx_intf_2.metadata),
   .grp_d_rx_metadata(eth_bfm_rx_intf_3.metadata),
   .grp_a_rx_time_stamp('0),
   .grp_b_rx_time_stamp('0),
   .grp_c_rx_time_stamp('0),
   .grp_d_rx_time_stamp('0),
//edr   .grp_a_rx_data_w_ecc(eth_bfm_rx_intf_0.data_w_ecc),
//edr   .grp_b_rx_data_w_ecc(eth_bfm_rx_intf_1.data_w_ecc),
//edr   .grp_c_rx_data_w_ecc(eth_bfm_rx_intf_2.data_w_ecc),
//edr   .grp_d_rx_data_w_ecc(eth_bfm_rx_intf_3.data_w_ecc),
   .grp_a_rx_data('0),
   .grp_b_rx_data('0),
   .grp_c_rx_data('0),
   .grp_d_rx_data('0),
   .grp_a_tx_pfc_xoff(),
   .grp_b_tx_pfc_xoff(),                                                           
   .grp_c_tx_pfc_xoff(),
   .grp_d_tx_pfc_xoff(),
   .grp_a_tx_pfc_tc_sync(),
   .grp_b_tx_pfc_tc_sync(),
   .grp_c_tx_pfc_tc_sync(),
   .grp_d_tx_pfc_tc_sync(),
   .vp_rx_port_num(eth_bfm_rx_intf_4.port_num),
   .vp_rx_data_valid(eth_bfm_rx_intf_4.data_valid),
   .vp_rx_metadata(eth_bfm_rx_intf_4.metadata),
   .vp_rx_time_stamp('0),
   .vp_cpp_rx_metadata(20'b0),
   .vp_tx_pfc_xoff(eth_bfm_rx_intf_4.pfc_xoff),
   .rx_ppe_igr(rx_ppe_igr_if_i.igr),
   .igr_rx_ppe(igr_rx_ppe_if_i.igr),
   .egr_igr_wreq(egr_igr_wreq_if_i.receive),  
   .igr_egr_rreq(igr_egr_rreq_if_i.request),                                                 
   .mim_wreq_0(mim_wreq_0_if_i.request),
   .mim_wreq_1(mim_wreq_1_if_i.request),
   .mim_wreq_2(mim_wreq_2_if_i.request),
   .mim_wreq_3(mim_wreq_3_if_i.request),
   .mim_wreq_4(mim_wreq_4_if_i.request),
   .mim_wreq_5(mim_wreq_5_if_i.request),
   .o_gpolring0(),
   .o_gpolring1(),
   .i_gpolring_update0('0),   
   .i_gpolring_update1('0)   
   //.o_tag_ring_lltag0(tag_bfm_intf_0.intf_data_pkt),
   //.o_tag_ring_lltag1(tag_bfm_intf_1.intf_data_pkt)
   
);
