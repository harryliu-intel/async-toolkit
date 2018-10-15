//-----------------------------------------------------------------------------
// Title         : Ingress top instance module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_top_inst.v
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

always_comb rx_ppe_igr_if_i.intf0 = 0;
always_comb rx_ppe_igr_if_i.intf1 = 0;

igr_top igr_top_i (
  .cclk(ingress_clock),
	.rst(ingress_reset),
	.grp_a_rx_ecc(eth_bfm_tx_intf_0.ecc),
	.grp_b_rx_ecc(eth_bfm_tx_intf_1.ecc),
	.grp_c_rx_ecc(eth_bfm_tx_intf_2.ecc),
	.grp_d_rx_ecc(eth_bfm_tx_intf_3.ecc),
	.grp_a_rx_port_num(eth_bfm_tx_intf_0.port_num),
	.grp_b_rx_port_num(eth_bfm_tx_intf_1.port_num),
	.grp_c_rx_port_num(eth_bfm_tx_intf_2.port_num),
	.grp_d_rx_port_num(eth_bfm_tx_intf_3.port_num),
	.grp_a_rx_data_valid(eth_bfm_tx_intf_0.data_valid),
	.grp_b_rx_data_valid(eth_bfm_tx_intf_1.data_valid),
	.grp_c_rx_data_valid(eth_bfm_tx_intf_2.data_valid),
	.grp_d_rx_data_valid(eth_bfm_tx_intf_3.data_valid),
	.grp_a_rx_metadata(eth_bfm_tx_intf_0.metadata),
	.grp_b_rx_metadata(eth_bfm_tx_intf_1.metadata),
	.grp_c_rx_metadata(eth_bfm_tx_intf_2.metadata),
	.grp_d_rx_metadata(eth_bfm_tx_intf_3.metadata),
	.grp_a_rx_time_stamp(36'b0),
	.grp_b_rx_time_stamp(36'b0),
	.grp_c_rx_time_stamp(36'b0),
	.grp_d_rx_time_stamp(36'b0),
	.grp_a_rx_data_w_ecc(eth_bfm_tx_intf_0.data_w_ecc),
	.grp_b_rx_data_w_ecc(eth_bfm_tx_intf_1.data_w_ecc),
	.grp_c_rx_data_w_ecc(eth_bfm_tx_intf_2.data_w_ecc),
	.grp_d_rx_data_w_ecc(eth_bfm_tx_intf_3.data_w_ecc),
	.grp_a_rx_pfc_xoff(eth_bfm_tx_intf_0.pfc_xoff),
	.grp_b_rx_pfc_xoff(eth_bfm_tx_intf_1.pfc_xoff),
	.grp_c_rx_pfc_xoff(eth_bfm_tx_intf_2.pfc_xoff),
	.grp_d_rx_pfc_xoff(eth_bfm_tx_intf_3.pfc_xoff),
	.grp_a_rx_flow_control_tc(eth_bfm_tx_intf_0.flow_control_tc),
	.grp_b_rx_flow_control_tc(eth_bfm_tx_intf_1.flow_control_tc),
	.grp_c_rx_flow_control_tc(eth_bfm_tx_intf_2.flow_control_tc),
	.grp_d_rx_flow_control_tc(eth_bfm_tx_intf_3.flow_control_tc),
	.vp_rx_ecc(eth_bfm_tx_intf_4.ecc),
	.vp_rx_port_num(eth_bfm_tx_intf_0.port_num),
	.vp_rx_data_valid(eth_bfm_tx_intf_0.data_valid),
	.vp_rx_metadata(eth_bfm_tx_intf_0.metadata),
	.vp_rx_time_stamp(36'b0),
	.vp_cpp_rx_metadata(20'b0),
	.vp_rx_data_w_ecc(eth_bfm_tx_intf_0.data_w_ecc),
	.vp_rx_flow_control_tc(eth_bfm_tx_intf_0.flow_control_tc),
	.vp_tx_pfc_xoff(eth_bfm_tx_intf_0.pfc_xoff),
	.rx_ppe_igr(rx_ppe_igr_if_i.igr),
	.igr_rx_ppe(igr_rx_ppe_if_i.igr)
  
);