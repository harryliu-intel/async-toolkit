// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  10/04/2018
// Description  :  IGR test island instantiation and connectivity macros
//------------------------------------------------------------------------------

`define igr_conn(FC_ENV_PATH, DUT_PATH, MPP_IDX, MGP_IDX, IDX) \
    \
    mby_igr_env_if ingress_if_``IDX``(); \
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_0_``IDX``(); \
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_0_``IDX``(); \
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_1_``IDX``(); \
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_1_``IDX``(); \
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_2_``IDX``(); \
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_2_``IDX``(); \
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_3_``IDX``(); \
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_3_``IDX``(); \
    mby_ec_cdi_tx_intf eth_bfm_tx_intf_igr_4_``IDX``(); \
    mby_ec_cdi_rx_intf eth_bfm_rx_intf_igr_4_``IDX``(); \
    \
    assign eth_bfm_tx_intf_igr_0_``IDX``.ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_ecc; \
    assign eth_bfm_tx_intf_igr_0_``IDX``.port_num = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_port_num; \
    assign eth_bfm_tx_intf_igr_0_``IDX``.data_valid = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_data_valid; \
    assign eth_bfm_tx_intf_igr_0_``IDX``.metadata = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_metadata; \
    assign eth_bfm_tx_intf_igr_0_``IDX``.data_w_ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_data_w_ecc; \
    assign eth_bfm_tx_intf_igr_0_``IDX``.pfc_xoff = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_pfc_xoff; \
    assign eth_bfm_tx_intf_igr_0_``IDX``.flow_control_tc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_a_rx_flow_control_tc; \
    \
    assign eth_bfm_tx_intf_igr_1_``IDX``.ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_ecc; \
    assign eth_bfm_tx_intf_igr_1_``IDX``.port_num = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_port_num; \
    assign eth_bfm_tx_intf_igr_1_``IDX``.data_valid = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_data_valid; \
    assign eth_bfm_tx_intf_igr_1_``IDX``.metadata = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_metadata; \
    assign eth_bfm_tx_intf_igr_1_``IDX``.data_w_ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_data_w_ecc; \
    assign eth_bfm_tx_intf_igr_1_``IDX``.pfc_xoff = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_pfc_xoff; \
    assign eth_bfm_tx_intf_igr_1_``IDX``.flow_control_tc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_b_rx_flow_control_tc; \
    \
    assign eth_bfm_tx_intf_igr_2_``IDX``.ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_ecc; \
    assign eth_bfm_tx_intf_igr_2_``IDX``.port_num = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_port_num; \
    assign eth_bfm_tx_intf_igr_2_``IDX``.data_valid = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_data_valid; \
    assign eth_bfm_tx_intf_igr_2_``IDX``.metadata = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_metadata; \
    assign eth_bfm_tx_intf_igr_2_``IDX``.data_w_ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_data_w_ecc; \
    assign eth_bfm_tx_intf_igr_2_``IDX``.pfc_xoff = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_pfc_xoff; \
    assign eth_bfm_tx_intf_igr_2_``IDX``.flow_control_tc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_c_rx_flow_control_tc; \
    \
    assign eth_bfm_tx_intf_igr_3_``IDX``.ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_ecc; \
    assign eth_bfm_tx_intf_igr_3_``IDX``.port_num = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_port_num; \
    assign eth_bfm_tx_intf_igr_3_``IDX``.data_valid = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_data_valid; \
    assign eth_bfm_tx_intf_igr_3_``IDX``.metadata = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_metadata; \
    assign eth_bfm_tx_intf_igr_3_``IDX``.data_w_ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_data_w_ecc; \
    assign eth_bfm_tx_intf_igr_3_``IDX``.pfc_xoff = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_pfc_xoff; \
    assign eth_bfm_tx_intf_igr_3_``IDX``.flow_control_tc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.grp_d_rx_flow_control_tc; \
    \
    assign eth_bfm_tx_intf_igr_4_``IDX``.ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_rx_ecc; \
    assign eth_bfm_tx_intf_igr_4_``IDX``.port_num = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_rx_port_num; \
    assign eth_bfm_tx_intf_igr_4_``IDX``.data_valid = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_rx_data_valid; \
    assign eth_bfm_tx_intf_igr_4_``IDX``.metadata = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_rx_metadata; \
    assign eth_bfm_tx_intf_igr_4_``IDX``.data_w_ecc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_rx_data_w_ecc; \
    assign eth_bfm_tx_intf_igr_4_``IDX``.pfc_xoff = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_tx_pfc_xoff; \
    assign eth_bfm_tx_intf_igr_4_``IDX``.flow_control_tc = ``DUT_PATH.mby_mpp_``MPP_IDX``.mgp``MGP_IDX``.igr.vp_rx_flow_control_tc; \
    \
    mby_igr_ti_high #( \
        .IP_ENV(``"FC_ENV_PATH``.igr_subenv.ingress_env_inst_``IDX``") \
    ) u_ingress_ti_``IDX`` ( \
        .ingress_if(ingress_if_``IDX``), \
        .eth_bfm_tx_intf_0   (eth_bfm_tx_intf_igr_0_``IDX``), \
        .eth_bfm_rx_intf_0   (eth_bfm_rx_intf_igr_0_``IDX``), \
        .eth_bfm_tx_intf_1   (eth_bfm_tx_intf_igr_1_``IDX``), \
        .eth_bfm_rx_intf_1   (eth_bfm_rx_intf_igr_1_``IDX``), \
        .eth_bfm_tx_intf_2   (eth_bfm_tx_intf_igr_2_``IDX``), \
        .eth_bfm_rx_intf_2   (eth_bfm_rx_intf_igr_2_``IDX``), \
        .eth_bfm_tx_intf_3   (eth_bfm_tx_intf_igr_3_``IDX``), \
        .eth_bfm_rx_intf_3   (eth_bfm_rx_intf_igr_3_``IDX``), \
        .eth_bfm_tx_intf_4   (eth_bfm_tx_intf_igr_4_``IDX``), \
        .eth_bfm_rx_intf_4   (eth_bfm_rx_intf_igr_4_``IDX``) \
    ); \
    \
/*
 *  ----------------
 *    End of file
 *  ----------------
*/
// <<< VIM SETTINGS
// vim: ts=4 et
// >>>
