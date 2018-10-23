// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  10/04/2018
// Description  :  IGR test island instantiation and connectivity macros
//------------------------------------------------------------------------------

`define igr_conn(FC_ENV_PATH, DUT_PATH, IDX) \
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
