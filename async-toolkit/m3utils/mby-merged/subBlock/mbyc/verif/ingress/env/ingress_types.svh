//-----------------------------------------------------------------------------
// Title         : Ingress types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_types.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// Type definitions for ingress cte
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------
`ifndef INGRESS_TYPES_SV
`define INGRESS_TYPES_SV

`define NUM_VPS_PER_IGR   1
`define NUM_PORTS_PER_VP  4
`define NUM_EPLS_PER_IGR  4
`define NUM_PORTS_PER_EPL 4

typedef virtual ingress_env_if                                   igr_env_if_t;

typedef mby_ec_bfm_pkg::eth_bfm#(.MAX_PORTS(`NUM_PORTS_PER_VP))  igr_vp_bfm_t;
typedef mby_ec_bfm_pkg::eth_bfm#(.MAX_PORTS(`NUM_PORTS_PER_EPL)) igr_eth_bfm_t;
typedef mby_ec_bfm_pkg::mby_ec_cdi_tx_io                         igr_eth_bfm_tx_io_t;
typedef mby_ec_bfm_pkg::mby_ec_cdi_rx_io                         igr_eth_bfm_rx_io_t;
typedef virtual mby_ec_cdi_tx_intf                               igr_eth_bfm_tx_intf_t;
typedef virtual mby_ec_cdi_rx_intf                               igr_eth_bfm_rx_intf_t;

`endif // INGRESS_TYPES_SV

