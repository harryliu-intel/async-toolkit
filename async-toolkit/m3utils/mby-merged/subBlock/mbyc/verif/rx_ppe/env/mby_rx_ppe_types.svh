//-----------------------------------------------------------------------------
// Title         : RX_PPE types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : rx_ppe_types
// Author        : Nathan Mai
// Created       : 12/6/2018

//-----------------------------------------------------------------------------
// Description :
// Type definitions for rx_ppe cte
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------
`ifndef MBY_RX_PPE_TYPES_SV
`define MBY_RX_PPE_TYPES_SV

`define NUM_VPS_PER_IGR   1
`define NUM_PORTS_PER_VP  8
`define NUM_EPLS_PER_RX_PPE 1
`define NUM_PORTS_PER_EPL 16


typedef mby_ec_bfm_pkg::eth_bfm#(.MAX_PORTS(`NUM_PORTS_PER_VP))  rx_ppe_vp_bfm_t;
typedef mby_ec_bfm_pkg::eth_bfm#(.MAX_PORTS(`NUM_PORTS_PER_EPL)) rx_ppe_eth_bfm_t;
typedef mby_ec_bfm_pkg::mby_ec_cdi_tx_io                         rx_ppe_eth_bfm_tx_io_t;
typedef mby_ec_bfm_pkg::mby_ec_cdi_rx_io                         rx_ppe_eth_bfm_rx_io_t;
typedef virtual mby_ec_cdi_tx_intf                               rx_ppe_eth_bfm_tx_intf_t;
typedef virtual mby_ec_cdi_rx_intf                               rx_ppe_eth_bfm_rx_intf_t;

`endif // MBY_RX_PPE_TYPES_SV

