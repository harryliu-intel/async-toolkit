//-----------------------------------------------------------------------------
// Title         : Egress test island module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_ti_high.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// IP Test island
// This module will hold all the "shared" TB content between the IP and the
// integration level.
// MBY_EGR_TOP_RTL define should be use to monitor internal signals.
// This define will be override in integration level.
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`ifndef MBY_EGR_TOP_RTL
   `define MBY_EGR_TOP_RTL mby_egr_tb.mby_egr_top
`endif

module mby_egr_ti_high #(
                         parameter string IP_ENV = "*.env*"
                        )
  (
    mby_egr_env_if      egress_if
   ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_0
   ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_0
   ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_1
   ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_1
   ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_2
   ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_2
   ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_3
   ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_3
  );

   import uvm_pkg::*;

   `include "mby_egr_params.sv"
   `include "mby_egr_defines.sv"

   // ===============================================
   // Test Island LOW instance
   // ===============================================
   mby_egr_ti_low #(.IP_ENV(IP_ENV))
      u_egress_ti_low (
                       .egress_if         (egress_if)
                      ,.eth_bfm_tx_intf_0 (eth_bfm_tx_intf_0)
                      ,.eth_bfm_rx_intf_0 (eth_bfm_rx_intf_0)
                      ,.eth_bfm_tx_intf_1 (eth_bfm_tx_intf_1)
                      ,.eth_bfm_rx_intf_1 (eth_bfm_rx_intf_1)
                      ,.eth_bfm_tx_intf_2 (eth_bfm_tx_intf_2)
                      ,.eth_bfm_rx_intf_2 (eth_bfm_rx_intf_2)
                      ,.eth_bfm_tx_intf_3 (eth_bfm_tx_intf_3)
                      ,.eth_bfm_rx_intf_3 (eth_bfm_rx_intf_3)
                     );

endmodule : mby_egr_ti_high
