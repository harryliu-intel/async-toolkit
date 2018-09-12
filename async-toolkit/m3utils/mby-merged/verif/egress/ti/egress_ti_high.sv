//-----------------------------------------------------------------------------
// Title         : Egress test island module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_ti_high.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// IP Test island
// This moudle will hold all the "shared" TB content between the IP and the
// integration level.
// EGRESS_TOP_RTL define should be use to monitor internal signals.
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

`ifndef EGRESS_TOP_RTL
`define EGRESS_TOP_RTL egress_tb.egress_top
`endif

module egress_ti_high #(
                         parameter string IP_ENV = "*.env*"
                        )
  (
    egress_env_if      egress_if
   ,mby_ec_cdi_tx_intf  cdi_tx_intf
   ,mby_ec_cdi_rx_intf  cdi_rx_intf
  );

  import uvm_pkg::*;

`ifdef XVM
  import ovm_pkg::*;
  import xvm_pkg::*;
`include "ovm_macros.svh"
`include "sla_macros.svh"
`endif

  import sla_pkg::*;

`include "egress_params.sv"
`include "egress_defines.sv"

  // ===============================================
  // Test Island LOW instance
  // ===============================================
  egress_ti_low #(.IP_ENV(IP_ENV))
    u_egress_ti_low (
                       .egress_if  (egress_if)
                      ,.cdi_tx_intf (cdi_tx_intf)
                      ,.cdi_rx_intf (cdi_rx_intf)
                     );

endmodule
