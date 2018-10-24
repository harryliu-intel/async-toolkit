//-----------------------------------------------------------------------------
// Title         : Egress test island module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_ti_low.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This moudle will hold all the "shared" TB content between the IP and
// the integration level.
// EGRESS_TOP_RTL define should be use to monitor internal signals.
// This define will be overriden at integration level.
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

module egress_ti_low #(
                    parameter string IP_ENV = "*.env*"
                   )
  (
    egress_env_if      egress_if
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

`ifdef XVM
   import ovm_pkg::*;
   import xvm_pkg::*;
   `include "ovm_macros.svh"
   `include "sla_macros.svh"
`endif

  import sla_pkg::*;
  import egress_env_pkg::*;

`include "egress_params.sv"
`include "egress_defines.sv"

  // Adding MBY if to Saola container
  initial begin
    sla_pkg::slu_resource_db#(virtual egress_env_if)::add("egress_if", egress_if, `__FILE__, `__LINE__);
    uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_tx_vintf0" , eth_bfm_tx_intf_0);
    uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_rx_vintf0" , eth_bfm_rx_intf_0);
    uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_tx_vintf1" , eth_bfm_tx_intf_1);
    uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_rx_vintf1" , eth_bfm_rx_intf_1);
    uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_tx_vintf2" , eth_bfm_tx_intf_2);
    uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_rx_vintf2" , eth_bfm_rx_intf_2);
    uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_tx_vintf3" , eth_bfm_tx_intf_3);
    uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "egr_eth_bfm_rx_vintf3" , eth_bfm_rx_intf_3);
  end

  egress_ti_config ti_config;

  initial begin
    ti_config = new();
    //set ti path for env
    ti_config.egress_ti_low_path = $psprintf("%m");
    uvm_config_object::set(null, IP_ENV, "egress_ti_config",ti_config);
  end

endmodule
