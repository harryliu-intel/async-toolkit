//-----------------------------------------------------------------------------
// Title         : Ingress test island module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_ti_low.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This module will hold all the "shared" TB content between the IP and
// the integration level.
// MBY_IGR_TOP_RTL define should be use to monitor internal signals.
// This define will be overriden at integration level.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`ifndef MBY_IGR_TOP_RTL
   `define MBY_IGR_TOP_RTL mby_igr_tb.mby_igr_top
`endif

module mby_igr_ti_low #(
                        parameter string IP_ENV = "*.env*"
                       )
   (
     mby_igr_env_if      ingress_if
    ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_0
    ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_0
    ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_1
    ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_1
    ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_2
    ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_2
    ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_3
    ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_3
    ,mby_ec_cdi_tx_intf  eth_bfm_tx_intf_4
    ,mby_ec_cdi_rx_intf  eth_bfm_rx_intf_4
   );

   import uvm_pkg::*;
   import mby_igr_env_pkg::*;

   `include "mby_igr_params.sv"
   `include "mby_igr_defines.sv"

   initial begin
      uvm_config_db#(virtual mby_igr_env_if)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "ingress_if", ingress_if);
      uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_tx_vintf0" , eth_bfm_tx_intf_0);
      uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_rx_vintf0" , eth_bfm_rx_intf_0);
      uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_tx_vintf1" , eth_bfm_tx_intf_1);
      uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_rx_vintf1" , eth_bfm_rx_intf_1);
      uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_tx_vintf2" , eth_bfm_tx_intf_2);
      uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_rx_vintf2" , eth_bfm_rx_intf_2);
      uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_tx_vintf3" , eth_bfm_tx_intf_3);
      uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_rx_vintf3" , eth_bfm_rx_intf_3);
      uvm_config_db#(virtual mby_ec_cdi_tx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_tx_vintf4" , eth_bfm_tx_intf_4);
      uvm_config_db#(virtual mby_ec_cdi_rx_intf)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "igr_eth_bfm_rx_vintf4" , eth_bfm_rx_intf_4);
   end

   mby_igr_ti_cfg ti_config;

   initial begin
      ti_config = new();
      //set ti path for env
      ti_config.ingress_ti_low_path = $psprintf("%m");
      uvm_config_object::set(null, IP_ENV, "ingress_ti_config",ti_config);
   end

endmodule
