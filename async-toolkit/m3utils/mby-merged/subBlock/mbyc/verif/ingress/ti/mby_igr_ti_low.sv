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
    ,mby_tag_bfm_uc_if tag_bfm_intf_0
    ,mby_tag_bfm_uc_if tag_bfm_intf_1
   );

   import uvm_pkg::*;
   import mby_igr_env_pkg::*;

   `include "mby_igr_params.sv"
   `include "mby_igr_defines.sv"
   `include "mby_igr_types.svh"

   initial begin
       
      igr_eth_bfm_tx_intf_t eth_bfm_tx_intf_array[5]; 
      igr_eth_bfm_rx_intf_t eth_bfm_rx_intf_array[5]; 
       
      // Variable:  eth_bfm_tx_io
      // MAC Client BFM io policy
      igr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_IGR];

      // Variable:  eth_bfm_rx_io
      // MAC Client BFM io policy
      igr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_IGR]; 
       
      // Variable:  eth_bfm_tx_io
      // MAC Client BFM io policy
      igr_eth_bfm_tx_io_t vp_bfm_tx_io[`NUM_VPS_PER_IGR];

      // Variable:  eth_bfm_rx_io
      // MAC Client BFM io policy
      igr_eth_bfm_rx_io_t vp_bfm_rx_io[`NUM_VPS_PER_IGR]; 
       
      eth_bfm_tx_intf_array = '{eth_bfm_tx_intf_0, eth_bfm_tx_intf_1, eth_bfm_tx_intf_2, eth_bfm_tx_intf_3, eth_bfm_tx_intf_4};
      eth_bfm_rx_intf_array = '{eth_bfm_rx_intf_0, eth_bfm_rx_intf_1, eth_bfm_rx_intf_2, eth_bfm_rx_intf_3, eth_bfm_rx_intf_4};
      
      //Create io policies and assign interfaces. Store policies for further use.
      foreach(eth_bfm_tx_io[i])begin
          eth_bfm_tx_io[i] = new($sformatf("%m.eth_bfm_tx_io%0d",i), eth_bfm_tx_intf_array[i]);
          eth_bfm_rx_io[i] = new($sformatf("%m.eth_bfm_rx_io%0d",i), eth_bfm_rx_intf_array[i]);
          uvm_config_db#(igr_eth_bfm_tx_io_t)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), $sformatf("igr_eth_bfm_tx_io%0d",i) , eth_bfm_tx_io[i]);
          uvm_config_db#(igr_eth_bfm_rx_io_t)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), $sformatf("igr_eth_bfm_rx_io%0d",i) , eth_bfm_rx_io[i]);
      end

      foreach(vp_bfm_tx_io[i])begin
          vp_bfm_tx_io[i] = new($sformatf("%m.vp_bfm_tx_io%0d",i), eth_bfm_tx_intf_array[i+4]);
          vp_bfm_rx_io[i] = new($sformatf("%m.vp_bfm_rx_io%0d",i), eth_bfm_rx_intf_array[i+4]);
          uvm_config_db#(igr_eth_bfm_tx_io_t)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), $sformatf("igr_vp_bfm_tx_io%0d",i) , vp_bfm_tx_io[i]);
          uvm_config_db#(igr_eth_bfm_rx_io_t)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), $sformatf("igr_vp_bfm_rx_io%0d",i) , vp_bfm_rx_io[i]);
      end
      
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
      uvm_config_db#(virtual mby_tag_bfm_uc_if)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "tag_bfm_vintf0" , tag_bfm_intf_0);
      uvm_config_db#(virtual mby_tag_bfm_uc_if)::set(uvm_root::get(), $sformatf("%s*",IP_ENV), "tag_bfm_vintf1" , tag_bfm_intf_1);
   end

   mby_igr_ti_cfg ti_config;

   initial begin
      ti_config = new();
      //set ti path for env
      ti_config.ingress_ti_low_path = $psprintf("%m");
      uvm_config_object::set(null, IP_ENV, "ingress_ti_config",ti_config);
   end

endmodule
