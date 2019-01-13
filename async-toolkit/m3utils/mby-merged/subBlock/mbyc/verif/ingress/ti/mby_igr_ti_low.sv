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
    ,mby_tag_bfm_uc_if   tag_bfm_intf_0
    ,mby_tag_bfm_uc_if   tag_bfm_intf_1
   );

   import uvm_pkg::*;
   import mby_igr_env_pkg::*;
   import mby_tag_bfm_pkg::*;

   `include "mby_igr_params.sv"
   `include "mby_igr_defines.sv"
   `include "mby_igr_types.svh"

   initial begin
      /////////////////////////////////////////////////////////////////////////
      // I/O policy declarations for all BFMs
      /////////////////////////////////////////////////////////////////////////
      // MAC Client BFM io policy
      igr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_IGR];
      // MAC Client BFM io policy
      igr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_IGR];
      // MAC Client BFM io policy
      igr_eth_bfm_tx_io_t vp_bfm_tx_io[`NUM_VPS_PER_IGR];
      // MAC Client BFM io policy
      igr_eth_bfm_rx_io_t vp_bfm_rx_io[`NUM_VPS_PER_IGR];
      // Tag BFM io policy
      mby_tag_bfm_uc_io tag_bfm_io[`NUM_TAG_PORTS];

      /////////////////////////////////////////////////////////////////////////
      // virtual interface array declarations to simplify assignments
      /////////////////////////////////////////////////////////////////////////
      igr_eth_bfm_tx_intf_t eth_bfm_tx_intf_array[5];
      igr_eth_bfm_rx_intf_t eth_bfm_rx_intf_array[5];
      virtual mby_tag_bfm_uc_if  tag_bfm_vif_array[`NUM_TAG_PORTS];

      /////////////////////////////////////////////////////////////////////////
      // virtual interface array assignments
      /////////////////////////////////////////////////////////////////////////
      tag_bfm_vif_array     = '{tag_bfm_intf_0, tag_bfm_intf_1};

      eth_bfm_tx_intf_array = '{eth_bfm_tx_intf_0, eth_bfm_tx_intf_1, eth_bfm_tx_intf_2,
                                eth_bfm_tx_intf_3, eth_bfm_tx_intf_4};

      eth_bfm_rx_intf_array = '{eth_bfm_rx_intf_0, eth_bfm_rx_intf_1, eth_bfm_rx_intf_2,
                                eth_bfm_rx_intf_3, eth_bfm_rx_intf_4};

      /////////////////////////////////////////////////////////////////////////
      // Creating I/O policies and registering them in the cfg_db
      //
      /////////////////////////////////////////////////////////////////////////
      // Eth BFM
      foreach(eth_bfm_tx_io[i])begin
          eth_bfm_tx_io[i] = new($sformatf("%m.eth_bfm_tx_io%0d",i), eth_bfm_tx_intf_array[i]);
          eth_bfm_rx_io[i] = new($sformatf("%m.eth_bfm_rx_io%0d",i), eth_bfm_rx_intf_array[i]);
          uvm_config_db#(igr_eth_bfm_tx_io_t)::set(uvm_root::get(),
             $sformatf("%s*",IP_ENV), $sformatf("igr_eth_bfm_tx_io%0d",i) , eth_bfm_tx_io[i]);
          uvm_config_db#(igr_eth_bfm_rx_io_t)::set(uvm_root::get(),
             $sformatf("%s*",IP_ENV), $sformatf("igr_eth_bfm_rx_io%0d",i) , eth_bfm_rx_io[i]);
      end
      // VPT BFM
      foreach(vp_bfm_tx_io[i])begin
          vp_bfm_tx_io[i] = new($sformatf("%m.vp_bfm_tx_io%0d",i), eth_bfm_tx_intf_array[i+4]);
          vp_bfm_rx_io[i] = new($sformatf("%m.vp_bfm_rx_io%0d",i), eth_bfm_rx_intf_array[i+4]);
          uvm_config_db#(igr_eth_bfm_tx_io_t)::set(uvm_root::get(),
             $sformatf("%s*",IP_ENV), $sformatf("igr_vp_bfm_tx_io%0d",i) , vp_bfm_tx_io[i]);
          uvm_config_db#(igr_eth_bfm_rx_io_t)::set(uvm_root::get(),
             $sformatf("%s*",IP_ENV), $sformatf("igr_vp_bfm_rx_io%0d",i) , vp_bfm_rx_io[i]);
      end
      // TAG BFM
      foreach(tag_bfm_io[i]) begin
         tag_bfm_io[i] = new($sformatf("%m.tag_bfm_uc_io%0d",i), tag_bfm_vif_array[i]);
         uvm_config_db#(mby_tag_bfm_uc_io)::set(uvm_root::get(),
            $sformatf("%s*tag_bfm%0d*", IP_ENV, i), "io_pol", tag_bfm_io[i]);
      end

      /////////////////////////////////////////////////////////////////////////
      // Other interfaces registration
      /////////////////////////////////////////////////////////////////////////
      uvm_config_db#(virtual mby_igr_env_if)::set(uvm_root::get(),
         $sformatf("%s*",IP_ENV), "ingress_if", ingress_if);

   end

   mby_igr_ti_cfg ti_config;

   initial begin
      ti_config = new();
      //set ti path for env
      ti_config.ingress_ti_low_path = $psprintf("%m");
      uvm_config_object::set(null, IP_ENV, "ingress_ti_config",ti_config);
   end

endmodule
