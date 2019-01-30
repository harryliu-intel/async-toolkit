//-----------------------------------------------------------------------------
// Title         : Egress test island module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_ti_low.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This module will hold all the "shared" TB content between the IP and
// the integration level.
// MBY_EGR_TOP_RTL define should be use to monitor internal signals.
// This define will be overriden at integration level.
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

module mby_egr_ti_low #(
   parameter string IP_ENV = "*.env*"
   )(
     mby_egr_env_if            egress_if
    ,mby_ec_cdi_tx_intf        eth_bfm_tx_intf_0
    ,mby_ec_cdi_rx_intf        eth_bfm_rx_intf_0
    ,mby_ec_cdi_tx_intf        eth_bfm_tx_intf_1
    ,mby_ec_cdi_rx_intf        eth_bfm_rx_intf_1
    ,mby_ec_cdi_tx_intf        eth_bfm_tx_intf_2
    ,mby_ec_cdi_rx_intf        eth_bfm_rx_intf_2
    ,mby_ec_cdi_tx_intf        eth_bfm_tx_intf_3
    ,mby_ec_cdi_rx_intf        eth_bfm_rx_intf_3
    ,mby_tag_bfm_uc_if         tag_bfm_intf_0
    ,mby_tag_bfm_uc_if         tag_bfm_intf_1
    ,mby_smm_bfm_mrd_req_if    memrd_req_if
    ,mby_smm_bfm_mwr_req_if    memwr_req_if
   );

   import uvm_pkg::*;
   import mby_egr_env_pkg::*;
   import mby_tag_bfm_pkg::*;
   import mby_smm_bfm_pkg::*;

   `include "mby_egr_params.sv"
   `include "mby_egr_defines.sv"
   `include "mby_egr_types.svh"

   initial begin
      /////////////////////////////////////////////////////////////////////////
      // I/O policy declarations for all BFMs
      /////////////////////////////////////////////////////////////////////////
      mby_tag_bfm_uc_io          tag_bfm_io[2];
      smm_bfm_row_rd_io          smm_bfm_rd_io;
      smm_bfm_row_wr_io          smm_bfm_wr_io;
      
      // MAC Client BFM io policy
      egr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_EGR];
      // MAC Client BFM io policy
      egr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_EGR];
      // MAC Client BFM io policy

      /////////////////////////////////////////////////////////////////////////
      // virtual interface array declarations to simplify assignments
      /////////////////////////////////////////////////////////////////////////
      virtual mby_tag_bfm_uc_if  tag_bfm_vif_array[2];
      egr_eth_bfm_tx_intf_t eth_bfm_tx_intf_array[4];
      egr_eth_bfm_rx_intf_t eth_bfm_rx_intf_array[4];
      
      /////////////////////////////////////////////////////////////////////////
      // virtual interface array assignments
      /////////////////////////////////////////////////////////////////////////
      tag_bfm_vif_array = '{tag_bfm_intf_0, tag_bfm_intf_1};
      
      eth_bfm_tx_intf_array = '{eth_bfm_tx_intf_0, eth_bfm_tx_intf_1, eth_bfm_tx_intf_2,
                                eth_bfm_tx_intf_3};

      eth_bfm_rx_intf_array = '{eth_bfm_rx_intf_0, eth_bfm_rx_intf_1, eth_bfm_rx_intf_2,
                                eth_bfm_rx_intf_3};
      

      /////////////////////////////////////////////////////////////////////////
      // Creating I/O policies and registering them in the cfg_db
      //
      /////////////////////////////////////////////////////////////////////////
      // Eth BFM
      foreach(eth_bfm_tx_io[i])begin
          eth_bfm_tx_io[i] = new($sformatf("%m.eth_bfm_tx_io%0d",i), eth_bfm_tx_intf_array[i]);
          eth_bfm_rx_io[i] = new($sformatf("%m.eth_bfm_rx_io%0d",i), eth_bfm_rx_intf_array[i]);
          uvm_config_db#(egr_eth_bfm_tx_io_t)::set(uvm_root::get(),
             $sformatf("%s*",IP_ENV), $sformatf("igr_eth_bfm_tx_io%0d",i) , eth_bfm_tx_io[i]);
          uvm_config_db#(egr_eth_bfm_rx_io_t)::set(uvm_root::get(),
             $sformatf("%s*",IP_ENV), $sformatf("igr_eth_bfm_rx_io%0d",i) , eth_bfm_rx_io[i]);
      end
      // Tag bfm I/O policy constructor and uvm_config_db registration
      foreach(tag_bfm_io[i]) begin
         tag_bfm_io[i] = new($sformatf("%m.tag_bfm_uc_io%0d",i), tag_bfm_vif_array[i]);
         uvm_config_db#(mby_tag_bfm_uc_io)::set(uvm_root::get(), $sformatf("%s*tag_bfm_%0d.tag_uc_agent",IP_ENV, i), "io_policy", tag_bfm_io[i]);
      end
      /////////////////////////////////////////////////////////////////////////
      // SMM BFM I/O policies constructors and uvm_config_db registration
      smm_bfm_rd_io = new("smm_bfm_rd_io", memrd_req_if);
      uvm_config_db#(smm_bfm_row_rd_io)::set(uvm_root::get(),
         $sformatf("%s*smm_bfm*egr_rd_req_agent*", IP_ENV), "io_policy", smm_bfm_rd_io);
      smm_bfm_wr_io = new("smm_bfm_wr_io", memwr_req_if);
      uvm_config_db#(smm_bfm_row_wr_io)::set(uvm_root::get(),
         $sformatf("%s*smm_bfm*igr_wr_req_agent*", IP_ENV), "io_policy", smm_bfm_wr_io);

      /////////////////////////////////////////////////////////////////////////
      // Other interfaces registration
      /////////////////////////////////////////////////////////////////////////
      uvm_config_db#(virtual mby_egr_env_if)::set(uvm_root::get(),     $sformatf("%s*",IP_ENV), "egress_if", egress_if);
      //uvm_config_db#(virtual mby_smm_bfm_mwr_req_if)::set(uvm_root::get(), $sformatf("%s*egr_wr_req_agent*",IP_ENV), "vintf", memwr_req_if);
      //uvm_config_db#(virtual mby_smm_bfm_mrd_req_if)::set(uvm_root::get(), $sformatf("%s*egr_rd_req_agent*",IP_ENV), "vintf", memrd_req_if);
   end

   mby_egr_ti_cfg ti_config;

   initial begin
      ti_config = new();
      //set ti path for env
      ti_config.egress_ti_low_path = $psprintf("%m");
      uvm_config_object::set(null, IP_ENV, "egress_ti_config",ti_config);
   end

endmodule
