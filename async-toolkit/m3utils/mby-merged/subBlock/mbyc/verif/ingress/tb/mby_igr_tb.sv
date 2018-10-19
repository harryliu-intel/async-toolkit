// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog
//-----------------------------------------------------------------------------
// Title         : Ingress testbench module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_tb.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
//   This top module will:
//   1. Instance 1 RTL Unit
//   2. Control Dumping features for FSDB/DVE.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`timescale 1ps/1fs

`include "mby_igr_defines.sv"

module mby_igr_tb ();

  import uvm_pkg::*;
  
  logic primary_clock;
  logic gated_primary_clock;
  logic secondary_clock;
  logic gated_secondary_clock;
  logic ingress_primary_reset;
  logic ingress_primary_clock;

  // ===============================================
  // =                FSDB variable                =
  // ===============================================
  string  fsdb_file = "dump";
  int     fsdb_config_file;
  longint fsdb_on,fsdb_off;
  int     fsdb_file_size = 1800;
  string  str;
  logic   vccRail;

  
  // ===============================================
  // Verification Test Library
  // ===============================================
  mby_igr_test_lib test();
  
  // ===============================================
  // DUT RTL instance
  // ===============================================
`include "mby_igr_top_inst.v"

  // Parameters for interfaces
`include "mby_igr_params.sv"

  // ===============================================
  // Interfaces instance
  // ===============================================

  // ===============================================
  // Clock block instance
  // ===============================================
  int idle_conter;

  initial begin
    primary_clock       = 1'b0;
    gated_primary_clock = 1'b0;
    secondary_clock     = 1'b0;
    idle_conter         = 0;
  end

  always #416666fs primary_clock   = (ingress_if.enable_primary_clock   ? ~primary_clock   : 0);
  always #833333fs secondary_clock = (ingress_if.enable_secondary_clock ? ~secondary_clock : 0);

  always @(primary_clock) begin
    if (idle_conter > 8) begin
      gated_primary_clock <= 0;
    end else begin
      gated_primary_clock <=  primary_clock;
    end
  end

  // ===============================================
  // Reset block instance
  // ===============================================

  // ===============================================
  // INSTANCES
  // ===============================================
  // Ingress controller instance + signal connections
  // ===============================================

  // Connecting clocks to RTL
  // ===============================================
  assign ingress_primary_clock   = gated_primary_clock;
  assign ingress_secondary_clock = gated_secondary_clock;

  // Instance Ingress env interface
  // This is done in the TB if the IP need to drive also signals.
  // ===============================================
  mby_igr_env_if ingress_if();
  assign ingress_power_good_reset    = ingress_if.power_good_reset;
  assign ingress_secondary_reset     = ingress_if.secondary_reset;
  assign ingress_primary_reset       = ingress_if.primary_reset;
  assign ingress_if.primary_clock    = primary_clock;
  assign ingress_if.secondary_clock  = secondary_clock;
  assign ingress_if.ingress_int_wire = 0;

  mby_ec_cdi_tx_intf eth_bfm_tx_intf_0 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_0 (ingress_power_good_reset, primary_clock);
  assign eth_bfm_rx_intf_0.ecc             = eth_bfm_tx_intf_0.ecc;
  assign eth_bfm_rx_intf_0.port_num        = eth_bfm_tx_intf_0.port_num;
  assign eth_bfm_rx_intf_0.data_valid      = eth_bfm_tx_intf_0.data_valid;
  assign eth_bfm_rx_intf_0.metadata        = eth_bfm_tx_intf_0.metadata;
  assign eth_bfm_rx_intf_0.data_w_ecc      = eth_bfm_tx_intf_0.data_w_ecc;
  assign eth_bfm_rx_intf_0.pfc_xoff        = eth_bfm_tx_intf_0.pfc_xoff;
  assign eth_bfm_rx_intf_0.au_credits      = eth_bfm_tx_intf_0.au_credits;
  assign eth_bfm_rx_intf_0.flow_control_tc = eth_bfm_tx_intf_0.flow_control_tc;
  assign eth_bfm_tx_intf_0.enable          = 1;

  mby_ec_cdi_tx_intf eth_bfm_tx_intf_1 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_1 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_2 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_2 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_3 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_3 (ingress_power_good_reset, primary_clock);

  mby_ec_cdi_tx_intf eth_bfm_tx_intf_4 (ingress_power_good_reset, primary_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_4 (ingress_power_good_reset, primary_clock);

  assign eth_bfm_tx_intf_1.enable = 1;
  assign eth_bfm_tx_intf_2.enable = 1;
  assign eth_bfm_tx_intf_3.enable = 1;

  // ===============================================
  // Test Island instance
  // ===============================================
  mby_igr_ti_high #()
    u_ingress_ti_high (
                    .ingress_if          (ingress_if)
                   ,.eth_bfm_tx_intf_0   (eth_bfm_tx_intf_0)
                   ,.eth_bfm_rx_intf_0   (eth_bfm_rx_intf_0)
                   ,.eth_bfm_tx_intf_1   (eth_bfm_tx_intf_1)
                   ,.eth_bfm_rx_intf_1   (eth_bfm_rx_intf_1)
                   ,.eth_bfm_tx_intf_2   (eth_bfm_tx_intf_2)
                   ,.eth_bfm_rx_intf_2   (eth_bfm_rx_intf_2)
                   ,.eth_bfm_tx_intf_3   (eth_bfm_tx_intf_3)
                   ,.eth_bfm_rx_intf_3   (eth_bfm_rx_intf_3)
                   ,.eth_bfm_tx_intf_4   (eth_bfm_tx_intf_4)
                   ,.eth_bfm_rx_intf_4   (eth_bfm_rx_intf_4)
                  );

`include "std_ace_util.vic"

  initial begin
    dump_hier();
  end // initial begin

  initial begin
    dump_vcd();
  end

  initial begin
    dump_fsdb();
  end

  initial begin
    if ($test$plusargs("vpd")) begin
      $vcdpluson();
    end

    if($test$plusargs("fsdb")) begin                                    // Plusarg to enable FSDB dump
      if ($value$plusargs("FSDB_ON=%s",str)) begin                      // Plusarg to enable a start sample time after a delay
        fsdb_on = convert_time(str);                                    // Time to start (Converted time via plusarg)
        $display("FSDB DUMP:  Waiting %s before starting fsdb dump", str);
        #fsdb_on;                                                       // Wait - time before start
      end else begin
        fsdb_on = 0;                                                    // Time to start (no specified Time, so start at 0)
      end

      if ($value$plusargs("FSDB_SIZE=%d",fsdb_file_size)) begin         // Plusarg to specify a FSDB dump file size Limit
        if (fsdb_file_size<32) begin                                    // Must be at least 32
          fsdb_file_size = 32;
        end
      end

      $value$plusargs("FSDB_FILE=%s",fsdb_file);                        // Plusarg to specify user defined FSDB dump file name

      $fsdbAutoSwitchDumpfile(fsdb_file_size,fsdb_file,0,{fsdb_file,"_vf.log"}); // Enablement of Auto file switching

      if ($test$plusargs("fsdb_config")) begin                          // Plusarg to indicate a FSDB.dump.config file will be used. It indicates which modules to sample.
        $fsdbDumpvarsToFile ("fsdb.dump.config");
      end else begin
        $fsdbDumpvars(0,mby_igr_tb,"+all");                             // Default - FSDB dump of all signals in the design
        $fsdbDumpSVA();
      end

      if ($value$plusargs("FSDB_OFF=%s",str)) begin                     // Plusarg to Enable a stop time of sampling
        fsdb_off = convert_time(str) - fsdb_on;                         // Overall Time to stop, not a length of time. (Converted time via plusarg, subtract Start time)
        $display("FSDB DUMP:  Stopping FSDB dump in %s", str);
        if (fsdb_off>0) begin                                           // calculated difference must be greater than 0, else stop imediately
          #fsdb_off;                                                    // Wait - time before stop
        end
        $fsdbDumpoff();                                                 // Turn off FSDB dumping
        $display("FSDB DUMP :  Stopped FSDB dump");
        fsdb_on = -1; //fsdb is off
      end
    end // FSDB dump
  end

  function automatic longint convert_time(string in);
    longint out = in.atoi();
    case (in.substr(in.len()-2, in.len()-1))
      "ms": out *= 1000000000; // ONE BILLION ps steps in a ms unit
      "us": out *= 1000000;    // ONE MILLION ps steps in a us unit
      "ns": out *= 1000;       // ONE THOUSAND ps steps in a ns unit
    endcase // case suffix
    return out;
  endfunction // convert_time()

endmodule // mby_igr_tb
