// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog
//-----------------------------------------------------------------------------
// Title         : Ingress testbench module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_tb.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
//   This top module will:
//   1. Instance DUT
//   2. Control Dumping features for FSDB/DVE.
//   3. Create interface instances and test island.
//   4. Create clock/reset
//   5. Instance white model
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`timescale 1ps/1fs

`include "ingress_defines.sv"

module ingress_tb ();

`ifdef XVM
  import ovm_pkg::*;
  import xvm_pkg::*;
 `include "ovm_macros.svh"
 `include "slu_macros.svh"
`endif

  import uvm_pkg::*;
  
  // ===============================================
  // =                FSDB variable                =
  // ===============================================
  string  fsdb_file = "dump";
  int     fsdb_config_file;
  longint fsdb_on,fsdb_off;
  int     fsdb_file_size = 1800;
  string  str;
  logic   vccRail;

  logic ingress_clock;
  logic ingress_reset;

  // ===============================================
  // Verification Test Library
  // ===============================================
  ingress_test_lib test();
  
  // ===============================================
  // DUT RTL instance
  // ===============================================
  `include "ingress_top_inst.v"

  // Parameters for interfaces
  `include "ingress_params.sv"

  // ===============================================
  // Interfaces instance
  // ===============================================

  // eth_bfm interfaces
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_0 (ingress_reset, ingress_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_0 (ingress_reset, ingress_clock);
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_1 (ingress_reset, ingress_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_1 (ingress_reset, ingress_clock);
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_2 (ingress_reset, ingress_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_2 (ingress_reset, ingress_clock);
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_3 (ingress_reset, ingress_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_3 (ingress_reset, ingress_clock);
  mby_ec_cdi_tx_intf eth_bfm_tx_intf_4 (ingress_reset, ingress_clock);
  mby_ec_cdi_rx_intf eth_bfm_rx_intf_4 (ingress_reset, ingress_clock);

  assign eth_bfm_tx_intf_0.enable = 1;
  assign eth_bfm_tx_intf_1.enable = 1;
  assign eth_bfm_tx_intf_2.enable = 1;
  assign eth_bfm_tx_intf_3.enable = 1;
  assign eth_bfm_tx_intf_4.enable = 1;

  // ===============================================
  // Clock block instance
  // ===============================================
  shdv_clk_gen  ingress_clk_gen(ingress_clock);

  initial begin : clk_gen_settings
    ingress_clk_gen.period = 833333fs;
    ingress_clk_gen.jitter = 0ps;
  end


  // ===============================================
  // INSTANCES
  // ===============================================

  // ===============================================
  // MBY White Model instance
  // ===============================================
  mby_wm_top mby_wm();

  // ===============================================
  // Instance Ingress env interface
  // This is done in the TB if the IP need to drive also signals.
  // ===============================================
  ingress_env_if ingress_if();
  assign ingress_reset               = ingress_if.reset;
  assign ingress_if.clock            = ingress_clock;

  // ===============================================
  // Test Island instance
  // ===============================================
  ingress_ti_high #()
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
        $fsdbDumpvars(0,ingress_tb,"+all");                             // Default - FSDB dump of all signals in the design
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

endmodule // ingress_tb
