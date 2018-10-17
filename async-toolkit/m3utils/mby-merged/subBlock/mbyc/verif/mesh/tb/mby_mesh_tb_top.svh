// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//
// Mesh testbench module
// Description :
//   This top module will:
//   1. Instance 1 RTL Unit
//   2. Control Dumping features for FSDB/DVE.

`timescale 1ps/1ps

module mby_mesh_tb_top();
   
   import uvm_pkg::*;
   import sla_pkg::*;
   `include "uvm_macros.svh"
   `include "slu_macros.svh"


   // =============================================== 
   // Clock block instance
   // ===============================================

   logic         fabric_clk;                             // Fabric Clock - 1.2 Ghz
   shdv_clk_gen  fabric_clk_gen(fabric_clk);

   initial begin

      fabric_clk_gen.period     = 833333fs;
      fabric_clk_gen.jitter     = 0ps;

   end

   // ===============================================
   // =                FSDB variable                =
   // ===============================================
   string       fsdb_file = "dump";
   int          fsdb_config_file;
   longint      fsdb_on,fsdb_off;
   int          fsdb_file_size = 1800;
   string       str;

   mby_mesh_tb_if mesh_tb_if();

 
   assign mesh_tb_if.fab_clk  = fabric_clk;

   shdv_base_tb_intf shdv_intf();

   assign   shdv_intf.ref_clk   = mesh_tb_if.fab_clk; 
   assign   shdv_intf.ref_rst   = mesh_tb_if.hard_reset;

   //-----------------------------------------------------------------------------
   // Verification Test Island
   //-----------------------------------------------------------------------------
   mby_mesh_ti #(
   ) mesh_ti(
       .mby_mesh_tb_if               (mesh_tb_if),
       .shdv_intf                    (shdv_intf)

   );
  
   //////////////////////////////////////////////
   // Hierarchy-Based RTL File List Dumping ////
   /////////////////////////////////////////////

   `include  "std_ace_util.vic"

   initial begin
      dump_hier();
   end

   initial begin
      dump_vcd();
   end

   initial begin
      dump_fsdb();
   end

   initial begin
      if ($test$plusargs("vpd")) begin                                      // Plusarg to Enable VPD dump of all signals in the design
         $vcdpluson();
         $vcdplusmemon();
      end

      if($test$plusargs("fsdb")) begin                                      // Plusarg to enable FSDB dump
         if ($value$plusargs("FSDB_ON=%s",str)) begin                       // Plusarg to enable a start sample time after a delay
            fsdb_on = convert_time(str);                                    // Time to start (Converted time via plusarg)
            $display("FSDB DUMP:  Waiting %s before starting fsdb dump", str);
            #fsdb_on;                                                       // Wait - time before start
         end else begin
            fsdb_on = 0;                                                    // Time to start (no specified Time, so start at 0)
         end

         if ($value$plusargs("FSDB_SIZE=%d",fsdb_file_size)) begin               // Plusarg to specify a FSDB dump file size Limit
            if (fsdb_file_size<32) begin                                         // Must be at least 32
               fsdb_file_size = 32;
            end
         end

         $value$plusargs("FSDB_FILE=%s",fsdb_file);                         // Plusarg to specify user defined FSDB dump file name

         $fsdbAutoSwitchDumpfile(fsdb_file_size,fsdb_file,0,{fsdb_file,"_vf.log"}); // Enablement of Auto file switching

         if ($test$plusargs("fsdb_config")) begin                           // Plusarg to indicate a FSDB.dump.config file will be used. It indicates which modules to sample.
            $fsdbDumpvarsToFile ("fsdb.dump.config");
         end else begin
            $fsdbDumpvars(0,mby_mesh_tb_top,"+all");                         // Default - FSDB dump of all signals in the design
         end

         if ($value$plusargs("FSDB_OFF=%s",str)) begin                      // Plusarg to Enable a stop time of sampling
            fsdb_off = convert_time(str) - fsdb_on;                         // Overall Time to stop, not a length of time. (Converted time via plusarg, subtract Start time)
            $display("FSDB DUMP:  Stopping FSDB dump in %s", str);
            if (fsdb_off>0) begin                                           // calculated difference must be greater than 0, else stop immediately
               #fsdb_off;                                                   // Wait - time before stop
            end
            $fsdbDumpoff();                                                 // Turn off FSDB dumping
            $display("FSDB DUMP :  Stopped FSDB dump");
            fsdb_on = -1; //fsdb is off
         end
      end // if ($test$plusargs("fsdb"))
   end // initial begin
   

   function automatic longint convert_time(string in);
      longint out = in.atoi();

      case (in.substr(in.len()-2, in.len()-1))
         "ms": out *= 1000000000; // ONE BILLION ps steps in a ms unit
         "us": out *= 1000000;    // ONE MILLION ps steps in a us unit
         "ns": out *= 1000;       // ONE THOUSAND ps steps in a ns unit
      endcase //case suffix

      return out;
   endfunction //convert_time()

   // ===============================================
   // Verification Test Library
   // ===============================================
   mby_mesh_test_lib test();

   // MBY Mesh Dut
   // ===============================================
   // ===============================================

endmodule
