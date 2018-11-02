//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Config Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_config.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 30.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base configuration object for Madison Bay.
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
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
`ifndef __MBY_BASE_PKG__
`error "Attempt to include file outside of mby_igr_env_pkg."
`endif
`ifndef __MBY_BASE_CONFIG__
`define __MBY_BASE_CONFIG__
//-----------------------------------------------------------------------------
// CLASS: mby_base_config
//
//-----------------------------------------------------------------------------
class mby_base_config extends shdv_base_config;

   // VARIABLE: driver_active
   // Agent is configured to be active or passive
   uvm_active_passive_enum driver_active;

   // VARIABLE: monitor_active
   // Agent is configured to be active or passive
   uvm_active_passive_enum monitor_active;

   // VARIABLE: rsp_req
   // Responses are required for this agent.
   bit rsp_req = 0;

   // UVM object utils macro
   `uvm_object_utils_begin(mby_base_config)
   `uvm_object_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name - An identifier for this configuration object.
   // -------------------------------------------------------------------------
   function new(string name = "mby_base_config");
      super.new(name);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: do_print
   //
   // print and sprint functions use the do_print function to print out the
   // class, here the driver/monitor active are printed out.
   //
   // ARGUMENTS:
   //    uvm_printer printer - APIs of the uvm_printer class are used to print
   //    the class information.
   // -------------------------------------------------------------------------
   virtual function void do_print(uvm_printer printer);
      super.do_print(printer);
      // pretty print the configuration object
      printer.print_field("driver_active" , driver_active , 1);
      printer.print_field("monitor_active", monitor_active, 1);
      printer.print_field("resp_req", rsp_req, 1);
   endfunction : do_print

endclass : mby_base_config
`endif
