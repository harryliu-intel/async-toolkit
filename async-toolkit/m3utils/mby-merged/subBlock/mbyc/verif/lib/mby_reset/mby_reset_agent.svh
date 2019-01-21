//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model Package
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_reset_agent.svh
// Author        : Jesus Alfonso Lopez Chin <jesus.a.lopez.chin@intel.com>
// Created       : 01.17.2019
//-----------------------------------------------------------------------------
// Description :
// This is the SMM BFM package file
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

class mby_reset_agent extends shdv_reset_pkg::shdv_reset_agent;

   mby_reset_cfg           cfg_obj;
   mby_reset_driver        rst_driver;
   mby_reset_monitor       rst_monitor;
   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_utils(mby_reset_agent)

   function new(string name, uvm_component parent);
      super.new(name, parent);
      cfg_obj     = mby_reset_cfg::type_id::create("cfg_obj", this);
      rst_driver  = mby_reset_driver::type_id::create("rst_driver",this);
      rst_monitor = mby_reset_monitor::type_id::create("rst_monitor",this);
   endfunction

   function void build_phase(uvm_phase phase);
      //uvm_config_db#(mby_reset_cfg)::get(this, "reset_agent", "cfg_obj", cfg_obj);

      cfg_obj.randomize() with {reset_mode == RESET_EGR_MODE;};

      if(cfg_obj.driver_active == UVM_ACTIVE) begin
         rst_driver.assign_cfg(cfg_obj);
         rst_driver.cfg_obj.driver_active = UVM_ACTIVE;

      end
      if(cfg_obj.monitor_active == UVM_ACTIVE) begin
         rst_monitor.assign_cfg(cfg_obj);
         rst_monitor.cfg_obj.monitor_active = UVM_ACTIVE;

      end

   endfunction : build_phase

   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase


   task run_phase(uvm_phase phase);
      super.run_phase(phase);
   endtask : run_phase

endclass:mby_reset_agent


