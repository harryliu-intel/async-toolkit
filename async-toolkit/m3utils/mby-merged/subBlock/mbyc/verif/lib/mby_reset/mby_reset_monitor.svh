//-----------------------------------------------------------------------------
// Title         : Madison Bay Reset Monitor
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_reset_monitor.svh
// Author        : Jesus Alfonso Lopez Chin <jesus.a.lopez.chin@intel.com>
// Created       : 01.17.2019
//-----------------------------------------------------------------------------
// Description :
// This is the MBY RESET MONITOR. It's in charge of listening to reset events and
// notifying other components through more events.
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
class mby_reset_monitor extends shdv_reset_pkg::shdv_reset_monitor;

   mby_egr_env_if_h     vintf;
   mby_base_config      cfg_obj;

   shdv_reset_event     reset_event;
   shdv_reset_manager   reset_manager;
   shdv_reset_data      reset_data;

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_utils(mby_reset_monitor)

   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction

   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
   endfunction

   function void assign_vintf(mby_egr_env_if_h vintf);
      this.vintf = vintf;
   endfunction

   // This task will be looking at the different type of resets and will create
   // a new event accordingly
   task monitor_reset();
      wait(vintf.power_good_reset === 0);

      forever begin
         fork
            begin //Looking at "H_RESET"
               @(posedge vintf.reset);
               `uvm_info(get_name(), "jesusalo: RESET asserted. Creating a new event to notify all components.", UVM_NONE);
               reset_event             = reset_manager.get_global("egr_hard_reset");
               reset_data              = shdv_reset_data::type_id::create("reset_data");
               reset_data.rst_domain   = "egr";
               reset_data.rst_type     = "hard";

               reset_event.set(reset_data);

               @(negedge vintf.reset);
               `uvm_info(get_name(), "jesusalo: RESET de-asserted. About to clear the reset event.", UVM_NONE);
               reset_event.clear();
            end

            begin //Looking at "S_RESET"

            end
         join
      end
   endtask : monitor_reset



   task run_phase(uvm_phase phase);
      super.run_phase(phase);
   endtask : run_phase

// -------------------------------------------------------------------------
// FUNCTION: assign_cfg()
//
// This function is called by the agent once the driver is created and the
// agent has accessed the configuration database to obtain the config obj.
// This is to reduce the number of config db accesses as it has a toll in
// performance.
//
// ARGUMENTS:
//     mby_base_config cfg - A pointer to the config obj to be used by the
//                 driver
//
// -------------------------------------------------------------------------
   function void assign_cfg(mby_base_config cfg);
      this.cfg_obj = cfg;
   endfunction : assign_cfg

endclass:mby_reset_monitor


