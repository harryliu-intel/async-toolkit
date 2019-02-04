//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Bus Functional Model Package
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_reset_driver.svh
// Author        : Jesus Alfonso Lopez Chin <jesus.a.lopez.chin@intel.com>
// Created       : 01.17.2019
//-----------------------------------------------------------------------------
// Description :
// This is the MBY RESET DRIVER. It's in charge of listening to reset events and
// drive the right type of reset to the reset interface.
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
class mby_reset_driver extends shdv_reset_pkg::shdv_reset_driver;

   mby_egr_env_if_h vintf;

   // VARIABLE: cfg_obj
   // The agent's configuration object
   mby_base_config cfg_obj;

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_utils(mby_reset_driver)

   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction

   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);

      //Register reset events in the reset manager related to this driver
      add_reset("egr_reset");
      add_reset("egr_power_good");
   endfunction

   // -------------------------------------------------------------------------
   // TASK: set_reset
   //
   // Whenever the right event is asserted, set_reset() method is executed.
   //
   // -------------------------------------------------------------------------
   task set_reset(shdv_reset_data data);

      // ========= EGRESS =========
      if(data.rst_domain == "egr") begin
         case (data.rst_type)
            "hard"         : begin
               vintf.reset             <= 1;
               vintf.egress_int_wire   <= 1;
            end
            "power_good"   : begin
               vintf.power_good_reset  <= 1;
               vintf.reset             <= 1;
            end
            default  : begin
               `uvm_warning(get_type_name(), "None of the expected reset events matched the incoming reset event.")
            end
         endcase
      // RESET_EVENT: hard_reset
      end

      // ========= INGRESS =========
      if(data.rst_domain == "igr") begin

      end

   endtask:set_reset

   // -------------------------------------------------------------------------
   // TASK: clear_reset
   //
   // Whenever the right event is de-asserted, clear_reset() is executed.
   //
   // -------------------------------------------------------------------------
   task clear_reset(shdv_reset_data data);

      // ========= EGRESS =========
      if(data.rst_domain == "egr") begin
         case (data.rst_type)
            "hard"         : begin
               vintf.reset             <= 0;
               vintf.egress_int_wire   <= 0;
            end
            "power_good"   : begin
               vintf.power_good_reset  <= 0;
               vintf.reset             <= 0;
            end
            default  : begin
               `uvm_warning(get_type_name(), "None of the expected reset events matched the incoming reset event.")
            end
         endcase
      // RESET_EVENT: hard_reset
      end

      // ========= INGRESS =========
      if(data.rst_domain == "igr") begin

      end

   endtask : clear_reset

   // -------------------------------------------------------------------------
   // FUNCTION: assign_vintf()
   //
   // This function is called by the agent once the driver is created and 
   // assigns the right virtual interface to the driver 
   //
   // ARGUMENTS:
   //     mby_egr_env_if_h vintf - A pointer to the vintf to drive
   //
   // -------------------------------------------------------------------------
   function void assign_vintf(mby_egr_env_if_h vintf);
      this.vintf = vintf;
   endfunction

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

endclass:mby_reset_driver


