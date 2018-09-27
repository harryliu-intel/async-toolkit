//-----------------------------------------------------------------------------
// Title         : Egress env monitor
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_env_monitor.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// 
// This is a monitor for Egress env
// 
// It detect and monitor events in the IP and trigger UVM events on them
// 
// Supported events:
// 
// 5. EGRESS_INT_ASSERT
// 6. EGRESS_INT_DEASSERT
// 
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class egress_env_monitor extends uvm_component;

  // Variable: enable_monitor
  // Enable/Disable the env monitor
  protected bit enable_monitor = 1;

  // Variable: egress_if
  // Pointer to ENV interafce
  virtual egress_env_if egress_if;

  // Variable: egress_epool
  // Egress event pool
  uvm_event_pool egress_epool;

  `uvm_component_utils_begin(egress_env_monitor)
    `uvm_field_int(enable_monitor, UVM_ALL_ON)
  `uvm_component_utils_end

  function new(string name="egress_env_monitor", uvm_component parent=null);
    super.new(name,parent);
  endfunction // new

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    // get global event pool
    egress_epool = egress_epool.get_global_pool();
  endfunction
  
  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
  endfunction // void

  task run_phase (uvm_phase phase);
    super.run_phase(phase);
    if (enable_monitor == 1) begin
      fork
	egress_int_monitor();
      join_none
    end
  endtask
  
  /*
   Task: egress_int_monitor 
   Monitor the DUT interrupts and trigger UVM event
   EGRESS_INT_ASSERT & EGRESS_INT_DEASSERT
  */
  task egress_int_monitor();
    uvm_event egress_int_assert_e;
    uvm_event egress_int_deassert_e;
    egress_int_assert_e   = egress_epool.get("EGRESS_INT_ASSERT");
    egress_int_deassert_e = egress_epool.get("EGRESS_INT_DEASSERT");

    forever begin
      @(posedge egress_if.egress_int_wire);
      egress_int_assert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("EGRESS_INT_ASSERT event detected"));
      @(negedge egress_if.egress_int_wire);
      egress_int_deassert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("EGRESS_INT_DEASSERT event detected"));
    end
  endtask // egress_int_monitor

  /*
   Function: set_monitor_enablee
   Set the monitor to enable/disable
   */
  function void set_monitor_enable (bit val = 1);
    enable_monitor = val;
  endfunction

endclass // egress_env_monitor


