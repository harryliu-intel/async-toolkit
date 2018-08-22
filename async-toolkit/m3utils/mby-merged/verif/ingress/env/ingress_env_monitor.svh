//-----------------------------------------------------------------------------
// Title         : Ingress env monitor
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_env_monitor.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// 
// This is a monitor for Ingress env
// 
// It detect and monitor events in the IP and trigger UVM events on them
// 
// Supported events:
// 
// 5. INGRESS_INT_ASSERT
// 6. INGRESS_INT_DEASSERT
// 
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_env_monitor extends uvm_component;

  // Variable: enable_monitor
  // Enable/Disable the env monitor
  protected bit enable_monitor = 1;

  // Variable: ingress_if
  // Pointer to ENV interafce
  virtual ingress_env_if ingress_if;

  // Variable: ingress_epool
  // Ingress event pool
  uvm_event_pool ingress_epool;

  `uvm_component_utils_begin(ingress_env_monitor)
    `uvm_field_int(enable_monitor, UVM_ALL_ON)
  `uvm_component_utils_end

  function new(string name="ingress_env_monitor", uvm_component parent=null);
    super.new(name,parent);
  endfunction // new

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    // get global event pool
    ingress_epool = ingress_epool.get_global_pool();
  endfunction
  
  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
  endfunction // void

  task run_phase (uvm_phase phase);
    super.run_phase(phase);
    if (enable_monitor == 1) begin
      fork
	ingress_int_monitor();
      join_none
    end
  endtask
  
  /*
   Task: ingress_int_monitor 
   Monitor the DUT interrupts and trigger UVM event
   INGRESS_INT_ASSERT & INGRESS_INT_DEASSERT
  */
  task ingress_int_monitor();
    uvm_event ingress_int_assert_e;
    uvm_event ingress_int_deassert_e;
    ingress_int_assert_e   = ingress_epool.get("INGRESS_INT_ASSERT");
    ingress_int_deassert_e = ingress_epool.get("INGRESS_INT_DEASSERT");

    forever begin
      @(posedge ingress_if.ingress_int_wire);
      ingress_int_assert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("INGRESS_INT_ASSERT event detected"));
      @(negedge ingress_if.ingress_int_wire);
      ingress_int_deassert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("INGRESS_INT_DEASSERT event detected"));
    end
  endtask // ingress_int_monitor

  /*
   Function: set_monitor_enablee
   Set the monitor to enable/disable
   */
  function void set_monitor_enable (bit val = 1);
    enable_monitor = val;
  endfunction

endclass // ingress_env_monitor


