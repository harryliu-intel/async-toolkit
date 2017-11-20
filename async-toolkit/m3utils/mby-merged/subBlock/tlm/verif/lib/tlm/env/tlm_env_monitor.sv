/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_env_monitor.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 ENV monitor

 This is a monitor for TLM1 ENV
 
 It detect and monitor events in the IP and trigger UVM events on them
 
 Supported events:
 
 1. TLM1_IOSF_PRIMARY_RESET_ASSERT
 2. TLM1_IOSF_PRIMARY_RESET_DEASSERT
 3. TLM1_IOSF_SIDEBAND_RESET_ASSERT
 4. TLM1_IOSF_SIDEBAND_RESET_DEASSERT
 5. TLM1_INT_ASSERT
 6. TLM1_INT_DEASSERT
 7. TLM1_DETECT_FUSEPULL_COMP_SB_MSG
 
 

*/
class tlm_env_monitor extends uvm_component;

  /* 
   Variable: enable_monitor
   Enable/Disable the env monitor
   */
  protected bit enable_monitor = 1;

    `uvm_component_utils_begin(tlm_env_monitor)
      `uvm_field_int(enable_monitor, UVM_ALL_ON)
    `uvm_component_utils_end

 

  // Variable: tlm_if
  // Pointer to ENV interafce
  virtual tlm_env_if tlm_if;

  // Variable: TLM1evPool
  // TLM1 event pool
  uvm_event_pool    TLM1evPool;
  
  // Main fifo that will collect all SB trasnactions
  /*
   Function: new
   
   constractor 
   
   */
  function new(string name="tlm_pri_scbd", uvm_component parent=null);
    super.new(name,parent);
  endfunction // new

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    // get global event pool
    TLM1evPool = TLM1evPool.get_global_pool();
  endfunction
  
  /*
   Function:  connect 
   
   connect phase of tlm_env_monitor
   
   
   
   */
  function void connect_phase(uvm_phase phase);
     super.connect_phase(phase);
  endfunction // void

  /*
   Function:   run
   
   run  phase of tlm_env_monitor
   
   invoke in parallel all monitor tasks
   
   */
  task run_phase (uvm_phase phase);
    super.run_phase(phase);

    if (enable_monitor == 1) begin
      fork
	tlm_int_monitor();
      join_none
    end
  endtask

  
  /*
   Task: tlm_int_monitor 
   
   Monitor the DUT interrupts and trigger UVM event
   
   TLM1_INT_ASSERT & TLM1_INT_DEASSERT
   
   
   */
  task tlm_int_monitor();
    uvm_event tlm_int_assert_e;
    uvm_event tlm_int_deassert_e;
    tlm_int_assert_e = TLM1evPool.get("TLM1_INT_ASSERT");
    tlm_int_deassert_e = TLM1evPool.get("TLM1_INT_DEASSERT");

    forever begin
      @(posedge tlm_if.tlm_int_wire);
      tlm_int_assert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("TLM1_INT_ASSERT event detected"));
      @(negedge tlm_if.tlm_int_wire);
      tlm_int_deassert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("TLM1_INT_DEASSERT event detected"));
    end
  endtask // tlm_int_monitor

  
  /*
   Task: tlm_primary_reset_monitor 
   
   Monitor the DUT primary  reset and trigger UVM event
   
   TLM1_IOSF_PRIMARY_RESET_ASSERT & TLM1_IOSF_PRIMARY_RESET_DEASSERT
   
   
   */
  task tlm_primary_reset_monitor();
    uvm_event tlm_primary_reset_assert_e;
    uvm_event tlm_primary_reset_deassert_e;
    tlm_primary_reset_assert_e = TLM1evPool.get("TLM1_IOSF_PRIMARY_RESET_ASSERT");
    tlm_primary_reset_deassert_e = TLM1evPool.get("TLM1_IOSF_PRIMARY_RESET_DEASSERT");

    forever begin
      @(posedge tlm_if.primary_reset);
      tlm_primary_reset_assert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("TLM1_IOSF_PRIMARY_RESET_ASSERT event detected"));
      @(negedge tlm_if.primary_reset);
      tlm_primary_reset_deassert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("TLM1_IOSF_PRIMARY_RESET_DEASSERT event detected"));
    end
  endtask // tlm_primary_reset_monitor

  /*
   Task: tlm_sideband_reset_monitor 
   
   Monitor the DUT sideband  reset and trigger UVM event
   
   TLM1_IOSF_SIDEBAND_RESET_ASSERT & TLM1_IOSF_SIDEBAND_RESET_DEASSERT
   
   
   */
  task tlm_sideband_reset_monitor();
    uvm_event tlm_sideband_reset_assert_e;
    uvm_event tlm_sideband_reset_deassert_e;
    tlm_sideband_reset_assert_e = TLM1evPool.get("TLM1_IOSF_SIDEBAND_RESET_ASSERT");
    tlm_sideband_reset_deassert_e = TLM1evPool.get("TLM1_IOSF_SIDEBAND_RESET_DEASSERT");

    forever begin
      @(posedge tlm_if.secondary_reset);
      tlm_sideband_reset_assert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("TLM1_IOSF_SIDEBAND_RESET_ASSERT event detected"));
      @(negedge tlm_if.secondary_reset);
      tlm_sideband_reset_deassert_e.trigger();
      `slu_msg (UVM_HIGH, get_name(), ("TLM1_IOSF_SIDEBAND_RESET_DEASSERT event detected"));
    end
  endtask // tlm_sideband_reset_monitor

  task tlm_sideband_monitor();
    //current msg
  endtask

  /*
   Function: set_monitor_enablee
   
   Set the monitor to enable/disable
   */
  function void set_monitor_enable (bit val = 1);
     enable_monitor = val;
  endfunction
endclass // tlm_env_monitor

