/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_env_monitor.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY ENV monitor

 This is a monitor for MBY ENV
 
 It detect and monitor events in the IP and trigger OVM events on them
 
 Supported events:
 
 1. MBY_IOSF_PRIMARY_RESET_ASSERT
 2. MBY_IOSF_PRIMARY_RESET_DEASSERT
 3. MBY_IOSF_SIDEBAND_RESET_ASSERT
 4. MBY_IOSF_SIDEBAND_RESET_DEASSERT
 5. MBY_INT_ASSERT
 6. MBY_INT_DEASSERT
 7. MBY_DETECT_FUSEPULL_COMP_SB_MSG
 
 

*/
class mby_env_monitor extends ovm_component;

  /* 
   Variable: enable_monitor
   Enable/Disable the env monitor
   */
  protected bit enable_monitor = 1;

    `ovm_component_utils_begin(mby_env_monitor)
      `ovm_field_int(enable_monitor, OVM_ALL_ON)
    `ovm_component_utils_end

 

  // Variable: mby_if
  // Pointer to ENV interafce
  virtual mby_env_if mby_if;

  // Variable: MBYevPool
  // MBY event pool
  ovm_event_pool    MBYevPool;
  
  // Main fifo that will collect all SB trasnactions
// START IOSF_NOT_PRESENT
   tlm_analysis_fifo #(iosfsbm_cm::xaction) sb_msg_fifo;
// END IOSF_NOT_PRESENT
  /*
   Function: new
   
   constractor 
   
   */
  function new(string name="mby_pri_scbd", ovm_component parent=null);
    super.new(name,parent);
// START IOSF_NOT_PRESENT
    sb_msg_fifo  = new("sb_msg_fifo", this);
// END IOSF_NOT_PRESENT
  endfunction // new

  virtual function void build();
    super.build();
    // get global event pool
    MBYevPool = MBYevPool.get_global_pool();
  endfunction
  
  /*
   Function:  connect 
   
   connect phase of mby_env_monitor
   
   
   
   */
  function void connect();
     super.connect();
  endfunction // void

  /*
   Function:   run
   
   run  phase of mby_env_monitor
   
   invoke in parallel all monitor tasks
   
   */
  task run();
    super.run();

    if (enable_monitor == 1) begin
      fork
// START IOSF_NOT_PRESENT
	mby_primary_reset_monitor();
	mby_sideband_reset_monitor();
// END IOSF_NOT_PRESENT
	mby_int_monitor();
// START IOSF_NOT_PRESENT
	mby_sideband_monitor();
// END IOSF_NOT_PRESENT
      join_none
    end
  endtask

  
  /*
   Task: mby_int_monitor 
   
   Monitor the DUT interrupts and trigger OVM event
   
   MBY_INT_ASSERT & MBY_INT_DEASSERT
   
   
   */
  task mby_int_monitor();
    ovm_event mby_int_assert_e;
    ovm_event mby_int_deassert_e;
    mby_int_assert_e = MBYevPool.get("MBY_INT_ASSERT");
    mby_int_deassert_e = MBYevPool.get("MBY_INT_DEASSERT");

    forever begin
      @(posedge mby_if.mby_int_wire);
      mby_int_assert_e.trigger();
      `sla_msg (OVM_HIGH, get_name(), ("MBY_INT_ASSERT event detected"));
      @(negedge mby_if.mby_int_wire);
      mby_int_deassert_e.trigger();
      `sla_msg (OVM_HIGH, get_name(), ("MBY_INT_DEASSERT event detected"));
    end
  endtask // mby_int_monitor

  
  /*
   Task: mby_primary_reset_monitor 
   
   Monitor the DUT primary  reset and trigger OVM event
   
   MBY_IOSF_PRIMARY_RESET_ASSERT & MBY_IOSF_PRIMARY_RESET_DEASSERT
   
   
   */
  task mby_primary_reset_monitor();
    ovm_event mby_primary_reset_assert_e;
    ovm_event mby_primary_reset_deassert_e;
    mby_primary_reset_assert_e = MBYevPool.get("MBY_IOSF_PRIMARY_RESET_ASSERT");
    mby_primary_reset_deassert_e = MBYevPool.get("MBY_IOSF_PRIMARY_RESET_DEASSERT");

    forever begin
      @(posedge mby_if.primary_reset);
      mby_primary_reset_assert_e.trigger();
      `sla_msg (OVM_HIGH, get_name(), ("MBY_IOSF_PRIMARY_RESET_ASSERT event detected"));
      @(negedge mby_if.primary_reset);
      mby_primary_reset_deassert_e.trigger();
      `sla_msg (OVM_HIGH, get_name(), ("MBY_IOSF_PRIMARY_RESET_DEASSERT event detected"));
    end
  endtask // mby_primary_reset_monitor

  /*
   Task: mby_sideband_reset_monitor 
   
   Monitor the DUT sideband  reset and trigger OVM event
   
   MBY_IOSF_SIDEBAND_RESET_ASSERT & MBY_IOSF_SIDEBAND_RESET_DEASSERT
   
   
   */
  task mby_sideband_reset_monitor();
    ovm_event mby_sideband_reset_assert_e;
    ovm_event mby_sideband_reset_deassert_e;
    mby_sideband_reset_assert_e = MBYevPool.get("MBY_IOSF_SIDEBAND_RESET_ASSERT");
    mby_sideband_reset_deassert_e = MBYevPool.get("MBY_IOSF_SIDEBAND_RESET_DEASSERT");

    forever begin
      @(posedge mby_if.secondary_reset);
      mby_sideband_reset_assert_e.trigger();
      `sla_msg (OVM_HIGH, get_name(), ("MBY_IOSF_SIDEBAND_RESET_ASSERT event detected"));
      @(negedge mby_if.secondary_reset);
      mby_sideband_reset_deassert_e.trigger();
      `sla_msg (OVM_HIGH, get_name(), ("MBY_IOSF_SIDEBAND_RESET_DEASSERT event detected"));
    end
  endtask // mby_sideband_reset_monitor

  task mby_sideband_monitor();
    //current msg
// START IOSF_NOT_PRESENT
    iosfsbm_cm::xaction cur_msg;
    ovm_event mby_fusepull_comp_e;
    mby_fusepull_comp_e = MBYevPool.get("MBY_DETECT_FUSEPULL_COMP_SB_MSG");
    forever begin
      //blocking gate from fifo
      sb_msg_fifo.get(cur_msg);

      // FUse pull completion
      if (cur_msg.dest_pid == `MBY_SB_PORT_ID &&
	  cur_msg.src_pid  == `MBY_FUSE_PULL_EP_ID &&
	  cur_msg.opcode == iosfsbm_cm::OP_CMPD) begin
	`sla_msg (OVM_HIGH, get_name(), ("MBY_DETECT_FUSEPULL_COMP_SB_MSG event detected"));
	mby_fusepull_comp_e.trigger(cur_msg);
      end

      
      
    end
// END IOSF_NOT_PRESENT
  endtask

  /*
   Function: set_monitor_enablee
   
   Set the monitor to enable/disable
   */
  function void set_monitor_enable (bit val = 1);
     enable_monitor = val;
  endfunction
endclass // mby_env_monitor

