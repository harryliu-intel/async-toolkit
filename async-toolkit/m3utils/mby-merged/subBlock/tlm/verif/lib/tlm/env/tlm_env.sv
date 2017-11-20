

/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 Soala IP env

 This is the main IP env file.
 
 In this file the IP delclare, build and connects the diffrrent IP's agnets and VCs.
 
 

*/
class tlm_env extends tlm_base_env;

     tlm_rtl_config rtl_config;
    //protected string tlm_ti_low_path   = "XYZ_tb.u_tlm_ti";


    `uvm_component_utils_begin(tlm_env)
//     `uvm_field_string(tlm_ti_low_path, UVM_ALL_ON)
    `uvm_component_utils_end
      
  // Variable: _tlm_env 
  // static pointer to the env
  static tlm_env _tlm_env;
  
  /* 
   Variable: tlm_if
   TLM1 env interface
   */
  virtual tlm_env_if tlm_if;

  // ***************************************************************
  // Variable: TLM1evPool
  // TLM1 event pool
  // ***************************************************************
  uvm_event_pool    TLM1evPool;
  
  // ***************************************************************
  // IP Agents and VC's decleration
  // ***************************************************************
 


  //Chassis reset env
  // ===============================================================


  // ***************************************************************
  // IP SCBD and general checkers
  // ***************************************************************
  /* 
   Variable: pri_scbd
   TLM1 primary scbd
   */


   /* 
   Variable: env_monitor
   TLM1 env event monitor
   */
  tlm_env_monitor env_monitor;
  
  
  function new (string name="tlm_env", uvm_component parent = null);
    super.new(name, parent);

    // Steeing the env static pointer
    _tlm_env = this;
  endfunction: new
  

  // ***************************************************************
  // TLM1 ENV UVM phases functions / tasks
  // ***************************************************************

  
  /*
   FunctIon: tlm_env build
   
   build phase of tlm_env
   
   All VC's and Agent should be build in this pahse.
   
   For each new VC's/Agnet it is recommand to add it an a specific function
   */
  virtual function void build_phase(uvm_phase phase);  

    uvm_object tmp_rtl_cfg_obj;

    super.build_phase(phase);

    if(uvm_config_object::get(this, "","tlm_rtl_config",tmp_rtl_cfg_obj)) begin 
       assert($cast(rtl_config,tmp_rtl_cfg_obj));
    end 

  
    // Env monitor
    assert($cast(env_monitor, create_component("tlm_env_monitor","env_monitor")));
    env_monitor.set_monitor_enable(cfg.get_monitors_enabled());
    
    // get global event pool
    TLM1evPool = TLM1evPool.get_global_pool();

    
  endfunction // void


  /*
   Function: tlm_env connect 
   
   connect phase of tlm_env
   
   */
   function void connect_phase(uvm_phase phase);
     uvm_object temp;
     super.connect_phase(phase);
     
     tlm_if = slu_resource_db#(virtual tlm_env_if)::get("tlm_if",`__FILE__,`__LINE__);

     if( _level == SLA_TOP )
       // Connnect IOSF VC's sequencser only of IP is standalone
       begin
       end

     
     
     if (env_monitor != null) begin
       env_monitor.tlm_if = tlm_if;
     end
   endfunction // void

  /*
   Function: tlm_env end_of_elaboration 
   
   end_of_elaboration  phase of tlm_env
   
   In this pahse we randomize the fuse env
   
   */
  virtual function void end_of_elaboration_phase (uvm_phase phase);
    super.end_of_elaboration_phase(phase);
   if (_level == SLA_TOP) begin
   end

  endfunction

  /*
   Function: tlm_env start_of_simulation 
   
   start_of_simulation  phase of tlm_env
   
   */
  virtual function void start_of_simulation_phase (uvm_phase phase);
    super.start_of_simulation_phase(phase);
  endfunction

  /*
   Function: tlm_env  run
   
   run  phase of tlm_env
   
   */
  virtual task run_phase (uvm_phase phase);
    
    super.run_phase(phase);
    
    fork
      if (_level == SLA_TOP)
	// this task is called only in IP level
	tlm_im_monitor();
    join_none
    
    
  endtask // run
  


  // ***************************************************************
  // TLM1 ENV VC"s & VC's  UVM phases functions / tasks
  // ***************************************************************
  
  /*
  Function: build iosf sideband
 
  Build and set up IOSF SB VC & CFG object
 
  */


  // ***************************************************************
  // TLM1 ENV Saola functions / tasks
  // ***************************************************************
  static function tlm_env get_tlm_env();
    return _tlm_env;
  endfunction

   // Saola TB clk
  virtual task set_clk_rst();
    forever 
      begin
        ->sys_clk_r;
        #10000;
        ->sys_clk_f;
        #10000;
      end
  endtask // set_clk_rst


  // ***************************************************************
  // TLM1 ENV Specific functions / tasks
  // ***************************************************************
  
  /*
   Task: tlm_im_monitor 
   
   Wait for interuppt event and trigger IM  
   ISR (Interrupt Service Routine) only in IP level
   
   
   */
  task tlm_im_monitor();
    uvm_event tlm_int_detect_e;
    tlm_int_detect_e = TLM1evPool.get("TLM1_INT_ASSERT");

    forever begin
      tlm_int_detect_e.wait_trigger();
    end
  endtask

  
  /*
   Function: load_fuse_to_sb_vc
   
   This function load the data from the fuse env
   to the SB VC.
   
   This function should be called after every change in fuse values
   
   
   */

endclass // tlm_env

