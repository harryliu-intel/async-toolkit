

/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY Soala IP env

 This is the main IP env file.
 
 In this file the IP delclare, build and connects the diffrrent IP's agnets and VCs.
 
 

*/
class mby_env extends mby_base_env;

     mby_rtl_config rtl_config;
    //protected string mby_ti_low_path   = "XYZ_tb.u_mby_ti";


    `ovm_component_utils_begin(mby_env)
//     `ovm_field_string(mby_ti_low_path, OVM_ALL_ON)
    `ovm_component_utils_end
      
  // Variable: _mby_env 
  // static pointer to the env
  static mby_env _mby_env;
  
  /* 
   Variable: mby_if
   MBY env interface
   */
  virtual mby_env_if mby_if;

  // ***************************************************************
  // Variable: MBYevPool
  // MBY event pool
  // ***************************************************************
  ovm_event_pool    MBYevPool;
  
  // ***************************************************************
  // IP Agents and VC's decleration
  // ***************************************************************
 
// START IOSF_NOT_PRESENT 
  // IOSF primary channel
  // ===============================================================
  IosfFabricVc MBYIosfPriVc;  
  
  // IOSF Sideband port
  // ===============================================================
  iosfsbm_fbrc::iosfsbm_fbrcvc MBYIosfSbVc;
// END IOSF_NOT_PRESENT 


  //Chassis reset env
  // ===============================================================
// START CHASSIS_NOT_PRESENT
  chassis_reset_env            ch_rst_env;
// END CHASSIS_NOT_PRESENT


  // ***************************************************************
  // IP SCBD and general checkers
  // ***************************************************************
  /* 
   Variable: pri_scbd
   MBY primary scbd
   */

// START IOSF_NOT_PRESENT
  mby_pri_scbd pri_scbd;
// END IOSF_NOT_PRESENT

   /* 
   Variable: env_monitor
   MBY env event monitor
   */
  mby_env_monitor env_monitor;
  
  
  function new (string name="mby_env", ovm_component parent = null);
    super.new(name, parent);

    // Steeing the env static pointer
    _mby_env = this;
  endfunction: new
  

  // ***************************************************************
  // MBY ENV OVM phases functions / tasks
  // ***************************************************************

  
  /*
   FunctIon: mby_env build
   
   build phase of mby_env
   
   All VC's and Agent should be build in this pahse.
   
   For each new VC's/Agnet it is recommand to add it an a specific function
   */
  virtual function void build();

    ovm_object tmp_rtl_cfg_obj;

    super.build();

    if(get_config_object("mby_rtl_config",tmp_rtl_cfg_obj)) begin 
       assert($cast(rtl_config,tmp_rtl_cfg_obj));
    end 

// START IOSF_NOT_PRESENT
    // Build IOSF VC's
    cfg.MBYIosfSbVcCfg.inst_name = {rtl_config.mby_ti_low_path ,".mby_iosf_sb_ti"};
    cfg.MBYIosfPriVcCfg.iosfAgtCfg[1].intfName = { rtl_config.mby_ti_low_path ,".mby_iosf_primary_ti"};
    cfg.MBYIosfPriVcCfg.iosfAgtCfg[1].isActive = ovm_active_passive_enum'( _level == SLA_TOP);
    cfg.MBYIosfSbVcCfg.is_active = ovm_active_passive_enum'(_level == SLA_TOP);

    MBYIosfPriVc = IosfPkg::IosfFabricVc::type_id::create( "MBYIosfPriVc", this);
    set_config_object ("MBYIosfPriVc", "iosfFabCfg", cfg.MBYIosfPriVcCfg, 0);
    MBYIosfPriVc.setIosfRevision (Iosf::REV110);

    MBYIosfSbVc = iosfsbm_fbrc::iosfsbm_fbrcvc::type_id::create("MBYIosfSbVc", this);
    set_config_object ("MBYIosfSbVc", "fabric_cfg", cfg.MBYIosfSbVcCfg , 0);
// END IOSF_NOT_PRESENT
// START CHASSIS_NOT_PRESENT
    // Build chassis reset VC
  if(_level == SLA_TOP && cfg.mby_has_reset_pkg)
    build_ch_rst_env();
// END CHASSIS_NOT_PRESENT
  
// START IOSF_NOT_PRESENT
    //scbd
    pri_scbd = mby_pri_scbd::type_id::create("pri_scbd", this);
    pri_scbd.set_scbd_enable(cfg.get_checkers_enabled());
// END IOSF_NOT_PRESENT
    // Env monitor
    assert($cast(env_monitor, create_component("mby_env_monitor","env_monitor")));
    env_monitor.set_monitor_enable(cfg.get_monitors_enabled());
    
    // get global event pool
    MBYevPool = MBYevPool.get_global_pool();

    
  endfunction // void


  /*
   Function: mby_env connect 
   
   connect phase of mby_env
   
   */
   function void connect();
     ovm_object temp;
     super.connect();
     
     mby_if = sla_resource_db#(virtual mby_env_if)::get("mby_if",`__FILE__,`__LINE__);

     if( _level == SLA_TOP )
       // Connnect IOSF VC's sequencser only of IP is standalone
       begin
// START IOSF_NOT_PRESENT
         void'(this.add_sequencer( "IOSF", cfg.mby_primary_access, MBYIosfPriVc.getSequencer()));
         void'(this.add_sequencer( "IOSF", cfg.mby_sideband_access, MBYIosfSbVc.get_sequencer()));
// END IOSF_NOT_PRESENT
       end

     
     
// START IOSF_NOT_PRESENT
     // Enbale trackers based on CFG object seeting
     if (cfg.get_trackers_enabled()) begin
	 // Enable IOSF primary tracker
	 MBYIosfPriVc.openTrackerFile("IOSF_PRI_MBY_TRK.out");
	 // Enable IOSF Secondary tracker
	 MBYIosfSbVc.open_tracker_file("IOSF_SB_MBY_TRK.out");
     end

     if (pri_scbd != null) begin
       MBYIosfPriVc.iosfMonAnalysisPort.connect(pri_scbd.expected_transaction_port);
       MBYIosfPriVc.iosfMonAnalysisPort.connect(pri_scbd.actaul_transaction_port);
     end
     
// END IOSF_NOT_PRESENT
     if (env_monitor != null) begin
       env_monitor.mby_if = mby_if;
// START IOSF_NOT_PRESENT
       MBYIosfSbVc.tx_ap.connect(env_monitor.sb_msg_fifo.analysis_export);
       MBYIosfSbVc.rx_ap.connect(env_monitor.sb_msg_fifo.analysis_export);
// END IOSF_NOT_PRESENT
     end
   endfunction // void

  /*
   Function: mby_env end_of_elaboration 
   
   end_of_elaboration  phase of mby_env
   
   In this pahse we randomize the fuse env
   
   */
  virtual function void end_of_elaboration ();
    super.end_of_elaboration();
   if (_level == SLA_TOP) begin
// START IOSF_NOT_PRESENT
     load_fuse_to_sb_vc();
// END IOSF_NOT_PRESENT
   end

  endfunction

  /*
   Function: mby_env start_of_simulation 
   
   start_of_simulation  phase of mby_env
   
   */
  virtual function void start_of_simulation ();
    super.start_of_simulation();
  endfunction

  /*
   Function: mby_env  run
   
   run  phase of mby_env
   
   */
  virtual task run();
    
    super.run();
    
    //fork
    //  if (_level == SLA_TOP)
	//// this task is called only in IP level
	//mby_im_monitor();
    //join_none
    
    
  endtask // run
  


  // ***************************************************************
  // MBY ENV VC"s & VC's  OVM phases functions / tasks
  // ***************************************************************
  
  /*
  Function: build iosf sideband
 
  Build and set up IOSF SB VC & CFG object
 
  */

// START CHASSIS_NOT_PRESENT

  /*
  Function: build chassis reset env
 
  Build and set up the Chassis Reset Package
 
  */

  function build_ch_rst_env();
    string pg_seqr_name 			= "pg_seqr";
    string pgcb_seqr_name 			= "pgcb_seqr";
    string ccu_seqr_name 			= "ccu_seqr";

    ch_rst_env = chassis_reset_env::type_id::create("mby_chassis_rst_env", this);
    if(_level == SLA_TOP)
        set_config_int("*mby_chassis_rst_env", "is_active", 1);

  //CH config - Change this code to configure the chassis reset pkg.
    ch_rst_env.chassis_cfg.ch_env_cfg.set_pg_seqr_name(pg_seqr_name);
    ch_rst_env.chassis_cfg.ch_env_cfg.set_ccu_seqr_name(ccu_seqr_name);
    ch_rst_env.chassis_cfg.ch_env_cfg.set_ip_env_type(chassis_defines::SIP); // Change this for PMC type
    ch_rst_env.chassis_cfg.ch_env_cfg.set_pmc_sideband_port_id(`MBY_PMC_EP_ID);
    ch_rst_env.chassis_cfg.ch_env_cfg.set_ip_ready_opcode(`MBY_RDY_FOR_RST_OPCODE);
    ch_rst_env.chassis_cfg.ch_env_cfg.set_pmc_secure_sai(`MBY_PMC_SAI);
    ch_rst_env.chassis_cfg.ch_env_cfg.set_num_early_boot_done(0);
    ch_rst_env.chassis_cfg.ch_env_cfg.tracker_enable("MBY_CHASSIS_RST_TRK");
    ch_rst_env.chassis_cfg.ch_env_cfg.set_verbose_debug_mode(cfg.mby_chassis_rst_verbose_dbg);


    //PG - Add code here to configure the Chassis PG VC that emulates the PMC
    ch_rst_env.chassis_cfg.pg_cfg.SetTrackerName("MBY_POWER_GATING_TRK");
    ch_rst_env.chassis_cfg.pg_cfg.AddFETBlock(.name("MBY_FET0"), .index(0));
    ch_rst_env.chassis_cfg.pg_cfg.AddSBEP(.index(0), .source_id(`MBY_SB_PORT_ID), .AON_EP('b0), .pmc_wake_index(0));
    ch_rst_env.chassis_cfg.pg_cfg.AddPrimEP(.index(0), .source_id(0), .AON_EP('b0), .pmc_wake_index(0));
    ch_rst_env.chassis_cfg.pg_cfg.AddSIPPGCB(.name("MBY_PGD1_PGCB"), 
                                                    .index(0), 
                                                    .fet_index(0), 
                                                    .pmc_wake_index(0), 
                                                    .sw_ent_index(0),
                                                    .SB_array({0}),
                                                    .prim_array({0}),
                                                    .initial_state(PowerGating::POWER_GATED), 
                                                    .sip_index(0), 
                                                    .ungate_priority(0), 
                                                    .ignore_sw_req(0), 
                                                    .initial_restore_asserted(0));

    ch_rst_env.chassis_cfg.pg_cfg.AddSIP(.name("MBY_SIP"), .sip_type(PowerGating::HOST), .pgcb_array({0}));	

    //CCU - Add code here to configure the Chassis CCU_VC (See CCU_VC integration guide)
    // Add your clock sources
    ch_rst_env.chassis_cfg.ccu_cfg.add_clk_source(0,   "DUMMY_CLK_SRC", 8000);

    // Add clock slices - up to parameter SLICE_NUM
    ch_rst_env.chassis_cfg.ccu_cfg.add_slice(.dcg_blk_num       (0),
                    .slice_num         (0), 
                    .slice_name        ("DUMMY_CLK"), 
                    .clk_src           (0),
                    .clk_status        (ccu_types::CLK_GATED),
                    .divide_ratio      (ccu_types::DIV_1),
                    .half_divide_ratio (0),
                    .clkack_delay      (8),
                    .usync_enabled     (0));

    //************************
    //Add reset groups and configure them
    //************************
    // Example:
    //  ch_rst_env.chassis_cfg.group_cfg.num_groups = 1;


    //  ch_rst_env.chassis_cfg.group_cfg.add_group_soc_details(.name("HOST"), 
    //          .force_message_mc_encoding('h88), 
    //          .boot_type_id({'h00}), 
    //          .sleep_reset_type_id({'h01, 'h03, 'h04, 'h05}),
    //          .cold_reset_type_id({'h11}), 
    //          .warm_reset_type_id({'h10}), 		
    //          .pwrgood_reset_name("HOST"),
    //          .deep_reset_name("HOST")
    //          //.pre_group("SOC")
    //  );

    //  ch_rst_env.chassis_cfg.group_cfg.set_group_ip_details(.name("HOST"), 
    //          .pg_subip_name({"SIP1"}), 
    //          .boot_prep_early({'hA0}),
    //          .ip_ready({'hA0 }),
    //          .boot_prep_general({'hA0}),
    //          .reset_prep_general({'hA0})			
    //  );
            

    /************************
    // pwrgood and deep reset
    ************************/
    //   ch_rst_env.chassis_cfg.pwrgood_deep_cfg.add_pwrgood_reset("HOST");
    //   ch_rst_env.chassis_cfg.pwrgood_deep_cfg.add_deep_reset("HOST");


    // Set the IOSF SB VC
    ch_rst_env.set_iosf_fabric_vc({MBYIosfSbVc}, {cfg.mby_sideband_access});

  endfunction // build_ch_rst_env
// END CHASSIS_NOT_PRESENT

  // ***************************************************************
  // MBY ENV Saola functions / tasks
  // ***************************************************************
  static function mby_env get_mby_env();
    return _mby_env;
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
  // MBY ENV Specific functions / tasks
  // ***************************************************************
  
  /*
   Task: mby_im_monitor 
   
   Wait for interuppt event and trigger IM  
   ISR (Interrupt Service Routine) only in IP level
   
   
   */
  task mby_im_monitor();
    ovm_event mby_int_detect_e;
    mby_int_detect_e = MBYevPool.get("MBY_INT_ASSERT");

    forever begin
      mby_int_detect_e.wait_trigger();
// START IOSF_NOT_PRESENT      
      im.interrupt("MBY_INT",SLA_FALSE, {cfg.mby_primary_access});
// END IOSF_NOT_PRESENT      
    end
  endtask

  
  /*
   Function: load_fuse_to_sb_vc
   
   This function load the data from the fuse env
   to the SB VC.
   
   This function should be called after every change in fuse values
   
   
   */
// START IOSF_NOT_PRESENT
  virtual function  load_fuse_to_sb_vc();
    sla_fuse_group mby_fuse_group;
    bit [15:0] mby_fuse_id;
    sla_fuse_pull_data fuseAddrDataQ[$];
    iosfsbm_cm::flit_t fuse_data[];
    iosfsbm_cm::flit_t fuse_sai[$];
    byte mby_fuse_sai;
    MBY_types::reqType rtype;
    MBY_types::reqOpCode opcode;
    mby_fuse_group = fuse.get_group_by_name("mby_fuse");
    mby_fuse_id = mby_fuse_group.get_fuse_id();
    mby_fuse_sai =  mby_fuse_group.get_sai();
    fuse_sai  = {8'h00,mby_fuse_sai, 8'h00, 8'h00}; 
    rtype = rtype.first;
    opcode = opcode.first;
    do 
      begin
	fuse_data.delete();
	fuse.get_pull_data_by_rtype(
				    .fuse_id            (mby_fuse_id),
				    .rtype              (rtype.name()),
				    .addr_data_q        (fuseAddrDataQ)
				    );
	
	if (fuseAddrDataQ.size() != 0)
	  begin
	    foreach (fuseAddrDataQ[i])
	      begin
		iosfsbm_cm::flit_t pkt[];
		genFusePacket(fuseAddrDataQ[i], pkt);
		fuse_data = new[fuse_data.size() + pkt.size()] (fuse_data);
		foreach (pkt[i])
		  begin
		    fuse_data[fuse_data.size - pkt.size + i] = pkt[i];
		  end
	      end // foreach (fuseAddrDataQ[i])
	    
	    
	    // Cause the VC to respond to fuse request
	    MBYIosfSbVc.set_compl_data_and_sai (`MBY_FUSE_PULL_EP_ID, 
						opcode, fuse_data, fuse_sai); 
	    MBYIosfSbVc.register_cb (iosfsbm_cm::SIMPLE, {opcode});
	  end
	rtype = rtype.next;
	opcode = opcode.next;
      end
    while (rtype != rtype.first);
    
  endfunction // load_fuse_to_sb_vc
    
  function void genFusePacket (
			       input  sla_fuse_pull_data fd,
			       output iosfsbm_cm::flit_t pkt[]
			       );
    int bits, bytes, dws;
    bit [3:0] fbe, lbe;
    iosfsbm_cm::flit_t stream[$];
    // Clear arrays
    stream = {};
    pkt.delete;
    // Initialize variables before further processing
    bits  = fd.data.size();
    bytes = (bits + 7) / 8;
    fbe   = (2**bytes) - 1;
    lbe   = ((fd.rcvr_addr[1:0] + bytes) > 4) ? 4'hF: 4'h0;
    // Generate FBE
    repeat (fd.rcvr_addr[1:0]) fbe = fbe << 1;
    // Append the byte array with 0 for the first empty bytes
    repeat (fd.rcvr_addr[1:0]) stream.push_back(0);
    // Append the fuse bit Q to aligned with byte size
    for (int i = bits; i < (bytes * 8); i++)
        if ((i + 1) > bits) fd.data.push_back(0);
    // Pack fuse bit into byte array
    for (int i = 0; i < (bytes * 8); i+=8)
      stream.push_back({
			fd.data[i+7], fd.data[i+6], fd.data[i+5], fd.data[i+4],
			fd.data[i+3], fd.data[i+2], fd.data[i+1], fd.data[i+0]
			});
    // Apend the last empty bytes
    bytes = stream.size();
    dws   = (bytes + 3) / 4;
    repeat ((dws * 4) - bytes) stream.push_back(0);
    // Generate LBE
    repeat ((dws * 4) - bytes) lbe = lbe >> 1;
    // Create packet
    pkt = new[4 + stream.size()];
    // Set header
    pkt[0] = fd.rcvr_addr[9:2];
    pkt[1] = {2'b0, fd.rcvr_addr[15:10]};
    pkt[2] = {lbe, fbe};
    pkt[3] = {3'b0, dws[4:0]};
    // Set data stream
    foreach (stream[i]) pkt[i+4] = stream[i];
  endfunction : genFusePacket
// END IOSF_NOT_PRESENT

endclass // mby_env

