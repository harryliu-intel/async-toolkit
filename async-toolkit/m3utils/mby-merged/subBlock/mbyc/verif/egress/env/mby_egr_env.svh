//-----------------------------------------------------------------------------
// Title         : Egress main environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_env.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This class builds and connects the different agents/BFMs/VCs needed by this
// cluster test environment
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//-----------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Class: mby_egr_env
//-----------------------------------------------------------------------------
class mby_egr_env extends mby_egr_base_env;

   mby_egr_ti_cfg ti_config;
   //protected string egress_ti_low_path   = "XYZ_tb.u_egress_ti";

   // Variable: _mby_egr_env
   // static pointer to the env
   static mby_egr_env _mby_egr_env;

   // Variable: egress_if
   // Egress env interface
   egr_env_if_t egress_if;

   // Variable: egress_epool
   // Egress event pool
   uvm_event_pool egress_epool;

   // ---------------------------------------------------------------
   // IP Agents and VC's declaration
   // ---------------------------------------------------------------


   // Variable:  eth_bfms
   // MAC Client BFM agent
   egr_eth_bfm_t eth_bfms[`NUM_EPLS_PER_EGR];

   // Variable:  eth_bfm_tx_io
   // MAC Client BFM io policy
   egr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_EGR];

   // Variable:  eth_bfm_rx_io
   // MAC Client BFM io policy
   egr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_EGR];

   // Variable:  eth_bfm_tx_vintf
   // MAC Client BFM virtual interface
   egr_eth_bfm_tx_intf_t eth_bfm_tx_vintf[`NUM_EPLS_PER_EGR];

   // Variable:  eth_bfm_rx_vintf
   // MAC Client BFM virtual interface
   egr_eth_bfm_rx_intf_t eth_bfm_rx_vintf[`NUM_EPLS_PER_EGR];

   mby_tag_bfm_pkg::mby_tag_bfm tag_bfm[`NUM_TAG_PORTS];

   mby_tag_bfm_pkg::mby_tag_bfm_uc_vif tag_bfm_intf[`NUM_TAG_PORTS];
   
   mby_smm_bfm_pkg::mby_smm_bfm_row_rd_req_vif smm_bfm_intf;
   

   // Shared Memory Mesh (SMM) Instance
   mby_smm_bfm_t   smm_bfm;
   mby_smm_bfm_cfg smm_bfm_igr_wr_req_cfg;
   mby_smm_bfm_cfg smm_bfm_egr_rd_req_cfg;

   // Variable: env_monitor
   // egress env event monitor
   mby_egr_env_monitor env_monitor;

   `uvm_component_utils_begin(mby_egr_env)
   `uvm_component_utils_end

   //--------------------------------------------------------------------------
   // Function: mby_egr_env new
   //--------------------------------------------------------------------------
   function new (string name="mby_egr_env", uvm_component parent = null);
      super.new(name, parent);
      // Setting the env static pointer
      _mby_egr_env = this;
   endfunction : new

   //--------------------------------------------------------------------------
   // Function: mby_egr_env build
   // build phase of mby_egr_env
   // All VC's and Agent should be build in this pahse.
   // For each new VC's/Agnet it is recommand to add it an a specific function
   //--------------------------------------------------------------------------
   virtual function void build_phase(uvm_phase phase);
      uvm_object tmp_ti_cfg_obj;

      super.build_phase(phase);

      // "get" the testbench configuration object set from the egress base test
      uvm_config_db#(mby_egr_tb_cfg)::get(this, "", "egr_tb_cfg", tb_cfg);
      if(tb_cfg == null) begin
         //PJP: TODO `uvm_fatal(get_name(), $sformatf("PJP: mby_egr_env:: tb_cfg is null!"));
         `uvm_warning(get_name(), $sformatf("PJP: mby_egr_env:: tb_cfg is null!"));
      end

      if(uvm_config_object::get(this, "", "egress_ti_config",tmp_ti_cfg_obj)) begin
         assert($cast(ti_config,tmp_ti_cfg_obj));
      end

      //build_eth_bfms();
      build_tag_bfm();
      build_smm_bfm();


      // Env monitor
      assert($cast(env_monitor, create_component("mby_egr_env_monitor","env_monitor")));
//PJP    env_monitor.set_monitor_enable(cfg.get_monitors_enabled());

      // get global event pool
      egress_epool = egress_epool.get_global_pool();

   endfunction : build_phase

   //--------------------------------------------------------------------------
   // Function: mby_egr_env connect
   // connect phase of mby_egr_env
   //--------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      uvm_object temp;
      super.connect_phase(phase);
      //connect_eth_bfms();
      connect_tag_bfms();
      connect_smm_bfm();
      
      uvm_config_db#(egr_env_if_t)::get(this, "", "egress_if", egress_if);
      if(egress_if == null) begin
         `uvm_fatal(get_name(), $sformatf("Couldn't find egress_if"));
      end

      if (env_monitor != null) begin
         env_monitor.egress_if = egress_if;
      end
   endfunction : connect_phase


   //--------------------------------------------------------------------------
   // Function: mby_egr_env end_of_elaboration
   // end_of_elaboration  phase of mby_egr_env
   // In this phase we randomize the fuse env
   //--------------------------------------------------------------------------
   virtual function void end_of_elaboration_phase (uvm_phase phase);
      super.end_of_elaboration_phase(phase);
   endfunction : end_of_elaboration_phase

   //--------------------------------------------------------------------------
   // Function: mby_egr_env start_of_simulation
   // start_of_simulation  phase of mby_egr_env
   //--------------------------------------------------------------------------
   virtual function void start_of_simulation_phase (uvm_phase phase);
      super.start_of_simulation_phase(phase);
   endfunction : start_of_simulation_phase

   //--------------------------------------------------------------------------
   // Function: mby_egr_env  run
   // run  phase of mby_egr_env
   //--------------------------------------------------------------------------
   virtual task run_phase (uvm_phase phase);
      super.run_phase(phase);
   endtask : run_phase

   //--------------------------------------------------------------------------
   // Function: build_eth_bfms
   // Gets the eth interfaces from the configuration database and creates
   // the ethernet Bus Functional Model instances
   //--------------------------------------------------------------------------
   function void build_eth_bfms();
      foreach(eth_bfms[i]) begin
         // Get the eth_bfm_vif ptrs
         if(!uvm_config_db#(egr_eth_bfm_tx_intf_t)::get(this, "", $sformatf("egr_eth_bfm_tx_vintf%0d", i),eth_bfm_tx_vintf[i])) begin
            `uvm_fatal(get_name(),"Config_DB.get() for ENV's egr_eth_bfm_tx_intf_t was not successful!")
         end
         if(!uvm_config_db#(egr_eth_bfm_rx_intf_t)::get(this, "", $sformatf("egr_eth_bfm_rx_vintf%0d", i), eth_bfm_rx_vintf[i])) begin
            `uvm_fatal(get_name(),"Config_DB.get() for ENV's egr_eth_bfm_rx_intf_t was not successful!")
         end
         // Create the bfm instances
         eth_bfms[i]                   = egr_eth_bfm_t::type_id::create($sformatf("egr_eth_bfm%0d", i), this);
         eth_bfms[i].cfg.mode          = eth_bfm_pkg::MODE_SLAVE;                            // Configure as SLAVE
         eth_bfms[i].cfg.port_speed    = {eth_bfm_pkg::SPEED_400G,                            // Configure speed.
            eth_bfm_pkg::SPEED_OFF,
            eth_bfm_pkg::SPEED_OFF,
            eth_bfm_pkg::SPEED_OFF};
         //eth_bfms[i].cfg.port_lanes    = {4,0,0,0};                                           // Configure num_ports.
         eth_bfms[i].cfg.group_size    = 8;
         eth_bfms[i].cfg.sop_alignment = 8;
         eth_bfm_tx_io[i] = egr_eth_bfm_tx_io_t::type_id::create($sformatf("eth_bfm_tx_io%0d", i), this);
         eth_bfm_rx_io[i] = egr_eth_bfm_rx_io_t::type_id::create($sformatf("eth_bfm_rx_io%0d", i), this);
      end
   endfunction : build_eth_bfms


   function void build_tag_bfm();
      foreach(tag_bfm[i]) begin
         // Get the eth_bfm_vif ptrs
         if(!uvm_config_db#(mby_tag_bfm_uc_vif)::get(this, "", $sformatf("tag_bfm_vintf%0d",i), tag_bfm_intf[i])) begin
            `uvm_fatal(get_name(),$sformatf("Config_DB.get() for ENV's tag_bfm_intf%0d was not successful!", i))
         end
         uvm_config_db #(virtual mby_tag_bfm_uc_if)::set(null, $sformatf("uvm_test_top.env.tag_bfm_%0d.tag_uc_agent",i), "vintf", tag_bfm_intf[i]);

         tag_bfm[i] = mby_tag_bfm_pkg::mby_tag_bfm::type_id::create($sformatf("tag_bfm_%0d",i), this);
         tag_bfm[i].cfg_obj.bfm_mode = TAG_BFM_EGR_MODE;
         tag_bfm[i].cfg_obj.driver_active = UVM_ACTIVE;
         //tag_bfm[i].cfg_obj.frame_gen_active = UVM_PASSIVE;
         //tag_bfm[i].cfg_obj.monitor_active = UVM_PASSIVE;
         tag_bfm[i].cfg_obj.traffic_mode = TAG_BFM_UC_MODE;
      end
   endfunction : build_tag_bfm

   //--------------------------------------------------------------------------
   // Function: build_smm_bfm
   // Builds the instance of the SMM BFM
   //--------------------------------------------------------------------------
   function void build_smm_bfm();
   /*   if(!uvm_config_db#(mby_smm_bfm_row_rd_req_if)::get(this, "", "smm_bfm_vintf", smm_bfm_intf)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's smm_bfm_intf was not successful!")
      end */
      smm_bfm                    = mby_smm_bfm_t::type_id::create("smm_bfm", this);
      smm_bfm_igr_wr_req_cfg     = new("smm_bfm_igr_wr_req_cfg");
      smm_bfm.igr_wr_req_cfg_obj = smm_bfm_igr_wr_req_cfg;
      smm_bfm_egr_rd_req_cfg     = new("smm_bfm_egr_rd_req_cfg");
      smm_bfm.egr_rd_req_cfg_obj = smm_bfm_egr_rd_req_cfg;
   endfunction : build_smm_bfm


   //--------------------------------------------------------------------------
   // Function: connect_eth_bfms
   // Sets VIF to the IO policies, adds IO policy class to the BFM and adds sequencer
   // pointer to SLA vsqr
   //--------------------------------------------------------------------------
   function void connect_eth_bfms();
      foreach(eth_bfms[i]) begin
         eth_bfm_tx_io[i].set_vintf(eth_bfm_tx_vintf[i]);
         eth_bfm_rx_io[i].set_vintf(eth_bfm_rx_vintf[i]);
         eth_bfms[i].set_io(eth_bfm_tx_io[i], eth_bfm_rx_io[i]);
         add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_rx0", i), eth_bfms[i].rx.frame_sequencer[0]);
         add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_rx1", i), eth_bfms[i].rx.frame_sequencer[1]);
         add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_rx2", i), eth_bfms[i].rx.frame_sequencer[2]);
         add_sequencer($sformatf("eth_bfm_%0d", i), $sformatf("eth_bfm_%0d_rx3", i), eth_bfms[i].rx.frame_sequencer[3]);
      end
   endfunction : connect_eth_bfms

   //////////////////////////////////////////////////////////////////////////////
   // Egress ENV functions / tasks
   //////////////////////////////////////////////////////////////////////////////

   //--------------------------------------------------------------------------
   // Function: connect_tag_bfm
   // adds sequencer
   //--------------------------------------------------------------------------
   function void connect_tag_bfms();
      foreach(tag_bfm[i])begin
         add_sequencer($sformatf("tag_bfm_%0d", i), $sformatf("tag_bfm_uc_%0d", i), tag_bfm[i].tag_uc_agent.sequencer);
         tag_bfm[i].tag_uc_agent.vintf = tag_bfm_intf[i];
      end
   endfunction: connect_tag_bfms


   //--------------------------------------------------------------------------
   // Function: connect_tag_bfm
   // adds sequencer
   //--------------------------------------------------------------------------
   function void connect_smm_bfm();

      add_sequencer("smm_bfm", "smm_bfm_wr_req", smm_bfm.igr_wr_req_agent.sequencer);
   //   smm_bfm.egr_rd_req_agent.vintf = ;
   endfunction: connect_smm_bfm


   //---------------------------------------------------------------------------
   // Function: get_egr_env
   // Returns pointer to self
   //---------------------------------------------------------------------------
   static function mby_egr_env get_egr_env();
      return _mby_egr_env;
   endfunction : get_egr_env

   //////////////////////////////////////////////////////////////////////////////
   // Egress ENV Specific functions / tasks
   //////////////////////////////////////////////////////////////////////////////

   //---------------------------------------------------------------------------
   // Task: ingress_im_monitor
   // Wait for interrupt event and trigger IM
   // ISR (Interrupt Service Routine) only in IP level
   //---------------------------------------------------------------------------
   task egress_im_monitor();
      uvm_event egress_int_detect_e;
      egress_int_detect_e = egress_epool.get("EGRESS_INT_ASSERT");

      forever begin
         egress_int_detect_e.wait_trigger();
      end
   endtask : egress_im_monitor

endclass : mby_egr_env
