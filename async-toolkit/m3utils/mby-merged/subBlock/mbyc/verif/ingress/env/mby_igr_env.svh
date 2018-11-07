//-----------------------------------------------------------------------------
// Title         : Ingress main environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_env.svh
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
// Class: mby_igr_env
//-----------------------------------------------------------------------------
class mby_igr_env extends shdv_base_env;
   // Variable: tb_cfg
   // static pointer to the testbench configuration class
   protected mby_igr_tb_cfg tb_cfg;

   // Variable: ti_config
   // static pointer to the env
   mby_igr_ti_cfg ti_config;
   //protected string ingress_ti_low_path   = "XYZ_tb.u_ingress_ti";

   // Variable: _mby_igr_env
   // static pointer to the env
   static mby_igr_env _mby_igr_env;

   // Variable: ingress_if
   // Ingress env interface
   //igr_env_if_t ingress_if;
   igr_env_if_t ingress_if;

   // Variable: ingress_epool
   // Ingress event pool
   uvm_event_pool ingress_epool;

   ////////////////////////////////////////////////////////////////////////////
   // IP Agents and VC's declaration
   ////////////////////////////////////////////////////////////////////////////

   // Variable:  vp_bfms
   // MAC Client BFM agent
   igr_vp_bfm_t vp_bfms[`NUM_VPS_PER_IGR];

   // Variable:  eth_bfm_tx_io
   // MAC Client BFM io policy
   igr_eth_bfm_tx_io_t vp_bfm_tx_io[`NUM_VPS_PER_IGR];

   // Variable:  eth_bfm_rx_io
   // MAC Client BFM io policy
   igr_eth_bfm_rx_io_t vp_bfm_rx_io[`NUM_VPS_PER_IGR];

   // Variable:  eth_bfm_tx_vintf
   // MAC Client BFM virtual interface
   igr_eth_bfm_tx_intf_t vp_bfm_tx_vintf[`NUM_VPS_PER_IGR];

   // Variable:  eth_bfm_rx_vintf
   // MAC Client BFM virtual interface
   igr_eth_bfm_rx_intf_t vp_bfm_rx_vintf[`NUM_VPS_PER_IGR];

   // Variable:  eth_bfms
   // MAC Client BFM agent
   igr_eth_bfm_t eth_bfms[`NUM_EPLS_PER_IGR];

   // Variable:  eth_bfm_tx_io
   // MAC Client BFM io policy
   igr_eth_bfm_tx_io_t eth_bfm_tx_io[`NUM_EPLS_PER_IGR];

   // Variable:  eth_bfm_rx_io
   // MAC Client BFM io policy
   igr_eth_bfm_rx_io_t eth_bfm_rx_io[`NUM_EPLS_PER_IGR];

   // Variable:  eth_bfm_tx_vintf
   // MAC Client BFM virtual interface
   igr_eth_bfm_tx_intf_t eth_bfm_tx_vintf[`NUM_EPLS_PER_IGR];

   // Variable:  eth_bfm_rx_vintf
   // MAC Client BFM virtual interface
   igr_eth_bfm_rx_intf_t eth_bfm_rx_vintf[`NUM_EPLS_PER_IGR];

   // Variable: env_monitor
   // ingress env event monitor
   mby_igr_env_monitor env_monitor;

   `uvm_component_utils_begin(mby_igr_env)
   `uvm_component_utils_end

   //--------------------------------------------------------------------------
   // Function: mby_igr_env new
   //--------------------------------------------------------------------------
   function new (string name="mby_igr_env", uvm_component parent = null);
      super.new(name, parent);
      // Setting the env static pointer
      _mby_igr_env = this;
   endfunction : new

   //--------------------------------------------------------------------------
   // Function: mby_igr_env build
   // build phase of mby_igr_env
   // All VC's and Agent should be build in this phase.
   // For each new VC's/Agent it is recommended to add it an a specific function
   //--------------------------------------------------------------------------
   virtual function void build_phase(uvm_phase phase);
      uvm_object tmp_ti_cfg_obj;

      super.build_phase(phase);

      // "get" the testbench configuration object set from the ingress base test
      uvm_config_db#(mby_igr_tb_cfg)::get(this, "", "igr_tb_cfg", tb_cfg);
      if(tb_cfg == null) begin
         //PJP: TODO `uvm_fatal(get_name(), $sformatf("PJP: mby_igr_env:: tb_cfg is null!")); // Need to fix this as it currently breaks the fullchip integration.
         `uvm_warning(get_name(), $sformatf("PJP: mby_igr_env:: tb_cfg is null!"));
      end

      if(uvm_config_object::get(this, "", "ingress_ti_config",tmp_ti_cfg_obj)) begin
         assert($cast(ti_config,tmp_ti_cfg_obj));
      end

      build_vpt_bfms();
      build_eth_bfms();

      // Env monitor
      assert($cast(env_monitor, create_component("mby_igr_env_monitor", "env_monitor")));
//      env_monitor.set_monitor_enable(tb_cfg.get_monitors_enabled()); // PJP: TODO: get_monitors_enabled is a Saola functionkk and I'm not sure of it's usage.  I'm leaving the code here for now to either be deleted later or replaced.

      // get global event pool
      ingress_epool = ingress_epool.get_global_pool();

   endfunction : build_phase

   //--------------------------------------------------------------------------
   // Function: mby_igr_env connect
   // connect phase of mby_igr_env
   //--------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      uvm_object temp;
      super.connect_phase(phase);
      connect_vpt_bfms();
      connect_eth_bfms();
      uvm_config_db#(igr_env_if_t)::get(this, "", "ingress_if", ingress_if);
      if(ingress_if == null) begin
         `uvm_fatal(get_name(), $sformatf("Couldn't find ingress_if"));
      end

      if (env_monitor != null) begin
         env_monitor.ingress_if = ingress_if;
      end
   endfunction : connect_phase

   //--------------------------------------------------------------------------
   // Function: mby_igr_env end_of_elaboration
   // end_of_elaboration  phase of mby_igr_env
   // In this phase we randomize the fuse env
   //--------------------------------------------------------------------------
   virtual function void end_of_elaboration_phase (uvm_phase phase);
      super.end_of_elaboration_phase(phase);
   endfunction : end_of_elaboration_phase

   //--------------------------------------------------------------------------
   // Function: mby_igr_env start_of_simulation
   // start_of_simulation  phase of mby_igr_env
   //--------------------------------------------------------------------------
   virtual function void start_of_simulation_phase (uvm_phase phase);
      super.start_of_simulation_phase(phase);
   endfunction : start_of_simulation_phase

   //--------------------------------------------------------------------------
   // Function: mby_igr_env  run
   // run  phase of mby_igr_env
   //--------------------------------------------------------------------------
   virtual task run_phase (uvm_phase phase);
      super.run_phase(phase);
   endtask : run_phase

   ////////////////////////////////////////////////////////////////////////////
   // Ingress ENV VC"s & VC's  UVM phases functions / tasks
   ////////////////////////////////////////////////////////////////////////////

   //--------------------------------------------------------------------------
   // Function: build_vpt_bfms
   // Gets the virtual port interfaces from the configuration database and
   // creates the virtual port Bus Functional Model instances
   //--------------------------------------------------------------------------
   function void build_vpt_bfms();
      foreach(vp_bfms[i]) begin
         // Get the vp_bfm_vif ptrs
         if(!uvm_config_db#(igr_eth_bfm_tx_intf_t)::get(this, "", $sformatf("igr_eth_bfm_tx_vintf%0d", i+4), vp_bfm_tx_vintf[i])) begin
            `uvm_fatal(get_name(), $sformatf("Config_DB.get() for ENV's igr_eth_bfm_tx_vintf%0d was not successful!", i+4))
         end
         if(!uvm_config_db#(igr_eth_bfm_rx_intf_t)::get(this, "", $sformatf("igr_eth_bfm_rx_vintf%0d", i+4), vp_bfm_rx_vintf[i])) begin
            `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_rx_intf_t was not successful!")
         end
         // Create the vp bfm instances
         vp_bfms[i]                = igr_vp_bfm_t::type_id::create($sformatf("igr_vp_bfm%0d", i), this);
         vp_bfms[i].cfg.mode       = eth_bfm_pkg::MODE_SLAVE;                            // Configure as SLAVE
         vp_bfms[i].cfg.port_speed = {eth_bfm_pkg::SPEED_400G,                            // Configure speed.
                                      eth_bfm_pkg::SPEED_OFF,
                                      eth_bfm_pkg::SPEED_OFF,
                                      eth_bfm_pkg::SPEED_OFF};
         //vp_bfms[i].cfg.port_lanes = {4,0,0,0};                                         // Configure num_ports.
         vp_bfm_tx_io[i] = igr_eth_bfm_tx_io_t::type_id::create($sformatf("vp_bfm_tx_io%0d", i), this);
         vp_bfm_rx_io[i] = igr_eth_bfm_rx_io_t::type_id::create($sformatf("vp_bfm_rx_io%0d", i), this);
      end
   endfunction : build_vpt_bfms

   //--------------------------------------------------------------------------
   // Function: build_eth_bfms
   // Gets the eth interfaces from the configuration database and creates
   // the ethernet Bus Functional Model instances
   //--------------------------------------------------------------------------
   function void build_eth_bfms();
      foreach(eth_bfms[i]) begin
         // Get the eth_bfm_vif ptrs
         if(!uvm_config_db#(igr_eth_bfm_tx_intf_t)::get(this, "", $sformatf("igr_eth_bfm_tx_vintf%0d", i),eth_bfm_tx_vintf[i])) begin
            `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_tx_intf_t was not successful!")
         end
         if(!uvm_config_db#(igr_eth_bfm_rx_intf_t)::get(this, "", $sformatf("igr_eth_bfm_rx_vintf%0d", i), eth_bfm_rx_vintf[i])) begin
            `uvm_fatal(get_name(),"Config_DB.get() for ENV's igr_eth_bfm_rx_intf_t was not successful!")
         end
         // Create the bfm instances
         eth_bfms[i]                   = igr_eth_bfm_t::type_id::create($sformatf("igr_eth_bfm%0d", i), this);
         eth_bfms[i].cfg.mode          = eth_bfm_pkg::MODE_SLAVE;                            // Configure as SLAVE
         eth_bfms[i].cfg.port_speed    = {eth_bfm_pkg::SPEED_400G,                            // Configure speed.
                                          eth_bfm_pkg::SPEED_OFF,
                                          eth_bfm_pkg::SPEED_OFF,
                                          eth_bfm_pkg::SPEED_OFF};
         //eth_bfms[i].cfg.port_lanes    = {4,0,0,0};                                           // Configure num_ports.
         eth_bfms[i].cfg.group_size    = 8;
         eth_bfms[i].cfg.sop_alignment = 8;
         eth_bfm_tx_io[i] = igr_eth_bfm_tx_io_t::type_id::create($sformatf("eth_bfm_tx_io%0d", i), this);
         eth_bfm_rx_io[i] = igr_eth_bfm_rx_io_t::type_id::create($sformatf("eth_bfm_rx_io%0d", i), this);
      end
   endfunction : build_eth_bfms

   //--------------------------------------------------------------------------
   // Function: connect_vpt_bfms
   // Sets VIF to the IO policies, adds IO policy class to the BFM and adds sequencer
   // pointer to SLA vsqr
   //--------------------------------------------------------------------------
   function void connect_vpt_bfms();
      foreach(vp_bfms[i]) begin
         vp_bfm_tx_io[i].set_vintf(vp_bfm_tx_vintf[i]);
         vp_bfm_rx_io[i].set_vintf(vp_bfm_rx_vintf[i]);
         vp_bfms[i].set_io(vp_bfm_tx_io[i], vp_bfm_rx_io[i]);
         add_sequencer($sformatf("vp_bfm_%0d", i), $sformatf("vp_bfm_%0d_rx0", i), vp_bfms[i].rx.frame_sequencer[0]);
         add_sequencer($sformatf("vp_bfm_%0d", i), $sformatf("vp_bfm_%0d_rx1", i), vp_bfms[i].rx.frame_sequencer[1]);
         add_sequencer($sformatf("vp_bfm_%0d", i), $sformatf("vp_bfm_%0d_rx2", i), vp_bfms[i].rx.frame_sequencer[2]);
         add_sequencer($sformatf("vp_bfm_%0d", i), $sformatf("vp_bfm_%0d_rx3", i), vp_bfms[i].rx.frame_sequencer[3]);
      end
   endfunction : connect_vpt_bfms

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
   // Ingress ENV functions / tasks
   //////////////////////////////////////////////////////////////////////////////

   //---------------------------------------------------------------------------
   // Function: get_igr_env
   // Returns pointer to self
   //---------------------------------------------------------------------------
   static function mby_igr_env get_igr_env();
      return _mby_igr_env;
   endfunction : get_igr_env

   //////////////////////////////////////////////////////////////////////////////
   // Ingress ENV Specific functions / tasks
   //////////////////////////////////////////////////////////////////////////////

   //---------------------------------------------------------------------------
   // Task: ingress_im_monitor
   // Wait for interrupt event and trigger IM
   // ISR (Interrupt Service Routine) only in IP level
   //---------------------------------------------------------------------------
   task ingress_im_monitor();
      uvm_event ingress_int_detect_e;
      ingress_int_detect_e = ingress_epool.get("INGRESS_INT_ASSERT");

      forever begin
         ingress_int_detect_e.wait_trigger();
      end
   endtask : ingress_im_monitor

endclass : mby_igr_env
