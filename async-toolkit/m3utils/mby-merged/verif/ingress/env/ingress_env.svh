//-----------------------------------------------------------------------------
// Title         : Ingress main environment class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_env.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This class builds and connects the differents agents/BFMs/VCs needed by this
// cluster test environment
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class ingress_env extends ingress_base_env;

  ingress_ti_config ti_config;
  //protected string ingress_ti_low_path   = "XYZ_tb.u_ingress_ti";

  // Variable: _ingress_env
  // static pointer to the env
  static ingress_env _ingress_env;

  // Variable: ingress_if
  // Ingress env interface
  virtual ingress_env_if ingress_if;

  // Variable: ingress_epool
  // Ingress event pool
  uvm_event_pool    ingress_epool;

  // ---------------------------------------------------------------
  // IP Agents and VC's declaration
  // ---------------------------------------------------------------

  // Variable:  eth_cdi_bfm
  // MAC Client BFM agent
  ec_env_defines::mby_ec_bfm_t eth_cdi_bfm;

  // Variable:  cdi_tx_io
  // MAC Client BFM io policy
  ec_env_defines::cdi_tx_io_t eth_cdi_tx_io;

  // Variable:  cdi_rx_io
  // MAC Client BFM io policy
  ec_env_defines::cdi_rx_io_t eth_cdi_rx_io;

  // Variable:  cdi_tx_vintf
  // MAC Client BFM virtual interface
  ec_env_defines::cdi_tx_vintf_t cdi_tx_vintf;

  // Variable:  cdi_rx_vintf
  // MAC Client BFM virtual interface
  ec_env_defines::cdi_rx_vintf_t cdi_rx_vintf;

  // Variable: env_monitor
  // ingress env event monitor
  ingress_env_monitor env_monitor;

  `uvm_component_utils_begin(ingress_env)
  //`uvm_field_string(ingress_ti_low_path, UVM_ALL_ON)
  `uvm_component_utils_end

  function new (string name="ingress_env", uvm_component parent = null);
    super.new(name, parent);
    // Setting the env static pointer
    _ingress_env = this;
  endfunction: new

  // Function: ingress_env build
  // build phase of ingress_env
  // All VC's and Agent should be build in this pahse.
  // For each new VC's/Agnet it is recommand to add it an a specific function
  virtual function void build_phase(uvm_phase phase);
    uvm_object tmp_ti_cfg_obj;

    super.build_phase(phase);

    if(uvm_config_object::get(this, "","ingress_ti_config",tmp_ti_cfg_obj)) begin
      assert($cast(ti_config,tmp_ti_cfg_obj));
    end
    if(!uvm_config_db#(ec_env_defines::cdi_tx_vintf_t)::get(this, "", "cdi_tx_vintf", cdi_tx_vintf)) begin
      `uvm_fatal(get_name(),"Config_DB.get() for ENV's cdi_tx_vintf was not successful!")
    end
    if(!uvm_config_db#(ec_env_defines::cdi_rx_vintf_t)::get(this, "", "cdi_rx_vintf", cdi_rx_vintf)) begin
      `uvm_fatal(get_name(),"Config_DB.get() for ENV's cdi_rx_vintf was not successful!")
    end

    eth_cdi_bfm               = ec_env_defines::mby_ec_bfm_t::type_id::create("eth_cdi_bfm", this); // Create the bfm instance
    eth_cdi_bfm.cfg.mode      = eth_bfm_pkg::MODE_MASTER;                                           // Configure as MASTER
    eth_cdi_bfm.cfg.speed     = eth_bfm_pkg::SPEED_400G;                                            // Configure speed.
    eth_cdi_bfm.cfg.num_ports = 2;                                                                  // Configure num_ports.
    eth_cdi_bfm.cfg.ack_delay = 0;
    eth_cdi_bfm.cfg.enable_to_data_tx_delay = 0;
    eth_cdi_bfm.cfg.push_down_knobs();                                                              // Push Down the Config Knobs

    eth_cdi_tx_io = ec_env_defines::cdi_tx_io_t::type_id::create("eth_cdi_tx_io", this);
    eth_cdi_rx_io = ec_env_defines::cdi_rx_io_t::type_id::create("eth_cdi_rx_io", this);
    data_phase_mode = SLA_RANDOM_NONE;
    this.max_run_clocks = 2_000_000_000;

    // Env monitor
    assert($cast(env_monitor, create_component("ingress_env_monitor","env_monitor")));
    env_monitor.set_monitor_enable(cfg.get_monitors_enabled());

    // get global event pool
    ingress_epool = ingress_epool.get_global_pool();

  endfunction // void

  // Function: ingress_env connect
  // connect phase of ingress_env
  function void connect_phase(uvm_phase phase);
    uvm_object temp;
    super.connect_phase(phase);

    ingress_if = slu_resource_db#(virtual ingress_env_if)::get("ingress_if",`__FILE__,`__LINE__);

    eth_cdi_tx_io.set_vintf(cdi_tx_vintf);
    eth_cdi_rx_io.set_vintf(cdi_rx_vintf);
    eth_cdi_bfm.set_io(eth_cdi_tx_io, eth_cdi_rx_io);   // Set the IO Policy in the CDI BFM
    void'(this.add_sequencer("eth_agent", "tx0", eth_cdi_bfm.tx.frame_sequencer[0]));
    void'(this.add_sequencer("eth_agent", "tx1", eth_cdi_bfm.tx.frame_sequencer[1]));
    void'(this.add_sequencer("eth_agent", "tx2", eth_cdi_bfm.tx.frame_sequencer[2]));
    void'(this.add_sequencer("eth_agent", "tx3", eth_cdi_bfm.tx.frame_sequencer[3]));

    if (env_monitor != null) begin
      env_monitor.ingress_if = ingress_if;
    end
  endfunction // void

  // Function: ingress_env end_of_elaboration
  // end_of_elaboration  phase of ingress_env
  // In this pahse we randomize the fuse env
  virtual function void end_of_elaboration_phase (uvm_phase phase);
    super.end_of_elaboration_phase(phase);
  endfunction

  // Function: ingress_env start_of_simulation
  // start_of_simulation  phase of ingress_env
  virtual function void start_of_simulation_phase (uvm_phase phase);
    super.start_of_simulation_phase(phase);
  endfunction

  // Function: ingress_env  run
  // run  phase of ingress_env
  virtual task run_phase (uvm_phase phase);
    super.run_phase(phase);
    //fork
    //  if (_level == SLA_TOP)
        //// this task is called only in IP level
        //ingress_im_monitor();
    //join_none
  endtask // run

  // ---------------------------------------------------------------
  // Ingress ENV VC"s & VC's  UVM phases functions / tasks
  // ---------------------------------------------------------------

  // ---------------------------------------------------------------
  // Ingress ENV Saola functions / tasks
  // ---------------------------------------------------------------

  static function ingress_env get_ingress_env();
    return _ingress_env;
  endfunction

  // Saola TB clk
  virtual task set_clk_rst();
    fork
      forever begin
        @(ingress_if.primary_clock);
        #0;
        if (ingress_if.primary_clock === 1'b1)
          ->sys_clk_r;
        if (ingress_if.primary_clock === 1'b0)
          ->sys_clk_f;
      end
      forever begin
        @(ingress_if.primary_reset);
        #0;
        if (ingress_if.primary_reset === 1'b1)
          -> sys_rst_r;
        if (ingress_if.primary_reset === 1'b0)
          -> sys_rst_f;
      end
    join_none
  endtask // set_clk_rst

  // ---------------------------------------------------------------
  // Ingress ENV Specific functions / tasks
  // ---------------------------------------------------------------

  // Task: ingress_im_monitor
  // Wait for interuppt event and trigger IM
  // ISR (Interrupt Service Routine) only in IP level
  task ingress_im_monitor();
    uvm_event ingress_int_detect_e;
    ingress_int_detect_e = ingress_epool.get("INGRESS_INT_ASSERT");

    forever begin
      ingress_int_detect_e.wait_trigger();
    end
  endtask

endclass // ingress_env
