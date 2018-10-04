
`ifndef __INSIDE_MBY_MC_TEST_LIB
`error "Attempt to include file outside of mby_mc_test_lib."
`endif

class mby_mc_ahb_random_access_test extends mby_mc_base_test;

   `uvm_component_utils_begin(mby_mc_ahb_random_access_test)
   `uvm_component_utils_end

   //------------------------------------------------------------------------------
   // Constructor: new
   // Configure the TimeFormat - $timeformat(-9, 0, "ns", 10);
   //
   //  Arguments:
   //  name   - Mplex base test object name.
   //  parent - Component parent object.
   //------------------------------------------------------------------------------
   function      new(string name = "mby_mc_ahb_random_access_test", uvm_component parent = null);
      super.new (name, parent);
      $timeformat(-9, 0, "ns", 10);
   endfunction: new

   virtual function void build_phase(uvm_phase phase);
      `uvm_info("build_phase", "Entered...", UVM_LOW)

      super.build_phase(phase);

      //cfg.env_cfg.ahb_num_slv = 1;
      //cfg.env_cfg.ahb_slv_is_active = 1;
      
      set_config_object("env", "mby_mc_tb_top_cfg", cfg, 0);
      
      `uvm_info("build_phase", "Exiting...", UVM_LOW)
   endfunction: build_phase

   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      //
      // Set the sequence to be used for the Saola user data phase.Exiting...
      //
      env.set_test_phase_type("env", "USER_DATA_PHASE", "ahb_slave_rsp_sequence");

   endfunction 

   function void final_phase(uvm_phase phase);
      uvm_report_server svr;
      `uvm_info("final_phase", "Entered...",UVM_LOW)

      super.final_phase(phase);

      `uvm_info("final_phase", "Exiting...", UVM_LOW)
   endfunction: final_phase

endclass 



class ahb_slave_rsp_sequence extends mby_mc_seq_lib::mby_mc_env_base_seq;

   /** UVM Object Utility macro */
   `uvm_object_utils(ahb_slave_rsp_sequence)

   /** Class Constructor */
   function new(string name="ahb_slave_rsp_sequence");
      super.new(name);
      set_env(slu_tb_env::get_top_tb_env());
   endfunction
  
   virtual task body();
      svt_ahb_bfm_pkg::ahb_slave_random_response_sequence  ahb_slv_rsp_seq;

      ahb_slv_rsp_seq = svt_ahb_bfm_pkg::ahb_slave_random_response_sequence::type_id::create("ahb_slv_rsp_seq");
      
      //ahb_slv_rsp_seq.start(env.ahb_bfm.ahb_system_env.slave[0].sequencer);
      //`uvm_do_on(ahb_slv_rsp_seq, env.ahb_bfm.ahb_system_env.slave[0].sequencer)

      `uvm_info("body", "Exiting...", UVM_LOW)
  endtask: body

endclass: ahb_slave_rsp_sequence

