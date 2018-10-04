// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//   Class:    ahb_bfm_env
//
//   This is the SVT_AHB_BFM Environment file.
//------------------------------------------------------------------------------
typedef class cust_svt_ahb_system_configuration;
   
class ahb_bfm_env extends uvm_env;

   // Defines a System ENV for the AHB SVT suite. 
   // The AHB System ENV encapsulates the master agents,
   // slave agents, Interconnect Env, system monitor, 
   // system sequencer and the system configuration.
   // The number of master and slave agents is configured
   //  based on the system configuration provided by the user.
   // The user can specify whether Interconnect Env is 
   // required in the System Env, using the system configuration

   /** Variable:  ahb_system_env
   /** AHB System ENV */
   svt_ahb_system_env   ahb_system_env;

   // Variable:  ahb_if
   /** Handle to the virtual interface */
   virtual svt_ahb_if ahb_if;

   // Variable:  sequencer
   /** Virtual Sequencer */
   ahb_virtual_sequencer sequencer;

   // Variable:  cfg
   /** AHB System Configuration */
   cust_svt_ahb_system_configuration cfg;

   /** UVM Component Utility macro */
   `uvm_component_utils(ahb_bfm_env)

   /** Class Constructor */
   // ------------------------------------------------------------------------
   // Constructor: new
   // Create a Static pointer to this environment. Define the config type. Create
   // the env_cfg for controlling the SVT BFM.
   //
   // Arguments:
   // name   - ahb_bfm_env object name.
   // parent - parent component.
   // ------------------------------------------------------------------------
   function new (string name="ahb_bfm_env", uvm_component parent=null);
      super.new (name, parent);
   endfunction

   /** Function to set interface handle */
   // ------------------------------------------------------------------------
   // Function: set_vif
   //
   // Arguments:
   // ahb_vif : virtual interface handle 
   // ------------------------------------------------------------------------
   virtual function void set_vif(virtual svt_ahb_if ahb_vif);
      ahb_if = ahb_vif;
   endfunction 
   
   /** Build the AHB System ENV */
   // ------------------------------------------------------------------------
   // Function: build_phase()
   // Get Handle to the Test Island Path.
   // Create Synopsys AHB BFM Configuration and Agent. Based on selected mode.
   // As well set up the custom configuration for those objects.
   // Create CallBack's.
   //
   // Arguments:
   // phase   - uvm_phase handle.
   // ------------------------------------------------------------------------
   virtual function void build_phase(uvm_phase phase);
      `uvm_info("build_phase", "Entered...",UVM_LOW)

      super.build_phase(phase);

      uvm_config_db#(svt_ahb_vif)::set(this, "ahb_system_env", "vif", ahb_if);

      
      /**
      * Check if the configuration is passed to the environment.
      * If not then create the configuration and pass it to the agent.
      */

      if (cfg == null) begin
         cfg = cust_svt_ahb_system_configuration::type_id::create("cfg");
	 `uvm_info ("build_phase" , $sformatf("ahb_cfg : %s", cfg.sprint()), UVM_NONE) 
      end

      /** Set config db to system env's cfg */
      uvm_config_db#(svt_ahb_system_configuration)::set(this, "ahb_system_env", "cfg", cfg);
      
      /** Construct the system agent */
      ahb_system_env = svt_ahb_system_env::type_id::create("ahb_system_env", this);
      /** Construct the virtual sequencer */
      sequencer = ahb_virtual_sequencer::type_id::create("sequencer", this);

      `uvm_info("build_phase", "Exiting...", UVM_LOW)
   endfunction

   /** Connect phase: connect various components*/
   // ------------------------------------------------------------------------
   // Function: connect_phase()
   // Connect the CallBack's.  Connect the Virtual Sequencer.
   //
   // Arguments:
   // phase   - uvm_phase handle.
   // ------------------------------------------------------------------------
   virtual function void connect_phase(uvm_phase phase);
      `uvm_info("connect_phase", "Entered...",UVM_LOW)

      super.connect_phase(phase);
      
   endfunction 
   
   /** Set up the ahb bfm: set num of masters, slaves, configuration and data width */
   // ------------------------------------------------------------------------
   // Function setup_bfm()
   // Set the number of AHB BFM Masters, Slaves and data width.
   //
   // Arguments:
   // int num_masters        -Number of AHB masters.
   // int num_slaves         -Number of AHB Slaves.
   // bit mstr_is_active     -AHB Master is_active.
   // bit slv_is_active      -AHB Slave is_active.

   // ------------------------------------------------------------------------
   function void setup_bfm(int num_masters, int num_slaves, bit mstr_is_active, bit slv_is_active);

      cfg.num_masters       = num_masters;
      cfg.num_slaves        = num_slaves;
      cfg.create_sub_cfgs(num_masters,num_slaves);

      for (int idx = 1 ; idx <= num_masters ; idx ++ ) begin
         cfg.master_cfg[idx-1].ahb_interface_type = ahb_bfm_defines::AHB_INTERFACE_TYPE;
         cfg.master_cfg[idx-1].data_width =  ahb_bfm_defines::AHB_DATA_WIDTH;
         cfg.master_cfg[idx-1].addr_width = ahb_bfm_defines::AHB_ADDR_WIDTH;
         cfg.master_cfg[idx-1].is_active = mstr_is_active;

         //Enable tracker file generation.
         cfg.master_cfg[idx-1].enable_tracing = 1;
         cfg.master_cfg[idx-1].enable_reporting = 1;

         // Enable transaction level coverage
         cfg.master_cfg[idx-1].transaction_coverage_enable = 1;
         cfg.master_cfg[idx-1].pa_format_type = svt_xml_writer::FSDB;
      end

      for (int idx = 1 ; idx <= num_slaves ; idx ++ ) begin
         cfg.slave_cfg[idx-1].ahb_interface_type = ahb_bfm_defines::AHB_INTERFACE_TYPE;
         cfg.slave_cfg[idx-1].is_active = slv_is_active;
         cfg.slave_cfg[idx-1].data_width = ahb_bfm_defines::AHB_DATA_WIDTH;
         cfg.slave_cfg[idx-1].addr_width = ahb_bfm_defines::AHB_ADDR_WIDTH;

         //Enable tracker file generation.
         cfg.slave_cfg[idx-1].enable_tracing = 1;
         cfg.slave_cfg[idx-1].enable_reporting = 1;

         // Enable transaction level coverage
         cfg.slave_cfg[idx-1].transaction_coverage_enable = 1;
         cfg.slave_cfg[idx-1].pa_format_type= svt_xml_writer::FSDB;
      end
  endfunction: setup_bfm

endclass 

