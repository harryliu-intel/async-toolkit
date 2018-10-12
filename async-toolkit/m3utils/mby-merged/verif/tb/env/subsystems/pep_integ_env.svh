// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Jerry Shaw
// Created On   :  09/22/2018
// Description  :  PEP integration environment
// -----------------------------------------------------------------------------

`ifndef _pep_integ_env__svh_
 `define _pep_integ_env__svh_

`ifdef CDN_PCIE_ENV_ENABLE

class pep_integ_env extends subsystem_base_env;

    pcie_ep_env epDutEnv0;
    pcie_env    rcBfmEnv0;

    pcie_virtual_sequencer rcSd;

   `uvm_component_utils_begin(pep_integ_env)
      `uvm_field_object(epDutEnv0, UVM_ALL_ON)
      `uvm_field_object(rcBfmEnv0, UVM_ALL_ON)
      `uvm_field_object(rcSd, UVM_ALL_ON)
   `uvm_component_utils_end


    // ***************************************************************
    // Method : new
    // Desc.  : Call the constructor of the parent class.
    // ***************************************************************
    function new( string name ="pep_integ_env", uvm_component parent = null);
      super.new(name, parent);
      set_type_override_by_type(cdnPcieUvmSequencer::get_type(),   pcie_sequencer::get_type());
      set_type_override_by_type(cdnPcieUvmDriver::get_type(),      pcie_driver::get_type());
      set_type_override_by_type(cdnPcieUvmInstance::get_type(),    pcie_instance::get_type());
      set_type_override_by_type(cdnPcieUvmMonitor::get_type(),     pcie_monitor::get_type());
      set_type_override_by_type(cdnPcieUvmMemInstance::get_type(), pcie_mem_instance::get_type());
   endfunction : new

  // ***************************************************************
  // Method : build_phase
  // Desc.  : Creates ARcPEpEnv, EpDutEnv and passes configuration
  // ***************************************************************
   virtual function void build_phase(uvm_phase phase);
      string inst_name;
      super.build_phase(phase);

      uvm_config_db#(string)::set(this, "rcBfmEnv0.ep_mon", "hdlPath",      "fc_hvl_top.denali_monitor");
      uvm_config_db#(string)::set(this, "rcBfmEnv0.rc_bfm", "hdlPath",      "fc_hvl_top.denali_model");

      uvm_config_db#(string)::set(this, "epDutEnv0.ep_bfm", "hdlPath",      "fc_hvl_top.DUT");

      uvm_config_db#(string)::set(this, "rcBfmEnv0.rc_bfm.sequencer.srcConfig", "name", "fc_hvl_top.denali_model(cfg_0_0)");
      uvm_config_db#(string)::set(this, "rcBfmEnv0.rc_bfm.sequencer.dstConfig", "name", "fc_hvl_top.DUT(cfg_0_0)");

      uvm_config_db#(string)::set(this, "epDutEnv0.ep_bfm.sequencer.srcConfig", "name", "fc_hvl_top.DUT(cfg_0_0)");
      uvm_config_db#(string)::set(this, "epDutEnv0.ep_bfm.sequencer.dstConfig", "name", "fc_hvl_top.denali_model(cfg_0_0)");

      epDutEnv0       = pcie_ep_env::type_id::create("epDutEnv0", this);
      rcBfmEnv0       = pcie_env::type_id::create("rcBfmEnv0", this);
      rcSd            = pcie_virtual_sequencer::type_id::create("rcSd", this);
//      epSd            = pcie_virtual_sequencer::type_id::create("epSd", this);

   endfunction : build_phase

  // ***************************************************************
  // Method : Connect_phase
  // Desc.  : connects env sequencer with agent sequencer
  // ***************************************************************
   virtual function void connect_phase(uvm_phase phase);

      super.connect_phase(phase);
      $cast(rcSd.pSeqr0, rcBfmEnv0.rc_bfm.sequencer);
      $cast(rcSd.pEnv, rcBfmEnv0);

//      $cast(epSd.pSeqr0, epDutEnv0.ep_bfm.sequencer);
//      $cast(epSd.pEnv, epDutEnv0);

   endfunction : connect_phase

endclass : pep_integ_env



`endif // CDN_PCIE_ENV_ENABLE

`endif // _pep_integ_env__svh_
