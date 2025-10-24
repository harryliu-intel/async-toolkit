// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2015 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Nelson Crumbaker
// -- Project Name : Madison Bay
// -- Description :
// --
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_pcie_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __PCIE_ENV_SV__
`define __PCIE_ENV_SV__

class pcie_env extends uvm_env;

   pcie_agent     rc_bfm;
   pcie_agent     ep_mon; 
   pcie_sequencer rc_bfm_seqr;  // Used to put this pcie_env into the rc_bfm.sequencer.pEnv???

   `uvm_component_utils_begin(pcie_env)
      `uvm_field_object(rc_bfm, UVM_ALL_ON)
      `uvm_field_object(ep_mon, UVM_ALL_ON)
      `uvm_field_object(rc_bfm_seqr, UVM_ALL_ON)
   `uvm_component_utils_end

   function new(string name = "pcie_env", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

   virtual function void build();
      string inst_name;
      super.build();
       
      // RC BFM
      set_config_int("rc_bfm", "is_active", UVM_ACTIVE);
      set_config_int("rc_bfm", "portType", DenaliSvPcie::PCIE_PORT_RC);
      set_config_int("rc_bfm.sequencer", "count", 1);
      rc_bfm = pcie_agent::type_id::create("rc_bfm", this);

      // EP Monitor
      set_config_int("ep_mon", "is_active", UVM_PASSIVE);
      set_config_int("ep_mon", "portType", DenaliSvPcie::PCIE_PORT_EP_EXPRESS);
      set_config_int("ep_mon.monitor", "coverageEnable", 0);
      ep_mon = pcie_agent::type_id::create("ep_mon", this);
       
   endfunction : build

   virtual task run();
      int regVal, status;

      `uvm_info(get_type_name(), "Setting callbacks", UVM_LOW);

      // Enable PureSpec callbacks. Uncomment as necessary
      // Refer to the User Guide for callbacks description

      // RC BFM
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_RX_packet);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_packet);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_DL_TX_queue_exit);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_DL_RX_queue_exit);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_user_queue_exit);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_transmit_queue_enter);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_PL_TX_end_packet);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_PL_RX_end_packet);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_to_DL);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_DL_to_TL);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_completion_queue_enter);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_completion_queue_exit);
      rc_bfm.setCallback(DenaliSvPcie::PCIE_CB_TX_trans_done);

   // FIXME: this bypasses the training sequence, remove when real DUT comes online
    `uvm_info(get_type_name(), "Writing the byPassPLSpd16GT", UVM_LOW);
    regVal = rc_bfm.regInst.readReg(DenaliSvPcie::PCIE_REG_DEN_SIM_ST);
    regVal |= DenaliSvPcie::PCIE_Rmask__DEN_SIM_ST_byPassPLSpd16GT;
    rc_bfm.regInst.writeReg(DenaliSvPcie::PCIE_REG_DEN_SIM_ST, regVal);

      // Passive Ep
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_TL_RX_packet);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_packet);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_DL_TX_queue_exit);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_DL_RX_queue_exit);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_PL_TX_end_packet);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_PL_RX_end_packet);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_TL_to_DL);
      ep_mon.setCallback(DenaliSvPcie::PCIE_CB_DL_to_TL);

   // FIXME: this bypasses the training sequence, remove when real DUT comes online
   `uvm_info(get_type_name(), "Writing the byPassPLSpd16GT", UVM_LOW);
    regVal = ep_mon.regInst.readReg(DenaliSvPcie::PCIE_REG_DEN_SIM_ST);
    regVal |= DenaliSvPcie::PCIE_Rmask__DEN_SIM_ST_byPassPLSpd16GT;
    ep_mon.regInst.writeReg(DenaliSvPcie::PCIE_REG_DEN_SIM_ST, regVal);

      `uvm_info(get_type_name(), "Setting callbacks ... DONE", UVM_LOW);

      super.run();
   endtask : run

   virtual function void connect();
 
      super.connect();
       $cast(rc_bfm_seqr, rc_bfm.sequencer);
       $cast(rc_bfm_seqr.pEnv, this);

   endfunction : connect

   virtual function ep_mon_mem_inst_tclEval(string tcl_cmd);
      ep_mon.regInst.tclEval(tcl_cmd);
   endfunction : ep_mon_mem_inst_tclEval

endclass : pcie_env

`endif /* __PCIE_ENV_SV__ */

// -------------------------------------------------------------------
//
