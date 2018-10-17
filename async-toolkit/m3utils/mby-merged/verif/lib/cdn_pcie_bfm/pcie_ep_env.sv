// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Jerry Shaw
// -- Project Name : Madison Bay
// -- Description :
// --
// -------------------------------------------------------------------

`ifndef __INSIDE_CDN_PCIE_PKG__
** ERROR: This file is meant to be used only through cdn_pcie_pkg.sv.  Do not include it individually.;
`endif  /* __INSIDE_CDN_PCIE_PKG__ */

`ifndef __PCIE_EP_ENV_SV__
`define __PCIE_EP_ENV_SV__

//===============================================================================
// Class: pcie_ep_env
// Description: This class implements EP DUT environment
//              This Demo env has EP PCIe VIP as Active Agent (mimicking DUT)
//===============================================================================
class pcie_ep_env extends uvm_env;

   pcie_agent     ep_bfm;
//   pcie_sequencer ep_bfm_seqr;  // Used to put this pcie_env into the rc_bfm.sequencer.pEnv???

   `uvm_component_utils_begin(pcie_ep_env)
      `uvm_field_object(ep_bfm, UVM_ALL_ON)
 //     `uvm_field_object(ep_bfm_seqr, UVM_ALL_ON)
   `uvm_component_utils_end


  // ***************************************************************
  // Method : new
  // Desc.  : Call the constructor of the parent class.
  // ***************************************************************
   function new(string name = "pcie_ep_env", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

  // ***************************************************************
  // Method : build_phase
  // Desc.  : Creates EP agent and passes DUT configuration.
  // ***************************************************************
   virtual function void build_phase(uvm_phase phase);
      string inst_name;
      super.build_phase(phase);
       
      // EP BFM
      uvm_config_db#(int)::set(this, "ep_bfm",                     "is_active",         UVM_ACTIVE);
      uvm_config_db#(int)::set(this, "ep_bfm",                     "portType",          DenaliSvPcie::PCIE_PORT_EP_EXPRESS);
      uvm_config_db#(int)::set(this, "ep_bfm.sequencer",           "count",             0);

      uvm_config_db#(string)::set(this, "ep_bfm.inst",             "instName",          "fc_hvl_top.DUT");
//      uvm_config_db#(string)::set(this, "ep_bfm.sequencer",        "default_sequence",  "pcie_TLReadAfterWriteSeq");
//      uvm_config_db#(uvm_object_wrapper)::set(this, "ep_bfm.sequencer.run_phase/", "default_sequence", my_sequence::get_type() );
      uvm_config_db#(string)::set(this, "ep_bfm.sequencer.srcCfg", "name",              "fc_hvl_top.DUT(cfg_0_0)");
      uvm_config_db#(string)::set(this, "ep_bfm.sequencer.dstCfg", "name",              "fc_hvl_top.denali_model(cfg_0_0)");
      ep_bfm = pcie_agent::type_id::create("ep_bfm", this);

   endfunction : build_phase

  // ***************************************************************
  // Method : connect_phase
  // Desc.  : connects sequencer, configs to agents and mem instance
  // ***************************************************************
   virtual function void connect_phase(uvm_phase phase);

      super.connect_phase(phase);
 //     $cast(ep_bfm_seqr, ep_bfm.sequencer);
 //     $cast(ep_bfm_seqr.pEnv, this);

   endfunction : connect_phase



  // ***************************************************************
  // Method : run_phase
  // Desc.  : Enable all required callbacks on per-instance basis
  // ***************************************************************
   virtual task run_phase(uvm_phase phase);
    int regVal, status;

      // Enable PureSpec callbacks. Uncomment as necessary
      // Refer to the User Guide for callbacks description

      // EP BFM
      ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_RX_packet);
      ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_packet);
      ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_DL_TX_queue_exit);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_DL_RX_queue_exit);
      ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_user_queue_exit);
      ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_transmit_queue_enter);
      ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_PL_TX_end_packet);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_PL_RX_end_packet);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_to_DL);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_DL_to_TL);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_completion_queue_enter);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TL_TX_completion_queue_exit);
      // ep_bfm.setCallback(DenaliSvPcie::PCIE_CB_TX_trans_done);


//      regval = ep_bfm.regInst.readReg(DenaliSvPcie::PCIE_REG_DEN_SIM_ST);
//      regVal |= PCIE_Rmask__DEN_SIM_ST_byPassPLSpd16GT;
//      ep_bfm.regInst.writeReg(PCIE_REG_DEN_SIM_ST, regVal);

      super.run_phase(phase);
   endtask : run_phase

endclass : pcie_ep_env

`endif /* __PCIE_EP_ENV_SV__ */

// -------------------------------------------------------------------
//

