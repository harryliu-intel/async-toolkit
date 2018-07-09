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

`ifndef __PCIE_INSTANCE_SV__
`define __PCIE_INSTANCE_SV__

class pcie_instance extends cdnPcieUvm::cdnPcieUvmInstance;

   `uvm_component_utils(pcie_instance)

   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new
   
   virtual function void build();
       super.build();
   endfunction : build

   virtual function void connect();
       super.connect();
   endfunction : connect
   
   /* virtual function int DefaultCbF (ref DenaliSvPcie::denaliPciePacket trans);
      `uvm_info (get_full_name(), $sformatf("\nDefaultCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.DefaultCbF(trans);
   endfunction : DefaultCbF */
 
   virtual function int TL_user_queue_exitCbF (ref DenaliSvPcie::denaliPciePacket trans);
      //`uvm_info (get_full_name(), $sformatf("\nTL_user_queue_exitCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.TL_user_queue_exitCbF(trans);
   endfunction : TL_user_queue_exitCbF
 
   virtual function int PL_TX_end_packetCbF (ref DenaliSvPcie::denaliPciePacket trans);
      //`uvm_info (get_full_name(), $sformatf("\nPL_TX_end_packetCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.PL_TX_end_packetCbF(trans);
   endfunction : PL_TX_end_packetCbF
 
   /* virtual function int PL_RX_end_packetCbF (ref DenaliSvPcie::denaliPciePacket trans);
      `uvm_info (get_full_name(), $sformatf("\nPL_RX_end_packetCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.PL_RX_end_packetCbF(trans);
   endfunction : PL_RX_end_packetCbF */
 
   virtual function int TL_RX_packetCbF (ref DenaliSvPcie::denaliPciePacket trans);
      //`uvm_info (get_full_name(), $sformatf("\nTL_RX_packetCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.TL_RX_packetCbF(trans);
   endfunction : TL_RX_packetCbF
 
   /* virtual function int TL_TX_completion_queue_enterCbF (ref DenaliSvPcie::denaliPciePacket trans);
      `uvm_info (get_full_name(), $sformatf("\nTL_TX_completion_queue_enterCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.TL_TX_completion_queue_enterCbF(trans);
   endfunction : TL_TX_completion_queue_enterCbF */
 
   /* virtual function int TL_TX_completion_queue_exitCbF (ref DenaliSvPcie::denaliPciePacket trans);
      `uvm_info (get_full_name(), $sformatf("\nTL_TX_completion_queue_exitCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.TL_TX_completion_queue_exitCbF(trans);
   endfunction : TL_TX_completion_queue_exitCbF */
 
   /* virtual function int TX_trans_doneCbF (ref DenaliSvPcie::denaliPciePacket trans);
      `uvm_info (get_full_name(), $sformatf("\nTX_trans_doneCbF:\n%s",trans.sprintInfo()), UVM_HIGH);
      return super.TX_trans_doneCbF(trans);
   endfunction : TX_trans_doneCbF */
 
endclass : pcie_instance

`endif /* __PCIE_INSTANCE_SV__ */

// -------------------------------------------------------------------
//
