// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Cadence Axi Virtual sequencer
// ----------------------------------------------------------------------------

class cdn_axi_virtual_sequencer extends uvm_sequencer;
  
  cdnAxiUvmSequencer master_seqr;
  cdnAxiUvmSequencer slaveSeqr;
  
  cdn_axi_env p_env;
  
  `uvm_component_utils_begin(cdn_axi_virtual_sequencer)
    `uvm_field_object(master_seqr, UVM_ALL_ON)
    `uvm_field_object(slaveSeqr, UVM_ALL_ON)
    `uvm_field_object(p_env, UVM_ALL_ON)
  `uvm_component_utils_end
   
  function new(string name = "cdn_axi_virtual_sequencer", uvm_component parent = null);
    super.new(name, parent);
  endfunction : new

endclass 
