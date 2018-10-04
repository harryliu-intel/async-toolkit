// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB Master Transaction
// -----------------------------------------------------------------------------

class apb_master_txn extends svt_apb_master_transaction;

  int WRITE_wt = 40;
  int READ_wt  = 40;
  int IDLE_wt  = 10;

  // Declare user-defined constraints
  constraint master_constraints {
    xact_type dist { svt_apb_transaction::WRITE := WRITE_wt,
                     svt_apb_transaction::READ  := READ_wt,
                     svt_apb_transaction::IDLE  := IDLE_wt
                   }; 
  }

  /** UVM Object Utility macro */
  `uvm_object_utils_begin(apb_master_txn)
     `uvm_field_int(WRITE_wt, UVM_ALL_ON)
     `uvm_field_int(READ_wt, UVM_ALL_ON)
     `uvm_field_int(IDLE_wt, UVM_ALL_ON)
  `uvm_object_utils_end

  /** Class Constructor */
  function new (string name = "apb_master_txn");
    super.new(name);
  endfunction: new

endclass: apb_master_txn

