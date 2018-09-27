// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Derived AXI Master transaction of SNPS AXI master txn.
// -----------------------------------------------------------------------------

class axi_master_txn extends svt_axi_master_transaction;

  int burst_type_fixed_wt = 10;
  int burst_type_incr_wt  = 80;
  int burst_type_wrap_wt  = 10;

  // Declare user-defined constraints
  constraint master_constraints {
    burst_type dist { svt_axi_transaction::FIXED := burst_type_fixed_wt,
                      svt_axi_transaction::INCR  := burst_type_incr_wt,
                      svt_axi_transaction::WRAP  := burst_type_wrap_wt };

    addr >=0 ;
  }

  //UVM Object Utility macro 
  `uvm_object_utils_begin(axi_master_txn)
     `uvm_field_int(burst_type_fixed_wt,UVM_ALL_ON)
     `uvm_field_int(burst_type_incr_wt ,UVM_ALL_ON)
     `uvm_field_int(burst_type_wrap_wt ,UVM_ALL_ON)
  `uvm_object_utils_end

  // Class Constructor 
  function new (string name = "axi_master_txn");
    super.new(name);
  endfunction: new

endclass: axi_master_txn

