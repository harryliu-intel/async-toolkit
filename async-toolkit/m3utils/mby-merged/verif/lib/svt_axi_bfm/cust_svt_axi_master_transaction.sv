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
//=======================================================================
// COPYRIGHT (C)  2012 SYNOPSYS INC.
// This software and the associated documentation are confidential and
// proprietary to Synopsys, Inc. Your use or disclosure of this software
// is subject to the terms and conditions of a written license agreement
// between you, or your company, and Synopsys, Inc. In the event of
// publications, the following notice is applicable:
//
// ALL RIGHTS RESERVED
//
// The entire notice above must be reproduced on all authorized copies.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//Class:    cust_axi_master_transaction
//
//This file defines a class that represents a customized AXI Master
//transaction.  This class extends the AXI VIP's svt_axi_master_transaction
//class.  It adds pre-defined distribution constraints for transaction
//weighting, and adds constraints on burst type.
//It implements the necessary virtual functions like copy(), compare(), etc...
//by using `uvm_object_utils macro.
//
//The transaction instance replaces the default master sequencer's transaction
//object, which is shown in tests/ts.basic_random_test.sv
//

`ifndef __CUST_AXI_MASTER_TRANSACTION_GUARD
`define __CUST_AXI_MASTER_TRANSACTION_GUARD



`ifndef __INSIDE_SVT_AXI_BFM_PKG__
`error "File is meant to be used only through the svt_axi_bfm_pkg.  Do not include it individually."
`endif

class cust_axi_master_transaction extends svt_axi_master_transaction;

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

  /** UVM Object Utility macro */
  `uvm_object_utils_begin(cust_axi_master_transaction)
     `uvm_field_int(burst_type_fixed_wt,UVM_ALL_ON)
     `uvm_field_int(burst_type_incr_wt ,UVM_ALL_ON)
     `uvm_field_int(burst_type_wrap_wt ,UVM_ALL_ON)
  `uvm_object_utils_end

  /** Class Constructor */
  function new (string name = "cust_axi_master_transaction");
    super.new(name);
  endfunction: new

endclass: cust_axi_master_transaction

`endif // __CUST_AXI_MASTER_TRANSACTION_GUARD
