// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC training sequence
// -----------------------------------------------------------------------------

class fc_training_seq extends fc_base_seq;

    const string CLASSNAME = "fc_training_seq";

    // -----------------------------------------------------------------------
    function new(string name = "fc_training_seq");
        super.new(name);
    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils(fc_seq_pkg::fc_training_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    // -----------------------------------------------------------------------
    virtual task body();
        super.body();
    
        `uvm_info(get_name(), "enter fc_training_seq", UVM_LOW);
        
        `uvm_info(get_name(), "exit fc_training_seq", UVM_LOW);
    endtask
endclass
