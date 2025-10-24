// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC configuration sequence
// -----------------------------------------------------------------------------

class fc_config_seq extends fc_base_seq;

    const string CLASSNAME = "fc_config_seq";
   
    // -----------------------------------------------------------------------
    function new(string name = "fc_config_seq");
        super.new(name);
    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils(fc_seq_pkg::fc_config_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    // -----------------------------------------------------------------------
    virtual task body();


    endtask

endclass

