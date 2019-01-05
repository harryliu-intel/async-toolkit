// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC Powergood sequence
// -----------------------------------------------------------------------------

class fc_powergood_seq extends fc_base_seq;
    const string CLASSNAME = "fc_powergood_seq";

    protected uvm_event       soc_group_e;
    protected uvm_event_pool  event_pool;

    // -----------------------------------------------------------------------
    function new(string name = "fc_powergood_seq");
        super.new(name);

        //`slu_assert( ($cast(pmu_mmr, ral.find_file("pmu_mmr")) && (pmu_mmr != null)), ("unable to get handle to PMU MMR register file"));

    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils(fc_seq_pkg::fc_powergood_seq) 
    `uvm_declare_p_sequencer(slu_sequencer)

    // -----------------------------------------------------------------------
    virtual task body();

        super.body();
        `uvm_info(get_name(), "fc_powergood_seq started", UVM_MEDIUM);

        //TODO: temp driving this in sequence. The powergood will be driven as part of IMC powergood sequence once its ready.
        fc_sig_if.powergood = 0;
        #20ns;
        fc_sig_if.powergood = 1;

        #2us;

        `uvm_info(get_name(), "fc_powergood_seq ends", UVM_MEDIUM);
    endtask

    // --------------------------------------------------------------------------------
endclass : fc_powergood_seq 

