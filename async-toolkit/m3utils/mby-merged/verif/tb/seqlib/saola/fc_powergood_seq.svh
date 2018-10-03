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

    //pmu_mmr_file  pmu_mmr;

    
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

        slu_status_t status;
        sla_status_t sla_status;
        slu_ral_data_t ral_data, rd_val;

        super.body();
        `uvm_info(get_name(), "fc_powergood_seq started", UVM_MEDIUM);

        //pmu_mmr.SBI_IP_MSG_EN_0.write(.status(sla_status), .val(32'h00000C40),.access_path("axi_slu"), .parent_seq(this));

        `uvm_info(get_name(), "fc_powergood_seq ends", UVM_MEDIUM);
    endtask

    // --------------------------------------------------------------------------------
endclass : fc_powergood_seq 

