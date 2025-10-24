// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC MemAccess Component
// -----------------------------------------------------------------------------

class fc_mem_access extends uvm_component;

    // ------------------------------------------------------------------------
    function new(string n, uvm_component p = null);
        super.new(n, p);
    endfunction

    // ------------------------------------------------------------------------
    virtual function void setup_reporting();
        set_report_severity_action(UVM_INFO,    UVM_LOG);
        set_report_severity_action(UVM_WARNING, UVM_LOG);
        set_report_severity_action(UVM_ERROR,   UVM_DISPLAY | UVM_LOG);
        set_report_severity_action(UVM_FATAL,   UVM_DISPLAY | UVM_LOG | UVM_EXIT);
    endfunction

    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);

        setup_reporting();

    endfunction

    // ------------------------------------------------------------------------
    virtual function void connect_phase(uvm_phase phase);
    endfunction


    // ------------------------------------------------------------------------
    `uvm_component_utils(fc_mem_access)

endclass

