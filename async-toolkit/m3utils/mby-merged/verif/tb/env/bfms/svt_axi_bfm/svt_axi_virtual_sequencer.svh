// ------------------------------------------------------------------------------------------------------------------------
// Copyright(C) 2012 Intel Corporation, Confidential Information
// ------------------------------------------------------------------------------------------------------------------------
//
// Created By:  Raghu Prasad Gudla
// Created On:  08/21/2018
// Class:    svt_axi_virtual_sequencer
//
//   This class is Virtual Sequencer class, which encapsulates the
//   agent's sequencers and allows a fine grain control over the user's
//   stimulus application to the selective sequencer.


class axi_virtual_sequencer extends uvm_sequencer;

    //Typedef of the reset modport to simplify access
    typedef virtual AxiResetIf.axi_reset_modport AXI_RESET_MP;

    // Reset modport provides access to the reset signal
    AXI_RESET_MP reset_mp;

    `uvm_component_utils(axi_virtual_sequencer)

    // Variable: sequencer
    // Instance of txrx sequencer
    svt_axi_system_sequencer sequencer;

    function new(string name="axi_virtual_sequencer", uvm_component parent=null);
        super.new(name,parent);
    endfunction // new

    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);

        if (!uvm_config_db#(AXI_RESET_MP)::get(this, "", "reset_mp", reset_mp)) begin
            `uvm_fatal("build_phase", "An axi_reset_modport must be set using the config db.");
        end

    endfunction

endclass




