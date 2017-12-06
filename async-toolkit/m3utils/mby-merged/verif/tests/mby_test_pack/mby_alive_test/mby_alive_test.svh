// File: mby_alive_test.svh

// Class: mby_alive_test

`ifndef MBY_ALIVE_TEST__SVH
`define MBY_ALIVE_TEST__SVH

class mby_alive_test extends mby_base_test;

    `uvm_component_utils(mby_alive_test)

    function new (string name="mby_alive_test", uvm_component parent=null);
        super.new (name, parent);
    endfunction :  new

    function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       // TODO: Create config obj here.
    endfunction : build_phase

    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       // TODO: Set data phase seq here. 
       // mby_env.set_test_phase_type("mby_env", "USER_DATA_PHASE", "mby_alive_seq");
    endfunction : connect_phase

endclass : mby_alive_test

`endif
