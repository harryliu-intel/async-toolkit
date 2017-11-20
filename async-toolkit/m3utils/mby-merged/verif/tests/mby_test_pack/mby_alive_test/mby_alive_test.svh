// File: mby_alive_test.svh

// Class: mby_alive_test

`ifndef MBY_ALIVE_TEST__SVH
`define MBY_ALIVE_TEST__SVH

class mby_alive_test extends mby_base_test;

    `ovm_component_utils(mby_alive_test)

    function new (string name="mby_alive_test", ovm_component parent=null);
        super.new (name, parent);
    endfunction :  new

    function void build();
       super.build();
       // TODO: Create config obj here.
    endfunction : build

    function void connect();
       super.connect();
       // TODO: Set data phase seq here. 
       // mby_env.set_test_phase_type("mby_env", "USER_DATA_PHASE", "mby_alive_seq");
    endfunction : connect

endclass : mby_alive_test

`endif
