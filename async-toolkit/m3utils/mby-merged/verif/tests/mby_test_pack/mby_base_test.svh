// File: mby_base_test.svh

// Class: mby_base_test

`ifndef MBY_BASE_TEST__SVH
`define MBY_BASE_TEST__SVH

class mby_base_test extends uvm_test;
    `uvm_component_utils(mby_base_test)

    mby_env env;

    function new (string name="mby_base_test", uvm_component parent=null);
        super.new (name, parent);
        $timeformat(-9, 0, "ns", 10);
    endfunction : new

    extern function void build_phase(uvm_phase phase);
    extern function void connect_phase(uvm_phase phase);
    extern virtual function void report_phase(uvm_phase phase);
endclass : mby_base_test

function void mby_base_test::report_phase(uvm_phase phase);
    uvm_report_server foo;
    int err_cnt;
    super.report_phase(phase);
    foo = uvm_top.get_report_server();

    // sum num of errors from sequences (use global report server)
    err_cnt = foo.get_severity_count(UVM_ERROR) + foo.get_severity_count(UVM_FATAL);

    // sum num of errors from this test.
    foo = get_report_server();
    err_cnt += (foo.get_severity_count(UVM_ERROR) + foo.get_severity_count(UVM_FATAL));


    if (err_cnt == 0) begin
        uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
        uvm_report_info(get_full_name(),  "         _______  _______  _______  _______         ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |       ||   _   ||       ||       |        ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |    _  ||  |_|  ||  _____||  _____|        ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |   |_| ||       || |_____ | |_____         ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |    ___||       ||_____  ||_____  |        ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |   |    |   _   | _____| | _____| |        ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |___|    |__| |__||_______||_______|        ", UVM_LOG);
        uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
    end
    else begin
        uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
        uvm_report_info(get_full_name(),  "         _______  _______  ___   ___                ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |       ||   _   ||   | |   |               ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |    ___||  |_|  ||   | |   |               ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |   |___ |       ||   | |   |               ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |    ___||       ||   | |   |___            ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |   |    |   _   ||   | |       |           ", UVM_LOG);
        uvm_report_info(get_full_name(),  "        |___|    |__| |__||___| |_______|           ", UVM_LOG);
        uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);
        uvm_report_info(get_full_name(),  "                                                    ", UVM_LOG);

    end 
endfunction

function void mby_base_test::build_phase(uvm_phase phase);
   uvm_report_info(get_full_name(),"Build", UVM_LOG);
   env = mby_env::type_id::create("env",this);  
endfunction : build_phase

function void mby_base_test::connect_phase(uvm_phase phase);
   super.connect_phase(phase);
   // START IOSF_NOT_PRESENT
   //env.set_test_phase_type("env", "HARD_RESET_PHASE", "mby_hard_reset_seq");
   //env.set_test_phase_type("env", "CONFIG_PHASE", "mby_pci_config_seq");
   // END IOSF_NOT_PRESENT
endfunction : connect_phase

`endif
