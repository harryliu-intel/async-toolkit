// File: mby_base_test.svh

// Class: mby_base_test

`ifndef MBY_BASE_TEST__SVH
`define MBY_BASE_TEST__SVH

class mby_base_test extends ovm_test;
    `ovm_component_utils(mby_base_test)

    mby_env env;

    function new (string name="mby_base_test", ovm_component parent=null);
        super.new (name, parent);
        $timeformat(-9, 0, "ns", 10);
    endfunction : new

    extern function void build();
    extern function void connect();
    extern virtual function void report();
endclass : mby_base_test

function void mby_base_test::report();
    ovm_report_server foo;
    int err_cnt;
    super.report();
    foo = ovm_top.get_report_server();

    // sum num of errors from sequences (use global report server)
    err_cnt = foo.get_severity_count(OVM_ERROR) + foo.get_severity_count(OVM_FATAL);

    // sum num of errors from this test.
    foo = get_report_server();
    err_cnt += (foo.get_severity_count(OVM_ERROR) + foo.get_severity_count(OVM_FATAL));


    if (err_cnt == 0) begin
        ovm_report_info(get_full_name(),  "                                                    ", OVM_LOG);
        ovm_report_info(get_full_name(),  "         _______  _______  _______  _______         ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |       ||   _   ||       ||       |        ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |    _  ||  |_|  ||  _____||  _____|        ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |   |_| ||       || |_____ | |_____         ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |    ___||       ||_____  ||_____  |        ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |   |    |   _   | _____| | _____| |        ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |___|    |__| |__||_______||_______|        ", OVM_LOG);
        ovm_report_info(get_full_name(),  "                                                    ", OVM_LOG);
    end
    else begin
        ovm_report_info(get_full_name(),  "                                                    ", OVM_LOG);
        ovm_report_info(get_full_name(),  "         _______  _______  ___   ___                ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |       ||   _   ||   | |   |               ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |    ___||  |_|  ||   | |   |               ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |   |___ |       ||   | |   |               ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |    ___||       ||   | |   |___            ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |   |    |   _   ||   | |       |           ", OVM_LOG);
        ovm_report_info(get_full_name(),  "        |___|    |__| |__||___| |_______|           ", OVM_LOG);
        ovm_report_info(get_full_name(),  "                                                    ", OVM_LOG);
        ovm_report_info(get_full_name(),  "                                                    ", OVM_LOG);

    end 
endfunction

function void mby_base_test::build();
   ovm_report_info(get_full_name(),"Build", OVM_LOG);
   env = mby_env::type_id::create("env",this);  
endfunction : build

function void mby_base_test::connect();
   super.connect();
   // START IOSF_NOT_PRESENT
   //env.set_test_phase_type("env", "HARD_RESET_PHASE", "mby_hard_reset_seq");
   //env.set_test_phase_type("env", "CONFIG_PHASE", "mby_pci_config_seq");
   // END IOSF_NOT_PRESENT
endfunction : connect

`endif
