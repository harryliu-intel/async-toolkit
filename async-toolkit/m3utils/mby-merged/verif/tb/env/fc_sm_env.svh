// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC testbench SM environment
// -----------------------------------------------------------------------------


class fc_sm_env extends slu_sm_env;
    const string CLASSNAME = "fc_sm_env";

    `uvm_component_utils(fc_sm_env)

    fc_tb_env   env;

    slu_sm_ag_result r;
    slu_sm_ag_status_t status;

    // -----------------------------------------------------------------------
    function new(string name, uvm_component parent);
        bit rc;

        super.new(name, parent);

        rc = $cast(env, slu_tb_env::get_top_tb_env());
        assert (rc) else `uvm_fatal(get_name(), "unable to get TOP TB ENV")

    endfunction

    // -----------------------------------------------------------------------
    function void build_phase(uvm_phase phase);
        `uvm_info(get_type_name(), "start fc_sm_env build phase", UVM_MEDIUM)

        super.build_phase(phase);

        `uvm_info(get_type_name(), "end   fc_sm_env build phase", UVM_MEDIUM)
    endfunction


    // -----------------------------------------------------------------------
    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
    endfunction

    // -----------------------------------------------------------------------
    // the SM is configured in end_of_elaboration phase as it might need to
    // read registers that are randomized in the connect phase
    // -----------------------------------------------------------------------
    virtual function void end_of_elaboration_phase(uvm_phase phase);

        `uvm_info(get_type_name(), "start fc_sm_env end_of_elaboration phase", UVM_MEDIUM)

        super.end_of_elaboration_phase(phase);

        `uvm_info(get_name(), "printing tag maps...", UVM_LOW);
        print_tagmap();

        `uvm_info(get_type_name(), "end   fc_sm_env end_of_elaboration phase", UVM_MEDIUM)
    endfunction //end_of_elab

    // -----------------------------------------------------------------------
    // the FC implementation of saola SM do_write 
    // -----------------------------------------------------------------------
    virtual task do_write(input sla_pkg::addr_t addr, 
                          input byte_t data[$],
                          input bit be[$], 
                          input string region, 
                          bit backdoor,
                          input string map_name, 
                          input string allocated_name = "", 
                          input uvm_object user_object=null);

        `uvm_info(get_name, $sformatf("SM: do_write addr=0x%x, length=0x%x, region=%s, backdoor=%b map_name=%s", addr, data.size(), region, backdoor, map_name), UVM_MEDIUM);
    endtask

    // -----------------------------------------------------------------------
    // the FC implementation of saola SM do_read
    // -----------------------------------------------------------------------
    virtual task do_read(input sla_pkg::addr_t addr, 
                          input sla_pkg::addr_t length,
                          output byte_t data[$],
                          input string region, 
                          bit backdoor,
                          input string map_name, 
                          input string allocated_name = "", 
                          input uvm_object user_object=null);

        `uvm_info(get_name, $sformatf("SM: do_read addr=0x%x, length=0x%x, region=%s, backdoor=%b map_name=%s", addr, length, region, backdoor, map_name), UVM_MEDIUM);
    endtask


    function void start_of_simulation_phase(uvm_phase phase);
		super.start_of_simulation_phase(phase);
	endfunction : start_of_simulation_phase
endclass

