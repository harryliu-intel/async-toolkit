// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC testbench IM Environment
// -----------------------------------------------------------------------------

typedef class fc_tb_env;
typedef class fc_cfg_obj;

class fc_im_env extends slu_im_env;
    `uvm_component_utils(fc_im_env)
 
    sla_pkg::slu_im_env       im;
    sla_pkg::slu_sm_env       sm;
    fc_tb_env                   tb_env;
    fc_cfg_obj                  cfg_obj;
    virtual sig_if            pins;
    sla_pkg::slu_im_intr_info intr_info;

    event msi_seen;

    // -----------------------------------------------------------------------

    function new(string name, uvm_component parent);
        bit rc;

        super.new(name, parent);
        get_tb_env();   // gets a handle to the fc_tb_env

    endfunction

    // -----------------------------------------------------------------------
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);

    endfunction

    // -----------------------------------------------------------------------
    function void connect_phase(uvm_phase phase);
        string int_name,seq_name,tmp_str;
        int i;
        super.connect_phase(phase);

        im = sla_pkg::slu_im_env::get_ptr();
        assert(im) else `uvm_error("connect", "could not find slu_im_env pointer");

    endfunction

    // -----------------------------------------------------------------------
    virtual function void end_of_elaboration_phase(uvm_phase phase);
        super.end_of_elaboration_phase(phase);
    endfunction

    // -----------------------------------------------------------------------
    // gets a handle to fc_tb_env
    // -----------------------------------------------------------------------
    function void get_tb_env();
        `slu_assert($cast(tb_env, slu_utils::get_comp_by_name("tb_env")),
                    ($sformatf("fc_tb_env $cast failed to %s", "tb_env")));
        `slu_assert(tb_env, ($sformatf("could not fetch %s handle", "tb_env")));

        cfg_obj     = tb_env.get_cfg_obj_handle();
        //pins        = tb_env.get_pins();
    endfunction

    // -----------------------------------------------------------------------
    task run_phase (uvm_phase phase);

    endtask

endclass
