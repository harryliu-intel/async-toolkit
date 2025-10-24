// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  : 
// -----------------------------------------------------------------------------

typedef enum {WR_RD,WR,RD} RW_TRANS;
typedef class fc_base_seq;

virtual class fc_base_seq extends uvm_sequence;

    protected fc_tb_env           tb_env;
    protected fc_cfg_obj          cfg_obj;

    protected virtual sig_if      fc_sig_if;
    //protected virtual fc_dut_if     dut_if;
    protected string              access_path;
    //    fc_sm_env sm;
    //    protected fc_im_env tb_im;

    bit rc;
    uvm_pkg::uvm_object tmp_obj;

    string inst,port;


    // -----------------------------------------------------------------------
    function new(string name="fc_base_seq");
        super.new(name);
        get_tb_env();   // gets a handle to the fc_tb_env

        //`slu_assert($cast(sm,slu_sm_env::get_ptr()), ("unable to get handle to SM"));
   
    endfunction

    // -----------------------------------------------------------------------
    // gets a handle to fc_tb_env
    // -----------------------------------------------------------------------
    function void get_tb_env();
        `slu_assert($cast(tb_env, slu_utils::get_comp_by_name("tb_env")),
                    ($sformatf("fc_tb_env $cast failed to %s", "tb_env")));
        `slu_assert(tb_env, ($sformatf("could not fetch %s handle", "tb_env")));

        //`slu_assert($cast(tb_im,slu_im_env::get_ptr()) && tb_im != null, ("unable to get handle to saola IM"));
        
        fc_sig_if   = tb_env.get_fc_sig_if();
        //dut_if      = tb_env.get_dut_if();
        cfg_obj     = tb_env.get_cfg_obj_handle();
        //tb_im       = tb_env.fc_intr_env;
        //tb_im       = tb_env.get_fc_im_env_handle();
    endfunction

    virtual task body();
        `uvm_info(get_name(), "inside sequence body of fc_base_seq", UVM_MEDIUM);

    endtask: body

endclass: fc_base_seq

