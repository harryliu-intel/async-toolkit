/// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  : 
// -----------------------------------------------------------------------------
typedef class fc_tb_env;

class fc_cfg_obj extends uvm_object;

    fc_tb_env           cluster;
    rand fc_chip_cfg_obj fc_chip_confg_obj;
    fc_uvm_reg_map reg_model;

    //------------------------------------
    function new(string name = "");
        super.new(name);

        fc_chip_confg_obj = fc_chip_cfg_obj::type_id::create(FC::FC_CHIP_CFG_OBJ);
    endfunction

    // -----------------------------------------------------------------------
    function void pre_randomize();
    endfunction

    // -----------------------------------------------------------------------
    function void post_randomize();
        string str;
        super.post_randomize();
        str = this.sprint();
        uvm_report_info("fc_cfg_obj", str, UVM_MEDIUM);
    endfunction

    // -----------------------------------------------------------------------
    virtual function void set_cluster (fc_tb_env cluster);
        this.cluster = cluster;
    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils_begin(fc_cfg_obj)
        `uvm_field_object(fc_chip_confg_obj, UVM_ALL_ON)
    `uvm_object_utils_end
endclass
