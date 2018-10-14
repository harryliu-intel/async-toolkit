// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  : 
// -----------------------------------------------------------------------------

`define MPCH_SKT_CNT 8

typedef class fc_tb_env;

class fc_chip_cfg_obj extends uvm_object;

    fc_tb_env    cluster;

    //generic IP config object
    rand logic [7:0]  bus_num;
    rand logic        cfg_type;

    //SB iosf access variables
    //rand FC::sb_access_path_t    sb_access_type;
    //rand FC::p_access_path_t     p_access_type;
    rand logic        sb_read_allign_dis;
    rand logic [7:0]  sb_sai;
    rand logic        sb_root_space;
    rand logic        sb_wait_for_complete;

    constraint sb_wait_for_comp_c{
        soft sb_wait_for_complete == 1;
    }

    //------------------------------------
    function new(string name = "");
        string ip_name;
        super.new(name);

    endfunction

    // -----------------------------------------------------------------------
    function void pre_randomize();
    endfunction

    // -----------------------------------------------------------------------
    function void post_randomize();
        super.post_randomize();

    endfunction

    // -----------------------------------------------------------------------
    virtual function void set_cluster (fc_tb_env cluster);
        this.cluster = cluster;
    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils_begin(fc_chip_cfg_obj)
        //`uvm_field_sarray_object(fc_ip_cfg_obj, UVM_ALL_ON)
    `uvm_object_utils_end
endclass
