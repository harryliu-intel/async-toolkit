// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  test configuration object to control specific features:
//                 resets, power mgmt flows, interrupts etc. 
// -----------------------------------------------------------------------------

class fc_test_cfg_obj extends uvm_object;

    fc_rst_cfg_obj    fc_rst_cfg_obj;

    //------------------------------------
    function new(string name = "");
        super.new(name);
        fc_rst_cfg_obj = fc_rst_cfg_obj::type_id::create(FC::FC_RST_CFG_OBJ);
    endfunction

    // -----------------------------------------------------------------------
    function void pre_randomize();
    endfunction

    // -----------------------------------------------------------------------
    function void post_randomize();
        string str;

        super.post_randomize();
    endfunction

    // -----------------------------------------------------------------------
    `uvm_object_utils_begin(fc_test_cfg_obj)
       `uvm_field_object(fc_rst_cfg_obj, UVM_ALL_ON)
    `uvm_object_utils_end
endclass
