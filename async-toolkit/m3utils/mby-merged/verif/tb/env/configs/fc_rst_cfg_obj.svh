// -----------------------------------------------------------------------------
// INTEL CO// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// description  :  reset configuration object to control various soC reset flows
// -----------------------------------------------------------------------------

class fc_rst_cfg_obj extends uvm_object;

    //------------------------------------
    function new(string name = "");
        super.new(name);
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
    `uvm_object_utils_begin(fc_rst_cfg_obj)
    `uvm_object_utils_end
endclass
