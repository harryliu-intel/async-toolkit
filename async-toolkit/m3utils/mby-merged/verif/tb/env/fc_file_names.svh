// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Contains list of file names used by the testbench. Will 
//                 automatically uniquify (based on TbEnv name) with multiple
//                 instantiations. This is to keep it clean and ensure all names
//                 are managed in one place
// -----------------------------------------------------------------------------


typedef class fc_tb_env;

class fc_file_names extends uvm_object;

    protected fc_tb_env  _tb_env;              // handle to the parent tb_env
    protected string   _file_names[string];  // hash table for the file names
    
    // -------------------------------------------------------------------------
    function new(string name = "fc_file_names");
        super.new(name);
    endfunction

    // -------------------------------------------------------------------------
    // initialize the hash table to set of known strings
    // -------------------------------------------------------------------------
    function void init_table(fc_tb_env tb_env);
        this._tb_env = tb_env;
        //_cfg_obj = _tb_env.get_cfg_obj_handle();
        
        // file name defaults (automatically uniqified by cluster name
        _file_names[FC::UVM_FILE] = "fc_uvm_hier.out";
        _file_names[FC::TB_FILE]  = "fc_tb.out";
        _file_names[FC::BFM_FILE] = "fc_bfm.out";

        // file name overrides
        tb_env.get_config_string(FC::UVM_FILE, _file_names[FC::UVM_FILE]);
        tb_env.get_config_string(FC::TB_FILE,  _file_names[FC::TB_FILE]);
        tb_env.get_config_string(FC::BFM_FILE, _file_names[FC::BFM_FILE]);
        
    endfunction

    // -------------------------------------------------------------------------
    // return the string registered in the table. 
    // -------------------------------------------------------------------------
    function string get_file_name(string key);
        if ( ! _file_names.exists( key ) ) begin
            `uvm_error("fc_file_names", {key, " not found in _file_names hash table"});
            return "";
        end      
           
        return _file_names[key];
    endfunction
    
    // -------------------------------------------------------------------------
    `uvm_object_utils(fc_file_names)
    
endclass
