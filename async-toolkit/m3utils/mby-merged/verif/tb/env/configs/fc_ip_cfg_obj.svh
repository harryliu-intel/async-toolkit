// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// description  :  generic config object for disabliing/enabling IP's
// -----------------------------------------------------------------------------

class fc_ip_cfg_obj extends uvm_object;
    `uvm_object_utils_begin(fc_ip_cfg_obj)
       `uvm_field_string (ip_name, UVM_ALL_ON)
       `uvm_field_int (en, UVM_ALL_ON)
       `uvm_field_int (strap_dis, UVM_ALL_ON)
       `uvm_field_int (fuse_dis, UVM_ALL_ON)
       `uvm_field_int (cfg_en, UVM_ALL_ON)
       `uvm_field_int (d3_en, UVM_ALL_ON)
       `uvm_field_int (clkgate_en, UVM_ALL_ON)
    `uvm_object_utils_end

    string  ip_name;
    rand bit en;             //enable/disable for the IP through fuse or soft strap
    rand bit strap_dis;      //enable/disable using strap 
    rand bit fuse_dis;       //enable/disable using fuse
    rand bit cfg_en;         //control the config sequence enable/disable
    rand bit d3_en;     //control the powergating (D3 state) enable/disable
    rand bit clkgate_en;     //control the clock gating enable/disable

    constraint default_c{
        en dist {1 :=50, 0 := 50}; 
    }

    //ensure fuse and strap are not both 0.
    constraint strap_or_fuse_c{
        solve en before fuse_dis; 
        solve en before strap_dis; 
        solve fuse_dis before strap_dis; 

        en == 1 -> {
             fuse_dis   == 0;
             strap_dis  == 0;
        }
        en == 0 -> {
            (!fuse_dis)  -> strap_dis !=0;
            cfg_en == 0;
            d3_en == 0;
            clkgate_en == 0;
        }
    }

    function new(string name = "");
        super.new(name);
        //ip_name = name;
    endfunction

    function void post_randomize();
        string str;

        super.post_randomize();

        str = this.sprint();
        uvm_report_info("fc_cfg_obj", str, UVM_MEDIUM);
    endfunction

endclass

