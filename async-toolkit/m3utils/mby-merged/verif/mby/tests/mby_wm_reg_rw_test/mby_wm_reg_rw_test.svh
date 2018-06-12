//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// Author      : Akshay Kotian
// Project     : MBY
//Description  : This test performs basic read and write to MBY WM registers.
//------------------------------------------------------------------------------


`ifndef MBY_WM_REG_RW__SVH
`define MBY_WM_REG_RW__SVH

class mby_wm_reg_rw_test extends mby_base_test;

    `uvm_component_utils(mby_wm_reg_rw_test)

    function new (string name="mby_wm_reg_rw_test", uvm_component parent=null);
        super.new (name, parent);
    endfunction :  new

    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
    endfunction : build_phase

    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        env.set_test_phase_type("env", "USER_DATA_PHASE", "mby_wm_reg_rw_seq");
    endfunction : connect_phase


endclass : mby_wm_reg_rw_test


class mby_wm_reg_rw_seq extends mby_base_seq;

    `uvm_object_utils(mby_wm_reg_rw_seq)

    function new (string name="mby_wm_reg_rw_seq");
        super.new (name);
    endfunction :  new

    task body();

        sla_ral_reg    temp_csr;
        sla_ral_data_t data,temp_data;
        sla_status_t   rs;

        `uvm_info(get_name(), "starting Basic reg RW test", UVM_MEDIUM)


        temp_csr  = mby.find_reg("DEVVENDID");     

        csr_read(temp_csr, data, "white_model");
        `uvm_info(get_name(),  $sformatf("CSR read: %0s: val=%0h", temp_csr.get_name(), data),UVM_FULL)

        //
        //"Primary" access type is not yet defined, this code can be
        //uncommented once the RTL is ready to accept Frontdoor CSR access.
        //
        
//        mby.DEVVENDID.DEVICEID.set(2'h1);
//        mby.DEVVENDID.VENDORID.set(2'h1);
//        temp_data = mby.DEVVENDID.get();
//
//        csr_write(temp_csr, temp_data, "primary");
//        `uvm_info(get_name(),  $sformatf("CSR Write: %0s: val=%0h", temp_csr.get_name(),temp_csr.get()),UVM_FULL)
//
//        csr_read(temp_csr, data, "white_model");
//        `uvm_info(get_name(),  $sformatf("CSR read: %0s: val=%0h", temp_csr.get_name(), data),UVM_FULL)
//
//
//        if (temp_data !== data)
//            `uvm_error(get_name(), $sformatf("read data mismatch: Expected: %0h , actual:%0h",temp_data, data))


        `uvm_info(get_name(), "test complete", UVM_MEDIUM)
        
    endtask

endclass

`endif

