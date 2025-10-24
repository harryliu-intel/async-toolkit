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

class mby_wm_reg_rw_test extends uvm_test;


    `uvm_component_utils_begin(mby_wm_reg_rw_test);
    `uvm_component_utils_end

    function new(string name = "basic_reg_rw_test", uvm_component parent = null);
        super.new(name, parent);

    endfunction

    task run_phase(uvm_phase phase);
        int addr;
        longint wr_val,rd_val;

        `uvm_info(get_name(), "starting Basic reg RW test", UVM_MEDIUM)
        phase.raise_objection(this, "mby_wm_reg_rw_test");

        /* In HLP this is BSM_SCRATCH_0[0] */
        addr = 'h0010000;
        wr_val = $urandom();

        `uvm_info(get_name(),  $sformatf("Writing register: addr=%0h val=%0h", addr, wr_val),UVM_HIGH)
        if(wm_reg_write(addr, wr_val))
            `uvm_error(get_name(), "ERROR writing register:")

        `uvm_info(get_name(),  $sformatf("Reading register: addr=%0h ", addr),UVM_HIGH)
        if(wm_reg_read(addr, rd_val))
            `uvm_error(get_name(), "ERROR Reading register:")

        if(rd_val !== wr_val)
            `uvm_error(get_name(), $sformatf("Read data: %0h does not match the write data : %0h", rd_val,wr_val))
        else
            `uvm_info(get_name(),  $sformatf( "Read data: %0h matches the write data : %0h", rd_val,wr_val),UVM_HIGH)


        phase.drop_objection(this, "mby_wm_reg_rw_test");

        `uvm_info(get_name(), "test complete", UVM_MEDIUM)
    endtask

endclass

