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


class mby_wm_reg_rw_seq extends mby_env_base_seq;

    `uvm_object_utils(mby_wm_reg_rw_seq)

    function new (string name="mby_wm_reg_rw_seq");
        super.new (name);
    endfunction :  new

    task body();

        sla_ral_reg    temp_csr;
        sla_ral_data_t data,temp_data;
        sla_status_t   rs;

        byte pkt_send_data[];
        int err_info;
        wm_pkt_t push_pkt,get_pkt;

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


        push_pkt.port = 1; //$urandom_range(16, 1);
        push_pkt.len = 64; //$urandom_range(64, 1);
        pkt_send_data = new[push_pkt.len];

        pkt_send_data =
        {
            8'h00, 8'h01, 8'h02, 8'h03, 8'h04, 8'h05, 8'h06, 8'h07, 8'h08, 8'h09, 8'h0a, 8'h0b,
            8'h0c, 8'h0d, 8'h0e, 8'h0f, 8'h10, 8'h11, 8'h12, 8'h13, 8'h14, 8'h15, 8'h16, 8'h17,
            8'h18, 8'h19, 8'h1a, 8'h1b, 8'h1c, 8'h1d, 8'h1e, 8'h1f, 8'h20, 8'h21, 8'h22, 8'h23,
            8'h24, 8'h25, 8'h26, 8'h27, 8'h28, 8'h29, 8'h2a, 8'h2b, 8'h2c, 8'h2d, 8'h2e, 8'h2f,
            8'h30, 8'h31, 8'h32, 8'h33, 8'h34, 8'h35, 8'h36, 8'h37, 8'h38, 8'h39, 8'h3a, 8'h3b,
            8'hee, 8'h7f, 8'hec, 8'hb0
        };

        foreach(pkt_send_data[idx])
            push_pkt.data[idx] = pkt_send_data[idx];

       
        `uvm_info(get_name(), "Sending pkt to WM", UVM_MEDIUM)
        wm_pkt_push(push_pkt);
        `uvm_info(get_name(),  $sformatf("PKT send: Port:%0d, length:%0d ",push_pkt.port ,push_pkt.len),UVM_HIGH)
        
        `uvm_info(get_name(), "Receiving pkt from WM", UVM_MEDIUM)
        err_info = wm_pkt_get(get_pkt);
        `uvm_info(get_name(),  $sformatf("PKT Get: err_info: %0d Port:%0d, length:%0d ",err_info,get_pkt.port,get_pkt.len),UVM_HIGH)

       // for(int idx = 0; idx < get_pkt.len ; idx++)
       //     `uvm_info(get_name(),  $sformatf("Get Data[%0d]: %0x",idx,get_pkt.data[idx]),UVM_HIGH)
                 
        `uvm_info(get_name(), "test complete", UVM_MEDIUM)

    endtask

endclass

`endif

