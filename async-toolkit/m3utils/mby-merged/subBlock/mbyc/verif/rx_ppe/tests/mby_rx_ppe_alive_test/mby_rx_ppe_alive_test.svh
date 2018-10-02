// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

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
// and confidential information of Intel or its suppliers and licensors.  The
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
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Class:    mby_rx_ppe_alive_test
//

`ifndef MBY_RX_PPE_ALIVE_TEST__SV
`define MBY_RX_PPE_ALIVE_TEST__SV

`ifndef __INSIDE_MBY_RX_PPE_TEST_LIB
`error "Attempt to include file outside of mby_rx_ppe_test_lib."
`endif

class mby_rx_ppe_alive_test extends mby_rx_ppe_base_test;

    `uvm_component_utils(mby_rx_ppe_alive_test)
    //------------------------------------------------------------------------------
    // Constructor: new
    //  Arguments:
    //  name   - rx_ppe alive test object name.
    //  parent - Component parent object.
    //------------------------------------------------------------------------------
    function new (string name="mby_rx_ppe_alive_test", uvm_component parent=null);
        super.new (name, parent);
    endfunction :  new

    //------------------------------------------------------------------------------
    // Function: build_phase
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
    endfunction : build_phase

    //------------------------------------------------------------------------------
    // Function: connect_phase
    // Sets USER_DATA_PHASE sequence.
    //
    //  Arguments:
    //  phase - uvm_phase object.
    //------------------------------------------------------------------------------
    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase);
        env.set_test_phase_type("env", "USER_DATA_PHASE", "mby_rx_ppe_alive_seq");
    endfunction : connect_phase

endclass : mby_rx_ppe_alive_test

class mby_rx_ppe_alive_seq extends mby_rx_ppe_seq_lib::mby_rx_ppe_env_base_seq;

    `uvm_object_utils(mby_rx_ppe_alive_seq)

    eth_frame      los_frames[4];
    eth_sequencer  los_sequencers[4];
    //------------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  name   - rx_ppe alive test  seq object name.
    //------------------------------------------------------------------------------
    function new (string name="mby_rx_ppe_alive_seq");
        super.new (name);
        set_env(slu_tb_env::get_top_tb_env());
    endfunction :  new

    //------------------------------------------------------------------------------
    //  Task:  body
    //  Sends basic Ethernet packets. 
    //------------------------------------------------------------------------------
    virtual task body();

        int count[4] = {0,0,0,0};

        `slu_info(this.get_name(), ("Starting eth simple sequence..."))

        foreach(los_sequencers[i]) begin
            `slu_assert($cast(los_sequencers[i],
                    env.get_slu_sqcr().pick_sequencer($sformatf("tx%0d", i))),
                ("Could not get a pointer to the sequencer%0d", i))
        end
        foreach(los_frames[i]) begin
            los_frames[i] = eth_frame::type_id::create($sformatf("los_frames_%0d", i));
            los_frames[i].set_item_context(this, los_sequencers[i]);
        end
        foreach(los_frames[i]) begin
            automatic int auto_i = i;
            fork
                begin
                    repeat (20) begin
                        `slu_assert(los_frames[auto_i].randomize() with {
                                bubble         inside {[7:13]};
                                kind           inside {BASIC_FRAME,
                                    IPV4_FRAME,
                                    IPV6_FRAME};
                                payload.size() inside {[64:512]};
                                dmac            == 'h000102030405 + count[auto_i];
                                smac            == 'h060708090a0b + count[auto_i];
                                tc              == count[auto_i][3:0];
                                (kind == BASIC_FRAME) ->
                                foreach (payload[idx])
                                payload[idx] == idx;
                            }, ("Unable to randomize eth_pkt"))
                        count[auto_i]++;
                        `slu_info(this.get_name(), ("Started eth_frame %0d %0d", auto_i, count[auto_i]))
                        `uvm_send(los_frames[auto_i])

                        `slu_info(this.get_name(), ("Sent eth_frame %0d %0d", auto_i, count[auto_i]))
                    end
                end
            join_none
        end
        wait fork;
    endtask

endclass : mby_rx_ppe_alive_seq

`endif // MBY_RX_PPE_ALIVE_TEST__SV
