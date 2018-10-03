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

//   Class:    mby_rx_ppe_env_cfg_seq
//
//   This is the rx_ppe ENV Config sequence file.

`ifndef __MBY_RX_PPE_ENV_CFG_SEQ_GUARD
`define __MBY_RX_PPE_ENV_CFG_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif

class mby_rx_ppe_env_cfg_seq extends shdv_base_config_seq;

    // Variable: env
    // Protected rx_ppe Top Level Env
    protected mby_rx_ppe_env_pkg::mby_rx_ppe_env   env;

    // Variable: status
    // RAL Status
    sla_status_t status;

    // Variable: rd_val
    // Value for RAL Read
    sla_ral_data_t rd_val;

    // Variable: wr_val
    // Value for RAL Write
    sla_ral_data_t wr_val;

    // Variable: ral
    // RAL ENV
    sla_ral_env ral;

    // Variable: access_type
    // RAL ENV Access Type
    string access_type = "BACKDOOR";

    `uvm_object_utils(mby_rx_ppe_env_cfg_seq)
    `uvm_declare_p_sequencer(slu_sequencer)

    //------------------------------------------------------------------------------
    //  Constructor: new
    //  New rx_ppe env Config Sequence Object.
    //  Gets handle to the rx_ppe ENV.
    //
    //  Arguments:
    //  string name  - rx_ppe env config sequence object name.
    //------------------------------------------------------------------------------
    function new(input string name = "mby_rx_ppe_env_cfg_seq");
        super.new(name);
        set_env(slu_tb_env::get_top_tb_env());
    endfunction: new

    //------------------------------------------------------------------------------
    //  Function: set_env
    //  Handle to rx_ppe Top Level env for use in sequences
    //
    //  Arguments:
    //  slu_tb_env tb_env  -  Handle to the ENV
    //------------------------------------------------------------------------------
    virtual function void set_env(slu_tb_env tb_env);
        mby_rx_ppe_env_pkg::mby_rx_ppe_env temp_env;
        bit stat;

        stat = $cast(temp_env,tb_env);
        `slu_assert(    stat, ($psprintf("Cast of $s(type: $s) failed!!!",tb_env.get_name(),tb_env.get_type_name())));
        `slu_assert(temp_env, ("Could not fetch slu_tb_env handle!!!"));

        this.env    = temp_env;
    endfunction : set_env

    //------------------------------------------------------------------------------
    //  Function: sm_config
    //  This finction calls the "allocate_mem" function which allocates memory
    //  address space for exclusive use.
    //------------------------------------------------------------------------------
    virtual function void sm_config();
        sm.ag.allocate_mem(ag_result, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
    endfunction

    //------------------------------------------------------------------------------
    //  Task: body
    //  Configures rx_ppe DUT.
    //------------------------------------------------------------------------------
    virtual task     body();
        `uvm_info(get_name(), "MplexTop Env Configuration Sequence", UVM_MEDIUM);

        $value$plusargs("CONFIG_ACCESS_TYPE=%s", access_type);

    //Configure DUT here..

    endtask : body

endclass : mby_rx_ppe_env_cfg_seq

`endif // __MBY_RX_PPE_ENV_CFG_SEQ_GUARD
