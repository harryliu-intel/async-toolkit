
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

//   Class:  mby_rx_ppe_env_base_seq
//
//   This is the rx_ppe base seq extended from MBY Base seq which is in-turn extended 
//   from shdv_base_seq. This sequence class has methods to setup rx_ppe env object handle 
//   and methods to perform register access both for RTL and WM.
//
//  All the sequences in rx_ppe env will extend from this base sequence to inherit its
//  functionality.  


`ifndef __MBY_RX_PPE_ENV_BASE_SEQ_GUARD
`define __MBY_RX_PPE_ENV_BASE_SEQ_GUARD

`ifndef __INSIDE_MBY_RX_PPE_SEQ_LIB
`error "Attempt to include file outside of mby_rx_ppe_seq_lib."
`endif

class mby_rx_ppe_env_base_seq extends mby_common_pkg::mby_base_seq;

    // Variable: env
    // rx_ppe Top Level Env.
    mby_rx_ppe_env_pkg::mby_rx_ppe_env         env;

    // Variable: cfg
    // rx_ppe environment cfg.
    mby_rx_ppe_env_pkg::mby_rx_ppe_tb_top_cfg  cfg;

    // Variable: ral
    // rx_ppe RAL env.
    mby_rx_ppe_env_pkg::mby_rx_ppe_ral_env     ral;

    // Variable: vif
    // Handle to rx_ppe Tb interface.
    virtual mby_rx_ppe_tb_if                   vif;

    // ------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  string name   - rx_ppe env base sequence object name.
    // ------------------------------------------------------------------------
    function new(string name = "mby_rx_ppe_env_base_seq");
        super.new();
    endfunction : new

    // ------------------------------------------------------------------------
    virtual function void set_env(slu_tb_env tb_env);
        mby_rx_ppe_env_pkg::mby_rx_ppe_env temp_env;
        bit stat;

        stat = $cast(temp_env,tb_env);
        if(!stat) begin
            `ovm_fatal(get_name(), "Cast of sla_tb_env failed");
        end
        if(temp_env == null) begin
            `ovm_fatal(get_name(), "Could not fetch sla_tb_env handle!!!");
        end

        this.env = temp_env;
        this.cfg = temp_env.get_tb_cfg();
        this.ral = temp_env.get_tb_ral();
        this.vif = temp_env.get_tb_vif();

    endfunction : set_env

endclass : mby_rx_ppe_env_base_seq

`endif // __MBY_RX_PPE_ENV_BASE_SEQ_GUARD
