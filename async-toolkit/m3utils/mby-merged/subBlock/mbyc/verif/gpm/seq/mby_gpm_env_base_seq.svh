// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0


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

`ifndef __MBY_GPM_ENV_BASE_SEQ_GUARD
`define __MBY_GPM_ENV_BASE_SEQ_GUARD

`ifndef __INSIDE_MBY_GPM_SEQ_LIB
`error "Attempt to include file outside of mby_gpm_seq_lib."
`endif

//   Class:  mby_gpm_env_base_seq
//
//   This is the GPM base seq extended from MBY Base seq which is in-turn extended 
//   from shdv_base_seq. This sequence class has methods to setup gpm env object handle 
//   and methods to perform register access both for RTL.
//
//  All the sequences in gpm env will extend from this base sequence to inherit its
//  functionality.  

class mby_gpm_env_base_seq extends shdv_base_sequence; 

    // Variable: env
    // mby_gpm_env handle

    mby_gpm_env_pkg::mby_gpm_env         env;

    // Variable: tb_cfg
    // GPM tb cfg.
    mby_gpm_env_pkg::mby_gpm_tb_top_cfg   tb_cfg;

    // Variable: vif
    // Handle to GPM Tb interface.
    virtual mby_gpm_tb_if                 vif;
   
    // Variable: ral
    // Handle to GPM RAL.
    mby_gpm_reg_pkg::mby_gpm_reg_blk     ral;

    // ------------------------------------------------------------------------
    //  Constructor: new
    //  Arguments:
    //  string name   - GPM env base sequence object name.
    // ------------------------------------------------------------------------
    function new(string name = "mby_gpm_env_base_seq");
       super.new();
       set_env (shdv_base_env::get_top_tb_env());
    endfunction : new

    // ------------------------------------------------------------------------
    //  Function: set_env
    //  Arguments: shdv_base_env 
    // ------------------------------------------------------------------------
    function void set_env(shdv_base_env tb_env);
       mby_gpm_env_pkg::mby_gpm_env temp_env;

       $cast(temp_env,tb_env);

       this.env    = temp_env;
       this.vif    = temp_env.get_tb_vif();
       this.ral    = temp_env.get_tb_ral();
       this.tb_cfg = temp_env.get_tb_cfg();
  
    endfunction : set_env
 
endclass : mby_gpm_env_base_seq

`endif // __MBY_GPM_ENV_BASE_SEQ_GUARD
