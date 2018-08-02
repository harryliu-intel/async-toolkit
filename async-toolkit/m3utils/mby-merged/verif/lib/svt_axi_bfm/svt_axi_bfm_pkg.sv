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

//   Package:   svt_axi_bfm_pkg

`ifndef __SVT_AXI_BFM_PKG_GUARD
`define __SVT_AXI_BFM_PKG_GUARD

    package svt_axi_bfm_pkg;
`ifdef XVM
        import ovm_pkg::*;
        import xvm_pkg::*;
        `include "ovm_macros.svh"
        `include "sla_macros.svh"
`endif

        import uvm_pkg::*;
        import sla_pkg::*;
        `include "uvm_macros.svh"
        `include "slu_macros.svh"

        import shdv_base_pkg::*;
       // import eth_bfm_pkg::*;
        import svt_uvm_pkg::*;
      //  import svt_ethernet_enum_pkg::*;
        import svt_bfm_shell_uvm_pkg::*;
        import svt_axi_uvm_pkg::*;

        `define __INSIDE_SVT_AXI_BFM_PKG__
        
        `include "svt_axi_bfm_defines.sv"
        `include "cust_svt_axi_master_transaction.sv"
        `include "cust_svt_axi_system_cfg.sv"
        `include "svt_axi_scoreboard.sv"
        `include "svt_axi_virtual_sequencer.sv"
            
        `include "svt_axi_bfm_env.sv"
      
        `include "svt_axi_master_directed_sequence.sv"
        `undef  __INSIDE_SVT_AXI_BFM_PKG__
    endpackage

`endif // __SVT_AXI_BFM_PKG_GUARD

