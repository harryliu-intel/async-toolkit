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
//=======================================================================
// COPYRIGHT (C) 2012 SYNOPSYS INC.
// This software and the associated documentation are confidential and
// proprietary to Synopsys, Inc. Your use or disclosure of this software
// is subject to the terms and conditions of a written license agreement
// between you, or your company, and Synopsys, Inc. In the event of
// publications, the following notice is applicable:
//
// ALL RIGHTS RESERVED
//
// The entire notice above must be reproduced on all authorized copies.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Class:    svt_ethernet_virtual_sequencer
//
//   This class is Virtual Sequencer class, which encapsulates the
//   agent's sequencers and allows a fine grain control over the user's
//   stimulus application to the selective sequencer.


`ifndef __AXI_VIRTUAL_SEQUENCER_GUARD
`define __AXI_VIRTUAL_SEQUENCER_GUARD


`ifndef __INSIDE_SVT_AXI_BFM_PKG__
`error "File is meant to be used only through the svt_axi_bfm_pkg.  Do not include it individually."
`endif

class axi_virtual_sequencer extends uvm_sequencer;

    //Typedef of the reset modport to simplify access
    typedef virtual axi_reset_if.axi_reset_modport AXI_RESET_MP;

    // Reset modport provides access to the reset signal
    AXI_RESET_MP reset_mp;

    `uvm_component_utils(axi_virtual_sequencer)

    // Variable: sequencer
    // Instance of txrx sequencer
    svt_axi_system_sequencer sequencer;

    function new(string name="axi_virtual_sequencer", uvm_component parent=null);
        super.new(name,parent);
    endfunction // new

    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);

        if (!uvm_config_db#(AXI_RESET_MP)::get(this, "", "reset_mp", reset_mp)) begin
            `uvm_fatal("build_phase", "An axi_reset_modport must be set using the config db.");
        end

    endfunction

endclass

`endif // __AXI_VIRTUAL_SEQUENCER_GUARD



