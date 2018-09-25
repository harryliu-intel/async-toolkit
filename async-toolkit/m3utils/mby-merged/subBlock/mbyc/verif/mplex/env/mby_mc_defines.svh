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

//   Defines : mby_mc_defines
//
//  This file contain any PARAMETERS or Defines.  Also contains Topology
//  configuration ENUM.

`ifndef __MBY_MC_DEFINES_GUARD
`define __MBY_MC_DEFINES_GUARD

`ifndef __INSIDE_MBY_MC_ENV_PKG
`error "Attempt to include file outside of mby_mc_env_pkg."
`endif


class mby_mc_defines extends uvm_object;

    // Enumeration: mc_topology_e
    // Definition of different mplex TB topologies.
    //    -UNK_TOPO          -Used to detect integration error
    //    -MPLEX             -MC TB with Real Processor RTL and PHY
    //    -MPLEX_NP          -MC TB with PHY and Processor BFM
    //    -MPLEX_NP_NPHY     -MC TB with Processor BFM and no PHY (PIPE mode)
    typedef enum int {
        UNK_TOPO           = 0,
        MPLEX              = 1,
        MPLEX_NP           = 2,
        MPLEX_NP_NPHY      = 3
    } mc_topology_e ;


    `uvm_object_utils(mby_mc_env_pkg::mby_mc_defines)

    //---------------------------------------------------------------------------
    //  Constructor: new
    //  Collect any plusargs and re-configure variables from default, if used.
    //  Arguments:
    //  name   - MC Defines object name.
    //---------------------------------------------------------------------------
    function       new(string name = "mby_mc_defines");
        super.new(name);
    //  $value$plusargs("DISABLE_END2END_FRAME_SB=%d",  disable_end2end_frame_sb);

    endfunction: new

endclass: mby_mc_defines

`endif // __MBY_MC_DEFINES_GUARD
