//-----------------------------------------------------------------------------
// Title         : Madison Bay IGR E2E Scoreboard
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_base_scbd.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.12.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main igr_base_scbd class
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
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
`ifndef __MBY_IGR_SCBD_PKG__
`error "Attempt to include file outside of mby_igr_scbd_pkg."
`endif
`ifndef __MBY_IGR_BASE_SCBD__
`define __MBY_IGR_BASE_SCBD__
//------------------------------------------------------------------------------
// CLASS: mby_igr_base_scbd
//
// This is the main igr_base_scbd class. This class implements the common 
// functionality across different igr scoreboards types.
//
//------------------------------------------------------------------------------
class mby_igr_base_scbd extends shdv_base_scoreboard;
    `uvm_component_utils(mby_igr_base_scbd)
    //---------------------------------------------------------------------------
    // CONSTRUCTOR: new
    //
    // Constructor.
    //
    // ARGUMENTS:
    //    string name          - An instance name of the scoreboard
    //    uvm_component parent - The scoreboard's parent component pointer.
    //---------------------------------------------------------------------------
    function new (string name, uvm_component parent);
        super.new(name, parent);
    endfunction

    //---------------------------------------------------------------------------
    // FUNCTION: start_of_simulation_phase
    //
    // Initializes all the scoreboard variables.
    //
    // ARGUMENTS:
    //    uvm_phase phase - UVM phase
    //---------------------------------------------------------------------------
    function void start_of_simulation_phase(uvm_phase phase);
        super.start_of_simulation_phase(phase);
    endfunction

    //---------------------------------------------------------------------------
    // FUNCTION: check_phase
    //
    // Every scoreboard shall extend the UVM check() function to ensure that end
    // of test has been executed completely.
    //
    // ARGUMENTS:
    //    uvm_phase phase - UVM phase
    //---------------------------------------------------------------------------
    function void check_phase(uvm_phase phase);
        `uvm_error(get_name(), "check_phase function has not been extended")
    endfunction
endclass

`endif // __MBY_IGR_BASE_SCBD__
