// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay EGR E2E Scoreboard
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_e2e_scbd.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.12.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main egr_e2e_scbd class
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
`ifndef __MBY_EGR_SCBD_PKG__
`error "Attempt to include file outside of mby_egr_scbd_pkg."
`endif
`ifndef __MBY_EGR_E2E_SCBD__
`define __MBY_EGR_E2E_SCBD__
// Forward declaration of the predictor and comparator classes.
// Inform compiler that those classes might be used before actual class definition.
typedef class mby_egr_e2e_predictor;
typedef class mby_egr_e2e_comparator;
//------------------------------------------------------------------------------
// CLASS: mby_egr_e2e_scbd
//
// This is the main egr_e2e_scbd class which extends from the egr base scoreboard.
// It should include a predictor and a comparator 
//
//------------------------------------------------------------------------------
class mby_egr_e2e_scbd extends mby_egr_base_scbd;
    `uvm_component_utils(mby_egr_e2e_scbd)

    // VARIABLE: predictor
    // The scoreboard's predictor
    mby_egr_e2e_predictor predictor;
    // VARIABLE: comparator
    // The scoreboard's comparator
    mby_egr_e2e_comparator comparator;
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
   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // The predictor and comparator are created and the configuration object
   // is assigned to them.
   //
   // ARGUMENTS:
   //    uvm_phase phase - UVM phase
   //---------------------------------------------------------------------------
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        predictor = mby_egr_e2e_predictor::type_id::create("predictor", this);
        comparator = mby_egr_e2e_comparator::type_id::create("comparator", this);
    endfunction

endclass
//------------------------------------------------------------------------------
// CLASS: mby_egr_e2e_predictor
//
// This is the main egr_e2e_predictor class which extends from the egr base predictor.
//
//------------------------------------------------------------------------------
class mby_egr_e2e_predictor extends mby_egr_base_predictor;//FIXME: extends mby_egr_base_predictor#(xaction);
    `uvm_component_utils(mby_egr_e2e_predictor)
    //---------------------------------------------------------------------------
    // CONSTRUCTOR: new
    //
    // Constructor.
    //
    // ARGUMENTS:
    //    string name          - An instance name of the predictor
    //    uvm_component parent - The predictor's parent component pointer.
    //---------------------------------------------------------------------------
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction
endclass
//------------------------------------------------------------------------------
// CLASS: mby_egr_e2e_comparator
//
// This is the main egr_e2e_comparator class which extends from the egr base comparator.
//
//------------------------------------------------------------------------------
class mby_egr_e2e_comparator extends mby_egr_base_comparator;
    `uvm_component_utils(mby_egr_e2e_comparator)
    //---------------------------------------------------------------------------
    // CONSTRUCTOR: new
    //
    // Constructor.
    //
    // ARGUMENTS:
    //    string name          - An instance name of the comparator
    //    uvm_component parent - The comparator's parent component pointer.
    //---------------------------------------------------------------------------
    function new (string name, uvm_component parent);
        super.new(name, parent);
    endfunction
endclass

`endif // __MBY_EGR_E2E_SCBD__
