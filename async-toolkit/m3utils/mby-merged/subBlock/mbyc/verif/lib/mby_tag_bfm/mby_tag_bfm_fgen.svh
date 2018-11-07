//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag BFM Frame Generator
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_fgen.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the frame generator class used by the Tag BFM
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
`ifndef __MBY_TAG_BFM_PKG__
`error "Attempt to include file outside of mby_tag_bfm_pkg."
`endif
`ifndef __MBY_TAG_BFM_FGEN__
`define __MBY_TAG_BFM_FGEN__
//`uvm_analysis_imp_decl(_fprt)
//`uvm_analysis_imp_decl(_frame)
//-----------------------------------------------------------------------------
// CLASS: mby_tag_bfm_fgen
//
// This is the frame generator that is instantiated in the tag bfm.
// It gets free ptr xactions from the gmm_bfm, stores those address values in
// a queue. Gets ethernet xactions from its analysis port and/or generates new
// ones based on its own configuration. It partitions the ethernet xaction into
// 256B segments and then again in 64B chunks. Associates each 256B segment
// with a free ptr
// from its free ptr queue and calculates the addresses of each of the 64B
// chunks. Creates an addr/data write xaction per chunk and sends it to its
// smm analysis port. Creates a new tag xaction with the segment addresses
// and random metadata and starts it in the tag agent
//
//-----------------------------------------------------------------------------
class mby_tag_bfm_fgen extends uvm_component;

   // Registering class with the factory
   `uvm_component_utils(mby_tag_bfm_fgen)

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the frame generator.
   //    uvm_component parent - The generators's parent component.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

endclass : mby_tag_bfm_fgen

`endif

