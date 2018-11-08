//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag Bus Functional Model
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main tag_bfm class
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
`ifndef __MBY_TAG_BFM__
`define __MBY_TAG_BFM__
//-----------------------------------------------------------------------------
// CLASS: mby_tag_bfm
//
// This is the main tag_bfm class, it is just a container that instantiates and
// connects the frame generator and the tag agent.
//
//-----------------------------------------------------------------------------
class mby_tag_bfm extends uvm_component;

   // VARIABLE: cfg_obj
   // The agent's configuration object
   mby_tag_bfm_cfg cfg_obj;

   // VARIABLE: tag_agent
   // This is the tag_agent instance, it interfaces with the tag ring. Receives
   // tag transactions from the frame generator and monitors the tag ring intf.
   // This tag agent is a parameterized mby_base_agent class.
   // (code starts)
   //   typedef mby_base_agent#(.T_req(mby_tag_bfm_xaction), .T_vif(mby_tag_bfm_vif)) mby_tag_bfm_agent;
   // (end)
   mby_tag_bfm_agent tag_agent;

   // VARIABLE: frame_gen
   // This is the frame generator component, it gets free ptr xactions from
   // the gmm_bfm, stores those address values in a queue. Gets ethernet xactions
   // from its analysis port and/or generates new ones based on its own
   // configuration. It partitions the ethernet xaction into 256B segments and
   // then again in 64B chunks. Associates each 256B segment with a free ptr
   // from its free ptr queue and calculates the addresses of each of the 64B
   // chunks. Creates an addr/data write xaction per chunk and sends it to its
   // smm analysis port. Creates a new tag xaction with the segment addresses
   // and random metadata and starts it in the tag agent
   mby_tag_bfm_fgen frame_gen;

   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_tag_bfm)
   `uvm_component_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the agent.
   //    uvm_component parent - The agent's parent component pointer.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // The tag_agent is created and the configuration object is assigned to it.
   // When operating in the egress mode, the frame generator is created.
   //
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      // ----------------------------------------------------------------------
      // Creating the tag_bfm_agent and assigning configuration object
      // ----------------------------------------------------------------------
      tag_agent = mby_tag_bfm_agent::type_id::create("tag_agent", this);
      tag_agent.cfg_obj = this.cfg_obj;
      `uvm_info(this.get_full_name(),
         "Created the tag_agent instance and assigned the cfg_obj",
         UVM_DEBUG)
      // ----------------------------------------------------------------------
      // Create the frame_gen only when the bfm is in egress mode
      // ----------------------------------------------------------------------
      if(cfg_obj.bfm_mode == TAG_BFM_EGR_MODE) begin
         frame_gen = mby_tag_bfm_fgen::type_id::create("frame_gen", this);
         `uvm_info(this.get_full_name(),
            "Egress mode set, created the frame generator",
            UVM_DEBUG)
      end
   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   //
   // When operating in the egress mode, the frame generator is connected to
   // the tag_agent.
   //
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      // ----------------------------------------------------------------------
      // Connect the frame_gen only when the bfm is in egress mode
      // ----------------------------------------------------------------------
      if(cfg_obj.bfm_mode == TAG_BFM_EGR_MODE) begin
         // TODO: add the connection(s) here
         `uvm_info(this.get_full_name(),
            "Done connecting the frame generator",
            UVM_DEBUG)
      end
    endfunction : connect_phase


endclass : mby_tag_bfm
`endif
