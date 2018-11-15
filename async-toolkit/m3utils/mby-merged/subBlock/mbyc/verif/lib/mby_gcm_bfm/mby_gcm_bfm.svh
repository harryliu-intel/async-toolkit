//-----------------------------------------------------------------------------
// Title         : Madison Bay GCM Bus Functional Model
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gcm_bfm.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the main gcm_bfm class
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
`ifndef __MBY_GCM_BFM_PKG__
`error "Attempt to include file outside of mby_gcm_bfm_pkg."
`endif
`ifndef __MBY_GCM_BFM__
`define __MBY_GCM_BFM__
//-----------------------------------------------------------------------------
// CLASS: mby_gcm_bfm
//
// This is the main gcm_bfm class, it is just a container that instantiates and
// connects the different agents.
//
//-----------------------------------------------------------------------------
class mby_gcm_bfm extends uvm_component;

   // VARIABLE: cfg_obj
   // The bfm's configuration object
   mby_gcm_bfm_cfg cfg_obj;

   // VARIABLE: queue_agent
   // This is the queue agent that is connected to the tag ring and monitors
   // the traffic going from IGR to the GCM
   gcm_queue_bfm_agent queue_agent;

   // VARIABLE: rx_wm_agent
   // This is the RX watermark agent that sends watermark information to the
   // IGR logic.
   gcm_rx_wm_bfm_agent rx_wm_agent;

   // VARIABLE: rx_sm_wm_agent
   // This is the RX shared memory watermark agent that sends watermark
   // information to the IGR logic.
   gcm_rx_smem_wm_bfm_agent rx_sm_wm_agent;

   // VARIABLE: deque_agent
   // This is the dequeue agent that connects to the EGR's dequeue interface
   gcm_deque_bfm_agent deque_agent;

   // VARIABLE: tx_sm_wm_agent
   // This is the TX shared memory watermark agent that sends watermark
   // information to the EGR logic.
   gcm_rx_smem_wm_bfm_agent tx_sm_wm_agent;


   // -------------------------------------------------------------------------
   // Macro to register new class type
   // -------------------------------------------------------------------------
   `uvm_component_param_utils_begin(mby_gcm_bfm)
   `uvm_component_utils_end

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the bfm.
   //    uvm_component parent - The bfm's parent component pointer.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // The free, dirty and mesh agents are created and the configuration object
   // is assigned to them.
   //
   //
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if(cfg_obj.bfm_mode == GCM_BFM_IGR_MODE) begin
         queue_agent    = gcm_queue_bfm_agent::type_id::create("queue_agent", this);
         rx_wm_agent    = gcm_rx_wm_bfm_agent::type_id::create("rx_wm_agent", this);
         rx_sm_wm_agent = gcm_rx_smem_wm_bfm_agent::type_id::create("rx_sm_wm_agent", this);
      end else if(cfg_obj.bfm_mode == GCM_BFM_EGR_MODE) begin
         deque_agent    = gcm_deque_bfm_agent::type_id::create("deque_agent", this);
         tx_sm_wm_agent = gcm_tx_smem_wm_bfm_agent::type_id::create("tx_sm_wm_agent", this);
      end
   endfunction

   // ------------------------------------------------------------------------
   // FUNCTION: connect_phase
   //
   // TODO: Connect the pptr_gen to the corresponding agent based on the bfm
   //       mode.
   //
   // ------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if(cfg_obj.bfm_mode == GCM_BFM_IGR_MODE) begin
         // TODO: connect agents to central component (tbd)
      end else if(cfg_obj.bfm_mode == GCM_BFM_EGR_MODE) begin
         // TODO: connect agents to central component (tbd)
      end
   endfunction

endclass : mby_gcm_bfm
`endif
