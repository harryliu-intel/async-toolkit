// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR CPTR req
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_cptr_req.svh
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.14.2018
//-----------------------------------------------------------------------------
// Description :
// This is the subscriber to the clean ptr requests
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
`ifndef __MBY_PBR_BFM_PKG__
`error "Attempt to include file outside of mby_pbr_bfm_pkg."
`endif
`ifndef __MBY_PBR_BFM_CPTR_REQ__
`define __MBY_PBR_BFM_CPTR_REQ__
//-----------------------------------------------------------------------------
// CLASS: mby_pbr_bfm_cptr_req
//
// This mrd req class is instantiated in the top pbr_bfm class.
// It consumes cptr requests from the different interfaces
//
// PARAMETERS::
//     type T_req - sequence item type to be handled.
//-----------------------------------------------------------------------------

class mby_pbr_bfm_cptr_req
   #(
      type T_req  = shdv_base_sequence_item//mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

   // VARIABLE: cptr_req_cfg_obj
   //    The bfm's mrd request configuration objects
   mby_pbr_bfm_cfg cptr_req_cfg_obj; //FIXME: raalfaro

   // VARIABLE: cpb_agent_ptr
   //    Pointer to the pbr BFM cpb agent.
   mby_pbr_bfm_cpb_agent cpb_agent_ptr;

   // VARIABLE: cptr_req_pending
   //    Counter of the current cptr reqs.
   int cptr_req_pending;

   int xaction_n;

   // Registering class with the factory
   `uvm_component_utils(mby_pbr_bfm_cptr_req#(T_req))

   // -------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS
   //    string name          - An instance name of the address translator.
   //    uvm_component parent - The translator's parent component.
   // -------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: set_agent_ptr
   //
   // Assigns the internal agent pointer to be the same as the input argument.
   //
   // ARGUMENTS:
   //    mby_pbr_bfm_cpb_agent cpb_agent_ptr  - An instance name of the address translator.
   // -------------------------------------------------------------------------
   function void set_agent_ptr(mby_pbr_bfm_cpb_agent cpb_agent_ptr);
      this.cpb_agent_ptr = cpb_agent_ptr;
   endfunction : set_agent_ptr

   // ------------------------------------------------------------------------
   // FUNCTION: build_phase
   //
   // Gets the agent's configuration object and vif from the config db.
   // Creates monitor, sequencer and driver as specified in configuration object
   //
   // ARGUMENTS:
   //    uvm_phase phase - phase object.
   // ------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);

      cptr_req_cfg_obj = new("cptr_req_cfg_obj");
      cptr_req_pending = 0;
      xaction_n = 0;
      `uvm_info(get_name(), ("DBG_ALF: Done building cptr req sub"), UVM_DEBUG)
   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   //
   // ARGUMENTS:
   //    T_req ap_item - cptr request issued to the pbr BFM.
   // ------------------------------------------------------------------------
   function void write(T_req ap_item);
      //if (ap_item.data.req_valid === 1)
      xaction_n++;
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: cptr --write called xaction_n=0x%0x",xaction_n), UVM_DEBUG)
      //start_cptr_req_rsp_task(ap_item);
      // Have a local copy of cptr_req, task can be forked safely.
      fork
         cptr_req_rsp(ap_item);
      join_none
   endfunction

   virtual protected task automatic cptr_req_rsp(T_req cptr_req);
      string         msg_str;
      mby_pbr_bfm_cptr_response_seq cptr_rsp_seq;
      T_req cptr_rsp_xaction;

      cptr_req_pending++;

//      msg_str     = $sformatf("DBG_ALF: cptr_req_rsp() : cptr request start. req_id=0x%0x req_valid=0x%0x", cptr_req.data.req_id, cptr_req.data.req_valid);
//      `uvm_info(get_type_name(), msg_str, UVM_DEBUG)

//      msg_str     = $sformatf("DBG_ALF: cptr_req_rsp() : Received xaction req: %s", cptr_req.convert2string());
//      `uvm_info(get_type_name(), msg_str, UVM_DEBUG)

      cptr_rsp_seq = mby_pbr_bfm_cptr_response_seq::type_id::create("cptr_rsp_seq", this);
      cptr_rsp_xaction = T_req::type_id::create("cptr_rsp_xaction", this);

      if(!cptr_rsp_xaction.randomize() with {
               data.req_seg_ptr == 0;
               data.req_valid == 0;
               data.req_id == xaction_n;//debug

               data.fifo_ack == 1;
               data.data_valid == 0;
               data.data_clean_ptr == 0;

               data.id_ack_id == 0;
               data.id_ack_valid == 0;
            })begin
         `uvm_error(get_name(), "Unable to randomize cptr_xaction ack");
      end

      //repeat(10) @(posedge this.cpb_agent_ptr.driver.io_policy.vintf.clk); //cpb clk
      `uvm_info(get_name(),{"DBG_ALF",cptr_rsp_xaction.convert2string()},UVM_DEBUG)
      cptr_rsp_seq.cptr_rsp = cptr_rsp_xaction;
      cptr_rsp_seq.start(this.cpb_agent_ptr.sequencer);
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: cptr_rsp_xaction ///ended ack xaction_n=0x%0x",xaction_n), UVM_DEBUG)

      if(!cptr_rsp_xaction.randomize() with {
               data.req_seg_ptr == 0;
               data.req_valid == 0;
               data.req_id == xaction_n;//debug

               data.fifo_ack == 0;
               data.data_valid == 1;
               data.data_clean_ptr == 9 + xaction_n;// A, B ,C

               data.id_ack_id == cptr_req.data.req_id; //same ack id from req id
               data.id_ack_valid == 1;
            })begin
         `uvm_error(get_name(), "Unable to randomize cptr_xaction data");
      end

      repeat(3) @(posedge this.cpb_agent_ptr.driver.io_policy.vintf.clk); //cpb clk
      `uvm_info(get_name(),{"DBG_ALF",cptr_rsp_xaction.convert2string()},UVM_DEBUG)
      cptr_rsp_seq.cptr_rsp = cptr_rsp_xaction;
      cptr_rsp_seq.start(this.cpb_agent_ptr.sequencer);
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: cptr_rsp_xaction ///ended data xaction_n=0x%0x",xaction_n), UVM_DEBUG)

//      msg_str     = $sformatf("DBG_ALF: cptr_req_rsp() : cptr request done. req_id=0x%0x", cptr_req.data.req_id);
//      `uvm_info(get_type_name(), msg_str, UVM_DEBUG)

      cptr_req_pending--;
   endtask : cptr_req_rsp

   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main monitor thread starts: calls the monitor_if() virtual task
   //
   // ARGUMENTS:
   //    uvm_phase phase - phase object.
   // -------------------------------------------------------------------------
   task run_phase (uvm_phase phase);
      string   msg_str;
      bit      objection_raised = 0;

      forever @ (posedge this.cpb_agent_ptr.driver.io_policy.vintf.clk) begin
            if (!objection_raised && cptr_req_pending > 0) begin
               msg_str = $sformatf("DBG_ALF: run_phase(): There are %0d pending cptr requests", cptr_req_pending);
            //`uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            end else if (objection_raised && cptr_req_pending == 0) begin
               phase.drop_objection(this, "SMM BFM: No pending cptr requests.", 1);
               msg_str = $sformatf("DBG_ALF: run_phase(): There are %0d pending cptr requests, dropped phase objection.",
                  cptr_req_pending);
               //`uvm_info(get_type_name(), msg_str, UVM_DEBUG)
               objection_raised = 0;
            end
         end
   endtask : run_phase
endclass : mby_pbr_bfm_cptr_req

`endif

