// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR DPTR req
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_dptr_req.svh
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
`ifndef __MBY_PBR_BFM_DPTR_REQ__
`define __MBY_PBR_BFM_DPTR_REQ__
//-----------------------------------------------------------------------------
// CLASS: mby_pbr_bfm_dptr_req
//
// This mrd req class is instantiated in the top pbr_bfm class.
// It consumes dptr requests from the different interfaces
//
// PARAMETERS::
//     type T_req - sequence item type to be handled.
//-----------------------------------------------------------------------------

class mby_pbr_bfm_dptr_req
   #(
      type T_req  = shdv_base_sequence_item//mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber #(T_req);

   // VARIABLE: dptr_req_cfg_obj
   //    The bfm's mrd request configuration objects
   mby_pbr_bfm_cfg dptr_req_cfg_obj; //FIXME: raalfaro

   // VARIABLE: dpm_agent_ptr
   //    Pointer to the pbr BFM dpm agent.
   mby_pbr_bfm_dpm_agent dpm_agent_ptr;

   // VARIABLE: dptr_req_pending
   //    Counter of the current dptr reqs.
   int dptr_req_pending;

   int xaction_n;

   // Registering class with the factory
   `uvm_component_utils(mby_pbr_bfm_dptr_req#(T_req))

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
   //    mby_pbr_bfm_dpm_agent dpm_agent_ptr  - An instance name of the address translator.
   // -------------------------------------------------------------------------
   function void set_agent_ptr(mby_pbr_bfm_dpm_agent dpm_agent_ptr);
      this.dpm_agent_ptr = dpm_agent_ptr;
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

      dptr_req_cfg_obj = new("dptr_req_cfg_obj");
      dptr_req_pending = 0;
      xaction_n = 0;
      `uvm_info(get_name(), ("DBG_ALF: Done building dptr req sub"), UVM_DEBUG)
   endfunction : build_phase

   // ------------------------------------------------------------------------
   // FUNCTION: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   //
   // ARGUMENTS:
   //    T_req ap_item - dptr request issued to the pbr BFM.
   // ------------------------------------------------------------------------
   function void write(T_req ap_item);
      xaction_n++;
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: dptr --write called xaction_n=0x%0x",xaction_n), UVM_DEBUG)
      //start_dptr_req_rsp_task(ap_item);
      // Have a local copy of dptr_req, task can be forked safely.
      fork
         dptr_req_rsp(ap_item);
      join_none
   endfunction

   virtual protected task automatic dptr_req_rsp(T_req dptr_req);
      string         msg_str;
      mby_pbr_bfm_dptr_response_seq dptr_rsp_seq;
      T_req dptr_rsp_xaction;

      dptr_req_pending++;


      msg_str     = $sformatf("DBG_ALF: dptr_req_rsp() : dptr request start. xaction_n=0x%0x pod_put_req=0x%0x",
         xaction_n, dptr_req.data.pod_put_req);
      `uvm_info(get_type_name(), msg_str, UVM_DEBUG)

      dptr_rsp_seq = mby_pbr_bfm_dptr_response_seq::type_id::create("dptr_rsp_seq", this);
      dptr_rsp_xaction = T_req::type_id::create("dptr_rsp_xaction", this);

      if(!dptr_rsp_xaction.randomize() with {
               data.data_dirty_ptr == 'hBACA;
               data.pod_put_req == 0;
               data.pod_put_type == 0;
               data.pod_put_ack == 1;
               data.schedule_stall == 0;
            })begin
         `uvm_error(get_name(), "Unable to randomize dptr_xaction");
      end
      `uvm_info(get_name(),{"DBG_ALF",dptr_rsp_xaction.convert2string()},UVM_DEBUG)
      dptr_rsp_seq.dptr_rsp = dptr_rsp_xaction;
      dptr_rsp_seq.start(this.dpm_agent_ptr.sequencer);
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: dptr_rsp_xaction ///ended pod_put_ack xaction_n=0x%0x",xaction_n), UVM_DEBUG)

      repeat(1) @(posedge this.dpm_agent_ptr.driver.io_policy.vintf.clk); //dpm clk
      if(!dptr_rsp_xaction.randomize() with {
               data.data_dirty_ptr == 'hBEBE;
               data.pod_put_req == 0;
               data.pod_put_type == 0;
               data.pod_put_ack == 0;
               data.schedule_stall == 1;
            })begin
         `uvm_error(get_name(), "Unable to randomize dptr_xaction");
      end
      `uvm_info(get_name(),{"DBG_ALF",dptr_rsp_xaction.convert2string()},UVM_DEBUG)
      dptr_rsp_seq.dptr_rsp = dptr_rsp_xaction;
      dptr_rsp_seq.start(this.dpm_agent_ptr.sequencer);
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: dptr_rsp_xaction ///ended schedule_stall_1 xaction_n=0x%0x",xaction_n), UVM_DEBUG)

      repeat(20) @(posedge this.dpm_agent_ptr.driver.io_policy.vintf.clk); //dpm clk
      if(!dptr_rsp_xaction.randomize() with {
               data.data_dirty_ptr == 'hCAFE;
               data.pod_put_req == 0;
               data.pod_put_type == 0;
               data.pod_put_ack == 0;
               data.schedule_stall == 0;
            })begin
         `uvm_error(get_name(), "Unable to randomize dptr_xaction");
      end
      `uvm_info(get_name(),{"DBG_ALF",dptr_rsp_xaction.convert2string()},UVM_DEBUG)
      dptr_rsp_seq.dptr_rsp = dptr_rsp_xaction;
      dptr_rsp_seq.start(this.dpm_agent_ptr.sequencer);
      `uvm_info(get_type_name(), $sformatf("DBG_ALF: dptr_rsp_xaction ///ended schedule_stall_0 xaction_n=0x%0x",xaction_n), UVM_DEBUG)



      dptr_req_pending--;
   endtask : dptr_req_rsp

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

      forever @ (posedge this.dpm_agent_ptr.driver.io_policy.vintf.clk) begin
            if (!objection_raised && dptr_req_pending > 0) begin
               msg_str = $sformatf("DBG_ALF: run_phase(): There are %0d pending dptr requests", dptr_req_pending);
               `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
            end else if (objection_raised && dptr_req_pending == 0) begin
               phase.drop_objection(this, "PBR BFM: No pending dptr requests.", 1);
               msg_str = $sformatf("DBG_ALF: run_phase(): There are %0d pending dptr requests, dropped phase objection.",
                  dptr_req_pending);
               `uvm_info(get_type_name(), msg_str, UVM_DEBUG)
               objection_raised = 0;
            end
         end
   endtask : run_phase
endclass : mby_pbr_bfm_dptr_req

`endif
