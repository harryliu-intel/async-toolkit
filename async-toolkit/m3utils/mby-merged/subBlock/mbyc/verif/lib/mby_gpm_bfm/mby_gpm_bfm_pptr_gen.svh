//-----------------------------------------------------------------------------
// Title         : Madison Bay GPM pod pointer generator class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gpm_bfm_pptr_gen.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// This is the pod pointer generator class
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
`ifndef __MBY_GPM_BFM_PKG__
`error "Attempt to include file outside of mby_gpm_bfm_pkg."
`endif
`ifndef __MBY_GPM_BFM_PPTR_GEN__
`define __MBY_GPM_BFM_PPTR_GEN__
//-----------------------------------------------------------------------------
// CLASS: mby_gpm_bfm_pptr_gen
//
// The pod pointer generator is the component that interfaces with all the
// agents in the gpm_bfm. This generator has two modes of operation too:
// ingress and egress.
//
// When operating in ingress mode, a process will be creating random addresses
// with a normal distribution. Each time there are ~85 (exact number is TBD)
// of these addresses, a set of 4 mesh_xaction transactions will be created:
// the 85th random address will be used to create 4 new addresses to target
// each of the 4 64B-words of the 256B location. The data to write in those
// 4 locations is the corresponding to the 84 addresses that were previously
// generated. These mesh transactions will go out on the port (1). Once the
// mesh transactions are sent, the pptr_gen will create a pod_xaction using
// the 85th address and will send it to the free pod pointer agent
// (fpptr_agent) in port (2), so it can be driven to the pod ring interface,
// and eventually consumed by the ingress RTL.
//
// When operating in egress mode, a process will be creating random addresses
// with a normal distribution, and in this case, each time there's a new
// random address, an ingress tag transaction (tbd) will be created and sent
// over the port (1) to the ingress tag bfm (intag_bfm).
// In this mode, the dpptr_agent will be sending dirty pod xactions
// (pod_xaction) to the pptr_gen through the port (2), the generator will
// take the address from the pod xaction, create the corresponding
// mesh_xactions and will send read operations to the smm_bfm in (3) and
// obtain the dirty pointer data from the smm_bfm in (4). The generator will
// then parse this data, filter out any invalid addresses and then will
// 'convert' the dirty pointers into clean free pointers and eventually
// send them out again in port (5).
//
// PARAMETERS::
//    type T_req - sequence item type to be handled.
//-----------------------------------------------------------------------------

class mby_gpm_bfm_pptr_gen 
   #(
      type T_req = mby_base_sequence_item
   )
   extends uvm_pkg::uvm_subscriber#(.T(T_req));

   //--------------------------------------------------------------------------
   //
   // TODO : This is an empty shell for the pod pointer generator.
   //        - add analysis port declarations, build, connect and run phases.
   //
   //--------------------------------------------------------------------------

   // VARIABLE: cfg_obj
   // The bfm's configuration object
   mby_gpm_bfm_cfg cfg_obj;

   // Registering class with the factory
   `uvm_component_utils(mby_gpm_bfm_pptr_gen)

   //--------------------------------------------------------------------------
   // CONSTRUCTOR: new
   //
   // Constructor
   //
   // ARGUMENTS:
   //    string name          - An instance name of the pptr generator.
   //    uvm_component parent - The generators's parent component.
   //--------------------------------------------------------------------------
   function new(string name, uvm_component parent);
      super.new(name, parent);
   endfunction : new

   // -------------------------------------------------------------------------
   // FUNCTION: write
   //
   // Method that must be defined in each uvm_subscriber subclass. Access
   // to this method by outside components should be done via the
   // analysis_export.
   //
   // ARGUMENTS:
   //    T_req ap_item - Dirty pod pointer issued to the GPM BFM.
   // -------------------------------------------------------------------------
   function void write(T_req ap_item);
      //TODO: What to do when a dirty pod pointer arrives
   endfunction
   
   // -------------------------------------------------------------------------
   // TASK: run_phase
   //
   // Main pod pointer generator task
   //
   // ARGUMENTS:
   //    uvm_phase phase - phase object.
   // -------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      string msg_str;
      
      gpm_bfm_pod_seq_t fpod_rsp_seq;
      
      
   endtask : run_phase
   
endclass : mby_gpm_bfm_pptr_gen
`endif