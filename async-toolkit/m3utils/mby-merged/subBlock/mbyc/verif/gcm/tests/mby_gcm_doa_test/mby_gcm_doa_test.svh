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
//
//   Class:    mby_gcm_doa_test
//

`ifndef MBY_GCM_RANDOM_TEST__SV
`define MBY_GCM_RANDOM_TEST__SV

typedef class mby_gcm_doa_seq;
class mby_gcm_doa_test extends mby_gcm_base_test;

   `uvm_component_utils(mby_gcm_doa_test)
   //------------------------------------------------------------------------------
   // Constructor: new
   //  Arguments:
   //  name   - GCM random test object name.
   //  parent - Component parent object.
   //------------------------------------------------------------------------------
   function new (string name="mby_gcm_doa_test", uvm_component parent=null);
      super.new (name, parent);
   endfunction :  new

   //------------------------------------------------------------------------------
   // Function: build_phase
   //  Arguments:
   //  phase - uvm_phase object.
   //------------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);
   endfunction : build_phase

   //------------------------------------------------------------------------------
   // Function: connect_phase
   // Sets USER_DATA_PHASE sequence.
   //
   //  Arguments:
   //  phase - uvm_phase object.
   //------------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      set_default_sequences();
   endfunction : connect_phase

   //---------------------------------------------------------------------------
   // Function: set_default_sequences()
   //---------------------------------------------------------------------------
   function void set_default_sequences();
      super.set_default_sequences();
      `uvm_info("::set_default_sequences", "Setting phase sequences", UVM_NONE)

      // Specifying main phase sequence
      uvm_config_db#(uvm_object_wrapper)::set(this,
         "env.tb_seqr.main_phase",
         "default_sequence",
         mby_gcm_doa_seq::type_id::get());
   endfunction : set_default_sequences

   //---------------------------------------------------------------------------
   // Function: start_of_simulation_phase()
   //
   // Updates the uvm report server to use MBY's report server
   //
   //---------------------------------------------------------------------------
   function void start_of_simulation_phase( uvm_phase phase );
      super.start_of_simulation_phase( phase );

      `uvm_info("start_of_simulation_phase()", "Exiting start_of_sim phase", UVM_NONE)
   endfunction: start_of_simulation_phase

   //---------------------------------------------------------------------------
   // Task: run_phase()
   //
   // Prints out a message to identify start of run phase
   //---------------------------------------------------------------------------
   task run_phase(uvm_phase phase);
      phase.raise_objection(this);
      `uvm_info("::run_phase()", "Starting run phase", UVM_NONE)
      phase.drop_objection(this);
      
   endtask : run_phase
endclass : mby_gcm_doa_test

class mby_gcm_doa_seq extends mby_gcm_seq_lib::mby_gcm_env_base_seq;

   `uvm_object_utils(mby_gcm_doa_seq)

   //------------------------------------------------------------------------------
   //  Constructor: new
   //  Arguments:
   //  name   - GCM random test  seq object name.
   //------------------------------------------------------------------------------
   function new (string name="mby_gcm_doa_seq");
      super.new (name);
      this.set_automatic_phase_objection(1);
      `uvm_info(this.get_name(), ("mby_gcm_doa_seq::new"), UVM_LOW)

   endfunction :  new

   //------------------------------------------------------------------------------
   //  Task:  body
   //  Counts 50 clocks and then completes.
   //------------------------------------------------------------------------------
   task body ();
      int count;
     
      this.set_name("mby_gcm_doa_seq");

      `uvm_info(get_name(), "mby_gcm_doa_seq is running!", UVM_MEDIUM);
      

      repeat(500) begin
         @(posedge vif.fab_clk);
         count++;
         `uvm_info(get_name(), $sformatf("mby_gcm_doa_seq: clock edge %0d",count), UVM_FULL);
      end
   

   endtask

endclass : mby_gcm_doa_seq

`endif // MBY_GCM_RANDOM_TEST__SV
