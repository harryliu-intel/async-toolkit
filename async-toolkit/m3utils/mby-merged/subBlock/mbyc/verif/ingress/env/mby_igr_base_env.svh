//-----------------------------------------------------------------------------
// Title         : Ingress Testbench Base
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_tb_base.sv
// Author        : 
// Created       : 
// Last modified : 
//-----------------------------------------------------------------------------
// Description :
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// xx.xx.2018 : created
//-----------------------------------------------------------------------------

`ifndef __MBY_IGR_ENV_PKG_GUARD
   `error "Attempt to include file outside of mby_igr_env_pkg."
`endif

`ifndef __MBY_IGR_BASE_ENV_SVH__
`define __MBY_IGR_BASE_ENV_SVH__

//-------------------------------------------------------------------------------
// Class: mby_igr_base_env
//-------------------------------------------------------------------------------
class mby_igr_base_env extends shdv_base_env;
   protected mby_igr_tb_cfg tb_cfg;

   `uvm_component_utils_begin(mby_igr_base_env)
   `uvm_component_utils_end

   protected static mby_igr_base_env     _top_tb_env;
   protected static mby_igr_tb_sequencer _tb_sequencer;

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "mby_igr_base_env", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

   //---------------------------------------------------------------------------
   // Function: mby_igr_base_env build
   // build phase of mby_igr_base_env
   // All VC's and Agent should be build in this pahse.
   // For each new VC's/Agnet it is recommand to add it an a specific function
   //---------------------------------------------------------------------------
   function void build_phase(uvm_phase phase);
      super.build_phase(phase);

      // populate the static members only if the top most tb has not already been constructed
      if(_top_tb_env == null) begin
         _top_tb_env = this;
         _tb_sequencer = mby_igr_tb_sequencer::type_id::create("mby_igr_tb_sequencer",this);
         _tb_sequencer.add_sequencer("TB_SEQUENCER",get_name(),_tb_sequencer);
      end
   endfunction : build_phase

   //---------------------------------------------------------------------------
   // Function: get_top_tb_env
   //---------------------------------------------------------------------------
   static function mby_igr_base_env get_top_tb_env();
      return(_top_tb_env);
   endfunction : get_top_tb_env

   //---------------------------------------------------------------------------
   // Function: mby_igr_base_env connect
   // connect phase of mby_igr_base_env
   //---------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
   endfunction : connect_phase

   //---------------------------------------------------------------------------
   // Function: mby_igr_base_env end_of_elaboration
   // end_of_elaboration phase of mby_igr_base_env
   // In this phase we randomize the fuse env
   //---------------------------------------------------------------------------
   virtual function void end_of_elaboration_phase (uvm_phase phase);
      super.end_of_elaboration_phase(phase);
   endfunction : end_of_elaboration_phase

   //---------------------------------------------------------------------------
   // Function: mby_igr_base_env start_of_simulation
   // start_of_simulation phase of mby_igr_base_env
   //---------------------------------------------------------------------------
   virtual function void start_of_simulation_phase (uvm_phase phase);
      super.start_of_simulation_phase(phase);
   endfunction : start_of_simulation_phase

   //---------------------------------------------------------------------------
   // Function: get_tb_seqr
   //---------------------------------------------------------------------------
   static function mby_igr_tb_sequencer get_tb_seqr();
      return(_tb_sequencer);
   endfunction : get_tb_seqr

   //---------------------------------------------------------------------------
   // Function: add_sequencer
   // Add a sequencer. Adds the uvm_sequencer_base object to the _sequencer_list 
   // list.
   //---------------------------------------------------------------------------
   function bit add_sequencer ( string agent_type, string agent_name, uvm_sequencer_base sequencer);
      `uvm_info({get_name(),".add_sequencer"},$sformatf("Adding sequencer %s %s %s", agent_type, agent_name, sequencer.get_full_name()),UVM_LOW);
      return (_tb_sequencer.add_sequencer( agent_type, agent_name, sequencer));
   endfunction : add_sequencer

endclass : mby_igr_base_env

`endif // __MBY_IGR_TB_BASE_SVH__
