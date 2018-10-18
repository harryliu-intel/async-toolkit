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
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//Class : mby_mesh_env
//This is the MBY Shared memory mesh Environment file which is extended from shdv_base_env.
//This class instantiates agents and also creates and connects scoreboards.
//------------------------------------------------------------------------------

`ifndef __MBY_MESH_ENV_GUARD
`define __MBY_MESH_ENV_GUARD

`ifndef __INSIDE_MBY_MESH_ENV_PKG
`error "Attempt to include file outside of mby_mesh_env_pkg."
`endif

class mby_mesh_env extends shdv_base_env;

   // Variable:  tb_cfg
   // Protected Top Level mesh environment configuration.
   protected mby_mesh_tb_top_cfg                           tb_cfg;

   // Variable:  tb_vif
   // Interface handle to mesh Testbench.
   virtual   mby_mesh_tb_if                                tb_vif;


   `uvm_component_utils_begin(mby_mesh_env)
       `uvm_field_object  (tb_cfg,                          UVM_ALL_ON)
   `uvm_component_utils_end

   //---------------------------------------------------------------------------
   //  Constructor: new
   //  Set Top_cfg type in Saola
   //
   //  Arguments:
   //      string name - MBY MESH environment object name.
   //      uvm_component parent - Component parent object.
   //---------------------------------------------------------------------------
   function new(string name = "mby_mesh_env", uvm_component parent = null);
      super.new(name, parent);

      config_type = "mby_mesh_tb_top_cfg";

   endfunction : new

   //---------------------------------------------------------------------------
   //  Function: build_phase
   //  Create the agents, create the End to End scoreboards.
   //
   //  Arguments:
   //      phase - uvm_phase object.
   //---------------------------------------------------------------------------
   virtual function void build_phase(uvm_phase phase);
      uvm_object tmp_cfg;

      super.build_phase(phase);

      if(get_config_object("mby_mesh_tb_top_cfg", tmp_cfg)) begin
         $cast(tb_cfg, tmp_cfg);
      end

      if (tb_cfg == null) begin
         `uvm_fatal(get_full_name(), "Unable to acquire handle to mby_mesh_tb_top_cfg object!")
      end

      `uvm_info (get_full_name , $sformatf("Mesh Top _cfg : %s", tb_cfg.sprint()), UVM_FULL)

      if(!uvm_config_db#(string)::get(this, "" , "TI_PATH", tb_cfg.ti_path)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's TI_PATH was not successful!")
      end
      `uvm_info(get_full_name(),$sformatf("This Mesh Build Phase set tb_cfg.ti_path = %s", tb_cfg.ti_path),UVM_FULL)

      if(!uvm_config_db#(string)::get(this, "" , "RTL_TOP_PATH", tb_cfg.rtl_top_path)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's RTL_TOP_PATH was not successful!")
      end
      `uvm_info(get_full_name(),$sformatf("This Mesh Build Phase set tb_cfg.rtl_top_path = %s", tb_cfg.rtl_top_path),UVM_FULL)


      if(!uvm_config_db#(virtual mby_mesh_tb_if)::get(this, "", "mby_mesh_tb_if", tb_vif)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's TB_IF was not successful!")
      end

   endfunction: build_phase

   //---------------------------------------------------------------------------
   //  Function: connect_phase
   //  Connects different BFM interfaces and Scoreboard
   //  Arguments:
   //      phase - uvm_phase object.
   //---------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);

      if( _level == SLA_TOP ) begin
      end
   endfunction: connect_phase

   //---------------------------------------------------------------------------
   //  Function: end_of_elaboration_phase
   //  Randomizes the RAL objects.  Prints BFM configurations.
   //
   //  Arguments:
   //  phase - uvm_phase object.
   //---------------------------------------------------------------------------
   function void end_of_elaboration_phase(uvm_phase phase);
      super.end_of_elaboration_phase(phase);

      if (_level == SLA_TOP) begin
      end

   endfunction: end_of_elaboration_phase

   //---------------------------------------------------------------------------
   //  Function: start_of_simulation_phase
   //
   //  Arguments:
   //  phase - uvm_phase object
   //---------------------------------------------------------------------------
   function void start_of_simulation_phase(uvm_phase phase);
      super.start_of_simulation_phase(phase);
   endfunction: start_of_simulation_phase

   //---------------------------------------------------------------------------
   // Function: get_tb_vif()
   // Returns a handle to mesh testbench virtual interface.
   //---------------------------------------------------------------------------
   function virtual mby_mesh_tb_if get_tb_vif();
      return tb_vif;
   endfunction:get_tb_vif

   //---------------------------------------------------------------------------
   // Function: get_tb_cfg()
   // Returns object handle to mesh env configuration (mby_mesh_tb_top_cfg)
   //---------------------------------------------------------------------------
   function mby_mesh_tb_top_cfg get_tb_cfg();
      return tb_cfg;
   endfunction : get_tb_cfg

endclass

`endif // __MBY_MESH_ENV_GUARD

