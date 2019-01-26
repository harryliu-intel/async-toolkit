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

`ifndef __MBY_GPOL_ENV_GUARD
`define __MBY_GPOL_ENV_GUARD

`ifndef __INSIDE_MBY_GPOL_ENV_PKG
`error "Attempt to include file outside of mby_gpol_env_pkg."
`endif

//------------------------------------------------------------------------------
//Class : mby_gpol_env
//This is the MBY Shared memory gpol Environment file which is extended from shdv_base_env.
//This class instantiates agents and also creates and connects scoreboards.
//------------------------------------------------------------------------------

class mby_gpol_env extends shdv_base_env;

   // Variable:  tb_cfg
   // Protected Top Level gpol environment configuration.
   protected mby_gpol_tb_top_cfg                           tb_cfg;

   // Variable:  tb_vif
   // Interface handle to gpol Testbench.
   virtual   mby_gpol_tb_if                                tb_vif;

   
   // Variable:  tb_ral
   // Handle to gpol RAL.
   mby_gpol_reg_pkg::mby_gpol_reg_blk                      tb_ral;

   `uvm_component_utils_begin(mby_gpol_env)
   `uvm_component_utils_end

   //---------------------------------------------------------------------------
   //  Constructor: new
   //  Set Top_cfg type in Saola
   //
   //  Arguments:
   //      string name - MBY GPOL environment object name.
   //      uvm_component parent - Component parent object.
   //---------------------------------------------------------------------------
   function new(string name = "mby_gpol_env", uvm_component parent = null);
      super.new(name, parent);

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

      //
      // TB Cfg handle
      //
      if(get_config_object("mby_gpol_tb_top_cfg", tmp_cfg)) begin
         $cast(tb_cfg, tmp_cfg);
      end

      if (tb_cfg == null) begin
         `uvm_fatal(get_full_name(), "Unable to acquire handle to mby_gpol_tb_top_cfg object!")
      end

      `uvm_info (get_full_name , $sformatf("GPOL Top _cfg : %s", tb_cfg.sprint()), UVM_FULL)

      //
      // TI_PATH handle
      //
      if(!uvm_config_db#(string)::get(this, "" , "TI_PATH", tb_cfg.ti_path)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's TI_PATH was not successful!")
      end
      `uvm_info(get_full_name(),$sformatf(" GPOL Build Phase set tb_cfg.ti_path = %s", tb_cfg.ti_path),UVM_FULL)

      //
      // RTL_TOP Path handle
      //
      if(!uvm_config_db#(string)::get(this, "" , "RTL_TOP_PATH", tb_cfg.rtl_top_path)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's RTL_TOP_PATH was not successful!")
      end
      `uvm_info(get_full_name(),$sformatf("This GPOL Build Phase set tb_cfg.rtl_top_path = %s", tb_cfg.rtl_top_path),UVM_FULL)

      //
      // GPOL TB interface
      //
      if(!uvm_config_db#(virtual mby_gpol_tb_if)::get(this, "", "mby_gpol_tb_if", tb_vif)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's TB_IF was not successful!")
      end

     
      build_ral();

   endfunction: build_phase


   //---------------------------------------------------------------------------
   //  Function: build_ral
   //  Builds GPOL register model.
   //
   //---------------------------------------------------------------------------
   virtual function void build_ral();

      // Check if ral is already set by FC
      if (tb_ral == null) begin
         tb_ral = mby_gpol_reg_pkg::mby_gpol_reg_blk::type_id::create("tb_ral");
         tb_ral.build();
         //TODO: Update the base addr.
         tb_ral.default_map.set_base_addr(`UVM_REG_ADDR_WIDTH'h4000);
         tb_ral.lock_model();

         tb_ral.set_hdl_path_root("mby_gpol_tb_top.msh_node_top");

       // Build the Adapter's based on agt's active        
      end
      
   endfunction: build_ral
      
   //---------------------------------------------------------------------------
   //  Function: connect_phase
   //  Connects different BFM interfaces and Scoreboard
   //  Arguments:
   //      phase - uvm_phase object.
   //---------------------------------------------------------------------------
   function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
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
   // Returns a handle to gpol testbench virtual interface.
   //---------------------------------------------------------------------------
   function virtual mby_gpol_tb_if get_tb_vif();
      return tb_vif;
   endfunction:get_tb_vif

   //---------------------------------------------------------------------------
   // Function: get_tb_cfg()
   // Returns object handle to gpol env configuration (mby_gpol_tb_top_cfg)
   //---------------------------------------------------------------------------
   function mby_gpol_tb_top_cfg get_tb_cfg();
      return tb_cfg;
   endfunction : get_tb_cfg
   
   //---------------------------------------------------------------------------
   // Function: get_tb_ral()
   // Returns object handle to gpol ral (mby_gpol_reg_blk)
   //---------------------------------------------------------------------------
   function mby_gpol_reg_pkg::mby_gpol_reg_blk get_tb_ral();
      return tb_ral;
   endfunction : get_tb_ral
   
   //---------------------------------------------------------------------------
   // Function: set_tb_ral()
   // Sets handle to gpol ral (mby_gpol_reg_blk). Used to pass handle to RAL from fullchip env.
   //---------------------------------------------------------------------------
   function set_tb_ral(mby_gpol_reg_pkg::mby_gpol_reg_blk ral);
      tb_ral = ral;
   endfunction : set_tb_ral

endclass

`endif // __MBY_GPOL_ENV_GUARD

