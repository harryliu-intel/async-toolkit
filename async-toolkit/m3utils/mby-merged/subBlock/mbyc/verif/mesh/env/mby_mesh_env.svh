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
   virtual   mby_mgp_mim_req_if                                req_wb_if;
   virtual   mby_mgp_mim_req_if                                req_eb_if;
   virtual   mby_mgp_mim_rsp_if                                rsp_wb_if;
   virtual   mby_mgp_mim_rsp_if                                rsp_eb_if;

   mby_mgp_bfm_pkg::mby_mgp_bfm                            wb_mgp_bfm[mby_mgp_bfm_pkg::NUM_MSH_ROWS];
   mby_mgp_bfm_pkg::mby_mgp_bfm                            eb_mgp_bfm[mby_mgp_bfm_pkg::NUM_MSH_ROWS];
   mby_mgp_bfm_pkg::mby_mgp_bfm                            sb_mgp_bfm[mby_mgp_bfm_pkg::NUM_MSH_COLS];
   mby_mgp_bfm_pkg::mby_mgp_bfm                            nb_mgp_bfm[mby_mgp_bfm_pkg::NUM_MSH_COLS];
   
   // Variable:  tb_ral
   // Handle to mesh RAL.
   mby_mesh_reg_pkg::mby_mesh_reg_blk                      tb_ral;

   `uvm_component_utils_begin(mby_mesh_env)
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

      
      if(!uvm_config_db#(virtual mby_mgp_mim_req_if)::get(this, "", "mby_mgp_mim_req_if", req_eb_if)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for Mesh Interface was not successful!")
      end
      if(!uvm_config_db#(virtual mby_mgp_mim_req_if)::get(this, "", "mby_mgp_mim_req_if", req_wb_if)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for Mesh Interface was not successful!")
      end

      if(!uvm_config_db#(virtual mby_mgp_mim_rsp_if)::get(this, "", "mby_mgp_mim_rsp_if", rsp_eb_if)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for Mesh Interface was not successful!")
      end
      if(!uvm_config_db#(virtual mby_mgp_mim_rsp_if)::get(this, "", "mby_mgp_mim_rsp_if", rsp_wb_if)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for Mesh Interface was not successful!")
      end

      build_mgp_bfm();
      build_ral();
   endfunction: build_phase

   //---------------------------------------------------------------------------
   //  Function: build_mgp_bfm
   //  Build the 4 mgp bfms and assign interfaces and cfg objects. 
   //---------------------------------------------------------------------------
   function void build_mgp_bfm();


      for (int idx = 0; idx < mby_mgp_bfm_pkg::NUM_MSH_ROWS; idx++) begin
	 
         eb_mgp_bfm[idx] = mby_mgp_bfm_pkg::mby_mgp_bfm::type_id::create($sformatf("eb_mgp_bfm%0d", idx), this);
	 wb_mgp_bfm[idx] = mby_mgp_bfm_pkg::mby_mgp_bfm::type_id::create($sformatf("wb_mgp_bfm%0d", idx), this);

	 eb_mgp_bfm[idx].assign_cfg(tb_cfg.env_cfg.bfm_cfg);
	 eb_mgp_bfm[idx].assign_vi(req_eb_if, rsp_eb_if);
	 wb_mgp_bfm[idx].assign_cfg(tb_cfg.env_cfg.bfm_cfg);
	 wb_mgp_bfm[idx].assign_vi(req_wb_if, rsp_wb_if);

      end
/*
      for (int idx = 0; idx < mby_mgp_bfm_pkg::NUM_MSH_COLS; idx++) begin
         sb_mgp_bfm[idx] = mby_mgp_bfm_pkg::mby_mgp_bfm::type_id::create($sformatf("sb_mgp_bfm%0d", idx), this);
	 nb_mgp_bfm[idx] = mby_mgp_bfm_pkg::mby_mgp_bfm::type_id::create($sformatf("nb_mgp_bfm%0d", idx), this);

	 sb_mgp_bfm[idx].assign_cfg(tb_cfg.env_cfg.bfm_cfg);
	 sb_mgp_bfm[idx].assign_vi(mby_gmm_mig_rd_op_sb_if, mby_gmm_mig_rsp_op_sb_if, mby_gmm_mig_wr_op_sb_if, mby_gmm_mig_rsp_data_sb_if, mby_gmm_mig_wr_data_sb_if);

	 nb_mgp_bfm[idx].assign_cfg(tb_cfg.env_cfg.bfm_cfg);
	 nb_mgp_bfm[idx].assign_vi(mby_gmm_mig_rd_op_nb_if, mby_gmm_mig_rsp_op_nb_if, mby_gmm_mig_wr_op_nb_if, mby_gmm_mig_rsp_data_nb_if, mby_gmm_mig_wr_data_nb_if);
	 
      end */
   endfunction: build_mgp_bfm
   
   //---------------------------------------------------------------------------
   //  Function: build_ral
   //  Builds Mesh register model.
   //
   //---------------------------------------------------------------------------
   virtual function void build_ral();

      // Check if ral is already set by FC
      if (tb_ral == null) begin
         tb_ral = mby_mesh_reg_pkg::mby_mesh_reg_blk::type_id::create("tb_ral");
         tb_ral.build();
         //TODO: Update the base addr.
         tb_ral.default_map.set_base_addr(`UVM_REG_ADDR_WIDTH'h4000);
         tb_ral.lock_model();
         tb_ral.set_hdl_path_root("mby_mesh_tb_top.msh_node_top");

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
   
   //---------------------------------------------------------------------------
   // Function: get_tb_ral()
   // Returns object handle to mesh ral (mby_mesh_reg_blk)
   //---------------------------------------------------------------------------
   function mby_mesh_reg_pkg::mby_mesh_reg_blk get_tb_ral();
      return tb_ral;
   endfunction : get_tb_ral
   
   //---------------------------------------------------------------------------
   // Function: set_tb_ral()
   // Sets handle to mesh ral (mby_mesh_reg_blk). Used to pass handle to RAL from fullchip env.
   //---------------------------------------------------------------------------
   function set_tb_ral(mby_mesh_reg_pkg::mby_mesh_reg_blk ral);
      tb_ral = ral;
   endfunction : set_tb_ral

endclass

`endif // __MBY_MESH_ENV_GUARD

