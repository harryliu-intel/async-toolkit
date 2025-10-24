// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
//   Author        : Nathan Mai
//                 : Lewis Sternberg
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//
//Class : mby_rx_ppe_env
//This is the MBY RX_PPE Testbench Environment file which is extended from shdv_base_env.
//

`ifndef __MBY_RX_PPE_ENV_GUARD
`define __MBY_RX_PPE_ENV_GUARD

`ifndef __INSIDE_MBY_RX_PPE_ENV_PKG
`error "Attempt to include file outside of mby_rx_ppe_env_pkg."
`endif


class mby_rx_ppe_env extends shdv_base_env;

   // Variable:  tb_cfg
   // Protected Top Level rx_ppe environment configuration.
   protected mby_rx_ppe_tb_top_cfg                             tb_cfg;

   // Variable:  tb_vif
   // Interface handle to rx_ppe Testbench.
   //FIXME: LNS: to be made into an array for each inter-block interface
   virtual   mby_rx_ppe_tb_if                                  tb_vif;
   
   // Variable:  tb_ral
   // Handle to RX PPE RAL.
   mby_rx_ppe_reg_pkg::mby_rx_ppe_reg_blk                      tb_ral;

   // Variable:  eth_bfms
   // MAC Client BFM agent
   rx_ppe_eth_bfm_t                                            eth_bfm; //ned 1 bf for both LPP & FPP

   // Variable:  eth_bfm_rx_vintf
   // MAC Client BFM virtual interface
   rx_ppe_eth_bfm_rx_intf_t                                    eth_bfm_rx_vintf;//[`NUM_EPLS_PER_RX_PPE]; //just 1 EPL for RX_PPE env

   //TODO: Add IO policy for INTF0 & INTF1 
   //eth_bfm parser io

   // Variable:  rx_ppe_sb
   // array of scoreboards -- one per block
   // declare as the base class (which is not mby_rx_ppe_sb, by the way), and cast as necessary
   // TODO: use shdv_scoreboard when available
   uvm_scoreboard                                             scoreboards[];

   `uvm_component_utils_begin(mby_rx_ppe_env)
      `uvm_field_object  (tb_cfg,                          UVM_ALL_ON)
   `uvm_component_utils_end

   //---------------------------------------------------------------------------
   //  Constructor: new
   //
   //  Arguments:
   //      string name - MBY Rx_ppe environment object name.
   //      uvm_component parent - Component parent object.
   //---------------------------------------------------------------------------
   function new(string name = "mby_rx_ppe_env", uvm_component parent = null);
      super.new(name, parent);
   endfunction : new

   //---------------------------------------------------------------------------
   //  Function: build_phase
   //  Create the agents and scoreboards
   //
   //  Arguments:
   //      phase - uvm_phase object.
   //---------------------------------------------------------------------------
   virtual function void build_phase(uvm_phase phase);
      uvm_object tmp_cfg;

      scoreboards = new[mby_rx_ppe_topology_e_num];

      super.build_phase(phase);

      if(get_config_object("mby_rx_ppe_tb_top_cfg", tmp_cfg)) begin
         if (!$cast(tb_cfg, tmp_cfg)) begin
            `uvm_fatal(get_name(), "build_phase(): cast to tb_cfg failed")
         end
      end

      if (tb_cfg == null) begin
         `uvm_fatal(get_full_name(), "Unable to acquire handle to mby_rx_ppe_tb_top_cfg object")
      end

      `uvm_info (get_full_name , $sformatf("rx_ppe Top _cfg : %s", tb_cfg.sprint()), UVM_FULL)

      if(!uvm_config_db#(string)::get(this, "" , "TI_PATH", tb_cfg.ti_path)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's TI_PATH was not successful")
      end
      `uvm_info(get_full_name(),$sformatf("This rx_ppe Build Phase set tb_cfg.ti_path = %s", tb_cfg.ti_path),UVM_FULL)

      if(!uvm_config_db#(string)::get(this, "" , "RTL_TOP_PATH", tb_cfg.rtl_top_path)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's RTL_TOP_PATH was not successful")
      end
      `uvm_info(get_full_name(),$sformatf("This rx_ppe Build Phase set tb_cfg.rtl_top_path = %s", tb_cfg.rtl_top_path),UVM_FULL)

// FIXME LNS: if tb_cfg is an object in the env, then why are we going through the uvm_config_db to access its members?
//      if(!uvm_config_db#(int)::get(this, "", "TOPOLOGY", tb_cfg.topology)) begin
//         `uvm_fatal(get_name(),$sformatf("Unable to acquire valid topology value"))
//      end
//      `uvm_info(get_full_name(),$sformatf("This rx_ppe Build Phase set tb_cfg.topology = %s", tb_cfg.topology),UVM_FULL)

      if(!uvm_config_db#(virtual mby_rx_ppe_tb_if)::get(this, "", "mby_rx_ppe_tb_if", tb_vif)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's TB_IF was not successful")
      end

      if(!uvm_config_db#(rx_ppe_eth_bfm_rx_intf_t)::get(this, "", "eth_bfm_rx_vintf", eth_bfm_rx_vintf)) begin
         `uvm_fatal(get_name(),"Config_DB.get() for ENV's cdi_rx_vintf was not successful")
      end

      //AK :TODO: uncomment after register base address is defined correctly. 
      //build_ral();
      build_eth_bfm();

      foreach (scoreboards[ii]) begin
         if(tb_cfg.scoreboards[ii] != 0) begin
            case(ii)
               PARSER            : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_parser_obj)          ::type_id::create("rx_ppe_sb_parser"         , this);
               MAPPER            : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_mapper_obj)          ::type_id::create("rx_ppe_sb_mapper"         , this);
               CLASSIFIER        : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_classifier_obj)      ::type_id::create("rx_ppe_sb_classifier"     , this);
               HASH              : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_hash_obj)            ::type_id::create("rx_ppe_sb_hash"           , this);
               NEXT_HOP          : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_next_hop_obj)        ::type_id::create("rx_ppe_sb_next_hop"       , this);
               MASK_GEN          : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_mask_gen_obj)        ::type_id::create("rx_ppe_sb_mask_gen"       , this);
               TRIGGERS          : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_triggers_obj)        ::type_id::create("rx_ppe_sb_triggers"       , this);
               CONGESTION_MGT    : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_congestion_mgt_obj)  ::type_id::create("rx_ppe_sb_congestion_mgt" , this);
               RX_STATS          : scoreboards[ii] = mby_rx_ppe_sb#(mby_rx_ppe_rx_stats_obj)        ::type_id::create("rx_ppe_sb_rx_stats"       , this);
               default : `uvm_fatal(get_name(), $sformatf("Unexpected scoreboard index: %0d", ii))
            endcase
         end
      end

   endfunction: build_phase

   //---------------------------------------------------------------------------
   //  Function: build_eth_bfm
   //  Build and configure Eth_bfm.
   //---------------------------------------------------------------------------
   function void build_eth_bfm();

      // Create the bfm instances
      eth_bfm                   =  rx_ppe_eth_bfm_t::type_id::create("rx_ppe_eth_bfm", this);
      eth_bfm.cfg.mode          =  eth_bfm_pkg::MODE_SLAVE;                            // Configure as SLAVE
      eth_bfm.cfg.port_speed    =  {eth_bfm_pkg::SPEED_400G,                           // Configure speed.
                                    eth_bfm_pkg::SPEED_OFF ,
                                    eth_bfm_pkg::SPEED_OFF ,
                                    eth_bfm_pkg::SPEED_OFF };
      //eth_bfms.cfg.port_lanes    = {4,0,0,0};                                        // Configure num_ports.
      eth_bfm.cfg.group_size    = 8;
      eth_bfm.cfg.sop_alignment = 8;

   endfunction : build_eth_bfm

   //---------------------------------------------------------------------------
   //  Function: build_ral
   //  Builds Rx_PPE register model.
   //
   //---------------------------------------------------------------------------
   virtual function void build_ral();
      
      //TODO: AK- Check if this is OK.
      // UVM registers are generated with coverage disabled. When uvm reg is built it looks up for
      //"include_coverage" in the uvm_config_db and throws a warning that it cant find that variable for
      // every single register. Setting the "include_coverage" variable to 0 to avoid the warning.    
      uvm_config_db#(int)::set(null, "uvm_test_top.env.tb_ral", "include_coverage", 0);

      // Check if ral is already set by FC
      if (tb_ral == null) begin
         tb_ral = mby_rx_ppe_reg_pkg::mby_rx_ppe_reg_blk::type_id::create("tb_ral");
         tb_ral.build();
         //TODO: Update register map base address.
         tb_ral.default_map.set_base_addr(`UVM_REG_ADDR_WIDTH'h4000);
         tb_ral.lock_model();
                
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

      //eth_cdi_rx_io.set_vintf(eth_bfm_rx_vintf);
      //eth_cdi_bfm.set_io(eth_cdi_tx_io, eth_cdi_rx_io);   // Set the IO Policy in the CDI BFM
   endfunction: connect_phase

   //---------------------------------------------------------------------------
   //  Function: end_of_elaboration_phase
   //FIXME: LNS: if the super.end_of_elaboration_phase() does these things, make that clear in the following line.  If it does not, then delete the following line or insert a TODO to make it so.
   //  Randomizes the RAL objects.  Prints BFM configurations.
   //
   //  Arguments:
   //  phase - uvm_phase object.
   //----------------------------------------------------------------------------
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
   // Returns a handle to rx_ppe testbench virtual interface.
   //---------------------------------------------------------------------------
   function virtual mby_rx_ppe_tb_if get_tb_vif();
      return tb_vif;
   endfunction:get_tb_vif

   //---------------------------------------------------------------------------
   // Function: get_tb_cfg()
   // Returns object handle to rx_ppe env configuration (mby_rx_ppe_tb_top_cfg)
   //---------------------------------------------------------------------------
   function mby_rx_ppe_tb_top_cfg get_tb_cfg();
      return tb_cfg;
   endfunction : get_tb_cfg

   //---------------------------------------------------------------------------
   // Function: get_tb_ral()
   // Returns object handle to rx_ppe RAL  (mby_rx_ppe_reg_blk)
   //---------------------------------------------------------------------------
   function mby_rx_ppe_reg_pkg::mby_rx_ppe_reg_blk get_tb_ral();
      return tb_ral;
   endfunction : get_tb_ral

   //---------------------------------------------------------------------------
   // Function: set_tb_ral()
   // Sets handle to rx_ppe ral (mby_rx_ppe_reg_blk). Used to pass handle to RAL from fullchip env.
   //---------------------------------------------------------------------------
   function set_tb_ral(mby_rx_ppe_reg_pkg::mby_rx_ppe_reg_blk ral);
      tb_ral = ral;
   endfunction : set_tb_ral


endclass

`endif // __MBY_RX_PPE_ENV_GUARD

