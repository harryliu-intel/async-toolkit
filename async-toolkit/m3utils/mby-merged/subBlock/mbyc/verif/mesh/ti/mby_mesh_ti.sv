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
//  Author        : Dhivya Sankar
//  Project       : Madison Bay
//  Description   : Mplex Test Island.This module will hold all the shared TB content between the IP and
//                  the integration level.
//------------------------------------------------------------------------------

module mby_mesh_ti #(parameter string   RTL_TOP_PATH = "",             // The RTL path to the top level EC IP RTL Block
                     parameter string TB_ENV_PATH = "uvm_test_top.env"          // The hierarchy path to the environment class
                    )
   (
       mby_mesh_tb_if     mby_mesh_tb_if,
       mby_mgp_rreq_if    rreq_eb_if,
       mby_mgp_rreq_if    rreq_wb_if,
       mby_mgp_wreq_if    wreq_eb_if,
       mby_mgp_wreq_if    wreq_wb_if,
       mby_mgp_rsp_if     rsp_eb_if,
       mby_mgp_rsp_if     rsp_wb_if,
       mby_mgp_rreq_if    rreq_sb_if,
       mby_mgp_rreq_if    rreq_nb_if,
       mby_mgp_wreq_if    wreq_sb_if,
       mby_mgp_wreq_if    wreq_nb_if,
       mby_mgp_rsp_if     rsp_sb_if,
       mby_mgp_rsp_if     rsp_nb_if,
       mby_mgp_data_if    wrdata_eb_if,
       mby_mgp_data_if    wrdata_wb_if,
       mby_mgp_data_if    wrdata_sb_if,
       mby_mgp_data_if    wrdata_nb_if,
       mby_mgp_data_if    rddata_eb_if,
       mby_mgp_data_if    rddata_wb_if,
       mby_mgp_data_if    rddata_sb_if,
       mby_mgp_data_if    rddata_nb_if
   );

   import uvm_pkg::*;
   initial begin

      // Set MESH TI Path in the database
      uvm_config_db#(string)::set(null, TB_ENV_PATH, "TI_PATH", $sformatf("%m"));

      // Set MESH RTL_TOP Path in the database
      uvm_config_db#(string)::set(null, TB_ENV_PATH, "RTL_TOP_PATH", RTL_TOP_PATH);

      // Set the MESH_TB_IF in the database
      uvm_config_db#(virtual mby_mesh_tb_if)::set(uvm_root::get(), TB_ENV_PATH , "mby_mesh_tb_if", mby_mesh_tb_if);
      
      // Set the Mesh Req_IF in the database
      uvm_config_db#(virtual mby_mgp_rreq_if)::set(uvm_root::get(), TB_ENV_PATH , "rd_eb", rreq_eb_if);
      uvm_config_db#(virtual mby_mgp_rreq_if)::set(uvm_root::get(), TB_ENV_PATH , "rd_wb", rreq_wb_if);
      uvm_config_db#(virtual mby_mgp_wreq_if)::set(uvm_root::get(), TB_ENV_PATH , "wr_eb", wreq_eb_if);
      uvm_config_db#(virtual mby_mgp_wreq_if)::set(uvm_root::get(), TB_ENV_PATH , "wr_wb", wreq_wb_if);
      uvm_config_db#(virtual mby_mgp_rsp_if)::set(uvm_root::get(), TB_ENV_PATH , "rp_eb", rsp_eb_if);
      uvm_config_db#(virtual mby_mgp_rsp_if)::set(uvm_root::get(), TB_ENV_PATH , "rp_wb", rsp_wb_if);

      uvm_config_db#(virtual mby_mgp_rreq_if)::set(uvm_root::get(), TB_ENV_PATH , "rd_sb", rreq_sb_if);
      uvm_config_db#(virtual mby_mgp_rreq_if)::set(uvm_root::get(), TB_ENV_PATH , "rd_nb", rreq_nb_if);
      uvm_config_db#(virtual mby_mgp_wreq_if)::set(uvm_root::get(), TB_ENV_PATH , "wr_sb", wreq_sb_if);
      uvm_config_db#(virtual mby_mgp_wreq_if)::set(uvm_root::get(), TB_ENV_PATH , "wr_nb", wreq_nb_if);
      uvm_config_db#(virtual mby_mgp_rsp_if)::set(uvm_root::get(), TB_ENV_PATH , "rp_sb", rsp_sb_if);
      uvm_config_db#(virtual mby_mgp_rsp_if)::set(uvm_root::get(), TB_ENV_PATH , "rp_nb", rsp_nb_if);

       uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "wrdata_eb", wrdata_eb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "wrdata_wb", wrdata_wb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "wrdata_sb", wrdata_sb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "wrdata_nb", wrdata_nb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "rddata_eb", rddata_eb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "rddata_wb", rddata_wb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "rddata_sb", rddata_sb_if);
      uvm_config_db#(virtual mby_mgp_data_if)::set(uvm_root::get(), TB_ENV_PATH , "rddata_nb", rddata_nb_if);
   end

endmodule
