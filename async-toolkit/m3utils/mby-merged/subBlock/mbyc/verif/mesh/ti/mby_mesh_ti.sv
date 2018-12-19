
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


module mby_mesh_ti #( parameter string   RTL_TOP_PATH = "",             // The RTL path to the top level EC IP RTL Block
        parameter string TB_ENV_PATH = "uvm_test_top.env"          // The hierarchy path to the environment class
    )
    (
        mby_mesh_tb_if     mby_mesh_tb_if,
        shdv_base_tb_intf  shdv_intf,
        //MGP --> Msh write op
        mby_mgp_mim_op_if    mby_mgp_mim_wr_op_wb_if,
        mby_mgp_mim_op_if    mby_mgp_mim_wr_op_eb_if,
        //MGP --> Msh Rd op
        mby_mgp_mim_op_if    mby_mgp_mim_rd_op_wb_if,
        mby_mgp_mim_op_if    mby_mgp_mim_rd_op_eb_if,
        //MGP --> Msh Rsp op
        mby_mgp_mim_op_if    mby_mgp_mim_rsp_op_wb_if,
        mby_mgp_mim_op_if    mby_mgp_mim_rsp_op_eb_if,
        //MGP --> Msh write data
        mby_mgp_mim_data_if  mby_mgp_mim_wr_data_wb_if,
        mby_mgp_mim_data_if  mby_mgp_mim_wr_data_eb_if,
        //MGP --> Msh Rsp data
        mby_mgp_mim_data_if  mby_mgp_mim_rsp_data_wb_if,
        mby_mgp_mim_data_if  mby_mgp_mim_rsp_data_eb_if,
        //MIG --> Msh write op
        mby_gmm_mig_op_if    mby_gmm_mig_wr_op_sb_if_0,
        mby_gmm_mig_op_if    mby_gmm_mig_wr_op_sb_if_1,
        mby_gmm_mig_op_if    mby_gmm_mig_wr_op_sb_if_2,
        mby_gmm_mig_op_if    mby_gmm_mig_wr_op_nb_if_0,
        //MIG --> Msh read op
        mby_gmm_mig_op_if    mby_gmm_mig_rd_op_sb_if_0,
        mby_gmm_mig_op_if    mby_gmm_mig_rd_op_sb_if_1,
        mby_gmm_mig_op_if    mby_gmm_mig_rd_op_sb_if_2,
        mby_gmm_mig_op_if    mby_gmm_mig_rd_op_sb_if_3,
        mby_gmm_mig_op_if    mby_gmm_mig_rd_op_nb_if_0,
        //MIG --> Msh res op
        mby_gmm_mig_op_if    mby_gmm_mig_rsp_op_sb_if_0,
        mby_gmm_mig_op_if    mby_gmm_mig_rsp_op_sb_if_1,
        mby_gmm_mig_op_if    mby_gmm_mig_rsp_op_sb_if_2,
        mby_gmm_mig_op_if    mby_gmm_mig_rsp_op_sb_if_3,
        mby_gmm_mig_op_if    mby_gmm_mig_rsp_op_nb_if_0,
        //MIG --> Msh write data
        mby_gmm_mig_data_if    mby_gmm_mig_wr_data_sb_if_0,
        mby_gmm_mig_data_if    mby_gmm_mig_wr_data_sb_if_1,
        mby_gmm_mig_data_if    mby_gmm_mig_wr_data_sb_if_2,
        mby_gmm_mig_data_if    mby_gmm_mig_wr_data_nb_if_0,
        //MIG --> Msh res data
        mby_gmm_mig_data_if    mby_gmm_mig_rsp_data_sb_if_0,
        mby_gmm_mig_data_if    mby_gmm_mig_rsp_data_sb_if_1,
        mby_gmm_mig_data_if    mby_gmm_mig_rsp_data_sb_if_2,
        mby_gmm_mig_data_if    mby_gmm_mig_rsp_data_sb_if_3,
        mby_gmm_mig_data_if    mby_gmm_mig_rsp_data_nb_if_0
    );

    import uvm_pkg::*;
    initial begin

        // Set MESH TI Path in the database
        uvm_config_db#(string)::set(null, TB_ENV_PATH, "TI_PATH", $sformatf("%m"));

        // Set MESH RTL_TOP Path in the database
        uvm_config_db#(string)::set(null, TB_ENV_PATH, "RTL_TOP_PATH", RTL_TOP_PATH);

        // Set the MESH_TB_IF in the database
        uvm_config_db#(virtual mby_mesh_tb_if)::set(uvm_root::get(), TB_ENV_PATH , "mby_mesh_tb_if", mby_mesh_tb_if);

        uvm_config_db#(virtual mby_mgp_mim_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_wr_op_wb_if" , mby_mgp_mim_wr_op_wb_if);
        uvm_config_db#(virtual mby_mgp_mim_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_wr_op_eb_if" , mby_mgp_mim_wr_op_eb_if);

        uvm_config_db#(virtual mby_mgp_mim_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_rd_op_wb_if" , mby_mgp_mim_rd_op_wb_if);
        uvm_config_db#(virtual mby_mgp_mim_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_rd_op_eb_if" , mby_mgp_mim_rd_op_eb_if);

        uvm_config_db#(virtual mby_mgp_mim_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_rsp_op_wb_if" , mby_mgp_mim_rsp_op_wb_if);
        uvm_config_db#(virtual mby_mgp_mim_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_rsp_op_eb_if" , mby_mgp_mim_rsp_op_eb_if);

        uvm_config_db#(virtual mby_mgp_mim_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_wr_data_wb_if" , mby_mgp_mim_wr_data_wb_if);
        uvm_config_db#(virtual mby_mgp_mim_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_wr_data_eb_if" , mby_mgp_mim_wr_data_eb_if);

        uvm_config_db#(virtual mby_mgp_mim_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_rsp_data_wb_if" , mby_mgp_mim_rsp_data_wb_if);
        uvm_config_db#(virtual mby_mgp_mim_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_mgp_mim_rsp_data_eb_if" , mby_mgp_mim_rsp_data_eb_if);

        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_op_sb_if_0" , mby_gmm_mig_wr_op_sb_if_0);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_op_sb_if_1" , mby_gmm_mig_wr_op_sb_if_1);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_op_sb_if_2" , mby_gmm_mig_wr_op_sb_if_2);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_op_nb_if_0" , mby_gmm_mig_wr_op_nb_if_0);

        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rd_op_sb_if_0" , mby_gmm_mig_rd_op_sb_if_0);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rd_op_sb_if_1" , mby_gmm_mig_rd_op_sb_if_1);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rd_op_sb_if_2" , mby_gmm_mig_rd_op_sb_if_2);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rd_op_sb_if_3" , mby_gmm_mig_rd_op_sb_if_3);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rd_op_nb_if_0" , mby_gmm_mig_rd_op_nb_if_0);

        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_op_sb_if_0" , mby_gmm_mig_rsp_op_sb_if_0);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_op_sb_if_1" , mby_gmm_mig_rsp_op_sb_if_1);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_op_sb_if_2" , mby_gmm_mig_rsp_op_sb_if_2);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_op_sb_if_3" , mby_gmm_mig_rsp_op_sb_if_3);
        uvm_config_db#(virtual mby_gmm_mig_op_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_op_nb_if_0" , mby_gmm_mig_rsp_op_nb_if_0);

        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_data_sb_if_0" , mby_gmm_mig_wr_data_sb_if_0);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_data_sb_if_1" , mby_gmm_mig_wr_data_sb_if_1);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_data_sb_if_2" , mby_gmm_mig_wr_data_sb_if_2);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_wr_data_nb_if_0" , mby_gmm_mig_wr_data_nb_if_0);

        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_data_sb_if_0" , mby_gmm_mig_rsp_data_sb_if_0);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_data_sb_if_1" , mby_gmm_mig_rsp_data_sb_if_1);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_data_sb_if_2" , mby_gmm_mig_rsp_data_sb_if_2);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_data_sb_if_3" , mby_gmm_mig_rsp_data_sb_if_3);
        uvm_config_db#(virtual mby_gmm_mig_data_if)::set(uvm_root::get(),TB_ENV_PATH , "mby_gmm_mig_rsp_data_nb_if_0" , mby_gmm_mig_rsp_data_nb_if_0);

    end

endmodule
