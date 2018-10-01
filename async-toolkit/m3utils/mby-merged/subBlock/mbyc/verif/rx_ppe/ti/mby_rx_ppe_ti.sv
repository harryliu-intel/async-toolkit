
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
//  Author        : Akshay Kotian
//  Project       : Madison Bay
//  Description   : rx_ppe Test Island.This module will hold all the shared TB content between the IP and
//                  the integration level.
//------------------------------------------------------------------------------


module mby_rx_ppe_ti #( parameter string   RTL_TOP_PATH = "",             // The RTL path to the top level EC IP RTL Block
        parameter string   TB_ENV_PATH = "uvm_test_top.env",          // The hierarchy path to the environment class
        parameter mby_rx_ppe_env_pkg::mby_rx_ppe_defines::rx_ppe_topology_e TOPOLOGY =  mby_rx_ppe_env_pkg::mby_rx_ppe_defines::RX_PPE_FULL
    )
    (
        mby_rx_ppe_tb_if  mby_rx_ppe_tb_if,

        shdv_base_tb_intf shdv_intf,
        mby_ec_cdi_tx_intf  cdi_tx_intf,
        mby_ec_cdi_rx_intf  cdi_rx_intf
    );

    import uvm_pkg::*;
    import sla_pkg::*;
    import ec_env_pkg::*;

    initial begin
        // Set MC TI Path in the database
        uvm_config_db#(string)::set(null, TB_ENV_PATH, "TI_PATH", $sformatf("%m"));

        // Set MC RTL_TOP Path in the database
        uvm_config_db#(string)::set(null, TB_ENV_PATH, "RTL_TOP_PATH", RTL_TOP_PATH);

        //Set MC TB Topology
        uvm_config_db#(int)::set(null, TB_ENV_PATH, "TOPOLOGY", TOPOLOGY);

        // Set the MC_TB_IF in the database
        uvm_config_db#(virtual mby_rx_ppe_tb_if)::set(uvm_root::get(), TB_ENV_PATH , "mby_rx_ppe_tb_if", mby_rx_ppe_tb_if);

        // Set the SHDV tb_intf in the database
        slu_resource_db#(virtual shdv_base_tb_intf)::add({"env", ".IP_intf"}, shdv_intf, `__FILE__, `__LINE__);

        uvm_config_db#(ec_env_defines::cdi_tx_vintf_t)::set(uvm_root::get(), TB_ENV_PATH, "cdi_tx_vintf" , cdi_tx_intf);
        uvm_config_db#(ec_env_defines::cdi_rx_vintf_t)::set(uvm_root::get(), TB_ENV_PATH, "cdi_rx_vintf" , cdi_rx_intf);


    end

endmodule
