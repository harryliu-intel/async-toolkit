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
//  Author        : Akshay Kotian
//  Project       : Madison Bay
//  Description   : MCE Test Island.This module will hold all the shared TB content between the IP and
//                  the integration level.
//------------------------------------------------------------------------------

module mby_mce_ti #(parameter string   RTL_TOP_PATH = "",             // The RTL path to the top level EC IP RTL Block
      parameter string TB_ENV_PATH = "uvm_test_top.env"          // The hierarchy path to the environment class
   )
   (
      mby_mce_tb_if         mby_mce_tb_if

   );

   import uvm_pkg::*;
   initial begin

      // Set MCE TI Path in the database
      uvm_config_db#(string)::set(null, TB_ENV_PATH, "TI_PATH", $sformatf("%m"));

      // Set MCE RTL_TOP Path in the database
      uvm_config_db#(string)::set(null, TB_ENV_PATH, "RTL_TOP_PATH", RTL_TOP_PATH);

      // Set the MCE_TB_IF in the database
      uvm_config_db#(virtual mby_mce_tb_if)::set(uvm_root::get(), TB_ENV_PATH , "mby_mce_tb_if", mby_mce_tb_if);

   end

endmodule
