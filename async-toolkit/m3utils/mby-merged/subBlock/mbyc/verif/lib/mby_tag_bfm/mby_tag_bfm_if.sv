//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag Interface
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_if.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// Madison Bay Tag interface file
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
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
`ifndef __MBY_TAG_BFM_IF__
`define __MBY_TAG_BFM_IF__

interface mby_tag_bfm_if(input logic clk, input logic rst);
   import mby_egr_pkg::*;

   lltformat_t intf_data_pkt;
   logic       debg_data_pkt;

   localparam DATA_WIDTH = $bits(lltformat_t);
   localparam DEBG_WIDTH = 1;

   initial begin : initialize_intf
      intf_data_pkt <= 0;
   end

   task drive_data(logic [DATA_WIDTH-1:0] data_pkt, logic[DEBG_WIDTH-1:0] debg_pkt);
      @(posedge clk);
      intf_data_pkt = data_pkt;
      debg_data_pkt = debg_pkt;
   endtask

   task mon_start();
      // wait for valid signal
      // wait(intf_data_pkt.tc !== 0);
   endtask

   task mon_data(output logic [DATA_WIDTH-1:0] data_pkt, output logic[DEBG_WIDTH-1:0] debg_pkt);
      data_pkt = intf_data_pkt;
      debg_pkt = debg_data_pkt;
   endtask

endinterface : mby_tag_bfm_if

`endif

