//-----------------------------------------------------------------------------
// Title         : Madison Bay GCM Interface
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_gcm_bfm_deque_if.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// Madison Bay GCM BFM interface file
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
`ifndef __MBY_GCM_BFM_DEQUE_IF__
`define __MBY_GCM_BFM_DEQUE_IF__
//------------------------------------------------------------------------------
// INTERFACE: mby_gcm_bfm_deque_if
//
// This is the interface that connects EGR to the GCM
//
//------------------------------------------------------------------------------
interface mby_gcm_bfm_deque_if(input logic clk, input logic rst);
   import mby_gmm_pkg::*;

   mby_unicast_deque_t intf_data_pkt;
   logic               intf_debg_pkt;

   localparam DATA_WIDTH = $bits(mby_unicast_deque_t);
   localparam DEBG_WIDTH = 1;
   localparam DLAY_WIDTH = 32;

   //---------------------------------------------------------------------------
   // Initializing the interface at time 0
   //---------------------------------------------------------------------------
   initial begin : initialize_intf
      intf_data_pkt <= 0;
   end

   //---------------------------------------------------------------------------
   // TASK: drive_data
   //
   // This task is called by a driver, it implements the necessary protocol to
   // put valid data on the bus.
   //
   // ARGUMENTS:
   //    logic [DATA_WIDTH-1:0] data_pkt - The data packet to be driven, this is
   //       usually a struct that contains all the fields of the transaction item.
   //    logic [DEBG_WIDTH-1:0] debg_pkt - The debug information (if any is
   //       needed, can be passed using this argument. This information is not
   //       part of the actual bus protocol, but extra debug info that can be
   //       used as part of the verification strategy).
   //    logic [DLAY_WIDTH-1:0] delay    - The transaction item delay property.
   //
   //---------------------------------------------------------------------------
   task drive_data(logic [DATA_WIDTH-1:0] data_pkt,
                   logic [DEBG_WIDTH-1:0] debg_pkt,
                   logic [DLAY_WIDTH-1:0] delay);
      @(posedge clk);
      intf_data_pkt <= data_pkt;
      intf_debg_pkt <= debg_pkt;
   endtask

   //---------------------------------------------------------------------------
   // TASK: mon_start
   //
   // This task is called by a monitor to start the monitor process. This task
   // should block the forever loop inside the monitor code. A new transaction
   // item will be created after this task returns.
   //
   //---------------------------------------------------------------------------
   task mon_start();
      // wait for valid signal
      //wait(intf_data_pkt.valid === 1);
   endtask

   //---------------------------------------------------------------------------
   // TASK: mon_data
   //
   // This is the main monitor task, it captures the data out of the interface
   // based on a specific protocol.
   //
   // ARGUMENTS:
   //    output logic [DATA_WIDTH-1:0] data_pkt - The data packet captured at
   //       the interface. It is a struct that contains all the fields.
   //    output logic [DEBG_WIDTH-1:0] debg_pkt - The debug information (if
   //       any is needed, can be obtained from this argument. This
   //       information is not part of the actual bus protocol, but extra
   //       debug info that can be used as part of the verification strategy).
   //
   //---------------------------------------------------------------------------
   task mon_data(output logic [DATA_WIDTH-1:0] data_pkt,
                 output logic [DEBG_WIDTH-1:0] debg_pkt);
      data_pkt = intf_data_pkt;
      debg_pkt = intf_debg_pkt;
   endtask

endinterface : mby_gcm_bfm_deque_if

`endif
