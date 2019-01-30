//-----------------------------------------------------------------------------
// Title         : Madison Bay SMM Interface
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_smm_bfm_mrd_req_if.sv
// Author        : Roman Bernal <r.bernal@intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// Madison Bay SMM interface file for Memory Read & Response Requests
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
`ifndef __MBY_SMM_BFM_MRD_REQ_IF__
`define __MBY_SMM_BFM_MRD_REQ_IF__

interface mby_smm_bfm_mrd_req_if(input logic clk, input logic rst);
   import mby_smm_bfm_pkg_pre::*;

   // VARIABLE:
   //    Packet interface used to drive read responses data.
   mby_smm_bfm_row_rd_req_t intf_data_pkt;
   
   // VARIABLE:
   //    TODO : not used within the class, any usage intended?
   logic                    intf_val_pkt;
   
   // VARIABLE:
   //    Packet interface used to flag? debug data. TODO : confirm this
   logic                    intf_debg_pkt;

   localparam DATA_WIDTH = $bits(mby_smm_bfm_row_rd_req_t);
   localparam DEBG_WIDTH = 1;
   
   //---------------------------------------------------------------------------
   // Initializing the interface at time 0
   //---------------------------------------------------------------------------
   initial begin : initialize_intf
      //intf_data_pkt = 0;
      forever begin 
         @(posedge clk);
         intf_data_pkt.mim_rrsp_valid = 'z;
         intf_data_pkt.mim_rrsp_dest_block = 'z;
         intf_data_pkt.mim_rrsp_req_id =  'z;
         intf_data_pkt.mim_rd_data = 'z;
      end
   end

   //---------------------------------------------------------------------------
   // TASK: drive_data
   //
   // This task is called by a driver, it implements the necessary protocol to
   // put valid data on the bus.
   //
   // ARGUMENTS:
   //    mby_smm_bfm_row_rd_req_t data_pkt - The data packet to be driven, this is
   //       a struct that contains all the fields of the transaction item.
   //    logic [DEBG_WIDTH-1:0] debug_pkt - The debug information (if any is
   //       needed, can be passed using this argument. This information is not
   //       part of the actual bus protocol, but extra debug info that can be
   //       used as part of the verification strategy).
   //
   //---------------------------------------------------------------------------
   task drive_data(mby_smm_bfm_row_rd_req_t data_pkt, logic [DEBG_WIDTH-1:0] debug_pkt, logic delay);
      @(posedge clk);
      //Driving MRd Response
      intf_data_pkt.mim_rrsp_valid = data_pkt.mim_rrsp_valid;
      intf_data_pkt.mim_rrsp_dest_block = data_pkt.mim_rrsp_dest_block;
      intf_data_pkt.mim_rrsp_req_id =  data_pkt.mim_rrsp_req_id;
      intf_data_pkt.mim_rd_data = data_pkt.mim_rd_data;
      intf_debg_pkt = debug_pkt;
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
      wait(intf_data_pkt.mim_rreq_valid === 1);
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
   //    output logic [DEBG_WIDTH-1:0] debug_pkt - The debug information (if
   //       any is needed, can be obtained from this argument. This
   //       information is not part of the actual bus protocol, but extra
   //       debug info that can be used as part of the verification strategy).
   //
   //---------------------------------------------------------------------------
   task mon_data(output logic [DATA_WIDTH-1:0] data_pkt, output logic [DEBG_WIDTH-1:0] debug_pkt);
      data_pkt = intf_data_pkt;
      debug_pkt = intf_debg_pkt;
       @(posedge clk);
   endtask


   task hard_reset();
      $display("Executing hard reset in the interface.");
   endtask

   task soft_reset();
      $display("Executing soft reset in the interface.");
   endtask
endinterface : mby_smm_bfm_mrd_req_if



`endif

