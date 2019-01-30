//-----------------------------------------------------------------------------
// Title         : Madison Bay PBR Interface
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_pbr_bfm_dptr_master_if.sv
// Author        : ricardo.a.alfaro.gomez  <raalfaro@ichips.intel.com>
// 2ry contact   : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 12.19.2018
//-----------------------------------------------------------------------------
// Description :
// Madison Bay PBR mesh interface file
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
`ifndef __MBY_PBR_BFM_DPTR_MASTER_IF__
`define __MBY_PBR_BFM_DPTR_MASTER_IF__
//------------------------------------------------------------------------------
// INTERFACE: mby_pbr_bfm_dptr_master_if
//
// NYI (Not yet implemented):
// This is the interface to connect the pbr model to the IGR/EGR RTL.
//------------------------------------------------------------------------------
interface mby_pbr_bfm_dptr_master_if(input logic clk, input logic rst);
   import mby_pbr_bfm_pkg_pre::*;

   mby_pbr_bfm_dptr_data_t intf_data_pkt;
   logic                   intf_debg_pkt;

//FIXME: Error-[ICTA] Incompatible complex type
   localparam DATA_WIDTH = $bits(mby_pbr_bfm_dptr_data_t);
   localparam DEBG_WIDTH = 1;
   localparam DLAY_WIDTH = 32;

   //---------------------------------------------------------------------------
   // Initializing the interface at time 0
   //---------------------------------------------------------------------------
   initial begin : initialize_intf
      intf_data_pkt.pod_put_req <= 0;
   end

   //---------------------------------------------------------------------------
   // TASK: drive_data
   //
   // This task is called by a driver, it implements the necessary protocol to
   // put valid data on the bus.
   //
   // ARGUMENTS:
   //    logic [DATA_WIDTH-1:0] data - The data packet to be driven, this is
   //       usually a struct that contains all the fields of the transaction item.
   //    logic [DEBG_WIDTH-1:0] debg_pkt - The debug information (if any is
   //       needed, can be passed using this argument. This information is not
   //       part of the actual bus protocol, but extra debug info that can be
   //       used as part of the verification strategy).
   //    logic [DLAY_WIDTH-1:0] delay    - The transaction item delay property.
   //
   //---------------------------------------------------------------------------
   task drive_data(logic [DATA_WIDTH-1:0] data,
         logic [DEBG_WIDTH-1:0] debg_pkt,
         logic [DLAY_WIDTH-1:0] delay);
      mby_pbr_bfm_dptr_data_t cast_data_pkt;
      
      if(!$cast(cast_data_pkt,data)) begin
         $display("DBG_ALF: Could not cast cast_data_pkt");
      end
      @(posedge clk);
//      if(intf_data_pkt.pod_put_req === 1 && cast_data_pkt.pod_put_ack === 1) begin //send ack after req
//         $display(". . . . . . . %g DBG_ALF: drive_data::dptr ack after req",$time);
//         intf_data_pkt <= data;
//         intf_debg_pkt <= debg_pkt;
//      end
//      else if(cast_data_pkt.pod_put_req === 1) begin //send req
//         $display(". . . . . . . %g DBG_ALF: drive_data::dptr send req wait stall",$time);
//         $display(". . . . . . . %g DBG_ALF: drive_data:: schedule_stall:0x%0x data_dirty_ptr:0x%0x BEFORE",$time,intf_data_pkt.schedule_stall, cast_data_pkt.data_dirty_ptr);
//         wait (intf_data_pkt.schedule_stall === 0);
//         $display(". . . . . . . %g DBG_ALF: drive_data::dptr back req wait stall",$time);
//         @(posedge clk);
//         @(posedge clk);
//         $display(". . . . . . . %g DBG_ALF: drive_data::dptr clkafter req wait stall",$time);
//         $display(". . . . . . . %g DBG_ALF: drive_data:: schedule_stall:0x%0x data_dirty_ptr:0x%0x AFTER",$time,intf_data_pkt.schedule_stall, cast_data_pkt.data_dirty_ptr);
//         intf_data_pkt <= cast_data_pkt;
//         intf_debg_pkt <= debg_pkt;
//      end else begin
         $display(". . . . . . . %g DBG_ALF: master drive_data::dptr ELSE condition",$time);
         intf_data_pkt.pod_put_req <= cast_data_pkt.pod_put_req;
         //intf_debg_pkt <= debg_pkt;
//      end

   // wait (stall === 0)
   // req = 1
   // bus <= data
   // wait ack === 1
   // req = 0
   // exit


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
      @(posedge clk);
      // wait for valid signal
      wait(intf_data_pkt.pod_put_req === 1 && clk == 0);
   endtask

   //---------------------------------------------------------------------------
   // TASK: mon_data
   //
   // This is the main monitor task, it captures the data out of the interface
   // based on a specific protocol.
   //
   // ARGUMENTS:
   //    output logic [DATA_WIDTH-1:0] data - The data packet captured at
   //       the interface. It is a struct that contains all the fields.
   //    output logic [DEBG_WIDTH-1:0] debg_pkt - The debug information (if
   //       any is needed, can be obtained from this argument. This
   //       information is not part of the actual bus protocol, but extra
   //       debug info that can be used as part of the verification strategy).
   //
   //---------------------------------------------------------------------------
   task mon_data(output logic [DATA_WIDTH-1:0] data,
         output logic [DEBG_WIDTH-1:0] debg_pkt);
      data = intf_data_pkt;
      debg_pkt = intf_debg_pkt;
   endtask

endinterface : mby_pbr_bfm_dptr_master_if

`endif
