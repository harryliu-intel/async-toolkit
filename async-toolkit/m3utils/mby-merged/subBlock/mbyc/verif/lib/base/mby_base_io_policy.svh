//-----------------------------------------------------------------------------
// Title         : Madison Bay Base I/O Policy Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_io_policy.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 29.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base i/o policy abstract class for Madison Bay
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
//-----------------------------------------------------------------------------
`ifndef __MBY_BASE_PKG__
`error "Attempt to include file outside of mby_base_pkg."
`endif
`ifndef __MBY_BASE_IO_POLICY__
`define __MBY_BASE_IO_POLICY__
//-----------------------------------------------------------------------------
// CLASS: mby_base_io_policy
//
// This is an abstract parameterized class used by mby_base_agent. It needs to
// be extended by the user to implement the necessary 'interface' drive and
// monitor tasks. Interface can be an actual system verilog interface or any
// other means of communication with other testbench components
//
// PARAMETERS:
//     T_req - sequence item to be used, mby_base_sequence_item by default
//
//-----------------------------------------------------------------------------
virtual class mby_base_io_policy
   #(
      type T_req = mby_base_sequence_item
   ); // TODO: extends uvm_object; // should this be uvm_object ??

   // VARIABLE: name
   // The name of this I/O policy class
   string name;

   // -------------------------------------------------------------------------
   // TASK: drive_data
   //
   // Driver task this is where the actual implementation of the driver happens
   //
   // ARGUMENTS:
   //    T_req item  - This is the item to be driven.
   // -------------------------------------------------------------------------
   pure virtual task drive_data(T_req item);

   //---------------------------------------------------------------------------
   // TASK: mon_start
   //
   // This task is called by a monitor to start the monitor process. This task
   // should block the forever loop inside the monitor code. A new transaction
   // item will be created after this task returns.
   //
   //---------------------------------------------------------------------------
   pure virtual task mon_start();

   //---------------------------------------------------------------------------
   // TASK: mon_data
   //
   // This is the main monitor task, it captures the data out of the interface
   // based on a specific protocol.
   //
   // ARGUMENTS:
   //    output T_req item  - This is the item that was monitored from the
   //                         interface and till be sent to the monitor's
   //                         analysis port.
   //
   //---------------------------------------------------------------------------
   pure virtual task mon_data(output T_req item);

   //---------------------------------------------------------------------------
   // TASK: reset
   //
   // This is the reset task, it gets called by the driver to reset the vintf
   // based on a specific protocol.
   //
   //---------------------------------------------------------------------------
   pure virtual task reset();

endclass : mby_base_io_policy

`endif
