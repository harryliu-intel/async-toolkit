//-----------------------------------------------------------------------------
// Title         : Madison Bay Base Flow Control Policy Class
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_base_flow_control.svh
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 29.10.2018
//-----------------------------------------------------------------------------
// Description :
// Base Flow Control policy abstract class for Madison Bay
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
`ifndef __MBY_BASE_FLOW_CONTROL__
`define __MBY_BASE_FLOW_CONTROL__
//-----------------------------------------------------------------------------
// CLASS: mby_base_flow_control
//
// This is an abstract class used by mby_base_agent. It needs to
// be implemented by the user to do the necessary flow control to drive
// a particular interface
//
//-----------------------------------------------------------------------------
virtual class mby_base_flow_control;

   // -------------------------------------------------------------------------
   // Function: reset
   //
   // Entering reset.
   // -------------------------------------------------------------------------
   pure virtual function void reset();

   // -------------------------------------------------------------------------
   // Function: start
   //
   // Leaving reset and starting.
   // -------------------------------------------------------------------------
   pure virtual function void start();

   // -------------------------------------------------------------------------
   // Function: is_blocked
   //
   // Returns true if a bus cycle can proceed and false to hold the
   // data and have a idle pkt sent instead.
   // -------------------------------------------------------------------------
   pure virtual function bit is_blocked();

   // -------------------------------------------------------------------------
   // Function: return_credit
   //
   // Returns a credit
   // -------------------------------------------------------------------------
   // pure virtual function return_credit();

   // -------------------------------------------------------------------------
   // Function: consume_credit
   //
   // Account for data items sent
   // -------------------------------------------------------------------------
   //pure virtual function void consume_credit();

   // -------------------------------------------------------------------------
   // Function: restore_credit
   //
   // Account for data items received
   // -------------------------------------------------------------------------
   //pure virtual function void restore_credit();

endclass : mby_base_flow_control


//-----------------------------------------------------------------------------
// CLASS: mby_base_empty_flow_control
//
// This is an empty implementation of the base flow control to be used in
// agents where it is not necessary to have an actual flow control
//
//-----------------------------------------------------------------------------
class mby_base_empty_flow_control extends mby_base_flow_control;

   // -------------------------------------------------------------------------
   // Function: reset
   //
   // Entering reset.
   // -------------------------------------------------------------------------
   virtual function void reset();
   endfunction

   // -------------------------------------------------------------------------
   // Function: start
   //
   // Leaving reset and starting.
   // -------------------------------------------------------------------------
   virtual function void start();
   endfunction

   // -------------------------------------------------------------------------
   // Function: is_blocked
   //
   // Returns true if a bus cycle can proceed and false to hold the
   // data and have a idle pkt sent instead.
   // -------------------------------------------------------------------------
   virtual function bit is_blocked();
   endfunction

endclass : mby_base_empty_flow_control



`endif
