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
//=======================================================================
// COPYRIGHT (C)  2012 SYNOPSYS INC.
// This software and the associated documentation are confidential and
// proprietary to Synopsys, Inc. Your use or disclosure of this software
// is subject to the terms and conditions of a written license agreement
// between you, or your company, and Synopsys, Inc. In the event of
// publications, the following notice is applicable:
//
// ALL RIGHTS RESERVED
//
// The entire notice above must be reproduced on all authorized copies.
//
//------------------------------------------------------------------------------
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// class : ahb_slave_random_response_sequence
// Abstract:
// class ahb_slave_random_response_sequence defines a sequence class that the
// testbench uses to provide slave response to the Slave agent present in the
// System agent. The sequence receives a response object of type
// svt_ahb_slave_transaction, from slave sequencer. The sequence class then
// randomizes the response with constraints and provides it to the slave driver
// within the slave agent.
//------------------------------------------------------------------------------

`ifndef GUARD_AHB_SLAVE_RANDOM_RESPONSE_SEQUENCE_SV
`define GUARD_AHB_SLAVE_RANDOM_RESPONSE_SEQUENCE_SV

class ahb_slave_random_response_sequence extends svt_ahb_slave_transaction_base_sequence;

  svt_ahb_slave_transaction resp_req;

  /** UVM Object Utility macro */
  `uvm_object_utils(ahb_slave_random_response_sequence)

  /** Class Constructor */
  function new(string name="ahb_slave_random_response_sequence");
    super.new(name);
  endfunction

  virtual task body();
    `uvm_info("body", "Entered ...", UVM_LOW)

    forever begin
      p_sequencer.response_request_port.peek(resp_req);

      $cast(req,resp_req);

      /** 
       * Demonstration of response randomization with constraints.
       */
      `uvm_rand_send_with(req,
         {
           response_type inside { svt_ahb_transaction::ERROR, svt_ahb_transaction::OKAY };
         })

    end

    `uvm_info("body", "Exiting...", UVM_LOW)
  endtask: body

endclass: ahb_slave_random_response_sequence

`endif // GUARD_AHB_SLAVE_RANDOM_RESPONSE_SEQUENCE_SV
