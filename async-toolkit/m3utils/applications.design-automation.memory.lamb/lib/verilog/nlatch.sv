//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2021 - 2021 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related
// to the source code ("Material") are owned by Intel Corporation or its
// suppliers or licensors. Title to the Material remains with Intel
// Corporation or its suppliers and licensors. The Material contains trade
// secrets and proprietary and confidential information of Intel or its
// suppliers and licensors. The Material is protected by worldwide copyright
// and trade secret laws and treaty provisions. No part of the Material may
// be used, copied, reproduced, modified, published, uploaded, posted,
// transmitted, distributed, or disclosed in any way without Intel's prior
// express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or
// delivery of the Materials, either expressly, by implication, inducement,
// estoppel or otherwise. Any license under such intellectual property rights
// must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//
// Behavioral model of naked latch
//
// Author : mika.nystroem@intel.com
// April, 2022
//

`resetall
`default_nettype none

`ifndef _NLATCH_SV
`define _NLATCH_SV

`include "latch.sv"

module nlatch
  (
   // inputs
   // in the following actH means active high; actL means active low

   input logic  ck,
   // write strobe          actH

   input logic  ckb,
   // write strobe inverted actL

   input logic  dx,
   // D input inverted

   output logic q
   // output/state bit
   );

   logic        d, cken;
   
   always_comb d    = ~dx;
   assign cken = ~ckb & ck;

   LATCH m_latch (q, cken, d);

   always_comb begin : assert_strobe_blk
      
      AssertWritesComplementary:
        // the !== '0 syntax allows us to handle the X case on reset
        assert final ((~ck & ckb) | (ck & ~ckb) !== '0);
   end

endmodule




`endif // !_NLATCH_SV

`default_nettype wire
