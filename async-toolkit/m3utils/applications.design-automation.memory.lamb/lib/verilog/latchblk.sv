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

`resetall
`default_nettype none

primitive LATCH( Q, CK, D );
output Q;
reg Q;
input CK, D;
table
//     CK  D  :  Q  :  Q
        0  ?  :  ?  :  -;
        1  1  :  ?  :  1;
        ?  1  :  1  :  1;
        1  0  :  ?  :  0;
        ?  0  :  0  :  0;

endtable
endprimitive

module latchblk
  #(
    parameter DEPTH      = 4,
    parameter DEPTHOVER2 = (DEPTH + 1)/2
    )
   (
    input  logic  ck [ DEPTH - 1 : 0 ],
    input  logic  ckb [ DEPTH - 1 : 0 ],
    input  logic  rwl [ DEPTH - 1 : 0 ],
    input  logic  rwlx[ DEPTH - 1 : 0 ],
    input  logic  dx [ DEPTHOVER2 - 1 : 0],
    input  logic  z,
    output logic q [ DEPTH - 1 : 0 ],
    output logic y
    );

   logic         d     [ DEPTHOVER2 - 1 : 0 ];
   logic         cken  [ DEPTH - 1      : 0 ];
   logic         rwlen [ DEPTH - 1      : 0 ];
   logic         dout  [ DEPTH - 1      : 0 ];
   logic         y1;

   assign d    = ~dx;
   assign cken = ~ckbx & ck;
   
   generate
      for (genvar i=0; i < DEPTH; ++i)
        LATCH m_latch[i] (q[i], cken[i], d[i/2]);
   endgenerate

   assign rwlen = ~rwlx & rwl;
   assign dout  = rwlen & q;
   assign y1 = |dout;
   assign y  = ~z & y1;
   
endmodule // latchblk

`default_nettype wire
