//  INTEL CONFIDENTIAL
//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
// -- Author       : Edward C. Ross
// -- Project Name : Madison Bay
// -- Description  : Macro definitions used in 
//                   Ingress(IGR) block.
//------------------------------------------------------------------------------

`ifndef MBY_IGR_MACROS_VH
  `define MBY_IGR_MACROS_VH 

`define MBY_MSFF(q,i,clock)                                  \
   always_ff @(posedge clock)                                \
      begin                                                  \
   `ifdef VAL4_OPTIMIZED                                     \
        q <=  (clock) ? i : q;                               \
   `else                                                     \
        q <= i;                                              \
   `endif                                                    \
      end                                                    \

`define MBY_EN_MSFF(q,i,clock,enable)                        \
   always_ff @(posedge clock)                                \
      begin                                                  \
  `ifdef VAL4_OPTIMIZED                                      \
         q <=  ((clock) & (enable)) ? i : q;                 \
   `else                                                     \
         if ((enable)) q <= i;                               \
   `endif                                                    \
       end                                                   \

`define MBY_EN_RST_MSFF(q,i,clock,enable,rst)                \
   always_ff @(posedge clock )                               \
      begin                                                  \
   `ifdef VAL4_OPTIMIZED                                     \
         q <= (clock) ? ((rst) ? '0 : ((enable) ? i : q)) : q; \
   `else                                                     \
            if ( rst )         q <= '0 ;                     \
            else if ( enable ) q <=  i ;                     \
   `endif                                                    \
      end                                                    \

`define MBY_RST_MSFF(q,i,clock,rst)                          \
   always_ff @(posedge clock)                                \
      begin                                                  \
   `ifdef VAL4_OPTIMIZED                                     \
          q <= (clock) ? ((rst) ? '0 : i) : q ;              \
   `else                                                     \
           if (rst) q <= '0;                                 \
          else     q <=  i;                                  \
   `endif                                                    \
      end                                                    \
      
`endif //MBY_IGR_MACROS_VH
