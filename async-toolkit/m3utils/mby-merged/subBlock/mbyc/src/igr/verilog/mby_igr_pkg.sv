//------------------------------------------------------------------------------
//
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
// -- Description  : This is the package including all the structs used in 
//                   Ingress(IGR) block.
//------------------------------------------------------------------------------


`ifndef MBY_IGR_PKG_SV
  `define MBY_IGR_PKG_SV

package mby_igr_pkg;

  `include "mby_igr_macros.vh"
  

  typedef struct packed {
    logic [ 7:0]   ecc;
    logic [63:0]  data;
  } data64_w_ecc_t;
  
  typedef struct packed {  //format per 1588 timestamp only valid if SOP valid
    logic [3:0]     s;
    logic [31:0]  ns;    
  } epl_ts_t;
  
  typedef struct packed {
    logic          rsvd1;  //   [23] Reserved
    logic          multi;  //   [22] MAC address in multicast range
    logic           fast;  //   [21] Fastpast ethertype detected
    logic [1:0] fcs_hint;  //[20:19] Part/all of FCS in next clock cycle for this logical port
    logic            dei;  //   [18] Drop eligible inticator
    logic [1:0]    error;  //[17:16] Bad packet
    logic          rsvd0;  //   [15] Rserved
    logic            eop;  //   [14] End of packet
    logic [2:0]  eop_pos;  //[13:11] Flit containing eop, undefined eop=0
    logic [2:0] byte_pos;  // [10:8] Most significant valid byte in eop flit
    logic            sop;  //    [7] Start of packet
    logic [2:0]  sop_pos;  //  [6:4] Flit containing sop, undefined sop=0
    logic [3:0]       tc;  //  [3:0] MAC received traffic class, undefined sop=0
  } epl_md_t;
    

endpackage

`endif //MBY_IGR_PKG_SV
