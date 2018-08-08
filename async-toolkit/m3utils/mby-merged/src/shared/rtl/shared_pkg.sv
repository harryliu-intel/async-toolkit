///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Jon Bagge <jon.bagge@intel.com>
// -- Project Name : Madison Bay
// -- Description : Shared package file
// --
// -------------------------------------------------------------------

`ifndef SHARED_PKG_INCLUDE
`define SHARED_PKG_INCLUDE

package shared_pkg;

typedef struct packed {
    logic   [1:0]       err;   //tail information CRC error notification
    logic   [13:0]      len;   //tail information packet length
    logic   [14:0]      id;    //tail information packet ID
    logic               valid; //tail information valid
} igr_pre_ppe_rx_ppe_tail_t;

typedef struct packed {
    logic   [13:0]      ts;     //header segment timestamp
    logic   [2:0]       tc;     //header segment TC
    logic   [4:0]       port;   //header segment incoming port ID
    logic   [14:0]      id;     //header segment ID
    logic   [71:0]      ecc;    //header segment ECC
    logic   [1023:0]    data;   //header segment data
    logic               valid;  //header segment valid
} igr_pre_ppe_rx_ppe_head_t;

typedef struct packed {
    logic               last;       //last multicast or mirror copy for packet
    logic               multicast;  //packet is part of a multicast sequence
    logic               mirror;     //packet is part of a mirrored pair
    logic   [14:0]      addr;       //local storage address
    logic   [2:0]       tc;         //TC
    logic   [3:0]       port;       //destination port
    logic   [14:0]      id;         //header segment ID
    logic               valid;      //valid
} rx_ppe_igr_post_ppe_t;

//
// LSM (management port)
//
  localparam W_MGMT_ADDR      =    28; // Bit width of chip address
  localparam W_MGMT_DATA64    =    64; // Width of 64b management ripple channel
  localparam W_MGMT_DATA32    =    32; // Width of 32b management ripple channel
 
  typedef struct packed {
    logic                          atomic;
    logic                          done;
    logic                          uerr;
    logic                          rw;
    logic [W_MGMT_ADDR-1:0]        addr;
    logic [W_MGMT_DATA64-1:0]      data;
  } mgmt64_t;

endpackage : shared_pkg

`endif	// SHARED_PKG_INCLUDE
