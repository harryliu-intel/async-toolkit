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
//------------------------------------------------------------------------------
//   Author        : Nathan Mai
//                 : Lewis Sternberg
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//   Defines : types useful to mby_rx_ppe testbench
//
//  This file contain any PARAMETERS or Defines.  Also contains Topology
//  configuration ENUM.

`ifndef __MBY_RX_PPE_DEFINES_GUARD
`define __MBY_RX_PPE_DEFINES_GUARD

`ifndef __INSIDE_MBY_RX_PPE_ENV_PKG
`error "Attempt to include file outside of mby_rx_ppe_env_pkg."
`endif

// LNS: encapsulating this as a class prevents vcs from being able to use mby_rx_ppe_topology_e_num as an array size


// Enumeration: mby_rx_ppe_topology_e
// Used to identify the blocks to be instantiated in RTL & in the testbench
// For example, if the MAPPER block *alone* is instantiated:
//              cfg.topology[MAPPER] will be 1 -- all other cfg.stages[*] will be 0
//              scoreboard[MAPPER]   will be created for the post-mapper scoreboard
//              agent[PARSER]        will be created with its driver enabled to drive the mapper RTL
//              agent[MAPPER]        will be created with its monitor enabled to pass the mapper RTL output to scoreboard[MAPPER]
//

//TODO:delete this comment when no longer needed
// LNS: for the record, this had been:
//   typedef enum int {
//      UNK_TOPO           = 0,
//      RX_PPE_FULL        = 1
//   } rx_ppe_topology_e ;
// 

// TODO: LNS: I'm not so sure about the latter stages -- review is needed
typedef enum int {
   PARSER            = 0,
   MAPPER            = 1,
   CLASSIFIER        = 2,
   POLICER           = 3,
   HASH              = 4,
   NEXT_HOP_LOOKUP   = 5,
   MASK_GEN          = 6,
   TRIGGERS          = 7,
   CONGESTION_MGT    = 8,
   TELEMETRY         = 9,
   MIRRORS_MCAST     = 10,
   METADATA_GEN      = 11,
   MGMT_INT          = 12
} mby_rx_ppe_topology_e ;

// Per the SV LRM:
//    "The num method returns the number of elements in the given enumeration."
// However, I've yet to get this to work, thus:
const integer mby_rx_ppe_topology_e_num = 13;
// the above, when used in mby_rx_ppe_tb_top_cfg thus:
//   int topology [mby_rx_ppe_topology_e_num];
// produces:
// -I-:Error-[TCF-CETE] Cannot evaluate the expression
// -I-:/nfs/site/disks/sc_mby_00072/lnstern/mby/work_root/mby-mby-x0/subBlock/mbyc/verif/rx_ppe/env/mby_rx_ppe_tb_top_cfg.svh, 47
// -I-:"(mby_rx_ppe_topology_e_num + (~1'sd0))"
// -I-:  Cannot evaluate the expression in right dimension bound.
// -I-:  The expression must be compile time constant.
// 
//stupid annoying work-around:
`define MBY_RX_PPE_TOPOLOGY_E_NUM  13


`endif // __MBY_RX_PPE_DEFINES_GUARD
