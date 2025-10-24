// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

//-----------------------------------------------------------------------------
// Title         : RX_PPE types
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : rx_ppe_types
// Author        : Nathan Mai
// Created       : 12/6/2018

//-----------------------------------------------------------------------------
// Description :
// Type definitions for rx_ppe cte
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------
`ifndef MBY_RX_PPE_TYPES_SV
`define MBY_RX_PPE_TYPES_SV


typedef mby_ec_bfm_pkg::eth_bfm#(.MAX_PORTS(`NUM_PORTS_PER_VP))  rx_ppe_vp_bfm_t;
typedef mby_ec_bfm_pkg::eth_bfm#(.MAX_PORTS(`NUM_PORTS_PER_EPL)) rx_ppe_eth_bfm_t;
typedef mby_ec_bfm_pkg::mby_ec_cdi_tx_io                         rx_ppe_eth_bfm_tx_io_t;
typedef mby_ec_bfm_pkg::mby_ec_cdi_rx_io                         rx_ppe_eth_bfm_rx_io_t;
typedef virtual mby_ec_cdi_tx_intf                               rx_ppe_eth_bfm_tx_intf_t;
typedef virtual mby_ec_cdi_rx_intf                               rx_ppe_eth_bfm_rx_intf_t;

// for mby_rx_ppe_tb_top_cfg::get_tb_topology() 
typedef int                          rx_ppe_topology_array_t[];

// Enumeration: mby_rx_ppe_topology_e
// Used to identify the blocks to be instantiated in RTL & in the testbench
// For example, if the MAPPER block *alone* is instantiated:
//              cfg.topology[MAPPER] will be 1 -- all other cfg.stages[*] will be 0
//              scoreboard[MAPPER]   will be created for the post-mapper scoreboard
//              agent[PARSER]        will be created with its driver enabled to drive the mapper RTL
//              agent[MAPPER]        will be created with its monitor enabled to pass the mapper RTL output to scoreboard[MAPPER]
//

typedef enum int {
   PARSER            = 0,
   MAPPER            = 1,
   CLASSIFIER        = 2,
   HASH              = 4,
   NEXT_HOP          = 5,
   MASK_GEN          = 6,
   TRIGGERS          = 7,
   CONGESTION_MGT    = 8,
   RX_STATS          = 9
} mby_rx_ppe_topology_e ;

// Per the SV LRM:
//    "The num method returns the number of elements in the given enumeration."
// However, I've yet to get this to work, thus:
const integer mby_rx_ppe_topology_e_num = 10;
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
`define MBY_RX_PPE_TOPOLOGY_E_NUM  10

`endif // MBY_RX_PPE_TYPES_SV

