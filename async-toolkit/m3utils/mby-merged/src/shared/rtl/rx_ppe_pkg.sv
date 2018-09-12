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
// -- Description : RX-PPE package file
// --
// -------------------------------------------------------------------

`ifndef RX_PPE_PKG_INCLUDE
`define RX_PPE_PKG_INCLUDE

package rx_ppe_pkg;

  import pkt_pkg::*;
  import pkt_meta_pkg::pkt_meta_t;

  localparam W_MGMT_META      =    16; // Width of management meta
  localparam W_SEG            =   256; // Segment size in bytes (non-head).
  localparam N_PORTS          =    24; // Number of logical ports per tile.
  localparam W_TXSEG          =   320; // Segment size in bytes (EXBAR)
  localparam W_INT_ID     = 10;
  localparam W_MGMT_ADDR      =    28; // Bit width of chip address
  localparam W_MGMT_DATA64    =    64; // Width of 64b management ripple channel
  localparam N_PARSER_KEYS    =    80; // Number of PARSER keys.
  localparam N_PARSER_FLAGS   =    47; // Number of PARSER flags.
  localparam N_PARSER_PTRS    =     7; // Number of PARSER ptrs.
  localparam N_PARSER_STAGES  =    32; // Number of analyzer stages in PARSER.

  typedef logic [ 2:0]                  ptot_t;
  typedef logic [15:0] key16_t;
  typedef logic[W_MGMT_META-1:0] mgmt_meta_t;
  typedef logic [ 7:0] parser_ptr_t;
  typedef logic [$clog2(W_SEG+1)-1:0]   seg_len_t;
  typedef logic [$clog2(N_PORTS)-1:0]   local_port_t;
  typedef logic [$clog2(W_TXSEG+1)-1:0] txseg_len_t;
  typedef logic [ 2:0]                  next_len_t;
  typedef logic [13:0]                  packet_len_t;
  typedef logic   [15:0] csum_t;        // L3 or L4 checksum

  typedef struct packed {
    logic                          set_clr_n;
    logic           [W_INT_ID-1:0] id;
  } int_t;

  typedef struct packed {
    logic                          atomic;
    logic                          done;
    logic                          uerr;
    logic                          rw;
    logic [W_MGMT_ADDR-1:0]        addr;
    logic [W_MGMT_DATA64-1:0]      data;
  } mgmt64_t;

  typedef struct packed {
     ptot_t               curr_ptot;
     int_t                intr;
     logic                intr_v;
     logic                intr_slot;
     mgmt_meta_t          mgmt_meta;
     mgmt64_t             mgmt;
     logic                mgmt_v;
     logic                mem_init_done;
     logic                func_init_done;
  } imn_rpl_frwd_t;

  typedef struct packed {
     logic                mgmt_e;
  } imn_rpl_bkwd_t;

  typedef struct packed {
    seg_len_t                      adj_seg_len;
    key16_t [0:N_PARSER_KEYS-1]    keys;        // lintra s-2028 "big endian"
    logic   [0:N_PARSER_KEYS-1]    keys_valid;  // lintra s-2028 "big endian"
    logic   [N_PARSER_FLAGS:1]     flags;
    parser_ptr_t [N_PARSER_PTRS:1] ptrs;
    logic [N_PARSER_PTRS:1]        ptrs_valid;
    logic [1:0]                    csum_ok;
    logic                          ex_depth_exceed;
    logic                          ex_trunc_header;
    logic                          ex_parsing_done;
    logic [$clog2(N_PARSER_STAGES)-1:0] ex_stage;
    local_port_t                        port;
  } parse_result_t;

  typedef struct packed {
    parse_result_t                 parse_result;
    pkt_meta_t                     pkt_meta;
  } parser_out_t;

  typedef struct packed {
    local_port_t                   port;
  } global_port_t;

  // Error code used to identify different sources of an invalid packet.
  typedef enum logic [1:0] {
    ERR_NO_ERR       = 0,           // No error; normal packet.
    ERR_FCS_ERR      = 1,           // Error in Ethernet FCS checksum.
    ERR_FRAMING_ERR  = 2,           // Error in Ethernet framing.
    ERR_INTERNAL_ERR = 3,           // Internal HLP error, or term due to reset
    XPROP_ERR        = 2'bxx
  } err_t;

  typedef struct packed {
    global_port_t                  rx_port;
    logic                          sop;
    logic                          eop;
    err_t                          err;
    txseg_len_t                    len;
    next_len_t                     next_len;
    ptot_t                         ptot;
  } seg_meta_t;

  typedef struct packed {
    logic [2:0]                    case_code;
    packet_len_t                   pkt_len;
    csum_t                         csum;
  } l4csum_len_common_t;  // covers default, compute, and store TCP/UDP cases

  typedef struct packed {
    logic [2:0]                    case_code;
    packet_len_t                   pkt_len;
    logic [12:0]                   rsvd;
    logic                          drop;
    logic                          l3_err;
    logic                          l4_err;
  } l4csum_len_validate_t;

  typedef struct packed {
    logic                          case_code;
    logic [31:0]                   csum;
  } l4csum_len_store_sctp_t;

  typedef union packed {
    l4csum_len_common_t            common;
    l4csum_len_validate_t          validate;
    l4csum_len_store_sctp_t        store_sctp;
  } l4csum_len_t;

  typedef struct packed {
    logic                          uerr;       // a Parser uerr used by Tail.  may occur on any packet segment
    seg_meta_t                     seg_meta;
    logic                          force_saf;
    logic                          drop;
    logic                          csum_err;
    logic                          l3len_err;
    l4csum_len_t                   l4csum_len;
  } tail_info_t;

endpackage : rx_ppe_pkg

`endif	// RX_PPE_PKG_INCLUDE
