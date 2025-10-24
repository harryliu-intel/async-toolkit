// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

///------------------------------------------------------------------------------
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
// -- Author : Lewis Stenberg
// -- Project Name : Madison Bay
// -- Description : Type declarations for Parser to mapper interface
// --               This started out in life as Jon Bagge's par_class_if.sv -- supplimented for use by the testbench
// TODO: Confirm that this conforms to the MAS when the MAS is available
// --
// -------------------------------------------------------------------

//TODO:  I believe there's a LOT of redundancy in the types below & a lot of opportunity to consolodate many typedefs into one



localparam W_MGMT_META      =    16; // Width of management meta
localparam W_MGMT_ADDR      =    28; // Bit width of chip address
localparam W_MGMT_DATA64    =    64; // Width of 64b management ripple channel

localparam N_PARSER_KEYS    =    80; // Number of PARSER keys.
localparam N_PARSER_FLAGS   =    47; // Number of PARSER flags.
localparam N_PARSER_PTRS    =     7; // Number of PARSER ptrs.
localparam N_PARSER_STAGES  =    32; // Number of analyzer stages in PARSER.
localparam W_INT_ID         = 10;

localparam IGR_PPE_TS_WIDTH         = 64;       //IGR to PPE timestamp field width
localparam MGP_TC_CNT               = 9;        //MGP traffic class count
localparam MGP_PORT_CNT             = 17;       //MGP port count, 1 for CP/CPP
localparam MGP_PKT_ID_CNT           = 32768;    //MGP packet ID count
localparam IGR_PPE_ERR_WIDTH        = 2;        //IGR to PPE error field width
localparam IGR_PPE_PKT_LEN_WIDTH    = 14;       //packet length field width

localparam W_SEG            =   256; // Segment size in bytes (non-head).
localparam N_PORTS          =    24; // Number of logical ports per tile.

typedef struct packed {
   logic                mgmt_e;
} imn_rpl_bkwd_t;


typedef logic [ 2:0]           ptot_t;
typedef logic[W_MGMT_META-1:0] mgmt_meta_t;

typedef struct packed {
   logic                          atomic;
   logic                          done;
   logic                          uerr;
   logic                          rw;
   logic [W_MGMT_ADDR-1:0]        addr;
   logic [W_MGMT_DATA64-1:0]      data;
} mgmt64_t;


  typedef struct packed {
    logic                          set_clr_n;
    logic           [W_INT_ID-1:0] id;
  } int_t;
  
//------------------------------------------------------------------------------
// added by George Guo
//------------------------------------------------------------------------------
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


  typedef logic [$clog2(W_SEG+1)-1:0]   seg_len_t;
  typedef logic [15:0] key16_t;
  typedef logic [ 7:0] parser_ptr_t;
typedef logic [$clog2(N_PORTS)-1:0]   local_port_t;




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



typedef enum logic [7:0] {
   LAN_RX = 8'h00,
   LAN_TX = 8'h01,
   MARKER = 8'h02,
   DSI_RX = 8'h14,
   DSI_TX = 8'h16,
   RIM_RX = 8'h18,
   RIM_TX = 8'h1A
} pkt_meta_type_t;

typedef struct packed {
   // row 7
   logic [31:0]    _pad_row7_31_0_;

   // row 6
   logic [31:0]    _pad_row6_31_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [10:0]    _pad_row4_31_21_;
   logic [4:0]     src_port;
   logic           cpm_offload;
   logic [14:0]    _pad_row4_14_0_;

   // row 3
   logic [7:0]     esp_hdr_offset;
   logic [15:0]    pkt_len;
   logic [7:0]     _pad_row3_7_0_;

   // row 2
   logic [7:0]     ip_hdr_offset;
   logic [15:0]    _pad_row2_23_8_;
   logic [7:0]     timestamp_7_0;

   // row 1
   logic [31:0]    timestamp_39_8;

   // row 0
   logic [1:0]     _pad_row0_31_30_;
   logic [2:0]     cd;
   logic [2:0]     _pad_row0_26_24_;
   logic           loopback;
   logic [4:0]     _pad_row0_22_18_;
   logic [9:0]     vsi;
   pkt_meta_type_t typ;
} pkt_meta_lan_rx_hlp_cpm_t;



typedef struct packed {
   // row 7
   logic [31:0]    _pad_row7_31_0_;

   // row 6
   logic [31:0]    _pad_row6_31_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [10:0]    _pad_row4_31_21_;
   logic [4:0]     src_port;
   logic           cpm_offload;
   logic [8:0]     esp_trailer_len;
   logic [2:0]     _pad_row4_5_3;
   logic [2:0]     esp_hdr_len;

   // row 3
   logic [7:0]     esp_hdr_offset;
   logic [19:0]    sa_id;
   logic [3:0]     cpm_status;

   // row 2
   logic [7:0]     _pad_row2_31_24_;
   logic [7:0]     next_hdr;
   logic [7:0]     _pad_row2_15_8_;
   logic [7:0]     timestamp_7_0;

   // row 1
   logic [31:0]    timestamp_39_8;

   // row 0
   logic           pf;
   logic           _pad_row0_30_;
   logic [2:0]     cd;
   logic [2:0]     _pad_row0_26_24_;
   logic           loopback;
   logic [4:0]     _pad_row0_22_18_;
   logic [9:0]     vsi;
   pkt_meta_type_t typ;
} pkt_meta_lan_rx_cpm_hlp_t;


typedef struct packed {
   // row 7
   logic [31:0]    _pad_row7_31_0_;

   // row 6
   logic [31:0]    _pad_row6_31_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [10:0]    _pad_row4_31_21_;
   logic [4:0]     src_port;
   logic           cpm_offload;
   logic [8:0]     esp_trailer_len;
   logic [1:0]     _pad_row4_5_4_;
   logic [3:0]     esp_hdr_len;

   // row 3
   logic [7:0]     _pad_row3_31_24_;
   logic [19:0]    sa_id;
   logic [3:0]     cpm_status;

   // row 2
   logic [7:0]     _pad_row2_31_24_;
   logic [7:0]     next_hdr;
   logic [7:0]     _pad_row2_15_8_;
   logic [7:0]     timestamp_7_0;

   // row 1
   logic [31:0]    timestamp_39_8;

   // row 0
   logic           pf;
   logic           _pad_row0_30_;
   logic [2:0]     cd;
   logic [2:0]     _pad_row0_26_24_;
   logic           loopback;
   logic [4:0]     _pad_row0_22_18_;
   logic [9:0]     vsi;
   pkt_meta_type_t typ;
} pkt_meta_lan_rx_hlp_cpk_t;


typedef struct packed {
   // row 7
   logic [31:0]    _pad_row7_31_0_;

   // row 6
   logic [31:0]    _pad_row6_31_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [10:0]    _pad_row4_31_21_;
   logic [4:0]     src_port;
   logic           cpm_offload;
   logic [14:0]    _pad_row4_14_0_;

   // row 3
   logic [31:0]    _pad_row3_31_0_;

   // row 2
   logic [23:0]    _pad_row2_31_8_;
   logic [7:0]     timestamp_7_0;

   // row 1
   logic [31:0]    timestamp_39_8;

   // row 0
   logic [1:0]     _pad_row0_31_30_;
   logic [2:0]     cd;
   logic [2:0]     _pad_row0_26_24_;
   logic           loopback;
   logic [4:0]     _pad_row0_22_18_;
   logic [9:0]     vsi;
   pkt_meta_type_t typ;
} pkt_meta_lan_rx_common_t;


typedef struct packed {
   // row 7
   logic [31:0]    cpm_esn;

   // row 6
   logic [19:0]    cpm_sa_index;
   logic [11:0]    _pad_row6_11_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [15:0]    _pad_row4_31_16_;
   logic           cpm_offload;
   logic           cpm_tso;
   logic [13:0]    _pad_row4_13_0_;

   // row 3
   logic [31:0]    _pad_row3_31_0_;

   // row 2
   logic [24:0]    _pad_row2_31_7_;
   logic           ts_cap_idx_v;
   logic [5:0]     ts_cap_idx;

   // row 1
   logic [31:0]    cpk_opaque_data;

   // row 0
   logic           pf;
   logic [1:0]     _pad_row0_30_29_;
   logic [4:0]     cd_5b;
   logic [1:0]     cpk_fwd;
   logic           _pad_row0_21_;
   logic [4:0]     dest_port;
   logic [7:0]     func_num;
   pkt_meta_type_t typ;
} pkt_meta_lan_tx_cpk_hlp_t;



typedef struct packed {
   // row 7
   logic [31:0]    cpm_esn;

   // row 6
   logic [19:0]    cpm_sa_index;
   logic [11:0]    _pad_row6_11_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [15:0]    _pad_row4_31_16_;
   logic           cpm_offload;
   logic           cpm_tso;
   logic [13:0]    _pad_row4_13_0_;

   // row 3
   logic [7:0]     esp_hdr_offset;
   logic [15:0]    pkt_len;
   logic [7:0]     _pad_row3_7_0_;

   // row 2
   logic [7:0]     ip_hdr_offset;
   logic [16:0]    _pad_row2_24_7_;
   logic           ts_cap_idx_v;
   logic [5:0]     ts_cap_idx;

   // row 1
   logic [31:0]    cpk_opaque_data;

   // row 0
   logic           pf;
   logic           _pad_row0_30_;
   logic [2:0]     cd;
   logic [2:0]     _pad_row0_26_24_;
   logic [1:0]     cpk_fwd;
   logic           _pad_row0_21_;
   logic [4:0]     dest_port;
   logic [7:0]     func_num;
   pkt_meta_type_t typ;
} pkt_meta_lan_tx_hlp_cpm_t;


typedef struct packed {
   // row 7
   logic [31:0]    _pad_row7_31_0_;

   // row 6
   logic [31:0]    _pad_row6_31_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [15:0]    _pad_row4_31_16_;
   logic           cpm_offload;
   logic [14:0]    _pad_row4_14_0_;

   // row 3
   logic [27:0]    _pad_row3_31_4_;
   logic [3:0]     cpm_status;

   // row 2
   logic [24:0]    _pad_row2_31__;
   logic           ts_cap_idx_v;
   logic [5:0]     ts_cap_idx;

   // row 1
   logic [31:0]    cpk_opaque_data;

   // row 0
   logic           pf;
   logic           _pad_row0_30_;
   logic [2:0]     cd;
   logic [2:0]     _pad_row0_26_24_;
   logic [1:0]     cpk_fwd;
   logic           _pad_row0_21_;
   logic [4:0]     dest_port;
   logic [7:0]     func_num;
   pkt_meta_type_t typ;
} pkt_meta_lan_tx_cpm_hlp_t;


typedef struct packed {
   // row 7
   logic [31:0]    _pad_row7_31_0_;

   // row 6
   logic [31:0]    _pad_row6_31_0_;

   // row 5
   logic [31:0]    _pad_row5_31_0_;

   // row 4
   logic [15:0]    _pad_row4_31_16_;
   logic           cpm_offload;
   logic [14:0]    _pad_row4_14_0_;

   // row 3
   logic [31:0]    _pad_row3_31_0_;

   // row 2
   logic [24:0]    _pad_row2_31_7_;
   logic           ts_cap_idx_v;
   logic [5:0]     ts_cap_idx;

   // row 1
   logic [31:0]    cpk_opaque_data;

   // row 0
   logic           pf;
   logic [6:0]     _pad_row0_30_24_;
   logic [1:0]     cpk_fwd;
   logic           _pad_row0_21_;
   logic [4:0]     dest_port;
   logic [7:0]     func_num;
   pkt_meta_type_t typ;
} pkt_meta_lan_tx_common_t;

typedef logic   [ 7:0] octet_t;       // byte; for ease of coding and debugging

typedef struct packed {
   // rows 1-7
   octet_t [27:0]  _pad_rows_7_1_;

   // row 0
   logic [1:0]     ft;
   logic           _pad_row0_29_;
   logic [4:0]     tx_cd_5b;
   logic [2:0]     rx_cd;
   logic [2:0]     _pad_row0_20_18_;
   logic [9:0]     vm_or_vf;
   pkt_meta_type_t typ;
} pkt_meta_marker_t;


typedef struct packed {
   // row 7
   logic [8:0]     inner_id;
   logic [6:0]     pkt_type;
   logic [2:0]     _pad_row7_15_13_;
   logic [1:0]     pol_color_a;
   logic [10:0]    _pad_row7_10_0_;

   // row 6
   logic [15:0]    hqm_lock_id;
   logic [15:0]    outer_id;

   // row 5
   logic [11:0]    acl_id;
   logic           _pad_row5_19_;
   logic [5:0]     l3_domain;
   logic [8:0]     l2_domain;
   logic [3:0]     operator_id;

   // row 4
   logic [10:0]    rx_flags;
   logic [4:0]     src_port;
   logic           cpm_offload;
   logic [8:0]     esp_trailer_length;
   logic [1:0]     _pad_row4_5_4_;
   logic [3:0]     esp_hdr_len;

   // row 3
   logic [7:0]     esp_hdr_offset;
   logic [19:0]    sa_id;
   logic [3:0]     cpm_status;

   // row 2
   logic [7:0]     _pad_row2_31_24_;
   logic [7:0]     next_header;
   logic [7:0]     _pad_row2_15_8_;
   logic [7:0]     timestamp_7_0;

   // row 1
   logic [31:0]    timestamp_39_8;

   // row 0
   logic           pf;
   logic           _pad_row0_30_;
   logic [2:0]     cd;
   logic [10:0]    queue;
   logic [7:0]     func_num;
   pkt_meta_type_t typ;
} pkt_meta_dsi_rx_cpm_hlp_t;


typedef struct packed {
   // row 7
   logic [8:0]     inner_id;
   logic [6:0]     pkt_type;
   logic [2:0]     _pad_row7_15_13_;
   logic [1:0]     pol_color_a;
   logic [10:0]    _pad_row7_10_0_;

   // row 6
   logic [15:0]    hqm_lock_id;
   logic [15:0]    outer_id;

   // row 5
   logic [11:0]    acl_id;
   logic           _pad_row5_19_;
   logic [5:0]     l3_domain;
   logic [8:0]     l2_domain;
   logic [3:0]     operator_id;

   // row 4
   logic [10:0]    rx_flags;
   logic [4:0]     src_port;
   logic           cpm_offload;
   logic [14:0]    _pad_row4_14_0_;

   // row 3
   logic [7:0]     esp_hdr_offset;
   logic [15:0]    pkt_len;
   logic [7:0]     _pad_row3_7_0_;

   // row 2
   logic [7:0]     ip_hdr_offset;
   logic [7:0]     _pad_row2_23_16_;
   logic [7:0]     queue_level;
   logic [7:0]     timestamp_7_0;

   // row 1
   logic [31:0]    timestamp_39_8;

   // row 0
   logic           pf;
   logic           _pad_row0_30_;
   logic [2:0]     cd;
   logic [10:0]    queue;
   logic [7:0]     func_num;
   pkt_meta_type_t typ;
} pkt_meta_dsi_rx_hlp_cpm_t;



  typedef struct packed {
    // row 7
    logic [8:0]     inner_id;
    logic [6:0]     pkt_type;
    logic [15:0]    csum_15_0;

    // row 6
    logic [15:0]    hqm_lock_id;
    logic [15:0]    outer_id;

    // row 5
    logic [11:0]    acl_id;
    logic           _pad_row5_19_;
    logic [5:0]     l3_domain;
    logic [8:0]     l2_domain;
    logic [3:0]     operator_id;

    // row 4
    logic [10:0]    rx_flags;
    logic [4:0]     src_port;
    logic           cpm_offload;
    logic [1:0]     pol_color_b;
    logic [1:0]     pol_color_a;
    logic [10:0]    spd_id;

    // row 3
    logic [7:0]     csum_23_16;
    logic [19:0]    sa_id;
    logic [3:0]     cpm_status;

    // row 2
    logic [7:0]     csum_31_24;
    logic [7:0]     _pad_row2_23_16_;
    logic [7:0]     queue_level;
    logic [7:0]     timestamp_7_0;

    // row 1
    logic [31:0]    timestamp_39_8;

    // row 0
    logic           pf;
    logic           _pad_row0_30_;
    logic [2:0]     cd;
    logic [10:0]    queue;
    logic [7:0]     func_num;
    pkt_meta_type_t typ;
  } pkt_meta_dsi_rx_hlp_cpk_t;


  typedef struct packed {
    // row 7
    logic [8:0]     inner_id;
    logic [6:0]     pkt_type;
    logic [15:0]    _pad_row7_15_0_;

    // row 6
    logic [15:0]    hqm_lock_id;
    logic [15:0]    outer_id;

    // row 5
    logic [11:0]    acl_id;
    logic           _pad_row5_19_;
    logic [5:0]     l3_domain;
    logic [8:0]     l2_domain;
    logic [3:0]     operator_id;

    // row 4
    logic [10:0]    rx_flags;
    logic [4:0]     src_port;
    logic           cpm_offload;
    logic [14:0]    _pad_row4_14_0_;

    // row 3
    logic [31:0]    _pad_row3_31_0_;

    // row 2
    logic [15:0]    _pad_row2_31_16_;
    logic [7:0]     queue_level;
    logic [7:0]     timestamp_7_0;

    // row 1
    logic [31:0]    timestamp_39_8;

    // row 0
    logic           pf;
    logic           _pad_row0_30_;
    logic [2:0]     cd;
    logic [10:0]    queue;
    logic [7:0]     func_num;
    pkt_meta_type_t typ;
  } pkt_meta_dsi_rx_common_t;



  typedef struct packed {
    // row 7
    logic [31:0]    cpm_esn;

    // row 6
    logic [19:0]    cpm_sa_index;
    logic [11:0]    _pad_row6_11_0_;

    // row 5
    logic [7:0]     lag_hash_mod;
    logic [7:0]     tx_flags;
    logic [2:0]     _pad_row5_15_13_;
    logic [8:0]     l2_domain;
    logic [3:0]     operator_id;

    // row 4
    logic [15:0]    jitter_timestamp;
    logic           cpm_offload;
    logic           cpm_tso;
    logic [13:0]    _pad_row4_13_0_;

    // row 3
    logic [31:0]    _pad_row3_31_0_;

    // row 2
    logic [24:0]    _pad_row2_31_7_;
    logic           ts_cap_idx_v;
    logic [5:0]     ts_cap_idx;

    // row 1
    logic [20:0]    _pad_row1_31_11_;
    logic [2:0]     tc;
    logic [7:0]     dest_port;

    // row 0
    logic           pf;
    logic [1:0]     _pad_row0_30_29_;
    logic [4:0]     cd_5b;
    logic [7:0]     _pad_row0_23_16_;
    logic [7:0]     func_num;
    pkt_meta_type_t typ;
  } pkt_meta_dsi_tx_cpk_hlp_t;


  typedef struct packed {
    // row 7
    logic [31:0]    cpm_esn;

    // row 6
    logic [19:0]    cpm_sa_index;
    logic [11:0]    _pad_row6_11_0_;

    // row 5
    logic [7:0]     lag_hash_mod;
    logic [7:0]     tx_flags;
    logic [2:0]     _pad_row5_15_13_;
    logic [8:0]     l2_domain;
    logic [3:0]     operator_id;

    // row 4
    logic [15:0]    jitter_timestamp;
    logic           cpm_offload;
    logic           cpm_tso;
    logic [13:0]    _pad_row4_13_0_;

    // row 3
    logic [7:0]     esp_hdr_offset;
    logic [15:0]    pkt_len;
    logic [7:0]     _pad_row3_7_0_;

    // row 2
    logic [7:0]     ip_hdr_offset;
    logic [7:0]     _pad_row2_23_16_;
    logic [7:0]     queue_level;
    logic           _pad_row2_7_;
    logic           ts_cap_idx_v;
    logic [5:0]     ts_cap_idx;

    // row 1
    logic [20:0]    _pad_row1_31_11_;
    logic [2:0]     tc;
    logic [7:0]     dest_port;

    // row 0
    logic           pf;
    logic           _pad_row0_30_;
    logic [2:0]     cd;
    logic [10:0]    _pad_row0_26_16_;
    logic [7:0]     func_num;
    pkt_meta_type_t typ;
  } pkt_meta_dsi_tx_hlp_cpm_t;


  typedef struct packed {
    // row 7
    logic [31:0]    _pad_row7_31_0_;

    // row 6
    logic [19:0]    cpm_sa_index;
    logic [11:0]    _pad_row6_11_0_;

    // row 5
    logic [7:0]     lag_hash_mod;
    logic [7:0]     tx_flags;
    logic [2:0]     _pad_row5_15_13_;
    logic [8:0]     l2_domain;
    logic [3:0]     operator_id;

    // row 4
    logic [15:0]    jitter_timestamp;
    logic           cpm_offload;
    logic [14:0]    _pad_row4_14_0_;

    // row 3
    logic [27:0]    _pad_row3_31_4_;
    logic [3:0]     cpm_status;

    // row 2
    logic [15:0]    _pad_row2_31_16_;
    logic [7:0]     queue_level;
    logic           _pad_row2_7_;
    logic           ts_cap_idx_v;
    logic [5:0]     ts_cap_idx;

    // row 1
    logic [20:0]    _pad_row1_31_11_;
    logic [2:0]     tc;
    logic [7:0]     dest_port;

    // row 0
    logic           pf;
    logic           _pad_row0_30_;
    logic [2:0]     cd;
    logic [10:0]    _pad_row0_26_16_;
    logic [7:0]     func_num;
    pkt_meta_type_t typ;
  } pkt_meta_dsi_tx_cpm_hlp_t;



  typedef struct packed {
    // row 7
    logic [31:0]    _pad_row7_31_0_;

    // row 6
    logic [31:0]    _pad_row6_31_0_;

    // row 5
    logic [7:0]     lag_hash_mod;
    logic [7:0]     tx_flags;
    logic [2:0]     _pad_row5_15_13_;
    logic [8:0]     l2_domain;
    logic [3:0]     operator_id;

    // row 4
    logic [15:0]    jitter_timestamp;
    logic           cpm_offload;
    logic [14:0]    _pad_row4_14_0_;

    // row 3
    logic [31:0]    _pad_row3_31_0_;

    // row 2
    logic [15:0]    _pad_row2_31_16_;
    logic [7:0]     queue_level;
    logic           _pad_row2_7_;
    logic           ts_cap_idx_v;
    logic [5:0]     ts_cap_idx;

    // row 1
    logic [20:0]    _pad_row1_31_11_;
    logic [2:0]     tc;
    logic [7:0]     dest_port;

    // row 0
    logic           pf;
    logic [14:0]    _pad_row0_30_16_;
    logic [7:0]     func_num;
    pkt_meta_type_t typ;
  } pkt_meta_dsi_tx_common_t;


  typedef struct packed {
    logic [31:0]    timestamp_39_8;
    logic [14:0]    _pad_31_17_;
    logic           msec_sys_rx_cntrld;
    logic [7:0]     timestamp_7_0;
    pkt_meta_type_t typ;
  } pkt_meta_rim_rx_t;


  typedef struct packed {
    logic [(6*32)-1:0] _pad_;
    pkt_meta_rim_rx_t  rim_rx;
  } pkt_meta_rim_rx_pad_t;

  

  typedef struct packed {
    logic [14:0]    _pad_31_17_;
    logic           msec_sys_tx_cntrld;
    logic           _pad_15_;
    logic           ts_cap_idx_v;
    logic [5:0]     ts_cap_idx;
    pkt_meta_type_t typ;
  } pkt_meta_rim_tx_t;
  

  typedef struct packed {
    logic [(7*32)-1:0] _pad_;
    pkt_meta_rim_tx_t  rim_tx;
  } pkt_meta_rim_tx_pad_t;


typedef union  packed {
   // type==LAN_RX
   pkt_meta_lan_rx_hlp_cpm_t  lan_rx_hlp_cpm;
   pkt_meta_lan_rx_cpm_hlp_t  lan_rx_cpm_hlp;
   pkt_meta_lan_rx_hlp_cpk_t  lan_rx_hlp_cpk;
   pkt_meta_lan_rx_common_t   lan_rx_common;

   // type==LAN_TX
   pkt_meta_lan_tx_cpk_hlp_t  lan_tx_cpk_hlp;
   pkt_meta_lan_tx_hlp_cpm_t  lan_tx_hlp_cpm;
   pkt_meta_lan_tx_cpm_hlp_t  lan_tx_cpm_hlp;
   pkt_meta_lan_tx_common_t   lan_tx_common;

   // type==MARKER
   pkt_meta_marker_t          marker;

   // type==DSI_RX
   pkt_meta_dsi_rx_cpm_hlp_t  dsi_rx_cpm_hlp;
   pkt_meta_dsi_rx_hlp_cpm_t  dsi_rx_hlp_cpm;
   pkt_meta_dsi_rx_hlp_cpk_t  dsi_rx_hlp_cpk;
   pkt_meta_dsi_rx_common_t   dsi_rx_common;

   // type==DSI_TX
   pkt_meta_dsi_tx_cpk_hlp_t  dsi_tx_cpk_hlp;
   pkt_meta_dsi_tx_hlp_cpm_t  dsi_tx_hlp_cpm;
   pkt_meta_dsi_tx_cpm_hlp_t  dsi_tx_cpm_hlp;
   pkt_meta_dsi_tx_common_t   dsi_tx_common;

   // type==RIM_RX
   pkt_meta_rim_rx_pad_t      rim_rx_pad;

   // type==RIM_TX
   pkt_meta_rim_tx_pad_t      rim_tx_pad;

   // octet arrays
   octet_t [31:0]            b;             // little endian, b[0] xmit first
   octet_t [0:31]            big_endian;    // lintra s-2028 "big endian"
} pkt_meta_t;


typedef struct packed {
   parse_result_t                 parse_result;
   pkt_meta_t                     pkt_meta;
} parser_out_t;


//vp_cpp_rx_metadata when normally switched
typedef struct packed {
    logic           snap_timestamp; //snap timestamp
    logic           data_type;      //data_type, determines lower bits
    logic   [3:0]   net_add_dom;    //network address domain
    logic   [5:0]   l3_dom;         //l3 domain
    logic   [7:0]   l2_dom;         //l2 domain
} vp_cpp_rx_md_swt_t;


//vp_cpp_rx_metadata when directed
typedef struct packed {
    logic           snap_timestamp; //snap timestamp
    logic           data_type;      //data_type, determines lower bits
    logic   [1:0]   reserved_1_0;   //unused
    logic   [15:0]  dglort;         //destination glort
} vp_cpp_rx_md_dir_t;


typedef union packed {
    vp_cpp_rx_md_swt_t  switched;   //Cport metadata when switched
    vp_cpp_rx_md_dir_t  directed;   //Cport metadata when directed
} vp_cpp_rx_md_t;


typedef struct packed {
    vp_cpp_rx_md_t                          cpp_md; //Cport metadata
    logic   [IGR_PPE_TS_WIDTH-1:0]          ts;     //header segment timestamp
    logic   [$clog2(MGP_TC_CNT)-1:0]        tc;     //header segment TC
    logic   [$clog2(MGP_PORT_CNT)-1:0]      port;   //header segment incoming port ID
    logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;     //header segment packet ID
} igr_rx_ppe_md_t;


typedef struct packed {
    logic   [IGR_PPE_ERR_WIDTH-1:0]         err;    //tail information CRC error notification
    logic   [IGR_PPE_PKT_LEN_WIDTH-1:0]     len;    //tail information packet length
    logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;     //tail information packet ID
    logic                                   valid;  //tail information valid
} igr_rx_ppe_tail_t;

