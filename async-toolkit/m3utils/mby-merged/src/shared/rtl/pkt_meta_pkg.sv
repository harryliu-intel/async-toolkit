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

package pkt_meta_pkg;

  import pkt_pkg::octet_t;

  typedef enum logic [7:0] {
    LAN_RX = 8'h00,
    LAN_TX = 8'h01,
    MARKER = 8'h02,
    DSI_RX = 8'h14,
    DSI_TX = 8'h16,
    RIM_RX = 8'h18,
    RIM_TX = 8'h1A
  } pkt_meta_type_t;

  ////////////////////////////////////////////////////////////////
  //                        type==LAN_RX                        //
  ////////////////////////////////////////////////////////////////

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
  
  ////////////////////////////////////////////////////////////////
  //                        type==LAN_TX                        //
  ////////////////////////////////////////////////////////////////

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
  
  ////////////////////////////////////////////////////////////////
  //                        type==MARKER                        //
  ////////////////////////////////////////////////////////////////

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
  
  ////////////////////////////////////////////////////////////////
  //                        type==DSI_RX                        //
  ////////////////////////////////////////////////////////////////

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
  
  ////////////////////////////////////////////////////////////////
  //                        type==DSI_TX                        //
  ////////////////////////////////////////////////////////////////

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
  
  ////////////////////////////////////////////////////////////////
  //                        type==RIM_RX                        //
  ////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////
  //                        type==RIM_TX                        //
  ////////////////////////////////////////////////////////////////

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
  
  ////////////////////////////////////////////////////////////////
  //                   ONPI Metadata Union                      //
  ////////////////////////////////////////////////////////////////

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

endpackage
