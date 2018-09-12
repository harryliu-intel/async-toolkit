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

localparam IGR_PPE_ERR_WIDTH        = 2;        //IGR to PPE error field width
localparam IGR_PPE_PKT_LEN_WIDTH    = 14;       //packet length field width
localparam MGP_PKT_ID_CNT           = 32768;    //MGP packet ID count
localparam IGR_PPE_TS_WIDTH         = 64;       //IGR to PPE timestamp field width
localparam MGP_TC_CNT               = 9;        //MGP traffic class count
localparam MGP_PORT_CNT             = 17;       //MGP port count, 1 for CP/CPP
localparam IGR_PPE_ECC_WIDTH        = 72;       //IGR to PPE ecc field width
localparam IGR_PPE_DATA_WIDTH       = 1024;     //IGR to PPE data field width
localparam MC_GROUP_CNT             = 16384;    //multicast group count
localparam POLICER_CNT              = 4096;     //number of policers

typedef struct packed {
    logic   [IGR_PPE_ERR_WIDTH-1:0]         err;    //tail information CRC error notification
    logic   [IGR_PPE_PKT_LEN_WIDTH-1:0]     len;    //tail information packet length
    logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;     //tail information packet ID
    logic                                   valid;  //tail information valid
} igr_rx_ppe_tail_t;

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
    igr_rx_ppe_md_t                     md;     //header segment metadata
    logic   [IGR_PPE_ECC_WIDTH-1:0]     ecc;    //header segment ECC
    logic   [IGR_PPE_DATA_WIDTH-1:0]    data;   //header segment data
    logic                               valid;  //header segment valid
} igr_rx_ppe_head_t;

//PPE to ingress packet type
typedef struct packed {
    logic   mc;     //when set indicates a multicast packet
    logic   cpp;    //when set indicates need to mirror packet to CPP/CP
    logic   mirror; //when set indicates need to mirror packet to a normal port
} rx_ppe_igr_pkt_type_t;

// Unicast/MC Packet metadata
typedef struct packed {
    logic   [121:0]                 reserved;
    logic   [3:0]                   packet_type;
    logic   [23:0]                  mod_profile;
    logic   [19:0]                  mod_md;
    logic   [11:0]                  ivid;
    logic   [11:0]                  evid;
    logic   [3:0]                   vpri;
    logic   [1:0]                   tx_tag;
    logic   [3:0]                   tag_flags;      //parser flags
    logic   [47:0]                  dmac;
    logic   [7:0]                   rx_port;        //source port
    logic   [5:0]                   l3d;            //L3 domain
    logic   [IGR_PPE_TS_WIDTH-1:0]  igr_ts;         //Ingress timestamp
    logic   [7:0]                   l3_mcgrpid;
    logic                           tx_drop;
    logic   [5:0]                   dscp;
    logic   [3:0]                   ttl_ctl;
    logic   [1:0]                   ecn_val;
    logic                           aqm_mark_en;
    logic   [63:0]                  hdr_ptr;
    logic   [63:0]                  hdr_ptr_offset;
} rx_ppe_igr_port_md_t;

// CPP Packet metadata
typedef struct packed {
    logic   [4:0]                   packet_type;
    logic   [7:0]                   dest_port;
    logic   [23:0]                  mod_profile;
    logic   [31:0]                  mod_md;
    logic   [11:0]                  ivid1;
    logic   [11:0]                  ivid2;
    logic   [11:0]                  evid1;
    logic   [11:0]                  evid2;
    logic   [3:0]                   vpri1;
    logic   [3:0]                   vpri2;
    logic   [1:0]                   tx_tag;
    logic   [3:0]                   tag_flags;      //parser flags
    logic   [47:0]                  dmac;
    logic   [7:0]                   sglort;
    logic   [15:0]                  dglort;
    logic   [31:0]                  user_id;
    logic   [5:0]                   l3d;            //L3 domain
    logic   [IGR_PPE_TS_WIDTH-1:0]  igr_ts;         //Ingress timestamp
    logic   [7:0]                   l3_mcgrpid;
    logic   [31:0]                  pol_res;
    logic   [3:0]                   dscp_ctl;
    logic   [3:0]                   ttl_ctl;
    logic   [3:0]                   tc_ctl;
    logic   [3:0]                   ecn_ctl;
    logic   [63:0]                  hdr_ptr;
    logic   [55:0]                  hdr_ptr_offset;
} rx_ppe_igr_cpp_md_t;

typedef union packed {
    rx_ppe_igr_port_md_t    cpp_md;     //CPP metadata
    rx_ppe_igr_port_md_t    port_md;    //Normal port metadata
} rx_ppe_igr_md_t;

typedef struct packed {
    rx_ppe_igr_md_t                         md;             //metadata
    logic   [$clog2(POLICER_CNT)-1:0]       policer_idx;    //Policer index
    logic   [$clog2(MGP_TC_CNT)-1:0]        tc;             //Egress TC
    logic   [$clog2(MGP_PORT_CNT)-1:0]      port;           //egress port ID
    logic   [2:0]                           pkt_type;       //one hot packet type indication, bit2:INT, bit1:CPP, bit0:normal
    logic   [$clog2(MGP_PKT_ID_CNT)-1:0]    id;             //header segment packet ID
    logic                                   valid;          //valid
} rx_ppe_igr_t;

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
