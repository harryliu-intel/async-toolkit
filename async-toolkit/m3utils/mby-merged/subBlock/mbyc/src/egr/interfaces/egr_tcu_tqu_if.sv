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
//------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 

/** INTERFACE: egr_tcu_tqu_if
* TQU to TCU Interface
*
* For connecting the Transmit Queuing Unit to the Transmit Controller Unit
* 
*/
interface egr_tcu_tqu_if ();
    import mby_egr_pkg::*;
    import shared_pkg::*;

////////////////// PARAMS AND STRUCTS
// Service dtq_ctrl_pull
localparam W_EOP_WD_LEN = $clog2(W_WORD); // 64 possible lengths (6)
localparam W_TAG_MD     = 4;              // Tag Mdata bits: SLL, ERR, XMD, MC/UC (4)
localparam W_PKT_ID     = 9;              // Packet ID (9)
localparam N_MAX_LP     = 4;              // Max Number of Logic Ports (4)
localparam W_DTQ_SEL    = $clog2(MGP_TC_CNT*N_MAX_LP); // Data Transmit Queue select (6)

typedef struct packed {
    logic                 ctrl_type; // [21]    Type of control word: 0: metadata. 1: header
    logic                       eoh; // [20]    End of Header bit for control word
    logic                       eop; // [19]    EOP bit for control word
    logic [W_EOP_WD_LEN-1:0] length; // [18:13] Number of valid bytes in EOP word
    logic [W_TAG_MD-1:0]  tag_mdata; // [12:9]  Tag Metadata bits
    logic [W_PKT_ID-1:0]     pkt_id; // [8:0]   Packet ID
} ctrl_mdata_t;

typedef struct packed {
    logic [W_DTQ_SEL-1:0] dtq_sel; // DTQ Ctrl select
    logic                peek_pop; // DTQ Ctrl operation. 0:Peek. 1:Pop
    logic                     req; // DTQ Ctrl Request
} dtq_ctrl_pull_t;

//Service dtq_data_pull
localparam N_MAX_PKT_WORDS = 160; // Jumbo packet size in words (160)
localparam W_PKT_LEN_WDS = $clog2(N_MAX_PKT_WORDS); // Packet length in words (8)

typedef struct packed {
    logic                        eop; // [9]   EOP bit
    logic               length_valid; // [8]   Length valid when sending length and not eop
    logic [W_PKT_LEN_WDS-1:0] length; // [7:0] Number of packet words (eop==0) or bytes in eop word (eop==1)
} pkt_word_mdata_t;

typedef struct packed {
    logic [W_DTQ_SEL-1:0] dtq_sel; // DTQ select
    logic                peek_pop; // DTQ operation. 0:Peek. 1:Pop
    logic                     req; // DTQ Request
} dtq_data_pull_t;

////////////////// SIGNALS
// Service dtq_ctrl_pull
dtq_ctrl_pull_t  [EPL_PER_MGP-1:0]   dtq_ctrl_pull; // TCU DTQ Control Pull Request
ctrl_mdata_t     [EPL_PER_MGP-1:0]      ctrl_mdata; // TQU Control Word Metadata
data_word_t      [EPL_PER_MGP-1:0]       ctrl_word; // TQU Control Word Data
logic            [EPL_PER_MGP-1:0] ctrl_word_valid; // TQU Control Word Valid

//Service dtq_data_pull
dtq_data_pull_t  [EPL_PER_MGP-1:0]   dtq_data_pull; // TQU DTQ Data Pull Request
pkt_word_mdata_t [EPL_PER_MGP-1:0]  pkt_word_mdata; // TQU Pkt Word Metadata
data_word_t      [EPL_PER_MGP-1:0]        pkt_word; // TQU Pkt Data Word
logic            [EPL_PER_MGP-1:0] data_word_valid; // TQU Data Word Valid


////////////////// MODPORTS
// Requestor of data
modport tcu(
    //Service dtq_ctrl_pull
    output  dtq_ctrl_pull,
    input      ctrl_mdata,
    input       ctrl_word,
    input ctrl_word_valid,
    //Service dtq_data_pull
    output  dtq_data_pull,
    input  pkt_word_mdata,
    input        pkt_word,
    input data_word_valid
    );

// Provider of data and control (metadata) words
modport tqu(
    //Service dtq_ctrl_pull
    input    dtq_ctrl_pull,
    output      ctrl_mdata,
    output       ctrl_word,
    output ctrl_word_valid,
    //Service dtq_data_pull
    input    dtq_data_pull,
    output  pkt_word_mdata,
    output        pkt_word,
    output data_word_valid
    );

endinterface : egr_tcu_tqu_if

