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
// and confidential information of Intel or its suppliers and licensors. The
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
//   Author        : 
//   Project       : 
//   Description   : 
//------------------------------------------------------------------------------


`define MBY_N_PARSER_KEYS         80 //parser_extract_cfg_rf_PARSER_EXTRACT_CFG__nd 
`define MBY_N_PARSER_FLGS         48
`define MBY_N_PARSER_PTRS         8

typedef struct  {
   bit [15:0] RX_PORT;
   int  RX_LENGHT;
   byte RX_DATA[`MAX_PKT_LEN];
} mbyRxMacToParser;

typedef struct {
    byte                    OFFSET      [`MBY_N_PARSER_PTRS]; // offsets to data of interest within packet
    bit                     OFFSET_VALID[`MBY_N_PARSER_PTRS]; // parser offset valid flags
    byte                    PROT_ID     [`MBY_N_PARSER_PTRS]; // parser protocol IDs
} mbyParserHdrPtrs;

typedef struct {
   bit [15:0]              PA_ADJ_SEG_LEN;                   ///< Adjusted segment length
   byte                    PA_CSUM_OK;                       ///< Checksum OK result for outer (bit 0) and inner (bit 1) IPv4 headers
   bit                     PA_DROP;                          ///< Checksum validation error, drop pkt in tail
   bit                     PA_EX_DEPTH_EXCEED;               ///< Parser stopped: EOS exception and segment was not EOP
   bit                     PA_EX_PARSING_DONE;               ///< Parser stopped: Parsing Done exception
   byte                    PA_EX_STAGE;                      ///< Analyzer stage where exception occurred
   bit                     PA_EX_TRUNC_HEADER;               ///< Parser stopped: EOS exception and segment was EOP:
   bit                     PA_FLAGS     [`MBY_N_PARSER_FLGS]; ///< Parser flags assigned by extract
   bit [15:0]              PA_KEYS      [`MBY_N_PARSER_KEYS]; ///< 16-bit parser keys
   bit                     PA_KEYS_VALID[`MBY_N_PARSER_KEYS]; ///< Parser keys valid flags
   bit                     PA_L3LEN_ERR;                     ///< L3 length error
   bit [15:0]              PA_PACKET_TYPE;                   ///< Packet type (new for MBY)
   mbyParserHdrPtrs        PA_HDR_PTRS;                      ///< Parser header pointers
   int                     RX_PORT;                          ///< Ingress port
   int                     RX_LENGTH;                        ///< Ingress packet data length [bytes]
   byte                    RX_DATA[`MAX_PKT_LEN];            ///< Ingress (receive) packet data


} mbyParserToMapper;

