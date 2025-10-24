/* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. */
/* SPDX-License-Identifier: Apache-2.0 */

// -*- mode:c -*-

// Copyright (C) 2019 Intel Corporation

#ifndef MBY_PAR2MAP_H
#define MBY_PAR2MAP_H

#include "mby_parserToMapper.h"

#include "mby_par_hdr_ptrs.h" // mbyParserHdrPtrs

typedef mby_parserToMapper mbyParserToMapper;

#define MBY_N_PARSER_FLGS MBY_N_PARSER_FLAGS








#if 0

#include "fm_types.h"

#define MBY_N_PARSER_KEYS  parser_extract_cfg_rf_PARSER_EXTRACT_CFG__nd // 80
#define MBY_N_PARSER_FLGS  48

typedef struct mbyParserToMapperStruct
{
    fm_uint16        PA_ADJ_SEG_LEN;                   ///< Adjusted segment length
    fm_byte          PA_CSUM_OK;                       ///< Checksum OK result for outer (bit 0) and inner (bit 1) IPv4 headers
    fm_bool          PA_DROP;                          ///< Checksum validation error, drop pkt in tail
    fm_bool          PA_EX_DEPTH_EXCEED;               ///< Parser stopped: EOS exception and segment was not EOP
    fm_bool          PA_EX_PARSING_DONE;               ///< Parser stopped: Parsing Done exception
    fm_byte          PA_EX_STAGE;                      ///< Analyzer stage where exception occurred
    fm_bool          PA_EX_TRUNC_HEADER;               ///< Parser stopped: EOS exception and segment was EOP:
    fm_bool          PA_FLAGS     [MBY_N_PARSER_FLGS]; ///< Parser flags assigned by extract
    fm_uint16        PA_KEYS      [MBY_N_PARSER_KEYS]; ///< 16-bit parser keys
    fm_bool          PA_KEYS_VALID[MBY_N_PARSER_KEYS]; ///< Parser keys valid flags
    fm_bool          PA_L3LEN_ERR;                     ///< L3 length error
    fm_uint16        PA_PACKET_TYPE;                   ///< Packet type (new for MBY)
    mbyParserHdrPtrs PA_HDR_PTRS;                      ///< Parser header pointers
    fm_uint32        RX_PORT;                          ///< Ingress port
    fm_uint32        RX_LENGTH;                        ///< Ingress packet data length [bytes]

} mbyParserToMapper;
#endif

#endif
