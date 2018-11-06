//-----------------------------------------------------------------------------
// Title         : Madison Bay Tag Bus Functional Model Defines
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_tag_bfm_defines.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 01.11.2018
//-----------------------------------------------------------------------------
// Description :
// These are the TAG BFM definitions
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
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
`ifndef __MBY_TAG_BFM_PKG__
`error "Attempt to include file outside of mby_tag_bfm_pkg."
`endif
`ifndef __MBY_TAG_BFM_DEFINES__
`define __MBY_TAG_BFM_DEFINES__
// Definitions & local parameters

// Sizes
localparam  W_BYTE      =               8; // Byte Size
localparam  W_WORD      =              64; // Bytes per Word
localparam  W_WORD_BITS = (W_BYTE*W_WORD); // Bits per Word
localparam  W_FLIT      =              64; // EPL data flit width
localparam  N_FLIT      =               8; // EPL data number of flits
localparam  W_SEG_PTR   =              20; // Segment Pointer Size
localparam  W_SEMA      =               4; // Semaphore Bits Size
localparam  W_WD_SEL    =               2; // Segment Word Selector Size

localparam  W_DQTAG_LENGTH     =         8; // Deep-Queue Tag Length of Packet Width
localparam  W_DQTAG_SRC_PORT   =         4; // Deep-Queue Tag IGR Source Port Width       //TODO maybe can be inherited
localparam  W_DQTAG_SRC_TC     =         3; // Deep-Queue Tag IGR Traffic Class Width     //TODO maybe can be inherited
localparam  W_DQTAG_SSL        =         3; // Deep-Queue Tag Switch Lifetime Limit Width //TODO maybe can be inherited
localparam  W_DQTAG_NEXT_LEN   =         2; // Deep-Queue Tag Next Length Width           //TODO maybe can be inherited

//// Low-Latency Tag Format
localparam  W_TAG_RING_TRACKS      = 32; // Tag Ring Tracks, 1 per EPP, 32 EPPs in MBY //TODO make parametrical
localparam  W_MCE_TAG_RING_TRACKS  =  4; // Tag Ring Tracks, 1 per EPP, 32 EPPs in MBY //TODO make parametrical
localparam  W_LLT_DST              = 16; // Low-Latency Tag Logical Destination width //FIXME width not final
localparam  W_LLT_TC               =  4; // Low-Latency Tag EGR Traffic Class width
localparam  W_LLT_MIRROR_DEST1     =  9; // Low-Latency Tag Mirror Copy Destination Port width
localparam  W_LLT_MIRROR_DEST2     =  2; // Low-Latency Tag Mirror Copy Destination Port for CP width //FIXME width not final
localparam  W_LLT_POLICER_IDX      = 14; // Low-Latency Tag Policer Index width //FIXME width not final




`endif
