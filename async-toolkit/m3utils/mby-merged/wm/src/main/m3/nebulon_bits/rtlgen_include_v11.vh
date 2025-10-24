// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

`ifndef RTLGEN_INC_V11
`define RTLGEN_INC_V11

//lintra push -68094

// ===================================================
// Sidebande IOSF and Nebulon req, ack conversions 

`ifndef RTLGEN_SAI_SB_TO_CR
`define RTLGEN_SAI_SB_TO_CR(sb_sai) \
   f_sai_sb_to_cr(sb_sai)
`endif // RTLGEN_SAI_SB_TO_CR

// sai_bits has two versions:
// old: treg_ext_header[15:8]
// new: treg_rx_sai 
`ifndef RTLGEN_CR_REQ_FROM_SB
`define RTLGEN_CR_REQ_FROM_SB(cr_req,sb_prf,sai_data) \
   assign cr_req.valid = ``sb_prf``treg_irdy; \
   assign cr_req.opcode = cfg_opcode_t'(``sb_prf``treg_opcode[3:0]); \
   assign cr_req.bar = ``sb_prf``treg_bar; \
   assign cr_req.be = ``sb_prf``treg_be;  \
   assign cr_req.fid = ``sb_prf``treg_fid;  \
   assign cr_req.addr = ``sb_prf``treg_addrlen ? (48'b0 | ``sb_prf``treg_addr) :  (48'b0 | ``sb_prf``treg_addr[15:0]); \
   assign cr_req.data = ``sb_prf``treg_wdata; \
   assign cr_req.sai[7:0] = `RTLGEN_SAI_SB_TO_CR((``sb_prf``treg_ext_header[6:0] != 7'h0) || !``sb_prf``treg_eh || ``sb_prf``treg_eh_discard  ? 8'h0 :  ``sb_prf````sai_data``); 
`endif // RTLGEN_CR_REQ_FROM_SB

`ifndef RTLGEN_CR_REQ_FROM_SB_SIGNALS
`define RTLGEN_CR_REQ_FROM_SB_SIGNALS(cr_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,,treg_rx_sai) 
`endif // RTLGEN_CR_REQ_FROM_SB_SIGNALS

`ifndef RTLGEN_CR_REQ_FROM_SB_REQ
`define RTLGEN_CR_REQ_FROM_SB_REQ(cr_req,sb_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,``sb_req``.,treg_rx_sai) 
`endif // RTLGEN_CR_REQ_FROM_SB

`ifndef RTLGEN_CR_REQ_FROM_SB_SIGNALS_OLD
`define RTLGEN_CR_REQ_FROM_SB_SIGNALS_OLD(cr_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,,treg_ext_header[15:8]) 
`endif // RTLGEN_CR_REQ_FROM_SB_SIGNALS

`ifndef RTLGEN_CR_REQ_FROM_SB_REQ_OLD
`define RTLGEN_CR_REQ_FROM_SB_REQ_OLD(cr_req,sb_req) \
   `RTLGEN_CR_REQ_FROM_SB(cr_req,``sb_req``.,treg_ext_header[15:8]) 
`endif // RTLGEN_CR_REQ_FROM_SB

// Notice treg_cerr changes:
// OLD: treg_cerr = (cr_ack.read_miss | cr_ack.write_miss | ~cr_ack.sai_successfull);
// NEW: treg_cerr = ~cr_ack.sai_successfull;
// According to IOSF chassis spec, for non-existent register, non-posted register request should report success.
`ifndef RTLGEN_SB_FROM_CR_ACK
`define RTLGEN_SB_FROM_CR_ACK(cr_ack,sb_prf) \
   ``sb_prf``treg_trdy = (cr_ack.read_valid | cr_ack.write_valid); \
   ``sb_prf``treg_cerr = ~cr_ack.sai_successfull; \
   ``sb_prf``treg_rdata = cr_ack.data;
`endif // RTLGEN_SB_FROM_CR_ACK

`ifndef RTLGEN_SB_ACK_FROM_CR_ACK
`define RTLGEN_SB_ACK_FROM_CR_ACK(sb_ack,cr_ack) \
  always_comb begin                           \
   `RTLGEN_SB_FROM_CR_ACK(cr_ack,``sb_ack``.) \
  end                                                               
`endif // RTLGEN_SB_ACK_FROM_CR_ACK

`ifndef RTLGEN_SB_SIGNALS_FROM_CR_ACK
`define RTLGEN_SB_SIGNALS_FROM_CR_ACK(cr_ack) \
  always_comb begin                \
   `RTLGEN_SB_FROM_CR_ACK(cr_ack,) \
  end                                                               
`endif // RTLGEN_SB_SIGNALS_FROM_CR_ACK

`ifndef RTLGEN_CR_ACK_LIST_TO_SB_ACK_LIST
`define RTLGEN_CR_ACK_LIST_TO_SB_ACK_LIST(cr_ack_list,sb_ack_list)     \
  always_comb begin                                                    \
     for (int i=0; i<$size(sb_ack_list); i++) begin                    \
        `RTLGEN_SB_FROM_CR_ACK(``cr_ack_list``[i],``sb_ack_list``[i].) \
     end                                                               \
  end                                                               
`endif // RTLGEN_CR_ACK_LIST_TO_SB_ACK_LIST                         

// ===================================================
// Misc Macros 

`ifndef RTLGEN_LCB
`define RTLGEN_LCB(clock,enable,gate_clk) \
always_comb gate_clk = clock & enable;
`endif // RTLGEN_LCB

`ifndef IMPORT_RTLGEN_LATEST_PKG 
`define IMPORT_RTLGEN_LATEST_PKG import rtlgen_pkg_v11::*; 
`endif

//lintra pop

`endif
