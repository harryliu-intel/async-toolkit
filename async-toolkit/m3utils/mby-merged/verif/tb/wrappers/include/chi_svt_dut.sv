// -----------------------------------------------------------------------------
// Copyright(C) 2012 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Created By:  Raghu Prasad Gudla
// Created On:  08/21/2018
// Description: CHI DUT module

/**
 * Abstract:
 *   This module represents a DUT that has 2 CHI Protocol Interfaces.
 *   CHI Protocol interface 1 connects to an CHI RN, and all pins have the suffix _RN1.
 *   CHI Protocol interface 2 connects to an CHI SN, and all pins have the suffix _SN1. 
 *   The behavior of this module is to simply connect the two protocol interfaces i.e. Master/RN
 *   and Slave/SN by assigning the appropriate inputs on the Slave/SN side to the outputs on the  
 *   Master/RN side, and vice versa.
 */
//`ifndef GUARD_HDL_INTERCONNECT_V
//`define GUARD_HDL_INTERCONNECT_V

`include "svt_chi_port_defines.svi"

module chi_svt_dut
(
  //-----------------------------------------------------------------------
  // CHI Interface signals 
  //-----------------------------------------------------------------------  
  /**
   * Clock and resetn
   */
`ifdef SVT_CHI_ENABLE_MULTI_CLOCK
  rn_clk,
  sn_clk,
`else
  clk,
`endif
`ifdef SVT_CHI_ENABLE_MULTI_RESET 
  rn_resetn,
  sn_resetn,
`else 
  resetn,    
`endif
  /**
   * CHI RN1 side Interface
   */
  //-----------------------------------------------------------------------
  // Link Activation Status Signals
  //-----------------------------------------------------------------------
  TXSACTIVE_RN1,
  RXSACTIVE_RN1,

  //-----------------------------------------------------------------------
  // Link Activation Status Signals
  //-----------------------------------------------------------------------
  TXLINKACTIVEREQ_RN1,
  TXLINKACTIVEACK_RN1,
  RXLINKACTIVEREQ_RN1,
  RXLINKACTIVEACK_RN1,

  //-----------------------------------------------------------------------
  // TX Request Virtual Channel
  //-----------------------------------------------------------------------
  TXREQFLITPEND_RN1,
  TXREQFLITV_RN1,
  TXREQFLIT_RN1,
  TXREQLCRDV_RN1,

  //-----------------------------------------------------------------------
  // RX Response Virtual Channel
  //-----------------------------------------------------------------------
  RXRSPFLITPEND_RN1,
  RXRSPFLITV_RN1,
  RXRSPFLIT_RN1,
  RXRSPLCRDV_RN1,

  //-----------------------------------------------------------------------
  // RX Dat Virtual Channel
  //-----------------------------------------------------------------------
  RXDATFLITPEND_RN1,
  RXDATFLITV_RN1,
  RXDATFLIT_RN1,
  RXDATLCRDV_RN1,

  //-----------------------------------------------------------------------
  // RX Snoop Virtual Channel
  //-----------------------------------------------------------------------
  RXSNPFLITPEND_RN1,
  RXSNPFLITV_RN1,
  RXSNPFLIT_RN1,
  RXSNPLCRDV_RN1,

  //-----------------------------------------------------------------------
  // TX Response Virtual Channel
  //-----------------------------------------------------------------------
  TXRSPFLITPEND_RN1,
  TXRSPFLITV_RN1,
  TXRSPFLIT_RN1,
  TXRSPLCRDV_RN1,

  //-----------------------------------------------------------------------
  // TX Dat Virtual Channel
  //-----------------------------------------------------------------------
  TXDATFLITPEND_RN1,
  TXDATFLITV_RN1,
  TXDATFLIT_RN1,
  TXDATLCRDV_RN1,

  /**
   * CHI SN1 side Interface
   */  
  //-----------------------------------------------------------------------
  // Link Activation Status Signals
  //-----------------------------------------------------------------------
  TXSACTIVE_SN1,
  RXSACTIVE_SN1,

  //-----------------------------------------------------------------------
  // Link Activation Status Signals
  //-----------------------------------------------------------------------
  TXLINKACTIVEREQ_SN1,
  TXLINKACTIVEACK_SN1,
  RXLINKACTIVEREQ_SN1,
  RXLINKACTIVEACK_SN1,

  //-----------------------------------------------------------------------
  // RX Request Virtual Channel
  //-----------------------------------------------------------------------
  RXREQFLITPEND_SN1,
  RXREQFLITV_SN1,
  RXREQFLIT_SN1,
  RXREQLCRDV_SN1,

  //-----------------------------------------------------------------------
  // TX Response Virtual Channel
  //-----------------------------------------------------------------------
  TXRSPFLITPEND_SN1,
  TXRSPFLITV_SN1,
  TXRSPFLIT_SN1,
  TXRSPLCRDV_SN1,

  //-----------------------------------------------------------------------
  // TX Dat Virtual Channel
  //-----------------------------------------------------------------------
  TXDATFLITPEND_SN1,
  TXDATFLITV_SN1,
  TXDATFLIT_SN1,
  TXDATLCRDV_SN1,

  //-----------------------------------------------------------------------
  // RX Dat Virtual Channel
  //-----------------------------------------------------------------------
  RXDATFLITPEND_SN1,
  RXDATFLITV_SN1,
  RXDATFLIT_SN1,
  RXDATLCRDV_SN1
);

  /**
   * Clock and Reset signals
   */
`ifdef SVT_CHI_ENABLE_MULTI_CLOCK
  input  rn_clk[`SVT_CHI_MAX_NUM_RNS-1:0];
  input  sn_clk[`SVT_CHI_MAX_NUM_SNS-1:0];
`else
  input  clk;
`endif
`ifdef SVT_CHI_ENABLE_MULTI_RESET
  input  rn_resetn[`SVT_CHI_MAX_NUM_RNS-1:0];
  input  sn_resetn[`SVT_CHI_MAX_NUM_SNS-1:0];
`else  
  input resetn;
`endif
  /**
   * CHI Protocol Interface RN1 - Dataflow signals
   */  
  // Link Activation Status Signals
  input   TXSACTIVE_RN1;
  output  RXSACTIVE_RN1;

  // Link Activation Status Signals
  input   TXLINKACTIVEREQ_RN1;
  output  TXLINKACTIVEACK_RN1;
  output  RXLINKACTIVEREQ_RN1;
  input   RXLINKACTIVEACK_RN1;

  // TX Request Virtual Channel
  input   TXREQFLITPEND_RN1;
  input   TXREQFLITV_RN1;
  input   [`SVT_CHI_MAX_REQ_FLIT_WIDTH-1:0] TXREQFLIT_RN1;
  output  TXREQLCRDV_RN1;

  // RX Response Virtual Channel
  output  RXRSPFLITPEND_RN1;
  output  RXRSPFLITV_RN1;
  output  [`SVT_CHI_MAX_RSP_FLIT_WIDTH-1:0] RXRSPFLIT_RN1;
  input   RXRSPLCRDV_RN1;

  // RX Dat Virtual Channel
  output  RXDATFLITPEND_RN1;
  output  RXDATFLITV_RN1;
  output  [`SVT_CHI_MAX_DAT_FLIT_WIDTH-1:0] RXDATFLIT_RN1;
  input   RXDATLCRDV_RN1;

  // RX Snoop Virtual Channel
  output  RXSNPFLITPEND_RN1;
  output  RXSNPFLITV_RN1;
  output  [`SVT_CHI_MAX_SNP_FLIT_WIDTH-1:0] RXSNPFLIT_RN1;
  input   RXSNPLCRDV_RN1;

  // TX Response Virtual Channel
  input  TXRSPFLITPEND_RN1;
  input  TXRSPFLITV_RN1;
  input  [`SVT_CHI_MAX_RSP_FLIT_WIDTH-1:0] TXRSPFLIT_RN1;
  output reg TXRSPLCRDV_RN1;

  // TX Dat Virtual Channel
  input  TXDATFLITPEND_RN1;
  input  TXDATFLITV_RN1;
  input  [`SVT_CHI_MAX_DAT_FLIT_WIDTH-1:0] TXDATFLIT_RN1;
  output TXDATLCRDV_RN1;

  /**
   * CHI Protocol Interface SN1 - Dataflow signals
   */  
  // Link Activation Status Signals
  input  TXSACTIVE_SN1;
  output RXSACTIVE_SN1;

  // Link Activation Status Signals
  input  TXLINKACTIVEREQ_SN1;
  output TXLINKACTIVEACK_SN1;
  output RXLINKACTIVEREQ_SN1;
  input  RXLINKACTIVEACK_SN1;

  // RX Request Virtual Channel
  output  RXREQFLITPEND_SN1;
  output  RXREQFLITV_SN1;
  output  [`SVT_CHI_MAX_REQ_FLIT_WIDTH-1:0] RXREQFLIT_SN1;
  input   RXREQLCRDV_SN1;

  // TX Response Virtual Channel
  input  TXRSPFLITPEND_SN1;
  input  TXRSPFLITV_SN1;
  input  [`SVT_CHI_MAX_RSP_FLIT_WIDTH-1:0] TXRSPFLIT_SN1;
  output TXRSPLCRDV_SN1;

  // TX Dat Virtual Channel
  input  TXDATFLITPEND_SN1;
  input  TXDATFLITV_SN1;
  input  [`SVT_CHI_MAX_DAT_FLIT_WIDTH-1:0] TXDATFLIT_SN1;
  output TXDATLCRDV_SN1;

  // RX Dat Virtual Channel
  output RXDATFLITPEND_SN1;
  output RXDATFLITV_SN1;
  output [`SVT_CHI_MAX_DAT_FLIT_WIDTH-1:0] RXDATFLIT_SN1;
  input  RXDATLCRDV_SN1;

// ===================================================================================================
// Appropriate inputs from CHI Interface RN1 are connected to outputs on CHI Interface SN1 and viceversa
// ---------------------------------------------------------------------------------------------------

  // Link Activation Status Signal connections.
  assign RXSACTIVE_RN1 = TXSACTIVE_SN1;
  assign RXSACTIVE_SN1 = TXSACTIVE_RN1;
  assign TXLINKACTIVEACK_RN1 = RXLINKACTIVEACK_SN1;
  assign RXLINKACTIVEREQ_RN1 = TXLINKACTIVEREQ_SN1;
  assign TXLINKACTIVEACK_SN1 = RXLINKACTIVEACK_RN1;
  assign RXLINKACTIVEREQ_SN1 = TXLINKACTIVEREQ_RN1;
  // TX-RX Request Virtual Channel signal connections.
  assign RXREQFLITPEND_SN1 = TXREQFLITPEND_RN1;
  assign RXREQFLITV_SN1 = TXREQFLITV_RN1;
  assign RXREQFLIT_SN1 = TXREQFLIT_RN1;
  assign TXREQLCRDV_RN1 = RXREQLCRDV_SN1;
  // TX-RX Response Virtual Channel signal connections.
  assign RXRSPFLITPEND_RN1 = TXRSPFLITPEND_SN1;
  assign RXRSPFLITV_RN1 = TXRSPFLITV_SN1;
  assign RXRSPFLIT_RN1 = TXRSPFLIT_SN1;
  assign TXRSPLCRDV_SN1 = RXRSPLCRDV_RN1;
  // TX-RX Dat Virtual Channel signal connections.
  assign RXDATFLITPEND_RN1 = TXDATFLITPEND_SN1;
  assign RXDATFLITV_RN1 = TXDATFLITV_SN1;
  assign RXDATFLIT_RN1 = TXDATFLIT_SN1;
  assign TXDATLCRDV_SN1 = RXDATLCRDV_RN1;
  assign RXDATFLITPEND_SN1 = TXDATFLITPEND_RN1;
  assign RXDATFLITV_SN1 = TXDATFLITV_RN1;
  assign RXDATFLIT_SN1 = TXDATFLIT_RN1;
  assign TXDATLCRDV_RN1 = RXDATLCRDV_SN1;
  // RX Snoop Virtual Channel
  assign RXSNPLCRDV_RN1 = 1'b0;

  initial begin
    // Tx Rsp Virtual Channel
    TXRSPLCRDV_RN1 <= 1'b0;
  end
  
endmodule
// =============================================================================
//`endif //GUARD_HDL_INTERCONNECT_V

