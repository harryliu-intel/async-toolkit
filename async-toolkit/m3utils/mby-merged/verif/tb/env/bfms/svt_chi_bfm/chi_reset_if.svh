// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI Reset Interface
// ------------------------------------------------------------------------------------------------------------------------


interface ChiResetIf();

  bit clk;
  bit rstn;
`ifdef SVT_CHI_ENABLE_MULTI_RESET
  logic rn_resetn[`SVT_CHI_MAX_NUM_RNS-1:0];
  logic sn_resetn[`SVT_CHI_MAX_NUM_SNS-1:0];
`endif
  modport chi_reset_modport (
                             input   clk,
                             output   rstn
`ifdef SVT_CHI_ENABLE_MULTI_RESET
                             ,
                             output rn_resetn,
                             output sn_resetn
`endif
                             );
endinterface

