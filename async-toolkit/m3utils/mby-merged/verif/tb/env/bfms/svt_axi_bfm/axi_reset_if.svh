// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI Reset Interface
// ------------------------------------------------------------------------------------------------------------------------

interface AxiResetIf();

  logic reset;
  logic clk;

  modport axi_reset_modport (input clk, output reset);

endinterface

