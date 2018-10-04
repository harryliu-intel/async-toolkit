// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  APB Reset interface 
// ------------------------------------------------------------------------------------------------------------------------


interface ApbResetIf();

  logic presetn;
  logic pclk;

  modport apb_reset_modport (input pclk, output presetn);

endinterface

