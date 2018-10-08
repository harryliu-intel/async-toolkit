// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Top level module for FC HDL TB environment
// -----------------------------------------------------------------------------


module fc_hdl_top #(

);

`include "ti_macro_defines.svh"

FcDutIf fctop_dut_if();

// DUT Instantiation
`ifndef MODEL_WITHOUT_DUT
`include "dut_connect.svh"
//soc soc();
`endif

// AXI DUT Instantiation
//`ifdef AXI_ENV_ENABLE
//`include "axi_svt_dut_sv_wrapper.svh"
//`endif

// APB DUT Instantiation
//`ifdef APB_ENV_ENABLE
//`include "apb_svt_dut_sv_wrapper.svh"
//`endif

// CHI DUT Instantiation
//`ifdef CHI_ENV_ENABLE
//`include "chi_svt_dut_sv_wrapper.svh"
//`endif

endmodule
