// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Full-Chip Config stub
// -----------------------------------------------------------------------------

//`include "fc_liblist.svh"
`ifdef AXI_ENV_ENABLE
`include "axi_svt_dut.sv"
`endif
`ifdef APB_ENV_ENABLE
`include "apb_svt_dut.sv"
`endif
`ifdef CHI_ENV_ENABLE
`include "chi_svt_dut.sv"
`endif

//config pmu_sb_cfg;
//    design pmu_rtl_lib.pmusb;
//    cell pmu_sbebase liblist pmu_rtl_lib;
//endconfig

config `HDL_TOP_CFG;

    design `HDL_TOP_LIB.`HDL_TOP;

    `ifdef FC_MODEL
//TODO        `include "fc_config_include.svh";
    `endif
  //   cell pmusb use `HDL_TOP_LIB.pmu_sb_cfg;
  //      `include "axi_svt_dut.sv"
endconfig


