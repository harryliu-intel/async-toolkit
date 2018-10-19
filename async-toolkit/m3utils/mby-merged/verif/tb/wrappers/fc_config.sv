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

//config `HDL_TOP_CFG;
config fc_hdl_top_cfg;

    design `HDL_TOP_LIB.`HDL_TOP;

    `ifdef FC_MODEL
        `include "fc_config_include.svh";
    `endif

    `ifdef MPP_8
        instance `soc.mby_mpp_1 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_2 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_3 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_4 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_5 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_6 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_7 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_8 liblist mby_mpp_rtl_lib;
    `elsif MPP_2
        instance `soc.mby_mpp_1 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_2 liblist mby_mpp_rtl_lib;
        instance `soc.mby_mpp_3 liblist soc_ip_stub_lib;
        instance `soc.mby_mpp_4 liblist soc_ip_stub_lib;
        instance `soc.mby_mpp_5 liblist soc_ip_stub_lib;
        instance `soc.mby_mpp_6 liblist soc_ip_stub_lib; 
        instance `soc.mby_mpp_7 liblist soc_ip_stub_lib;
        instance `soc.mby_mpp_8 liblist soc_ip_stub_lib;
    `endif

    `ifdef EPC_8
        instance `soc.mby_ec_top_1 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_2 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_3 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_4 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_5 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_6 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_7 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_8 liblist mby_ec_top_lib;
    `elsif EPC_2
        instance `soc.mby_ec_top_1 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_2 liblist mby_ec_top_lib;
        instance `soc.mby_ec_top_3 liblist soc_ip_stub_lib;
        instance `soc.mby_ec_top_4 liblist soc_ip_stub_lib;
        instance `soc.mby_ec_top_5 liblist soc_ip_stub_lib;
        instance `soc.mby_ec_top_6 liblist soc_ip_stub_lib; 
        instance `soc.mby_ec_top_7 liblist soc_ip_stub_lib;
        instance `soc.mby_ec_top_8 liblist soc_ip_stub_lib;

    `endif

    `ifdef MPP_WITH_SERDES
    `endif

    `ifdef EPC_WITH_SERDES
    `endif

  //   cell pmusb use `HDL_TOP_LIB.pmu_sb_cfg;
  //      `include "axi_svt_dut.sv"
endconfig


