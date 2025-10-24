// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Jerry Shaw
// Created On   :  09/24/2018
// Description  :  PCIe test island instantiation and connectivity
//------------------------------------------------------------------------------
// There are three components instantiated in the tb module:
//           DUT         (pcie_device.v)
//      denali_monitor   (pcie_monitor.v)
//       denali_model    (pcie_model.v)
//
//  +-----------------+           +-----------------+
//  |                 |           |                 |
//  |                 |  PCI      |                 |
//  |     Root        | <=======> |     Endpoint    |--+
//  |     Complex     |  Express  |                 |  |
//  |                 |           |                 |  |
//  | denali_model    |           | DUT             |  |
//  +-----------------+           +-----------------+  |
//                                   |                 |
//                                   | denali_mointor  |
//                                   +-----------------+
`ifdef CDN_PCIE_ENV_ENABLE

    `define DENALI_GEN3_DUT
    `define DENALI_GEN3_MODEL
    `define DEFAULT_LINK_WIDTH 8

    `ifdef LINK_WIDTH
    `else
    `define LINK_WIDTH `DEFAULT_LINK_WIDTH
    `endif

    `define DEFAULT_SOMA_PATH "$DENALI/ddvapi/sv/uvm/pcie/examples/ep_dut/soma_uvm"
    `ifdef DENALI_PS_SOMA
    `define SOMA_PATH_0 "$DENALI_PS_SOMA"
    `else
    `define SOMA_PATH_0 `DEFAULT_SOMA_PATH
    `endif
    `define DENALI_DUT_VER "g3_x"
    `define DENALI_MODEL_VER "g3_x"

    `ifdef DUT
        // DUT is not Denali
    `else
        `define DUT_IS_DENALI
    `endif


    `include "uvm_macros.svh"

    import uvm_pkg::*;

    import DenaliSvPcie::*;
    import DenaliSvMem::*;

    // Include the VIP UVM base classes
    import cdnPcieUvm::*;


    parameter linkWidth = `LINK_WIDTH;
    parameter soma_dir = `SOMA_PATH_0;
    parameter linkWidthStr = (linkWidth==32) ? "32"
                           : (linkWidth==16) ? "16"
                           : (linkWidth==12) ? "12"
                           : (linkWidth==8)  ? "08"
                           : (linkWidth==4)  ? "04"
                           : (linkWidth==2)  ? "02"
                           : (linkWidth==1)  ? "01"
                           : "XX";

    wire [(linkWidth-1):0] TX, TX_;
    wire [(linkWidth-1):0] RX, RX_;

    // define clock used by test cases
    // 250MHz clock from denali_model
    //wire clk = denali_model.CLK_TX;
    clkgen_if #(.PERIOD(4000), .DELAY(0), .DUTYCYCLE(50)) pcie_ref_clk(); 

    wire clk = pcie_ref_clk.clk; // denali_model.CLK_TX;
    wire aClk = denali_model.CLK_TX;
    wire CLK_TX;
    wire CLK_RX;

    // system reset
    reg PERST_n;
    initial begin
         PERST_n = 0;
         #10;
         PERST_n = 1;
    end


    `ifdef DUT_IS_DENALI
        // denali dut
        defparam DUT.interface_soma = {soma_dir,"/pcie_device_ep_with_reset_", `DENALI_DUT_VER, linkWidthStr,".spc"};
        pcie_device_with_reset #(linkWidth) DUT (
        //pcie_ep_bfm_phy DUT (
            .CLK_TX(),
            .CLK_RX(),
            .PERST_n(PERST_n),
            .TX(TX),
            .TX_(TX_),
            .RX(RX),
            .RX_(RX_)
        );

    `else
        // real dut
//        instantiate_actual_device_here #(linkWidth) DUT (
//            .PERST_(PERST_),
//            .TX(TX),
//            .TX_(TX_),
//            .RX(RX),
//            .RX_(RX_)
//        );
    `endif

    // monitor - between dut and bfm
    defparam denali_monitor.interface_soma = {soma_dir,"/pcie_monitor_ep_with_reset_",`DENALI_DUT_VER,linkWidthStr,".spc"};
    pcie_monitor_with_reset #(linkWidth) denali_monitor (
    //pcie_ep_mon_phy denali_monitor (
        .CLK_TX(),
        .CLK_RX(),
        .PERST_n(PERST_n),
        .TX(TX),
        .TX_(TX_),
        .RX(RX),
        .RX_(RX_)
      );

    // model - bfm
    defparam denali_model.interface_soma = {"cfg_0_0:",soma_dir,"/pcie_model_rc_with_reset_",`DENALI_MODEL_VER, linkWidthStr,".spc",
                                            " p_0.cfg_0_0:",soma_dir,"/pcie_device_ep_with_reset_",`DENALI_DUT_VER,linkWidthStr,".spc"};
    pcie_model_with_reset #(linkWidth) denali_model (
    //pcie_rc_bfm_phy denali_model (
        .CLK_TX(CLK_TX),
        .CLK_RX(CLK_RX),
        .PERST_n(PERST_n),
        .TX(RX),   // notice rx/tx swap
        .TX_(RX_),
        .RX(TX),
        .RX_(TX_)
      );

`endif


// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

