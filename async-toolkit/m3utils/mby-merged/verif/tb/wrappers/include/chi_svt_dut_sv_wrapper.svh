// -----------------------------------------------------------------------------
// Copyright(C) 2012 Intel Corporation, Confidential Information
// -----------------------------------------------------------------------------
//
// Created By:  Raghu Prasad Gudla
// Created On:  08/21/2018
// Description: CHI DUT wrapper module
/**
 * Abstract: A HDL Interconnect wrapper that connects the Verilog HDL
 * Interconnect to the SystemVerilog interface.This is a SystemVerilog file
 * which should be created for the Verilog RTL DUT.
 */

//`include "hdl_interconnect.v"

//module basic_interconnect_sv_wrapper(svt_chi_if chi_if);

/* HDL Interconnect Instantiation */

chi_svt_dut  chi_svt_dut(
    //-----------------------------------------------------------------------
    // CHI Interface signals 
    //-----------------------------------------------------------------------
    /**
     * Clock
     */
`ifdef SVT_CHI_ENABLE_MULTI_CLOCK
    .rn_clk (fc_hvl_top.chi_vif.rn_clk),
    .sn_clk (fc_hvl_top.chi_vif.sn_clk),
`else		          
    .clk (fc_hvl_top.chi_vif.clk),
`endif
`ifdef SVT_CHI_ENABLE_MULTI_RESET		          
    .rn_resetn (fc_hvl_top.chi_vif.rn_resetn),
    .sn_resetn (fc_hvl_top.chi_vif.sn_resetn),
`else
    .resetn (fc_hvl_top.chi_vif.resetn),
`endif
    /**
     * CHI RN1 side Interface
     */
    //-----------------------------------------------------------------------
    // Link Activation Status Signals
    //-----------------------------------------------------------------------
    .TXSACTIVE_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXSACTIVE),
    .RXSACTIVE_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXSACTIVE),

    //-----------------------------------------------------------------------
    // Link Activation Status Signals
    //-----------------------------------------------------------------------
    .TXLINKACTIVEREQ_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXLINKACTIVEREQ),
    .TXLINKACTIVEACK_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXLINKACTIVEACK),
    .RXLINKACTIVEREQ_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXLINKACTIVEREQ),
    .RXLINKACTIVEACK_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXLINKACTIVEACK),

    //-----------------------------------------------------------------------
    // TX Request Virtual Channel
    //-----------------------------------------------------------------------
    .TXREQFLITPEND_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXREQFLITPEND),
    .TXREQFLITV_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXREQFLITV),
    .TXREQFLIT_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXREQFLIT),
    .TXREQLCRDV_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXREQLCRDV),

    //-----------------------------------------------------------------------
    // RX Response Virtual Channel
    //-----------------------------------------------------------------------
    .RXRSPFLITPEND_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXRSPFLITPEND),
    .RXRSPFLITV_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXRSPFLITV),
    .RXRSPFLIT_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXRSPFLIT),
    .RXRSPLCRDV_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXRSPLCRDV),

    //-----------------------------------------------------------------------
    // RX Dat Virtual Channel
    //-----------------------------------------------------------------------
    .RXDATFLITPEND_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXDATFLITPEND),
    .RXDATFLITV_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXDATFLITV),
    .RXDATFLIT_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXDATFLIT),
    .RXDATLCRDV_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXDATLCRDV),

    //-----------------------------------------------------------------------
    // RX Snoop Virtual Channel
    //-----------------------------------------------------------------------
    .RXSNPFLITPEND_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXSNPFLITPEND),
    .RXSNPFLITV_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXSNPFLITV),
    .RXSNPFLIT_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXSNPFLIT),
    .RXSNPLCRDV_RN1 (fc_hvl_top.chi_vif.rn_if[0].RXSNPLCRDV),

    //-----------------------------------------------------------------------
    // TX Response Virtual Channel
    //-----------------------------------------------------------------------
    .TXRSPFLITPEND_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXRSPFLITPEND),
    .TXRSPFLITV_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXRSPFLITV),
    .TXRSPFLIT_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXRSPFLIT),
    .TXRSPLCRDV_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXRSPLCRDV),

    //-----------------------------------------------------------------------
    // TX Dat Virtual Channel
    //-----------------------------------------------------------------------
    .TXDATFLITPEND_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXDATFLITPEND),
    .TXDATFLITV_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXDATFLITV),
    .TXDATFLIT_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXDATFLIT),
    .TXDATLCRDV_RN1 (fc_hvl_top.chi_vif.rn_if[0].TXDATLCRDV),

    /**
     * CHI SN1 side Interface
     */  
    //-----------------------------------------------------------------------
    // Link Activation Status Signals
    //-----------------------------------------------------------------------
    .TXSACTIVE_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXSACTIVE),
    .RXSACTIVE_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXSACTIVE),

    //-----------------------------------------------------------------------
    // Link Activation Status Signals
    //-----------------------------------------------------------------------
    .TXLINKACTIVEREQ_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXLINKACTIVEREQ),
    .TXLINKACTIVEACK_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXLINKACTIVEACK),
    .RXLINKACTIVEREQ_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXLINKACTIVEREQ),
    .RXLINKACTIVEACK_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXLINKACTIVEACK),

    //-----------------------------------------------------------------------
    // RX Request Virtual Channel
    //-----------------------------------------------------------------------
    .RXREQFLITPEND_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXREQFLITPEND),
    .RXREQFLITV_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXREQFLITV),
    .RXREQFLIT_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXREQFLIT),
    .RXREQLCRDV_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXREQLCRDV),

    //-----------------------------------------------------------------------
    // TX Response Virtual Channel
    //-----------------------------------------------------------------------
    .TXRSPFLITPEND_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXRSPFLITPEND),
    .TXRSPFLITV_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXRSPFLITV),
    .TXRSPFLIT_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXRSPFLIT),
    .TXRSPLCRDV_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXRSPLCRDV),

    //-----------------------------------------------------------------------
    // TX Dat Virtual Channel
    //-----------------------------------------------------------------------
    .TXDATFLITPEND_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXDATFLITPEND),
    .TXDATFLITV_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXDATFLITV),
    .TXDATFLIT_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXDATFLIT),
    .TXDATLCRDV_SN1 (fc_hvl_top.chi_vif.sn_if[0].TXDATLCRDV),

    //-----------------------------------------------------------------------
    // RX Dat Virtual Channel
    //-----------------------------------------------------------------------
    .RXDATFLITPEND_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXDATFLITPEND),
    .RXDATFLITV_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXDATFLITV),
    .RXDATFLIT_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXDATFLIT),
    .RXDATLCRDV_SN1 (fc_hvl_top.chi_vif.sn_if[0].RXDATLCRDV)   

    );
   
//endmodule : basic_interconnect_sv_wrapper

