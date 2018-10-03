/*
 ******************************************************************************
 *
 *                     I N T E L   C O R P O R A T I O N
 *
 *                         Copyright(c) 2016
 *
 *                         All Rights Reserved
 *
 ******************************************************************************
 *
 * File Name   : iosfsb_conn_defines.svh
 *
 * Author      : Chandran, Ajay K
 *
 * Last Update : 2016.w45.3
 *
 * Description : Specify the IOSF-SB connection for sideband bridge
 *
 ******************************************************************************
*/
`define IOSFSB_CONNECT_MST(IF_INST, DUT_INST, DUT_IN_PREFIX,DUT_OUT_PREFIX) \
    // <<< Master interface of DUT to target interface of VIP \
    assign IF_INST.tpccup            = DUT_INST.DUT_IN_PREFIX``mpccup; \
    assign IF_INST.tnpcup            = DUT_INST.DUT_IN_PREFIX``mnpcup; \
    assign IF_INST.tpcput            = DUT_INST.DUT_OUT_PREFIX``mpcput; \
    assign IF_INST.tnpput            = DUT_INST.DUT_OUT_PREFIX``mnpput; \
    assign IF_INST.teom              = DUT_INST.DUT_OUT_PREFIX``meom; \
    assign IF_INST.tpayload          = DUT_INST.DUT_OUT_PREFIX``mpayload; \
    // >>>

`define IOSFSB_CONNECT_TGT(IF_INST, DUT_INST, DUT_IN_PREFIX,DUT_OUT_PREFIX) \
    // <<< Target interface of DUT to master interface of VIP \
    assign IF_INST.mpccup            = DUT_INST.DUT_OUT_PREFIX``tpccup; \
    assign IF_INST.mnpcup            = DUT_INST.DUT_OUT_PREFIX``tnpcup; \
    assign IF_INST.mpcput            = DUT_INST.DUT_IN_PREFIX``tpcput; \
    assign IF_INST.mnpput            = DUT_INST.DUT_IN_PREFIX``tnpput; \
    assign IF_INST.meom              = DUT_INST.DUT_IN_PREFIX``teom; \
    assign IF_INST.mpayload          = DUT_INST.DUT_IN_PREFIX``tpayload;
    // >>>

`define IOSFSB_CONNECT_CLK(IF_INST,DUT_INST,POK,DUT_IN_PREFIX,DUT_OUT_PREFIX) \
    // <<< Clock, reset and power management \
    assign IF_INST.side_clkreq       = DUT_INST.DUT_OUT_PREFIX``clkreq; \
    assign IF_INST.side_clkack       = DUT_INST.DUT_IN_PREFIX``clkack; \
    assign IF_INST.side_pok          = DUT_INST.POK; \
    assign IF_INST.side_ism_fabric   = DUT_INST.DUT_IN_PREFIX``ism_fabric; \
    assign IF_INST.side_ism_agent    = DUT_INST.DUT_OUT_PREFIX``ism_agent; \
    // >>> 

`define SBB_IOSFSB_CONNECT(IDX, IF_PREFIX,DUT_INST,POK,DUT_IN_PREFIX,DUT_OUT_PREFIX) \
    iosf_sbc_intf #(`DEF_IOSF_SB_IF_PARAMS) IF_PREFIX``_``IDX \
        (.side_clk(`SBL``IDX``_SB_CLK), \
         .side_rst_b(`SBL``IDX``_SB_RST_B)); \
    `IOSFSB_CONNECT_CLK(IF_PREFIX``_``IDX,DUT_INST,POK,DUT_IN_PREFIX``side_,DUT_OUT_PREFIX``side_) \
    `IOSFSB_CONNECT_MST(IF_PREFIX``_``IDX, DUT_INST, DUT_IN_PREFIX,DUT_OUT_PREFIX) \
    `IOSFSB_CONNECT_TGT(IF_PREFIX``_``IDX, DUT_INST, DUT_IN_PREFIX,DUT_OUT_PREFIX) \

`define IOSFSB_STUB_OUT(IF_INST) \
    assign IF_INST.meom = '0; \
    assign IF_INST.mpccup = '0; \
    assign IF_INST.tpcput = '0; \
    assign IF_INST.mnpcup = '0; \
    assign IF_INST.tnpput = '0; \
    assign IF_INST.teom = '0; \
    assign IF_INST.tpayload ='0;  \
    assign IF_INST.mpayload = '0; \
    assign IF_INST.tpccup = '0; \
    assign IF_INST.mpcput = '0; \
    assign IF_INST.tnpcup = '0; \
    assign IF_INST.mnpput = '0; \
    assign IF_INST.side_ism_agent  = '0; \
    assign IF_INST.side_ism_fabric  = '0; \
    assign IF_INST.side_clkreq = '1; \
    assign IF_INST.side_clkack = '1;

/*
 *  ----------------
 *    End of file
 *  ----------------
*/
// <<< VIM SETTINGS
// vim: ts=4 et 
// >>>
