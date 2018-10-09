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
 * File Name   : ti_macro_defines.svh
 *
 * Author      : Mat.Khir Md Haniffa
 *
 * Last Update : 2016.w34.1
 *
 * Description : List the macros used for test island connectivity.
 *               RTL hierarchies must not be hardcoded in this file, instead
 *               should reference fc_hier_defines_generated.sv, which is a
 *               collage generated file
 *
 ******************************************************************************
*/

`define HVL_TOP              fc_hvl_top
`define HDL_TOP              fc_hdl_top
`define DUT_IF               `HDL_TOP.fctop_dut_if
`define soc                  `HDL_TOP.dut
`define FCSIG_IF             `HVL_TOP.fc_sig_if
`define HDL_TOP_LIB          `HDL_TOP``_lib
`define HDL_TOP_CFG          `HDL_TOP``_cfg
`define HVL_TOP_LIB          `HVL_TOP``_lib
`define HVL_TOP_CFG          `HVL_TOP``_cfg
`define HDL_TOP_REALMIA_CFG  `HDL_TOP``_realmia_cfg
`define RTL_STUB_LIB         soc_ip_stub_lib
`define FCGPIO_IF            `HVL_TOP.fc_gpio_if1
`define FC_ENV               uvm_test_top.tb_env

`ifdef FC_64
     `define NUM_IGR 16
`else
     `define NUM_IGR 4
`endif

/*
 *  ----------------
 *    End of file
 *  ----------------
*/
// <<< VIM SETTINGS
// vim: ts=4 et
// >>>
