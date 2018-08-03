

// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//   Description   : MBY Mplex TB Configuration (No Xtensa Processor, No PCIe PHY)
//------------------------------------------------------------------------------


config mby_mc_cfg_np_nphy;

design mby_mc_tb_top;
//default liblist mby_mplex_rtl_lib;
//
//instance tb_top.mplex.mc_proc_top.processor liblist apr_mplex_no_proc_rtl_lib;
//instance tb_top.mplex.mc_pcie_top.pcic_mac liblist apr_mplex_no_phy_rtl_lib;
//instance tb_top.mplex.mc_pcie_top.apr_phy_wrapper liblist apr_mplex_no_phy_rtl_lib;


endconfig