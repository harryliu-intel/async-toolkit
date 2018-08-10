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
//------------------------------------------------------------------------------

//   Defines:    svt_axi_bfm_defines
//
//   This file contain any SVT_BFM level Defines, Typedefs, or PARAMETERS.


`ifndef __SVT_BFM_DEFINES_GUARD
`define __SVT_BFM_DEFINES_GUARD

`ifndef __INSIDE_SVT_AXI_BFM_PKG__
`error "Attempt to include file outside of svt_ethernet_bfm_pkg."
`endif


   `define SVT_AXI_USER_DEFINES_SVI

   `define SVT_AXI_MAX_ID_WIDTH 12


`endif // __SVT_BFM_DEFINES_GUARD
