//-----------------------------------------------------------------------------
// Title         : Ingress definitions
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : ingress_defines.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
//  This file contain all the IP macros (`define)
//   each define must be protected with "ifndef" to be able to be overide 
//   from the command line
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

`ifndef MBY_IGR_TOP
 `define MBY_IGR_TOP igr_tb.igr_top
`endif
`ifndef MBY_IGR_TOP_PATH
 `define MBY_IGR_TOP_PATH igr_tb
`endif

