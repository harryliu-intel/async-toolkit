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

//include : mby_mc_np_conn
//
// AXI connections to Cport processor in Mplex no proc TB when the Cport
// processor is black-boxed. 

// Using the STUB Processor so connect the AXI agent to the CPort signals

// axi write address channel:
assign mplex_top.cup_proc.AWADDR    =  axi_if.master_if[0].awaddr;
assign mplex_top.cup_proc.AWVALID   =  axi_if.master_if[0].awvalid;
assign mplex_top.cup_proc.AWBURST   =  axi_if.master_if[0].awburst;
assign mplex_top.cup_proc.AWLOCK    =  axi_if.master_if[0].awlock;
assign mplex_top.cup_proc.AWSIZE    =  axi_if.master_if[0].awsize;
assign mplex_top.cup_proc.AWPROT    =  axi_if.master_if[0].awprot;
assign mplex_top.cup_proc.AWID	    =  axi_if.master_if[0].awid;
assign mplex_top.cup_proc.AWLEN	    =  axi_if.master_if[0].awlen;
assign mplex_top.cup_proc.AWCACHE   =  axi_if.master_if[0].awcache;
assign mplex_top.cup_proc.AWQOS	    =  axi_if.master_if[0].awqos;

// axi write data channel:  
assign mplex_top.cup_proc.WDATA	    =  axi_if.master_if[0].wdata; 
assign mplex_top.cup_proc.WVALID    =  axi_if.master_if[0].wvalid;
assign mplex_top.cup_proc.WLAST	    =  axi_if.master_if[0].wlast;
assign mplex_top.cup_proc.WSTRB	    =  axi_if.master_if[0].wstrb; 
  
// axi write response channel
assign mplex_top.cup_proc.BREADY    =  axi_if.master_if[0].bready;

// axi read address channel
assign mplex_top.cup_proc.ARADDR    =  axi_if.master_if[0].araddr;
assign mplex_top.cup_proc.ARBURST   =  axi_if.master_if[0].arburst;
assign mplex_top.cup_proc.ARCACHE   =  axi_if.master_if[0].arcache;
assign mplex_top.cup_proc.ARID	    =  axi_if.master_if[0].arid;
assign mplex_top.cup_proc.ARLEN	    =  axi_if.master_if[0].arlen;	 
assign mplex_top.cup_proc.ARLOCK    =  axi_if.master_if[0].arlock;
assign mplex_top.cup_proc.ARPROT    =  axi_if.master_if[0].arprot;
assign mplex_top.cup_proc.ARQOS	    =  axi_if.master_if[0].arqos;	 
assign mplex_top.cup_proc.ARSIZE    =  axi_if.master_if[0].arsize;
assign mplex_top.cup_proc.ARVALID   =  axi_if.master_if[0].arvalid;

// axi read data channel
assign mplex_top.cup_proc.RREADY    =  axi_if.master_if[0].rready;
