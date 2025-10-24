// ---------------------------------------------------------------------------------------------------
// Copyright(C) 2013 Intel Corporation, Confidential Information
// ---------------------------------------------------------------------------------------------------
//
// Created By:  Raghu Prasad Gudla
// Created On:  8/28/2018
// Description: Top level module for AXI TB environment
// ---------------------------------------------------------------------------------------------------


/*
 * Abstract:
 * This module represents a HDL interconnect DUT that has 2 AXI Protocol Interfaces.
 * Protocol interface 1 connects to an AXI Master Agent, and all pins have the suffix _m1.
 * Protocol interface 2 connects to an AXI Slave Agent, and all pins have the suffix _s1.. 
 * The behavior of this module is to simply connect the two protocol interfaces 
 * by assigning the inputs on the Slave side to the outputs on the Master side,and vice versa.
 */


module axi_svt_dut (
  /**
   * Clock and reset
   */
  aclk,
  aresetn,

  /**
   * AXI Master side Interface
   */

  /**
   * Write Address Channel
   */
  awaddr_m1,
  awvalid_m1,
  awlen_m1,
  awburst_m1,
  awsize_m1,
  awlock_m1,
  awprot_m1,
  awcache_m1,
  awid_m1,
  awready_m1,

  /**
   * Read Address Channel
   */
  araddr_m1,
  arvalid_m1,
  arlen_m1,
  arburst_m1,
  arsize_m1,
  arlock_m1,
  arprot_m1,
  arcache_m1,
  arid_m1,
  arready_m1,

  /**
   * Write Data Channel
   */
  wdata_m1,
  wvalid_m1,
  wstrb_m1,
  wlast_m1,
  wid_m1,
  wready_m1,

  /**
   * Read Data Channel
   */
  rdata_m1,
  rvalid_m1,
  rlast_m1,
  rresp_m1,
  rid_m1,
  rready_m1,

  /**
   * Write Response Channel
   */
  bresp_m1,
  bvalid_m1,
  bid_m1,
  bready_m1,

  /**
   * AXI Slave side Interface
   */

  /**
   * Write Address Channel
   */
  awaddr_s1,
  awvalid_s1,
  awlen_s1,
  awburst_s1,
  awsize_s1,
  awlock_s1,
  awprot_s1,
  awcache_s1,
  awid_s1,
  awready_s1,

  /**
   * Read Address Channel
   */
  araddr_s1,
  arvalid_s1,
  arlen_s1,
  arburst_s1,
  arsize_s1,
  arlock_s1,
  arprot_s1,
  arcache_s1,
  arid_s1,
  arready_s1,

  /**
   * Write Data Channel
   */
  wdata_s1,
  wvalid_s1,
  wstrb_s1,
  wlast_s1,
  wid_s1,
  wready_s1,

  /**
   * Read Data Channel
   */
  rdata_s1,
  rvalid_s1,
  rlast_s1,
  rresp_s1,
  rid_s1,
  rready_s1,

  /**
   * Write Response Channel
   */
  bresp_s1,
  bvalid_s1,
  bid_s1,
  bready_s1

  );
 

  /**
   * Clock and Reset signals
   */
  input  aclk;
  input  aresetn;

  /**
   * AXI Protocol Interface M1 - Dataflow signals
   */
  input  [`SVT_AXI_MAX_ADDR_WIDTH-1:0]          awaddr_m1;
  input                                         awvalid_m1;
  input  [`SVT_AXI_MAX_BURST_LENGTH_WIDTH-1:0]  awlen_m1;
  input  [`SVT_AXI_BURST_WIDTH-1:0]             awburst_m1;
  input  [`SVT_AXI_SIZE_WIDTH-1:0]              awsize_m1;
  input  [`SVT_AXI_LOCK_WIDTH-1:0]              awlock_m1;
  input  [`SVT_AXI_PROT_WIDTH-1:0]              awprot_m1;
  input  [`SVT_AXI_CACHE_WIDTH-1:0]             awcache_m1;
  input  [`SVT_AXI_MAX_ID_WIDTH-1:0]            awid_m1;
  output                                        awready_m1;

  input  [`SVT_AXI_MAX_ADDR_WIDTH-1:0]          araddr_m1;
  input                                         arvalid_m1;
  input  [`SVT_AXI_MAX_BURST_LENGTH_WIDTH-1:0]  arlen_m1;
  input  [`SVT_AXI_BURST_WIDTH-1:0]             arburst_m1;
  input  [`SVT_AXI_SIZE_WIDTH-1:0]              arsize_m1;
  input  [`SVT_AXI_LOCK_WIDTH-1:0]              arlock_m1;
  input  [`SVT_AXI_PROT_WIDTH-1:0]              arprot_m1;
  input  [`SVT_AXI_CACHE_WIDTH-1:0]             arcache_m1;
  input  [`SVT_AXI_MAX_ID_WIDTH-1:0]            arid_m1;
  output                                        arready_m1;

  input  [`SVT_AXI_MAX_DATA_WIDTH-1:0]          wdata_m1;
  input                                         wvalid_m1;
  input  [`SVT_AXI_MAX_DATA_WIDTH/8-1:0]        wstrb_m1;
  input                                         wlast_m1;
  input  [`SVT_AXI_MAX_ID_WIDTH-1:0]            wid_m1;
  output                                        wready_m1;

  output [`SVT_AXI_MAX_DATA_WIDTH-1:0]          rdata_m1;
  output                                        rvalid_m1;
  output                                        rlast_m1;
  output [`SVT_AXI_RESP_WIDTH-1:0]              rresp_m1;
  output [`SVT_AXI_MAX_ID_WIDTH-1:0]            rid_m1;
  input                                         rready_m1;

  output [`SVT_AXI_RESP_WIDTH-1:0]              bresp_m1;
  output                                        bvalid_m1;
  output [`SVT_AXI_MAX_ID_WIDTH-1:0]            bid_m1;
  input                                         bready_m1;

  /**
   * AXI Protocol Interface S1 - Dataflow signals
   */
  output [`SVT_AXI_MAX_ADDR_WIDTH-1:0]          awaddr_s1;
  output                                        awvalid_s1;
  output [`SVT_AXI_MAX_BURST_LENGTH_WIDTH-1:0]  awlen_s1;
  output [`SVT_AXI_BURST_WIDTH-1:0]             awburst_s1;
  output [`SVT_AXI_SIZE_WIDTH-1:0]              awsize_s1;
  output [`SVT_AXI_LOCK_WIDTH-1:0]              awlock_s1;
  output [`SVT_AXI_PROT_WIDTH-1:0]              awprot_s1;
  output [`SVT_AXI_CACHE_WIDTH-1:0]             awcache_s1;
  output [`SVT_AXI_MAX_ID_WIDTH-1:0]            awid_s1;
  input                                         awready_s1;

  output [`SVT_AXI_MAX_ADDR_WIDTH-1:0]          araddr_s1;
  output                                        arvalid_s1;
  output [`SVT_AXI_MAX_BURST_LENGTH_WIDTH-1:0]  arlen_s1;
  output [`SVT_AXI_BURST_WIDTH-1:0]             arburst_s1;
  output [`SVT_AXI_SIZE_WIDTH-1:0]              arsize_s1;
  output [`SVT_AXI_LOCK_WIDTH-1:0]              arlock_s1;
  output [`SVT_AXI_PROT_WIDTH-1:0]              arprot_s1;
  output [`SVT_AXI_CACHE_WIDTH-1:0]             arcache_s1;
  output [`SVT_AXI_MAX_ID_WIDTH-1:0]            arid_s1;
  input                                         arready_s1;

  output [`SVT_AXI_MAX_DATA_WIDTH-1:0]          wdata_s1;
  output                                        wvalid_s1;
  output [`SVT_AXI_MAX_DATA_WIDTH/8-1:0]        wstrb_s1;
  output                                        wlast_s1;
  output [`SVT_AXI_MAX_ID_WIDTH-1:0]            wid_s1;
  input                                         wready_s1;

  input  [`SVT_AXI_MAX_DATA_WIDTH-1:0]          rdata_s1;
  input                                         rvalid_s1;
  input                                         rlast_s1;
  input  [`SVT_AXI_RESP_WIDTH-1:0]              rresp_s1;
  input  [`SVT_AXI_MAX_ID_WIDTH-1:0]            rid_s1;
  output                                        rready_s1;

  input  [`SVT_AXI_RESP_WIDTH-1:0]              bresp_s1;
  input                                         bvalid_s1;
  input  [`SVT_AXI_MAX_ID_WIDTH-1:0]            bid_s1;
  output                                        bready_s1;

  /**
   * Pass-Through Assignments: Inputs from AXI Interface m1 are copied
   * to Outputs on AXI Interface s1, and vice versa
   */

  assign awvalid_s1 = awvalid_m1;
  assign awaddr_s1  = awaddr_m1;
  assign awlen_s1   = awlen_m1;
  assign awsize_s1  = awsize_m1;
  assign awburst_s1 = awburst_m1;
  assign awlock_s1  = awlock_m1;
  assign awcache_s1 = awcache_m1;
  assign awprot_s1  = awprot_m1;
  assign awid_s1    = awid_m1;
  assign awready_m1 = awready_s1;

  assign arvalid_s1 = arvalid_m1;
  assign araddr_s1  = araddr_m1;
  assign arlen_s1   = arlen_m1;
  assign arsize_s1  = arsize_m1;
  assign arburst_s1 = arburst_m1;
  assign arlock_s1  = arlock_m1;
  assign arcache_s1 = arcache_m1;
  assign arprot_s1  = arprot_m1;
  assign arid_s1    = arid_m1;
  assign arready_m1 = arready_s1;
  
  assign rvalid_m1  = rvalid_s1;
  assign rlast_m1   = rlast_s1;
  assign rdata_m1   = rdata_s1;
  assign rresp_m1   = rresp_s1;
  assign rid_m1     = rid_s1;
  assign rready_s1  = rready_m1;
  
  assign wvalid_s1  = wvalid_m1;
  assign wlast_s1   = wlast_m1;
  assign wdata_s1   = wdata_m1;
  assign wstrb_s1   = wstrb_m1;
  assign wid_s1     = wid_m1;
  assign wready_m1  = wready_s1;
  
  assign bvalid_m1  = bvalid_s1;
  assign bresp_m1   = bresp_s1;
  assign bid_m1     = bid_s1;
  assign bready_s1  = bready_m1;
  
endmodule

