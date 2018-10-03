
/** 
 * Abstract: A HDL Interconnect wrapper that connects the Verilog HDL
 * Interconnect to the SystemVerilog interface.
 */

 //`include "axi_svt_dut.sv"
`include "svt_axi_common_defines.svi"
`include "svt_axi_port_defines.svi"

//module axi_svt_dut_sv_wrapper (svt_axi_if axi_if);

  // Added local AXI signals as wire for resolve W,CUVIHR warning in IUS.
  // This updates is applicable to other simulator also because these local signals are  
  // instanstiated inside the module.
  wire local_awvalid;
  wire [`SVT_AXI_MAX_ADDR_WIDTH-1:0]           local_awaddr;
  wire [`SVT_AXI_MAX_BURST_LENGTH_WIDTH-1:0]   local_awlen;
  wire [`SVT_AXI_SIZE_WIDTH-1:0]               local_awsize;
  wire [`SVT_AXI_BURST_WIDTH-1:0]              local_awburst;
  wire [`SVT_AXI_LOCK_WIDTH-1:0]               local_awlock;
  wire [`SVT_AXI_CACHE_WIDTH-1:0]              local_awcache;
  wire [`SVT_AXI_PROT_WIDTH-1:0]               local_awprot;
  wire [`SVT_AXI_MAX_ID_WIDTH-1:0]             local_awid;

  wire                                         local_arvalid;
  wire [`SVT_AXI_MAX_ADDR_WIDTH-1:0]           local_araddr;
  wire [`SVT_AXI_MAX_BURST_LENGTH_WIDTH-1:0]   local_arlen;
  wire [`SVT_AXI_SIZE_WIDTH-1:0]               local_arsize;
  wire [`SVT_AXI_BURST_WIDTH-1:0]              local_arburst;
  wire [`SVT_AXI_LOCK_WIDTH-1:0]               local_arlock;
  wire [`SVT_AXI_CACHE_WIDTH-1:0]              local_arcache;
  wire [`SVT_AXI_PROT_WIDTH-1:0]               local_arprot;
  wire [`SVT_AXI_MAX_ID_WIDTH-1:0]             local_arid;

  wire                                         local_wvalid;
  wire                                         local_wlast;
  wire [`SVT_AXI_MAX_DATA_WIDTH-1:0]           local_wdata;
  wire [`SVT_AXI_MAX_DATA_WIDTH/8-1:0]         local_wstrb;
  wire [`SVT_AXI_MAX_ID_WIDTH-1:0]             local_wid;

  wire local_rready;
  wire local_bready;
  wire local_awready;
  wire local_arready;
  wire local_rvalid;
  wire local_rlast;
  wire [`SVT_AXI_MAX_DATA_WIDTH-1:0] local_rdata;
  wire [`SVT_AXI_RESP_WIDTH-1:0] local_rresp;
  wire [`SVT_AXI_MAX_ID_WIDTH-1:0] local_rid;
  wire local_wready;
  wire local_bvalid;
  wire [`SVT_AXI_RESP_WIDTH-1:0] local_bresp;
  wire [`SVT_AXI_MAX_ID_WIDTH-1:0] local_bid;
  


  /** 
   * HDL Interconnect Instantiation: Example HDL Interconnect is just
   * pass-through connection. 
   */
  axi_svt_dut axi_svt_dut (
    .aclk (`HVL_TOP.axi_vif.master_if[0].internal_aclk) ,
    .aresetn (`HVL_TOP.axi_vif.master_if[0].aresetn) ,

    /**
     * write address channel 
     */
    .awvalid_m1 (`HVL_TOP.axi_vif.master_if[0].awvalid) ,
    .awaddr_m1 (`HVL_TOP.axi_vif.master_if[0].awaddr) ,
    .awlen_m1 (`HVL_TOP.axi_vif.master_if[0].awlen) ,
    .awsize_m1 (`HVL_TOP.axi_vif.master_if[0].awsize) ,
    .awburst_m1 (`HVL_TOP.axi_vif.master_if[0].awburst) ,
    .awlock_m1 (`HVL_TOP.axi_vif.master_if[0].awlock) ,
    .awcache_m1 (`HVL_TOP.axi_vif.master_if[0].awcache) ,
    .awprot_m1 (`HVL_TOP.axi_vif.master_if[0].awprot) ,
    .awid_m1 (`HVL_TOP.axi_vif.master_if[0].awid) ,
    .awready_m1 (local_awready) ,

    /**
     * read address channel 
     */
    .arvalid_m1 (`HVL_TOP.axi_vif.master_if[0].arvalid) ,
    .araddr_m1 (`HVL_TOP.axi_vif.master_if[0].araddr) ,
    .arlen_m1 (`HVL_TOP.axi_vif.master_if[0].arlen) ,
    .arsize_m1 (`HVL_TOP.axi_vif.master_if[0].arsize) ,
    .arburst_m1 (`HVL_TOP.axi_vif.master_if[0].arburst) ,
    .arlock_m1 (`HVL_TOP.axi_vif.master_if[0].arlock) ,
    .arcache_m1 (`HVL_TOP.axi_vif.master_if[0].arcache) ,
    .arprot_m1 (`HVL_TOP.axi_vif.master_if[0].arprot) ,
    .arid_m1 (`HVL_TOP.axi_vif.master_if[0].arid) ,
    .arready_m1 (local_arready) ,

    /**
     * read channel 
     */
    .rvalid_m1 (local_rvalid) ,
    .rlast_m1 (local_rlast) ,
    .rdata_m1 (local_rdata) ,
    .rresp_m1 (local_rresp) ,
    .rid_m1 (local_rid) ,
    .rready_m1 (`HVL_TOP.axi_vif.master_if[0].rready) ,

    /**
     * write channel 
     */
    .wvalid_m1 (`HVL_TOP.axi_vif.master_if[0].wvalid) ,
    .wlast_m1 (`HVL_TOP.axi_vif.master_if[0].wlast) ,
    .wdata_m1 (`HVL_TOP.axi_vif.master_if[0].wdata) ,
    .wstrb_m1 (`HVL_TOP.axi_vif.master_if[0].wstrb) ,
    .wid_m1 (`HVL_TOP.axi_vif.master_if[0].wid) ,
    .wready_m1 (local_wready) ,

    /**
     * write response channel 
     */
    .bvalid_m1 (local_bvalid) ,
    .bresp_m1 (local_bresp) ,
    .bid_m1 (local_bid) ,
    .bready_m1 (`HVL_TOP.axi_vif.master_if[0].bready),


    /**
     * write address channel 
     */
    .awvalid_s1 (local_awvalid) ,
    .awaddr_s1 (local_awaddr) ,
    .awlen_s1 (local_awlen) ,
    .awsize_s1 (local_awsize) ,
    .awburst_s1 (local_awburst) ,
    .awlock_s1 (local_awlock) ,
    .awcache_s1 (local_awcache) ,
    .awprot_s1 (local_awprot) ,
    .awid_s1 (local_awid) ,
    .awready_s1 (`HVL_TOP.axi_vif.slave_if[0].awready) ,

    /**
     * read address channel 
     */
    .arvalid_s1 (local_arvalid) ,
    .araddr_s1 (local_araddr) ,
    .arlen_s1 (local_arlen) ,
    .arsize_s1 (local_arsize) ,
    .arburst_s1 (local_arburst) ,
    .arlock_s1 (local_arlock) ,
    .arcache_s1 (local_arcache) ,
    .arprot_s1 (local_arprot) ,
    .arid_s1 (local_arid) ,
    .arready_s1 (`HVL_TOP.axi_vif.slave_if[0].arready) ,

    /**
     * read channel 
     */
    .rvalid_s1 (`HVL_TOP.axi_vif.slave_if[0].rvalid) ,
    .rlast_s1 (`HVL_TOP.axi_vif.slave_if[0].rlast) ,
    .rdata_s1 (`HVL_TOP.axi_vif.slave_if[0].rdata) ,
    .rresp_s1 (`HVL_TOP.axi_vif.slave_if[0].rresp) ,
    .rid_s1 (`HVL_TOP.axi_vif.slave_if[0].rid) ,
    .rready_s1 (local_rready) ,
                       
    /**                
     * write channel   
     */                
    .wvalid_s1 (local_wvalid) ,
    .wlast_s1 (local_wlast) ,
    .wdata_s1 (local_wdata) ,
    .wstrb_s1 (local_wstrb) ,
    .wid_s1 (local_wid) ,
    .wready_s1 (`HVL_TOP.axi_vif.slave_if[0].wready) ,

    /**
     * write response channel 
     */
    .bvalid_s1 (`HVL_TOP.axi_vif.slave_if[0].bvalid) ,
    .bresp_s1 (`HVL_TOP.axi_vif.slave_if[0].bresp) ,
    .bid_s1 (`HVL_TOP.axi_vif.slave_if[0].bid) ,
    .bready_s1 (local_bready)
    
  );

  // Assign local AXI signals to slave and master interface for resolve W,CUVIHR warning in IUS.
  // This updates is applicable to other simulator also because these local signals are  
  // instanstiated inside the module.  
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].awready = local_awready;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].arready = local_arready;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].rvalid = local_rvalid;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].rlast = local_rlast;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].rdata = local_rdata;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].rresp = local_rresp;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].rid = local_rid;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].wready = local_wready;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].bvalid = local_bvalid;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].bresp = local_bresp;
  always @ (*) assign `HVL_TOP.axi_vif.master_if[0].bid = local_bid;

  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awvalid = local_awvalid;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awaddr = local_awaddr;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awlen = local_awlen;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awsize = local_awsize;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awburst = local_awburst;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awlock = local_awlock;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awcache = local_awcache;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awprot = local_awprot;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].awid = local_awid;

  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arvalid = local_arvalid;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].araddr = local_araddr;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arlen = local_arlen;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arsize = local_arsize;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arburst = local_arburst;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arlock = local_arlock;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arcache = local_arcache;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arprot = local_arprot;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].arid = local_arid;

  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].wvalid = local_wvalid;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].wlast = local_wlast;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].wdata = local_wdata;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].wstrb = local_wstrb;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].wid = local_wid;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].rready = local_rready;
  always @ (*) assign `HVL_TOP.axi_vif.slave_if[0].bready = local_bready;



//endmodule
