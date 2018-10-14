
/** 
 * Abstract: A HDL Interconnect wrapper that connects the Verilog HDL
 * Interconnect to the SystemVerilog interface.
 */


//`include "apb_svt_dut.sv"
//`include "svt_apb_if.svi"
`include "svt_apb_common_defines.svi"
`include "svt_apb_port_defines.svi"

//module apb_svt_dut_sv_wrapper(svt_apb_if `HVL_TOP.apb_master_vif, svt_apb_if `HVL_TOP.apb_slave_vif);
  wire [(`SVT_APB_MAX_NUM_SLAVES-1):0] local_psel;
  wire local_penable;
  wire local_pwrite;
  wire [(`SVT_APB_MAX_ADDR_WIDTH - 1):0] local_paddr;
  wire [(`SVT_APB_PWDATA_WIDTH-1):0]     local_pwdata;
  wire [((`SVT_APB_PWDATA_WIDTH/8)-1):0] local_pstrb;
  wire [2:0]                             local_pprot;
  wire [(`SVT_APB_PWDATA_WIDTH-1):0]     local_prdata;
  wire                                   local_pready;
  wire                                   local_pslverr;

  apb_svt_dut apb_svt_dut (
    // Master side master signals
    .psel_m    (`HVL_TOP.apb_master_vif.psel),
    .penable_m (`HVL_TOP.apb_master_vif.penable),
    .pwrite_m  (`HVL_TOP.apb_master_vif.pwrite),
    .paddr_m   (`HVL_TOP.apb_master_vif.paddr),
    .pwdata_m  (`HVL_TOP.apb_master_vif.pwdata),
    .pstrb_m   (`HVL_TOP.apb_master_vif.pstrb),
    .pprot_m   (`HVL_TOP.apb_master_vif.pprot),

    // Master side slave signals
    .prdata_m0  (local_prdata),
    .pready_m0  (local_pready),
    .pslverr_m0 (local_pslverr),

    // Slave side master signals
    .psel_s    (local_psel),
    .penable_s (local_penable),
    .pwrite_s  (local_pwrite),
    .paddr_s   (local_paddr),
    .pwdata_s  (local_pwdata),
    .pstrb_s   (local_pstrb),
    .pprot_s   (local_pprot),

    // Slave side slave signals
    .prdata_s0  (`HVL_TOP.apb_slave_vif.slave_if[0].prdata),
    .pready_s0  (`HVL_TOP.apb_slave_vif.slave_if[0].pready),
    .pslverr_s0 (`HVL_TOP.apb_slave_vif.slave_if[0].pslverr)
  );

  always @ (*) assign `HVL_TOP.apb_slave_vif.psel = local_psel;
  always @ (*) assign `HVL_TOP.apb_slave_vif.penable = local_penable;
  always @ (*) assign `HVL_TOP.apb_slave_vif.pwrite = local_pwrite;
  always @ (*) assign `HVL_TOP.apb_slave_vif.paddr = local_paddr;
  always @ (*) assign `HVL_TOP.apb_slave_vif.pwdata = local_pwdata;
  always @ (*) assign `HVL_TOP.apb_slave_vif.pstrb = local_pstrb;
  always @ (*) assign `HVL_TOP.apb_slave_vif.pprot = local_pprot;
  always @ (*) assign `HVL_TOP.apb_master_vif.slave_if[0].prdata = local_prdata;
  always @ (*) assign `HVL_TOP.apb_master_vif.slave_if[0].pready = local_pready;
  always @ (*) assign `HVL_TOP.apb_master_vif.slave_if[0].pslverr = local_pslverr;

//endmodule
