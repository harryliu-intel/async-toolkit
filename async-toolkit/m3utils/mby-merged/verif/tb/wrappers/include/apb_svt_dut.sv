
/*
 * Abstract:
 * This module represents a HDL interconnect DUT that has 2 APB Protocol Interfaces.
 * Protocol interface 1 connects to an APB Master, and all pins have the suffix _m1.
 * Protocol interface 2 connects to an APB Slave, and all pins have the suffix _s1.
 * The behavior of this module is to simply connect the two protocol interfaces 
 * by assigning the inputs on the Slave side to the outputs on the Master side, 
 * and vice versa.
 */


module apb_svt_dut (
  // Master side master signals
  psel_m,
  penable_m,
  pwrite_m,
  paddr_m,
  pwdata_m,
  pstrb_m,
  pprot_m,

  // Master side slave signals
  prdata_m0,
  pready_m0,
  pslverr_m0,

  // Slave side master signals
  psel_s,
  penable_s,
  pwrite_s,
  paddr_s,
  pwdata_s,
  pstrb_s,
  pprot_s,

  // Slave side slave signals
  prdata_s0,
  pready_s0,
  pslverr_s0
);

  // Master Signals
  input [(`SVT_APB_MAX_NUM_SLAVES-1):0]   psel_m;
  input                                   penable_m;
  input                                   pwrite_m;
  input [(`SVT_APB_PADDR_WIDTH-1):0]      paddr_m;
  input [(`SVT_APB_PWDATA_WIDTH-1):0]     pwdata_m;
  input [((`SVT_APB_PWDATA_WIDTH/8)-1):0] pstrb_m;
  input [2:0]                             pprot_m;

  // Slave Signals
  output [(`SVT_APB_PWDATA_WIDTH-1):0]      prdata_m0;
  output                                    pready_m0;
  output                                    pslverr_m0;

  // Master Signals
  output [(`SVT_APB_MAX_NUM_SLAVES-1):0]    psel_s;
  output                                    penable_s;
  output                                    pwrite_s;
  output [(`SVT_APB_PADDR_WIDTH-1):0]       paddr_s;
  output [(`SVT_APB_PWDATA_WIDTH-1):0]      pwdata_s;
  output [((`SVT_APB_PWDATA_WIDTH/8)-1):0]  pstrb_s;
  output [2:0]                              pprot_s;

  // Slave Signals
  input [(`SVT_APB_PWDATA_WIDTH-1):0]   prdata_s0;
  input                                 pready_s0;
  input                                 pslverr_s0;

  /**
   * Connect the master output signals from the active master VIP to the passive
   * master VIP signals.
   */
  assign psel_s = psel_m;
  assign penable_s = penable_m;
  assign pwrite_s = pwrite_m;
  assign paddr_s = paddr_m;
  assign pwdata_s = pwdata_m;
  assign pstrb_s = pstrb_m;
  assign pprot_s = pprot_m;

  /**
   * Connect the slave output signals from the active slave VIP to the passive
   * slave VIP signals.
   */
  assign prdata_m0 = prdata_s0;
  assign pready_m0 = pready_s0;
  assign pslverr_m0 = pslverr_s0;

endmodule

