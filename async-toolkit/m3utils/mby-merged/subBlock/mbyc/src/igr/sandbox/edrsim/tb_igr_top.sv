//`timescale 1ns/100ps
//  `include "mby_rp_inq_pkg.sv"

module TB_IGR_TOP
  import mby_igr_pkg::*, shared_pkg::*;
(

);

  // Clock.
  logic                               cclk;
  // Unsynchronized warm reset.
  logic                               rst_n= '1;
  
  logic [7:0]                       grp_A_rx_ecc = '0;
  logic [7:0]                       grp_B_rx_ecc = '0;
  logic [7:0]                       grp_C_rx_ecc = '0;
  logic [7:0]                       grp_D_rx_ecc = '0;
  
  logic [1:0]                  grp_A_rx_port_num = '0;
  logic [1:0]                  grp_B_rx_port_num = '0;
  logic [1:0]                  grp_C_rx_port_num = '0;
  logic [1:0]                  grp_D_rx_port_num = '0;
  
  logic [7:0]                grp_A_rx_data_valid = '1;
  logic [7:0]                grp_B_rx_data_valid = '0;
  logic [7:0]                grp_C_rx_data_valid = '0;
  logic [7:0]                grp_D_rx_data_valid = '0;
  
  epl_md_t                     grp_A_rx_metadata = '0;
  epl_md_t                     grp_B_rx_metadata = '0;
  epl_md_t                     grp_C_rx_metadata = '0;
  epl_md_t                     grp_D_rx_metadata = '0;

  epl_ts_t                   grp_A_rx_time_stamp = '0;
  epl_ts_t                   grp_B_rx_time_stamp = '0;
  epl_ts_t                   grp_C_rx_time_stamp = '0;
  epl_ts_t                   grp_D_rx_time_stamp = '0;
   
  data64_w_ecc_t [0:7]       grp_A_rx_data_w_ecc = '0;
  data64_w_ecc_t [0:7]       grp_B_rx_data_w_ecc = '0;
  data64_w_ecc_t [0:7]       grp_C_rx_data_w_ecc = '0;
  data64_w_ecc_t [0:7]       grp_D_rx_data_w_ecc = '0;
 
  logic                        grp_A_rx_pfc_xoff = '0;
  logic                        grp_B_rx_pfc_xoff = '0;
  logic                        grp_C_rx_pfc_xoff = '0;
  logic                        grp_D_rx_pfc_xoff = '0;
  
  logic [2:0]           grp_A_rx_flow_control_tc = '0;
  logic [2:0]           grp_B_rx_flow_control_tc = '0;
  logic [2:0]           grp_C_rx_flow_control_tc = '0;
  logic [2:0]           grp_D_rx_flow_control_tc = '0;

  logic [7:0]                       vp_rx_ecc = '0;
  logic [1:0]                  vp_rx_port_num = '0;
  logic [7:0]                vp_rx_data_valid = '0;  
  epl_md_t                     vp_rx_metadata = '0;
  epl_ts_t                   vp_rx_time_stamp = '0;
  logic [19:0]             vp_cpp_rx_metadata = '0;   
  data64_w_ecc_t [0:7]     vp_rx_data_w_ecc = '0;
  logic [2:0]           vp_rx_flow_control_tc = '0;
  logic                        vp_tx_pfc_xoff;
  
  igr_rx_ppe_if     igr_rx_ppe();
  rx_ppe_igr_if     rx_ppe_igr();
  assign igr_rx_ppe.intf0_ack = '0;
  assign igr_rx_ppe.intf1_ack = '0;
  assign rx_ppe_igr.intf0 = '0;
  assign rx_ppe_igr.intf1 = '0;

initial
begin
cclk = 1'b0;
forever
 #1 cclk = ~cclk;
end


igr_top
  top(
  // Clock.
  .cclk(cclk),
  // Unsynchronized warm reset.
  .rst_n(rst_n),
      
// EPL I/O from MBY FS Dataplane Interface signals.
  //EPL0
  .grp_A_rx_ecc(grp_A_rx_ecc),
  .grp_B_rx_ecc(grp_B_rx_ecc),
  .grp_C_rx_ecc(grp_C_rx_ecc),
  .grp_D_rx_ecc(grp_D_rx_ecc),

  .grp_A_rx_port_num(grp_A_rx_port_num),
  .grp_B_rx_port_num(grp_B_rx_port_num),
  .grp_C_rx_port_num(grp_C_rx_port_num),
  .grp_D_rx_port_num(grp_D_rx_port_num),
  
  .grp_A_rx_data_valid(grp_A_rx_data_valid),
  .grp_B_rx_data_valid(grp_B_rx_data_valid),
  .grp_C_rx_data_valid(grp_C_rx_data_valid),
  .grp_D_rx_data_valid(grp_D_rx_data_valid),
  
  .grp_A_rx_metadata(grp_A_rx_metadata),
  .grp_B_rx_metadata(grp_B_rx_metadata),
  .grp_C_rx_metadata(grp_C_rx_metadata),
  .grp_D_rx_metadata(grp_D_rx_metadata),

  .grp_A_rx_time_stamp(grp_A_rx_time_stamp),
  .grp_B_rx_time_stamp(grp_B_rx_time_stamp),
  .grp_C_rx_time_stamp(grp_C_rx_time_stamp),
  .grp_D_rx_time_stamp(grp_D_rx_time_stamp),
  
  .grp_A_rx_data_w_ecc(grp_A_rx_data_w_ecc),
  .grp_B_rx_data_w_ecc(grp_B_rx_data_w_ecc),
  .grp_C_rx_data_w_ecc(grp_C_rx_data_w_ecc),
  .grp_D_rx_data_w_ecc(grp_D_rx_data_w_ecc),
 
  .grp_A_rx_pfc_xoff(grp_A_rx_pfc_xoff),
  .grp_B_rx_pfc_xoff(grp_B_rx_pfc_xoff),
  .grp_C_rx_pfc_xoff(grp_C_rx_pfc_xoff),
  .grp_D_rx_pfc_xoff(grp_D_rx_pfc_xoff),
  
  .grp_A_rx_flow_control_tc(grp_A_rx_flow_control_tc),
  .grp_B_rx_flow_control_tc(grp_B_rx_flow_control_tc),
  .grp_C_rx_flow_control_tc(grp_C_rx_flow_control_tc),
  .grp_D_rx_flow_control_tc(grp_D_rx_flow_control_tc),
  
  .vp_rx_ecc(vp_rx_ecc),
  .vp_rx_port_num(vp_rx_port_num),
  .vp_rx_data_valid(vp_rx_data_valid),
  .vp_rx_metadata(vp_rx_metadata),
  .vp_rx_time_stamp(vp_rx_time_stamp),
  .vp_cpp_rx_metadata(vp_cpp_rx_metadata),
  .vp_rx_data_w_ecc(vp_rx_data_w_ecc),
  .vp_rx_flow_control_tc(vp_rx_flow_control_tc),
  .vp_tx_pfc_xoff(vp_tx_pfc_xoff),

  
  .igr_rx_ppe(igr_rx_ppe),
  .rx_ppe_igr(rx_ppe_igr)
  

);


initial
begin
`include "tst"
  $finish;
end


endmodule
