//`timescale 1ns/100ps
//  `include "mby_rp_inq_pkg.sv"

module TB_IGR_TOP
  import mby_igr_pkg::*, shared_pkg::*;
(

);

  // Clock.
  logic                               cclk;
  // Unsynchronized warm reset.
  logic                               rst= '0;
  
  logic [7:0]                       grp_a_rx_ecc = '0;
  logic [7:0]                       grp_b_rx_ecc = '0;
  logic [7:0]                       grp_c_rx_ecc = '0;
  logic [7:0]                       grp_d_rx_ecc = '0;
  
  logic [1:0]                  grp_a_rx_port_num = '0;
  logic [1:0]                  grp_b_rx_port_num = '0;
  logic [1:0]                  grp_c_rx_port_num = '0;
  logic [1:0]                  grp_d_rx_port_num = '0;
  
  logic [7:0]                grp_a_rx_data_valid = '0;
  logic [7:0]                grp_b_rx_data_valid = '0;
  logic [7:0]                grp_c_rx_data_valid = '0;
  logic [7:0]                grp_d_rx_data_valid = '0;
  
  epl_md_t                     grp_a_rx_metadata = '0;
  epl_md_t                     grp_b_rx_metadata = '0;
  epl_md_t                     grp_c_rx_metadata = '0;
  epl_md_t                     grp_d_rx_metadata = '0;

  epl_ts_t                   grp_a_rx_time_stamp = '0;
  epl_ts_t                   grp_b_rx_time_stamp = '0;
  epl_ts_t                   grp_c_rx_time_stamp = '0;
  epl_ts_t                   grp_d_rx_time_stamp = '0;
   
  data64_w_ecc_t [0:7]       grp_a_rx_data_w_ecc = '0;
  data64_w_ecc_t [0:7]       grp_b_rx_data_w_ecc = '0;
  data64_w_ecc_t [0:7]       grp_c_rx_data_w_ecc = '0;
  data64_w_ecc_t [0:7]       grp_d_rx_data_w_ecc = '0;
 
  logic                        grp_a_rx_pfc_xoff = '0;
  logic                        grp_b_rx_pfc_xoff = '0;
  logic                        grp_c_rx_pfc_xoff = '0;
  logic                        grp_d_rx_pfc_xoff = '0;
  
  logic [2:0]           grp_a_rx_flow_control_tc = '0;
  logic [2:0]           grp_b_rx_flow_control_tc = '0;
  logic [2:0]           grp_c_rx_flow_control_tc = '0;
  logic [2:0]           grp_d_rx_flow_control_tc = '0;

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


mby_igr_top
  top(
  // Clock.
  .cclk(cclk),
  // Unsynchronized warm reset.
  .rst(rst),
      
// EPL I/O from MBY FS Dataplane Interface signals.
  //EPL0
  .grp_a_rx_ecc(grp_a_rx_ecc),
  .grp_b_rx_ecc(grp_b_rx_ecc),
  .grp_c_rx_ecc(grp_c_rx_ecc),
  .grp_d_rx_ecc(grp_d_rx_ecc),

  .grp_a_rx_port_num(grp_a_rx_port_num),
  .grp_b_rx_port_num(grp_b_rx_port_num),
  .grp_c_rx_port_num(grp_c_rx_port_num),
  .grp_d_rx_port_num(grp_d_rx_port_num),
  
  .grp_a_rx_data_valid(grp_a_rx_data_valid),
  .grp_b_rx_data_valid(grp_b_rx_data_valid),
  .grp_c_rx_data_valid(grp_c_rx_data_valid),
  .grp_d_rx_data_valid(grp_d_rx_data_valid),
  
  .grp_a_rx_metadata(grp_a_rx_metadata),
  .grp_b_rx_metadata(grp_b_rx_metadata),
  .grp_c_rx_metadata(grp_c_rx_metadata),
  .grp_d_rx_metadata(grp_d_rx_metadata),

  .grp_a_rx_time_stamp(grp_a_rx_time_stamp),
  .grp_b_rx_time_stamp(grp_b_rx_time_stamp),
  .grp_c_rx_time_stamp(grp_c_rx_time_stamp),
  .grp_d_rx_time_stamp(grp_d_rx_time_stamp),
  
  .grp_a_rx_data_w_ecc(grp_a_rx_data_w_ecc),
  .grp_b_rx_data_w_ecc(grp_b_rx_data_w_ecc),
  .grp_c_rx_data_w_ecc(grp_c_rx_data_w_ecc),
  .grp_d_rx_data_w_ecc(grp_d_rx_data_w_ecc),
 
  .grp_a_rx_pfc_xoff(grp_a_rx_pfc_xoff),
  .grp_b_rx_pfc_xoff(grp_b_rx_pfc_xoff),
  .grp_c_rx_pfc_xoff(grp_c_rx_pfc_xoff),
  .grp_d_rx_pfc_xoff(grp_d_rx_pfc_xoff),
  
  .grp_a_rx_flow_control_tc(grp_a_rx_flow_control_tc),
  .grp_b_rx_flow_control_tc(grp_b_rx_flow_control_tc),
  .grp_c_rx_flow_control_tc(grp_c_rx_flow_control_tc),
  .grp_d_rx_flow_control_tc(grp_d_rx_flow_control_tc),
  
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
