//`timescale 1ns/100ps
//  `include "mby_rp_inq_pkg.sv"

module TB_IGR_TOP
  import mby_igr_pkg::*, mby_rx_metadata_pkg::*,shared_pkg::*;
(

);

  // Clock.
  logic                               cclk;
  // Unsynchronized warm reset.
  logic                               rst= '0;
  
  
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

  logic [EPL_IGR_TS_W-1:0]                   grp_a_rx_time_stamp = '0;
  logic [EPL_IGR_TS_W-1:0]                   grp_b_rx_time_stamp = '0;
  logic [EPL_IGR_TS_W-1:0]                   grp_c_rx_time_stamp = '0;
  logic [EPL_IGR_TS_W-1:0]                   grp_d_rx_time_stamp = '0;
   
  logic [0:7][63:0]      grp_a_rx_data = '0;
  logic [0:7][63:0]      grp_b_rx_data = '0;
  logic [0:7][63:0]      grp_c_rx_data = '0;
  logic [0:7][63:0]      grp_d_rx_data = '0;
  
 
  logic [3:0]                  grp_a_tx_pfc_xoff;
  logic [3:0]                  grp_b_tx_pfc_xoff;
  logic [3:0]                  grp_c_tx_pfc_xoff;
  logic [3:0]                  grp_d_tx_pfc_xoff;
  logic                     grp_a_tx_pfc_tc_sync;
  logic                     grp_b_tx_pfc_tc_sync;
  logic                     grp_c_tx_pfc_tc_sync;
  logic                     grp_d_tx_pfc_tc_sync;

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
  
  igr_rx_ppe_tail_t          igr_rx_ppe_intf0_tail; // Blasted Interface igr_rx_ppe_if.igr igr_rx_ppe
  igr_rx_ppe_head_t          igr_rx_ppe_intf0_head; 
  logic                       igr_rx_ppe_intf0_ack; 
  igr_rx_ppe_tail_t          igr_rx_ppe_intf1_tail; 
  igr_rx_ppe_head_t          igr_rx_ppe_intf1_head; 
  logic                       igr_rx_ppe_intf1_ack; 

  rx_ppe_igr_t                rx_ppe_igr_intf0; // Blasted Interface rx_ppe_igr_if.igr rx_ppe_igr
  rx_ppe_igr_t                rx_ppe_igr_intf1; 

    assign igr_rx_ppe_intf0_tail = igr_rx_ppe.intf0_tail;
    assign igr_rx_ppe_intf0_head = igr_rx_ppe.intf0_head;
    assign igr_rx_ppe.intf0_ack = igr_rx_ppe_intf0_ack;
    assign igr_rx_ppe_intf1_tail = igr_rx_ppe.intf1_tail;
    assign igr_rx_ppe_intf1_head = igr_rx_ppe.intf1_head;
    assign igr_rx_ppe.intf1_ack = igr_rx_ppe_intf1_ack;

    assign rx_ppe_igr.intf0 = rx_ppe_igr_t'(rx_ppe_igr_intf0);
    assign rx_ppe_igr.intf1 = rx_ppe_igr_t'(rx_ppe_igr_intf1);

    rx_ppe_bfm  rx_ppe
      (
      .cclk                                    ( cclk ),
      .rst                                     ( rst ),

      .igr_rx_ppe_intf0_ack                    ( igr_rx_ppe_intf0_ack ),
      .igr_rx_ppe_intf1_ack                    ( igr_rx_ppe_intf1_ack ),
      .igr_rx_ppe_intf0_tail                   ( igr_rx_ppe_intf0_tail ),
      .igr_rx_ppe_intf0_head                   ( igr_rx_ppe_intf0_head ),
      .igr_rx_ppe_intf1_tail                   ( igr_rx_ppe_intf1_tail ),
      .igr_rx_ppe_intf1_head                   ( igr_rx_ppe_intf1_head ),
      .rx_ppe_igr_intf0                        ( rx_ppe_igr_intf0 ),
      .rx_ppe_igr_intf1                        ( rx_ppe_igr_intf1 )

       );
    

  mim_wr_if    egr_igr_wreq();
  mim_rd_if    igr_egr_rreq();  
  mim_wr_if    mim_wreq_0();
  mim_wr_if    mim_wreq_1();
  mim_wr_if    mim_wreq_2();
  mim_wr_if    mim_wreq_3();
  mim_wr_if    mim_wreq_4();
  mim_wr_if    mim_wreq_5();


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
  
  .grp_a_rx_data(grp_a_rx_data),
  .grp_b_rx_data(grp_b_rx_data),
  .grp_c_rx_data(grp_c_rx_data),
  .grp_d_rx_data(grp_d_rx_data),

  .grp_a_tx_pfc_xoff(grp_a_tx_pfc_xoff),
  .grp_b_tx_pfc_xoff(grp_b_tx_pfc_xoff),
  .grp_c_tx_pfc_xoff(grp_c_tx_pfc_xoff),
  .grp_d_tx_pfc_xoff(grp_d_tx_pfc_xoff),
  
  .grp_a_tx_pfc_tc_sync(grp_a_tx_pfc_tc_sync),
  .grp_b_tx_pfc_tc_sync(grp_b_tx_pfc_tc_sync),
  .grp_c_tx_pfc_tc_sync(grp_c_tx_pfc_tc_sync),
  .grp_d_tx_pfc_tc_sync(grp_d_tx_pfc_tc_sync),
  
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
  .rx_ppe_igr(rx_ppe_igr),
  
  .o_tag_ring_lltag0(),
  .o_tag_ring_lltag1(),

  // Egress Interface ///////////////////////////////////////////////////////////

  // Dirty pointer Write port
  .egr_igr_wreq(egr_igr_wreq),

  // Pointer cache read/rsp  
  .igr_egr_rreq(igr_egr_rreq),
  
 
// Mesh Interface (MIM block) /////////////////////////////////////////////////
                        
                        
  // Write mesh row 0                     
  .mim_wreq_0(mim_wreq_0),
  .mim_wreq_1(mim_wreq_1),
  .mim_wreq_2(mim_wreq_2),
  
  // Write mesh row 1
  .mim_wreq_3(mim_wreq_3),
  .mim_wreq_4(mim_wreq_4),
  .mim_wreq_5(mim_wreq_5),

// Global Congestion (GCM) Management interface
  .i_rx_cm_wm_out('0),    // RX watermark
  .i_rx_cm_sm_wm_out('0),  // Shared watermark
  
  // global policer IGR
  .o_gpolring0(),
  .o_gpolring1(),
  .i_gpolring_update0('0),
  .i_gpolring_update1('0)

);


initial
begin
`include "tst"
  $finish;
end


endmodule
