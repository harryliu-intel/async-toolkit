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


igr_top
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
  .rx_ppe_igr(rx_ppe_igr),

  // Egress Interface ///////////////////////////////////////////////////////////

  // Dirty pointer Write port
/*  input  logic */                     .i_egr_wreq_valid('0), 
/*  input  logic [W_SEG_PTR-1:0] */     .i_egr_wr_seg_ptr('0), //[19:0]
/*  input  logic [W_WD_SEL-1:0] */      .i_egr_wr_wd_sel('0),  //[ 2:0]
/*  input  logic [W_REQ_ID-1:0]  */     .i_egr_wreq_id('0),    //[12:0]
/*  input  logic [MSH_DATA_WIDTH-1:0] */ .i_egr_wr_data('0),    // 64*8
  
// Assume IGR has some kind of ACK for buffer space  
/*  output logic [W_XACT_CREDITS-1:0] */ .o_egr_wreq_credits(), // temp value
  
  
  // Pointer cache read/rsp

/*  output logic */                    .o_egr_rreq_valid(),
/*  output logic [W_SEG_PTR-1:0] */   .o_egr_seg_ptr(),      //[19:0]
/*  output logic [W_SEMA-1:0] */      .o_egr_sema(),         //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */    .o_egr_wd_sel(),       //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */    .o_egr_req_id(),       //[12:0]
    
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_egr_rreq_credits('0), // temp value   
  
  
/*  input  logic  */                       .i_egr_rrsp_valid('0),
/*  input  logic [W_RRSP_DEST_BLOCK-1:0] */ .i_egr_rrsp_dest_block('0),  //[2:0]
/*  input  logic [W_REQ_ID-1:0] */         .i_egr_rrsp_req_id('0),      //[12:0]
/*  input  logic [MSH_DATA_WIDTH-1:0] */   .i_egr_rd_data('0),          //64 x 8

  
// Mesh Interface (MIM block) /////////////////////////////////////////////////
                        
  // Write port 0
/*  output logic  */                    .o_mim_wreq_valid_w0(), 
/*  output logic [W_SEG_PTR-1:0] */     .o_mim_wr_seg_ptr_w0(), //[19:0]
/*  output logic [W_SEMA-1:0] */        .o_mim_wr_sema_w0(),    //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */      .o_mim_wr_wd_sel_w0(),  //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */      .o_mim_wreq_id_w0(),    //[12:0]
/*  output logic [MSH_DATA_WIDTH-1:0] */ .o_mim_wr_data_w0(),    // 64*8
         
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_mim_wreq_credits_w0('0), // temp value
  
  
  // Write port 1
/*  output logic */                     .o_mim_wreq_valid_w1(), 
/*  output logic [W_SEG_PTR-1:0] */     .o_mim_wr_seg_ptr_w1(), //[19:0]
/*  output logic [W_SEMA-1:0] */        .o_mim_wr_sema_w1(),    //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */      .o_mim_wr_wd_sel_w1(),  //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */      .o_mim_wreq_id_w1(),    //[12:0]
/*  output logic [MSH_DATA_WIDTH-1:0] */ .o_mim_wr_data_w1(),    // 64*8
         
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_mim_wreq_credits_w1('0), // temp value
  
  
  // Write port 2
/*  output logic */                     .o_mim_wreq_valid_w2(), 
/*  output logic [W_SEG_PTR-1:0] */     .o_mim_wr_seg_ptr_w2(), //[19:0]
/*  output logic [W_SEMA-1:0] */         .o_mim_wr_sema_w2(),    //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */      .o_mim_wr_wd_sel_w2(),  //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */      .o_mim_wreq_id_w2(),    //[12:0]
/*  output logic [MSH_DATA_WIDTH-1:0] */ .o_mim_wr_data_w2(),    // 64*8
         
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_mim_wreq_credits_w2('0), // temp value
  
  
  // Write port 3
/*  output logic */                     .o_mim_wreq_valid_w3(), 
/*  output logic [W_SEG_PTR-1:0] */     .o_mim_wr_seg_ptr_w3(), //[19:0]
/*  output logic [W_SEMA-1:0] */        .o_mim_wr_sema_w3(),    //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */      .o_mim_wr_wd_sel_w3(),  //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */      .o_mim_wreq_id_w3(),    //[12:0]
/*  output logic [MSH_DATA_WIDTH-1:0] */ .o_mim_wr_data_w3(),    // 64*8
         
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_mim_wreq_credits_w3('0), // temp value
  
  
  // Write port 4
/*  output logic */                     .o_mim_wreq_valid_w4(), 
/*  output logic [W_SEG_PTR-1:0] */     .o_mim_wr_seg_ptr_w4(), //[19:0]
/*  output logic [W_SEMA-1:0] */        .o_mim_wr_sema_w4(),    //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */      .o_mim_wr_wd_sel_w4(),  //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */      .o_mim_wreq_id_w4(),    //[12:0]
/*  output logic [MSH_DATA_WIDTH-1:0] */ .o_mim_wr_data_w4(),    // 64*8
         
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_mim_wreq_credits_w4('0), // temp value
  
  
  // Write port 5
/*  output logic  */                    .o_mim_wreq_valid_w5(), 
/*  output logic [W_SEG_PTR-1:0] */      .o_mim_wr_seg_ptr_w5(), //[19:0]
/*  output logic [W_SEMA-1:0] */        .o_mim_wr_sema_w5(),    //[ 3:0]
/*  output logic [W_WD_SEL-1:0] */      .o_mim_wr_wd_sel_w5(),  //[ 2:0]
/*  output logic [W_REQ_ID-1:0] */      .o_mim_wreq_id_w5(),    //[12:0]
/*  output logic [MSH_DATA_WIDTH-1:0] */ .o_mim_wr_data_w5(),    // 64*8
         
/*  input  logic [W_XACT_CREDITS-1:0] */ .i_mim_wreq_credits_w5('0) // temp value
  

);


initial
begin
`include "tst"
  $finish;
end


endmodule
