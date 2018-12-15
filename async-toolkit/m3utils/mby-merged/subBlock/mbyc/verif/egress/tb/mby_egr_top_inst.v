//-----------------------------------------------------------------------------
// Title         : Egress top instance module
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_egr_top_inst.v
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
//
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

mim_wr_if         igr_dirtypod_if(); //IGR Dirty Pod Write Interface //TODO Modify port names to similar of IGR
mim_rd_if         igr_cleanpod_if(); //IGR Clean Pod Read Req/Rsp Interface

mim_rd_if            mim_rd_if0_0(); //MIM Read Request Row0 Plane 0
mim_rd_if            mim_rd_if0_1(); //MIM Read Request Row0 Plane 1
mim_rd_if            mim_rd_if0_2(); //MIM Read Request Row0 Plane 2
mim_rd_if            mim_rd_if1_0(); //MIM Read Request Row1 Plane 0
mim_rd_if            mim_rd_if1_1(); //MIM Read Request Row1 Plane 1
mim_rd_if            mim_rd_if1_2(); //MIM Read Request Row1 Plane 2


egr_pod_if                   pod_if(); //EGR-Pod Ring Interface
egr_mc_mirror_pod_if mc_mirror_pod_if(); //EGR-MC Mirror Pod Ring Interface
egr_ahb_if                   ahb_if(); //EGR-AHB Interface

egr_tagring_if          tagring_if(); //EGR-Tag Ring Interface

egr_mce_tagring_if  mce_tagring_if();  //EGR-Tag Ring Interface
egr_tx_ppe_if            tx_ppe_if0(); //EGR-TxPPE 0 Interface
egr_tx_ppe_if            tx_ppe_if1(); //EGR-TxPPE 1 Interface
egr_ppe_stm_if           ppe_stm_if(); //EGR-PPE Shared Table Memory Interface
egr_epl_if                  epl_if0(); //EGR-EPL 0 Interface
egr_epl_if                  epl_if1(); //EGR-EPL 1 Interface
egr_epl_if                  epl_if2(); //EGR-EPL 2 Interface
egr_epl_if                  epl_if3(); //EGR-EPL 3 Interface
egr_mc_table_if        mc_table_if0(); //EGR-MultiCast Shared Table 0 Interface
egr_mc_table_if        mc_table_if1(); //EGR-MultiCast Shared Table 1 Interface
egr_cmring_if               cmring_if(); //EGR-Congestion Management Ring Interface

assign tagring_if.mby_tag_ring[0] = tag_bfm_intf_0.intf_data_pkt;
assign tagring_if.mby_tag_ring[1] = tag_bfm_intf_1.intf_data_pkt;



assign memrd_req_if.intf_data_pkt.mim_rreq_valid = mim_rd_if0_0.mim_rreq_valid ;
assign memrd_req_if.intf_data_pkt.mim_seg_ptr = mim_rd_if0_0.mim_seg_ptr;
assign memrd_req_if.intf_data_pkt.mim_sema = mim_rd_if0_0.mim_sema;
assign memrd_req_if.intf_data_pkt.mim_req_id = mim_rd_if0_0.mim_req_id;
assign memrd_req_if.intf_data_pkt.mim_wd_sel = mim_rd_if0_0.mim_wd_sel;

assign mim_rd_if0_0.mim_rd_data = memrd_req_if.intf_data_pkt.mim_rd_data;
assign mim_rd_if0_0.mim_rreq_credits = memrd_req_if.intf_data_pkt.mim_rreq_credits;
assign mim_rd_if0_0.mim_rrsp_dest_block = memrd_req_if.intf_data_pkt.mim_rrsp_dest_block;
assign mim_rd_if0_0.mim_rrsp_req_id = memrd_req_if.intf_data_pkt.mim_rrsp_req_id;
assign mim_rd_if0_0.mim_rrsp_valid = memrd_req_if.intf_data_pkt.mim_rrsp_valid;



always_comb ahb_if.dfxsignal_in = 0;
always_comb ahb_if.ahb_req_p = 0;
always_comb ahb_if.ahb_addr = 0;
always_comb ahb_if.ahb_wr = 0;
always_comb ahb_if.ahb_wr_data = 0;



always_ff @(posedge egress_clock) epl_if0.tx_enable_port_num <= egress_reset ? (epl_if0.tx_enable_port_num + 2'b1) : 2'b0;
always_comb epl_if0.tx_enable = 1'b1;
always_ff @(posedge egress_clock) epl_if1.tx_enable_port_num <= egress_reset ? (epl_if1.tx_enable_port_num + 2'b1) : 2'b0;
always_comb epl_if1.tx_enable = 1'b1;
always_ff @(posedge egress_clock) epl_if2.tx_enable_port_num <= egress_reset ? (epl_if2.tx_enable_port_num + 2'b1) : 2'b0;
always_comb epl_if2.tx_enable = 1'b1;
always_ff @(posedge egress_clock) epl_if3.tx_enable_port_num <= egress_reset ? (epl_if3.tx_enable_port_num + 2'b1) : 2'b0;
always_comb epl_if3.tx_enable = 1'b1;

egr_top egress (
   .clk   (egress_clock),
   .arst_n (~egress_reset),
   .igr_dirtypod_if(igr_dirtypod_if), //IGR Dirty Pod Write Interface //TODO Modify port names to similar of IGR
   .igr_cleanpod_if(igr_cleanpod_if), //IGR Clean Pod Read Req/Rsp Interface
   .pod_if(pod_if), //EGR-Pod Ring Interface
   .mc_mirror_pod_if(mc_mirror_pod_if), //EGR-MC Mirror Pod Ring Interface
   .ahb_if(ahb_if), //EGR-AHB and DFx Interface //TODO change IF to iBus

   .mim_rd_if0_0(mim_rd_if0_0), //MIM Read Request Row0 Plane 0
   .mim_rd_if0_1(mim_rd_if0_1), //MIM Read Request Row0 Plane 1
   .mim_rd_if0_2(mim_rd_if0_2), //MIM Read Request Row0 Plane 2
   .mim_rd_if1_0(mim_rd_if1_0), //MIM Read Request Row1 Plane 0
   .mim_rd_if1_1(mim_rd_if1_1), //MIM Read Request Row1 Plane 1
   .mim_rd_if1_2(mim_rd_if1_2), //MIM Read Request Row1 Plane 2
   .tagring_if(tagring_if), //EGR-Tag Ring Interface MGP
   .mce_tagring_if(mce_tagring_if), //EGR-Tag Ring Interface
   .cmring_if(cmring_if), //EGR-Congestion Management Ring Interface
   .tx_ppe_if0(tx_ppe_if0), //EGR-TxPPE 0 Interface
   .tx_ppe_if1(tx_ppe_if1), //EGR-TxPPE 1 Interface
   .ppe_stm_if(ppe_stm_if), //EGR-PPE Shared Table Memory Interface
   .epl_if0(epl_if0), //EGR-EPL 0 Interface
   .epl_if1(epl_if1), //EGR-EPL 1 Interface
   .epl_if2(epl_if2), //EGR-EPL 2 Interface
   .epl_if3(epl_if3), //EGR-EPL 3 Interface
   .mc_table_if0(mc_table_if0), //EGR-MultiCast Shared Table 0 Interface
   .mc_table_if1(mc_table_if1)  //EGR-MultiCast Shared Table 1 Interface
);
