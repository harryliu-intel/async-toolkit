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


egr_top egress (
   .clk   (egress_primary_clock),
   .arst_n (egress_primary_reset),
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
