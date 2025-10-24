// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0


module tm_wac_common_afifo_AW_2_DW_6_8 (
	wen, 
	wdata, 
	wful, 
	ren, 
	remp, 
	rdata, 
	LPCKG_tm_wac_common__RC_CG_TEST_PORT, 
	DFT_sdo, 
	DFT_sdo_2, 
	DFT_sdo_4, 
	DFT_sdo_6, 
	DFT_sdo_8, 
	DFT_sdi_9, 
	DFT_sdo_14, 
	n_107754, 
	SPCASCAN_N16776, 
	adrd_wclk_2, 
	adrd_wclk_0, 
	mem_0__3, 
	mem_0__4, 
	mem_2__3, 
	mem_2__4, 
	mem_1__4, 
	mem_0__0, 
	mem_2__0, 
	mem_2__5, 
	mem_3__0, 
	mem_0__5, 
	adwr_gray_1, 
	mem_2__2, 
	mem_1__5, 
	mem_1__1, 
	adwr_bin_0, 
	mem_1__2, 
	adwr_bin_1, 
	mem_3__2, 
	adwr_rclk_0, 
	adwr_rclk_2, 
	adrd_gray_2, 
	adrd_bin_1, 
	adrd_bin_2, 
	adrd_bin_0, 
	adrd_gray_0, 
	adwr_bin_2, 
	mem_2__1, 
	mem_3__3, 
	mem_3__1, 
	FE_OFN1025_reset_pps, 
	FE_OFN4955_n_301128, 
	FE_OFN4952_n_301128, 
	mem_1__0, 
	mem_0__2, 
	afifo_2qac_pre_2_2_BFN_mem_1__3, 
	mem_1__3, 
	pref_vld, 
	adwr_gray_2, 
	adwr_gray_0, 
	FE_OFN18597_n_301128, 
	FE_OFN18555_n_301128, 
	FE_OFN18554_n_301128, 
	pre_fifo_drop_cnt_1__3__3_1, 
	SPCASCAN_N21890, 
	afifo_2qac_pre_2_2_BFN_mem_1__2, 
	n_134355, 
	afifo_2qac_pre_2_2_BFN_adwr_rclk_2_1, 
	afifo_2qac_pre_2_2_BFN_adwr_gray_0_1, 
	p_6, 
	p_7, 
	rclk, 
	wclk, 
	FE_OFN39572_FE_OFN4711_n_301128, 
	FE_OFN40580_n, 
	rclk_clone1, 
	wclk_clone1, 
	wclk_clone2, 
	pre_fifo_drop_rdata_2__3__0, 
	SPCASCAN_N8309);
   input wen;
   input [5:0] wdata;
   output wful;
   input ren;
   output remp;
   output [5:0] rdata;
   input LPCKG_tm_wac_common__RC_CG_TEST_PORT;
   output DFT_sdo;
   output DFT_sdo_2;
   output DFT_sdo_4;
   output DFT_sdo_6;
   output DFT_sdo_8;
   input DFT_sdi_9;
   output DFT_sdo_14;
   input n_107754;
   input SPCASCAN_N16776;
   output adrd_wclk_2;
   output adrd_wclk_0;
   output mem_0__3;
   output mem_0__4;
   output mem_2__3;
   output mem_2__4;
   output mem_1__4;
   output mem_0__0;
   output mem_2__0;
   output mem_2__5;
   output mem_3__0;
   output mem_0__5;
   output adwr_gray_1;
   output mem_2__2;
   output mem_1__5;
   output mem_1__1;
   output adwr_bin_0;
   output mem_1__2;
   output adwr_bin_1;
   output mem_3__2;
   output adwr_rclk_0;
   output adwr_rclk_2;
   output adrd_gray_2;
   output adrd_bin_1;
   output adrd_bin_2;
   output adrd_bin_0;
   output adrd_gray_0;
   output adwr_bin_2;
   output mem_2__1;
   output mem_3__3;
   output mem_3__1;
   input FE_OFN1025_reset_pps;
   input FE_OFN4955_n_301128;
   input FE_OFN4952_n_301128;
   output mem_1__0;
   output mem_0__2;
   input afifo_2qac_pre_2_2_BFN_mem_1__3;
   output mem_1__3;
   output pref_vld;
   output adwr_gray_2;
   output adwr_gray_0;
   input FE_OFN18597_n_301128;
   input FE_OFN18555_n_301128;
   input FE_OFN18554_n_301128;
   input pre_fifo_drop_cnt_1__3__3_1;
   input SPCASCAN_N21890;
   input afifo_2qac_pre_2_2_BFN_mem_1__2;
   input n_134355;
   input afifo_2qac_pre_2_2_BFN_adwr_rclk_2_1;
   input afifo_2qac_pre_2_2_BFN_adwr_gray_0_1;
   input p_6;
   input p_7;
   input rclk;
   input wclk;
   input FE_OFN39572_FE_OFN4711_n_301128;
   input FE_OFN40580_n;
   input rclk_clone1;
   input wclk_clone1;
   input wclk_clone2;
   input pre_fifo_drop_rdata_2__3__0;
   input SPCASCAN_N8309;

   // Internal wires
   wire FE_PHN174566_SPCBSCAN_N27982;
   wire FE_PHN156264_SPCHSCAN_N20015;
   wire FE_PHN154742_SPCHSCAN_N20014;
   wire FE_PHN150344_SPCASCAN_N15475;
   wire FE_PHN148926_n_134871;
   wire FE_PHN144359_SPCASCAN_N16799;
   wire FE_PHN140010_SPCBSCAN_N27982;
   wire FE_PHN131170_FE_PSRN_1;
   wire FE_PHN123509_n_7;
   wire FE_PHN123407_n_9;
   wire FE_PHN113694_n_134871;
   wire FE_PHN109622_SPCBSCAN_N27982;
   wire FE_PHN109546_SPCASCAN_N16812;
   wire FE_PHN76605_SPCASCAN_N22691;
   wire FE_PHN67718_SPCASCAN_N15471;
   wire FE_PHN66920_n_134880;
   wire FE_PHN61167_SPCCSCAN_N29728;
   wire FE_PHN60359_SPCASCAN_N16809;
   wire FE_PHN58475_SPCASCAN_N15479;
   wire FE_PHN56782_SPCASCAN_N22694;
   wire FE_PHN56690_SPCASCAN_N15472;
   wire FE_PHN55038_SPCASCAN_N16810;
   wire FE_PHN54932_SPCASCAN_N16802;
   wire FE_PHN54391_SPCASCAN_N16801;
   wire FE_PHN53133_SPCASCAN_N15478;
   wire FE_PHN52600_SPCLSCAN_N23152;
   wire FE_PHN52542_SPC_SCAN_N38688;
   wire FE_PHN52517_SPCASCAN_N15476;
   wire FE_PHN51885_n_173143;
   wire FE_PHN51845_SPCASCAN_N16806;
   wire FE_PHN50473_SPCASCAN_N15483;
   wire FE_PHN49688_SPCBSCAN_N27988;
   wire FE_PHN48105_SPCASCAN_N15473;
   wire FE_PHN46389_SPCASCAN_N15480;
   wire FE_PHN46318_SPCASCAN_N15482;
   wire FE_PHN45114_SPCASCAN_N16799;
   wire FE_PHN44502_SPCASCAN_N15474;
   wire FE_PHN41400_SPCASCAN_N16813;
   wire FE_PHN40145_SPCASCAN_N16804;
   wire FE_PHN38744_SPCASCAN_N16805;
   wire FE_PHN36263_SPCHSCAN_N20015;
   wire FE_PHN30946_SPCHSCAN_N20014;
   wire FE_PHN27196_SPCASCAN_N15475;
   wire FE_PHN16405_n_134356;
   wire FE_PSRN_1;
   wire CTS_3;
   wire CTS_2;
   wire [2:0] wcnt;
   wire FE_OFN330543_n_41;
   wire [2:0] adwr_gray;
   wire [2:0] adrd_bnext;
   wire [5:0] mem_1_;
   wire [5:0] mem_0_;
   wire LPCKG_tm_wac_common__rc_gclk;
   wire n_1;
   wire n_2;
   wire n_3;
   wire n_5;
   wire n_6;
   wire n_7;
   wire n_8;
   wire n_9;
   wire n_10;
   wire n_11;
   wire n_12;
   wire n_13;
   wire n_14;
   wire n_15;
   wire n_16;
   wire n_17;
   wire n_18;
   wire n_19;
   wire n_20;
   wire n_21;
   wire n_22;
   wire n_23;
   wire n_24;
   wire n_25;
   wire n_26;
   wire n_27;
   wire n_28;
   wire n_30;
   wire n_31;
   wire n_32;
   wire n_35;
   wire n_36;
   wire n_37;
   wire n_38;
   wire n_40;
   wire n_42;
   wire n_43;
   wire n_44;
   wire n_45;
   wire n_49;
   wire n_50;
   wire n_52;
   wire n_53;
   wire n_54;
   wire n_55;
   wire n_56;
   wire n_57;
   wire n_58;
   wire n_59;
   wire n_65;
   wire n_66;
   wire n_67;
   wire n_68;
   wire n_69;
   wire n_70;
   wire n_71;
   wire n_96;
   wire n_97;
   wire n_98;
   wire n_99;
   wire n_100;

   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC174566_SPCBSCAN_N27982 (.I(FE_PHN140010_SPCBSCAN_N27982),
	.Z(FE_PHN174566_SPCBSCAN_N27982));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC156264_SPCHSCAN_N20015 (.I(FE_PHN156264_SPCHSCAN_N20015),
	.Z(FE_PHN36263_SPCHSCAN_N20015));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC154742_SPCHSCAN_N20014 (.I(FE_PHN154742_SPCHSCAN_N20014),
	.Z(FE_PHN30946_SPCHSCAN_N20014));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC150344_SPCASCAN_N15475 (.I(FE_PHN27196_SPCASCAN_N15475),
	.Z(FE_PHN150344_SPCASCAN_N15475));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC148926_n_134871 (.I(FE_PHN113694_n_134871),
	.Z(FE_PHN148926_n_134871));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC144359_SPCASCAN_N16799 (.I(FE_PHN144359_SPCASCAN_N16799),
	.Z(FE_PHN45114_SPCASCAN_N16799));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC140010_SPCBSCAN_N27982 (.I(FE_PHN109622_SPCBSCAN_N27982),
	.Z(FE_PHN140010_SPCBSCAN_N27982));
   DELAD1BWP240H11P57PDSVT PR_HOLDFE_PHC131170_FE_PSRN_1 (.I(FE_PHN131170_FE_PSRN_1),
	.Z(FE_PSRN_1));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC123509_n_7 (.I(n_7),
	.Z(FE_PHN123509_n_7));
   DELAD1BWP240H11P57PDSVT PR_HOLDFE_PHC123407_n_9 (.I(FE_PHN123407_n_9),
	.Z(n_9));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC113694_n_134871 (.I(DFT_sdi_9),
	.Z(FE_PHN113694_n_134871));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC109622_SPCBSCAN_N27982 (.I(adwr_bin_2),
	.Z(FE_PHN109622_SPCBSCAN_N27982));
   BUFFD1BWP240H11P57PDSVT PR_HOLDFE_PHC109546_SPCASCAN_N16812 (.I(FE_PHN109546_SPCASCAN_N16812),
	.Z(adwr_bin_0));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC76605_SPCASCAN_N22691 (.I(adrd_bin_2),
	.Z(FE_PHN76605_SPCASCAN_N22691));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC67718_SPCASCAN_N15471 (.I(mem_0__4),
	.Z(FE_PHN67718_SPCASCAN_N15471));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC66920_n_134880 (.I(DFT_sdo_14),
	.Z(FE_PHN66920_n_134880));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC61167_SPCCSCAN_N29728 (.I(afifo_2qac_pre_2_2_BFN_adwr_gray_0_1),
	.Z(FE_PHN61167_SPCCSCAN_N29728));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC60359_SPCASCAN_N16809 (.I(FE_PHN60359_SPCASCAN_N16809),
	.Z(mem_2__2));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC58475_SPCASCAN_N15479 (.I(FE_PHN58475_SPCASCAN_N15479),
	.Z(DFT_sdo_4));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC56782_SPCASCAN_N22694 (.I(adrd_gray_0),
	.Z(FE_PHN56782_SPCASCAN_N22694));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC56690_SPCASCAN_N15472 (.I(FE_PHN56690_SPCASCAN_N15472),
	.Z(mem_1__0));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC55038_SPCASCAN_N16810 (.I(mem_1__5),
	.Z(FE_PHN55038_SPCASCAN_N16810));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC54932_SPCASCAN_N16802 (.I(afifo_2qac_pre_2_2_BFN_mem_1__3),
	.Z(FE_PHN54932_SPCASCAN_N16802));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC54391_SPCASCAN_N16801 (.I(mem_1__4),
	.Z(FE_PHN54391_SPCASCAN_N16801));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC53133_SPCASCAN_N15478 (.I(mem_0__5),
	.Z(FE_PHN53133_SPCASCAN_N15478));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC52600_SPCLSCAN_N23152 (.I(mem_1__1),
	.Z(FE_PHN52600_SPCLSCAN_N23152));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC52542_SPC_SCAN_N38688 (.I(mem_2__4),
	.Z(FE_PHN52542_SPC_SCAN_N38688));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC52517_SPCASCAN_N15476 (.I(mem_3__1),
	.Z(FE_PHN52517_SPCASCAN_N15476));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC51885_n_173143 (.I(DFT_sdo_8),
	.Z(FE_PHN51885_n_173143));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC51845_SPCASCAN_N16806 (.I(FE_PHN51845_SPCASCAN_N16806),
	.Z(mem_3__0));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC50473_SPCASCAN_N15483 (.I(mem_1__3),
	.Z(FE_PHN50473_SPCASCAN_N15483));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC49688_SPCBSCAN_N27988 (.I(FE_PHN49688_SPCBSCAN_N27988),
	.Z(mem_2__1));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC48105_SPCASCAN_N15473 (.I(mem_0__2),
	.Z(FE_PHN48105_SPCASCAN_N15473));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC46389_SPCASCAN_N15480 (.I(DFT_sdo_6),
	.Z(FE_PHN46389_SPCASCAN_N15480));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC46318_SPCASCAN_N15482 (.I(mem_0__3),
	.Z(FE_PHN46318_SPCASCAN_N15482));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC45114_SPCASCAN_N16799 (.I(FE_PHN45114_SPCASCAN_N16799),
	.Z(mem_2__3));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC44502_SPCASCAN_N15474 (.I(FE_PHN44502_SPCASCAN_N15474),
	.Z(mem_3__2));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC41400_SPCASCAN_N16813 (.I(mem_1__2),
	.Z(FE_PHN41400_SPCASCAN_N16813));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC40145_SPCASCAN_N16804 (.I(mem_2__0),
	.Z(FE_PHN40145_SPCASCAN_N16804));
   CKBD4BWP240H11P57PDSVT PR_HOLDFE_PHC38744_SPCASCAN_N16805 (.I(mem_2__5),
	.Z(FE_PHN38744_SPCASCAN_N16805));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC36263_SPCHSCAN_N20015 (.I(FE_PHN36263_SPCHSCAN_N20015),
	.Z(adwr_gray_0));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC30946_SPCHSCAN_N20014 (.I(FE_PHN30946_SPCHSCAN_N20014),
	.Z(adwr_gray_2));
   CKBD1BWP240H11P57PDSVT PR_HOLDFE_PHC27196_SPCASCAN_N15475 (.I(adwr_gray_1),
	.Z(FE_PHN27196_SPCASCAN_N15475));
   DELAD1BWP240H11P57PDSVT PR_HOLDFE_PHC16405_n_134356 (.I(DFT_sdo_2),
	.Z(FE_PHN16405_n_134356));
   DELAD1BWP240H11P57PDSVT PR_HOLDFE_PHC14164_SPCASCAN_N22687 (.I(FE_PSRN_1),
	.Z(adrd_gray_2));
   IAOI21D1BWP240H11P57PDLVT FE_RC_1504_0 (.A1(adwr_bin_0),
	.A2(n_58),
	.B(n_59),
	.ZN(n_100));
   INVD3BWP240H11P57PDSVT FE_OFC17223_n_40 (.I(n_40),
	.ZN(FE_OFN330543_n_41));
   MB8SRLSDFQD1BWP240H11P57PDSVT CDN_MBIT_rdata_reg_2__MB_rdata_reg_0__MB_rdata_reg_4__MB_rdata_reg_1__MB_rdata_reg_3__MB_rdata_reg_5__MB_adrd_bin_reg_0__MB_adrd_bin_reg_1_ (.CP(LPCKG_tm_wac_common__rc_gclk),
	.D1(n_26),
	.D2(n_27),
	.D3(n_24),
	.D4(n_28),
	.D5(n_25),
	.D6(n_23),
	.D7(n_32),
	.D8(n_31),
	.Q1(rdata[2]),
	.Q2(rdata[0]),
	.Q3(rdata[4]),
	.Q4(rdata[1]),
	.Q5(rdata[3]),
	.Q6(rdata[5]),
	.Q7(adrd_bin_0),
	.Q8(adrd_bin_1),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN16405_n_134356));
   OR2D1BWP240H11P57PDSVT FE_RC_676_0 (.A1(n_30),
	.A2(n_36),
	.Z(n_40));
   XNR2D4BWP240H11P57PDSVT FE_RC_303_0 (.A1(n_100),
	.A2(n_56),
	.ZN(wful));
   tm_wac_common_LPCKG_tm_wac_common__RC_CG_MOD_1161 LPCKG_tm_wac_common__RC_CG_HIER_INST1161 (.enable(n_71),
	.ck_in(rclk),
	.ck_out(LPCKG_tm_wac_common__rc_gclk),
	.test(LPCKG_tm_wac_common__RC_CG_TEST_PORT));
   tm_wac_common_LPCKG_tm_wac_common__RC_CG_MOD_1162 LPCKG_tm_wac_common__RC_CG_HIER_INST1162 (.enable(n_70),
	.test(LPCKG_tm_wac_common__RC_CG_TEST_PORT),
	.ck_out(CTS_2),
	.ck_out_clone1(CTS_3),
	.ck_in(wclk_clone2));
   tm_wac_common_ctech_sync2_75 adrd_syncer_BFN_syncer0_BFN_DFT_MRK_SYNCER (.CP(wclk_clone1),
	.D(adrd_gray_0),
	.Q(adrd_wclk_0),
	.DFT_sen(FE_OFN4955_n_301128),
	.SPCASCAN_N21890(SPCASCAN_N21890));
   tm_wac_common_ctech_sync2_76 adrd_syncer_BFN_syncer1_BFN_DFT_MRK_SYNCER (.CP(wclk),
	.D(DFT_sdo_14),
	.Q(DFT_sdo),
	.DFT_sdi(SPCASCAN_N16776),
	.DFT_sen(FE_OFN39572_FE_OFN4711_n_301128));
   tm_wac_common_ctech_sync2_77 adrd_syncer_BFN_syncer2_BFN_DFT_MRK_SYNCER (.CP(wclk_clone1),
	.D(adrd_gray_2),
	.Q(adrd_wclk_2),
	.DFT_sen(FE_OFN40580_n),
	.n_134325(p_6));
   tm_wac_common_ctech_sync2_72 adwr_syncer_BFN_syncer0_BFN_DFT_MRK_SYNCER (.CP(rclk_clone1),
	.D(adwr_gray_0),
	.Q(adwr_rclk_0),
	.DFT_sen(FE_OFN40580_n),
	.n_134356(adwr_rclk_2));
   tm_wac_common_ctech_sync2_73 adwr_syncer_BFN_syncer1_BFN_DFT_MRK_SYNCER (.CP(rclk_clone1),
	.D(adwr_gray_1),
	.Q(DFT_sdo_2),
	.DFT_sen(FE_OFN40580_n),
	.n_134355(n_134355));
   tm_wac_common_ctech_sync2_74 adwr_syncer_BFN_syncer2_BFN_DFT_MRK_SYNCER (.CP(rclk_clone1),
	.D(adwr_gray_2),
	.Q(adwr_rclk_2),
	.DFT_sen(FE_OFN4952_n_301128),
	.afifo_2qac_pre_2_2_BFN_adwr_rclk_2(afifo_2qac_pre_2_2_BFN_adwr_rclk_2_1));
   IOA22D1BWP240H11P57PDSVT g1267 (.A1(adwr_bin_1),
	.A2(adrd_wclk_0),
	.B1(adrd_wclk_0),
	.B2(n_57),
	.ZN(n_59));
   INR2D2BWP240H8P57PDSVT g1268 (.A1(n_57),
	.B1(adwr_bin_1),
	.ZN(n_58));
   XNR2D1BWP240H11P57PDSVT g1270 (.A1(FE_PHN109622_SPCBSCAN_N27982),
	.A2(adrd_wclk_2),
	.ZN(n_56));
   INVD2BWP240H11P57PDSVT g1271 (.I(pref_vld),
	.ZN(remp));
   AO21D1BWP240H11P57PDSVT g1300 (.A1(n_66),
	.A2(n_65),
	.B(FE_OFN1025_reset_pps),
	.Z(n_71));
   AO211D1BWP240H11P57PDSVT g1301 (.A1(adrd_bin_1),
	.A2(n_54),
	.B(n_55),
	.C(n_53),
	.Z(n_66));
   MUX2ND1BWP240H11P57PDSVT g1302 (.I0(adrd_bin_1),
	.I1(n_54),
	.S(n_52),
	.ZN(n_55));
   XNR2D1BWP240H11P57PDSVT g1303 (.A1(n_69),
	.A2(adrd_bin_2),
	.ZN(adrd_bnext[2]));
   CKND2D1BWP240H11P57PDSVT g1304 (.A1(n_68),
	.A2(n_67),
	.ZN(adrd_bnext[1]));
   XNR2D1BWP240H11P57PDSVT g1305 (.A1(adwr_rclk_2),
	.A2(DFT_sdo_2),
	.ZN(n_54));
   XOR2D1BWP240H11P57PDSVT g1306 (.A1(adwr_rclk_2),
	.A2(adrd_bin_2),
	.Z(n_53));
   XNR2D1BWP240H11P57PDSVT g1307 (.A1(adrd_bin_0),
	.A2(adwr_rclk_0),
	.ZN(n_52));
   OR2D1BWP240H11P57PDSVT g1308 (.A1(wen),
	.A2(n_107754),
	.Z(n_70));
   IND2D1BWP240H11P57PDSVT g1309 (.A1(adrd_bin_0),
	.B1(adrd_bin_1),
	.ZN(n_67));
   IND2D1BWP240H11P57PDSVT g1310 (.A1(adrd_bin_1),
	.B1(adrd_bin_0),
	.ZN(n_68));
   IND2D1BWP240H11P57PDSVT g1311 (.A1(ren),
	.B1(pref_vld),
	.ZN(n_65));
   CKND2D1BWP240H11P57PDSVT g1312 (.A1(adrd_bin_1),
	.A2(adrd_bin_0),
	.ZN(n_69));
   SDFQD1BWP240H11P57PDSVT adwr_bin_reg_1_ (.CP(CTS_2),
	.D(n_96),
	.Q(adwr_bin_1),
	.SE(FE_OFN18554_n_301128),
	.SI(mem_2__3));
   SDFQD1BWP240H11P57PDSVT adwr_bin_reg_2_ (.CP(CTS_2),
	.D(n_50),
	.Q(adwr_bin_2),
	.SE(FE_OFN4952_n_301128),
	.SI(FE_PHN38744_SPCASCAN_N16805));
   SEDFQD1BWP240H11P57PDSVT mem_reg_2__5_ (.CP(CTS_2),
	.D(wdata[5]),
	.E(n_45),
	.Q(mem_2__5),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN55038_SPCASCAN_N16810));
   SEDFQD1BWP240H11P57PDSVT mem_reg_2__0_ (.CP(CTS_3),
	.D(wdata[0]),
	.E(n_45),
	.Q(mem_2__0),
	.SE(FE_OFN18597_n_301128),
	.SI(FE_PHN54932_SPCASCAN_N16802));
   SEDFQD1BWP240H11P57PDSVT mem_reg_2__1_ (.CP(CTS_3),
	.D(wdata[1]),
	.E(n_45),
	.Q(FE_PHN49688_SPCBSCAN_N27988),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN52517_SPCASCAN_N15476));
   SEDFQD1BWP240H11P57PDSVT mem_reg_2__2_ (.CP(CTS_3),
	.D(wdata[2]),
	.E(n_45),
	.Q(FE_PHN60359_SPCASCAN_N16809),
	.SE(FE_OFN18555_n_301128),
	.SI(mem_3__2));
   SEDFQD1BWP240H11P57PDSVT mem_reg_2__3_ (.CP(CTS_2),
	.D(wdata[3]),
	.E(n_45),
	.Q(FE_PHN144359_SPCASCAN_N16799),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN46389_SPCASCAN_N15480));
   SEDFQD1BWP240H11P57PDSVT mem_reg_2__4_ (.CP(CTS_2),
	.D(wdata[4]),
	.E(n_45),
	.Q(mem_2__4),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN53133_SPCASCAN_N15478));
   SDFQD1BWP240H11P57PDSVT adwr_bin_reg_0_ (.CP(CTS_2),
	.D(n_49),
	.Q(FE_PHN109546_SPCASCAN_N16812),
	.SE(FE_OFN4952_n_301128),
	.SI(FE_PHN109622_SPCBSCAN_N27982));
   INR2D1BWP240H11P57PDSVT g1314 (.A1(n_43),
	.B1(n_107754),
	.ZN(n_50));
   SEDFQD1BWP240H11P57PDSVT mem_reg_0__2_ (.CP(CTS_3),
	.D(wdata[2]),
	.E(n_44),
	.Q(mem_0__2),
	.SE(FE_OFN18555_n_301128),
	.SI(mem_3__0));
   SEDFQD1BWP240H11P57PDSVT mem_reg_0__3_ (.CP(CTS_3),
	.D(wdata[3]),
	.E(n_44),
	.Q(mem_0__3),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN48105_SPCASCAN_N15473));
   SEDFQD1BWP240H11P57PDSVT mem_reg_0__0_ (.CP(CTS_3),
	.D(wdata[0]),
	.E(n_44),
	.Q(mem_0__0),
	.SE(FE_OFN18597_n_301128),
	.SI(afifo_2qac_pre_2_2_BFN_mem_1__2));
   SEDFQD1BWP240H11P57PDSVT mem_reg_0__5_ (.CP(CTS_2),
	.D(wdata[5]),
	.E(n_44),
	.Q(mem_0__5),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN51885_n_173143));
   SEDFQD1BWP240H11P57PDSVT mem_reg_0__4_ (.CP(CTS_3),
	.D(wdata[4]),
	.E(n_44),
	.Q(mem_0__4),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN50473_SPCASCAN_N15483));
   SEDFQD1BWP240H11P57PDSVT mem_reg_0__1_ (.CP(CTS_3),
	.D(wdata[1]),
	.E(n_44),
	.Q(FE_PHN58475_SPCASCAN_N15479),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN46318_SPCASCAN_N15482));
   INR2D1BWP240H11P57PDSVT g1315 (.A1(n_38),
	.B1(n_107754),
	.ZN(n_49));
   SEDFQD1BWP240H11P57PDSVT mem_reg_3__4_ (.CP(CTS_2),
	.D(wdata[4]),
	.E(FE_OFN330543_n_41),
	.Q(DFT_sdo_8),
	.SE(FE_OFN18554_n_301128),
	.SI(adwr_bin_1));
   SEDFQD1BWP240H11P57PDSVT mem_reg_1__5_ (.CP(CTS_2),
	.D(wdata[5]),
	.E(n_42),
	.Q(mem_1__5),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN54391_SPCASCAN_N16801));
   SEDFQD1BWP240H11P57PDSVT mem_reg_3__0_ (.CP(CTS_3),
	.D(wdata[0]),
	.E(FE_OFN330543_n_41),
	.Q(FE_PHN51845_SPCASCAN_N16806),
	.SE(FE_OFN18597_n_301128),
	.SI(FE_PHN40145_SPCASCAN_N16804));
   SEDFQD1BWP240H11P57PDSVT mem_reg_3__1_ (.CP(CTS_3),
	.D(wdata[1]),
	.E(FE_OFN330543_n_41),
	.Q(mem_3__1),
	.SE(FE_OFN18555_n_301128),
	.SI(SPCASCAN_N8309));
   SEDFQD1BWP240H11P57PDSVT mem_reg_3__2_ (.CP(CTS_3),
	.D(wdata[2]),
	.E(FE_OFN330543_n_41),
	.Q(FE_PHN44502_SPCASCAN_N15474),
	.SE(FE_OFN18555_n_301128),
	.SI(p_7));
   SEDFQD1BWP240H11P57PDSVT mem_reg_3__3_ (.CP(CTS_3),
	.D(wdata[3]),
	.E(FE_OFN330543_n_41),
	.Q(mem_3__3),
	.SE(FE_OFN18555_n_301128),
	.SI(DFT_sdo_4));
   SEDFQD1BWP240H11P57PDSVT mem_reg_1__4_ (.CP(CTS_2),
	.D(wdata[4]),
	.E(n_42),
	.Q(mem_1__4),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN52542_SPC_SCAN_N38688));
   SEDFQD1BWP240H11P57PDSVT mem_reg_1__1_ (.CP(CTS_2),
	.D(wdata[1]),
	.E(n_42),
	.Q(mem_1__1),
	.SE(FE_OFN18555_n_301128),
	.SI(mem_1__0));
   SEDFQD1BWP240H11P57PDSVT mem_reg_1__3_ (.CP(CTS_3),
	.D(wdata[3]),
	.E(n_42),
	.Q(mem_1__3),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN61167_SPCCSCAN_N29728));
   SEDFQD1BWP240H11P57PDSVT mem_reg_1__2_ (.CP(CTS_3),
	.D(wdata[2]),
	.E(n_42),
	.Q(mem_1__2),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN67718_SPCASCAN_N15471));
   SEDFQD1BWP240H11P57PDSVT mem_reg_3__5_ (.CP(CTS_2),
	.D(wdata[5]),
	.E(FE_OFN330543_n_41),
	.Q(DFT_sdo_6),
	.SE(FE_OFN18554_n_301128),
	.SI(FE_PHN52600_SPCLSCAN_N23152));
   SEDFQD1BWP240H11P57PDSVT mem_reg_1__0_ (.CP(CTS_2),
	.D(wdata[0]),
	.E(n_42),
	.Q(FE_PHN56690_SPCASCAN_N15472),
	.SE(FE_OFN18555_n_301128),
	.SI(FE_PHN41400_SPCASCAN_N16813));
   CKNR2D1BWP240H11P57PDSVT g1327 (.A1(n_30),
	.A2(n_37),
	.ZN(n_45));
   XNR2D1BWP240H11P57PDSVT g1328 (.A1(n_40),
	.A2(FE_PHN109622_SPCBSCAN_N27982),
	.ZN(n_43));
   CKNR2D1BWP240H11P57PDSVT g1329 (.A1(adwr_bin_1),
	.A2(n_37),
	.ZN(n_44));
   SDFQD1BWP240H11P57PDSVT pref_vld_reg (.CP(rclk_clone1),
	.D(n_99),
	.Q(pref_vld),
	.SE(FE_OFN4955_n_301128),
	.SI(FE_PHN148926_n_134871));
   CKNR2D1BWP240H11P57PDSVT g1332 (.A1(n_36),
	.A2(adwr_bin_1),
	.ZN(n_42));
   IOAI21D1BWP240H11P57PDSVT g1335 (.A1(n_35),
	.A2(adwr_bin_0),
	.B(n_37),
	.ZN(n_38));
   IND2D1BWP240H11P57PDSVT g1336 (.A1(adwr_bin_0),
	.B1(n_35),
	.ZN(n_37));
   ND2OPTPAD1BWP240H11P57PDSVT g1337 (.A1(adwr_bin_0),
	.A2(n_35),
	.ZN(n_36));
   SDFQD1BWP240H11P57PDSVT adrd_bin_reg_2_ (.CP(LPCKG_tm_wac_common__rc_gclk),
	.D(n_98),
	.Q(adrd_bin_2),
	.SE(FE_OFN4955_n_301128),
	.SI(FE_PHN56782_SPCASCAN_N22694));
   INR3OPTPAD2BWP240H11P57PDSVT g1342 (.A1(wen),
	.B1(wful),
	.B2(n_107754),
	.ZN(n_35));
   NR2D1BWP240H11P57PDSVT g1344 (.A1(adrd_bin_0),
	.A2(FE_OFN1025_reset_pps),
	.ZN(n_32));
   INR2D1BWP240H11P57PDSVT g1345 (.A1(adrd_bnext[1]),
	.B1(FE_OFN1025_reset_pps),
	.ZN(n_31));
   INVD2BWP240H11P57PDSVT g1346 (.I(adwr_bin_1),
	.ZN(n_30));
   AOI21D1BWP240H11P57PDSVT g1427 (.A1(n_14),
	.A2(n_18),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_28));
   AOI21D1BWP240H11P57PDSVT g1428 (.A1(n_15),
	.A2(n_21),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_27));
   AOI21D1BWP240H11P57PDSVT g1429 (.A1(n_13),
	.A2(n_19),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_26));
   AOI21D1BWP240H11P57PDSVT g1430 (.A1(n_12),
	.A2(n_20),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_25));
   AOI21D1BWP240H11P57PDSVT g1431 (.A1(n_11),
	.A2(n_17),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_24));
   AOI21D1BWP240H11P57PDSVT g1432 (.A1(n_10),
	.A2(n_16),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_23));
   SDFQD1BWP240H11P57PDSVT adrd_gray_reg_1_ (.CP(LPCKG_tm_wac_common__rc_gclk),
	.D(n_22),
	.Q(DFT_sdo_14),
	.SE(FE_OFN4955_n_301128),
	.SI(FE_PHN76605_SPCASCAN_N22691));
   NR2D1BWP240H11P57PDSVT g1434 (.A1(n_8),
	.A2(FE_OFN1025_reset_pps),
	.ZN(n_22));
   SDFQD1BWP240H11P57PDSVT adwr_gray_reg_0_ (.CP(wclk),
	.D(FE_PHN123509_n_7),
	.Q(FE_PHN156264_SPCHSCAN_N20015),
	.SE(FE_OFN40580_n),
	.SI(FE_PHN150344_SPCASCAN_N15475));
   SDFQD1BWP240H11P57PDSVT adwr_gray_reg_1_ (.CP(wclk),
	.D(n_9),
	.Q(adwr_gray_1),
	.SE(FE_OFN40580_n),
	.SI(adwr_gray_2));
   AOI22D1BWP240H11P57PDSVT g1437 (.A1(n_6),
	.A2(mem_0__0),
	.B1(n_2),
	.B2(mem_1__0),
	.ZN(n_21));
   AOI22D1BWP240H11P57PDSVT g1438 (.A1(n_6),
	.A2(mem_0__3),
	.B1(n_2),
	.B2(mem_1__3),
	.ZN(n_20));
   AOI22D1BWP240H11P57PDSVT g1439 (.A1(n_6),
	.A2(mem_0__2),
	.B1(n_2),
	.B2(mem_1__2),
	.ZN(n_19));
   AOI22D1BWP240H11P57PDSVT g1440 (.A1(n_6),
	.A2(DFT_sdo_4),
	.B1(n_2),
	.B2(mem_1__1),
	.ZN(n_18));
   AOI22D1BWP240H11P57PDSVT g1441 (.A1(n_6),
	.A2(mem_0__4),
	.B1(n_2),
	.B2(FE_PHN54391_SPCASCAN_N16801),
	.ZN(n_17));
   AOI22D1BWP240H11P57PDSVT g1442 (.A1(n_6),
	.A2(mem_0__5),
	.B1(n_2),
	.B2(mem_1__5),
	.ZN(n_16));
   AOI22D1BWP240H11P57PDSVT g1443 (.A1(n_1),
	.A2(mem_3__0),
	.B1(n_3),
	.B2(mem_2__0),
	.ZN(n_15));
   SDFQD1BWP240H11P57PDSVT adrd_gray_reg_0_ (.CP(LPCKG_tm_wac_common__rc_gclk),
	.D(n_5),
	.Q(adrd_gray_0),
	.SE(FE_OFN4955_n_301128),
	.SI(pre_fifo_drop_rdata_2__3__0));
   SDFQD1BWP240H11P57PDSVT adrd_gray_reg_2_ (.CP(LPCKG_tm_wac_common__rc_gclk),
	.D(n_98),
	.Q(FE_PHN131170_FE_PSRN_1),
	.SE(FE_OFN4955_n_301128),
	.SI(FE_PHN66920_n_134880));
   AOI22D1BWP240H11P57PDSVT g1446 (.A1(n_1),
	.A2(mem_3__1),
	.B1(n_3),
	.B2(mem_2__1),
	.ZN(n_14));
   AOI22D1BWP240H11P57PDSVT g1447 (.A1(n_1),
	.A2(mem_3__2),
	.B1(n_3),
	.B2(mem_2__2),
	.ZN(n_13));
   AOI22D1BWP240H11P57PDSVT g1448 (.A1(n_1),
	.A2(mem_3__3),
	.B1(n_3),
	.B2(mem_2__3),
	.ZN(n_12));
   AOI22D1BWP240H11P57PDSVT g1449 (.A1(n_1),
	.A2(DFT_sdo_8),
	.B1(n_3),
	.B2(mem_2__4),
	.ZN(n_11));
   AOI22D1BWP240H11P57PDSVT g1450 (.A1(n_1),
	.A2(FE_PHN46389_SPCASCAN_N15480),
	.B1(n_3),
	.B2(mem_2__5),
	.ZN(n_10));
   XOR2D1BWP240H11P57PDSVT g1451 (.A1(FE_PHN109622_SPCBSCAN_N27982),
	.A2(adwr_bin_1),
	.Z(FE_PHN123407_n_9));
   XNR2D1BWP240H11P57PDSVT g1452 (.A1(adrd_bnext[2]),
	.A2(adrd_bnext[1]),
	.ZN(n_8));
   XOR2D1BWP240H11P57PDSVT g1453 (.A1(adwr_bin_0),
	.A2(adwr_bin_1),
	.Z(n_7));
   SDFQD1BWP240H11P57PDLVT adwr_gray_reg_2_ (.CP(wclk),
	.D(FE_PHN174566_SPCBSCAN_N27982),
	.Q(FE_PHN154742_SPCHSCAN_N20014),
	.SE(FE_OFN4952_n_301128),
	.SI(pre_fifo_drop_cnt_1__3__3_1));
   NR2D1BWP240H11P57PDLVT g1455 (.A1(adrd_bin_1),
	.A2(adrd_bin_0),
	.ZN(n_6));
   NR2D1BWP240H11P57PDSVT g1456 (.A1(adrd_bin_1),
	.A2(FE_OFN1025_reset_pps),
	.ZN(n_5));
   INVD2BWP240H11P57PDSVT g1458 (.I(n_67),
	.ZN(n_3));
   INVD2BWP240H11P57PDSVT g1459 (.I(n_68),
	.ZN(n_2));
   INVD2BWP240H11P57PDSVT g1460 (.I(n_69),
	.ZN(n_1));
   XOR2D2BWP240H11P57PDSVT g1476 (.A1(adrd_wclk_2),
	.A2(DFT_sdo),
	.Z(n_57));
   NR2D1BWP240H11P57PDSVT g2 (.A1(n_97),
	.A2(n_107754),
	.ZN(n_96));
   AOI21D1BWP240H11P57PDSVT g1477 (.A1(n_40),
	.A2(adwr_bin_1),
	.B(n_42),
	.ZN(n_97));
   INR2D1BWP240H11P57PDSVT g1478 (.A1(adrd_bnext[2]),
	.B1(FE_OFN1025_reset_pps),
	.ZN(n_98));
   IAOI21D1BWP240H11P57PDSVT g1479 (.A1(n_65),
	.A2(n_66),
	.B(FE_OFN1025_reset_pps),
	.ZN(n_99));
endmodule
