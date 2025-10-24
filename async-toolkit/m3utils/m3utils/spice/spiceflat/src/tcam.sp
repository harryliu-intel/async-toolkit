* ##############################################################################
* ## Intel Top Secret
* ##############################################################################
* ## Copyright (C) 2011; Intel Corporation All rights reserved.
* ##
* ## This is the property of Intel Corporation and may only be utilized
* ## pursuant to a written Restricted Use Nondisclosure Agreement
* ## with Intel Corporation. It may not be used; reproduced; or
* ## disclosed to others except in accordance with the terms and
* ## conditions of such agreement.
* ##
* ## All products; processes; computer systems; dates; and figures
* ## specified are preliminary based on current expectations; and are
* ## subject to change without notice.
* ##############################################################################

* ##############################################################################
* ##	FDK VERSION				REPLACED_WITH_PDK
* ##	FDK STDCELL VERSION				stdcell736_c.0.0p3
* ##	FDK TIC VERSION				tic736_r1.4hf1
* ##	IDSLIBS VERSION				1273.6_1.6
* ##############################################################################

 

 
************************************************************************
* auCdl Netlist:
* 
* Library Name:  ip736hs3p111dtcam_f736_prd
* Top Cell Name: ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl
* View Name:     schematic
* Netlisted on:  Sep 22 13:55:17 2014
************************************************************************

*.BIPOLAR
*.CAPVAL
*.DIOPERI
*.DIOAREA
*.EQUATION
*.SCALE METER
*.MEGA


*.EXPAND_ON_M_FACTOR

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_out_read_latch_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000 GND L.0 L.1 R Vdd
*.PININFO GND:I L.0:I L.1:I R:I Vdd:I
MR.0 R X.0 GND GND nuv1 W=2.1E-07 L=2.8E-08 m=1
MX.0.2 X.0 L.1 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.3 X.0 X.1 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.2 X.1 L.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.3 X.1 X.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.1 X.0 X.1 X.0.1# Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MR.1 R X.0 Vdd Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
MX.0.0 X.0.1# L.1 Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MX.1.0 X.1.1# L.0 Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.1 X.1 X.0 X.1.1# Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_unstab_or2_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_unstab_or2_4000 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[0] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[1] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_dff_cmd_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_cmd_4000 CLK GND L R.0 R.1 Vdd
*.PININFO GND:I L:I R.0:I R.1:I Vdd:I CLK:O
Mchain_0.0 chain_0.9# R.0 _R.0 GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_0.1 GND CLK chain_0.9# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# _L _R.0 GND nuv1 W=5.46E-07 L=2.8E-08 m=1
Mchain_1.1 N0 _R.1 chain_1.9# GND nuv1 W=5.46E-07 L=2.8E-08 m=1
Mchain_2.0 GND CLK N0 GND nuv1 W=5.46E-07 L=2.8E-08 m=1
Mchain_7.0 chain_7.9# R.1 _R.1 GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_7.1 GND CLK chain_7.9# GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_8.0 chain_8.9# L _R.1 GND nuv1 W=5.46E-07 L=2.8E-08 m=1
Mchain_8.1 N0 _R.0 chain_8.9# GND nuv1 W=5.46E-07 L=2.8E-08 m=1
M_L.0 _L L GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_3.0 N1 R.0 _R.0 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_4.0 Vdd _L N1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_5.0 Vdd _R.1 N1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_6.0 Vdd CLK _R.0 Vdd p W=1.26E-07 L=2.8E-08 m=1
Mchain_9.0 N2 R.1 _R.1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_10.0 Vdd L N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_11.0 Vdd _R.0 N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_12.0 Vdd CLK _R.1 Vdd p W=4.2E-07 L=2.8E-08 m=1
M_L.1 _L L Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MR.0.1 R.0 _R.0 Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MR.1.1 R.1 _R.1 Vdd Vdd puv1 W=7.14E-07 L=2.8E-08 m=1
MR.0.0 R.0 _R.0 GND GND n W=1.26E-07 L=2.8E-08 m=1
MR.1.0 R.1 _R.1 GND GND n W=3.78E-07 L=2.8E-08 m=1
Mgnac.0 GND GND L GND n W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_reset_sr_latch_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_reset_sr_latch_4000 GND L.0 L.1 R Vdd _RESET
*.PININFO GND:I L.0:I L.1:I R:I Vdd:I _RESET:I
MR.0 R X.0 GND GND nuv1 W=1.26E-07 L=2.8E-08 m=1
MReset.0 Reset _RESET GND GND nuv1 W=1.26E-07 L=2.8E-08 m=1
MX.0.2 X.0 L.1 GND GND nuv1 W=1.68E-07 L=2.8E-08 m=1
MX.0.3 X.0 X.1 GND GND nuv1 W=1.68E-07 L=2.8E-08 m=1
MX.1.3 X.1 Reset GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.4 X.1 L.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.5 X.1 X.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.1 X.0 X.1 X.0.1# Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MR.1 R X.0 Vdd Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
MReset.1 Reset _RESET Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MX.0.0 X.0.1# L.1 Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MX.1.0 X.1.1# Reset Vdd Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
MX.1.1 X.1.2# L.0 X.1.1# Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
MX.1.2 X.1 X.0 X.1.2# Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_or2_4001
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_or2_4001 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# Vdd p W=1.68E-07 L=2.8E-08 m=1
Mb.0 b.1# a[0] Vdd Vdd p W=1.68E-07 L=2.8E-08 m=1
Mb.2 b a[0] GND GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mb.3 b a[1] GND GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mx.0 x b GND GND n W=1.008E-06 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=1.008E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_invinv_4004
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4004 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv1 W=7.56E-07 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd p W=4.2E-07 L=2.8E-08 m=1
Mx.0 x b GND GND n W=1.596E-06 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=2.688E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_invinv_4040
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4040 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv1 W=4.62E-07 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd p W=4.62E-07 L=2.8E-08 m=1
Mx.0 x b GND GND n W=1.428E-06 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=1.68E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_and2_4002
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4002 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# GND n W=4.62E-07 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=4.62E-07 L=2.8E-08 m=1
Mb.2 b a[0] Vdd Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
Mb.3 b a[1] Vdd Vdd puv1 W=2.1E-07 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=1.428E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_dff_addr_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_4000 CLK GND L R.0 R.1 Vdd
*.PININFO GND:I L:I R.0:I R.1:I Vdd:I CLK:O
Mchain_0.0 chain_0.9# R.0 _R.0 GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_0.1 GND CLK chain_0.9# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# _L _R.0 GND nuv1 W=3.36E-07 L=2.8E-08 m=1
Mchain_1.1 N0 _R.1 chain_1.9# GND nuv1 W=3.36E-07 L=2.8E-08 m=1
Mchain_2.0 GND CLK N0 GND nuv1 W=3.36E-07 L=2.8E-08 m=1
Mchain_7.0 chain_7.9# R.1 _R.1 GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_7.1 GND CLK chain_7.9# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_8.0 chain_8.9# L _R.1 GND nuv1 W=3.36E-07 L=2.8E-08 m=1
Mchain_8.1 N0 _R.0 chain_8.9# GND nuv1 W=3.36E-07 L=2.8E-08 m=1
M_L.0 _L L GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_3.0 N1 R.0 _R.0 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_4.0 Vdd _L N1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_5.0 Vdd _R.1 N1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_6.0 Vdd CLK _R.0 Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_9.0 N2 R.1 _R.1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_10.0 Vdd L N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_11.0 Vdd _R.0 N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_12.0 Vdd CLK _R.1 Vdd p W=2.1E-07 L=2.8E-08 m=1
M_L.1 _L L Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MR.0.1 R.0 _R.0 Vdd Vdd puv1 W=4.2E-07 L=2.8E-08 m=1
MR.1.1 R.1 _R.1 Vdd Vdd puv1 W=4.2E-07 L=2.8E-08 m=1
MR.0.0 R.0 _R.0 GND GND n W=2.1E-07 L=2.8E-08 m=1
MR.1.0 R.1 _R.1 GND GND n W=2.1E-07 L=2.8E-08 m=1
Mgnac.0 GND GND L GND n W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_dff_addr_2_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_2_4000 A.0 A.1 A.2 A.3 ADDR[0] ADDR[1] GND RW Vdd
*.PININFO A.0:I A.1:I A.2:I A.3:I ADDR[1]:I GND:I RW:I Vdd:I ADDR[0]:O
Xand[0] GND Vdd a[0].0 a[1].0 A.0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4002
Xand[1] GND Vdd a[0].1 a[1].0 A.1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4002
Xand[2] GND Vdd a[0].0 a[1].1 A.2 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4002
Xand[3] GND Vdd a[0].1 a[1].1 A.3 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4002
Xbit[0] RW GND ADDR[0] a[0].0 a[0].1 Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_4000
Xbit[1] RW GND ADDR[1] a[1].0 a[1].1 Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_gnac_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_gnac_4000 GND X
*.PININFO GND:I X:I
Mgnac.0 GND GND X GND n W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_invinv_4020
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4020 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv1 W=2.52E-07 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd p W=1.68E-07 L=2.8E-08 m=1
Mx.0 x b GND GND n W=5.46E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=8.82E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tcam_ctrl_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_ctrl_4000 ADDR2[0].0 ADDR2[0].1 ADDR2[0].2 ADDR2[0].3 
+ ADDR2[1].0 ADDR2[1].1 ADDR2[1].2 ADDR2[1].3 ADDR2[2].0 ADDR2[2].1 ADDR2[2].2 
+ ADDR2[2].3 ADDR2[3].0 ADDR2[3].1 ADDR2[3].2 ADDR2[3].3 ADDR2[4].0 ADDR2[4].1 
+ ADDR2[4].2 ADDR2[4].3 ADDR[0] ADDR[1] ADDR[2] ADDR[3] ADDR[4] ADDR[5] 
+ ADDR[6] ADDR[7] ADDR[8] ADDR[9] AR AW CLK GND KEN LK[0] LK[1] LW[0] LW[1] 
+ REN RESET_N Vdd WEN _RESET
*.PININFO ADDR2[0].0:I ADDR2[0].1:I ADDR2[0].2:I ADDR2[0].3:I ADDR2[1].0:I 
*.PININFO ADDR2[1].1:I ADDR2[1].2:I ADDR2[1].3:I ADDR2[2].0:I ADDR2[2].1:I 
*.PININFO ADDR2[2].2:I ADDR2[2].3:I ADDR2[3].0:I ADDR2[3].1:I ADDR2[3].2:I 
*.PININFO ADDR2[3].3:I ADDR2[4].0:I ADDR2[4].1:I ADDR2[4].2:I ADDR2[4].3:I 
*.PININFO ADDR[1]:I ADDR[2]:I ADDR[3]:I ADDR[4]:I ADDR[5]:I ADDR[6]:I 
*.PININFO ADDR[7]:I ADDR[8]:I ADDR[9]:I AR:I AW:I CLK:I GND:I KEN:I LK[0]:I 
*.PININFO LK[1]:I LW[0]:I LW[1]:I REN:I RESET_N:I Vdd:I WEN:I _RESET:I 
*.PININFO ADDR[0]:O
Xor_lw GND Vdd WEN KEN lw0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_unstab_or2_4000
Xdff_ren CLK GND REN ren.0 ren.1 Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_cmd_4000
Xdff_wen CLK GND WEN wen.0 wen.1 Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_cmd_4000
Xdff_ken CLK GND KEN ken.0 ken.1 Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_cmd_4000
Xdff_lw CLK GND lw0 lw.0 lw.1 Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_cmd_4000
Xlatch_ar GND ren.0 ren.1 xar Vdd _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_reset_sr_latch_4000
Xlatch_aw GND wen.0 wen.1 xaw Vdd _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_reset_sr_latch_4000
Xor_rw GND Vdd xar xaw rw / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_or2_4001
Xramp_LW[0] GND Vdd lw.1 LW[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4004
Xramp_LW[1] GND Vdd lw.1 LW[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4004
Xramp_LK[0] GND Vdd ken.1 LK[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4040
Xramp_LK[1] GND Vdd ken.1 LK[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4040
Xdff_a[0] ADDR2[0].0 ADDR2[0].1 ADDR2[0].2 ADDR2[0].3 ADDR[0] ADDR[1] GND rw 
+ Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_2_4000
Xdff_a[1] ADDR2[1].0 ADDR2[1].1 ADDR2[1].2 ADDR2[1].3 ADDR[2] ADDR[3] GND rw 
+ Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_2_4000
Xdff_a[2] ADDR2[2].0 ADDR2[2].1 ADDR2[2].2 ADDR2[2].3 ADDR[4] ADDR[5] GND rw 
+ Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_2_4000
Xdff_a[3] ADDR2[3].0 ADDR2[3].1 ADDR2[3].2 ADDR2[3].3 ADDR[6] ADDR[7] GND rw 
+ Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_2_4000
Xdff_a[4] ADDR2[4].0 ADDR2[4].1 ADDR2[4].2 ADDR2[4].3 ADDR[8] ADDR[9] GND rw 
+ Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_addr_2_4000
Xgnac_RESET_N GND RESET_N / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_gnac_4000
Xgnac_CLK GND CLK / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_gnac_4000
Xreset GND Vdd RESET_N _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4020
Xramp_AR GND Vdd xar AR / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4020
Xramp_AW GND Vdd xaw AW / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4020
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_dff_data_core_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_core_4000 D GND LW M Vdd W.0 W.1
*.PININFO GND:I LW:I M:I Vdd:I W.0:I W.1:I D:O
Mchain_0.0 chain_0.9# W.0 _W.0 GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_0.1 GND LW chain_0.9# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# _D _W.0 GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_1.1 N0 _W.1 chain_1.9# GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_2.0 chain_2.9# M N0 GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_2.1 N1 _W.2 chain_2.9# GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_3.0 GND LW N1 GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_10.0 chain_10.9# W.1 _W.1 GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_10.1 GND LW chain_10.9# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_11.0 chain_11.9# D _W.1 GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_11.1 N0 _W.0 chain_11.9# GND nuv1 W=5.04E-07 L=2.8E-08 m=1
Mchain_18.0 chain_18.9# _W.2_inverse _W.2 GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mchain_18.1 GND LW chain_18.9# GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mchain_19.0 chain_19.9# _M _W.2 GND nuv1 W=4.2E-07 L=2.8E-08 m=1
Mchain_19.1 chain_19.1# _W.1 chain_19.9# GND nuv1 W=4.2E-07 L=2.8E-08 m=1
Mchain_19.2 N1 _W.0 chain_19.1# GND nuv1 W=4.2E-07 L=2.8E-08 m=1
M_D.0 _D D GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_M.0 _M M GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_W.2_inverse.0 _W.2_inverse _W.2 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_4.0 N2 W.0 _W.0 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_5.0 Vdd M N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_6.0 Vdd _D N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_7.0 Vdd _W.1 N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_8.0 Vdd _W.2 N2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_9.0 Vdd LW _W.0 Vdd p W=2.52E-07 L=2.8E-08 m=1
Mchain_12.0 N3 W.1 _W.1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_13.0 Vdd D N3 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_14.0 Vdd M N3 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_15.0 Vdd _W.0 N3 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_16.0 Vdd _W.2 N3 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_17.0 Vdd LW _W.1 Vdd p W=2.52E-07 L=2.8E-08 m=1
Mchain_20.0 N4 _W.2_inverse _W.2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_21.0 Vdd _M N4 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_22.0 Vdd _W.0 N4 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_23.0 Vdd _W.1 N4 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_24.0 Vdd LW _W.2 Vdd p W=8.4E-08 L=2.8E-08 m=1
M_D.1 _D D Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
M_M.1 _M M Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MW.0.1 W.0 _W.0 Vdd Vdd puv1 W=4.62E-07 L=2.8E-08 m=1
MW.1.1 W.1 _W.1 Vdd Vdd puv1 W=4.62E-07 L=2.8E-08 m=1
M_W.2_inverse.1 _W.2_inverse _W.2 Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MW.0.0 W.0 _W.0 GND GND n W=2.1E-07 L=2.8E-08 m=1
MW.1.0 W.1 _W.1 GND GND n W=2.1E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_invinv_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4000 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv1 W=4.2E-07 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd p W=2.1E-07 L=2.8E-08 m=1
Mx.0 x b GND GND n W=5.88E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=1.302E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_dff_data_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000 D GND LW M Vdd W[0].0 W[0].1 W[1].0 W[1].1
*.PININFO GND:I LW:I M:I Vdd:I W[0].0:I W[0].1:I W[1].0:I W[1].1:I D:O
Xgnac_D GND D / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_gnac_4000
Xgnac_M GND M / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_gnac_4000
Xcore D GND LW M Vdd rampW[0][0].a rampW[0][1].a / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_core_4000
XrampW[0][0] GND Vdd rampW[0][0].a W[0].0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4000
XrampW[0][1] GND Vdd rampW[0][1].a W[0].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4000
XrampW[1][0] GND Vdd rampW[0][0].a W[1].0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4000
XrampW[1][1] GND Vdd rampW[0][1].a W[1].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tcam_inputs_40_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_inputs_40_4000 ADDR2[0].0 ADDR2[0].1 ADDR2[0].2 ADDR2[0].3 
+ ADDR2[1].0 ADDR2[1].1 ADDR2[1].2 ADDR2[1].3 ADDR2[2].0 ADDR2[2].1 ADDR2[2].2 
+ ADDR2[2].3 ADDR2[3].0 ADDR2[3].1 ADDR2[3].2 ADDR2[3].3 ADDR2[4].0 ADDR2[4].1 
+ ADDR2[4].2 ADDR2[4].3 ADDR[0] ADDR[1] ADDR[2] ADDR[3] ADDR[4] ADDR[5] 
+ ADDR[6] ADDR[7] ADDR[8] ADDR[9] AR AW CLK DATA[0] DATA[1] DATA[2] DATA[3] 
+ DATA[4] DATA[5] DATA[6] DATA[7] DATA[8] DATA[9] DATA[10] DATA[11] DATA[12] 
+ DATA[13] DATA[14] DATA[15] DATA[16] DATA[17] DATA[18] DATA[19] DATA[20] 
+ DATA[21] DATA[22] DATA[23] DATA[24] DATA[25] DATA[26] DATA[27] DATA[28] 
+ DATA[29] DATA[30] DATA[31] DATA[32] DATA[33] DATA[34] DATA[35] DATA[36] 
+ DATA[37] DATA[38] DATA[39] GND KEN LK[0] LK[1] MASK[0] MASK[1] MASK[2] 
+ MASK[3] MASK[4] MASK[5] MASK[6] MASK[7] MASK[8] MASK[9] MASK[10] MASK[11] 
+ MASK[12] MASK[13] MASK[14] MASK[15] MASK[16] MASK[17] MASK[18] MASK[19] 
+ MASK[20] MASK[21] MASK[22] MASK[23] MASK[24] MASK[25] MASK[26] MASK[27] 
+ MASK[28] MASK[29] MASK[30] MASK[31] MASK[32] MASK[33] MASK[34] MASK[35] 
+ MASK[36] MASK[37] MASK[38] MASK[39] REN RESET_N Vdd WEN W[0][0].0 W[0][0].1 
+ W[0][1].0 W[0][1].1 W[0][2].0 W[0][2].1 W[0][3].0 W[0][3].1 W[0][4].0 
+ W[0][4].1 W[0][5].0 W[0][5].1 W[0][6].0 W[0][6].1 W[0][7].0 W[0][7].1 
+ W[0][8].0 W[0][8].1 W[0][9].0 W[0][9].1 W[0][10].0 W[0][10].1 W[0][11].0 
+ W[0][11].1 W[0][12].0 W[0][12].1 W[0][13].0 W[0][13].1 W[0][14].0 W[0][14].1 
+ W[0][15].0 W[0][15].1 W[0][16].0 W[0][16].1 W[0][17].0 W[0][17].1 W[0][18].0 
+ W[0][18].1 W[0][19].0 W[0][19].1 W[0][20].0 W[0][20].1 W[0][21].0 W[0][21].1 
+ W[0][22].0 W[0][22].1 W[0][23].0 W[0][23].1 W[0][24].0 W[0][24].1 W[0][25].0 
+ W[0][25].1 W[0][26].0 W[0][26].1 W[0][27].0 W[0][27].1 W[0][28].0 W[0][28].1 
+ W[0][29].0 W[0][29].1 W[0][30].0 W[0][30].1 W[0][31].0 W[0][31].1 W[0][32].0 
+ W[0][32].1 W[0][33].0 W[0][33].1 W[0][34].0 W[0][34].1 W[0][35].0 W[0][35].1 
+ W[0][36].0 W[0][36].1 W[0][37].0 W[0][37].1 W[0][38].0 W[0][38].1 W[0][39].0 
+ W[0][39].1 W[1][0].0 W[1][0].1 W[1][1].0 W[1][1].1 W[1][2].0 W[1][2].1 
+ W[1][3].0 W[1][3].1 W[1][4].0 W[1][4].1 W[1][5].0 W[1][5].1 W[1][6].0 
+ W[1][6].1 W[1][7].0 W[1][7].1 W[1][8].0 W[1][8].1 W[1][9].0 W[1][9].1 
+ W[1][10].0 W[1][10].1 W[1][11].0 W[1][11].1 W[1][12].0 W[1][12].1 W[1][13].0 
+ W[1][13].1 W[1][14].0 W[1][14].1 W[1][15].0 W[1][15].1 W[1][16].0 W[1][16].1 
+ W[1][17].0 W[1][17].1 W[1][18].0 W[1][18].1 W[1][19].0 W[1][19].1 W[1][20].0 
+ W[1][20].1 W[1][21].0 W[1][21].1 W[1][22].0 W[1][22].1 W[1][23].0 W[1][23].1 
+ W[1][24].0 W[1][24].1 W[1][25].0 W[1][25].1 W[1][26].0 W[1][26].1 W[1][27].0 
+ W[1][27].1 W[1][28].0 W[1][28].1 W[1][29].0 W[1][29].1 W[1][30].0 W[1][30].1 
+ W[1][31].0 W[1][31].1 W[1][32].0 W[1][32].1 W[1][33].0 W[1][33].1 W[1][34].0 
+ W[1][34].1 W[1][35].0 W[1][35].1 W[1][36].0 W[1][36].1 W[1][37].0 W[1][37].1 
+ W[1][38].0 W[1][38].1 W[1][39].0 W[1][39].1 _RESET
*.PININFO ADDR2[0].0:I ADDR2[0].1:I ADDR2[0].2:I ADDR2[0].3:I ADDR2[1].0:I 
*.PININFO ADDR2[1].1:I ADDR2[1].2:I ADDR2[1].3:I ADDR2[2].0:I ADDR2[2].1:I 
*.PININFO ADDR2[2].2:I ADDR2[2].3:I ADDR2[3].0:I ADDR2[3].1:I ADDR2[3].2:I 
*.PININFO ADDR2[3].3:I ADDR2[4].0:I ADDR2[4].1:I ADDR2[4].2:I ADDR2[4].3:I 
*.PININFO ADDR[1]:I ADDR[2]:I ADDR[3]:I ADDR[4]:I ADDR[5]:I ADDR[6]:I 
*.PININFO ADDR[7]:I ADDR[8]:I ADDR[9]:I AR:I AW:I CLK:I DATA[0]:I DATA[1]:I 
*.PININFO DATA[2]:I DATA[3]:I DATA[4]:I DATA[5]:I DATA[6]:I DATA[7]:I 
*.PININFO DATA[8]:I DATA[9]:I DATA[10]:I DATA[11]:I DATA[12]:I DATA[13]:I 
*.PININFO DATA[14]:I DATA[15]:I DATA[16]:I DATA[17]:I DATA[18]:I DATA[19]:I 
*.PININFO DATA[20]:I DATA[21]:I DATA[22]:I DATA[23]:I DATA[24]:I DATA[25]:I 
*.PININFO DATA[26]:I DATA[27]:I DATA[28]:I DATA[29]:I DATA[30]:I DATA[31]:I 
*.PININFO DATA[32]:I DATA[33]:I DATA[34]:I DATA[35]:I DATA[36]:I DATA[37]:I 
*.PININFO DATA[38]:I DATA[39]:I GND:I KEN:I LK[0]:I LK[1]:I MASK[0]:I 
*.PININFO MASK[1]:I MASK[2]:I MASK[3]:I MASK[4]:I MASK[5]:I MASK[6]:I 
*.PININFO MASK[7]:I MASK[8]:I MASK[9]:I MASK[10]:I MASK[11]:I MASK[12]:I 
*.PININFO MASK[13]:I MASK[14]:I MASK[15]:I MASK[16]:I MASK[17]:I MASK[18]:I 
*.PININFO MASK[19]:I MASK[20]:I MASK[21]:I MASK[22]:I MASK[23]:I MASK[24]:I 
*.PININFO MASK[25]:I MASK[26]:I MASK[27]:I MASK[28]:I MASK[29]:I MASK[30]:I 
*.PININFO MASK[31]:I MASK[32]:I MASK[33]:I MASK[34]:I MASK[35]:I MASK[36]:I 
*.PININFO MASK[37]:I MASK[38]:I MASK[39]:I REN:I RESET_N:I Vdd:I WEN:I 
*.PININFO W[0][0].0:I W[0][0].1:I W[0][1].0:I W[0][1].1:I W[0][2].0:I 
*.PININFO W[0][2].1:I W[0][3].0:I W[0][3].1:I W[0][4].0:I W[0][4].1:I 
*.PININFO W[0][5].0:I W[0][5].1:I W[0][6].0:I W[0][6].1:I W[0][7].0:I 
*.PININFO W[0][7].1:I W[0][8].0:I W[0][8].1:I W[0][9].0:I W[0][9].1:I 
*.PININFO W[0][10].0:I W[0][10].1:I W[0][11].0:I W[0][11].1:I W[0][12].0:I 
*.PININFO W[0][12].1:I W[0][13].0:I W[0][13].1:I W[0][14].0:I W[0][14].1:I 
*.PININFO W[0][15].0:I W[0][15].1:I W[0][16].0:I W[0][16].1:I W[0][17].0:I 
*.PININFO W[0][17].1:I W[0][18].0:I W[0][18].1:I W[0][19].0:I W[0][19].1:I 
*.PININFO W[0][20].0:I W[0][20].1:I W[0][21].0:I W[0][21].1:I W[0][22].0:I 
*.PININFO W[0][22].1:I W[0][23].0:I W[0][23].1:I W[0][24].0:I W[0][24].1:I 
*.PININFO W[0][25].0:I W[0][25].1:I W[0][26].0:I W[0][26].1:I W[0][27].0:I 
*.PININFO W[0][27].1:I W[0][28].0:I W[0][28].1:I W[0][29].0:I W[0][29].1:I 
*.PININFO W[0][30].0:I W[0][30].1:I W[0][31].0:I W[0][31].1:I W[0][32].0:I 
*.PININFO W[0][32].1:I W[0][33].0:I W[0][33].1:I W[0][34].0:I W[0][34].1:I 
*.PININFO W[0][35].0:I W[0][35].1:I W[0][36].0:I W[0][36].1:I W[0][37].0:I 
*.PININFO W[0][37].1:I W[0][38].0:I W[0][38].1:I W[0][39].0:I W[0][39].1:I 
*.PININFO W[1][0].0:I W[1][0].1:I W[1][1].0:I W[1][1].1:I W[1][2].0:I 
*.PININFO W[1][2].1:I W[1][3].0:I W[1][3].1:I W[1][4].0:I W[1][4].1:I 
*.PININFO W[1][5].0:I W[1][5].1:I W[1][6].0:I W[1][6].1:I W[1][7].0:I 
*.PININFO W[1][7].1:I W[1][8].0:I W[1][8].1:I W[1][9].0:I W[1][9].1:I 
*.PININFO W[1][10].0:I W[1][10].1:I W[1][11].0:I W[1][11].1:I W[1][12].0:I 
*.PININFO W[1][12].1:I W[1][13].0:I W[1][13].1:I W[1][14].0:I W[1][14].1:I 
*.PININFO W[1][15].0:I W[1][15].1:I W[1][16].0:I W[1][16].1:I W[1][17].0:I 
*.PININFO W[1][17].1:I W[1][18].0:I W[1][18].1:I W[1][19].0:I W[1][19].1:I 
*.PININFO W[1][20].0:I W[1][20].1:I W[1][21].0:I W[1][21].1:I W[1][22].0:I 
*.PININFO W[1][22].1:I W[1][23].0:I W[1][23].1:I W[1][24].0:I W[1][24].1:I 
*.PININFO W[1][25].0:I W[1][25].1:I W[1][26].0:I W[1][26].1:I W[1][27].0:I 
*.PININFO W[1][27].1:I W[1][28].0:I W[1][28].1:I W[1][29].0:I W[1][29].1:I 
*.PININFO W[1][30].0:I W[1][30].1:I W[1][31].0:I W[1][31].1:I W[1][32].0:I 
*.PININFO W[1][32].1:I W[1][33].0:I W[1][33].1:I W[1][34].0:I W[1][34].1:I 
*.PININFO W[1][35].0:I W[1][35].1:I W[1][36].0:I W[1][36].1:I W[1][37].0:I 
*.PININFO W[1][37].1:I W[1][38].0:I W[1][38].1:I W[1][39].0:I W[1][39].1:I 
*.PININFO _RESET:I ADDR[0]:O
Xctrl ADDR2[0].0 ADDR2[0].1 ADDR2[0].2 ADDR2[0].3 ADDR2[1].0 ADDR2[1].1 
+ ADDR2[1].2 ADDR2[1].3 ADDR2[2].0 ADDR2[2].1 ADDR2[2].2 ADDR2[2].3 ADDR2[3].0 
+ ADDR2[3].1 ADDR2[3].2 ADDR2[3].3 ADDR2[4].0 ADDR2[4].1 ADDR2[4].2 ADDR2[4].3 
+ ADDR[0] ADDR[1] ADDR[2] ADDR[3] ADDR[4] ADDR[5] ADDR[6] ADDR[7] ADDR[8] 
+ ADDR[9] AR AW CLK GND KEN LK[0] LK[1] lw[0] lw[1] REN RESET_N Vdd WEN _RESET 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_ctrl_4000
Xdata[0] DATA[0] GND lw[0] MASK[0] Vdd W[0][0].0 W[0][0].1 W[1][0].0 W[1][0].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[1] DATA[1] GND lw[0] MASK[1] Vdd W[0][1].0 W[0][1].1 W[1][1].0 W[1][1].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[2] DATA[2] GND lw[0] MASK[2] Vdd W[0][2].0 W[0][2].1 W[1][2].0 W[1][2].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[3] DATA[3] GND lw[0] MASK[3] Vdd W[0][3].0 W[0][3].1 W[1][3].0 W[1][3].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[4] DATA[4] GND lw[0] MASK[4] Vdd W[0][4].0 W[0][4].1 W[1][4].0 W[1][4].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[5] DATA[5] GND lw[0] MASK[5] Vdd W[0][5].0 W[0][5].1 W[1][5].0 W[1][5].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[6] DATA[6] GND lw[0] MASK[6] Vdd W[0][6].0 W[0][6].1 W[1][6].0 W[1][6].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[7] DATA[7] GND lw[0] MASK[7] Vdd W[0][7].0 W[0][7].1 W[1][7].0 W[1][7].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[8] DATA[8] GND lw[0] MASK[8] Vdd W[0][8].0 W[0][8].1 W[1][8].0 W[1][8].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[9] DATA[9] GND lw[0] MASK[9] Vdd W[0][9].0 W[0][9].1 W[1][9].0 W[1][9].1 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[10] DATA[10] GND lw[0] MASK[10] Vdd W[0][10].0 W[0][10].1 W[1][10].0 
+ W[1][10].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[11] DATA[11] GND lw[0] MASK[11] Vdd W[0][11].0 W[0][11].1 W[1][11].0 
+ W[1][11].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[12] DATA[12] GND lw[0] MASK[12] Vdd W[0][12].0 W[0][12].1 W[1][12].0 
+ W[1][12].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[13] DATA[13] GND lw[0] MASK[13] Vdd W[0][13].0 W[0][13].1 W[1][13].0 
+ W[1][13].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[14] DATA[14] GND lw[0] MASK[14] Vdd W[0][14].0 W[0][14].1 W[1][14].0 
+ W[1][14].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[15] DATA[15] GND lw[0] MASK[15] Vdd W[0][15].0 W[0][15].1 W[1][15].0 
+ W[1][15].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[16] DATA[16] GND lw[0] MASK[16] Vdd W[0][16].0 W[0][16].1 W[1][16].0 
+ W[1][16].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[17] DATA[17] GND lw[0] MASK[17] Vdd W[0][17].0 W[0][17].1 W[1][17].0 
+ W[1][17].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[18] DATA[18] GND lw[0] MASK[18] Vdd W[0][18].0 W[0][18].1 W[1][18].0 
+ W[1][18].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[19] DATA[19] GND lw[0] MASK[19] Vdd W[0][19].0 W[0][19].1 W[1][19].0 
+ W[1][19].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[20] DATA[20] GND lw[1] MASK[20] Vdd W[0][20].0 W[0][20].1 W[1][20].0 
+ W[1][20].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[21] DATA[21] GND lw[1] MASK[21] Vdd W[0][21].0 W[0][21].1 W[1][21].0 
+ W[1][21].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[22] DATA[22] GND lw[1] MASK[22] Vdd W[0][22].0 W[0][22].1 W[1][22].0 
+ W[1][22].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[23] DATA[23] GND lw[1] MASK[23] Vdd W[0][23].0 W[0][23].1 W[1][23].0 
+ W[1][23].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[24] DATA[24] GND lw[1] MASK[24] Vdd W[0][24].0 W[0][24].1 W[1][24].0 
+ W[1][24].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[25] DATA[25] GND lw[1] MASK[25] Vdd W[0][25].0 W[0][25].1 W[1][25].0 
+ W[1][25].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[26] DATA[26] GND lw[1] MASK[26] Vdd W[0][26].0 W[0][26].1 W[1][26].0 
+ W[1][26].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[27] DATA[27] GND lw[1] MASK[27] Vdd W[0][27].0 W[0][27].1 W[1][27].0 
+ W[1][27].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[28] DATA[28] GND lw[1] MASK[28] Vdd W[0][28].0 W[0][28].1 W[1][28].0 
+ W[1][28].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[29] DATA[29] GND lw[1] MASK[29] Vdd W[0][29].0 W[0][29].1 W[1][29].0 
+ W[1][29].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[30] DATA[30] GND lw[1] MASK[30] Vdd W[0][30].0 W[0][30].1 W[1][30].0 
+ W[1][30].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[31] DATA[31] GND lw[1] MASK[31] Vdd W[0][31].0 W[0][31].1 W[1][31].0 
+ W[1][31].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[32] DATA[32] GND lw[1] MASK[32] Vdd W[0][32].0 W[0][32].1 W[1][32].0 
+ W[1][32].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[33] DATA[33] GND lw[1] MASK[33] Vdd W[0][33].0 W[0][33].1 W[1][33].0 
+ W[1][33].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[34] DATA[34] GND lw[1] MASK[34] Vdd W[0][34].0 W[0][34].1 W[1][34].0 
+ W[1][34].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[35] DATA[35] GND lw[1] MASK[35] Vdd W[0][35].0 W[0][35].1 W[1][35].0 
+ W[1][35].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[36] DATA[36] GND lw[1] MASK[36] Vdd W[0][36].0 W[0][36].1 W[1][36].0 
+ W[1][36].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[37] DATA[37] GND lw[1] MASK[37] Vdd W[0][37].0 W[0][37].1 W[1][37].0 
+ W[1][37].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[38] DATA[38] GND lw[1] MASK[38] Vdd W[0][38].0 W[0][38].1 W[1][38].0 
+ W[1][38].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
Xdata[39] DATA[39] GND lw[1] MASK[39] Vdd W[0][39].0 W[0][39].1 W[1][39].0 
+ W[1][39].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_dff_data_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_out_hit_and_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_and_4000 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# GND n W=1.68E-07 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=1.68E-07 L=2.8E-08 m=1
Mgnac.0 GND GND a[0] GND n W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[0] Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
Mb.3 b a[1] Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=2.94E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=3.36E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_out_hit_latch_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_latch_4000 GND L.0 L.1 R Vdd
*.PININFO GND:I L.0:I L.1:I R:I Vdd:I
MR.0 R X.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.2 X.0 L.1 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.3 X.0 X.1 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.2 X.1 L.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.3 X.1 X.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.1 X.0 X.1 X.0.1# Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MR.1 R X.0 Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MX.0.0 X.0.1# L.1 Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MX.1.0 X.1.1# L.0 Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MX.1.1 X.1 X.0 X.1.1# Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_out_hit_core_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_core_4000 GND H.0 H.1 LH[0] LH[1] LK Vdd _DLK
*.PININFO GND:I H.0:I H.1:I LH[0]:I LH[1]:I LK:I Vdd:I _DLK:I
Mchain_0.0 Vdd LK LH[0] Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 Vdd LK LH[1] Vdd p W=8.4E-08 L=2.8E-08 m=1
M_lk.1 _lk LK Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_lk2.1 _lk2 lk2 Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_2.0 GND _lk2 H.0 GND n W=1.26E-07 L=2.8E-08 m=1
Mchain_3.0 GND H.1 H.0 GND n W=1.26E-07 L=2.8E-08 m=1
Mchain_8.0 GND _lk2 H.1 GND n W=1.26E-07 L=2.8E-08 m=1
Mchain_9.0 GND H.0 H.1 GND n W=1.26E-07 L=2.8E-08 m=1
Mlk2.0 lk2 _lk GND GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_4.0 N0 LH[0] H.0 Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_5.0 N1 H.1 N0 Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_6.0 Vdd _lk N1 Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_7.0 N0 LH[1] H.0 Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_10.0 chain_10.9# _DLK H.1 Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_10.1 N1 H.0 chain_10.9# Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
Mlk2.1 lk2 _lk Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
M_lk.0 _lk LK GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_lk2.0 _lk2 lk2 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_out_hit_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000 GND LHIT LH[0] LH[1] LK RHIT Vdd _DLK
*.PININFO GND:I LHIT:I LH[0]:I LH[1]:I LK:I RHIT:I Vdd:I _DLK:I
Xand GND Vdd LHIT hit RHIT / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_and_4000
Xlatch GND h.0 h.1 hit Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_latch_4000
Xcore GND h.0 h.1 LH[0] LH[1] LK Vdd _DLK / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_core_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_demux4_core_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_core_4000 A0.0 A0.1 A0.2 A0.3 A1 A2 GND Vdd _a.0 _a.1 _a.2 
+ _a.3
*.PININFO A0.0:I A0.1:I A0.2:I A0.3:I A2:I GND:I Vdd:I _a.0:I _a.1:I _a.2:I 
*.PININFO _a.3:I A1:O
Mchain_0.0 N0 A0.0 _a.0 GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# A2 N0 GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_1.1 GND A1 chain_1.9# GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_5.0 N0 A0.1 _a.1 GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_9.0 N0 A0.2 _a.2 GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_13.0 N0 A0.3 _a.3 GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_2.0 Vdd A1 _a.0 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_3.0 Vdd A2 _a.0 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_4.0 Vdd A0.0 _a.0 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_6.0 Vdd A1 _a.1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_7.0 Vdd A2 _a.1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_8.0 Vdd A0.1 _a.1 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_10.0 Vdd A1 _a.2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_11.0 Vdd A2 _a.2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_12.0 Vdd A0.2 _a.2 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_14.0 Vdd A1 _a.3 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_15.0 Vdd A2 _a.3 Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_16.0 Vdd A0.3 _a.3 Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_demux4_inv_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_inv_4000 A GND Vdd WL_Vcc _A
*.PININFO GND:I Vdd:I WL_Vcc:I _A:I A:O
Mchain_0.0 GND _A A GND n W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 WL_Vcc _A A Vdd puv1 W=1.68E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_demux4_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000 A0.0 A0.1 A0.2 A0.3 A1 A2 GND Vdd WL_Vcc a[0] a[1] 
+ a[2] a[3]
*.PININFO A0.0:I A0.1:I A0.2:I A0.3:I A2:I GND:I Vdd:I WL_Vcc:I a[0]:I a[1]:I 
*.PININFO a[2]:I a[3]:I A1:O
Xcore A0.0 A0.1 A0.2 A0.3 A1 A2 GND Vdd _a.0 _a.1 _a.2 _a.3 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_core_4000
Xinv[0] a[0] GND Vdd WL_Vcc _a.0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_inv_4000
Xinv[1] a[1] GND Vdd WL_Vcc _a.1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_inv_4000
Xinv[2] a[2] GND Vdd WL_Vcc _a.2 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_inv_4000
Xinv[3] a[3] GND Vdd WL_Vcc _a.3 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_inv_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_demux64_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux64_4000 ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 
+ ADDR[1].1 ADDR[1].2 ADDR[1].3 ADDR[2].0 ADDR[2].1 ADDR[2].2 ADDR[2].3 A[0] 
+ A[1] A[2] A[3] A[4] A[5] A[6] A[7] A[8] A[9] A[10] A[11] A[12] A[13] A[14] 
+ A[15] A[16] A[17] A[18] A[19] A[20] A[21] A[22] A[23] A[24] A[25] A[26] 
+ A[27] A[28] A[29] A[30] A[31] A[32] A[33] A[34] A[35] A[36] A[37] A[38] 
+ A[39] A[40] A[41] A[42] A[43] A[44] A[45] A[46] A[47] A[48] A[49] A[50] 
+ A[51] A[52] A[53] A[54] A[55] A[56] A[57] A[58] A[59] A[60] A[61] A[62] 
+ A[63] GND Vdd WL_Vcc
*.PININFO ADDR[0].0:I ADDR[0].1:I ADDR[0].2:I ADDR[0].3:I ADDR[1].0:I 
*.PININFO ADDR[1].1:I ADDR[1].2:I ADDR[1].3:I ADDR[2].0:I ADDR[2].1:I 
*.PININFO ADDR[2].2:I ADDR[2].3:I A[1]:I A[2]:I A[3]:I A[4]:I A[5]:I A[6]:I 
*.PININFO A[7]:I A[8]:I A[9]:I A[10]:I A[11]:I A[12]:I A[13]:I A[14]:I A[15]:I 
*.PININFO A[16]:I A[17]:I A[18]:I A[19]:I A[20]:I A[21]:I A[22]:I A[23]:I 
*.PININFO A[24]:I A[25]:I A[26]:I A[27]:I A[28]:I A[29]:I A[30]:I A[31]:I 
*.PININFO A[32]:I A[33]:I A[34]:I A[35]:I A[36]:I A[37]:I A[38]:I A[39]:I 
*.PININFO A[40]:I A[41]:I A[42]:I A[43]:I A[44]:I A[45]:I A[46]:I A[47]:I 
*.PININFO A[48]:I A[49]:I A[50]:I A[51]:I A[52]:I A[53]:I A[54]:I A[55]:I 
*.PININFO A[56]:I A[57]:I A[58]:I A[59]:I A[60]:I A[61]:I A[62]:I A[63]:I 
*.PININFO GND:I Vdd:I WL_Vcc:I A[0]:O
Xdemux[0] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 ADDR[2].0 GND Vdd 
+ WL_Vcc A[0] A[1] A[2] A[3] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[1] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].1 ADDR[2].0 GND Vdd 
+ WL_Vcc A[4] A[5] A[6] A[7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[2] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].2 ADDR[2].0 GND Vdd 
+ WL_Vcc A[8] A[9] A[10] A[11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[3] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].3 ADDR[2].0 GND Vdd 
+ WL_Vcc A[12] A[13] A[14] A[15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[4] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 ADDR[2].1 GND Vdd 
+ WL_Vcc A[16] A[17] A[18] A[19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[5] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].1 ADDR[2].1 GND Vdd 
+ WL_Vcc A[20] A[21] A[22] A[23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[6] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].2 ADDR[2].1 GND Vdd 
+ WL_Vcc A[24] A[25] A[26] A[27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[7] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].3 ADDR[2].1 GND Vdd 
+ WL_Vcc A[28] A[29] A[30] A[31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[8] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 ADDR[2].2 GND Vdd 
+ WL_Vcc A[32] A[33] A[34] A[35] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[9] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].1 ADDR[2].2 GND Vdd 
+ WL_Vcc A[36] A[37] A[38] A[39] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[10] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].2 ADDR[2].2 GND Vdd 
+ WL_Vcc A[40] A[41] A[42] A[43] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[11] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].3 ADDR[2].2 GND Vdd 
+ WL_Vcc A[44] A[45] A[46] A[47] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[12] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 ADDR[2].3 GND Vdd 
+ WL_Vcc A[48] A[49] A[50] A[51] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[13] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].1 ADDR[2].3 GND Vdd 
+ WL_Vcc A[52] A[53] A[54] A[55] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[14] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].2 ADDR[2].3 GND Vdd 
+ WL_Vcc A[56] A[57] A[58] A[59] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
Xdemux[15] ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].3 ADDR[2].3 GND Vdd 
+ WL_Vcc A[60] A[61] A[62] A[63] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux4_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_slice_mid32_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice_mid32_4000 ADDR[0][0].0 ADDR[0][0].1 ADDR[0][0].2 
+ ADDR[0][0].3 ADDR[0][1].0 ADDR[0][1].1 ADDR[0][1].2 ADDR[0][1].3 
+ ADDR[0][2].0 ADDR[0][2].1 ADDR[0][2].2 ADDR[0][2].3 ADDR[1][0].0 
+ ADDR[1][0].1 ADDR[1][0].2 ADDR[1][0].3 ADDR[1][1].0 ADDR[1][1].1 
+ ADDR[1][1].2 ADDR[1][1].3 ADDR[1][2].0 ADDR[1][2].1 ADDR[1][2].2 
+ ADDR[1][2].3 A[0][0] A[0][1] A[0][2] A[0][3] A[0][4] A[0][5] A[0][6] A[0][7] 
+ A[0][8] A[0][9] A[0][10] A[0][11] A[0][12] A[0][13] A[0][14] A[0][15] 
+ A[0][16] A[0][17] A[0][18] A[0][19] A[0][20] A[0][21] A[0][22] A[0][23] 
+ A[0][24] A[0][25] A[0][26] A[0][27] A[0][28] A[0][29] A[0][30] A[0][31] 
+ A[0][32] A[0][33] A[0][34] A[0][35] A[0][36] A[0][37] A[0][38] A[0][39] 
+ A[0][40] A[0][41] A[0][42] A[0][43] A[0][44] A[0][45] A[0][46] A[0][47] 
+ A[0][48] A[0][49] A[0][50] A[0][51] A[0][52] A[0][53] A[0][54] A[0][55] 
+ A[0][56] A[0][57] A[0][58] A[0][59] A[0][60] A[0][61] A[0][62] A[0][63] 
+ A[1][0] A[1][1] A[1][2] A[1][3] A[1][4] A[1][5] A[1][6] A[1][7] A[1][8] 
+ A[1][9] A[1][10] A[1][11] A[1][12] A[1][13] A[1][14] A[1][15] A[1][16] 
+ A[1][17] A[1][18] A[1][19] A[1][20] A[1][21] A[1][22] A[1][23] A[1][24] 
+ A[1][25] A[1][26] A[1][27] A[1][28] A[1][29] A[1][30] A[1][31] A[1][32] 
+ A[1][33] A[1][34] A[1][35] A[1][36] A[1][37] A[1][38] A[1][39] A[1][40] 
+ A[1][41] A[1][42] A[1][43] A[1][44] A[1][45] A[1][46] A[1][47] A[1][48] 
+ A[1][49] A[1][50] A[1][51] A[1][52] A[1][53] A[1][54] A[1][55] A[1][56] 
+ A[1][57] A[1][58] A[1][59] A[1][60] A[1][61] A[1][62] A[1][63] GND LHIT[0] 
+ LHIT[1] LHIT[2] LHIT[3] LHIT[4] LHIT[5] LHIT[6] LHIT[7] LHIT[8] LHIT[9] 
+ LHIT[10] LHIT[11] LHIT[12] LHIT[13] LHIT[14] LHIT[15] LHIT[16] LHIT[17] 
+ LHIT[18] LHIT[19] LHIT[20] LHIT[21] LHIT[22] LHIT[23] LHIT[24] LHIT[25] 
+ LHIT[26] LHIT[27] LHIT[28] LHIT[29] LHIT[30] LHIT[31] LH[0][0] LH[0][1] 
+ LH[0][2] LH[0][3] LH[0][4] LH[0][5] LH[0][6] LH[0][7] LH[0][8] LH[0][9] 
+ LH[0][10] LH[0][11] LH[0][12] LH[0][13] LH[0][14] LH[0][15] LH[0][16] 
+ LH[0][17] LH[0][18] LH[0][19] LH[0][20] LH[0][21] LH[0][22] LH[0][23] 
+ LH[0][24] LH[0][25] LH[0][26] LH[0][27] LH[0][28] LH[0][29] LH[0][30] 
+ LH[0][31] LH[1][0] LH[1][1] LH[1][2] LH[1][3] LH[1][4] LH[1][5] LH[1][6] 
+ LH[1][7] LH[1][8] LH[1][9] LH[1][10] LH[1][11] LH[1][12] LH[1][13] LH[1][14] 
+ LH[1][15] LH[1][16] LH[1][17] LH[1][18] LH[1][19] LH[1][20] LH[1][21] 
+ LH[1][22] LH[1][23] LH[1][24] LH[1][25] LH[1][26] LH[1][27] LH[1][28] 
+ LH[1][29] LH[1][30] LH[1][31] LK[0] LK[1] LK[2] LK[3] RHIT[0] RHIT[1] 
+ RHIT[2] RHIT[3] RHIT[4] RHIT[5] RHIT[6] RHIT[7] RHIT[8] RHIT[9] RHIT[10] 
+ RHIT[11] RHIT[12] RHIT[13] RHIT[14] RHIT[15] RHIT[16] RHIT[17] RHIT[18] 
+ RHIT[19] RHIT[20] RHIT[21] RHIT[22] RHIT[23] RHIT[24] RHIT[25] RHIT[26] 
+ RHIT[27] RHIT[28] RHIT[29] RHIT[30] RHIT[31] Vdd WL_Vcc[0] WL_Vcc[1] _DLK[0] 
+ _DLK[1] _DLK[2] _DLK[3]
*.PININFO ADDR[0][0].0:I ADDR[0][0].1:I ADDR[0][0].2:I ADDR[0][0].3:I 
*.PININFO ADDR[0][1].0:I ADDR[0][1].1:I ADDR[0][1].2:I ADDR[0][1].3:I 
*.PININFO ADDR[0][2].0:I ADDR[0][2].1:I ADDR[0][2].2:I ADDR[0][2].3:I 
*.PININFO ADDR[1][0].0:I ADDR[1][0].1:I ADDR[1][0].2:I ADDR[1][0].3:I 
*.PININFO ADDR[1][1].0:I ADDR[1][1].1:I ADDR[1][1].2:I ADDR[1][1].3:I 
*.PININFO ADDR[1][2].0:I ADDR[1][2].1:I ADDR[1][2].2:I ADDR[1][2].3:I 
*.PININFO A[0][1]:I A[0][2]:I A[0][3]:I A[0][4]:I A[0][5]:I A[0][6]:I 
*.PININFO A[0][7]:I A[0][8]:I A[0][9]:I A[0][10]:I A[0][11]:I A[0][12]:I 
*.PININFO A[0][13]:I A[0][14]:I A[0][15]:I A[0][16]:I A[0][17]:I A[0][18]:I 
*.PININFO A[0][19]:I A[0][20]:I A[0][21]:I A[0][22]:I A[0][23]:I A[0][24]:I 
*.PININFO A[0][25]:I A[0][26]:I A[0][27]:I A[0][28]:I A[0][29]:I A[0][30]:I 
*.PININFO A[0][31]:I A[0][32]:I A[0][33]:I A[0][34]:I A[0][35]:I A[0][36]:I 
*.PININFO A[0][37]:I A[0][38]:I A[0][39]:I A[0][40]:I A[0][41]:I A[0][42]:I 
*.PININFO A[0][43]:I A[0][44]:I A[0][45]:I A[0][46]:I A[0][47]:I A[0][48]:I 
*.PININFO A[0][49]:I A[0][50]:I A[0][51]:I A[0][52]:I A[0][53]:I A[0][54]:I 
*.PININFO A[0][55]:I A[0][56]:I A[0][57]:I A[0][58]:I A[0][59]:I A[0][60]:I 
*.PININFO A[0][61]:I A[0][62]:I A[0][63]:I A[1][0]:I A[1][1]:I A[1][2]:I 
*.PININFO A[1][3]:I A[1][4]:I A[1][5]:I A[1][6]:I A[1][7]:I A[1][8]:I 
*.PININFO A[1][9]:I A[1][10]:I A[1][11]:I A[1][12]:I A[1][13]:I A[1][14]:I 
*.PININFO A[1][15]:I A[1][16]:I A[1][17]:I A[1][18]:I A[1][19]:I A[1][20]:I 
*.PININFO A[1][21]:I A[1][22]:I A[1][23]:I A[1][24]:I A[1][25]:I A[1][26]:I 
*.PININFO A[1][27]:I A[1][28]:I A[1][29]:I A[1][30]:I A[1][31]:I A[1][32]:I 
*.PININFO A[1][33]:I A[1][34]:I A[1][35]:I A[1][36]:I A[1][37]:I A[1][38]:I 
*.PININFO A[1][39]:I A[1][40]:I A[1][41]:I A[1][42]:I A[1][43]:I A[1][44]:I 
*.PININFO A[1][45]:I A[1][46]:I A[1][47]:I A[1][48]:I A[1][49]:I A[1][50]:I 
*.PININFO A[1][51]:I A[1][52]:I A[1][53]:I A[1][54]:I A[1][55]:I A[1][56]:I 
*.PININFO A[1][57]:I A[1][58]:I A[1][59]:I A[1][60]:I A[1][61]:I A[1][62]:I 
*.PININFO A[1][63]:I GND:I LHIT[0]:I LHIT[1]:I LHIT[2]:I LHIT[3]:I LHIT[4]:I 
*.PININFO LHIT[5]:I LHIT[6]:I LHIT[7]:I LHIT[8]:I LHIT[9]:I LHIT[10]:I 
*.PININFO LHIT[11]:I LHIT[12]:I LHIT[13]:I LHIT[14]:I LHIT[15]:I LHIT[16]:I 
*.PININFO LHIT[17]:I LHIT[18]:I LHIT[19]:I LHIT[20]:I LHIT[21]:I LHIT[22]:I 
*.PININFO LHIT[23]:I LHIT[24]:I LHIT[25]:I LHIT[26]:I LHIT[27]:I LHIT[28]:I 
*.PININFO LHIT[29]:I LHIT[30]:I LHIT[31]:I LH[0][0]:I LH[0][1]:I LH[0][2]:I 
*.PININFO LH[0][3]:I LH[0][4]:I LH[0][5]:I LH[0][6]:I LH[0][7]:I LH[0][8]:I 
*.PININFO LH[0][9]:I LH[0][10]:I LH[0][11]:I LH[0][12]:I LH[0][13]:I 
*.PININFO LH[0][14]:I LH[0][15]:I LH[0][16]:I LH[0][17]:I LH[0][18]:I 
*.PININFO LH[0][19]:I LH[0][20]:I LH[0][21]:I LH[0][22]:I LH[0][23]:I 
*.PININFO LH[0][24]:I LH[0][25]:I LH[0][26]:I LH[0][27]:I LH[0][28]:I 
*.PININFO LH[0][29]:I LH[0][30]:I LH[0][31]:I LH[1][0]:I LH[1][1]:I LH[1][2]:I 
*.PININFO LH[1][3]:I LH[1][4]:I LH[1][5]:I LH[1][6]:I LH[1][7]:I LH[1][8]:I 
*.PININFO LH[1][9]:I LH[1][10]:I LH[1][11]:I LH[1][12]:I LH[1][13]:I 
*.PININFO LH[1][14]:I LH[1][15]:I LH[1][16]:I LH[1][17]:I LH[1][18]:I 
*.PININFO LH[1][19]:I LH[1][20]:I LH[1][21]:I LH[1][22]:I LH[1][23]:I 
*.PININFO LH[1][24]:I LH[1][25]:I LH[1][26]:I LH[1][27]:I LH[1][28]:I 
*.PININFO LH[1][29]:I LH[1][30]:I LH[1][31]:I LK[0]:I LK[1]:I LK[2]:I LK[3]:I 
*.PININFO RHIT[0]:I RHIT[1]:I RHIT[2]:I RHIT[3]:I RHIT[4]:I RHIT[5]:I 
*.PININFO RHIT[6]:I RHIT[7]:I RHIT[8]:I RHIT[9]:I RHIT[10]:I RHIT[11]:I 
*.PININFO RHIT[12]:I RHIT[13]:I RHIT[14]:I RHIT[15]:I RHIT[16]:I RHIT[17]:I 
*.PININFO RHIT[18]:I RHIT[19]:I RHIT[20]:I RHIT[21]:I RHIT[22]:I RHIT[23]:I 
*.PININFO RHIT[24]:I RHIT[25]:I RHIT[26]:I RHIT[27]:I RHIT[28]:I RHIT[29]:I 
*.PININFO RHIT[30]:I RHIT[31]:I Vdd:I WL_Vcc[0]:I WL_Vcc[1]:I _DLK[0]:I 
*.PININFO _DLK[1]:I _DLK[2]:I _DLK[3]:I A[0][0]:O
Xout[0][0] GND LHIT[0] LH[0][0] LH[1][0] LK[0] RHIT[0] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[0][1] GND LHIT[1] LH[0][1] LH[1][1] LK[1] RHIT[1] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[0][2] GND LHIT[2] LH[0][2] LH[1][2] LK[2] RHIT[2] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[0][3] GND LHIT[3] LH[0][3] LH[1][3] LK[3] RHIT[3] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[1][0] GND LHIT[4] LH[0][4] LH[1][4] LK[0] RHIT[4] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[1][1] GND LHIT[5] LH[0][5] LH[1][5] LK[1] RHIT[5] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[1][2] GND LHIT[6] LH[0][6] LH[1][6] LK[2] RHIT[6] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[1][3] GND LHIT[7] LH[0][7] LH[1][7] LK[3] RHIT[7] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[2][0] GND LHIT[8] LH[0][8] LH[1][8] LK[0] RHIT[8] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[2][1] GND LHIT[9] LH[0][9] LH[1][9] LK[1] RHIT[9] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[2][2] GND LHIT[10] LH[0][10] LH[1][10] LK[2] RHIT[10] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[2][3] GND LHIT[11] LH[0][11] LH[1][11] LK[3] RHIT[11] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[3][0] GND LHIT[12] LH[0][12] LH[1][12] LK[0] RHIT[12] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[3][1] GND LHIT[13] LH[0][13] LH[1][13] LK[1] RHIT[13] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[3][2] GND LHIT[14] LH[0][14] LH[1][14] LK[2] RHIT[14] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[3][3] GND LHIT[15] LH[0][15] LH[1][15] LK[3] RHIT[15] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[4][0] GND LHIT[16] LH[0][16] LH[1][16] LK[0] RHIT[16] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[4][1] GND LHIT[17] LH[0][17] LH[1][17] LK[1] RHIT[17] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[4][2] GND LHIT[18] LH[0][18] LH[1][18] LK[2] RHIT[18] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[4][3] GND LHIT[19] LH[0][19] LH[1][19] LK[3] RHIT[19] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[5][0] GND LHIT[20] LH[0][20] LH[1][20] LK[0] RHIT[20] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[5][1] GND LHIT[21] LH[0][21] LH[1][21] LK[1] RHIT[21] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[5][2] GND LHIT[22] LH[0][22] LH[1][22] LK[2] RHIT[22] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[5][3] GND LHIT[23] LH[0][23] LH[1][23] LK[3] RHIT[23] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[6][0] GND LHIT[24] LH[0][24] LH[1][24] LK[0] RHIT[24] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[6][1] GND LHIT[25] LH[0][25] LH[1][25] LK[1] RHIT[25] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[6][2] GND LHIT[26] LH[0][26] LH[1][26] LK[2] RHIT[26] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[6][3] GND LHIT[27] LH[0][27] LH[1][27] LK[3] RHIT[27] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[7][0] GND LHIT[28] LH[0][28] LH[1][28] LK[0] RHIT[28] Vdd _DLK[0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[7][1] GND LHIT[29] LH[0][29] LH[1][29] LK[1] RHIT[29] Vdd _DLK[1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[7][2] GND LHIT[30] LH[0][30] LH[1][30] LK[2] RHIT[30] Vdd _DLK[2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xout[7][3] GND LHIT[31] LH[0][31] LH[1][31] LK[3] RHIT[31] Vdd _DLK[3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_hit_4000
Xdemux[0] ADDR[0][0].0 ADDR[0][0].1 ADDR[0][0].2 ADDR[0][0].3 ADDR[0][1].0 
+ ADDR[0][1].1 ADDR[0][1].2 ADDR[0][1].3 ADDR[0][2].0 ADDR[0][2].1 
+ ADDR[0][2].2 ADDR[0][2].3 A[0][0] A[0][1] A[0][2] A[0][3] A[0][4] A[0][5] 
+ A[0][6] A[0][7] A[0][8] A[0][9] A[0][10] A[0][11] A[0][12] A[0][13] A[0][14] 
+ A[0][15] A[0][16] A[0][17] A[0][18] A[0][19] A[0][20] A[0][21] A[0][22] 
+ A[0][23] A[0][24] A[0][25] A[0][26] A[0][27] A[0][28] A[0][29] A[0][30] 
+ A[0][31] A[0][32] A[0][33] A[0][34] A[0][35] A[0][36] A[0][37] A[0][38] 
+ A[0][39] A[0][40] A[0][41] A[0][42] A[0][43] A[0][44] A[0][45] A[0][46] 
+ A[0][47] A[0][48] A[0][49] A[0][50] A[0][51] A[0][52] A[0][53] A[0][54] 
+ A[0][55] A[0][56] A[0][57] A[0][58] A[0][59] A[0][60] A[0][61] A[0][62] 
+ A[0][63] GND Vdd WL_Vcc[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux64_4000
Xdemux[1] ADDR[1][0].0 ADDR[1][0].1 ADDR[1][0].2 ADDR[1][0].3 ADDR[1][1].0 
+ ADDR[1][1].1 ADDR[1][1].2 ADDR[1][1].3 ADDR[1][2].0 ADDR[1][2].1 
+ ADDR[1][2].2 ADDR[1][2].3 A[1][0] A[1][1] A[1][2] A[1][3] A[1][4] A[1][5] 
+ A[1][6] A[1][7] A[1][8] A[1][9] A[1][10] A[1][11] A[1][12] A[1][13] A[1][14] 
+ A[1][15] A[1][16] A[1][17] A[1][18] A[1][19] A[1][20] A[1][21] A[1][22] 
+ A[1][23] A[1][24] A[1][25] A[1][26] A[1][27] A[1][28] A[1][29] A[1][30] 
+ A[1][31] A[1][32] A[1][33] A[1][34] A[1][35] A[1][36] A[1][37] A[1][38] 
+ A[1][39] A[1][40] A[1][41] A[1][42] A[1][43] A[1][44] A[1][45] A[1][46] 
+ A[1][47] A[1][48] A[1][49] A[1][50] A[1][51] A[1][52] A[1][53] A[1][54] 
+ A[1][55] A[1][56] A[1][57] A[1][58] A[1][59] A[1][60] A[1][61] A[1][62] 
+ A[1][63] GND Vdd WL_Vcc[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux64_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_lock_clock_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_lock_clock_4000 CLK GCLK GND SLICE_EN Vdd _DCLK _RESET
*.PININFO GCLK:I GND:I SLICE_EN:I Vdd:I _DCLK:I _RESET:I CLK:O
Mchain_0.0 chain_0.9# GCLK _GCLK GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_0.1 chain_0.1# en chain_0.9# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_0.2 GND _DCLK chain_0.1# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# CLK _GCLK GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_1.1 GND en chain_1.9# GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Men.0 en _en GND GND nuv1 W=1.26E-07 L=2.8E-08 m=1
Mchain_2.0 chain_2.9# GCLK _GCLK Vdd p W=4.2E-08 L=2.8E-08 m=1
Mchain_2.1 Vdd CLK chain_2.9# Vdd p W=4.2E-08 L=2.8E-08 m=1
Mchain_3.0 chain_3.9# _DCLK _GCLK Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_3.1 Vdd CLK chain_3.9# Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_4.0 Vdd en _GCLK Vdd p W=8.4E-08 L=2.8E-08 m=1
Men.1 en _en Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_en.1 _en SLICE_EN _en.1# GND n W=8.4E-08 L=2.8E-08 m=1
MGCLK.0 GCLK _GCLK GND GND n W=2.1E-07 L=2.8E-08 m=1
M_en.0 _en.1# _RESET GND GND n W=8.4E-08 L=2.8E-08 m=1
Mgnac.0 GND GND SLICE_EN GND n W=8.4E-08 L=2.8E-08 m=1
MGCLK.1 GCLK _GCLK Vdd Vdd puv1 W=2.52E-07 L=2.8E-08 m=1
M_en.2 _en _RESET Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
M_en.3 _en SLICE_EN Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_and3_4100
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4100 GND Vdd a[0] a[1] a[2] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I a[2]:I x:I
Mb.1 b.2# a[1] b.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[2] b.2# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.4 b a[1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.5 b a[2] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=1.26E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=1.68E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_slow_or2_4100
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_or2_4100 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[0] GND GND nuv2 W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[1] GND GND nuv2 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv2 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_delay_posedge_1_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_1_4000 A B CFG GND R Vdd
*.PININFO B:I CFG:I GND:I R:I Vdd:I A:O
Xand GND Vdd A B x[0] R / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4100
Xor GND Vdd B CFG x[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_or2_4100
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_slow_invinv_4100
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv2 W=8.4E-08 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv2 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_delay_posedge_2_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_2_4000 A B CFG GND R Vdd
*.PININFO B:I CFG:I GND:I R:I Vdd:I A:O
Xs[0] GND Vdd x[0] x[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xand GND Vdd A B x[1] R / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4100
Xor GND Vdd B CFG x[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_or2_4100
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_delay_posedge_4_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_4_4000 A B CFG GND R Vdd
*.PININFO B:I CFG:I GND:I R:I Vdd:I A:O
Xs[0] GND Vdd x[0] x[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[1] GND Vdd x[1] x[2] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[2] GND Vdd x[2] x[3] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xand GND Vdd A B x[3] R / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4100
Xor GND Vdd B CFG x[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_or2_4100
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_delay_posedge_8_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_8_4000 A B CFG GND R Vdd
*.PININFO B:I CFG:I GND:I R:I Vdd:I A:O
Xs[0] GND Vdd x[0] x[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[1] GND Vdd x[1] x[2] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[2] GND Vdd x[2] x[3] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[3] GND Vdd x[3] x[4] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[4] GND Vdd x[4] x[5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[5] GND Vdd x[5] x[6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xs[6] GND Vdd x[6] x[7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xand GND Vdd A B x[7] R / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4100
Xor GND Vdd B CFG x[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_or2_4100
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_inv_4030
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_inv_4030 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mx.0 x a GND GND nuv1 W=9.24E-07 L=2.8E-08 m=1
Mx.1 x a Vdd Vdd puv1 W=7.56E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_inv_with_vcc_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_inv_with_vcc_4000 A GND Vdd WL_Vcc _A
*.PININFO GND:I Vdd:I WL_Vcc:I _A:I A:O
Mn.0 GND _A A GND n W=6.72E-07 L=2.8E-08 m=1
Mp.0 WL_Vcc _A A Vdd puv1 W=1.344E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_invinv_4001
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4001 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND n W=2.1E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=4.62E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_and2_4001
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=2.1E-07 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=3.36E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_and2_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_and2_4000 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tall_inv_4002
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_inv_4002 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mx.0 x a GND GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mx.1 x a Vdd Vdd puv1 W=1.68E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_demux_s1_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux_s1_4000 A3 A4 A[0].0 A[0].1 A[0].2 A[0].3 A[1].0 A[1].1 
+ A[1].2 A[1].3 A[2].0 A[2].1 A[2].2 A[2].3 B[0].0 B[0].1 B[0].2 B[0].3 B[1].0 
+ B[1].1 B[1].2 B[1].3 B[2].0 B[2].1 B[2].2 B[2].3 GND Root_Vcc S Vdd WL_Vcc
*.PININFO A4:I A[0].0:I A[0].1:I A[0].2:I A[0].3:I A[1].0:I A[1].1:I A[1].2:I 
*.PININFO A[1].3:I A[2].0:I A[2].1:I A[2].2:I A[2].3:I B[0].0:I B[0].1:I 
*.PININFO B[0].2:I B[0].3:I B[1].0:I B[1].1:I B[1].2:I B[1].3:I B[2].0:I 
*.PININFO B[2].1:I B[2].2:I B[2].3:I GND:I Root_Vcc:I S:I Vdd:I WL_Vcc:I A3:O
XinvS1 GND Vdd _s S / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_inv_4030
Xinv_vcc WL_Vcc GND Vdd Root_Vcc _s / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_inv_with_vcc_4000
XrampB[0][0] GND Vdd A[0].0 B[0].0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4001
XrampB[0][1] GND Vdd A[0].1 B[0].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4001
XrampB[0][2] GND Vdd A[0].2 B[0].2 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4001
XrampB[0][3] GND Vdd A[0].3 B[0].3 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_invinv_4001
XandB[1][0] GND Vdd A3 A[1].0 B[1].0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[1][1] GND Vdd A3 A[1].1 B[1].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[1][2] GND Vdd A3 A[1].2 B[1].2 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[1][3] GND Vdd A3 A[1].3 B[1].3 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[2][0] GND Vdd A4 A[2].0 B[2].0 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[2][1] GND Vdd A4 A[2].1 B[2].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[2][2] GND Vdd A4 A[2].2 B[2].2 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandB[2][3] GND Vdd A4 A[2].3 B[2].3 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4001
XandS GND Vdd A3 A4 x / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_and2_4000
XinvS0 GND Vdd x _s / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_inv_4002
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_oa211_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_oa211_4000 A[0] A[1] B[0] B[1] GND Vdd X
*.PININFO A[1]:I B[0]:I B[1]:I GND:I Vdd:I X:I A[0]:O
MX.0 X.1# _a Vdd Vdd p W=1.26E-07 L=2.8E-08 m=1
MX.1 X _b X.1# Vdd p W=1.26E-07 L=2.8E-08 m=1
M_a.0 _a.1# A[0] Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_a.1 _a A[1] _a.1# Vdd p W=8.4E-08 L=2.8E-08 m=1
M_b.2 _b B[0] Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_b.3 _b B[1] Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_b.1 _b B[1] _b.1# GND n W=8.4E-08 L=2.8E-08 m=1
MX.2 X _a GND GND n W=8.4E-08 L=2.8E-08 m=1
MX.3 X _b GND GND n W=8.4E-08 L=2.8E-08 m=1
M_b.0 _b.1# B[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
M_a.2 _a A[0] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_a.3 _a A[1] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_driver_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_driver_4000 DN GND Vdd WL_Vcc _UP
*.PININFO GND:I Vdd:I WL_Vcc:I _UP:I DN:O
Mpu.0 WL_Vcc _UP Vdd Vdd puv1 W=1.512E-06 L=2.8E-08 m=1
Mpd.0 WL_Vcc DN GND GND n W=1.008E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_nand2_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_nand2_4000 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mx.1 x a[1] x.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mx.0 x.1# a[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mx.2 x a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.3 x a[1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_or3_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_or3_4000 GND Vdd a[0] a[1] a[2] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I a[2]:I x:I
Mb.1 b.2# a[1] b.1# Vdd p W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[2] b.2# Vdd p W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[0] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mb.4 b a[1] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mb.5 b a[2] GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND n W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_and2_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4000 GND Vdd a[0] a[1] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I x:I
Mb.1 b a[1] b.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_booster_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_booster_4000 CFG DO DONE GND Vdd WR
*.PININFO DO:I DONE:I GND:I Vdd:I WR:I CFG:O
MDONE.0 DONE.1# _DONE GND GND n W=8.4E-08 L=2.8E-08 m=1
MDONE.1 DONE CFG DONE.1# GND n W=8.4E-08 L=2.8E-08 m=1
MWR.0 WR _WR GND GND n W=8.4E-08 L=2.8E-08 m=1
MDONE.2 DONE _DONE Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MDONE.3 DONE CFG Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
MWR.1 WR _WR Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
M_WR.1 _WR DO _WR.1# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_DONE.0 _DONE WR GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_WR.0 _WR.1# CFG GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_DONE.1 _DONE WR Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_WR.2 _WR CFG Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_WR.3 _WR DO Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_and3_4002
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4002 GND Vdd a[0] a[1] a[2] x
*.PININFO GND:I Vdd:I a[0]:I a[1]:I a[2]:I x:I
Mb.1 b.2# a[1] b.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.0 b.1# a[0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mb.2 b a[2] b.2# GND n W=8.4E-08 L=2.8E-08 m=1
Mb.3 b a[0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.4 b a[1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mb.5 b a[2] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_under_over_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_under_over_4000 GND Vdd WL_Vcc WR[0][0] WR[0][1] WR[1][0] WR[1][1] 
+ WR[2][0] WR[2][1] _RD[0] _RD[1] _RD[2]
*.PININFO GND:I Vdd:I WL_Vcc:I WR[0][0]:I WR[0][1]:I WR[1][0]:I WR[1][1]:I 
*.PININFO WR[2][0]:I WR[2][1]:I _RD[0]:I _RD[1]:I _RD[2]:I
Mrd0.0 WL_Vcc _RD[0] GND Vdd puv1 W=3.36E-07 L=2.8E-08 m=1
Mrd1.0 WL_Vcc _RD[1] GND Vdd puv1 W=6.72E-07 L=2.8E-08 m=1
Mrd2.0 WL_Vcc _RD[2] GND Vdd puv1 W=1.344E-06 L=2.8E-08 m=1
Mwr1a.0 WR[1][0] WL_Vcc WR[1][1] GND n W=2.016E-06 L=2.8E-08 m=1
Mwr0.0 WR[0][0] WL_Vcc WR[0][1] GND n W=2.016E-06 L=2.8E-08 m=1
Mwr1b.0 WR[1][0] WL_Vcc WR[1][1] GND n W=2.016E-06 L=2.8E-08 m=1
Mwr2a.0 WR[2][0] WL_Vcc WR[2][1] GND n W=2.016E-06 L=2.8E-08 m=1
Mwr2b.0 WR[2][0] WL_Vcc WR[2][1] GND n W=2.016E-06 L=2.8E-08 m=1
Mwr2c.0 WR[2][0] WL_Vcc WR[2][1] GND n W=2.016E-06 L=2.8E-08 m=1
Mwr2d.0 WR[2][0] WL_Vcc WR[2][1] GND n W=2.016E-06 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_up_dn_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_up_dn_4000 AR AW DLY DLY_OUT DN DONE GND Vdd _UP
*.PININFO AW:I DLY:I DLY_OUT:I DN:I DONE:I GND:I Vdd:I _UP:I AR:O
Mchain_0.0 chain_0.9# DLY_OUT _DLY_OUT GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mchain_0.1 GND DLY chain_0.9# GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# _UP _DLY_OUT GND nuv1 W=1.26E-07 L=2.8E-08 m=1
Mchain_1.1 GND DLY chain_1.9# GND nuv1 W=1.26E-07 L=2.8E-08 m=1
Mchain_4.0 GND AR _UP GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_5.0 N0 DONE _UP GND nuv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_6.0 GND AW N0 GND nuv1 W=2.52E-07 L=2.8E-08 m=1
Mchain_7.0 N0 _DLY _UP GND nuv1 W=2.52E-07 L=2.8E-08 m=1
MDN.2 DN AR GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
MDN.3 DN AW GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_DLY.0 _DLY DLY GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mchain_2.0 chain_2.9# DLY_OUT _DLY_OUT Vdd p W=4.2E-08 L=2.8E-08 m=1
Mchain_2.1 Vdd _UP chain_2.9# Vdd p W=4.2E-08 L=2.8E-08 m=1
Mchain_3.0 Vdd DLY _DLY_OUT Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_8.0 N1 AW _UP Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_9.0 Vdd AR N1 Vdd p W=1.26E-07 L=2.8E-08 m=1
Mchain_10.0 chain_10.9# DONE _UP Vdd p W=1.26E-07 L=2.8E-08 m=1
Mchain_10.1 N1 _DLY chain_10.9# Vdd p W=1.26E-07 L=2.8E-08 m=1
MDN.0 DN.1# AR Vdd Vdd p W=1.68E-07 L=2.8E-08 m=1
MDN.1 DN AW DN.1# Vdd p W=1.68E-07 L=2.8E-08 m=1
M_DLY.1 _DLY DLY Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
MDLY_OUT.0 DLY_OUT _DLY_OUT GND GND n W=8.4E-08 L=2.8E-08 m=1
MDLY_OUT.1 DLY_OUT _DLY_OUT Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_rw_assist_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_rw_assist_4000 AR AW CFG[0] CFG[1] CFG[2] CFG[3] CFG[4] CFG[5] 
+ CFG[6] CFG[7] GND Vdd WL_Vcc
*.PININFO AW:I CFG[0]:I CFG[1]:I CFG[2]:I CFG[3]:I CFG[4]:I CFG[5]:I CFG[6]:I 
*.PININFO CFG[7]:I GND:I Vdd:I WL_Vcc:I AR:O
Xdriver dn GND Vdd WL_Vcc _up / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_driver_4000
Xunder[0] GND Vdd CFG[5] AR _rd[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_nand2_4000
Xunder[1] GND Vdd CFG[6] AR _rd[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_nand2_4000
Xunder[2] GND Vdd CFG[7] AR _rd[2] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_nand2_4000
Xor_do GND Vdd CFG[2] CFG[3] CFG[4] do[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_or3_4000
Xand_do GND Vdd AW do[0] do[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and2_4000
Xramp_wl_vcc GND Vdd WL_Vcc dly[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xdly_done[0] GND Vdd done[3] done[4] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xdly_done[1] GND Vdd done[4] done[5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xdly_done[2] GND Vdd done[5] done[6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xdly_done[3] GND Vdd done[6] done[7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slow_invinv_4100
Xdelay[0] do[1] dly[0] CFG[0] GND dly[1] Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_2_4000
Xdelay[1] do[1] dly[1] CFG[1] GND dly[2] Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_4_4000
Xboost[0] CFG[2] dly[3] done[0] GND Vdd wr[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_booster_4000
Xboost[1] CFG[3] dly[3] done[1] GND Vdd wr[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_booster_4000
Xboost[2] CFG[4] dly[3] done[2] GND Vdd wr[2] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_booster_4000
Xand_done GND Vdd done[0] done[1] done[2] done[3] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_and3_4002
Xunov GND Vdd WL_Vcc wr[0] wr[0] wr[1] wr[1] wr[2] wr[2] _rd[0] _rd[1] _rd[2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_under_over_4000
Xup_dn AR AW dly[2] dly[3] dn done[7] GND Vdd _up / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_up_dn_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_svt_invinv_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000 GND Vdd a x
*.PININFO GND:I Vdd:I a:I x:I
Mb.0 b a GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0 x b GND GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mgnac.0 GND GND a GND nuv1 W=8.4E-08 L=2.8E-08 m=1
Mb.1 b a Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1 x b Vdd Vdd puv1 W=1.68E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_slice_ctrl_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice_ctrl_4000 ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 
+ ADDR[1].1 ADDR[1].2 ADDR[1].3 ADDR[2].0 ADDR[2].1 ADDR[2].2 ADDR[2].3 
+ ADDR[3].0 ADDR[3].1 ADDR[4].0 AR AW GND LCFG[0] LCFG[1] LCFG[2] LCFG[3] 
+ LCFG[4] LCFG[5] LCFG[6] LCFG[7] LCFG[8] LCFG[9] LCFG[10] LCFG[11] LK RCFG[0] 
+ RCFG[1] RCFG[2] RCFG[3] RCFG[4] RCFG[5] RCFG[6] RCFG[7] RCFG[8] RCFG[9] 
+ RCFG[10] RCFG[11] SLICE_EN Vdd WL_Vcc[0] WL_Vcc[1] _RESET _dlk b[0][0].0 
+ b[0][0].1 b[0][0].2 b[0][0].3 b[0][1].0 b[0][1].1 b[0][1].2 b[0][1].3 
+ b[0][2].0 b[0][2].1 b[0][2].2 b[0][2].3 b[1][0].0 b[1][0].1 b[1][0].2 
+ b[1][0].3 b[1][1].0 b[1][1].1 b[1][1].2 b[1][1].3 b[1][2].0 b[1][2].1 
+ b[1][2].2 b[1][2].3 lk[1] lk[2] s[0] s[1]
*.PININFO ADDR[0].0:I ADDR[0].1:I ADDR[0].2:I ADDR[0].3:I ADDR[1].0:I 
*.PININFO ADDR[1].1:I ADDR[1].2:I ADDR[1].3:I ADDR[2].0:I ADDR[2].1:I 
*.PININFO ADDR[2].2:I ADDR[2].3:I ADDR[3].0:I ADDR[3].1:I ADDR[4].0:I AW:I 
*.PININFO GND:I LCFG[0]:I LCFG[1]:I LCFG[2]:I LCFG[3]:I LCFG[4]:I LCFG[5]:I 
*.PININFO LCFG[6]:I LCFG[7]:I LCFG[8]:I LCFG[9]:I LCFG[10]:I LCFG[11]:I LK:I 
*.PININFO RCFG[0]:I RCFG[1]:I RCFG[2]:I RCFG[3]:I RCFG[4]:I RCFG[5]:I 
*.PININFO RCFG[6]:I RCFG[7]:I RCFG[8]:I RCFG[9]:I RCFG[10]:I RCFG[11]:I 
*.PININFO SLICE_EN:I Vdd:I WL_Vcc[0]:I WL_Vcc[1]:I _RESET:I _dlk:I b[0][0].0:I 
*.PININFO b[0][0].1:I b[0][0].2:I b[0][0].3:I b[0][1].0:I b[0][1].1:I 
*.PININFO b[0][1].2:I b[0][1].3:I b[0][2].0:I b[0][2].1:I b[0][2].2:I 
*.PININFO b[0][2].3:I b[1][0].0:I b[1][0].1:I b[1][0].2:I b[1][0].3:I 
*.PININFO b[1][1].0:I b[1][1].1:I b[1][1].2:I b[1][1].3:I b[1][2].0:I 
*.PININFO b[1][2].1:I b[1][2].2:I b[1][2].3:I lk[1]:I lk[2]:I s[0]:I s[1]:I 
*.PININFO AR:O
XlockLK0 LK lk[0] GND SLICE_EN Vdd _dlk _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_lock_clock_4000
XrampLK1 GND Vdd lk[0] lk[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4040
XrampLK2 GND Vdd lk[1] lk[2] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_invinv_4040
XdelayDLK[0] lk[2] lk[2] RCFG[0] GND dlk[1] Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_1_4000
XdelayDLK[1] lk[2] dlk[1] RCFG[1] GND dlk[2] Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_2_4000
XdelayDLK[2] lk[2] dlk[2] RCFG[2] GND dlk[3] Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_4_4000
XdelayDLK[3] lk[2] dlk[3] RCFG[3] GND dlk[4] Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_delay_posedge_8_4000
XinvDLK GND Vdd dlk[4] _dlk / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tall_inv_4030
Xdemux[0] ADDR[3].0 ADDR[4].0 ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 
+ ADDR[1].0 ADDR[1].1 ADDR[1].2 ADDR[1].3 ADDR[2].0 ADDR[2].1 ADDR[2].2 
+ ADDR[2].3 b[0][0].0 b[0][0].1 b[0][0].2 b[0][0].3 b[0][1].0 b[0][1].1 
+ b[0][1].2 b[0][1].3 b[0][2].0 b[0][2].1 b[0][2].2 b[0][2].3 GND Root_Vcc 
+ s[0] Vdd WL_Vcc[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux_s1_4000
Xdemux[1] ADDR[3].1 ADDR[4].0 ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 
+ ADDR[1].0 ADDR[1].1 ADDR[1].2 ADDR[1].3 ADDR[2].0 ADDR[2].1 ADDR[2].2 
+ ADDR[2].3 b[1][0].0 b[1][0].1 b[1][0].2 b[1][0].3 b[1][1].0 b[1][1].1 
+ b[1][1].2 b[1][1].3 b[1][2].0 b[1][2].1 b[1][2].2 b[1][2].3 GND Root_Vcc 
+ s[1] Vdd WL_Vcc[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_demux_s1_4000
Xdecode_ar ADDR[3].0 ADDR[3].1 ADDR[4].0 AR GND Vdd xar / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_oa211_4000
Xdecode_aw ADDR[3].0 ADDR[3].1 ADDR[4].0 AW GND Vdd xaw / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_oa211_4000
Xassist xar xaw RCFG[4] RCFG[5] RCFG[6] RCFG[7] RCFG[8] RCFG[9] RCFG[10] 
+ RCFG[11] GND Vdd Root_Vcc / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_rw_assist_4000
XrampCFG[0] GND Vdd LCFG[0] RCFG[0] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[1] GND Vdd LCFG[1] RCFG[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[2] GND Vdd LCFG[2] RCFG[2] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[3] GND Vdd LCFG[3] RCFG[3] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[4] GND Vdd LCFG[4] RCFG[4] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[5] GND Vdd LCFG[5] RCFG[5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[6] GND Vdd LCFG[6] RCFG[6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[7] GND Vdd LCFG[7] RCFG[7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[8] GND Vdd LCFG[8] RCFG[8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[9] GND Vdd LCFG[9] RCFG[9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[10] GND Vdd LCFG[10] RCFG[10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
XrampCFG[11] GND Vdd LCFG[11] RCFG[11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_svt_invinv_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tbit_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000 GND K[0].0 K[0].1 Vdd a[0] a[1] b[0] b[1] hit[0]
*.PININFO GND:I K[0].0:I K[0].1:I Vdd:I a[0]:I a[1]:I b[0]:I b[1]:I hit[0]:I
Mn0a.0 GND x[0][1] x[0][0] GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mn0b.0 GND x[0][0] x[0][1] GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mn1a.0 GND x[1][1] x[1][0] GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mn1b.0 GND x[1][0] x[1][1] GND nuv1 W=4.2E-08 L=2.8E-08 m=1
Mp0a.0 Vdd x[0][1] x[0][0] Vdd puv2 W=4.2E-08 L=2.8E-08 m=1
Mp0b.0 Vdd x[0][0] x[0][1] Vdd puv2 W=4.2E-08 L=2.8E-08 m=1
Mp1a.0 Vdd x[1][1] x[1][0] Vdd puv2 W=4.2E-08 L=2.8E-08 m=1
Mp1b.0 Vdd x[1][0] x[1][1] Vdd puv2 W=4.2E-08 L=2.8E-08 m=1
Mg0a.0 b[0] a[0] x[0][0] GND nuv2 W=4.2E-08 L=2.8E-08 m=1
Mg0b.0 b[1] a[0] x[0][1] GND nuv2 W=4.2E-08 L=2.8E-08 m=1
Mg1a.0 b[0] a[1] x[1][0] GND nuv2 W=4.2E-08 L=2.8E-08 m=1
Mg1b.0 b[1] a[1] x[1][1] GND nuv2 W=4.2E-08 L=2.8E-08 m=1
Mh0.0 h0.9# K[0].0 GND GND nuv2 W=1.26E-07 L=2.8E-08 m=1
Mh0.1 hit[0] x[0][0] h0.9# GND nuv2 W=1.26E-07 L=2.8E-08 m=1
Mh1.0 h1.9# K[0].1 GND GND nuv2 W=1.26E-07 L=2.8E-08 m=1
Mh1.1 hit[0] x[1][0] h1.9# GND nuv2 W=1.26E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tbit_array_32_20_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_array_32_20_4000 GND Vdd a[0] a[1] a[2] a[3] a[4] a[5] a[6] 
+ a[7] a[8] a[9] a[10] a[11] a[12] a[13] a[14] a[15] a[16] a[17] a[18] a[19] 
+ a[20] a[21] a[22] a[23] a[24] a[25] a[26] a[27] a[28] a[29] a[30] a[31] 
+ a[32] a[33] a[34] a[35] a[36] a[37] a[38] a[39] a[40] a[41] a[42] a[43] 
+ a[44] a[45] a[46] a[47] a[48] a[49] a[50] a[51] a[52] a[53] a[54] a[55] 
+ a[56] a[57] a[58] a[59] a[60] a[61] a[62] a[63] b[0][0] b[0][1] b[1][0] 
+ b[1][1] b[2][0] b[2][1] b[3][0] b[3][1] b[4][0] b[4][1] b[5][0] b[5][1] 
+ b[6][0] b[6][1] b[7][0] b[7][1] b[8][0] b[8][1] b[9][0] b[9][1] b[10][0] 
+ b[10][1] b[11][0] b[11][1] b[12][0] b[12][1] b[13][0] b[13][1] b[14][0] 
+ b[14][1] b[15][0] b[15][1] b[16][0] b[16][1] b[17][0] b[17][1] b[18][0] 
+ b[18][1] b[19][0] b[19][1] hit[0][0] hit[0][1] hit[0][2] hit[0][3] hit[0][4] 
+ hit[0][5] hit[0][6] hit[0][7] hit[0][8] hit[0][9] hit[0][10] hit[0][11] 
+ hit[0][12] hit[0][13] hit[0][14] hit[0][15] hit[0][16] hit[0][17] hit[0][18] 
+ hit[0][19] hit[0][20] hit[0][21] hit[0][22] hit[0][23] hit[0][24] hit[0][25] 
+ hit[0][26] hit[0][27] hit[0][28] hit[0][29] hit[0][30] hit[0][31] k[0][0].0 
+ k[0][0].1 k[0][1].0 k[0][1].1 k[0][2].0 k[0][2].1 k[0][3].0 k[0][3].1 
+ k[0][4].0 k[0][4].1 k[0][5].0 k[0][5].1 k[0][6].0 k[0][6].1 k[0][7].0 
+ k[0][7].1 k[0][8].0 k[0][8].1 k[0][9].0 k[0][9].1 k[0][10].0 k[0][10].1 
+ k[0][11].0 k[0][11].1 k[0][12].0 k[0][12].1 k[0][13].0 k[0][13].1 k[0][14].0 
+ k[0][14].1 k[0][15].0 k[0][15].1 k[0][16].0 k[0][16].1 k[0][17].0 k[0][17].1 
+ k[0][18].0 k[0][18].1 k[0][19].0 k[0][19].1
*.PININFO GND:I Vdd:I a[0]:I a[1]:I a[2]:I a[3]:I a[4]:I a[5]:I a[6]:I a[7]:I 
*.PININFO a[8]:I a[9]:I a[10]:I a[11]:I a[12]:I a[13]:I a[14]:I a[15]:I 
*.PININFO a[16]:I a[17]:I a[18]:I a[19]:I a[20]:I a[21]:I a[22]:I a[23]:I 
*.PININFO a[24]:I a[25]:I a[26]:I a[27]:I a[28]:I a[29]:I a[30]:I a[31]:I 
*.PININFO a[32]:I a[33]:I a[34]:I a[35]:I a[36]:I a[37]:I a[38]:I a[39]:I 
*.PININFO a[40]:I a[41]:I a[42]:I a[43]:I a[44]:I a[45]:I a[46]:I a[47]:I 
*.PININFO a[48]:I a[49]:I a[50]:I a[51]:I a[52]:I a[53]:I a[54]:I a[55]:I 
*.PININFO a[56]:I a[57]:I a[58]:I a[59]:I a[60]:I a[61]:I a[62]:I a[63]:I 
*.PININFO b[0][0]:I b[0][1]:I b[1][0]:I b[1][1]:I b[2][0]:I b[2][1]:I 
*.PININFO b[3][0]:I b[3][1]:I b[4][0]:I b[4][1]:I b[5][0]:I b[5][1]:I 
*.PININFO b[6][0]:I b[6][1]:I b[7][0]:I b[7][1]:I b[8][0]:I b[8][1]:I 
*.PININFO b[9][0]:I b[9][1]:I b[10][0]:I b[10][1]:I b[11][0]:I b[11][1]:I 
*.PININFO b[12][0]:I b[12][1]:I b[13][0]:I b[13][1]:I b[14][0]:I b[14][1]:I 
*.PININFO b[15][0]:I b[15][1]:I b[16][0]:I b[16][1]:I b[17][0]:I b[17][1]:I 
*.PININFO b[18][0]:I b[18][1]:I b[19][0]:I b[19][1]:I hit[0][0]:I hit[0][1]:I 
*.PININFO hit[0][2]:I hit[0][3]:I hit[0][4]:I hit[0][5]:I hit[0][6]:I 
*.PININFO hit[0][7]:I hit[0][8]:I hit[0][9]:I hit[0][10]:I hit[0][11]:I 
*.PININFO hit[0][12]:I hit[0][13]:I hit[0][14]:I hit[0][15]:I hit[0][16]:I 
*.PININFO hit[0][17]:I hit[0][18]:I hit[0][19]:I hit[0][20]:I hit[0][21]:I 
*.PININFO hit[0][22]:I hit[0][23]:I hit[0][24]:I hit[0][25]:I hit[0][26]:I 
*.PININFO hit[0][27]:I hit[0][28]:I hit[0][29]:I hit[0][30]:I hit[0][31]:I 
*.PININFO k[0][0].0:I k[0][0].1:I k[0][1].0:I k[0][1].1:I k[0][2].0:I 
*.PININFO k[0][2].1:I k[0][3].0:I k[0][3].1:I k[0][4].0:I k[0][4].1:I 
*.PININFO k[0][5].0:I k[0][5].1:I k[0][6].0:I k[0][6].1:I k[0][7].0:I 
*.PININFO k[0][7].1:I k[0][8].0:I k[0][8].1:I k[0][9].0:I k[0][9].1:I 
*.PININFO k[0][10].0:I k[0][10].1:I k[0][11].0:I k[0][11].1:I k[0][12].0:I 
*.PININFO k[0][12].1:I k[0][13].0:I k[0][13].1:I k[0][14].0:I k[0][14].1:I 
*.PININFO k[0][15].0:I k[0][15].1:I k[0][16].0:I k[0][16].1:I k[0][17].0:I 
*.PININFO k[0][17].1:I k[0][18].0:I k[0][18].1:I k[0][19].0:I k[0][19].1:I
Xz[0][0] GND k[0][0].0 k[0][0].1 Vdd a[0] a[1] b[0][0] b[0][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][1] GND k[0][1].0 k[0][1].1 Vdd a[0] a[1] b[1][0] b[1][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][2] GND k[0][2].0 k[0][2].1 Vdd a[0] a[1] b[2][0] b[2][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][3] GND k[0][3].0 k[0][3].1 Vdd a[0] a[1] b[3][0] b[3][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][4] GND k[0][4].0 k[0][4].1 Vdd a[0] a[1] b[4][0] b[4][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][5] GND k[0][5].0 k[0][5].1 Vdd a[0] a[1] b[5][0] b[5][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][6] GND k[0][6].0 k[0][6].1 Vdd a[0] a[1] b[6][0] b[6][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][7] GND k[0][7].0 k[0][7].1 Vdd a[0] a[1] b[7][0] b[7][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][8] GND k[0][8].0 k[0][8].1 Vdd a[0] a[1] b[8][0] b[8][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][9] GND k[0][9].0 k[0][9].1 Vdd a[0] a[1] b[9][0] b[9][1] hit[0][0] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][10] GND k[0][10].0 k[0][10].1 Vdd a[0] a[1] b[10][0] b[10][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][11] GND k[0][11].0 k[0][11].1 Vdd a[0] a[1] b[11][0] b[11][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][12] GND k[0][12].0 k[0][12].1 Vdd a[0] a[1] b[12][0] b[12][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][13] GND k[0][13].0 k[0][13].1 Vdd a[0] a[1] b[13][0] b[13][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][14] GND k[0][14].0 k[0][14].1 Vdd a[0] a[1] b[14][0] b[14][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][15] GND k[0][15].0 k[0][15].1 Vdd a[0] a[1] b[15][0] b[15][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][16] GND k[0][16].0 k[0][16].1 Vdd a[0] a[1] b[16][0] b[16][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][17] GND k[0][17].0 k[0][17].1 Vdd a[0] a[1] b[17][0] b[17][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][18] GND k[0][18].0 k[0][18].1 Vdd a[0] a[1] b[18][0] b[18][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[0][19] GND k[0][19].0 k[0][19].1 Vdd a[0] a[1] b[19][0] b[19][1] hit[0][0] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][0] GND k[0][0].0 k[0][0].1 Vdd a[2] a[3] b[0][0] b[0][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][1] GND k[0][1].0 k[0][1].1 Vdd a[2] a[3] b[1][0] b[1][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][2] GND k[0][2].0 k[0][2].1 Vdd a[2] a[3] b[2][0] b[2][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][3] GND k[0][3].0 k[0][3].1 Vdd a[2] a[3] b[3][0] b[3][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][4] GND k[0][4].0 k[0][4].1 Vdd a[2] a[3] b[4][0] b[4][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][5] GND k[0][5].0 k[0][5].1 Vdd a[2] a[3] b[5][0] b[5][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][6] GND k[0][6].0 k[0][6].1 Vdd a[2] a[3] b[6][0] b[6][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][7] GND k[0][7].0 k[0][7].1 Vdd a[2] a[3] b[7][0] b[7][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][8] GND k[0][8].0 k[0][8].1 Vdd a[2] a[3] b[8][0] b[8][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][9] GND k[0][9].0 k[0][9].1 Vdd a[2] a[3] b[9][0] b[9][1] hit[0][1] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][10] GND k[0][10].0 k[0][10].1 Vdd a[2] a[3] b[10][0] b[10][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][11] GND k[0][11].0 k[0][11].1 Vdd a[2] a[3] b[11][0] b[11][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][12] GND k[0][12].0 k[0][12].1 Vdd a[2] a[3] b[12][0] b[12][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][13] GND k[0][13].0 k[0][13].1 Vdd a[2] a[3] b[13][0] b[13][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][14] GND k[0][14].0 k[0][14].1 Vdd a[2] a[3] b[14][0] b[14][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][15] GND k[0][15].0 k[0][15].1 Vdd a[2] a[3] b[15][0] b[15][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][16] GND k[0][16].0 k[0][16].1 Vdd a[2] a[3] b[16][0] b[16][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][17] GND k[0][17].0 k[0][17].1 Vdd a[2] a[3] b[17][0] b[17][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][18] GND k[0][18].0 k[0][18].1 Vdd a[2] a[3] b[18][0] b[18][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[1][19] GND k[0][19].0 k[0][19].1 Vdd a[2] a[3] b[19][0] b[19][1] hit[0][1] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][0] GND k[0][0].0 k[0][0].1 Vdd a[4] a[5] b[0][0] b[0][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][1] GND k[0][1].0 k[0][1].1 Vdd a[4] a[5] b[1][0] b[1][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][2] GND k[0][2].0 k[0][2].1 Vdd a[4] a[5] b[2][0] b[2][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][3] GND k[0][3].0 k[0][3].1 Vdd a[4] a[5] b[3][0] b[3][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][4] GND k[0][4].0 k[0][4].1 Vdd a[4] a[5] b[4][0] b[4][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][5] GND k[0][5].0 k[0][5].1 Vdd a[4] a[5] b[5][0] b[5][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][6] GND k[0][6].0 k[0][6].1 Vdd a[4] a[5] b[6][0] b[6][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][7] GND k[0][7].0 k[0][7].1 Vdd a[4] a[5] b[7][0] b[7][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][8] GND k[0][8].0 k[0][8].1 Vdd a[4] a[5] b[8][0] b[8][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][9] GND k[0][9].0 k[0][9].1 Vdd a[4] a[5] b[9][0] b[9][1] hit[0][2] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][10] GND k[0][10].0 k[0][10].1 Vdd a[4] a[5] b[10][0] b[10][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][11] GND k[0][11].0 k[0][11].1 Vdd a[4] a[5] b[11][0] b[11][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][12] GND k[0][12].0 k[0][12].1 Vdd a[4] a[5] b[12][0] b[12][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][13] GND k[0][13].0 k[0][13].1 Vdd a[4] a[5] b[13][0] b[13][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][14] GND k[0][14].0 k[0][14].1 Vdd a[4] a[5] b[14][0] b[14][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][15] GND k[0][15].0 k[0][15].1 Vdd a[4] a[5] b[15][0] b[15][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][16] GND k[0][16].0 k[0][16].1 Vdd a[4] a[5] b[16][0] b[16][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][17] GND k[0][17].0 k[0][17].1 Vdd a[4] a[5] b[17][0] b[17][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][18] GND k[0][18].0 k[0][18].1 Vdd a[4] a[5] b[18][0] b[18][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[2][19] GND k[0][19].0 k[0][19].1 Vdd a[4] a[5] b[19][0] b[19][1] hit[0][2] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][0] GND k[0][0].0 k[0][0].1 Vdd a[6] a[7] b[0][0] b[0][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][1] GND k[0][1].0 k[0][1].1 Vdd a[6] a[7] b[1][0] b[1][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][2] GND k[0][2].0 k[0][2].1 Vdd a[6] a[7] b[2][0] b[2][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][3] GND k[0][3].0 k[0][3].1 Vdd a[6] a[7] b[3][0] b[3][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][4] GND k[0][4].0 k[0][4].1 Vdd a[6] a[7] b[4][0] b[4][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][5] GND k[0][5].0 k[0][5].1 Vdd a[6] a[7] b[5][0] b[5][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][6] GND k[0][6].0 k[0][6].1 Vdd a[6] a[7] b[6][0] b[6][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][7] GND k[0][7].0 k[0][7].1 Vdd a[6] a[7] b[7][0] b[7][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][8] GND k[0][8].0 k[0][8].1 Vdd a[6] a[7] b[8][0] b[8][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][9] GND k[0][9].0 k[0][9].1 Vdd a[6] a[7] b[9][0] b[9][1] hit[0][3] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][10] GND k[0][10].0 k[0][10].1 Vdd a[6] a[7] b[10][0] b[10][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][11] GND k[0][11].0 k[0][11].1 Vdd a[6] a[7] b[11][0] b[11][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][12] GND k[0][12].0 k[0][12].1 Vdd a[6] a[7] b[12][0] b[12][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][13] GND k[0][13].0 k[0][13].1 Vdd a[6] a[7] b[13][0] b[13][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][14] GND k[0][14].0 k[0][14].1 Vdd a[6] a[7] b[14][0] b[14][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][15] GND k[0][15].0 k[0][15].1 Vdd a[6] a[7] b[15][0] b[15][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][16] GND k[0][16].0 k[0][16].1 Vdd a[6] a[7] b[16][0] b[16][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][17] GND k[0][17].0 k[0][17].1 Vdd a[6] a[7] b[17][0] b[17][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][18] GND k[0][18].0 k[0][18].1 Vdd a[6] a[7] b[18][0] b[18][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[3][19] GND k[0][19].0 k[0][19].1 Vdd a[6] a[7] b[19][0] b[19][1] hit[0][3] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][0] GND k[0][0].0 k[0][0].1 Vdd a[8] a[9] b[0][0] b[0][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][1] GND k[0][1].0 k[0][1].1 Vdd a[8] a[9] b[1][0] b[1][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][2] GND k[0][2].0 k[0][2].1 Vdd a[8] a[9] b[2][0] b[2][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][3] GND k[0][3].0 k[0][3].1 Vdd a[8] a[9] b[3][0] b[3][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][4] GND k[0][4].0 k[0][4].1 Vdd a[8] a[9] b[4][0] b[4][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][5] GND k[0][5].0 k[0][5].1 Vdd a[8] a[9] b[5][0] b[5][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][6] GND k[0][6].0 k[0][6].1 Vdd a[8] a[9] b[6][0] b[6][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][7] GND k[0][7].0 k[0][7].1 Vdd a[8] a[9] b[7][0] b[7][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][8] GND k[0][8].0 k[0][8].1 Vdd a[8] a[9] b[8][0] b[8][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][9] GND k[0][9].0 k[0][9].1 Vdd a[8] a[9] b[9][0] b[9][1] hit[0][4] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][10] GND k[0][10].0 k[0][10].1 Vdd a[8] a[9] b[10][0] b[10][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][11] GND k[0][11].0 k[0][11].1 Vdd a[8] a[9] b[11][0] b[11][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][12] GND k[0][12].0 k[0][12].1 Vdd a[8] a[9] b[12][0] b[12][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][13] GND k[0][13].0 k[0][13].1 Vdd a[8] a[9] b[13][0] b[13][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][14] GND k[0][14].0 k[0][14].1 Vdd a[8] a[9] b[14][0] b[14][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][15] GND k[0][15].0 k[0][15].1 Vdd a[8] a[9] b[15][0] b[15][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][16] GND k[0][16].0 k[0][16].1 Vdd a[8] a[9] b[16][0] b[16][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][17] GND k[0][17].0 k[0][17].1 Vdd a[8] a[9] b[17][0] b[17][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][18] GND k[0][18].0 k[0][18].1 Vdd a[8] a[9] b[18][0] b[18][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[4][19] GND k[0][19].0 k[0][19].1 Vdd a[8] a[9] b[19][0] b[19][1] hit[0][4] 
+ / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][0] GND k[0][0].0 k[0][0].1 Vdd a[10] a[11] b[0][0] b[0][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][1] GND k[0][1].0 k[0][1].1 Vdd a[10] a[11] b[1][0] b[1][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][2] GND k[0][2].0 k[0][2].1 Vdd a[10] a[11] b[2][0] b[2][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][3] GND k[0][3].0 k[0][3].1 Vdd a[10] a[11] b[3][0] b[3][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][4] GND k[0][4].0 k[0][4].1 Vdd a[10] a[11] b[4][0] b[4][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][5] GND k[0][5].0 k[0][5].1 Vdd a[10] a[11] b[5][0] b[5][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][6] GND k[0][6].0 k[0][6].1 Vdd a[10] a[11] b[6][0] b[6][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][7] GND k[0][7].0 k[0][7].1 Vdd a[10] a[11] b[7][0] b[7][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][8] GND k[0][8].0 k[0][8].1 Vdd a[10] a[11] b[8][0] b[8][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][9] GND k[0][9].0 k[0][9].1 Vdd a[10] a[11] b[9][0] b[9][1] hit[0][5] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][10] GND k[0][10].0 k[0][10].1 Vdd a[10] a[11] b[10][0] b[10][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][11] GND k[0][11].0 k[0][11].1 Vdd a[10] a[11] b[11][0] b[11][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][12] GND k[0][12].0 k[0][12].1 Vdd a[10] a[11] b[12][0] b[12][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][13] GND k[0][13].0 k[0][13].1 Vdd a[10] a[11] b[13][0] b[13][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][14] GND k[0][14].0 k[0][14].1 Vdd a[10] a[11] b[14][0] b[14][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][15] GND k[0][15].0 k[0][15].1 Vdd a[10] a[11] b[15][0] b[15][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][16] GND k[0][16].0 k[0][16].1 Vdd a[10] a[11] b[16][0] b[16][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][17] GND k[0][17].0 k[0][17].1 Vdd a[10] a[11] b[17][0] b[17][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][18] GND k[0][18].0 k[0][18].1 Vdd a[10] a[11] b[18][0] b[18][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[5][19] GND k[0][19].0 k[0][19].1 Vdd a[10] a[11] b[19][0] b[19][1] 
+ hit[0][5] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][0] GND k[0][0].0 k[0][0].1 Vdd a[12] a[13] b[0][0] b[0][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][1] GND k[0][1].0 k[0][1].1 Vdd a[12] a[13] b[1][0] b[1][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][2] GND k[0][2].0 k[0][2].1 Vdd a[12] a[13] b[2][0] b[2][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][3] GND k[0][3].0 k[0][3].1 Vdd a[12] a[13] b[3][0] b[3][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][4] GND k[0][4].0 k[0][4].1 Vdd a[12] a[13] b[4][0] b[4][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][5] GND k[0][5].0 k[0][5].1 Vdd a[12] a[13] b[5][0] b[5][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][6] GND k[0][6].0 k[0][6].1 Vdd a[12] a[13] b[6][0] b[6][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][7] GND k[0][7].0 k[0][7].1 Vdd a[12] a[13] b[7][0] b[7][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][8] GND k[0][8].0 k[0][8].1 Vdd a[12] a[13] b[8][0] b[8][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][9] GND k[0][9].0 k[0][9].1 Vdd a[12] a[13] b[9][0] b[9][1] hit[0][6] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][10] GND k[0][10].0 k[0][10].1 Vdd a[12] a[13] b[10][0] b[10][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][11] GND k[0][11].0 k[0][11].1 Vdd a[12] a[13] b[11][0] b[11][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][12] GND k[0][12].0 k[0][12].1 Vdd a[12] a[13] b[12][0] b[12][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][13] GND k[0][13].0 k[0][13].1 Vdd a[12] a[13] b[13][0] b[13][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][14] GND k[0][14].0 k[0][14].1 Vdd a[12] a[13] b[14][0] b[14][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][15] GND k[0][15].0 k[0][15].1 Vdd a[12] a[13] b[15][0] b[15][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][16] GND k[0][16].0 k[0][16].1 Vdd a[12] a[13] b[16][0] b[16][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][17] GND k[0][17].0 k[0][17].1 Vdd a[12] a[13] b[17][0] b[17][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][18] GND k[0][18].0 k[0][18].1 Vdd a[12] a[13] b[18][0] b[18][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[6][19] GND k[0][19].0 k[0][19].1 Vdd a[12] a[13] b[19][0] b[19][1] 
+ hit[0][6] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][0] GND k[0][0].0 k[0][0].1 Vdd a[14] a[15] b[0][0] b[0][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][1] GND k[0][1].0 k[0][1].1 Vdd a[14] a[15] b[1][0] b[1][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][2] GND k[0][2].0 k[0][2].1 Vdd a[14] a[15] b[2][0] b[2][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][3] GND k[0][3].0 k[0][3].1 Vdd a[14] a[15] b[3][0] b[3][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][4] GND k[0][4].0 k[0][4].1 Vdd a[14] a[15] b[4][0] b[4][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][5] GND k[0][5].0 k[0][5].1 Vdd a[14] a[15] b[5][0] b[5][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][6] GND k[0][6].0 k[0][6].1 Vdd a[14] a[15] b[6][0] b[6][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][7] GND k[0][7].0 k[0][7].1 Vdd a[14] a[15] b[7][0] b[7][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][8] GND k[0][8].0 k[0][8].1 Vdd a[14] a[15] b[8][0] b[8][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][9] GND k[0][9].0 k[0][9].1 Vdd a[14] a[15] b[9][0] b[9][1] hit[0][7] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][10] GND k[0][10].0 k[0][10].1 Vdd a[14] a[15] b[10][0] b[10][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][11] GND k[0][11].0 k[0][11].1 Vdd a[14] a[15] b[11][0] b[11][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][12] GND k[0][12].0 k[0][12].1 Vdd a[14] a[15] b[12][0] b[12][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][13] GND k[0][13].0 k[0][13].1 Vdd a[14] a[15] b[13][0] b[13][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][14] GND k[0][14].0 k[0][14].1 Vdd a[14] a[15] b[14][0] b[14][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][15] GND k[0][15].0 k[0][15].1 Vdd a[14] a[15] b[15][0] b[15][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][16] GND k[0][16].0 k[0][16].1 Vdd a[14] a[15] b[16][0] b[16][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][17] GND k[0][17].0 k[0][17].1 Vdd a[14] a[15] b[17][0] b[17][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][18] GND k[0][18].0 k[0][18].1 Vdd a[14] a[15] b[18][0] b[18][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[7][19] GND k[0][19].0 k[0][19].1 Vdd a[14] a[15] b[19][0] b[19][1] 
+ hit[0][7] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][0] GND k[0][0].0 k[0][0].1 Vdd a[16] a[17] b[0][0] b[0][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][1] GND k[0][1].0 k[0][1].1 Vdd a[16] a[17] b[1][0] b[1][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][2] GND k[0][2].0 k[0][2].1 Vdd a[16] a[17] b[2][0] b[2][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][3] GND k[0][3].0 k[0][3].1 Vdd a[16] a[17] b[3][0] b[3][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][4] GND k[0][4].0 k[0][4].1 Vdd a[16] a[17] b[4][0] b[4][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][5] GND k[0][5].0 k[0][5].1 Vdd a[16] a[17] b[5][0] b[5][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][6] GND k[0][6].0 k[0][6].1 Vdd a[16] a[17] b[6][0] b[6][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][7] GND k[0][7].0 k[0][7].1 Vdd a[16] a[17] b[7][0] b[7][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][8] GND k[0][8].0 k[0][8].1 Vdd a[16] a[17] b[8][0] b[8][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][9] GND k[0][9].0 k[0][9].1 Vdd a[16] a[17] b[9][0] b[9][1] hit[0][8] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][10] GND k[0][10].0 k[0][10].1 Vdd a[16] a[17] b[10][0] b[10][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][11] GND k[0][11].0 k[0][11].1 Vdd a[16] a[17] b[11][0] b[11][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][12] GND k[0][12].0 k[0][12].1 Vdd a[16] a[17] b[12][0] b[12][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][13] GND k[0][13].0 k[0][13].1 Vdd a[16] a[17] b[13][0] b[13][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][14] GND k[0][14].0 k[0][14].1 Vdd a[16] a[17] b[14][0] b[14][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][15] GND k[0][15].0 k[0][15].1 Vdd a[16] a[17] b[15][0] b[15][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][16] GND k[0][16].0 k[0][16].1 Vdd a[16] a[17] b[16][0] b[16][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][17] GND k[0][17].0 k[0][17].1 Vdd a[16] a[17] b[17][0] b[17][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][18] GND k[0][18].0 k[0][18].1 Vdd a[16] a[17] b[18][0] b[18][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[8][19] GND k[0][19].0 k[0][19].1 Vdd a[16] a[17] b[19][0] b[19][1] 
+ hit[0][8] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][0] GND k[0][0].0 k[0][0].1 Vdd a[18] a[19] b[0][0] b[0][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][1] GND k[0][1].0 k[0][1].1 Vdd a[18] a[19] b[1][0] b[1][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][2] GND k[0][2].0 k[0][2].1 Vdd a[18] a[19] b[2][0] b[2][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][3] GND k[0][3].0 k[0][3].1 Vdd a[18] a[19] b[3][0] b[3][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][4] GND k[0][4].0 k[0][4].1 Vdd a[18] a[19] b[4][0] b[4][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][5] GND k[0][5].0 k[0][5].1 Vdd a[18] a[19] b[5][0] b[5][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][6] GND k[0][6].0 k[0][6].1 Vdd a[18] a[19] b[6][0] b[6][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][7] GND k[0][7].0 k[0][7].1 Vdd a[18] a[19] b[7][0] b[7][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][8] GND k[0][8].0 k[0][8].1 Vdd a[18] a[19] b[8][0] b[8][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][9] GND k[0][9].0 k[0][9].1 Vdd a[18] a[19] b[9][0] b[9][1] hit[0][9] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][10] GND k[0][10].0 k[0][10].1 Vdd a[18] a[19] b[10][0] b[10][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][11] GND k[0][11].0 k[0][11].1 Vdd a[18] a[19] b[11][0] b[11][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][12] GND k[0][12].0 k[0][12].1 Vdd a[18] a[19] b[12][0] b[12][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][13] GND k[0][13].0 k[0][13].1 Vdd a[18] a[19] b[13][0] b[13][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][14] GND k[0][14].0 k[0][14].1 Vdd a[18] a[19] b[14][0] b[14][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][15] GND k[0][15].0 k[0][15].1 Vdd a[18] a[19] b[15][0] b[15][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][16] GND k[0][16].0 k[0][16].1 Vdd a[18] a[19] b[16][0] b[16][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][17] GND k[0][17].0 k[0][17].1 Vdd a[18] a[19] b[17][0] b[17][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][18] GND k[0][18].0 k[0][18].1 Vdd a[18] a[19] b[18][0] b[18][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[9][19] GND k[0][19].0 k[0][19].1 Vdd a[18] a[19] b[19][0] b[19][1] 
+ hit[0][9] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][0] GND k[0][0].0 k[0][0].1 Vdd a[20] a[21] b[0][0] b[0][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][1] GND k[0][1].0 k[0][1].1 Vdd a[20] a[21] b[1][0] b[1][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][2] GND k[0][2].0 k[0][2].1 Vdd a[20] a[21] b[2][0] b[2][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][3] GND k[0][3].0 k[0][3].1 Vdd a[20] a[21] b[3][0] b[3][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][4] GND k[0][4].0 k[0][4].1 Vdd a[20] a[21] b[4][0] b[4][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][5] GND k[0][5].0 k[0][5].1 Vdd a[20] a[21] b[5][0] b[5][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][6] GND k[0][6].0 k[0][6].1 Vdd a[20] a[21] b[6][0] b[6][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][7] GND k[0][7].0 k[0][7].1 Vdd a[20] a[21] b[7][0] b[7][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][8] GND k[0][8].0 k[0][8].1 Vdd a[20] a[21] b[8][0] b[8][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][9] GND k[0][9].0 k[0][9].1 Vdd a[20] a[21] b[9][0] b[9][1] hit[0][10] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][10] GND k[0][10].0 k[0][10].1 Vdd a[20] a[21] b[10][0] b[10][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][11] GND k[0][11].0 k[0][11].1 Vdd a[20] a[21] b[11][0] b[11][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][12] GND k[0][12].0 k[0][12].1 Vdd a[20] a[21] b[12][0] b[12][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][13] GND k[0][13].0 k[0][13].1 Vdd a[20] a[21] b[13][0] b[13][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][14] GND k[0][14].0 k[0][14].1 Vdd a[20] a[21] b[14][0] b[14][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][15] GND k[0][15].0 k[0][15].1 Vdd a[20] a[21] b[15][0] b[15][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][16] GND k[0][16].0 k[0][16].1 Vdd a[20] a[21] b[16][0] b[16][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][17] GND k[0][17].0 k[0][17].1 Vdd a[20] a[21] b[17][0] b[17][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][18] GND k[0][18].0 k[0][18].1 Vdd a[20] a[21] b[18][0] b[18][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[10][19] GND k[0][19].0 k[0][19].1 Vdd a[20] a[21] b[19][0] b[19][1] 
+ hit[0][10] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][0] GND k[0][0].0 k[0][0].1 Vdd a[22] a[23] b[0][0] b[0][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][1] GND k[0][1].0 k[0][1].1 Vdd a[22] a[23] b[1][0] b[1][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][2] GND k[0][2].0 k[0][2].1 Vdd a[22] a[23] b[2][0] b[2][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][3] GND k[0][3].0 k[0][3].1 Vdd a[22] a[23] b[3][0] b[3][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][4] GND k[0][4].0 k[0][4].1 Vdd a[22] a[23] b[4][0] b[4][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][5] GND k[0][5].0 k[0][5].1 Vdd a[22] a[23] b[5][0] b[5][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][6] GND k[0][6].0 k[0][6].1 Vdd a[22] a[23] b[6][0] b[6][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][7] GND k[0][7].0 k[0][7].1 Vdd a[22] a[23] b[7][0] b[7][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][8] GND k[0][8].0 k[0][8].1 Vdd a[22] a[23] b[8][0] b[8][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][9] GND k[0][9].0 k[0][9].1 Vdd a[22] a[23] b[9][0] b[9][1] hit[0][11] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][10] GND k[0][10].0 k[0][10].1 Vdd a[22] a[23] b[10][0] b[10][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][11] GND k[0][11].0 k[0][11].1 Vdd a[22] a[23] b[11][0] b[11][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][12] GND k[0][12].0 k[0][12].1 Vdd a[22] a[23] b[12][0] b[12][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][13] GND k[0][13].0 k[0][13].1 Vdd a[22] a[23] b[13][0] b[13][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][14] GND k[0][14].0 k[0][14].1 Vdd a[22] a[23] b[14][0] b[14][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][15] GND k[0][15].0 k[0][15].1 Vdd a[22] a[23] b[15][0] b[15][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][16] GND k[0][16].0 k[0][16].1 Vdd a[22] a[23] b[16][0] b[16][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][17] GND k[0][17].0 k[0][17].1 Vdd a[22] a[23] b[17][0] b[17][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][18] GND k[0][18].0 k[0][18].1 Vdd a[22] a[23] b[18][0] b[18][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[11][19] GND k[0][19].0 k[0][19].1 Vdd a[22] a[23] b[19][0] b[19][1] 
+ hit[0][11] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][0] GND k[0][0].0 k[0][0].1 Vdd a[24] a[25] b[0][0] b[0][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][1] GND k[0][1].0 k[0][1].1 Vdd a[24] a[25] b[1][0] b[1][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][2] GND k[0][2].0 k[0][2].1 Vdd a[24] a[25] b[2][0] b[2][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][3] GND k[0][3].0 k[0][3].1 Vdd a[24] a[25] b[3][0] b[3][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][4] GND k[0][4].0 k[0][4].1 Vdd a[24] a[25] b[4][0] b[4][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][5] GND k[0][5].0 k[0][5].1 Vdd a[24] a[25] b[5][0] b[5][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][6] GND k[0][6].0 k[0][6].1 Vdd a[24] a[25] b[6][0] b[6][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][7] GND k[0][7].0 k[0][7].1 Vdd a[24] a[25] b[7][0] b[7][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][8] GND k[0][8].0 k[0][8].1 Vdd a[24] a[25] b[8][0] b[8][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][9] GND k[0][9].0 k[0][9].1 Vdd a[24] a[25] b[9][0] b[9][1] hit[0][12] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][10] GND k[0][10].0 k[0][10].1 Vdd a[24] a[25] b[10][0] b[10][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][11] GND k[0][11].0 k[0][11].1 Vdd a[24] a[25] b[11][0] b[11][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][12] GND k[0][12].0 k[0][12].1 Vdd a[24] a[25] b[12][0] b[12][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][13] GND k[0][13].0 k[0][13].1 Vdd a[24] a[25] b[13][0] b[13][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][14] GND k[0][14].0 k[0][14].1 Vdd a[24] a[25] b[14][0] b[14][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][15] GND k[0][15].0 k[0][15].1 Vdd a[24] a[25] b[15][0] b[15][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][16] GND k[0][16].0 k[0][16].1 Vdd a[24] a[25] b[16][0] b[16][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][17] GND k[0][17].0 k[0][17].1 Vdd a[24] a[25] b[17][0] b[17][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][18] GND k[0][18].0 k[0][18].1 Vdd a[24] a[25] b[18][0] b[18][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[12][19] GND k[0][19].0 k[0][19].1 Vdd a[24] a[25] b[19][0] b[19][1] 
+ hit[0][12] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][0] GND k[0][0].0 k[0][0].1 Vdd a[26] a[27] b[0][0] b[0][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][1] GND k[0][1].0 k[0][1].1 Vdd a[26] a[27] b[1][0] b[1][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][2] GND k[0][2].0 k[0][2].1 Vdd a[26] a[27] b[2][0] b[2][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][3] GND k[0][3].0 k[0][3].1 Vdd a[26] a[27] b[3][0] b[3][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][4] GND k[0][4].0 k[0][4].1 Vdd a[26] a[27] b[4][0] b[4][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][5] GND k[0][5].0 k[0][5].1 Vdd a[26] a[27] b[5][0] b[5][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][6] GND k[0][6].0 k[0][6].1 Vdd a[26] a[27] b[6][0] b[6][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][7] GND k[0][7].0 k[0][7].1 Vdd a[26] a[27] b[7][0] b[7][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][8] GND k[0][8].0 k[0][8].1 Vdd a[26] a[27] b[8][0] b[8][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][9] GND k[0][9].0 k[0][9].1 Vdd a[26] a[27] b[9][0] b[9][1] hit[0][13] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][10] GND k[0][10].0 k[0][10].1 Vdd a[26] a[27] b[10][0] b[10][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][11] GND k[0][11].0 k[0][11].1 Vdd a[26] a[27] b[11][0] b[11][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][12] GND k[0][12].0 k[0][12].1 Vdd a[26] a[27] b[12][0] b[12][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][13] GND k[0][13].0 k[0][13].1 Vdd a[26] a[27] b[13][0] b[13][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][14] GND k[0][14].0 k[0][14].1 Vdd a[26] a[27] b[14][0] b[14][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][15] GND k[0][15].0 k[0][15].1 Vdd a[26] a[27] b[15][0] b[15][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][16] GND k[0][16].0 k[0][16].1 Vdd a[26] a[27] b[16][0] b[16][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][17] GND k[0][17].0 k[0][17].1 Vdd a[26] a[27] b[17][0] b[17][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][18] GND k[0][18].0 k[0][18].1 Vdd a[26] a[27] b[18][0] b[18][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[13][19] GND k[0][19].0 k[0][19].1 Vdd a[26] a[27] b[19][0] b[19][1] 
+ hit[0][13] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][0] GND k[0][0].0 k[0][0].1 Vdd a[28] a[29] b[0][0] b[0][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][1] GND k[0][1].0 k[0][1].1 Vdd a[28] a[29] b[1][0] b[1][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][2] GND k[0][2].0 k[0][2].1 Vdd a[28] a[29] b[2][0] b[2][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][3] GND k[0][3].0 k[0][3].1 Vdd a[28] a[29] b[3][0] b[3][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][4] GND k[0][4].0 k[0][4].1 Vdd a[28] a[29] b[4][0] b[4][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][5] GND k[0][5].0 k[0][5].1 Vdd a[28] a[29] b[5][0] b[5][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][6] GND k[0][6].0 k[0][6].1 Vdd a[28] a[29] b[6][0] b[6][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][7] GND k[0][7].0 k[0][7].1 Vdd a[28] a[29] b[7][0] b[7][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][8] GND k[0][8].0 k[0][8].1 Vdd a[28] a[29] b[8][0] b[8][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][9] GND k[0][9].0 k[0][9].1 Vdd a[28] a[29] b[9][0] b[9][1] hit[0][14] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][10] GND k[0][10].0 k[0][10].1 Vdd a[28] a[29] b[10][0] b[10][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][11] GND k[0][11].0 k[0][11].1 Vdd a[28] a[29] b[11][0] b[11][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][12] GND k[0][12].0 k[0][12].1 Vdd a[28] a[29] b[12][0] b[12][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][13] GND k[0][13].0 k[0][13].1 Vdd a[28] a[29] b[13][0] b[13][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][14] GND k[0][14].0 k[0][14].1 Vdd a[28] a[29] b[14][0] b[14][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][15] GND k[0][15].0 k[0][15].1 Vdd a[28] a[29] b[15][0] b[15][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][16] GND k[0][16].0 k[0][16].1 Vdd a[28] a[29] b[16][0] b[16][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][17] GND k[0][17].0 k[0][17].1 Vdd a[28] a[29] b[17][0] b[17][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][18] GND k[0][18].0 k[0][18].1 Vdd a[28] a[29] b[18][0] b[18][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[14][19] GND k[0][19].0 k[0][19].1 Vdd a[28] a[29] b[19][0] b[19][1] 
+ hit[0][14] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][0] GND k[0][0].0 k[0][0].1 Vdd a[30] a[31] b[0][0] b[0][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][1] GND k[0][1].0 k[0][1].1 Vdd a[30] a[31] b[1][0] b[1][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][2] GND k[0][2].0 k[0][2].1 Vdd a[30] a[31] b[2][0] b[2][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][3] GND k[0][3].0 k[0][3].1 Vdd a[30] a[31] b[3][0] b[3][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][4] GND k[0][4].0 k[0][4].1 Vdd a[30] a[31] b[4][0] b[4][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][5] GND k[0][5].0 k[0][5].1 Vdd a[30] a[31] b[5][0] b[5][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][6] GND k[0][6].0 k[0][6].1 Vdd a[30] a[31] b[6][0] b[6][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][7] GND k[0][7].0 k[0][7].1 Vdd a[30] a[31] b[7][0] b[7][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][8] GND k[0][8].0 k[0][8].1 Vdd a[30] a[31] b[8][0] b[8][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][9] GND k[0][9].0 k[0][9].1 Vdd a[30] a[31] b[9][0] b[9][1] hit[0][15] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][10] GND k[0][10].0 k[0][10].1 Vdd a[30] a[31] b[10][0] b[10][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][11] GND k[0][11].0 k[0][11].1 Vdd a[30] a[31] b[11][0] b[11][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][12] GND k[0][12].0 k[0][12].1 Vdd a[30] a[31] b[12][0] b[12][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][13] GND k[0][13].0 k[0][13].1 Vdd a[30] a[31] b[13][0] b[13][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][14] GND k[0][14].0 k[0][14].1 Vdd a[30] a[31] b[14][0] b[14][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][15] GND k[0][15].0 k[0][15].1 Vdd a[30] a[31] b[15][0] b[15][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][16] GND k[0][16].0 k[0][16].1 Vdd a[30] a[31] b[16][0] b[16][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][17] GND k[0][17].0 k[0][17].1 Vdd a[30] a[31] b[17][0] b[17][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][18] GND k[0][18].0 k[0][18].1 Vdd a[30] a[31] b[18][0] b[18][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[15][19] GND k[0][19].0 k[0][19].1 Vdd a[30] a[31] b[19][0] b[19][1] 
+ hit[0][15] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][0] GND k[0][0].0 k[0][0].1 Vdd a[32] a[33] b[0][0] b[0][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][1] GND k[0][1].0 k[0][1].1 Vdd a[32] a[33] b[1][0] b[1][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][2] GND k[0][2].0 k[0][2].1 Vdd a[32] a[33] b[2][0] b[2][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][3] GND k[0][3].0 k[0][3].1 Vdd a[32] a[33] b[3][0] b[3][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][4] GND k[0][4].0 k[0][4].1 Vdd a[32] a[33] b[4][0] b[4][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][5] GND k[0][5].0 k[0][5].1 Vdd a[32] a[33] b[5][0] b[5][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][6] GND k[0][6].0 k[0][6].1 Vdd a[32] a[33] b[6][0] b[6][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][7] GND k[0][7].0 k[0][7].1 Vdd a[32] a[33] b[7][0] b[7][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][8] GND k[0][8].0 k[0][8].1 Vdd a[32] a[33] b[8][0] b[8][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][9] GND k[0][9].0 k[0][9].1 Vdd a[32] a[33] b[9][0] b[9][1] hit[0][16] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][10] GND k[0][10].0 k[0][10].1 Vdd a[32] a[33] b[10][0] b[10][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][11] GND k[0][11].0 k[0][11].1 Vdd a[32] a[33] b[11][0] b[11][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][12] GND k[0][12].0 k[0][12].1 Vdd a[32] a[33] b[12][0] b[12][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][13] GND k[0][13].0 k[0][13].1 Vdd a[32] a[33] b[13][0] b[13][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][14] GND k[0][14].0 k[0][14].1 Vdd a[32] a[33] b[14][0] b[14][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][15] GND k[0][15].0 k[0][15].1 Vdd a[32] a[33] b[15][0] b[15][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][16] GND k[0][16].0 k[0][16].1 Vdd a[32] a[33] b[16][0] b[16][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][17] GND k[0][17].0 k[0][17].1 Vdd a[32] a[33] b[17][0] b[17][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][18] GND k[0][18].0 k[0][18].1 Vdd a[32] a[33] b[18][0] b[18][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[16][19] GND k[0][19].0 k[0][19].1 Vdd a[32] a[33] b[19][0] b[19][1] 
+ hit[0][16] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][0] GND k[0][0].0 k[0][0].1 Vdd a[34] a[35] b[0][0] b[0][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][1] GND k[0][1].0 k[0][1].1 Vdd a[34] a[35] b[1][0] b[1][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][2] GND k[0][2].0 k[0][2].1 Vdd a[34] a[35] b[2][0] b[2][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][3] GND k[0][3].0 k[0][3].1 Vdd a[34] a[35] b[3][0] b[3][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][4] GND k[0][4].0 k[0][4].1 Vdd a[34] a[35] b[4][0] b[4][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][5] GND k[0][5].0 k[0][5].1 Vdd a[34] a[35] b[5][0] b[5][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][6] GND k[0][6].0 k[0][6].1 Vdd a[34] a[35] b[6][0] b[6][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][7] GND k[0][7].0 k[0][7].1 Vdd a[34] a[35] b[7][0] b[7][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][8] GND k[0][8].0 k[0][8].1 Vdd a[34] a[35] b[8][0] b[8][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][9] GND k[0][9].0 k[0][9].1 Vdd a[34] a[35] b[9][0] b[9][1] hit[0][17] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][10] GND k[0][10].0 k[0][10].1 Vdd a[34] a[35] b[10][0] b[10][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][11] GND k[0][11].0 k[0][11].1 Vdd a[34] a[35] b[11][0] b[11][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][12] GND k[0][12].0 k[0][12].1 Vdd a[34] a[35] b[12][0] b[12][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][13] GND k[0][13].0 k[0][13].1 Vdd a[34] a[35] b[13][0] b[13][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][14] GND k[0][14].0 k[0][14].1 Vdd a[34] a[35] b[14][0] b[14][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][15] GND k[0][15].0 k[0][15].1 Vdd a[34] a[35] b[15][0] b[15][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][16] GND k[0][16].0 k[0][16].1 Vdd a[34] a[35] b[16][0] b[16][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][17] GND k[0][17].0 k[0][17].1 Vdd a[34] a[35] b[17][0] b[17][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][18] GND k[0][18].0 k[0][18].1 Vdd a[34] a[35] b[18][0] b[18][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[17][19] GND k[0][19].0 k[0][19].1 Vdd a[34] a[35] b[19][0] b[19][1] 
+ hit[0][17] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][0] GND k[0][0].0 k[0][0].1 Vdd a[36] a[37] b[0][0] b[0][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][1] GND k[0][1].0 k[0][1].1 Vdd a[36] a[37] b[1][0] b[1][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][2] GND k[0][2].0 k[0][2].1 Vdd a[36] a[37] b[2][0] b[2][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][3] GND k[0][3].0 k[0][3].1 Vdd a[36] a[37] b[3][0] b[3][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][4] GND k[0][4].0 k[0][4].1 Vdd a[36] a[37] b[4][0] b[4][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][5] GND k[0][5].0 k[0][5].1 Vdd a[36] a[37] b[5][0] b[5][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][6] GND k[0][6].0 k[0][6].1 Vdd a[36] a[37] b[6][0] b[6][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][7] GND k[0][7].0 k[0][7].1 Vdd a[36] a[37] b[7][0] b[7][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][8] GND k[0][8].0 k[0][8].1 Vdd a[36] a[37] b[8][0] b[8][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][9] GND k[0][9].0 k[0][9].1 Vdd a[36] a[37] b[9][0] b[9][1] hit[0][18] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][10] GND k[0][10].0 k[0][10].1 Vdd a[36] a[37] b[10][0] b[10][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][11] GND k[0][11].0 k[0][11].1 Vdd a[36] a[37] b[11][0] b[11][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][12] GND k[0][12].0 k[0][12].1 Vdd a[36] a[37] b[12][0] b[12][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][13] GND k[0][13].0 k[0][13].1 Vdd a[36] a[37] b[13][0] b[13][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][14] GND k[0][14].0 k[0][14].1 Vdd a[36] a[37] b[14][0] b[14][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][15] GND k[0][15].0 k[0][15].1 Vdd a[36] a[37] b[15][0] b[15][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][16] GND k[0][16].0 k[0][16].1 Vdd a[36] a[37] b[16][0] b[16][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][17] GND k[0][17].0 k[0][17].1 Vdd a[36] a[37] b[17][0] b[17][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][18] GND k[0][18].0 k[0][18].1 Vdd a[36] a[37] b[18][0] b[18][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[18][19] GND k[0][19].0 k[0][19].1 Vdd a[36] a[37] b[19][0] b[19][1] 
+ hit[0][18] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][0] GND k[0][0].0 k[0][0].1 Vdd a[38] a[39] b[0][0] b[0][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][1] GND k[0][1].0 k[0][1].1 Vdd a[38] a[39] b[1][0] b[1][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][2] GND k[0][2].0 k[0][2].1 Vdd a[38] a[39] b[2][0] b[2][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][3] GND k[0][3].0 k[0][3].1 Vdd a[38] a[39] b[3][0] b[3][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][4] GND k[0][4].0 k[0][4].1 Vdd a[38] a[39] b[4][0] b[4][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][5] GND k[0][5].0 k[0][5].1 Vdd a[38] a[39] b[5][0] b[5][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][6] GND k[0][6].0 k[0][6].1 Vdd a[38] a[39] b[6][0] b[6][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][7] GND k[0][7].0 k[0][7].1 Vdd a[38] a[39] b[7][0] b[7][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][8] GND k[0][8].0 k[0][8].1 Vdd a[38] a[39] b[8][0] b[8][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][9] GND k[0][9].0 k[0][9].1 Vdd a[38] a[39] b[9][0] b[9][1] hit[0][19] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][10] GND k[0][10].0 k[0][10].1 Vdd a[38] a[39] b[10][0] b[10][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][11] GND k[0][11].0 k[0][11].1 Vdd a[38] a[39] b[11][0] b[11][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][12] GND k[0][12].0 k[0][12].1 Vdd a[38] a[39] b[12][0] b[12][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][13] GND k[0][13].0 k[0][13].1 Vdd a[38] a[39] b[13][0] b[13][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][14] GND k[0][14].0 k[0][14].1 Vdd a[38] a[39] b[14][0] b[14][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][15] GND k[0][15].0 k[0][15].1 Vdd a[38] a[39] b[15][0] b[15][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][16] GND k[0][16].0 k[0][16].1 Vdd a[38] a[39] b[16][0] b[16][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][17] GND k[0][17].0 k[0][17].1 Vdd a[38] a[39] b[17][0] b[17][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][18] GND k[0][18].0 k[0][18].1 Vdd a[38] a[39] b[18][0] b[18][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[19][19] GND k[0][19].0 k[0][19].1 Vdd a[38] a[39] b[19][0] b[19][1] 
+ hit[0][19] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][0] GND k[0][0].0 k[0][0].1 Vdd a[40] a[41] b[0][0] b[0][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][1] GND k[0][1].0 k[0][1].1 Vdd a[40] a[41] b[1][0] b[1][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][2] GND k[0][2].0 k[0][2].1 Vdd a[40] a[41] b[2][0] b[2][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][3] GND k[0][3].0 k[0][3].1 Vdd a[40] a[41] b[3][0] b[3][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][4] GND k[0][4].0 k[0][4].1 Vdd a[40] a[41] b[4][0] b[4][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][5] GND k[0][5].0 k[0][5].1 Vdd a[40] a[41] b[5][0] b[5][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][6] GND k[0][6].0 k[0][6].1 Vdd a[40] a[41] b[6][0] b[6][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][7] GND k[0][7].0 k[0][7].1 Vdd a[40] a[41] b[7][0] b[7][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][8] GND k[0][8].0 k[0][8].1 Vdd a[40] a[41] b[8][0] b[8][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][9] GND k[0][9].0 k[0][9].1 Vdd a[40] a[41] b[9][0] b[9][1] hit[0][20] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][10] GND k[0][10].0 k[0][10].1 Vdd a[40] a[41] b[10][0] b[10][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][11] GND k[0][11].0 k[0][11].1 Vdd a[40] a[41] b[11][0] b[11][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][12] GND k[0][12].0 k[0][12].1 Vdd a[40] a[41] b[12][0] b[12][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][13] GND k[0][13].0 k[0][13].1 Vdd a[40] a[41] b[13][0] b[13][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][14] GND k[0][14].0 k[0][14].1 Vdd a[40] a[41] b[14][0] b[14][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][15] GND k[0][15].0 k[0][15].1 Vdd a[40] a[41] b[15][0] b[15][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][16] GND k[0][16].0 k[0][16].1 Vdd a[40] a[41] b[16][0] b[16][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][17] GND k[0][17].0 k[0][17].1 Vdd a[40] a[41] b[17][0] b[17][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][18] GND k[0][18].0 k[0][18].1 Vdd a[40] a[41] b[18][0] b[18][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[20][19] GND k[0][19].0 k[0][19].1 Vdd a[40] a[41] b[19][0] b[19][1] 
+ hit[0][20] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][0] GND k[0][0].0 k[0][0].1 Vdd a[42] a[43] b[0][0] b[0][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][1] GND k[0][1].0 k[0][1].1 Vdd a[42] a[43] b[1][0] b[1][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][2] GND k[0][2].0 k[0][2].1 Vdd a[42] a[43] b[2][0] b[2][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][3] GND k[0][3].0 k[0][3].1 Vdd a[42] a[43] b[3][0] b[3][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][4] GND k[0][4].0 k[0][4].1 Vdd a[42] a[43] b[4][0] b[4][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][5] GND k[0][5].0 k[0][5].1 Vdd a[42] a[43] b[5][0] b[5][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][6] GND k[0][6].0 k[0][6].1 Vdd a[42] a[43] b[6][0] b[6][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][7] GND k[0][7].0 k[0][7].1 Vdd a[42] a[43] b[7][0] b[7][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][8] GND k[0][8].0 k[0][8].1 Vdd a[42] a[43] b[8][0] b[8][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][9] GND k[0][9].0 k[0][9].1 Vdd a[42] a[43] b[9][0] b[9][1] hit[0][21] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][10] GND k[0][10].0 k[0][10].1 Vdd a[42] a[43] b[10][0] b[10][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][11] GND k[0][11].0 k[0][11].1 Vdd a[42] a[43] b[11][0] b[11][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][12] GND k[0][12].0 k[0][12].1 Vdd a[42] a[43] b[12][0] b[12][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][13] GND k[0][13].0 k[0][13].1 Vdd a[42] a[43] b[13][0] b[13][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][14] GND k[0][14].0 k[0][14].1 Vdd a[42] a[43] b[14][0] b[14][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][15] GND k[0][15].0 k[0][15].1 Vdd a[42] a[43] b[15][0] b[15][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][16] GND k[0][16].0 k[0][16].1 Vdd a[42] a[43] b[16][0] b[16][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][17] GND k[0][17].0 k[0][17].1 Vdd a[42] a[43] b[17][0] b[17][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][18] GND k[0][18].0 k[0][18].1 Vdd a[42] a[43] b[18][0] b[18][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[21][19] GND k[0][19].0 k[0][19].1 Vdd a[42] a[43] b[19][0] b[19][1] 
+ hit[0][21] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][0] GND k[0][0].0 k[0][0].1 Vdd a[44] a[45] b[0][0] b[0][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][1] GND k[0][1].0 k[0][1].1 Vdd a[44] a[45] b[1][0] b[1][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][2] GND k[0][2].0 k[0][2].1 Vdd a[44] a[45] b[2][0] b[2][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][3] GND k[0][3].0 k[0][3].1 Vdd a[44] a[45] b[3][0] b[3][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][4] GND k[0][4].0 k[0][4].1 Vdd a[44] a[45] b[4][0] b[4][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][5] GND k[0][5].0 k[0][5].1 Vdd a[44] a[45] b[5][0] b[5][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][6] GND k[0][6].0 k[0][6].1 Vdd a[44] a[45] b[6][0] b[6][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][7] GND k[0][7].0 k[0][7].1 Vdd a[44] a[45] b[7][0] b[7][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][8] GND k[0][8].0 k[0][8].1 Vdd a[44] a[45] b[8][0] b[8][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][9] GND k[0][9].0 k[0][9].1 Vdd a[44] a[45] b[9][0] b[9][1] hit[0][22] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][10] GND k[0][10].0 k[0][10].1 Vdd a[44] a[45] b[10][0] b[10][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][11] GND k[0][11].0 k[0][11].1 Vdd a[44] a[45] b[11][0] b[11][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][12] GND k[0][12].0 k[0][12].1 Vdd a[44] a[45] b[12][0] b[12][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][13] GND k[0][13].0 k[0][13].1 Vdd a[44] a[45] b[13][0] b[13][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][14] GND k[0][14].0 k[0][14].1 Vdd a[44] a[45] b[14][0] b[14][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][15] GND k[0][15].0 k[0][15].1 Vdd a[44] a[45] b[15][0] b[15][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][16] GND k[0][16].0 k[0][16].1 Vdd a[44] a[45] b[16][0] b[16][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][17] GND k[0][17].0 k[0][17].1 Vdd a[44] a[45] b[17][0] b[17][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][18] GND k[0][18].0 k[0][18].1 Vdd a[44] a[45] b[18][0] b[18][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[22][19] GND k[0][19].0 k[0][19].1 Vdd a[44] a[45] b[19][0] b[19][1] 
+ hit[0][22] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][0] GND k[0][0].0 k[0][0].1 Vdd a[46] a[47] b[0][0] b[0][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][1] GND k[0][1].0 k[0][1].1 Vdd a[46] a[47] b[1][0] b[1][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][2] GND k[0][2].0 k[0][2].1 Vdd a[46] a[47] b[2][0] b[2][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][3] GND k[0][3].0 k[0][3].1 Vdd a[46] a[47] b[3][0] b[3][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][4] GND k[0][4].0 k[0][4].1 Vdd a[46] a[47] b[4][0] b[4][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][5] GND k[0][5].0 k[0][5].1 Vdd a[46] a[47] b[5][0] b[5][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][6] GND k[0][6].0 k[0][6].1 Vdd a[46] a[47] b[6][0] b[6][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][7] GND k[0][7].0 k[0][7].1 Vdd a[46] a[47] b[7][0] b[7][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][8] GND k[0][8].0 k[0][8].1 Vdd a[46] a[47] b[8][0] b[8][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][9] GND k[0][9].0 k[0][9].1 Vdd a[46] a[47] b[9][0] b[9][1] hit[0][23] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][10] GND k[0][10].0 k[0][10].1 Vdd a[46] a[47] b[10][0] b[10][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][11] GND k[0][11].0 k[0][11].1 Vdd a[46] a[47] b[11][0] b[11][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][12] GND k[0][12].0 k[0][12].1 Vdd a[46] a[47] b[12][0] b[12][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][13] GND k[0][13].0 k[0][13].1 Vdd a[46] a[47] b[13][0] b[13][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][14] GND k[0][14].0 k[0][14].1 Vdd a[46] a[47] b[14][0] b[14][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][15] GND k[0][15].0 k[0][15].1 Vdd a[46] a[47] b[15][0] b[15][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][16] GND k[0][16].0 k[0][16].1 Vdd a[46] a[47] b[16][0] b[16][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][17] GND k[0][17].0 k[0][17].1 Vdd a[46] a[47] b[17][0] b[17][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][18] GND k[0][18].0 k[0][18].1 Vdd a[46] a[47] b[18][0] b[18][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[23][19] GND k[0][19].0 k[0][19].1 Vdd a[46] a[47] b[19][0] b[19][1] 
+ hit[0][23] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][0] GND k[0][0].0 k[0][0].1 Vdd a[48] a[49] b[0][0] b[0][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][1] GND k[0][1].0 k[0][1].1 Vdd a[48] a[49] b[1][0] b[1][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][2] GND k[0][2].0 k[0][2].1 Vdd a[48] a[49] b[2][0] b[2][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][3] GND k[0][3].0 k[0][3].1 Vdd a[48] a[49] b[3][0] b[3][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][4] GND k[0][4].0 k[0][4].1 Vdd a[48] a[49] b[4][0] b[4][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][5] GND k[0][5].0 k[0][5].1 Vdd a[48] a[49] b[5][0] b[5][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][6] GND k[0][6].0 k[0][6].1 Vdd a[48] a[49] b[6][0] b[6][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][7] GND k[0][7].0 k[0][7].1 Vdd a[48] a[49] b[7][0] b[7][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][8] GND k[0][8].0 k[0][8].1 Vdd a[48] a[49] b[8][0] b[8][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][9] GND k[0][9].0 k[0][9].1 Vdd a[48] a[49] b[9][0] b[9][1] hit[0][24] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][10] GND k[0][10].0 k[0][10].1 Vdd a[48] a[49] b[10][0] b[10][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][11] GND k[0][11].0 k[0][11].1 Vdd a[48] a[49] b[11][0] b[11][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][12] GND k[0][12].0 k[0][12].1 Vdd a[48] a[49] b[12][0] b[12][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][13] GND k[0][13].0 k[0][13].1 Vdd a[48] a[49] b[13][0] b[13][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][14] GND k[0][14].0 k[0][14].1 Vdd a[48] a[49] b[14][0] b[14][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][15] GND k[0][15].0 k[0][15].1 Vdd a[48] a[49] b[15][0] b[15][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][16] GND k[0][16].0 k[0][16].1 Vdd a[48] a[49] b[16][0] b[16][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][17] GND k[0][17].0 k[0][17].1 Vdd a[48] a[49] b[17][0] b[17][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][18] GND k[0][18].0 k[0][18].1 Vdd a[48] a[49] b[18][0] b[18][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[24][19] GND k[0][19].0 k[0][19].1 Vdd a[48] a[49] b[19][0] b[19][1] 
+ hit[0][24] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][0] GND k[0][0].0 k[0][0].1 Vdd a[50] a[51] b[0][0] b[0][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][1] GND k[0][1].0 k[0][1].1 Vdd a[50] a[51] b[1][0] b[1][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][2] GND k[0][2].0 k[0][2].1 Vdd a[50] a[51] b[2][0] b[2][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][3] GND k[0][3].0 k[0][3].1 Vdd a[50] a[51] b[3][0] b[3][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][4] GND k[0][4].0 k[0][4].1 Vdd a[50] a[51] b[4][0] b[4][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][5] GND k[0][5].0 k[0][5].1 Vdd a[50] a[51] b[5][0] b[5][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][6] GND k[0][6].0 k[0][6].1 Vdd a[50] a[51] b[6][0] b[6][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][7] GND k[0][7].0 k[0][7].1 Vdd a[50] a[51] b[7][0] b[7][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][8] GND k[0][8].0 k[0][8].1 Vdd a[50] a[51] b[8][0] b[8][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][9] GND k[0][9].0 k[0][9].1 Vdd a[50] a[51] b[9][0] b[9][1] hit[0][25] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][10] GND k[0][10].0 k[0][10].1 Vdd a[50] a[51] b[10][0] b[10][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][11] GND k[0][11].0 k[0][11].1 Vdd a[50] a[51] b[11][0] b[11][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][12] GND k[0][12].0 k[0][12].1 Vdd a[50] a[51] b[12][0] b[12][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][13] GND k[0][13].0 k[0][13].1 Vdd a[50] a[51] b[13][0] b[13][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][14] GND k[0][14].0 k[0][14].1 Vdd a[50] a[51] b[14][0] b[14][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][15] GND k[0][15].0 k[0][15].1 Vdd a[50] a[51] b[15][0] b[15][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][16] GND k[0][16].0 k[0][16].1 Vdd a[50] a[51] b[16][0] b[16][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][17] GND k[0][17].0 k[0][17].1 Vdd a[50] a[51] b[17][0] b[17][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][18] GND k[0][18].0 k[0][18].1 Vdd a[50] a[51] b[18][0] b[18][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[25][19] GND k[0][19].0 k[0][19].1 Vdd a[50] a[51] b[19][0] b[19][1] 
+ hit[0][25] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][0] GND k[0][0].0 k[0][0].1 Vdd a[52] a[53] b[0][0] b[0][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][1] GND k[0][1].0 k[0][1].1 Vdd a[52] a[53] b[1][0] b[1][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][2] GND k[0][2].0 k[0][2].1 Vdd a[52] a[53] b[2][0] b[2][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][3] GND k[0][3].0 k[0][3].1 Vdd a[52] a[53] b[3][0] b[3][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][4] GND k[0][4].0 k[0][4].1 Vdd a[52] a[53] b[4][0] b[4][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][5] GND k[0][5].0 k[0][5].1 Vdd a[52] a[53] b[5][0] b[5][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][6] GND k[0][6].0 k[0][6].1 Vdd a[52] a[53] b[6][0] b[6][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][7] GND k[0][7].0 k[0][7].1 Vdd a[52] a[53] b[7][0] b[7][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][8] GND k[0][8].0 k[0][8].1 Vdd a[52] a[53] b[8][0] b[8][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][9] GND k[0][9].0 k[0][9].1 Vdd a[52] a[53] b[9][0] b[9][1] hit[0][26] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][10] GND k[0][10].0 k[0][10].1 Vdd a[52] a[53] b[10][0] b[10][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][11] GND k[0][11].0 k[0][11].1 Vdd a[52] a[53] b[11][0] b[11][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][12] GND k[0][12].0 k[0][12].1 Vdd a[52] a[53] b[12][0] b[12][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][13] GND k[0][13].0 k[0][13].1 Vdd a[52] a[53] b[13][0] b[13][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][14] GND k[0][14].0 k[0][14].1 Vdd a[52] a[53] b[14][0] b[14][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][15] GND k[0][15].0 k[0][15].1 Vdd a[52] a[53] b[15][0] b[15][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][16] GND k[0][16].0 k[0][16].1 Vdd a[52] a[53] b[16][0] b[16][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][17] GND k[0][17].0 k[0][17].1 Vdd a[52] a[53] b[17][0] b[17][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][18] GND k[0][18].0 k[0][18].1 Vdd a[52] a[53] b[18][0] b[18][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[26][19] GND k[0][19].0 k[0][19].1 Vdd a[52] a[53] b[19][0] b[19][1] 
+ hit[0][26] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][0] GND k[0][0].0 k[0][0].1 Vdd a[54] a[55] b[0][0] b[0][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][1] GND k[0][1].0 k[0][1].1 Vdd a[54] a[55] b[1][0] b[1][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][2] GND k[0][2].0 k[0][2].1 Vdd a[54] a[55] b[2][0] b[2][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][3] GND k[0][3].0 k[0][3].1 Vdd a[54] a[55] b[3][0] b[3][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][4] GND k[0][4].0 k[0][4].1 Vdd a[54] a[55] b[4][0] b[4][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][5] GND k[0][5].0 k[0][5].1 Vdd a[54] a[55] b[5][0] b[5][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][6] GND k[0][6].0 k[0][6].1 Vdd a[54] a[55] b[6][0] b[6][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][7] GND k[0][7].0 k[0][7].1 Vdd a[54] a[55] b[7][0] b[7][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][8] GND k[0][8].0 k[0][8].1 Vdd a[54] a[55] b[8][0] b[8][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][9] GND k[0][9].0 k[0][9].1 Vdd a[54] a[55] b[9][0] b[9][1] hit[0][27] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][10] GND k[0][10].0 k[0][10].1 Vdd a[54] a[55] b[10][0] b[10][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][11] GND k[0][11].0 k[0][11].1 Vdd a[54] a[55] b[11][0] b[11][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][12] GND k[0][12].0 k[0][12].1 Vdd a[54] a[55] b[12][0] b[12][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][13] GND k[0][13].0 k[0][13].1 Vdd a[54] a[55] b[13][0] b[13][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][14] GND k[0][14].0 k[0][14].1 Vdd a[54] a[55] b[14][0] b[14][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][15] GND k[0][15].0 k[0][15].1 Vdd a[54] a[55] b[15][0] b[15][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][16] GND k[0][16].0 k[0][16].1 Vdd a[54] a[55] b[16][0] b[16][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][17] GND k[0][17].0 k[0][17].1 Vdd a[54] a[55] b[17][0] b[17][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][18] GND k[0][18].0 k[0][18].1 Vdd a[54] a[55] b[18][0] b[18][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[27][19] GND k[0][19].0 k[0][19].1 Vdd a[54] a[55] b[19][0] b[19][1] 
+ hit[0][27] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][0] GND k[0][0].0 k[0][0].1 Vdd a[56] a[57] b[0][0] b[0][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][1] GND k[0][1].0 k[0][1].1 Vdd a[56] a[57] b[1][0] b[1][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][2] GND k[0][2].0 k[0][2].1 Vdd a[56] a[57] b[2][0] b[2][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][3] GND k[0][3].0 k[0][3].1 Vdd a[56] a[57] b[3][0] b[3][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][4] GND k[0][4].0 k[0][4].1 Vdd a[56] a[57] b[4][0] b[4][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][5] GND k[0][5].0 k[0][5].1 Vdd a[56] a[57] b[5][0] b[5][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][6] GND k[0][6].0 k[0][6].1 Vdd a[56] a[57] b[6][0] b[6][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][7] GND k[0][7].0 k[0][7].1 Vdd a[56] a[57] b[7][0] b[7][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][8] GND k[0][8].0 k[0][8].1 Vdd a[56] a[57] b[8][0] b[8][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][9] GND k[0][9].0 k[0][9].1 Vdd a[56] a[57] b[9][0] b[9][1] hit[0][28] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][10] GND k[0][10].0 k[0][10].1 Vdd a[56] a[57] b[10][0] b[10][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][11] GND k[0][11].0 k[0][11].1 Vdd a[56] a[57] b[11][0] b[11][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][12] GND k[0][12].0 k[0][12].1 Vdd a[56] a[57] b[12][0] b[12][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][13] GND k[0][13].0 k[0][13].1 Vdd a[56] a[57] b[13][0] b[13][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][14] GND k[0][14].0 k[0][14].1 Vdd a[56] a[57] b[14][0] b[14][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][15] GND k[0][15].0 k[0][15].1 Vdd a[56] a[57] b[15][0] b[15][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][16] GND k[0][16].0 k[0][16].1 Vdd a[56] a[57] b[16][0] b[16][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][17] GND k[0][17].0 k[0][17].1 Vdd a[56] a[57] b[17][0] b[17][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][18] GND k[0][18].0 k[0][18].1 Vdd a[56] a[57] b[18][0] b[18][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[28][19] GND k[0][19].0 k[0][19].1 Vdd a[56] a[57] b[19][0] b[19][1] 
+ hit[0][28] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][0] GND k[0][0].0 k[0][0].1 Vdd a[58] a[59] b[0][0] b[0][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][1] GND k[0][1].0 k[0][1].1 Vdd a[58] a[59] b[1][0] b[1][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][2] GND k[0][2].0 k[0][2].1 Vdd a[58] a[59] b[2][0] b[2][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][3] GND k[0][3].0 k[0][3].1 Vdd a[58] a[59] b[3][0] b[3][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][4] GND k[0][4].0 k[0][4].1 Vdd a[58] a[59] b[4][0] b[4][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][5] GND k[0][5].0 k[0][5].1 Vdd a[58] a[59] b[5][0] b[5][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][6] GND k[0][6].0 k[0][6].1 Vdd a[58] a[59] b[6][0] b[6][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][7] GND k[0][7].0 k[0][7].1 Vdd a[58] a[59] b[7][0] b[7][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][8] GND k[0][8].0 k[0][8].1 Vdd a[58] a[59] b[8][0] b[8][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][9] GND k[0][9].0 k[0][9].1 Vdd a[58] a[59] b[9][0] b[9][1] hit[0][29] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][10] GND k[0][10].0 k[0][10].1 Vdd a[58] a[59] b[10][0] b[10][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][11] GND k[0][11].0 k[0][11].1 Vdd a[58] a[59] b[11][0] b[11][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][12] GND k[0][12].0 k[0][12].1 Vdd a[58] a[59] b[12][0] b[12][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][13] GND k[0][13].0 k[0][13].1 Vdd a[58] a[59] b[13][0] b[13][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][14] GND k[0][14].0 k[0][14].1 Vdd a[58] a[59] b[14][0] b[14][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][15] GND k[0][15].0 k[0][15].1 Vdd a[58] a[59] b[15][0] b[15][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][16] GND k[0][16].0 k[0][16].1 Vdd a[58] a[59] b[16][0] b[16][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][17] GND k[0][17].0 k[0][17].1 Vdd a[58] a[59] b[17][0] b[17][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][18] GND k[0][18].0 k[0][18].1 Vdd a[58] a[59] b[18][0] b[18][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[29][19] GND k[0][19].0 k[0][19].1 Vdd a[58] a[59] b[19][0] b[19][1] 
+ hit[0][29] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][0] GND k[0][0].0 k[0][0].1 Vdd a[60] a[61] b[0][0] b[0][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][1] GND k[0][1].0 k[0][1].1 Vdd a[60] a[61] b[1][0] b[1][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][2] GND k[0][2].0 k[0][2].1 Vdd a[60] a[61] b[2][0] b[2][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][3] GND k[0][3].0 k[0][3].1 Vdd a[60] a[61] b[3][0] b[3][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][4] GND k[0][4].0 k[0][4].1 Vdd a[60] a[61] b[4][0] b[4][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][5] GND k[0][5].0 k[0][5].1 Vdd a[60] a[61] b[5][0] b[5][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][6] GND k[0][6].0 k[0][6].1 Vdd a[60] a[61] b[6][0] b[6][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][7] GND k[0][7].0 k[0][7].1 Vdd a[60] a[61] b[7][0] b[7][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][8] GND k[0][8].0 k[0][8].1 Vdd a[60] a[61] b[8][0] b[8][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][9] GND k[0][9].0 k[0][9].1 Vdd a[60] a[61] b[9][0] b[9][1] hit[0][30] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][10] GND k[0][10].0 k[0][10].1 Vdd a[60] a[61] b[10][0] b[10][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][11] GND k[0][11].0 k[0][11].1 Vdd a[60] a[61] b[11][0] b[11][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][12] GND k[0][12].0 k[0][12].1 Vdd a[60] a[61] b[12][0] b[12][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][13] GND k[0][13].0 k[0][13].1 Vdd a[60] a[61] b[13][0] b[13][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][14] GND k[0][14].0 k[0][14].1 Vdd a[60] a[61] b[14][0] b[14][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][15] GND k[0][15].0 k[0][15].1 Vdd a[60] a[61] b[15][0] b[15][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][16] GND k[0][16].0 k[0][16].1 Vdd a[60] a[61] b[16][0] b[16][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][17] GND k[0][17].0 k[0][17].1 Vdd a[60] a[61] b[17][0] b[17][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][18] GND k[0][18].0 k[0][18].1 Vdd a[60] a[61] b[18][0] b[18][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[30][19] GND k[0][19].0 k[0][19].1 Vdd a[60] a[61] b[19][0] b[19][1] 
+ hit[0][30] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][0] GND k[0][0].0 k[0][0].1 Vdd a[62] a[63] b[0][0] b[0][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][1] GND k[0][1].0 k[0][1].1 Vdd a[62] a[63] b[1][0] b[1][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][2] GND k[0][2].0 k[0][2].1 Vdd a[62] a[63] b[2][0] b[2][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][3] GND k[0][3].0 k[0][3].1 Vdd a[62] a[63] b[3][0] b[3][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][4] GND k[0][4].0 k[0][4].1 Vdd a[62] a[63] b[4][0] b[4][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][5] GND k[0][5].0 k[0][5].1 Vdd a[62] a[63] b[5][0] b[5][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][6] GND k[0][6].0 k[0][6].1 Vdd a[62] a[63] b[6][0] b[6][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][7] GND k[0][7].0 k[0][7].1 Vdd a[62] a[63] b[7][0] b[7][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][8] GND k[0][8].0 k[0][8].1 Vdd a[62] a[63] b[8][0] b[8][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][9] GND k[0][9].0 k[0][9].1 Vdd a[62] a[63] b[9][0] b[9][1] hit[0][31] / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][10] GND k[0][10].0 k[0][10].1 Vdd a[62] a[63] b[10][0] b[10][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][11] GND k[0][11].0 k[0][11].1 Vdd a[62] a[63] b[11][0] b[11][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][12] GND k[0][12].0 k[0][12].1 Vdd a[62] a[63] b[12][0] b[12][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][13] GND k[0][13].0 k[0][13].1 Vdd a[62] a[63] b[13][0] b[13][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][14] GND k[0][14].0 k[0][14].1 Vdd a[62] a[63] b[14][0] b[14][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][15] GND k[0][15].0 k[0][15].1 Vdd a[62] a[63] b[15][0] b[15][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][16] GND k[0][16].0 k[0][16].1 Vdd a[62] a[63] b[16][0] b[16][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][17] GND k[0][17].0 k[0][17].1 Vdd a[62] a[63] b[17][0] b[17][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][18] GND k[0][18].0 k[0][18].1 Vdd a[62] a[63] b[18][0] b[18][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
Xz[31][19] GND k[0][19].0 k[0][19].1 Vdd a[62] a[63] b[19][0] b[19][1] 
+ hit[0][31] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_lookup_bit_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_lookup_bit_4000 GND K.0 K.1 LK Vdd W.0 W.1
*.PININFO GND:I K.0:I K.1:I LK:I Vdd:I W.0:I W.1:I
MK.0.0 K.0 _K.0 GND GND n W=2.1E-07 L=2.8E-08 m=1
MK.1.0 K.1 _K.1 GND GND n W=2.1E-07 L=2.8E-08 m=1
MK.0.1 K.0 _K.0 Vdd Vdd puv1 W=3.78E-07 L=2.8E-08 m=1
MK.1.1 K.1 _K.1 Vdd Vdd puv1 W=3.78E-07 L=2.8E-08 m=1
M_K.0.1 _K.0 W.0 _K.0.1# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_K.0.0 _K.0.1# LK GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_K.1.0 _K.1.1# LK GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_K.1.1 _K.1 W.1 _K.1.1# GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_K.0.2 _K.0 LK Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_K.0.3 _K.0 W.0 Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_K.1.2 _K.1 LK Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_K.1.3 _K.1 W.1 Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_read_bit_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_read_bit_4000 B[0][0] B[0][1] B[1][0] B[1][1] GND LR.0 LR.1 RR.0 
+ RR.1 Vdd
*.PININFO B[0][1]:I B[1][0]:I B[1][1]:I GND:I LR.0:I LR.1:I RR.0:I RR.1:I 
*.PININFO Vdd:I B[0][0]:O
MRR.0.0 RR.0 _y.0 GND GND n W=1.26E-07 L=2.8E-08 m=1
MRR.1.0 RR.1 _y.1 GND GND n W=1.26E-07 L=2.8E-08 m=1
Mx.0.0 x.0.1# B[0][1] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mx.0.1 x.0 B[1][1] x.0.1# GND n W=8.4E-08 L=2.8E-08 m=1
Mx.1.0 x.1.1# B[0][0] GND GND n W=8.4E-08 L=2.8E-08 m=1
Mx.1.1 x.1 B[1][0] x.1.1# GND n W=8.4E-08 L=2.8E-08 m=1
MRR.0.1 RR.0 _y.0 Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
MRR.1.1 RR.1 _y.1 Vdd Vdd puv1 W=1.26E-07 L=2.8E-08 m=1
Mx.0.2 x.0 B[0][1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.0.3 x.0 B[1][1] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1.2 x.1 B[0][0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
Mx.1.3 x.1 B[1][0] Vdd Vdd puv1 W=8.4E-08 L=2.8E-08 m=1
M_y.0.1 _y.0 x.0 _y.0.1# Vdd p W=8.4E-08 L=2.8E-08 m=1
M_y.0.0 _y.0.1# LR.0 Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_y.1.0 _y.1.1# LR.1 Vdd Vdd p W=8.4E-08 L=2.8E-08 m=1
M_y.1.1 _y.1 x.1 _y.1.1# Vdd p W=8.4E-08 L=2.8E-08 m=1
M_y.0.2 _y.0 LR.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_y.0.3 _y.0 x.0 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_y.1.2 _y.1 LR.1 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
M_y.1.3 _y.1 x.1 GND GND nuv1 W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_write_bit_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_write_bit_4000 B[0] B[1] GND S Vdd W.0 W.1
*.PININFO B[1]:I GND:I S:I Vdd:I W.0:I W.1:I B[0]:O
Mchain_0.0 chain_0.9# W.1 B[0] GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_0.1 GND S chain_0.9# GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_3.0 chain_3.9# W.0 B[1] GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_3.1 GND S chain_3.9# GND nuv1 W=1.68E-07 L=2.8E-08 m=1
Mchain_1.0 chain_1.9# B[1] B[0] Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_1.1 Vdd W.1 chain_1.9# Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_2.0 Vdd S B[0] Vdd p W=8.4E-08 L=2.8E-08 m=1
Mchain_4.0 chain_4.9# B[0] B[1] Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_4.1 Vdd W.0 chain_4.9# Vdd p W=2.1E-07 L=2.8E-08 m=1
Mchain_5.0 Vdd S B[1] Vdd p W=8.4E-08 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_env_bit_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000 B[0][0] B[0][1] B[1][0] B[1][1] GND K.0 K.1 LK LR.0 
+ LR.1 RR.0 RR.1 S[0] S[1] Vdd W.0 W.1
*.PININFO B[0][1]:I B[1][0]:I B[1][1]:I GND:I K.0:I K.1:I LK:I LR.0:I LR.1:I 
*.PININFO RR.0:I RR.1:I S[0]:I S[1]:I Vdd:I W.0:I W.1:I B[0][0]:O
Xlookup GND K.0 K.1 LK Vdd W.0 W.1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_lookup_bit_4000
Xread B[0][0] B[0][1] B[1][0] B[1][1] GND LR.0 LR.1 RR.0 RR.1 Vdd / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_read_bit_4000
Xwrite[0] B[0][0] B[0][1] GND S[0] Vdd W.0 W.1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_write_bit_4000
Xwrite[1] B[1][0] B[1][1] GND S[1] Vdd W.0 W.1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_write_bit_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_chunk64_20_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_chunk64_20_4000 A[0][0] A[0][1] A[0][2] A[0][3] A[0][4] A[0][5] 
+ A[0][6] A[0][7] A[0][8] A[0][9] A[0][10] A[0][11] A[0][12] A[0][13] A[0][14] 
+ A[0][15] A[0][16] A[0][17] A[0][18] A[0][19] A[0][20] A[0][21] A[0][22] 
+ A[0][23] A[0][24] A[0][25] A[0][26] A[0][27] A[0][28] A[0][29] A[0][30] 
+ A[0][31] A[0][32] A[0][33] A[0][34] A[0][35] A[0][36] A[0][37] A[0][38] 
+ A[0][39] A[0][40] A[0][41] A[0][42] A[0][43] A[0][44] A[0][45] A[0][46] 
+ A[0][47] A[0][48] A[0][49] A[0][50] A[0][51] A[0][52] A[0][53] A[0][54] 
+ A[0][55] A[0][56] A[0][57] A[0][58] A[0][59] A[0][60] A[0][61] A[0][62] 
+ A[0][63] A[1][0] A[1][1] A[1][2] A[1][3] A[1][4] A[1][5] A[1][6] A[1][7] 
+ A[1][8] A[1][9] A[1][10] A[1][11] A[1][12] A[1][13] A[1][14] A[1][15] 
+ A[1][16] A[1][17] A[1][18] A[1][19] A[1][20] A[1][21] A[1][22] A[1][23] 
+ A[1][24] A[1][25] A[1][26] A[1][27] A[1][28] A[1][29] A[1][30] A[1][31] 
+ A[1][32] A[1][33] A[1][34] A[1][35] A[1][36] A[1][37] A[1][38] A[1][39] 
+ A[1][40] A[1][41] A[1][42] A[1][43] A[1][44] A[1][45] A[1][46] A[1][47] 
+ A[1][48] A[1][49] A[1][50] A[1][51] A[1][52] A[1][53] A[1][54] A[1][55] 
+ A[1][56] A[1][57] A[1][58] A[1][59] A[1][60] A[1][61] A[1][62] A[1][63] GND 
+ H[0][0] H[0][1] H[0][2] H[0][3] H[0][4] H[0][5] H[0][6] H[0][7] H[0][8] 
+ H[0][9] H[0][10] H[0][11] H[0][12] H[0][13] H[0][14] H[0][15] H[0][16] 
+ H[0][17] H[0][18] H[0][19] H[0][20] H[0][21] H[0][22] H[0][23] H[0][24] 
+ H[0][25] H[0][26] H[0][27] H[0][28] H[0][29] H[0][30] H[0][31] H[1][0] 
+ H[1][1] H[1][2] H[1][3] H[1][4] H[1][5] H[1][6] H[1][7] H[1][8] H[1][9] 
+ H[1][10] H[1][11] H[1][12] H[1][13] H[1][14] H[1][15] H[1][16] H[1][17] 
+ H[1][18] H[1][19] H[1][20] H[1][21] H[1][22] H[1][23] H[1][24] H[1][25] 
+ H[1][26] H[1][27] H[1][28] H[1][29] H[1][30] H[1][31] LK LR[0].0 LR[0].1 
+ LR[1].0 LR[1].1 LR[2].0 LR[2].1 LR[3].0 LR[3].1 LR[4].0 LR[4].1 LR[5].0 
+ LR[5].1 LR[6].0 LR[6].1 LR[7].0 LR[7].1 LR[8].0 LR[8].1 LR[9].0 LR[9].1 
+ LR[10].0 LR[10].1 LR[11].0 LR[11].1 LR[12].0 LR[12].1 LR[13].0 LR[13].1 
+ LR[14].0 LR[14].1 LR[15].0 LR[15].1 LR[16].0 LR[16].1 LR[17].0 LR[17].1 
+ LR[18].0 LR[18].1 LR[19].0 LR[19].1 RR[0].0 RR[0].1 RR[1].0 RR[1].1 RR[2].0 
+ RR[2].1 RR[3].0 RR[3].1 RR[4].0 RR[4].1 RR[5].0 RR[5].1 RR[6].0 RR[6].1 
+ RR[7].0 RR[7].1 RR[8].0 RR[8].1 RR[9].0 RR[9].1 RR[10].0 RR[10].1 RR[11].0 
+ RR[11].1 RR[12].0 RR[12].1 RR[13].0 RR[13].1 RR[14].0 RR[14].1 RR[15].0 
+ RR[15].1 RR[16].0 RR[16].1 RR[17].0 RR[17].1 RR[18].0 RR[18].1 RR[19].0 
+ RR[19].1 S[0] S[1] Vdd W[0].0 W[0].1 W[1].0 W[1].1 W[2].0 W[2].1 W[3].0 
+ W[3].1 W[4].0 W[4].1 W[5].0 W[5].1 W[6].0 W[6].1 W[7].0 W[7].1 W[8].0 W[8].1 
+ W[9].0 W[9].1 W[10].0 W[10].1 W[11].0 W[11].1 W[12].0 W[12].1 W[13].0 
+ W[13].1 W[14].0 W[14].1 W[15].0 W[15].1 W[16].0 W[16].1 W[17].0 W[17].1 
+ W[18].0 W[18].1 W[19].0 W[19].1
*.PININFO A[0][1]:I A[0][2]:I A[0][3]:I A[0][4]:I A[0][5]:I A[0][6]:I 
*.PININFO A[0][7]:I A[0][8]:I A[0][9]:I A[0][10]:I A[0][11]:I A[0][12]:I 
*.PININFO A[0][13]:I A[0][14]:I A[0][15]:I A[0][16]:I A[0][17]:I A[0][18]:I 
*.PININFO A[0][19]:I A[0][20]:I A[0][21]:I A[0][22]:I A[0][23]:I A[0][24]:I 
*.PININFO A[0][25]:I A[0][26]:I A[0][27]:I A[0][28]:I A[0][29]:I A[0][30]:I 
*.PININFO A[0][31]:I A[0][32]:I A[0][33]:I A[0][34]:I A[0][35]:I A[0][36]:I 
*.PININFO A[0][37]:I A[0][38]:I A[0][39]:I A[0][40]:I A[0][41]:I A[0][42]:I 
*.PININFO A[0][43]:I A[0][44]:I A[0][45]:I A[0][46]:I A[0][47]:I A[0][48]:I 
*.PININFO A[0][49]:I A[0][50]:I A[0][51]:I A[0][52]:I A[0][53]:I A[0][54]:I 
*.PININFO A[0][55]:I A[0][56]:I A[0][57]:I A[0][58]:I A[0][59]:I A[0][60]:I 
*.PININFO A[0][61]:I A[0][62]:I A[0][63]:I A[1][0]:I A[1][1]:I A[1][2]:I 
*.PININFO A[1][3]:I A[1][4]:I A[1][5]:I A[1][6]:I A[1][7]:I A[1][8]:I 
*.PININFO A[1][9]:I A[1][10]:I A[1][11]:I A[1][12]:I A[1][13]:I A[1][14]:I 
*.PININFO A[1][15]:I A[1][16]:I A[1][17]:I A[1][18]:I A[1][19]:I A[1][20]:I 
*.PININFO A[1][21]:I A[1][22]:I A[1][23]:I A[1][24]:I A[1][25]:I A[1][26]:I 
*.PININFO A[1][27]:I A[1][28]:I A[1][29]:I A[1][30]:I A[1][31]:I A[1][32]:I 
*.PININFO A[1][33]:I A[1][34]:I A[1][35]:I A[1][36]:I A[1][37]:I A[1][38]:I 
*.PININFO A[1][39]:I A[1][40]:I A[1][41]:I A[1][42]:I A[1][43]:I A[1][44]:I 
*.PININFO A[1][45]:I A[1][46]:I A[1][47]:I A[1][48]:I A[1][49]:I A[1][50]:I 
*.PININFO A[1][51]:I A[1][52]:I A[1][53]:I A[1][54]:I A[1][55]:I A[1][56]:I 
*.PININFO A[1][57]:I A[1][58]:I A[1][59]:I A[1][60]:I A[1][61]:I A[1][62]:I 
*.PININFO A[1][63]:I GND:I H[0][0]:I H[0][1]:I H[0][2]:I H[0][3]:I H[0][4]:I 
*.PININFO H[0][5]:I H[0][6]:I H[0][7]:I H[0][8]:I H[0][9]:I H[0][10]:I 
*.PININFO H[0][11]:I H[0][12]:I H[0][13]:I H[0][14]:I H[0][15]:I H[0][16]:I 
*.PININFO H[0][17]:I H[0][18]:I H[0][19]:I H[0][20]:I H[0][21]:I H[0][22]:I 
*.PININFO H[0][23]:I H[0][24]:I H[0][25]:I H[0][26]:I H[0][27]:I H[0][28]:I 
*.PININFO H[0][29]:I H[0][30]:I H[0][31]:I H[1][0]:I H[1][1]:I H[1][2]:I 
*.PININFO H[1][3]:I H[1][4]:I H[1][5]:I H[1][6]:I H[1][7]:I H[1][8]:I 
*.PININFO H[1][9]:I H[1][10]:I H[1][11]:I H[1][12]:I H[1][13]:I H[1][14]:I 
*.PININFO H[1][15]:I H[1][16]:I H[1][17]:I H[1][18]:I H[1][19]:I H[1][20]:I 
*.PININFO H[1][21]:I H[1][22]:I H[1][23]:I H[1][24]:I H[1][25]:I H[1][26]:I 
*.PININFO H[1][27]:I H[1][28]:I H[1][29]:I H[1][30]:I H[1][31]:I LK:I 
*.PININFO LR[0].0:I LR[0].1:I LR[1].0:I LR[1].1:I LR[2].0:I LR[2].1:I 
*.PININFO LR[3].0:I LR[3].1:I LR[4].0:I LR[4].1:I LR[5].0:I LR[5].1:I 
*.PININFO LR[6].0:I LR[6].1:I LR[7].0:I LR[7].1:I LR[8].0:I LR[8].1:I 
*.PININFO LR[9].0:I LR[9].1:I LR[10].0:I LR[10].1:I LR[11].0:I LR[11].1:I 
*.PININFO LR[12].0:I LR[12].1:I LR[13].0:I LR[13].1:I LR[14].0:I LR[14].1:I 
*.PININFO LR[15].0:I LR[15].1:I LR[16].0:I LR[16].1:I LR[17].0:I LR[17].1:I 
*.PININFO LR[18].0:I LR[18].1:I LR[19].0:I LR[19].1:I RR[0].0:I RR[0].1:I 
*.PININFO RR[1].0:I RR[1].1:I RR[2].0:I RR[2].1:I RR[3].0:I RR[3].1:I 
*.PININFO RR[4].0:I RR[4].1:I RR[5].0:I RR[5].1:I RR[6].0:I RR[6].1:I 
*.PININFO RR[7].0:I RR[7].1:I RR[8].0:I RR[8].1:I RR[9].0:I RR[9].1:I 
*.PININFO RR[10].0:I RR[10].1:I RR[11].0:I RR[11].1:I RR[12].0:I RR[12].1:I 
*.PININFO RR[13].0:I RR[13].1:I RR[14].0:I RR[14].1:I RR[15].0:I RR[15].1:I 
*.PININFO RR[16].0:I RR[16].1:I RR[17].0:I RR[17].1:I RR[18].0:I RR[18].1:I 
*.PININFO RR[19].0:I RR[19].1:I S[0]:I S[1]:I Vdd:I W[0].0:I W[0].1:I W[1].0:I 
*.PININFO W[1].1:I W[2].0:I W[2].1:I W[3].0:I W[3].1:I W[4].0:I W[4].1:I 
*.PININFO W[5].0:I W[5].1:I W[6].0:I W[6].1:I W[7].0:I W[7].1:I W[8].0:I 
*.PININFO W[8].1:I W[9].0:I W[9].1:I W[10].0:I W[10].1:I W[11].0:I W[11].1:I 
*.PININFO W[12].0:I W[12].1:I W[13].0:I W[13].1:I W[14].0:I W[14].1:I 
*.PININFO W[15].0:I W[15].1:I W[16].0:I W[16].1:I W[17].0:I W[17].1:I 
*.PININFO W[18].0:I W[18].1:I W[19].0:I W[19].1:I A[0][0]:O
Xz[0] GND Vdd A[0][0] A[0][1] A[0][2] A[0][3] A[0][4] A[0][5] A[0][6] A[0][7] 
+ A[0][8] A[0][9] A[0][10] A[0][11] A[0][12] A[0][13] A[0][14] A[0][15] 
+ A[0][16] A[0][17] A[0][18] A[0][19] A[0][20] A[0][21] A[0][22] A[0][23] 
+ A[0][24] A[0][25] A[0][26] A[0][27] A[0][28] A[0][29] A[0][30] A[0][31] 
+ A[0][32] A[0][33] A[0][34] A[0][35] A[0][36] A[0][37] A[0][38] A[0][39] 
+ A[0][40] A[0][41] A[0][42] A[0][43] A[0][44] A[0][45] A[0][46] A[0][47] 
+ A[0][48] A[0][49] A[0][50] A[0][51] A[0][52] A[0][53] A[0][54] A[0][55] 
+ A[0][56] A[0][57] A[0][58] A[0][59] A[0][60] A[0][61] A[0][62] A[0][63] 
+ b[0][0][0] b[0][0][1] b[0][1][0] b[0][1][1] b[0][2][0] b[0][2][1] b[0][3][0] 
+ b[0][3][1] b[0][4][0] b[0][4][1] b[0][5][0] b[0][5][1] b[0][6][0] b[0][6][1] 
+ b[0][7][0] b[0][7][1] b[0][8][0] b[0][8][1] b[0][9][0] b[0][9][1] 
+ b[0][10][0] b[0][10][1] b[0][11][0] b[0][11][1] b[0][12][0] b[0][12][1] 
+ b[0][13][0] b[0][13][1] b[0][14][0] b[0][14][1] b[0][15][0] b[0][15][1] 
+ b[0][16][0] b[0][16][1] b[0][17][0] b[0][17][1] b[0][18][0] b[0][18][1] 
+ b[0][19][0] b[0][19][1] H[0][0] H[0][1] H[0][2] H[0][3] H[0][4] H[0][5] 
+ H[0][6] H[0][7] H[0][8] H[0][9] H[0][10] H[0][11] H[0][12] H[0][13] H[0][14] 
+ H[0][15] H[0][16] H[0][17] H[0][18] H[0][19] H[0][20] H[0][21] H[0][22] 
+ H[0][23] H[0][24] H[0][25] H[0][26] H[0][27] H[0][28] H[0][29] H[0][30] 
+ H[0][31] k[0].0 k[0].1 k[1].0 k[1].1 k[2].0 k[2].1 k[3].0 k[3].1 k[4].0 
+ k[4].1 k[5].0 k[5].1 k[6].0 k[6].1 k[7].0 k[7].1 k[8].0 k[8].1 k[9].0 k[9].1 
+ k[10].0 k[10].1 k[11].0 k[11].1 k[12].0 k[12].1 k[13].0 k[13].1 k[14].0 
+ k[14].1 k[15].0 k[15].1 k[16].0 k[16].1 k[17].0 k[17].1 k[18].0 k[18].1 
+ k[19].0 k[19].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_array_32_20_4000
Xz[1] GND Vdd A[1][0] A[1][1] A[1][2] A[1][3] A[1][4] A[1][5] A[1][6] A[1][7] 
+ A[1][8] A[1][9] A[1][10] A[1][11] A[1][12] A[1][13] A[1][14] A[1][15] 
+ A[1][16] A[1][17] A[1][18] A[1][19] A[1][20] A[1][21] A[1][22] A[1][23] 
+ A[1][24] A[1][25] A[1][26] A[1][27] A[1][28] A[1][29] A[1][30] A[1][31] 
+ A[1][32] A[1][33] A[1][34] A[1][35] A[1][36] A[1][37] A[1][38] A[1][39] 
+ A[1][40] A[1][41] A[1][42] A[1][43] A[1][44] A[1][45] A[1][46] A[1][47] 
+ A[1][48] A[1][49] A[1][50] A[1][51] A[1][52] A[1][53] A[1][54] A[1][55] 
+ A[1][56] A[1][57] A[1][58] A[1][59] A[1][60] A[1][61] A[1][62] A[1][63] 
+ b[1][0][0] b[1][0][1] b[1][1][0] b[1][1][1] b[1][2][0] b[1][2][1] b[1][3][0] 
+ b[1][3][1] b[1][4][0] b[1][4][1] b[1][5][0] b[1][5][1] b[1][6][0] b[1][6][1] 
+ b[1][7][0] b[1][7][1] b[1][8][0] b[1][8][1] b[1][9][0] b[1][9][1] 
+ b[1][10][0] b[1][10][1] b[1][11][0] b[1][11][1] b[1][12][0] b[1][12][1] 
+ b[1][13][0] b[1][13][1] b[1][14][0] b[1][14][1] b[1][15][0] b[1][15][1] 
+ b[1][16][0] b[1][16][1] b[1][17][0] b[1][17][1] b[1][18][0] b[1][18][1] 
+ b[1][19][0] b[1][19][1] H[1][0] H[1][1] H[1][2] H[1][3] H[1][4] H[1][5] 
+ H[1][6] H[1][7] H[1][8] H[1][9] H[1][10] H[1][11] H[1][12] H[1][13] H[1][14] 
+ H[1][15] H[1][16] H[1][17] H[1][18] H[1][19] H[1][20] H[1][21] H[1][22] 
+ H[1][23] H[1][24] H[1][25] H[1][26] H[1][27] H[1][28] H[1][29] H[1][30] 
+ H[1][31] k[0].0 k[0].1 k[1].0 k[1].1 k[2].0 k[2].1 k[3].0 k[3].1 k[4].0 
+ k[4].1 k[5].0 k[5].1 k[6].0 k[6].1 k[7].0 k[7].1 k[8].0 k[8].1 k[9].0 k[9].1 
+ k[10].0 k[10].1 k[11].0 k[11].1 k[12].0 k[12].1 k[13].0 k[13].1 k[14].0 
+ k[14].1 k[15].0 k[15].1 k[16].0 k[16].1 k[17].0 k[17].1 k[18].0 k[18].1 
+ k[19].0 k[19].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tbit_array_32_20_4000
Xe[0] b[0][0][0] b[0][0][1] b[1][0][0] b[1][0][1] GND k[0].0 k[0].1 LK LR[0].0 
+ LR[0].1 RR[0].0 RR[0].1 S[0] S[1] Vdd W[0].0 W[0].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[1] b[0][1][0] b[0][1][1] b[1][1][0] b[1][1][1] GND k[1].0 k[1].1 LK LR[1].0 
+ LR[1].1 RR[1].0 RR[1].1 S[0] S[1] Vdd W[1].0 W[1].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[2] b[0][2][0] b[0][2][1] b[1][2][0] b[1][2][1] GND k[2].0 k[2].1 LK LR[2].0 
+ LR[2].1 RR[2].0 RR[2].1 S[0] S[1] Vdd W[2].0 W[2].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[3] b[0][3][0] b[0][3][1] b[1][3][0] b[1][3][1] GND k[3].0 k[3].1 LK LR[3].0 
+ LR[3].1 RR[3].0 RR[3].1 S[0] S[1] Vdd W[3].0 W[3].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[4] b[0][4][0] b[0][4][1] b[1][4][0] b[1][4][1] GND k[4].0 k[4].1 LK LR[4].0 
+ LR[4].1 RR[4].0 RR[4].1 S[0] S[1] Vdd W[4].0 W[4].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[5] b[0][5][0] b[0][5][1] b[1][5][0] b[1][5][1] GND k[5].0 k[5].1 LK LR[5].0 
+ LR[5].1 RR[5].0 RR[5].1 S[0] S[1] Vdd W[5].0 W[5].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[6] b[0][6][0] b[0][6][1] b[1][6][0] b[1][6][1] GND k[6].0 k[6].1 LK LR[6].0 
+ LR[6].1 RR[6].0 RR[6].1 S[0] S[1] Vdd W[6].0 W[6].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[7] b[0][7][0] b[0][7][1] b[1][7][0] b[1][7][1] GND k[7].0 k[7].1 LK LR[7].0 
+ LR[7].1 RR[7].0 RR[7].1 S[0] S[1] Vdd W[7].0 W[7].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[8] b[0][8][0] b[0][8][1] b[1][8][0] b[1][8][1] GND k[8].0 k[8].1 LK LR[8].0 
+ LR[8].1 RR[8].0 RR[8].1 S[0] S[1] Vdd W[8].0 W[8].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[9] b[0][9][0] b[0][9][1] b[1][9][0] b[1][9][1] GND k[9].0 k[9].1 LK LR[9].0 
+ LR[9].1 RR[9].0 RR[9].1 S[0] S[1] Vdd W[9].0 W[9].1 / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[10] b[0][10][0] b[0][10][1] b[1][10][0] b[1][10][1] GND k[10].0 k[10].1 LK 
+ LR[10].0 LR[10].1 RR[10].0 RR[10].1 S[0] S[1] Vdd W[10].0 W[10].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[11] b[0][11][0] b[0][11][1] b[1][11][0] b[1][11][1] GND k[11].0 k[11].1 LK 
+ LR[11].0 LR[11].1 RR[11].0 RR[11].1 S[0] S[1] Vdd W[11].0 W[11].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[12] b[0][12][0] b[0][12][1] b[1][12][0] b[1][12][1] GND k[12].0 k[12].1 LK 
+ LR[12].0 LR[12].1 RR[12].0 RR[12].1 S[0] S[1] Vdd W[12].0 W[12].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[13] b[0][13][0] b[0][13][1] b[1][13][0] b[1][13][1] GND k[13].0 k[13].1 LK 
+ LR[13].0 LR[13].1 RR[13].0 RR[13].1 S[0] S[1] Vdd W[13].0 W[13].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[14] b[0][14][0] b[0][14][1] b[1][14][0] b[1][14][1] GND k[14].0 k[14].1 LK 
+ LR[14].0 LR[14].1 RR[14].0 RR[14].1 S[0] S[1] Vdd W[14].0 W[14].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[15] b[0][15][0] b[0][15][1] b[1][15][0] b[1][15][1] GND k[15].0 k[15].1 LK 
+ LR[15].0 LR[15].1 RR[15].0 RR[15].1 S[0] S[1] Vdd W[15].0 W[15].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[16] b[0][16][0] b[0][16][1] b[1][16][0] b[1][16][1] GND k[16].0 k[16].1 LK 
+ LR[16].0 LR[16].1 RR[16].0 RR[16].1 S[0] S[1] Vdd W[16].0 W[16].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[17] b[0][17][0] b[0][17][1] b[1][17][0] b[1][17][1] GND k[17].0 k[17].1 LK 
+ LR[17].0 LR[17].1 RR[17].0 RR[17].1 S[0] S[1] Vdd W[17].0 W[17].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[18] b[0][18][0] b[0][18][1] b[1][18][0] b[1][18][1] GND k[18].0 k[18].1 LK 
+ LR[18].0 LR[18].1 RR[18].0 RR[18].1 S[0] S[1] Vdd W[18].0 W[18].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
Xe[19] b[0][19][0] b[0][19][1] b[1][19][0] b[1][19][1] GND k[19].0 k[19].1 LK 
+ LR[19].0 LR[19].1 RR[19].0 RR[19].1 S[0] S[1] Vdd W[19].0 W[19].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_env_bit_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_decap_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000 GND Vdd
*.PININFO GND:I Vdd:I
M0.0 Vdd GND Vdd Vdd p W=2.94E-07 L=2.8E-08 m=1
M1.0 Vdd GND Vdd Vdd p W=2.94E-07 L=2.8E-08 m=1
M2.0 Vdd GND Vdd Vdd p W=2.94E-07 L=2.8E-08 m=1
M3.0 Vdd GND Vdd Vdd p W=2.94E-07 L=2.8E-08 m=1
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_slice64_40_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000 ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 
+ ADDR[1].1 ADDR[1].2 ADDR[1].3 ADDR[2].0 ADDR[2].1 ADDR[2].2 ADDR[2].3 
+ ADDR[3].0 ADDR[3].1 ADDR[4].0 AR AW GND LCFG[0] LCFG[1] LCFG[2] LCFG[3] 
+ LCFG[4] LCFG[5] LCFG[6] LCFG[7] LCFG[8] LCFG[9] LCFG[10] LCFG[11] LHIT[0] 
+ LHIT[1] LHIT[2] LHIT[3] LHIT[4] LHIT[5] LHIT[6] LHIT[7] LHIT[8] LHIT[9] 
+ LHIT[10] LHIT[11] LHIT[12] LHIT[13] LHIT[14] LHIT[15] LHIT[16] LHIT[17] 
+ LHIT[18] LHIT[19] LHIT[20] LHIT[21] LHIT[22] LHIT[23] LHIT[24] LHIT[25] 
+ LHIT[26] LHIT[27] LHIT[28] LHIT[29] LHIT[30] LHIT[31] LHIT[32] LHIT[33] 
+ LHIT[34] LHIT[35] LHIT[36] LHIT[37] LHIT[38] LHIT[39] LHIT[40] LHIT[41] 
+ LHIT[42] LHIT[43] LHIT[44] LHIT[45] LHIT[46] LHIT[47] LHIT[48] LHIT[49] 
+ LHIT[50] LHIT[51] LHIT[52] LHIT[53] LHIT[54] LHIT[55] LHIT[56] LHIT[57] 
+ LHIT[58] LHIT[59] LHIT[60] LHIT[61] LHIT[62] LHIT[63] LK LR[0].0 LR[0].1 
+ LR[1].0 LR[1].1 LR[2].0 LR[2].1 LR[3].0 LR[3].1 LR[4].0 LR[4].1 LR[5].0 
+ LR[5].1 LR[6].0 LR[6].1 LR[7].0 LR[7].1 LR[8].0 LR[8].1 LR[9].0 LR[9].1 
+ LR[10].0 LR[10].1 LR[11].0 LR[11].1 LR[12].0 LR[12].1 LR[13].0 LR[13].1 
+ LR[14].0 LR[14].1 LR[15].0 LR[15].1 LR[16].0 LR[16].1 LR[17].0 LR[17].1 
+ LR[18].0 LR[18].1 LR[19].0 LR[19].1 LR[20].0 LR[20].1 LR[21].0 LR[21].1 
+ LR[22].0 LR[22].1 LR[23].0 LR[23].1 LR[24].0 LR[24].1 LR[25].0 LR[25].1 
+ LR[26].0 LR[26].1 LR[27].0 LR[27].1 LR[28].0 LR[28].1 LR[29].0 LR[29].1 
+ LR[30].0 LR[30].1 LR[31].0 LR[31].1 LR[32].0 LR[32].1 LR[33].0 LR[33].1 
+ LR[34].0 LR[34].1 LR[35].0 LR[35].1 LR[36].0 LR[36].1 LR[37].0 LR[37].1 
+ LR[38].0 LR[38].1 LR[39].0 LR[39].1 RCFG[0] RCFG[1] RCFG[2] RCFG[3] RCFG[4] 
+ RCFG[5] RCFG[6] RCFG[7] RCFG[8] RCFG[9] RCFG[10] RCFG[11] RHIT[0] RHIT[1] 
+ RHIT[2] RHIT[3] RHIT[4] RHIT[5] RHIT[6] RHIT[7] RHIT[8] RHIT[9] RHIT[10] 
+ RHIT[11] RHIT[12] RHIT[13] RHIT[14] RHIT[15] RHIT[16] RHIT[17] RHIT[18] 
+ RHIT[19] RHIT[20] RHIT[21] RHIT[22] RHIT[23] RHIT[24] RHIT[25] RHIT[26] 
+ RHIT[27] RHIT[28] RHIT[29] RHIT[30] RHIT[31] RHIT[32] RHIT[33] RHIT[34] 
+ RHIT[35] RHIT[36] RHIT[37] RHIT[38] RHIT[39] RHIT[40] RHIT[41] RHIT[42] 
+ RHIT[43] RHIT[44] RHIT[45] RHIT[46] RHIT[47] RHIT[48] RHIT[49] RHIT[50] 
+ RHIT[51] RHIT[52] RHIT[53] RHIT[54] RHIT[55] RHIT[56] RHIT[57] RHIT[58] 
+ RHIT[59] RHIT[60] RHIT[61] RHIT[62] RHIT[63] RR[0].0 RR[0].1 RR[1].0 RR[1].1 
+ RR[2].0 RR[2].1 RR[3].0 RR[3].1 RR[4].0 RR[4].1 RR[5].0 RR[5].1 RR[6].0 
+ RR[6].1 RR[7].0 RR[7].1 RR[8].0 RR[8].1 RR[9].0 RR[9].1 RR[10].0 RR[10].1 
+ RR[11].0 RR[11].1 RR[12].0 RR[12].1 RR[13].0 RR[13].1 RR[14].0 RR[14].1 
+ RR[15].0 RR[15].1 RR[16].0 RR[16].1 RR[17].0 RR[17].1 RR[18].0 RR[18].1 
+ RR[19].0 RR[19].1 RR[20].0 RR[20].1 RR[21].0 RR[21].1 RR[22].0 RR[22].1 
+ RR[23].0 RR[23].1 RR[24].0 RR[24].1 RR[25].0 RR[25].1 RR[26].0 RR[26].1 
+ RR[27].0 RR[27].1 RR[28].0 RR[28].1 RR[29].0 RR[29].1 RR[30].0 RR[30].1 
+ RR[31].0 RR[31].1 RR[32].0 RR[32].1 RR[33].0 RR[33].1 RR[34].0 RR[34].1 
+ RR[35].0 RR[35].1 RR[36].0 RR[36].1 RR[37].0 RR[37].1 RR[38].0 RR[38].1 
+ RR[39].0 RR[39].1 SLICE_EN Vdd W[0].0 W[0].1 W[1].0 W[1].1 W[2].0 W[2].1 
+ W[3].0 W[3].1 W[4].0 W[4].1 W[5].0 W[5].1 W[6].0 W[6].1 W[7].0 W[7].1 W[8].0 
+ W[8].1 W[9].0 W[9].1 W[10].0 W[10].1 W[11].0 W[11].1 W[12].0 W[12].1 W[13].0 
+ W[13].1 W[14].0 W[14].1 W[15].0 W[15].1 W[16].0 W[16].1 W[17].0 W[17].1 
+ W[18].0 W[18].1 W[19].0 W[19].1 W[20].0 W[20].1 W[21].0 W[21].1 W[22].0 
+ W[22].1 W[23].0 W[23].1 W[24].0 W[24].1 W[25].0 W[25].1 W[26].0 W[26].1 
+ W[27].0 W[27].1 W[28].0 W[28].1 W[29].0 W[29].1 W[30].0 W[30].1 W[31].0 
+ W[31].1 W[32].0 W[32].1 W[33].0 W[33].1 W[34].0 W[34].1 W[35].0 W[35].1 
+ W[36].0 W[36].1 W[37].0 W[37].1 W[38].0 W[38].1 W[39].0 W[39].1 _RESET
*.PININFO ADDR[0].0:I ADDR[0].1:I ADDR[0].2:I ADDR[0].3:I ADDR[1].0:I 
*.PININFO ADDR[1].1:I ADDR[1].2:I ADDR[1].3:I ADDR[2].0:I ADDR[2].1:I 
*.PININFO ADDR[2].2:I ADDR[2].3:I ADDR[3].0:I ADDR[3].1:I ADDR[4].0:I AW:I 
*.PININFO GND:I LCFG[0]:I LCFG[1]:I LCFG[2]:I LCFG[3]:I LCFG[4]:I LCFG[5]:I 
*.PININFO LCFG[6]:I LCFG[7]:I LCFG[8]:I LCFG[9]:I LCFG[10]:I LCFG[11]:I 
*.PININFO LHIT[0]:I LHIT[1]:I LHIT[2]:I LHIT[3]:I LHIT[4]:I LHIT[5]:I 
*.PININFO LHIT[6]:I LHIT[7]:I LHIT[8]:I LHIT[9]:I LHIT[10]:I LHIT[11]:I 
*.PININFO LHIT[12]:I LHIT[13]:I LHIT[14]:I LHIT[15]:I LHIT[16]:I LHIT[17]:I 
*.PININFO LHIT[18]:I LHIT[19]:I LHIT[20]:I LHIT[21]:I LHIT[22]:I LHIT[23]:I 
*.PININFO LHIT[24]:I LHIT[25]:I LHIT[26]:I LHIT[27]:I LHIT[28]:I LHIT[29]:I 
*.PININFO LHIT[30]:I LHIT[31]:I LHIT[32]:I LHIT[33]:I LHIT[34]:I LHIT[35]:I 
*.PININFO LHIT[36]:I LHIT[37]:I LHIT[38]:I LHIT[39]:I LHIT[40]:I LHIT[41]:I 
*.PININFO LHIT[42]:I LHIT[43]:I LHIT[44]:I LHIT[45]:I LHIT[46]:I LHIT[47]:I 
*.PININFO LHIT[48]:I LHIT[49]:I LHIT[50]:I LHIT[51]:I LHIT[52]:I LHIT[53]:I 
*.PININFO LHIT[54]:I LHIT[55]:I LHIT[56]:I LHIT[57]:I LHIT[58]:I LHIT[59]:I 
*.PININFO LHIT[60]:I LHIT[61]:I LHIT[62]:I LHIT[63]:I LK:I LR[0].0:I LR[0].1:I 
*.PININFO LR[1].0:I LR[1].1:I LR[2].0:I LR[2].1:I LR[3].0:I LR[3].1:I 
*.PININFO LR[4].0:I LR[4].1:I LR[5].0:I LR[5].1:I LR[6].0:I LR[6].1:I 
*.PININFO LR[7].0:I LR[7].1:I LR[8].0:I LR[8].1:I LR[9].0:I LR[9].1:I 
*.PININFO LR[10].0:I LR[10].1:I LR[11].0:I LR[11].1:I LR[12].0:I LR[12].1:I 
*.PININFO LR[13].0:I LR[13].1:I LR[14].0:I LR[14].1:I LR[15].0:I LR[15].1:I 
*.PININFO LR[16].0:I LR[16].1:I LR[17].0:I LR[17].1:I LR[18].0:I LR[18].1:I 
*.PININFO LR[19].0:I LR[19].1:I LR[20].0:I LR[20].1:I LR[21].0:I LR[21].1:I 
*.PININFO LR[22].0:I LR[22].1:I LR[23].0:I LR[23].1:I LR[24].0:I LR[24].1:I 
*.PININFO LR[25].0:I LR[25].1:I LR[26].0:I LR[26].1:I LR[27].0:I LR[27].1:I 
*.PININFO LR[28].0:I LR[28].1:I LR[29].0:I LR[29].1:I LR[30].0:I LR[30].1:I 
*.PININFO LR[31].0:I LR[31].1:I LR[32].0:I LR[32].1:I LR[33].0:I LR[33].1:I 
*.PININFO LR[34].0:I LR[34].1:I LR[35].0:I LR[35].1:I LR[36].0:I LR[36].1:I 
*.PININFO LR[37].0:I LR[37].1:I LR[38].0:I LR[38].1:I LR[39].0:I LR[39].1:I 
*.PININFO RCFG[0]:I RCFG[1]:I RCFG[2]:I RCFG[3]:I RCFG[4]:I RCFG[5]:I 
*.PININFO RCFG[6]:I RCFG[7]:I RCFG[8]:I RCFG[9]:I RCFG[10]:I RCFG[11]:I 
*.PININFO RHIT[0]:I RHIT[1]:I RHIT[2]:I RHIT[3]:I RHIT[4]:I RHIT[5]:I 
*.PININFO RHIT[6]:I RHIT[7]:I RHIT[8]:I RHIT[9]:I RHIT[10]:I RHIT[11]:I 
*.PININFO RHIT[12]:I RHIT[13]:I RHIT[14]:I RHIT[15]:I RHIT[16]:I RHIT[17]:I 
*.PININFO RHIT[18]:I RHIT[19]:I RHIT[20]:I RHIT[21]:I RHIT[22]:I RHIT[23]:I 
*.PININFO RHIT[24]:I RHIT[25]:I RHIT[26]:I RHIT[27]:I RHIT[28]:I RHIT[29]:I 
*.PININFO RHIT[30]:I RHIT[31]:I RHIT[32]:I RHIT[33]:I RHIT[34]:I RHIT[35]:I 
*.PININFO RHIT[36]:I RHIT[37]:I RHIT[38]:I RHIT[39]:I RHIT[40]:I RHIT[41]:I 
*.PININFO RHIT[42]:I RHIT[43]:I RHIT[44]:I RHIT[45]:I RHIT[46]:I RHIT[47]:I 
*.PININFO RHIT[48]:I RHIT[49]:I RHIT[50]:I RHIT[51]:I RHIT[52]:I RHIT[53]:I 
*.PININFO RHIT[54]:I RHIT[55]:I RHIT[56]:I RHIT[57]:I RHIT[58]:I RHIT[59]:I 
*.PININFO RHIT[60]:I RHIT[61]:I RHIT[62]:I RHIT[63]:I RR[0].0:I RR[0].1:I 
*.PININFO RR[1].0:I RR[1].1:I RR[2].0:I RR[2].1:I RR[3].0:I RR[3].1:I 
*.PININFO RR[4].0:I RR[4].1:I RR[5].0:I RR[5].1:I RR[6].0:I RR[6].1:I 
*.PININFO RR[7].0:I RR[7].1:I RR[8].0:I RR[8].1:I RR[9].0:I RR[9].1:I 
*.PININFO RR[10].0:I RR[10].1:I RR[11].0:I RR[11].1:I RR[12].0:I RR[12].1:I 
*.PININFO RR[13].0:I RR[13].1:I RR[14].0:I RR[14].1:I RR[15].0:I RR[15].1:I 
*.PININFO RR[16].0:I RR[16].1:I RR[17].0:I RR[17].1:I RR[18].0:I RR[18].1:I 
*.PININFO RR[19].0:I RR[19].1:I RR[20].0:I RR[20].1:I RR[21].0:I RR[21].1:I 
*.PININFO RR[22].0:I RR[22].1:I RR[23].0:I RR[23].1:I RR[24].0:I RR[24].1:I 
*.PININFO RR[25].0:I RR[25].1:I RR[26].0:I RR[26].1:I RR[27].0:I RR[27].1:I 
*.PININFO RR[28].0:I RR[28].1:I RR[29].0:I RR[29].1:I RR[30].0:I RR[30].1:I 
*.PININFO RR[31].0:I RR[31].1:I RR[32].0:I RR[32].1:I RR[33].0:I RR[33].1:I 
*.PININFO RR[34].0:I RR[34].1:I RR[35].0:I RR[35].1:I RR[36].0:I RR[36].1:I 
*.PININFO RR[37].0:I RR[37].1:I RR[38].0:I RR[38].1:I RR[39].0:I RR[39].1:I 
*.PININFO SLICE_EN:I Vdd:I W[0].0:I W[0].1:I W[1].0:I W[1].1:I W[2].0:I 
*.PININFO W[2].1:I W[3].0:I W[3].1:I W[4].0:I W[4].1:I W[5].0:I W[5].1:I 
*.PININFO W[6].0:I W[6].1:I W[7].0:I W[7].1:I W[8].0:I W[8].1:I W[9].0:I 
*.PININFO W[9].1:I W[10].0:I W[10].1:I W[11].0:I W[11].1:I W[12].0:I W[12].1:I 
*.PININFO W[13].0:I W[13].1:I W[14].0:I W[14].1:I W[15].0:I W[15].1:I 
*.PININFO W[16].0:I W[16].1:I W[17].0:I W[17].1:I W[18].0:I W[18].1:I 
*.PININFO W[19].0:I W[19].1:I W[20].0:I W[20].1:I W[21].0:I W[21].1:I 
*.PININFO W[22].0:I W[22].1:I W[23].0:I W[23].1:I W[24].0:I W[24].1:I 
*.PININFO W[25].0:I W[25].1:I W[26].0:I W[26].1:I W[27].0:I W[27].1:I 
*.PININFO W[28].0:I W[28].1:I W[29].0:I W[29].1:I W[30].0:I W[30].1:I 
*.PININFO W[31].0:I W[31].1:I W[32].0:I W[32].1:I W[33].0:I W[33].1:I 
*.PININFO W[34].0:I W[34].1:I W[35].0:I W[35].1:I W[36].0:I W[36].1:I 
*.PININFO W[37].0:I W[37].1:I W[38].0:I W[38].1:I W[39].0:I W[39].1:I _RESET:I 
*.PININFO AR:O
Xmid[0] b[0][0].0 b[0][0].1 b[0][0].2 b[0][0].3 b[0][1].0 b[0][1].1 b[0][1].2 
+ b[0][1].3 b[0][2].0 b[0][2].1 b[0][2].2 b[0][2].3 b[0][0].0 b[0][0].1 
+ b[0][0].2 b[0][0].3 b[0][1].0 b[0][1].1 b[0][1].2 b[0][1].3 b[0][2].0 
+ b[0][2].1 b[0][2].2 b[0][2].3 a[0][0][0] a[0][0][1] a[0][0][2] a[0][0][3] 
+ a[0][0][4] a[0][0][5] a[0][0][6] a[0][0][7] a[0][0][8] a[0][0][9] 
+ a[0][0][10] a[0][0][11] a[0][0][12] a[0][0][13] a[0][0][14] a[0][0][15] 
+ a[0][0][16] a[0][0][17] a[0][0][18] a[0][0][19] a[0][0][20] a[0][0][21] 
+ a[0][0][22] a[0][0][23] a[0][0][24] a[0][0][25] a[0][0][26] a[0][0][27] 
+ a[0][0][28] a[0][0][29] a[0][0][30] a[0][0][31] a[0][0][32] a[0][0][33] 
+ a[0][0][34] a[0][0][35] a[0][0][36] a[0][0][37] a[0][0][38] a[0][0][39] 
+ a[0][0][40] a[0][0][41] a[0][0][42] a[0][0][43] a[0][0][44] a[0][0][45] 
+ a[0][0][46] a[0][0][47] a[0][0][48] a[0][0][49] a[0][0][50] a[0][0][51] 
+ a[0][0][52] a[0][0][53] a[0][0][54] a[0][0][55] a[0][0][56] a[0][0][57] 
+ a[0][0][58] a[0][0][59] a[0][0][60] a[0][0][61] a[0][0][62] a[0][0][63] 
+ a[0][1][0] a[0][1][1] a[0][1][2] a[0][1][3] a[0][1][4] a[0][1][5] a[0][1][6] 
+ a[0][1][7] a[0][1][8] a[0][1][9] a[0][1][10] a[0][1][11] a[0][1][12] 
+ a[0][1][13] a[0][1][14] a[0][1][15] a[0][1][16] a[0][1][17] a[0][1][18] 
+ a[0][1][19] a[0][1][20] a[0][1][21] a[0][1][22] a[0][1][23] a[0][1][24] 
+ a[0][1][25] a[0][1][26] a[0][1][27] a[0][1][28] a[0][1][29] a[0][1][30] 
+ a[0][1][31] a[0][1][32] a[0][1][33] a[0][1][34] a[0][1][35] a[0][1][36] 
+ a[0][1][37] a[0][1][38] a[0][1][39] a[0][1][40] a[0][1][41] a[0][1][42] 
+ a[0][1][43] a[0][1][44] a[0][1][45] a[0][1][46] a[0][1][47] a[0][1][48] 
+ a[0][1][49] a[0][1][50] a[0][1][51] a[0][1][52] a[0][1][53] a[0][1][54] 
+ a[0][1][55] a[0][1][56] a[0][1][57] a[0][1][58] a[0][1][59] a[0][1][60] 
+ a[0][1][61] a[0][1][62] a[0][1][63] GND LHIT[0] LHIT[1] LHIT[2] LHIT[3] 
+ LHIT[4] LHIT[5] LHIT[6] LHIT[7] LHIT[8] LHIT[9] LHIT[10] LHIT[11] LHIT[12] 
+ LHIT[13] LHIT[14] LHIT[15] LHIT[16] LHIT[17] LHIT[18] LHIT[19] LHIT[20] 
+ LHIT[21] LHIT[22] LHIT[23] LHIT[24] LHIT[25] LHIT[26] LHIT[27] LHIT[28] 
+ LHIT[29] LHIT[30] LHIT[31] hit[0][0] hit[0][1] hit[0][2] hit[0][3] hit[0][4] 
+ hit[0][5] hit[0][6] hit[0][7] hit[0][8] hit[0][9] hit[0][10] hit[0][11] 
+ hit[0][12] hit[0][13] hit[0][14] hit[0][15] hit[0][16] hit[0][17] hit[0][18] 
+ hit[0][19] hit[0][20] hit[0][21] hit[0][22] hit[0][23] hit[0][24] hit[0][25] 
+ hit[0][26] hit[0][27] hit[0][28] hit[0][29] hit[0][30] hit[0][31] hit[1][0] 
+ hit[1][1] hit[1][2] hit[1][3] hit[1][4] hit[1][5] hit[1][6] hit[1][7] 
+ hit[1][8] hit[1][9] hit[1][10] hit[1][11] hit[1][12] hit[1][13] hit[1][14] 
+ hit[1][15] hit[1][16] hit[1][17] hit[1][18] hit[1][19] hit[1][20] hit[1][21] 
+ hit[1][22] hit[1][23] hit[1][24] hit[1][25] hit[1][26] hit[1][27] hit[1][28] 
+ hit[1][29] hit[1][30] hit[1][31] lk[2] lk[2] lk[2] lk[2] RHIT[0] RHIT[1] 
+ RHIT[2] RHIT[3] RHIT[4] RHIT[5] RHIT[6] RHIT[7] RHIT[8] RHIT[9] RHIT[10] 
+ RHIT[11] RHIT[12] RHIT[13] RHIT[14] RHIT[15] RHIT[16] RHIT[17] RHIT[18] 
+ RHIT[19] RHIT[20] RHIT[21] RHIT[22] RHIT[23] RHIT[24] RHIT[25] RHIT[26] 
+ RHIT[27] RHIT[28] RHIT[29] RHIT[30] RHIT[31] Vdd WL_Vcc[0] WL_Vcc[0] _dlk 
+ _dlk _dlk _dlk / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice_mid32_4000
Xmid[1] b[1][0].0 b[1][0].1 b[1][0].2 b[1][0].3 b[1][1].0 b[1][1].1 b[1][1].2 
+ b[1][1].3 b[1][2].0 b[1][2].1 b[1][2].2 b[1][2].3 b[1][0].0 b[1][0].1 
+ b[1][0].2 b[1][0].3 b[1][1].0 b[1][1].1 b[1][1].2 b[1][1].3 b[1][2].0 
+ b[1][2].1 b[1][2].2 b[1][2].3 a[1][0][0] a[1][0][1] a[1][0][2] a[1][0][3] 
+ a[1][0][4] a[1][0][5] a[1][0][6] a[1][0][7] a[1][0][8] a[1][0][9] 
+ a[1][0][10] a[1][0][11] a[1][0][12] a[1][0][13] a[1][0][14] a[1][0][15] 
+ a[1][0][16] a[1][0][17] a[1][0][18] a[1][0][19] a[1][0][20] a[1][0][21] 
+ a[1][0][22] a[1][0][23] a[1][0][24] a[1][0][25] a[1][0][26] a[1][0][27] 
+ a[1][0][28] a[1][0][29] a[1][0][30] a[1][0][31] a[1][0][32] a[1][0][33] 
+ a[1][0][34] a[1][0][35] a[1][0][36] a[1][0][37] a[1][0][38] a[1][0][39] 
+ a[1][0][40] a[1][0][41] a[1][0][42] a[1][0][43] a[1][0][44] a[1][0][45] 
+ a[1][0][46] a[1][0][47] a[1][0][48] a[1][0][49] a[1][0][50] a[1][0][51] 
+ a[1][0][52] a[1][0][53] a[1][0][54] a[1][0][55] a[1][0][56] a[1][0][57] 
+ a[1][0][58] a[1][0][59] a[1][0][60] a[1][0][61] a[1][0][62] a[1][0][63] 
+ a[1][1][0] a[1][1][1] a[1][1][2] a[1][1][3] a[1][1][4] a[1][1][5] a[1][1][6] 
+ a[1][1][7] a[1][1][8] a[1][1][9] a[1][1][10] a[1][1][11] a[1][1][12] 
+ a[1][1][13] a[1][1][14] a[1][1][15] a[1][1][16] a[1][1][17] a[1][1][18] 
+ a[1][1][19] a[1][1][20] a[1][1][21] a[1][1][22] a[1][1][23] a[1][1][24] 
+ a[1][1][25] a[1][1][26] a[1][1][27] a[1][1][28] a[1][1][29] a[1][1][30] 
+ a[1][1][31] a[1][1][32] a[1][1][33] a[1][1][34] a[1][1][35] a[1][1][36] 
+ a[1][1][37] a[1][1][38] a[1][1][39] a[1][1][40] a[1][1][41] a[1][1][42] 
+ a[1][1][43] a[1][1][44] a[1][1][45] a[1][1][46] a[1][1][47] a[1][1][48] 
+ a[1][1][49] a[1][1][50] a[1][1][51] a[1][1][52] a[1][1][53] a[1][1][54] 
+ a[1][1][55] a[1][1][56] a[1][1][57] a[1][1][58] a[1][1][59] a[1][1][60] 
+ a[1][1][61] a[1][1][62] a[1][1][63] GND LHIT[32] LHIT[33] LHIT[34] LHIT[35] 
+ LHIT[36] LHIT[37] LHIT[38] LHIT[39] LHIT[40] LHIT[41] LHIT[42] LHIT[43] 
+ LHIT[44] LHIT[45] LHIT[46] LHIT[47] LHIT[48] LHIT[49] LHIT[50] LHIT[51] 
+ LHIT[52] LHIT[53] LHIT[54] LHIT[55] LHIT[56] LHIT[57] LHIT[58] LHIT[59] 
+ LHIT[60] LHIT[61] LHIT[62] LHIT[63] hit[0][32] hit[0][33] hit[0][34] 
+ hit[0][35] hit[0][36] hit[0][37] hit[0][38] hit[0][39] hit[0][40] hit[0][41] 
+ hit[0][42] hit[0][43] hit[0][44] hit[0][45] hit[0][46] hit[0][47] hit[0][48] 
+ hit[0][49] hit[0][50] hit[0][51] hit[0][52] hit[0][53] hit[0][54] hit[0][55] 
+ hit[0][56] hit[0][57] hit[0][58] hit[0][59] hit[0][60] hit[0][61] hit[0][62] 
+ hit[0][63] hit[1][32] hit[1][33] hit[1][34] hit[1][35] hit[1][36] hit[1][37] 
+ hit[1][38] hit[1][39] hit[1][40] hit[1][41] hit[1][42] hit[1][43] hit[1][44] 
+ hit[1][45] hit[1][46] hit[1][47] hit[1][48] hit[1][49] hit[1][50] hit[1][51] 
+ hit[1][52] hit[1][53] hit[1][54] hit[1][55] hit[1][56] hit[1][57] hit[1][58] 
+ hit[1][59] hit[1][60] hit[1][61] hit[1][62] hit[1][63] lk[2] lk[2] lk[2] 
+ lk[2] RHIT[32] RHIT[33] RHIT[34] RHIT[35] RHIT[36] RHIT[37] RHIT[38] 
+ RHIT[39] RHIT[40] RHIT[41] RHIT[42] RHIT[43] RHIT[44] RHIT[45] RHIT[46] 
+ RHIT[47] RHIT[48] RHIT[49] RHIT[50] RHIT[51] RHIT[52] RHIT[53] RHIT[54] 
+ RHIT[55] RHIT[56] RHIT[57] RHIT[58] RHIT[59] RHIT[60] RHIT[61] RHIT[62] 
+ RHIT[63] Vdd WL_Vcc[1] WL_Vcc[1] _dlk _dlk _dlk _dlk / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice_mid32_4000
Xctrl ADDR[0].0 ADDR[0].1 ADDR[0].2 ADDR[0].3 ADDR[1].0 ADDR[1].1 ADDR[1].2 
+ ADDR[1].3 ADDR[2].0 ADDR[2].1 ADDR[2].2 ADDR[2].3 ADDR[3].0 ADDR[3].1 
+ ADDR[4].0 AR AW GND LCFG[0] LCFG[1] LCFG[2] LCFG[3] LCFG[4] LCFG[5] LCFG[6] 
+ LCFG[7] LCFG[8] LCFG[9] LCFG[10] LCFG[11] LK RCFG[0] RCFG[1] RCFG[2] RCFG[3] 
+ RCFG[4] RCFG[5] RCFG[6] RCFG[7] RCFG[8] RCFG[9] RCFG[10] RCFG[11] SLICE_EN 
+ Vdd WL_Vcc[0] WL_Vcc[1] _RESET _dlk b[0][0].0 b[0][0].1 b[0][0].2 b[0][0].3 
+ b[0][1].0 b[0][1].1 b[0][1].2 b[0][1].3 b[0][2].0 b[0][2].1 b[0][2].2 
+ b[0][2].3 b[1][0].0 b[1][0].1 b[1][0].2 b[1][0].3 b[1][1].0 b[1][1].1 
+ b[1][1].2 b[1][1].3 b[1][2].0 b[1][2].1 b[1][2].2 b[1][2].3 lk[1] lk[2] s[0] 
+ s[1] / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice_ctrl_4000
Xchunk[0] a[0][0][0] a[0][0][1] a[0][0][2] a[0][0][3] a[0][0][4] a[0][0][5] 
+ a[0][0][6] a[0][0][7] a[0][0][8] a[0][0][9] a[0][0][10] a[0][0][11] 
+ a[0][0][12] a[0][0][13] a[0][0][14] a[0][0][15] a[0][0][16] a[0][0][17] 
+ a[0][0][18] a[0][0][19] a[0][0][20] a[0][0][21] a[0][0][22] a[0][0][23] 
+ a[0][0][24] a[0][0][25] a[0][0][26] a[0][0][27] a[0][0][28] a[0][0][29] 
+ a[0][0][30] a[0][0][31] a[0][0][32] a[0][0][33] a[0][0][34] a[0][0][35] 
+ a[0][0][36] a[0][0][37] a[0][0][38] a[0][0][39] a[0][0][40] a[0][0][41] 
+ a[0][0][42] a[0][0][43] a[0][0][44] a[0][0][45] a[0][0][46] a[0][0][47] 
+ a[0][0][48] a[0][0][49] a[0][0][50] a[0][0][51] a[0][0][52] a[0][0][53] 
+ a[0][0][54] a[0][0][55] a[0][0][56] a[0][0][57] a[0][0][58] a[0][0][59] 
+ a[0][0][60] a[0][0][61] a[0][0][62] a[0][0][63] a[1][0][0] a[1][0][1] 
+ a[1][0][2] a[1][0][3] a[1][0][4] a[1][0][5] a[1][0][6] a[1][0][7] a[1][0][8] 
+ a[1][0][9] a[1][0][10] a[1][0][11] a[1][0][12] a[1][0][13] a[1][0][14] 
+ a[1][0][15] a[1][0][16] a[1][0][17] a[1][0][18] a[1][0][19] a[1][0][20] 
+ a[1][0][21] a[1][0][22] a[1][0][23] a[1][0][24] a[1][0][25] a[1][0][26] 
+ a[1][0][27] a[1][0][28] a[1][0][29] a[1][0][30] a[1][0][31] a[1][0][32] 
+ a[1][0][33] a[1][0][34] a[1][0][35] a[1][0][36] a[1][0][37] a[1][0][38] 
+ a[1][0][39] a[1][0][40] a[1][0][41] a[1][0][42] a[1][0][43] a[1][0][44] 
+ a[1][0][45] a[1][0][46] a[1][0][47] a[1][0][48] a[1][0][49] a[1][0][50] 
+ a[1][0][51] a[1][0][52] a[1][0][53] a[1][0][54] a[1][0][55] a[1][0][56] 
+ a[1][0][57] a[1][0][58] a[1][0][59] a[1][0][60] a[1][0][61] a[1][0][62] 
+ a[1][0][63] GND hit[0][0] hit[0][1] hit[0][2] hit[0][3] hit[0][4] hit[0][5] 
+ hit[0][6] hit[0][7] hit[0][8] hit[0][9] hit[0][10] hit[0][11] hit[0][12] 
+ hit[0][13] hit[0][14] hit[0][15] hit[0][16] hit[0][17] hit[0][18] hit[0][19] 
+ hit[0][20] hit[0][21] hit[0][22] hit[0][23] hit[0][24] hit[0][25] hit[0][26] 
+ hit[0][27] hit[0][28] hit[0][29] hit[0][30] hit[0][31] hit[0][32] hit[0][33] 
+ hit[0][34] hit[0][35] hit[0][36] hit[0][37] hit[0][38] hit[0][39] hit[0][40] 
+ hit[0][41] hit[0][42] hit[0][43] hit[0][44] hit[0][45] hit[0][46] hit[0][47] 
+ hit[0][48] hit[0][49] hit[0][50] hit[0][51] hit[0][52] hit[0][53] hit[0][54] 
+ hit[0][55] hit[0][56] hit[0][57] hit[0][58] hit[0][59] hit[0][60] hit[0][61] 
+ hit[0][62] hit[0][63] lk[1] LR[0].0 LR[0].1 LR[1].0 LR[1].1 LR[2].0 LR[2].1 
+ LR[3].0 LR[3].1 LR[4].0 LR[4].1 LR[5].0 LR[5].1 LR[6].0 LR[6].1 LR[7].0 
+ LR[7].1 LR[8].0 LR[8].1 LR[9].0 LR[9].1 LR[10].0 LR[10].1 LR[11].0 LR[11].1 
+ LR[12].0 LR[12].1 LR[13].0 LR[13].1 LR[14].0 LR[14].1 LR[15].0 LR[15].1 
+ LR[16].0 LR[16].1 LR[17].0 LR[17].1 LR[18].0 LR[18].1 LR[19].0 LR[19].1 
+ RR[0].0 RR[0].1 RR[1].0 RR[1].1 RR[2].0 RR[2].1 RR[3].0 RR[3].1 RR[4].0 
+ RR[4].1 RR[5].0 RR[5].1 RR[6].0 RR[6].1 RR[7].0 RR[7].1 RR[8].0 RR[8].1 
+ RR[9].0 RR[9].1 RR[10].0 RR[10].1 RR[11].0 RR[11].1 RR[12].0 RR[12].1 
+ RR[13].0 RR[13].1 RR[14].0 RR[14].1 RR[15].0 RR[15].1 RR[16].0 RR[16].1 
+ RR[17].0 RR[17].1 RR[18].0 RR[18].1 RR[19].0 RR[19].1 s[0] s[1] Vdd W[0].0 
+ W[0].1 W[1].0 W[1].1 W[2].0 W[2].1 W[3].0 W[3].1 W[4].0 W[4].1 W[5].0 W[5].1 
+ W[6].0 W[6].1 W[7].0 W[7].1 W[8].0 W[8].1 W[9].0 W[9].1 W[10].0 W[10].1 
+ W[11].0 W[11].1 W[12].0 W[12].1 W[13].0 W[13].1 W[14].0 W[14].1 W[15].0 
+ W[15].1 W[16].0 W[16].1 W[17].0 W[17].1 W[18].0 W[18].1 W[19].0 W[19].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_chunk64_20_4000
Xchunk[1] a[0][1][0] a[0][1][1] a[0][1][2] a[0][1][3] a[0][1][4] a[0][1][5] 
+ a[0][1][6] a[0][1][7] a[0][1][8] a[0][1][9] a[0][1][10] a[0][1][11] 
+ a[0][1][12] a[0][1][13] a[0][1][14] a[0][1][15] a[0][1][16] a[0][1][17] 
+ a[0][1][18] a[0][1][19] a[0][1][20] a[0][1][21] a[0][1][22] a[0][1][23] 
+ a[0][1][24] a[0][1][25] a[0][1][26] a[0][1][27] a[0][1][28] a[0][1][29] 
+ a[0][1][30] a[0][1][31] a[0][1][32] a[0][1][33] a[0][1][34] a[0][1][35] 
+ a[0][1][36] a[0][1][37] a[0][1][38] a[0][1][39] a[0][1][40] a[0][1][41] 
+ a[0][1][42] a[0][1][43] a[0][1][44] a[0][1][45] a[0][1][46] a[0][1][47] 
+ a[0][1][48] a[0][1][49] a[0][1][50] a[0][1][51] a[0][1][52] a[0][1][53] 
+ a[0][1][54] a[0][1][55] a[0][1][56] a[0][1][57] a[0][1][58] a[0][1][59] 
+ a[0][1][60] a[0][1][61] a[0][1][62] a[0][1][63] a[1][1][0] a[1][1][1] 
+ a[1][1][2] a[1][1][3] a[1][1][4] a[1][1][5] a[1][1][6] a[1][1][7] a[1][1][8] 
+ a[1][1][9] a[1][1][10] a[1][1][11] a[1][1][12] a[1][1][13] a[1][1][14] 
+ a[1][1][15] a[1][1][16] a[1][1][17] a[1][1][18] a[1][1][19] a[1][1][20] 
+ a[1][1][21] a[1][1][22] a[1][1][23] a[1][1][24] a[1][1][25] a[1][1][26] 
+ a[1][1][27] a[1][1][28] a[1][1][29] a[1][1][30] a[1][1][31] a[1][1][32] 
+ a[1][1][33] a[1][1][34] a[1][1][35] a[1][1][36] a[1][1][37] a[1][1][38] 
+ a[1][1][39] a[1][1][40] a[1][1][41] a[1][1][42] a[1][1][43] a[1][1][44] 
+ a[1][1][45] a[1][1][46] a[1][1][47] a[1][1][48] a[1][1][49] a[1][1][50] 
+ a[1][1][51] a[1][1][52] a[1][1][53] a[1][1][54] a[1][1][55] a[1][1][56] 
+ a[1][1][57] a[1][1][58] a[1][1][59] a[1][1][60] a[1][1][61] a[1][1][62] 
+ a[1][1][63] GND hit[1][0] hit[1][1] hit[1][2] hit[1][3] hit[1][4] hit[1][5] 
+ hit[1][6] hit[1][7] hit[1][8] hit[1][9] hit[1][10] hit[1][11] hit[1][12] 
+ hit[1][13] hit[1][14] hit[1][15] hit[1][16] hit[1][17] hit[1][18] hit[1][19] 
+ hit[1][20] hit[1][21] hit[1][22] hit[1][23] hit[1][24] hit[1][25] hit[1][26] 
+ hit[1][27] hit[1][28] hit[1][29] hit[1][30] hit[1][31] hit[1][32] hit[1][33] 
+ hit[1][34] hit[1][35] hit[1][36] hit[1][37] hit[1][38] hit[1][39] hit[1][40] 
+ hit[1][41] hit[1][42] hit[1][43] hit[1][44] hit[1][45] hit[1][46] hit[1][47] 
+ hit[1][48] hit[1][49] hit[1][50] hit[1][51] hit[1][52] hit[1][53] hit[1][54] 
+ hit[1][55] hit[1][56] hit[1][57] hit[1][58] hit[1][59] hit[1][60] hit[1][61] 
+ hit[1][62] hit[1][63] lk[1] LR[20].0 LR[20].1 LR[21].0 LR[21].1 LR[22].0 
+ LR[22].1 LR[23].0 LR[23].1 LR[24].0 LR[24].1 LR[25].0 LR[25].1 LR[26].0 
+ LR[26].1 LR[27].0 LR[27].1 LR[28].0 LR[28].1 LR[29].0 LR[29].1 LR[30].0 
+ LR[30].1 LR[31].0 LR[31].1 LR[32].0 LR[32].1 LR[33].0 LR[33].1 LR[34].0 
+ LR[34].1 LR[35].0 LR[35].1 LR[36].0 LR[36].1 LR[37].0 LR[37].1 LR[38].0 
+ LR[38].1 LR[39].0 LR[39].1 RR[20].0 RR[20].1 RR[21].0 RR[21].1 RR[22].0 
+ RR[22].1 RR[23].0 RR[23].1 RR[24].0 RR[24].1 RR[25].0 RR[25].1 RR[26].0 
+ RR[26].1 RR[27].0 RR[27].1 RR[28].0 RR[28].1 RR[29].0 RR[29].1 RR[30].0 
+ RR[30].1 RR[31].0 RR[31].1 RR[32].0 RR[32].1 RR[33].0 RR[33].1 RR[34].0 
+ RR[34].1 RR[35].0 RR[35].1 RR[36].0 RR[36].1 RR[37].0 RR[37].1 RR[38].0 
+ RR[38].1 RR[39].0 RR[39].1 s[0] s[1] Vdd W[20].0 W[20].1 W[21].0 W[21].1 
+ W[22].0 W[22].1 W[23].0 W[23].1 W[24].0 W[24].1 W[25].0 W[25].1 W[26].0 
+ W[26].1 W[27].0 W[27].1 W[28].0 W[28].1 W[29].0 W[29].1 W[30].0 W[30].1 
+ W[31].0 W[31].1 W[32].0 W[32].1 W[33].0 W[33].1 W[34].0 W[34].1 W[35].0 
+ W[35].1 W[36].0 W[36].1 W[37].0 W[37].1 W[38].0 W[38].1 W[39].0 W[39].1 / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_chunk64_20_4000
Xdecap[0] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[1] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[2] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[3] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[4] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[5] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[6] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[7] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[8] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[9] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[10] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[11] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[12] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[13] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[14] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[15] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[16] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[17] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[18] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[19] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[20] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[21] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[22] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[23] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[24] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[25] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[26] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[27] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[28] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[29] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[30] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[31] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[32] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[33] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[34] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[35] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[36] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[37] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[38] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[39] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[40] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[41] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[42] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[43] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[44] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[45] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[46] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[47] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[48] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[49] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[50] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[51] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[52] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[53] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[54] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[55] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[56] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[57] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[58] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[59] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[60] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[61] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[62] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[63] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[64] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[65] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[66] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[67] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[68] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[69] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[70] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[71] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[72] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[73] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[74] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[75] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[76] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[77] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[78] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[79] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[80] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[81] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[82] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[83] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[84] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[85] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[86] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[87] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[88] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[89] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[90] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[91] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[92] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[93] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[94] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[95] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[96] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[97] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[98] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[99] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[100] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[101] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[102] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[103] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[104] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[105] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[106] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[107] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[108] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[109] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[110] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[111] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[112] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[113] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[114] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[115] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[116] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[117] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[118] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[119] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[120] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[121] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[122] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[123] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[124] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[125] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[126] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[127] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[128] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[129] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[130] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[131] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[132] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[133] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[134] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[135] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[136] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[137] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[138] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[139] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[140] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[141] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[142] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[143] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[144] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[145] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[146] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[147] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[148] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[149] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[150] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[151] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[152] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[153] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[154] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[155] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[156] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[157] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[158] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[159] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[160] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[161] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[162] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[163] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[164] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[165] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[166] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[167] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[168] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[169] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[170] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[171] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[172] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[173] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[174] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[175] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[176] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[177] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[178] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[179] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[180] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[181] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[182] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[183] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[184] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[185] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[186] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[187] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[188] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[189] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[190] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[191] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[192] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[193] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[194] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[195] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[196] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[197] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[198] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[199] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[200] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[201] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[202] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[203] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[204] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[205] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[206] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[207] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[208] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[209] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[210] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[211] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[212] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[213] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[214] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[215] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[216] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[217] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[218] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[219] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[220] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[221] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[222] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[223] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[224] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[225] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[226] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[227] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[228] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[229] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[230] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[231] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[232] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[233] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[234] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[235] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[236] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[237] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[238] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[239] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[240] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[241] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[242] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[243] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[244] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[245] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[246] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[247] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[248] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[249] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[250] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[251] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[252] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[253] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[254] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
Xdecap[255] GND Vdd / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_decap_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    wwc_tcam_512_40_4000
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000 ADDR[0] ADDR[1] ADDR[2] ADDR[3] ADDR[4] ADDR[5] 
+ ADDR[6] ADDR[7] ADDR[8] ADDR[9] CFG[0] CFG[1] CFG[2] CFG[3] CFG[4] CFG[5] 
+ CFG[6] CFG[7] CFG[8] CFG[9] CFG[10] CFG[11] CLK DATA[0] DATA[1] DATA[2] 
+ DATA[3] DATA[4] DATA[5] DATA[6] DATA[7] DATA[8] DATA[9] DATA[10] DATA[11] 
+ DATA[12] DATA[13] DATA[14] DATA[15] DATA[16] DATA[17] DATA[18] DATA[19] 
+ DATA[20] DATA[21] DATA[22] DATA[23] DATA[24] DATA[25] DATA[26] DATA[27] 
+ DATA[28] DATA[29] DATA[30] DATA[31] DATA[32] DATA[33] DATA[34] DATA[35] 
+ DATA[36] DATA[37] DATA[38] DATA[39] KEN LHIT[0] LHIT[1] LHIT[2] LHIT[3] 
+ LHIT[4] LHIT[5] LHIT[6] LHIT[7] LHIT[8] LHIT[9] LHIT[10] LHIT[11] LHIT[12] 
+ LHIT[13] LHIT[14] LHIT[15] LHIT[16] LHIT[17] LHIT[18] LHIT[19] LHIT[20] 
+ LHIT[21] LHIT[22] LHIT[23] LHIT[24] LHIT[25] LHIT[26] LHIT[27] LHIT[28] 
+ LHIT[29] LHIT[30] LHIT[31] LHIT[32] LHIT[33] LHIT[34] LHIT[35] LHIT[36] 
+ LHIT[37] LHIT[38] LHIT[39] LHIT[40] LHIT[41] LHIT[42] LHIT[43] LHIT[44] 
+ LHIT[45] LHIT[46] LHIT[47] LHIT[48] LHIT[49] LHIT[50] LHIT[51] LHIT[52] 
+ LHIT[53] LHIT[54] LHIT[55] LHIT[56] LHIT[57] LHIT[58] LHIT[59] LHIT[60] 
+ LHIT[61] LHIT[62] LHIT[63] LHIT[64] LHIT[65] LHIT[66] LHIT[67] LHIT[68] 
+ LHIT[69] LHIT[70] LHIT[71] LHIT[72] LHIT[73] LHIT[74] LHIT[75] LHIT[76] 
+ LHIT[77] LHIT[78] LHIT[79] LHIT[80] LHIT[81] LHIT[82] LHIT[83] LHIT[84] 
+ LHIT[85] LHIT[86] LHIT[87] LHIT[88] LHIT[89] LHIT[90] LHIT[91] LHIT[92] 
+ LHIT[93] LHIT[94] LHIT[95] LHIT[96] LHIT[97] LHIT[98] LHIT[99] LHIT[100] 
+ LHIT[101] LHIT[102] LHIT[103] LHIT[104] LHIT[105] LHIT[106] LHIT[107] 
+ LHIT[108] LHIT[109] LHIT[110] LHIT[111] LHIT[112] LHIT[113] LHIT[114] 
+ LHIT[115] LHIT[116] LHIT[117] LHIT[118] LHIT[119] LHIT[120] LHIT[121] 
+ LHIT[122] LHIT[123] LHIT[124] LHIT[125] LHIT[126] LHIT[127] LHIT[128] 
+ LHIT[129] LHIT[130] LHIT[131] LHIT[132] LHIT[133] LHIT[134] LHIT[135] 
+ LHIT[136] LHIT[137] LHIT[138] LHIT[139] LHIT[140] LHIT[141] LHIT[142] 
+ LHIT[143] LHIT[144] LHIT[145] LHIT[146] LHIT[147] LHIT[148] LHIT[149] 
+ LHIT[150] LHIT[151] LHIT[152] LHIT[153] LHIT[154] LHIT[155] LHIT[156] 
+ LHIT[157] LHIT[158] LHIT[159] LHIT[160] LHIT[161] LHIT[162] LHIT[163] 
+ LHIT[164] LHIT[165] LHIT[166] LHIT[167] LHIT[168] LHIT[169] LHIT[170] 
+ LHIT[171] LHIT[172] LHIT[173] LHIT[174] LHIT[175] LHIT[176] LHIT[177] 
+ LHIT[178] LHIT[179] LHIT[180] LHIT[181] LHIT[182] LHIT[183] LHIT[184] 
+ LHIT[185] LHIT[186] LHIT[187] LHIT[188] LHIT[189] LHIT[190] LHIT[191] 
+ LHIT[192] LHIT[193] LHIT[194] LHIT[195] LHIT[196] LHIT[197] LHIT[198] 
+ LHIT[199] LHIT[200] LHIT[201] LHIT[202] LHIT[203] LHIT[204] LHIT[205] 
+ LHIT[206] LHIT[207] LHIT[208] LHIT[209] LHIT[210] LHIT[211] LHIT[212] 
+ LHIT[213] LHIT[214] LHIT[215] LHIT[216] LHIT[217] LHIT[218] LHIT[219] 
+ LHIT[220] LHIT[221] LHIT[222] LHIT[223] LHIT[224] LHIT[225] LHIT[226] 
+ LHIT[227] LHIT[228] LHIT[229] LHIT[230] LHIT[231] LHIT[232] LHIT[233] 
+ LHIT[234] LHIT[235] LHIT[236] LHIT[237] LHIT[238] LHIT[239] LHIT[240] 
+ LHIT[241] LHIT[242] LHIT[243] LHIT[244] LHIT[245] LHIT[246] LHIT[247] 
+ LHIT[248] LHIT[249] LHIT[250] LHIT[251] LHIT[252] LHIT[253] LHIT[254] 
+ LHIT[255] LHIT[256] LHIT[257] LHIT[258] LHIT[259] LHIT[260] LHIT[261] 
+ LHIT[262] LHIT[263] LHIT[264] LHIT[265] LHIT[266] LHIT[267] LHIT[268] 
+ LHIT[269] LHIT[270] LHIT[271] LHIT[272] LHIT[273] LHIT[274] LHIT[275] 
+ LHIT[276] LHIT[277] LHIT[278] LHIT[279] LHIT[280] LHIT[281] LHIT[282] 
+ LHIT[283] LHIT[284] LHIT[285] LHIT[286] LHIT[287] LHIT[288] LHIT[289] 
+ LHIT[290] LHIT[291] LHIT[292] LHIT[293] LHIT[294] LHIT[295] LHIT[296] 
+ LHIT[297] LHIT[298] LHIT[299] LHIT[300] LHIT[301] LHIT[302] LHIT[303] 
+ LHIT[304] LHIT[305] LHIT[306] LHIT[307] LHIT[308] LHIT[309] LHIT[310] 
+ LHIT[311] LHIT[312] LHIT[313] LHIT[314] LHIT[315] LHIT[316] LHIT[317] 
+ LHIT[318] LHIT[319] LHIT[320] LHIT[321] LHIT[322] LHIT[323] LHIT[324] 
+ LHIT[325] LHIT[326] LHIT[327] LHIT[328] LHIT[329] LHIT[330] LHIT[331] 
+ LHIT[332] LHIT[333] LHIT[334] LHIT[335] LHIT[336] LHIT[337] LHIT[338] 
+ LHIT[339] LHIT[340] LHIT[341] LHIT[342] LHIT[343] LHIT[344] LHIT[345] 
+ LHIT[346] LHIT[347] LHIT[348] LHIT[349] LHIT[350] LHIT[351] LHIT[352] 
+ LHIT[353] LHIT[354] LHIT[355] LHIT[356] LHIT[357] LHIT[358] LHIT[359] 
+ LHIT[360] LHIT[361] LHIT[362] LHIT[363] LHIT[364] LHIT[365] LHIT[366] 
+ LHIT[367] LHIT[368] LHIT[369] LHIT[370] LHIT[371] LHIT[372] LHIT[373] 
+ LHIT[374] LHIT[375] LHIT[376] LHIT[377] LHIT[378] LHIT[379] LHIT[380] 
+ LHIT[381] LHIT[382] LHIT[383] LHIT[384] LHIT[385] LHIT[386] LHIT[387] 
+ LHIT[388] LHIT[389] LHIT[390] LHIT[391] LHIT[392] LHIT[393] LHIT[394] 
+ LHIT[395] LHIT[396] LHIT[397] LHIT[398] LHIT[399] LHIT[400] LHIT[401] 
+ LHIT[402] LHIT[403] LHIT[404] LHIT[405] LHIT[406] LHIT[407] LHIT[408] 
+ LHIT[409] LHIT[410] LHIT[411] LHIT[412] LHIT[413] LHIT[414] LHIT[415] 
+ LHIT[416] LHIT[417] LHIT[418] LHIT[419] LHIT[420] LHIT[421] LHIT[422] 
+ LHIT[423] LHIT[424] LHIT[425] LHIT[426] LHIT[427] LHIT[428] LHIT[429] 
+ LHIT[430] LHIT[431] LHIT[432] LHIT[433] LHIT[434] LHIT[435] LHIT[436] 
+ LHIT[437] LHIT[438] LHIT[439] LHIT[440] LHIT[441] LHIT[442] LHIT[443] 
+ LHIT[444] LHIT[445] LHIT[446] LHIT[447] LHIT[448] LHIT[449] LHIT[450] 
+ LHIT[451] LHIT[452] LHIT[453] LHIT[454] LHIT[455] LHIT[456] LHIT[457] 
+ LHIT[458] LHIT[459] LHIT[460] LHIT[461] LHIT[462] LHIT[463] LHIT[464] 
+ LHIT[465] LHIT[466] LHIT[467] LHIT[468] LHIT[469] LHIT[470] LHIT[471] 
+ LHIT[472] LHIT[473] LHIT[474] LHIT[475] LHIT[476] LHIT[477] LHIT[478] 
+ LHIT[479] LHIT[480] LHIT[481] LHIT[482] LHIT[483] LHIT[484] LHIT[485] 
+ LHIT[486] LHIT[487] LHIT[488] LHIT[489] LHIT[490] LHIT[491] LHIT[492] 
+ LHIT[493] LHIT[494] LHIT[495] LHIT[496] LHIT[497] LHIT[498] LHIT[499] 
+ LHIT[500] LHIT[501] LHIT[502] LHIT[503] LHIT[504] LHIT[505] LHIT[506] 
+ LHIT[507] LHIT[508] LHIT[509] LHIT[510] LHIT[511] MASK[0] MASK[1] MASK[2] 
+ MASK[3] MASK[4] MASK[5] MASK[6] MASK[7] MASK[8] MASK[9] MASK[10] MASK[11] 
+ MASK[12] MASK[13] MASK[14] MASK[15] MASK[16] MASK[17] MASK[18] MASK[19] 
+ MASK[20] MASK[21] MASK[22] MASK[23] MASK[24] MASK[25] MASK[26] MASK[27] 
+ MASK[28] MASK[29] MASK[30] MASK[31] MASK[32] MASK[33] MASK[34] MASK[35] 
+ MASK[36] MASK[37] MASK[38] MASK[39] READ_DATA[0] READ_DATA[1] READ_DATA[2] 
+ READ_DATA[3] READ_DATA[4] READ_DATA[5] READ_DATA[6] READ_DATA[7] 
+ READ_DATA[8] READ_DATA[9] READ_DATA[10] READ_DATA[11] READ_DATA[12] 
+ READ_DATA[13] READ_DATA[14] READ_DATA[15] READ_DATA[16] READ_DATA[17] 
+ READ_DATA[18] READ_DATA[19] READ_DATA[20] READ_DATA[21] READ_DATA[22] 
+ READ_DATA[23] READ_DATA[24] READ_DATA[25] READ_DATA[26] READ_DATA[27] 
+ READ_DATA[28] READ_DATA[29] READ_DATA[30] READ_DATA[31] READ_DATA[32] 
+ READ_DATA[33] READ_DATA[34] READ_DATA[35] READ_DATA[36] READ_DATA[37] 
+ READ_DATA[38] READ_DATA[39] REN RESET_N RHIT[0] RHIT[1] RHIT[2] RHIT[3] 
+ RHIT[4] RHIT[5] RHIT[6] RHIT[7] RHIT[8] RHIT[9] RHIT[10] RHIT[11] RHIT[12] 
+ RHIT[13] RHIT[14] RHIT[15] RHIT[16] RHIT[17] RHIT[18] RHIT[19] RHIT[20] 
+ RHIT[21] RHIT[22] RHIT[23] RHIT[24] RHIT[25] RHIT[26] RHIT[27] RHIT[28] 
+ RHIT[29] RHIT[30] RHIT[31] RHIT[32] RHIT[33] RHIT[34] RHIT[35] RHIT[36] 
+ RHIT[37] RHIT[38] RHIT[39] RHIT[40] RHIT[41] RHIT[42] RHIT[43] RHIT[44] 
+ RHIT[45] RHIT[46] RHIT[47] RHIT[48] RHIT[49] RHIT[50] RHIT[51] RHIT[52] 
+ RHIT[53] RHIT[54] RHIT[55] RHIT[56] RHIT[57] RHIT[58] RHIT[59] RHIT[60] 
+ RHIT[61] RHIT[62] RHIT[63] RHIT[64] RHIT[65] RHIT[66] RHIT[67] RHIT[68] 
+ RHIT[69] RHIT[70] RHIT[71] RHIT[72] RHIT[73] RHIT[74] RHIT[75] RHIT[76] 
+ RHIT[77] RHIT[78] RHIT[79] RHIT[80] RHIT[81] RHIT[82] RHIT[83] RHIT[84] 
+ RHIT[85] RHIT[86] RHIT[87] RHIT[88] RHIT[89] RHIT[90] RHIT[91] RHIT[92] 
+ RHIT[93] RHIT[94] RHIT[95] RHIT[96] RHIT[97] RHIT[98] RHIT[99] RHIT[100] 
+ RHIT[101] RHIT[102] RHIT[103] RHIT[104] RHIT[105] RHIT[106] RHIT[107] 
+ RHIT[108] RHIT[109] RHIT[110] RHIT[111] RHIT[112] RHIT[113] RHIT[114] 
+ RHIT[115] RHIT[116] RHIT[117] RHIT[118] RHIT[119] RHIT[120] RHIT[121] 
+ RHIT[122] RHIT[123] RHIT[124] RHIT[125] RHIT[126] RHIT[127] RHIT[128] 
+ RHIT[129] RHIT[130] RHIT[131] RHIT[132] RHIT[133] RHIT[134] RHIT[135] 
+ RHIT[136] RHIT[137] RHIT[138] RHIT[139] RHIT[140] RHIT[141] RHIT[142] 
+ RHIT[143] RHIT[144] RHIT[145] RHIT[146] RHIT[147] RHIT[148] RHIT[149] 
+ RHIT[150] RHIT[151] RHIT[152] RHIT[153] RHIT[154] RHIT[155] RHIT[156] 
+ RHIT[157] RHIT[158] RHIT[159] RHIT[160] RHIT[161] RHIT[162] RHIT[163] 
+ RHIT[164] RHIT[165] RHIT[166] RHIT[167] RHIT[168] RHIT[169] RHIT[170] 
+ RHIT[171] RHIT[172] RHIT[173] RHIT[174] RHIT[175] RHIT[176] RHIT[177] 
+ RHIT[178] RHIT[179] RHIT[180] RHIT[181] RHIT[182] RHIT[183] RHIT[184] 
+ RHIT[185] RHIT[186] RHIT[187] RHIT[188] RHIT[189] RHIT[190] RHIT[191] 
+ RHIT[192] RHIT[193] RHIT[194] RHIT[195] RHIT[196] RHIT[197] RHIT[198] 
+ RHIT[199] RHIT[200] RHIT[201] RHIT[202] RHIT[203] RHIT[204] RHIT[205] 
+ RHIT[206] RHIT[207] RHIT[208] RHIT[209] RHIT[210] RHIT[211] RHIT[212] 
+ RHIT[213] RHIT[214] RHIT[215] RHIT[216] RHIT[217] RHIT[218] RHIT[219] 
+ RHIT[220] RHIT[221] RHIT[222] RHIT[223] RHIT[224] RHIT[225] RHIT[226] 
+ RHIT[227] RHIT[228] RHIT[229] RHIT[230] RHIT[231] RHIT[232] RHIT[233] 
+ RHIT[234] RHIT[235] RHIT[236] RHIT[237] RHIT[238] RHIT[239] RHIT[240] 
+ RHIT[241] RHIT[242] RHIT[243] RHIT[244] RHIT[245] RHIT[246] RHIT[247] 
+ RHIT[248] RHIT[249] RHIT[250] RHIT[251] RHIT[252] RHIT[253] RHIT[254] 
+ RHIT[255] RHIT[256] RHIT[257] RHIT[258] RHIT[259] RHIT[260] RHIT[261] 
+ RHIT[262] RHIT[263] RHIT[264] RHIT[265] RHIT[266] RHIT[267] RHIT[268] 
+ RHIT[269] RHIT[270] RHIT[271] RHIT[272] RHIT[273] RHIT[274] RHIT[275] 
+ RHIT[276] RHIT[277] RHIT[278] RHIT[279] RHIT[280] RHIT[281] RHIT[282] 
+ RHIT[283] RHIT[284] RHIT[285] RHIT[286] RHIT[287] RHIT[288] RHIT[289] 
+ RHIT[290] RHIT[291] RHIT[292] RHIT[293] RHIT[294] RHIT[295] RHIT[296] 
+ RHIT[297] RHIT[298] RHIT[299] RHIT[300] RHIT[301] RHIT[302] RHIT[303] 
+ RHIT[304] RHIT[305] RHIT[306] RHIT[307] RHIT[308] RHIT[309] RHIT[310] 
+ RHIT[311] RHIT[312] RHIT[313] RHIT[314] RHIT[315] RHIT[316] RHIT[317] 
+ RHIT[318] RHIT[319] RHIT[320] RHIT[321] RHIT[322] RHIT[323] RHIT[324] 
+ RHIT[325] RHIT[326] RHIT[327] RHIT[328] RHIT[329] RHIT[330] RHIT[331] 
+ RHIT[332] RHIT[333] RHIT[334] RHIT[335] RHIT[336] RHIT[337] RHIT[338] 
+ RHIT[339] RHIT[340] RHIT[341] RHIT[342] RHIT[343] RHIT[344] RHIT[345] 
+ RHIT[346] RHIT[347] RHIT[348] RHIT[349] RHIT[350] RHIT[351] RHIT[352] 
+ RHIT[353] RHIT[354] RHIT[355] RHIT[356] RHIT[357] RHIT[358] RHIT[359] 
+ RHIT[360] RHIT[361] RHIT[362] RHIT[363] RHIT[364] RHIT[365] RHIT[366] 
+ RHIT[367] RHIT[368] RHIT[369] RHIT[370] RHIT[371] RHIT[372] RHIT[373] 
+ RHIT[374] RHIT[375] RHIT[376] RHIT[377] RHIT[378] RHIT[379] RHIT[380] 
+ RHIT[381] RHIT[382] RHIT[383] RHIT[384] RHIT[385] RHIT[386] RHIT[387] 
+ RHIT[388] RHIT[389] RHIT[390] RHIT[391] RHIT[392] RHIT[393] RHIT[394] 
+ RHIT[395] RHIT[396] RHIT[397] RHIT[398] RHIT[399] RHIT[400] RHIT[401] 
+ RHIT[402] RHIT[403] RHIT[404] RHIT[405] RHIT[406] RHIT[407] RHIT[408] 
+ RHIT[409] RHIT[410] RHIT[411] RHIT[412] RHIT[413] RHIT[414] RHIT[415] 
+ RHIT[416] RHIT[417] RHIT[418] RHIT[419] RHIT[420] RHIT[421] RHIT[422] 
+ RHIT[423] RHIT[424] RHIT[425] RHIT[426] RHIT[427] RHIT[428] RHIT[429] 
+ RHIT[430] RHIT[431] RHIT[432] RHIT[433] RHIT[434] RHIT[435] RHIT[436] 
+ RHIT[437] RHIT[438] RHIT[439] RHIT[440] RHIT[441] RHIT[442] RHIT[443] 
+ RHIT[444] RHIT[445] RHIT[446] RHIT[447] RHIT[448] RHIT[449] RHIT[450] 
+ RHIT[451] RHIT[452] RHIT[453] RHIT[454] RHIT[455] RHIT[456] RHIT[457] 
+ RHIT[458] RHIT[459] RHIT[460] RHIT[461] RHIT[462] RHIT[463] RHIT[464] 
+ RHIT[465] RHIT[466] RHIT[467] RHIT[468] RHIT[469] RHIT[470] RHIT[471] 
+ RHIT[472] RHIT[473] RHIT[474] RHIT[475] RHIT[476] RHIT[477] RHIT[478] 
+ RHIT[479] RHIT[480] RHIT[481] RHIT[482] RHIT[483] RHIT[484] RHIT[485] 
+ RHIT[486] RHIT[487] RHIT[488] RHIT[489] RHIT[490] RHIT[491] RHIT[492] 
+ RHIT[493] RHIT[494] RHIT[495] RHIT[496] RHIT[497] RHIT[498] RHIT[499] 
+ RHIT[500] RHIT[501] RHIT[502] RHIT[503] RHIT[504] RHIT[505] RHIT[506] 
+ RHIT[507] RHIT[508] RHIT[509] RHIT[510] RHIT[511] SLICE_EN[0] SLICE_EN[1] 
+ SLICE_EN[2] SLICE_EN[3] SLICE_EN[4] SLICE_EN[5] SLICE_EN[6] SLICE_EN[7] VDD 
+ VSS WEN
*.PININFO ADDR[1]:I ADDR[2]:I ADDR[3]:I ADDR[4]:I ADDR[5]:I ADDR[6]:I 
*.PININFO ADDR[7]:I ADDR[8]:I ADDR[9]:I CFG[0]:I CFG[1]:I CFG[2]:I CFG[3]:I 
*.PININFO CFG[4]:I CFG[5]:I CFG[6]:I CFG[7]:I CFG[8]:I CFG[9]:I CFG[10]:I 
*.PININFO CFG[11]:I CLK:I DATA[0]:I DATA[1]:I DATA[2]:I DATA[3]:I DATA[4]:I 
*.PININFO DATA[5]:I DATA[6]:I DATA[7]:I DATA[8]:I DATA[9]:I DATA[10]:I 
*.PININFO DATA[11]:I DATA[12]:I DATA[13]:I DATA[14]:I DATA[15]:I DATA[16]:I 
*.PININFO DATA[17]:I DATA[18]:I DATA[19]:I DATA[20]:I DATA[21]:I DATA[22]:I 
*.PININFO DATA[23]:I DATA[24]:I DATA[25]:I DATA[26]:I DATA[27]:I DATA[28]:I 
*.PININFO DATA[29]:I DATA[30]:I DATA[31]:I DATA[32]:I DATA[33]:I DATA[34]:I 
*.PININFO DATA[35]:I DATA[36]:I DATA[37]:I DATA[38]:I DATA[39]:I KEN:I 
*.PININFO LHIT[0]:I LHIT[1]:I LHIT[2]:I LHIT[3]:I LHIT[4]:I LHIT[5]:I 
*.PININFO LHIT[6]:I LHIT[7]:I LHIT[8]:I LHIT[9]:I LHIT[10]:I LHIT[11]:I 
*.PININFO LHIT[12]:I LHIT[13]:I LHIT[14]:I LHIT[15]:I LHIT[16]:I LHIT[17]:I 
*.PININFO LHIT[18]:I LHIT[19]:I LHIT[20]:I LHIT[21]:I LHIT[22]:I LHIT[23]:I 
*.PININFO LHIT[24]:I LHIT[25]:I LHIT[26]:I LHIT[27]:I LHIT[28]:I LHIT[29]:I 
*.PININFO LHIT[30]:I LHIT[31]:I LHIT[32]:I LHIT[33]:I LHIT[34]:I LHIT[35]:I 
*.PININFO LHIT[36]:I LHIT[37]:I LHIT[38]:I LHIT[39]:I LHIT[40]:I LHIT[41]:I 
*.PININFO LHIT[42]:I LHIT[43]:I LHIT[44]:I LHIT[45]:I LHIT[46]:I LHIT[47]:I 
*.PININFO LHIT[48]:I LHIT[49]:I LHIT[50]:I LHIT[51]:I LHIT[52]:I LHIT[53]:I 
*.PININFO LHIT[54]:I LHIT[55]:I LHIT[56]:I LHIT[57]:I LHIT[58]:I LHIT[59]:I 
*.PININFO LHIT[60]:I LHIT[61]:I LHIT[62]:I LHIT[63]:I LHIT[64]:I LHIT[65]:I 
*.PININFO LHIT[66]:I LHIT[67]:I LHIT[68]:I LHIT[69]:I LHIT[70]:I LHIT[71]:I 
*.PININFO LHIT[72]:I LHIT[73]:I LHIT[74]:I LHIT[75]:I LHIT[76]:I LHIT[77]:I 
*.PININFO LHIT[78]:I LHIT[79]:I LHIT[80]:I LHIT[81]:I LHIT[82]:I LHIT[83]:I 
*.PININFO LHIT[84]:I LHIT[85]:I LHIT[86]:I LHIT[87]:I LHIT[88]:I LHIT[89]:I 
*.PININFO LHIT[90]:I LHIT[91]:I LHIT[92]:I LHIT[93]:I LHIT[94]:I LHIT[95]:I 
*.PININFO LHIT[96]:I LHIT[97]:I LHIT[98]:I LHIT[99]:I LHIT[100]:I LHIT[101]:I 
*.PININFO LHIT[102]:I LHIT[103]:I LHIT[104]:I LHIT[105]:I LHIT[106]:I 
*.PININFO LHIT[107]:I LHIT[108]:I LHIT[109]:I LHIT[110]:I LHIT[111]:I 
*.PININFO LHIT[112]:I LHIT[113]:I LHIT[114]:I LHIT[115]:I LHIT[116]:I 
*.PININFO LHIT[117]:I LHIT[118]:I LHIT[119]:I LHIT[120]:I LHIT[121]:I 
*.PININFO LHIT[122]:I LHIT[123]:I LHIT[124]:I LHIT[125]:I LHIT[126]:I 
*.PININFO LHIT[127]:I LHIT[128]:I LHIT[129]:I LHIT[130]:I LHIT[131]:I 
*.PININFO LHIT[132]:I LHIT[133]:I LHIT[134]:I LHIT[135]:I LHIT[136]:I 
*.PININFO LHIT[137]:I LHIT[138]:I LHIT[139]:I LHIT[140]:I LHIT[141]:I 
*.PININFO LHIT[142]:I LHIT[143]:I LHIT[144]:I LHIT[145]:I LHIT[146]:I 
*.PININFO LHIT[147]:I LHIT[148]:I LHIT[149]:I LHIT[150]:I LHIT[151]:I 
*.PININFO LHIT[152]:I LHIT[153]:I LHIT[154]:I LHIT[155]:I LHIT[156]:I 
*.PININFO LHIT[157]:I LHIT[158]:I LHIT[159]:I LHIT[160]:I LHIT[161]:I 
*.PININFO LHIT[162]:I LHIT[163]:I LHIT[164]:I LHIT[165]:I LHIT[166]:I 
*.PININFO LHIT[167]:I LHIT[168]:I LHIT[169]:I LHIT[170]:I LHIT[171]:I 
*.PININFO LHIT[172]:I LHIT[173]:I LHIT[174]:I LHIT[175]:I LHIT[176]:I 
*.PININFO LHIT[177]:I LHIT[178]:I LHIT[179]:I LHIT[180]:I LHIT[181]:I 
*.PININFO LHIT[182]:I LHIT[183]:I LHIT[184]:I LHIT[185]:I LHIT[186]:I 
*.PININFO LHIT[187]:I LHIT[188]:I LHIT[189]:I LHIT[190]:I LHIT[191]:I 
*.PININFO LHIT[192]:I LHIT[193]:I LHIT[194]:I LHIT[195]:I LHIT[196]:I 
*.PININFO LHIT[197]:I LHIT[198]:I LHIT[199]:I LHIT[200]:I LHIT[201]:I 
*.PININFO LHIT[202]:I LHIT[203]:I LHIT[204]:I LHIT[205]:I LHIT[206]:I 
*.PININFO LHIT[207]:I LHIT[208]:I LHIT[209]:I LHIT[210]:I LHIT[211]:I 
*.PININFO LHIT[212]:I LHIT[213]:I LHIT[214]:I LHIT[215]:I LHIT[216]:I 
*.PININFO LHIT[217]:I LHIT[218]:I LHIT[219]:I LHIT[220]:I LHIT[221]:I 
*.PININFO LHIT[222]:I LHIT[223]:I LHIT[224]:I LHIT[225]:I LHIT[226]:I 
*.PININFO LHIT[227]:I LHIT[228]:I LHIT[229]:I LHIT[230]:I LHIT[231]:I 
*.PININFO LHIT[232]:I LHIT[233]:I LHIT[234]:I LHIT[235]:I LHIT[236]:I 
*.PININFO LHIT[237]:I LHIT[238]:I LHIT[239]:I LHIT[240]:I LHIT[241]:I 
*.PININFO LHIT[242]:I LHIT[243]:I LHIT[244]:I LHIT[245]:I LHIT[246]:I 
*.PININFO LHIT[247]:I LHIT[248]:I LHIT[249]:I LHIT[250]:I LHIT[251]:I 
*.PININFO LHIT[252]:I LHIT[253]:I LHIT[254]:I LHIT[255]:I LHIT[256]:I 
*.PININFO LHIT[257]:I LHIT[258]:I LHIT[259]:I LHIT[260]:I LHIT[261]:I 
*.PININFO LHIT[262]:I LHIT[263]:I LHIT[264]:I LHIT[265]:I LHIT[266]:I 
*.PININFO LHIT[267]:I LHIT[268]:I LHIT[269]:I LHIT[270]:I LHIT[271]:I 
*.PININFO LHIT[272]:I LHIT[273]:I LHIT[274]:I LHIT[275]:I LHIT[276]:I 
*.PININFO LHIT[277]:I LHIT[278]:I LHIT[279]:I LHIT[280]:I LHIT[281]:I 
*.PININFO LHIT[282]:I LHIT[283]:I LHIT[284]:I LHIT[285]:I LHIT[286]:I 
*.PININFO LHIT[287]:I LHIT[288]:I LHIT[289]:I LHIT[290]:I LHIT[291]:I 
*.PININFO LHIT[292]:I LHIT[293]:I LHIT[294]:I LHIT[295]:I LHIT[296]:I 
*.PININFO LHIT[297]:I LHIT[298]:I LHIT[299]:I LHIT[300]:I LHIT[301]:I 
*.PININFO LHIT[302]:I LHIT[303]:I LHIT[304]:I LHIT[305]:I LHIT[306]:I 
*.PININFO LHIT[307]:I LHIT[308]:I LHIT[309]:I LHIT[310]:I LHIT[311]:I 
*.PININFO LHIT[312]:I LHIT[313]:I LHIT[314]:I LHIT[315]:I LHIT[316]:I 
*.PININFO LHIT[317]:I LHIT[318]:I LHIT[319]:I LHIT[320]:I LHIT[321]:I 
*.PININFO LHIT[322]:I LHIT[323]:I LHIT[324]:I LHIT[325]:I LHIT[326]:I 
*.PININFO LHIT[327]:I LHIT[328]:I LHIT[329]:I LHIT[330]:I LHIT[331]:I 
*.PININFO LHIT[332]:I LHIT[333]:I LHIT[334]:I LHIT[335]:I LHIT[336]:I 
*.PININFO LHIT[337]:I LHIT[338]:I LHIT[339]:I LHIT[340]:I LHIT[341]:I 
*.PININFO LHIT[342]:I LHIT[343]:I LHIT[344]:I LHIT[345]:I LHIT[346]:I 
*.PININFO LHIT[347]:I LHIT[348]:I LHIT[349]:I LHIT[350]:I LHIT[351]:I 
*.PININFO LHIT[352]:I LHIT[353]:I LHIT[354]:I LHIT[355]:I LHIT[356]:I 
*.PININFO LHIT[357]:I LHIT[358]:I LHIT[359]:I LHIT[360]:I LHIT[361]:I 
*.PININFO LHIT[362]:I LHIT[363]:I LHIT[364]:I LHIT[365]:I LHIT[366]:I 
*.PININFO LHIT[367]:I LHIT[368]:I LHIT[369]:I LHIT[370]:I LHIT[371]:I 
*.PININFO LHIT[372]:I LHIT[373]:I LHIT[374]:I LHIT[375]:I LHIT[376]:I 
*.PININFO LHIT[377]:I LHIT[378]:I LHIT[379]:I LHIT[380]:I LHIT[381]:I 
*.PININFO LHIT[382]:I LHIT[383]:I LHIT[384]:I LHIT[385]:I LHIT[386]:I 
*.PININFO LHIT[387]:I LHIT[388]:I LHIT[389]:I LHIT[390]:I LHIT[391]:I 
*.PININFO LHIT[392]:I LHIT[393]:I LHIT[394]:I LHIT[395]:I LHIT[396]:I 
*.PININFO LHIT[397]:I LHIT[398]:I LHIT[399]:I LHIT[400]:I LHIT[401]:I 
*.PININFO LHIT[402]:I LHIT[403]:I LHIT[404]:I LHIT[405]:I LHIT[406]:I 
*.PININFO LHIT[407]:I LHIT[408]:I LHIT[409]:I LHIT[410]:I LHIT[411]:I 
*.PININFO LHIT[412]:I LHIT[413]:I LHIT[414]:I LHIT[415]:I LHIT[416]:I 
*.PININFO LHIT[417]:I LHIT[418]:I LHIT[419]:I LHIT[420]:I LHIT[421]:I 
*.PININFO LHIT[422]:I LHIT[423]:I LHIT[424]:I LHIT[425]:I LHIT[426]:I 
*.PININFO LHIT[427]:I LHIT[428]:I LHIT[429]:I LHIT[430]:I LHIT[431]:I 
*.PININFO LHIT[432]:I LHIT[433]:I LHIT[434]:I LHIT[435]:I LHIT[436]:I 
*.PININFO LHIT[437]:I LHIT[438]:I LHIT[439]:I LHIT[440]:I LHIT[441]:I 
*.PININFO LHIT[442]:I LHIT[443]:I LHIT[444]:I LHIT[445]:I LHIT[446]:I 
*.PININFO LHIT[447]:I LHIT[448]:I LHIT[449]:I LHIT[450]:I LHIT[451]:I 
*.PININFO LHIT[452]:I LHIT[453]:I LHIT[454]:I LHIT[455]:I LHIT[456]:I 
*.PININFO LHIT[457]:I LHIT[458]:I LHIT[459]:I LHIT[460]:I LHIT[461]:I 
*.PININFO LHIT[462]:I LHIT[463]:I LHIT[464]:I LHIT[465]:I LHIT[466]:I 
*.PININFO LHIT[467]:I LHIT[468]:I LHIT[469]:I LHIT[470]:I LHIT[471]:I 
*.PININFO LHIT[472]:I LHIT[473]:I LHIT[474]:I LHIT[475]:I LHIT[476]:I 
*.PININFO LHIT[477]:I LHIT[478]:I LHIT[479]:I LHIT[480]:I LHIT[481]:I 
*.PININFO LHIT[482]:I LHIT[483]:I LHIT[484]:I LHIT[485]:I LHIT[486]:I 
*.PININFO LHIT[487]:I LHIT[488]:I LHIT[489]:I LHIT[490]:I LHIT[491]:I 
*.PININFO LHIT[492]:I LHIT[493]:I LHIT[494]:I LHIT[495]:I LHIT[496]:I 
*.PININFO LHIT[497]:I LHIT[498]:I LHIT[499]:I LHIT[500]:I LHIT[501]:I 
*.PININFO LHIT[502]:I LHIT[503]:I LHIT[504]:I LHIT[505]:I LHIT[506]:I 
*.PININFO LHIT[507]:I LHIT[508]:I LHIT[509]:I LHIT[510]:I LHIT[511]:I 
*.PININFO MASK[0]:I MASK[1]:I MASK[2]:I MASK[3]:I MASK[4]:I MASK[5]:I 
*.PININFO MASK[6]:I MASK[7]:I MASK[8]:I MASK[9]:I MASK[10]:I MASK[11]:I 
*.PININFO MASK[12]:I MASK[13]:I MASK[14]:I MASK[15]:I MASK[16]:I MASK[17]:I 
*.PININFO MASK[18]:I MASK[19]:I MASK[20]:I MASK[21]:I MASK[22]:I MASK[23]:I 
*.PININFO MASK[24]:I MASK[25]:I MASK[26]:I MASK[27]:I MASK[28]:I MASK[29]:I 
*.PININFO MASK[30]:I MASK[31]:I MASK[32]:I MASK[33]:I MASK[34]:I MASK[35]:I 
*.PININFO MASK[36]:I MASK[37]:I MASK[38]:I MASK[39]:I READ_DATA[0]:I 
*.PININFO READ_DATA[1]:I READ_DATA[2]:I READ_DATA[3]:I READ_DATA[4]:I 
*.PININFO READ_DATA[5]:I READ_DATA[6]:I READ_DATA[7]:I READ_DATA[8]:I 
*.PININFO READ_DATA[9]:I READ_DATA[10]:I READ_DATA[11]:I READ_DATA[12]:I 
*.PININFO READ_DATA[13]:I READ_DATA[14]:I READ_DATA[15]:I READ_DATA[16]:I 
*.PININFO READ_DATA[17]:I READ_DATA[18]:I READ_DATA[19]:I READ_DATA[20]:I 
*.PININFO READ_DATA[21]:I READ_DATA[22]:I READ_DATA[23]:I READ_DATA[24]:I 
*.PININFO READ_DATA[25]:I READ_DATA[26]:I READ_DATA[27]:I READ_DATA[28]:I 
*.PININFO READ_DATA[29]:I READ_DATA[30]:I READ_DATA[31]:I READ_DATA[32]:I 
*.PININFO READ_DATA[33]:I READ_DATA[34]:I READ_DATA[35]:I READ_DATA[36]:I 
*.PININFO READ_DATA[37]:I READ_DATA[38]:I READ_DATA[39]:I REN:I RESET_N:I 
*.PININFO RHIT[0]:I RHIT[1]:I RHIT[2]:I RHIT[3]:I RHIT[4]:I RHIT[5]:I 
*.PININFO RHIT[6]:I RHIT[7]:I RHIT[8]:I RHIT[9]:I RHIT[10]:I RHIT[11]:I 
*.PININFO RHIT[12]:I RHIT[13]:I RHIT[14]:I RHIT[15]:I RHIT[16]:I RHIT[17]:I 
*.PININFO RHIT[18]:I RHIT[19]:I RHIT[20]:I RHIT[21]:I RHIT[22]:I RHIT[23]:I 
*.PININFO RHIT[24]:I RHIT[25]:I RHIT[26]:I RHIT[27]:I RHIT[28]:I RHIT[29]:I 
*.PININFO RHIT[30]:I RHIT[31]:I RHIT[32]:I RHIT[33]:I RHIT[34]:I RHIT[35]:I 
*.PININFO RHIT[36]:I RHIT[37]:I RHIT[38]:I RHIT[39]:I RHIT[40]:I RHIT[41]:I 
*.PININFO RHIT[42]:I RHIT[43]:I RHIT[44]:I RHIT[45]:I RHIT[46]:I RHIT[47]:I 
*.PININFO RHIT[48]:I RHIT[49]:I RHIT[50]:I RHIT[51]:I RHIT[52]:I RHIT[53]:I 
*.PININFO RHIT[54]:I RHIT[55]:I RHIT[56]:I RHIT[57]:I RHIT[58]:I RHIT[59]:I 
*.PININFO RHIT[60]:I RHIT[61]:I RHIT[62]:I RHIT[63]:I RHIT[64]:I RHIT[65]:I 
*.PININFO RHIT[66]:I RHIT[67]:I RHIT[68]:I RHIT[69]:I RHIT[70]:I RHIT[71]:I 
*.PININFO RHIT[72]:I RHIT[73]:I RHIT[74]:I RHIT[75]:I RHIT[76]:I RHIT[77]:I 
*.PININFO RHIT[78]:I RHIT[79]:I RHIT[80]:I RHIT[81]:I RHIT[82]:I RHIT[83]:I 
*.PININFO RHIT[84]:I RHIT[85]:I RHIT[86]:I RHIT[87]:I RHIT[88]:I RHIT[89]:I 
*.PININFO RHIT[90]:I RHIT[91]:I RHIT[92]:I RHIT[93]:I RHIT[94]:I RHIT[95]:I 
*.PININFO RHIT[96]:I RHIT[97]:I RHIT[98]:I RHIT[99]:I RHIT[100]:I RHIT[101]:I 
*.PININFO RHIT[102]:I RHIT[103]:I RHIT[104]:I RHIT[105]:I RHIT[106]:I 
*.PININFO RHIT[107]:I RHIT[108]:I RHIT[109]:I RHIT[110]:I RHIT[111]:I 
*.PININFO RHIT[112]:I RHIT[113]:I RHIT[114]:I RHIT[115]:I RHIT[116]:I 
*.PININFO RHIT[117]:I RHIT[118]:I RHIT[119]:I RHIT[120]:I RHIT[121]:I 
*.PININFO RHIT[122]:I RHIT[123]:I RHIT[124]:I RHIT[125]:I RHIT[126]:I 
*.PININFO RHIT[127]:I RHIT[128]:I RHIT[129]:I RHIT[130]:I RHIT[131]:I 
*.PININFO RHIT[132]:I RHIT[133]:I RHIT[134]:I RHIT[135]:I RHIT[136]:I 
*.PININFO RHIT[137]:I RHIT[138]:I RHIT[139]:I RHIT[140]:I RHIT[141]:I 
*.PININFO RHIT[142]:I RHIT[143]:I RHIT[144]:I RHIT[145]:I RHIT[146]:I 
*.PININFO RHIT[147]:I RHIT[148]:I RHIT[149]:I RHIT[150]:I RHIT[151]:I 
*.PININFO RHIT[152]:I RHIT[153]:I RHIT[154]:I RHIT[155]:I RHIT[156]:I 
*.PININFO RHIT[157]:I RHIT[158]:I RHIT[159]:I RHIT[160]:I RHIT[161]:I 
*.PININFO RHIT[162]:I RHIT[163]:I RHIT[164]:I RHIT[165]:I RHIT[166]:I 
*.PININFO RHIT[167]:I RHIT[168]:I RHIT[169]:I RHIT[170]:I RHIT[171]:I 
*.PININFO RHIT[172]:I RHIT[173]:I RHIT[174]:I RHIT[175]:I RHIT[176]:I 
*.PININFO RHIT[177]:I RHIT[178]:I RHIT[179]:I RHIT[180]:I RHIT[181]:I 
*.PININFO RHIT[182]:I RHIT[183]:I RHIT[184]:I RHIT[185]:I RHIT[186]:I 
*.PININFO RHIT[187]:I RHIT[188]:I RHIT[189]:I RHIT[190]:I RHIT[191]:I 
*.PININFO RHIT[192]:I RHIT[193]:I RHIT[194]:I RHIT[195]:I RHIT[196]:I 
*.PININFO RHIT[197]:I RHIT[198]:I RHIT[199]:I RHIT[200]:I RHIT[201]:I 
*.PININFO RHIT[202]:I RHIT[203]:I RHIT[204]:I RHIT[205]:I RHIT[206]:I 
*.PININFO RHIT[207]:I RHIT[208]:I RHIT[209]:I RHIT[210]:I RHIT[211]:I 
*.PININFO RHIT[212]:I RHIT[213]:I RHIT[214]:I RHIT[215]:I RHIT[216]:I 
*.PININFO RHIT[217]:I RHIT[218]:I RHIT[219]:I RHIT[220]:I RHIT[221]:I 
*.PININFO RHIT[222]:I RHIT[223]:I RHIT[224]:I RHIT[225]:I RHIT[226]:I 
*.PININFO RHIT[227]:I RHIT[228]:I RHIT[229]:I RHIT[230]:I RHIT[231]:I 
*.PININFO RHIT[232]:I RHIT[233]:I RHIT[234]:I RHIT[235]:I RHIT[236]:I 
*.PININFO RHIT[237]:I RHIT[238]:I RHIT[239]:I RHIT[240]:I RHIT[241]:I 
*.PININFO RHIT[242]:I RHIT[243]:I RHIT[244]:I RHIT[245]:I RHIT[246]:I 
*.PININFO RHIT[247]:I RHIT[248]:I RHIT[249]:I RHIT[250]:I RHIT[251]:I 
*.PININFO RHIT[252]:I RHIT[253]:I RHIT[254]:I RHIT[255]:I RHIT[256]:I 
*.PININFO RHIT[257]:I RHIT[258]:I RHIT[259]:I RHIT[260]:I RHIT[261]:I 
*.PININFO RHIT[262]:I RHIT[263]:I RHIT[264]:I RHIT[265]:I RHIT[266]:I 
*.PININFO RHIT[267]:I RHIT[268]:I RHIT[269]:I RHIT[270]:I RHIT[271]:I 
*.PININFO RHIT[272]:I RHIT[273]:I RHIT[274]:I RHIT[275]:I RHIT[276]:I 
*.PININFO RHIT[277]:I RHIT[278]:I RHIT[279]:I RHIT[280]:I RHIT[281]:I 
*.PININFO RHIT[282]:I RHIT[283]:I RHIT[284]:I RHIT[285]:I RHIT[286]:I 
*.PININFO RHIT[287]:I RHIT[288]:I RHIT[289]:I RHIT[290]:I RHIT[291]:I 
*.PININFO RHIT[292]:I RHIT[293]:I RHIT[294]:I RHIT[295]:I RHIT[296]:I 
*.PININFO RHIT[297]:I RHIT[298]:I RHIT[299]:I RHIT[300]:I RHIT[301]:I 
*.PININFO RHIT[302]:I RHIT[303]:I RHIT[304]:I RHIT[305]:I RHIT[306]:I 
*.PININFO RHIT[307]:I RHIT[308]:I RHIT[309]:I RHIT[310]:I RHIT[311]:I 
*.PININFO RHIT[312]:I RHIT[313]:I RHIT[314]:I RHIT[315]:I RHIT[316]:I 
*.PININFO RHIT[317]:I RHIT[318]:I RHIT[319]:I RHIT[320]:I RHIT[321]:I 
*.PININFO RHIT[322]:I RHIT[323]:I RHIT[324]:I RHIT[325]:I RHIT[326]:I 
*.PININFO RHIT[327]:I RHIT[328]:I RHIT[329]:I RHIT[330]:I RHIT[331]:I 
*.PININFO RHIT[332]:I RHIT[333]:I RHIT[334]:I RHIT[335]:I RHIT[336]:I 
*.PININFO RHIT[337]:I RHIT[338]:I RHIT[339]:I RHIT[340]:I RHIT[341]:I 
*.PININFO RHIT[342]:I RHIT[343]:I RHIT[344]:I RHIT[345]:I RHIT[346]:I 
*.PININFO RHIT[347]:I RHIT[348]:I RHIT[349]:I RHIT[350]:I RHIT[351]:I 
*.PININFO RHIT[352]:I RHIT[353]:I RHIT[354]:I RHIT[355]:I RHIT[356]:I 
*.PININFO RHIT[357]:I RHIT[358]:I RHIT[359]:I RHIT[360]:I RHIT[361]:I 
*.PININFO RHIT[362]:I RHIT[363]:I RHIT[364]:I RHIT[365]:I RHIT[366]:I 
*.PININFO RHIT[367]:I RHIT[368]:I RHIT[369]:I RHIT[370]:I RHIT[371]:I 
*.PININFO RHIT[372]:I RHIT[373]:I RHIT[374]:I RHIT[375]:I RHIT[376]:I 
*.PININFO RHIT[377]:I RHIT[378]:I RHIT[379]:I RHIT[380]:I RHIT[381]:I 
*.PININFO RHIT[382]:I RHIT[383]:I RHIT[384]:I RHIT[385]:I RHIT[386]:I 
*.PININFO RHIT[387]:I RHIT[388]:I RHIT[389]:I RHIT[390]:I RHIT[391]:I 
*.PININFO RHIT[392]:I RHIT[393]:I RHIT[394]:I RHIT[395]:I RHIT[396]:I 
*.PININFO RHIT[397]:I RHIT[398]:I RHIT[399]:I RHIT[400]:I RHIT[401]:I 
*.PININFO RHIT[402]:I RHIT[403]:I RHIT[404]:I RHIT[405]:I RHIT[406]:I 
*.PININFO RHIT[407]:I RHIT[408]:I RHIT[409]:I RHIT[410]:I RHIT[411]:I 
*.PININFO RHIT[412]:I RHIT[413]:I RHIT[414]:I RHIT[415]:I RHIT[416]:I 
*.PININFO RHIT[417]:I RHIT[418]:I RHIT[419]:I RHIT[420]:I RHIT[421]:I 
*.PININFO RHIT[422]:I RHIT[423]:I RHIT[424]:I RHIT[425]:I RHIT[426]:I 
*.PININFO RHIT[427]:I RHIT[428]:I RHIT[429]:I RHIT[430]:I RHIT[431]:I 
*.PININFO RHIT[432]:I RHIT[433]:I RHIT[434]:I RHIT[435]:I RHIT[436]:I 
*.PININFO RHIT[437]:I RHIT[438]:I RHIT[439]:I RHIT[440]:I RHIT[441]:I 
*.PININFO RHIT[442]:I RHIT[443]:I RHIT[444]:I RHIT[445]:I RHIT[446]:I 
*.PININFO RHIT[447]:I RHIT[448]:I RHIT[449]:I RHIT[450]:I RHIT[451]:I 
*.PININFO RHIT[452]:I RHIT[453]:I RHIT[454]:I RHIT[455]:I RHIT[456]:I 
*.PININFO RHIT[457]:I RHIT[458]:I RHIT[459]:I RHIT[460]:I RHIT[461]:I 
*.PININFO RHIT[462]:I RHIT[463]:I RHIT[464]:I RHIT[465]:I RHIT[466]:I 
*.PININFO RHIT[467]:I RHIT[468]:I RHIT[469]:I RHIT[470]:I RHIT[471]:I 
*.PININFO RHIT[472]:I RHIT[473]:I RHIT[474]:I RHIT[475]:I RHIT[476]:I 
*.PININFO RHIT[477]:I RHIT[478]:I RHIT[479]:I RHIT[480]:I RHIT[481]:I 
*.PININFO RHIT[482]:I RHIT[483]:I RHIT[484]:I RHIT[485]:I RHIT[486]:I 
*.PININFO RHIT[487]:I RHIT[488]:I RHIT[489]:I RHIT[490]:I RHIT[491]:I 
*.PININFO RHIT[492]:I RHIT[493]:I RHIT[494]:I RHIT[495]:I RHIT[496]:I 
*.PININFO RHIT[497]:I RHIT[498]:I RHIT[499]:I RHIT[500]:I RHIT[501]:I 
*.PININFO RHIT[502]:I RHIT[503]:I RHIT[504]:I RHIT[505]:I RHIT[506]:I 
*.PININFO RHIT[507]:I RHIT[508]:I RHIT[509]:I RHIT[510]:I RHIT[511]:I 
*.PININFO SLICE_EN[0]:I SLICE_EN[1]:I SLICE_EN[2]:I SLICE_EN[3]:I 
*.PININFO SLICE_EN[4]:I SLICE_EN[5]:I SLICE_EN[6]:I SLICE_EN[7]:I VDD:I VSS:I 
*.PININFO WEN:I ADDR[0]:O
Xread[0] VSS r[8][0].0 r[8][0].1 READ_DATA[0] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[1] VSS r[8][1].0 r[8][1].1 READ_DATA[1] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[2] VSS r[8][2].0 r[8][2].1 READ_DATA[2] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[3] VSS r[8][3].0 r[8][3].1 READ_DATA[3] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[4] VSS r[8][4].0 r[8][4].1 READ_DATA[4] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[5] VSS r[8][5].0 r[8][5].1 READ_DATA[5] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[6] VSS r[8][6].0 r[8][6].1 READ_DATA[6] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[7] VSS r[8][7].0 r[8][7].1 READ_DATA[7] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[8] VSS r[8][8].0 r[8][8].1 READ_DATA[8] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[9] VSS r[8][9].0 r[8][9].1 READ_DATA[9] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[10] VSS r[8][10].0 r[8][10].1 READ_DATA[10] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[11] VSS r[8][11].0 r[8][11].1 READ_DATA[11] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[12] VSS r[8][12].0 r[8][12].1 READ_DATA[12] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[13] VSS r[8][13].0 r[8][13].1 READ_DATA[13] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[14] VSS r[8][14].0 r[8][14].1 READ_DATA[14] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[15] VSS r[8][15].0 r[8][15].1 READ_DATA[15] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[16] VSS r[8][16].0 r[8][16].1 READ_DATA[16] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[17] VSS r[8][17].0 r[8][17].1 READ_DATA[17] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[18] VSS r[8][18].0 r[8][18].1 READ_DATA[18] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[19] VSS r[8][19].0 r[8][19].1 READ_DATA[19] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[20] VSS r[8][20].0 r[8][20].1 READ_DATA[20] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[21] VSS r[8][21].0 r[8][21].1 READ_DATA[21] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[22] VSS r[8][22].0 r[8][22].1 READ_DATA[22] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[23] VSS r[8][23].0 r[8][23].1 READ_DATA[23] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[24] VSS r[8][24].0 r[8][24].1 READ_DATA[24] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[25] VSS r[8][25].0 r[8][25].1 READ_DATA[25] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[26] VSS r[8][26].0 r[8][26].1 READ_DATA[26] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[27] VSS r[8][27].0 r[8][27].1 READ_DATA[27] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[28] VSS r[8][28].0 r[8][28].1 READ_DATA[28] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[29] VSS r[8][29].0 r[8][29].1 READ_DATA[29] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[30] VSS r[8][30].0 r[8][30].1 READ_DATA[30] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[31] VSS r[8][31].0 r[8][31].1 READ_DATA[31] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[32] VSS r[8][32].0 r[8][32].1 READ_DATA[32] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[33] VSS r[8][33].0 r[8][33].1 READ_DATA[33] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[34] VSS r[8][34].0 r[8][34].1 READ_DATA[34] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[35] VSS r[8][35].0 r[8][35].1 READ_DATA[35] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[36] VSS r[8][36].0 r[8][36].1 READ_DATA[36] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[37] VSS r[8][37].0 r[8][37].1 READ_DATA[37] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[38] VSS r[8][38].0 r[8][38].1 READ_DATA[38] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xread[39] VSS r[8][39].0 r[8][39].1 READ_DATA[39] VDD / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_out_read_latch_4000
Xinputs addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 addr2[0][1].0 
+ addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 addr2[0][2].1 
+ addr2[0][2].2 addr2[0][2].3 addr2[0][3].0 addr2[0][3].1 addr2[0][3].2 
+ addr2[0][3].3 addr2[0][4].0 addr2[0][4].1 addr2[0][4].2 addr2[0][4].3 
+ ADDR[0] ADDR[1] ADDR[2] ADDR[3] ADDR[4] ADDR[5] ADDR[6] ADDR[7] ADDR[8] 
+ ADDR[9] ar aw CLK DATA[0] DATA[1] DATA[2] DATA[3] DATA[4] DATA[5] DATA[6] 
+ DATA[7] DATA[8] DATA[9] DATA[10] DATA[11] DATA[12] DATA[13] DATA[14] 
+ DATA[15] DATA[16] DATA[17] DATA[18] DATA[19] DATA[20] DATA[21] DATA[22] 
+ DATA[23] DATA[24] DATA[25] DATA[26] DATA[27] DATA[28] DATA[29] DATA[30] 
+ DATA[31] DATA[32] DATA[33] DATA[34] DATA[35] DATA[36] DATA[37] DATA[38] 
+ DATA[39] VSS KEN lk[0] lk[1] MASK[0] MASK[1] MASK[2] MASK[3] MASK[4] MASK[5] 
+ MASK[6] MASK[7] MASK[8] MASK[9] MASK[10] MASK[11] MASK[12] MASK[13] MASK[14] 
+ MASK[15] MASK[16] MASK[17] MASK[18] MASK[19] MASK[20] MASK[21] MASK[22] 
+ MASK[23] MASK[24] MASK[25] MASK[26] MASK[27] MASK[28] MASK[29] MASK[30] 
+ MASK[31] MASK[32] MASK[33] MASK[34] MASK[35] MASK[36] MASK[37] MASK[38] 
+ MASK[39] REN RESET_N VDD WEN w[0][0].0 w[0][0].1 w[0][1].0 w[0][1].1 
+ w[0][2].0 w[0][2].1 w[0][3].0 w[0][3].1 w[0][4].0 w[0][4].1 w[0][5].0 
+ w[0][5].1 w[0][6].0 w[0][6].1 w[0][7].0 w[0][7].1 w[0][8].0 w[0][8].1 
+ w[0][9].0 w[0][9].1 w[0][10].0 w[0][10].1 w[0][11].0 w[0][11].1 w[0][12].0 
+ w[0][12].1 w[0][13].0 w[0][13].1 w[0][14].0 w[0][14].1 w[0][15].0 w[0][15].1 
+ w[0][16].0 w[0][16].1 w[0][17].0 w[0][17].1 w[0][18].0 w[0][18].1 w[0][19].0 
+ w[0][19].1 w[0][20].0 w[0][20].1 w[0][21].0 w[0][21].1 w[0][22].0 w[0][22].1 
+ w[0][23].0 w[0][23].1 w[0][24].0 w[0][24].1 w[0][25].0 w[0][25].1 w[0][26].0 
+ w[0][26].1 w[0][27].0 w[0][27].1 w[0][28].0 w[0][28].1 w[0][29].0 w[0][29].1 
+ w[0][30].0 w[0][30].1 w[0][31].0 w[0][31].1 w[0][32].0 w[0][32].1 w[0][33].0 
+ w[0][33].1 w[0][34].0 w[0][34].1 w[0][35].0 w[0][35].1 w[0][36].0 w[0][36].1 
+ w[0][37].0 w[0][37].1 w[0][38].0 w[0][38].1 w[0][39].0 w[0][39].1 w[1][0].0 
+ w[1][0].1 w[1][1].0 w[1][1].1 w[1][2].0 w[1][2].1 w[1][3].0 w[1][3].1 
+ w[1][4].0 w[1][4].1 w[1][5].0 w[1][5].1 w[1][6].0 w[1][6].1 w[1][7].0 
+ w[1][7].1 w[1][8].0 w[1][8].1 w[1][9].0 w[1][9].1 w[1][10].0 w[1][10].1 
+ w[1][11].0 w[1][11].1 w[1][12].0 w[1][12].1 w[1][13].0 w[1][13].1 w[1][14].0 
+ w[1][14].1 w[1][15].0 w[1][15].1 w[1][16].0 w[1][16].1 w[1][17].0 w[1][17].1 
+ w[1][18].0 w[1][18].1 w[1][19].0 w[1][19].1 w[1][20].0 w[1][20].1 w[1][21].0 
+ w[1][21].1 w[1][22].0 w[1][22].1 w[1][23].0 w[1][23].1 w[1][24].0 w[1][24].1 
+ w[1][25].0 w[1][25].1 w[1][26].0 w[1][26].1 w[1][27].0 w[1][27].1 w[1][28].0 
+ w[1][28].1 w[1][29].0 w[1][29].1 w[1][30].0 w[1][30].1 w[1][31].0 w[1][31].1 
+ w[1][32].0 w[1][32].1 w[1][33].0 w[1][33].1 w[1][34].0 w[1][34].1 w[1][35].0 
+ w[1][35].1 w[1][36].0 w[1][36].1 w[1][37].0 w[1][37].1 w[1][38].0 w[1][38].1 
+ w[1][39].0 w[1][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_inputs_40_4000
Xslice[1] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].2 addr2[0][3].3 
+ addr2[0][4].0 ar aw VSS cfg[1][0] cfg[1][1] cfg[1][2] cfg[1][3] cfg[1][4] 
+ cfg[1][5] cfg[1][6] cfg[1][7] cfg[1][8] cfg[1][9] cfg[1][10] cfg[1][11] 
+ LHIT[64] LHIT[65] LHIT[66] LHIT[67] LHIT[68] LHIT[69] LHIT[70] LHIT[71] 
+ LHIT[72] LHIT[73] LHIT[74] LHIT[75] LHIT[76] LHIT[77] LHIT[78] LHIT[79] 
+ LHIT[80] LHIT[81] LHIT[82] LHIT[83] LHIT[84] LHIT[85] LHIT[86] LHIT[87] 
+ LHIT[88] LHIT[89] LHIT[90] LHIT[91] LHIT[92] LHIT[93] LHIT[94] LHIT[95] 
+ LHIT[96] LHIT[97] LHIT[98] LHIT[99] LHIT[100] LHIT[101] LHIT[102] LHIT[103] 
+ LHIT[104] LHIT[105] LHIT[106] LHIT[107] LHIT[108] LHIT[109] LHIT[110] 
+ LHIT[111] LHIT[112] LHIT[113] LHIT[114] LHIT[115] LHIT[116] LHIT[117] 
+ LHIT[118] LHIT[119] LHIT[120] LHIT[121] LHIT[122] LHIT[123] LHIT[124] 
+ LHIT[125] LHIT[126] LHIT[127] lk[0] r[1][0].0 r[1][0].1 r[1][1].0 r[1][1].1 
+ r[1][2].0 r[1][2].1 r[1][3].0 r[1][3].1 r[1][4].0 r[1][4].1 r[1][5].0 
+ r[1][5].1 r[1][6].0 r[1][6].1 r[1][7].0 r[1][7].1 r[1][8].0 r[1][8].1 
+ r[1][9].0 r[1][9].1 r[1][10].0 r[1][10].1 r[1][11].0 r[1][11].1 r[1][12].0 
+ r[1][12].1 r[1][13].0 r[1][13].1 r[1][14].0 r[1][14].1 r[1][15].0 r[1][15].1 
+ r[1][16].0 r[1][16].1 r[1][17].0 r[1][17].1 r[1][18].0 r[1][18].1 r[1][19].0 
+ r[1][19].1 r[1][20].0 r[1][20].1 r[1][21].0 r[1][21].1 r[1][22].0 r[1][22].1 
+ r[1][23].0 r[1][23].1 r[1][24].0 r[1][24].1 r[1][25].0 r[1][25].1 r[1][26].0 
+ r[1][26].1 r[1][27].0 r[1][27].1 r[1][28].0 r[1][28].1 r[1][29].0 r[1][29].1 
+ r[1][30].0 r[1][30].1 r[1][31].0 r[1][31].1 r[1][32].0 r[1][32].1 r[1][33].0 
+ r[1][33].1 r[1][34].0 r[1][34].1 r[1][35].0 r[1][35].1 r[1][36].0 r[1][36].1 
+ r[1][37].0 r[1][37].1 r[1][38].0 r[1][38].1 r[1][39].0 r[1][39].1 cfg[2][0] 
+ cfg[2][1] cfg[2][2] cfg[2][3] cfg[2][4] cfg[2][5] cfg[2][6] cfg[2][7] 
+ cfg[2][8] cfg[2][9] cfg[2][10] cfg[2][11] RHIT[64] RHIT[65] RHIT[66] 
+ RHIT[67] RHIT[68] RHIT[69] RHIT[70] RHIT[71] RHIT[72] RHIT[73] RHIT[74] 
+ RHIT[75] RHIT[76] RHIT[77] RHIT[78] RHIT[79] RHIT[80] RHIT[81] RHIT[82] 
+ RHIT[83] RHIT[84] RHIT[85] RHIT[86] RHIT[87] RHIT[88] RHIT[89] RHIT[90] 
+ RHIT[91] RHIT[92] RHIT[93] RHIT[94] RHIT[95] RHIT[96] RHIT[97] RHIT[98] 
+ RHIT[99] RHIT[100] RHIT[101] RHIT[102] RHIT[103] RHIT[104] RHIT[105] 
+ RHIT[106] RHIT[107] RHIT[108] RHIT[109] RHIT[110] RHIT[111] RHIT[112] 
+ RHIT[113] RHIT[114] RHIT[115] RHIT[116] RHIT[117] RHIT[118] RHIT[119] 
+ RHIT[120] RHIT[121] RHIT[122] RHIT[123] RHIT[124] RHIT[125] RHIT[126] 
+ RHIT[127] r[2][0].0 r[2][0].1 r[2][1].0 r[2][1].1 r[2][2].0 r[2][2].1 
+ r[2][3].0 r[2][3].1 r[2][4].0 r[2][4].1 r[2][5].0 r[2][5].1 r[2][6].0 
+ r[2][6].1 r[2][7].0 r[2][7].1 r[2][8].0 r[2][8].1 r[2][9].0 r[2][9].1 
+ r[2][10].0 r[2][10].1 r[2][11].0 r[2][11].1 r[2][12].0 r[2][12].1 r[2][13].0 
+ r[2][13].1 r[2][14].0 r[2][14].1 r[2][15].0 r[2][15].1 r[2][16].0 r[2][16].1 
+ r[2][17].0 r[2][17].1 r[2][18].0 r[2][18].1 r[2][19].0 r[2][19].1 r[2][20].0 
+ r[2][20].1 r[2][21].0 r[2][21].1 r[2][22].0 r[2][22].1 r[2][23].0 r[2][23].1 
+ r[2][24].0 r[2][24].1 r[2][25].0 r[2][25].1 r[2][26].0 r[2][26].1 r[2][27].0 
+ r[2][27].1 r[2][28].0 r[2][28].1 r[2][29].0 r[2][29].1 r[2][30].0 r[2][30].1 
+ r[2][31].0 r[2][31].1 r[2][32].0 r[2][32].1 r[2][33].0 r[2][33].1 r[2][34].0 
+ r[2][34].1 r[2][35].0 r[2][35].1 r[2][36].0 r[2][36].1 r[2][37].0 r[2][37].1 
+ r[2][38].0 r[2][38].1 r[2][39].0 r[2][39].1 SLICE_EN[1] VDD w[0][0].0 
+ w[0][0].1 w[0][1].0 w[0][1].1 w[0][2].0 w[0][2].1 w[0][3].0 w[0][3].1 
+ w[0][4].0 w[0][4].1 w[0][5].0 w[0][5].1 w[0][6].0 w[0][6].1 w[0][7].0 
+ w[0][7].1 w[0][8].0 w[0][8].1 w[0][9].0 w[0][9].1 w[0][10].0 w[0][10].1 
+ w[0][11].0 w[0][11].1 w[0][12].0 w[0][12].1 w[0][13].0 w[0][13].1 w[0][14].0 
+ w[0][14].1 w[0][15].0 w[0][15].1 w[0][16].0 w[0][16].1 w[0][17].0 w[0][17].1 
+ w[0][18].0 w[0][18].1 w[0][19].0 w[0][19].1 w[0][20].0 w[0][20].1 w[0][21].0 
+ w[0][21].1 w[0][22].0 w[0][22].1 w[0][23].0 w[0][23].1 w[0][24].0 w[0][24].1 
+ w[0][25].0 w[0][25].1 w[0][26].0 w[0][26].1 w[0][27].0 w[0][27].1 w[0][28].0 
+ w[0][28].1 w[0][29].0 w[0][29].1 w[0][30].0 w[0][30].1 w[0][31].0 w[0][31].1 
+ w[0][32].0 w[0][32].1 w[0][33].0 w[0][33].1 w[0][34].0 w[0][34].1 w[0][35].0 
+ w[0][35].1 w[0][36].0 w[0][36].1 w[0][37].0 w[0][37].1 w[0][38].0 w[0][38].1 
+ w[0][39].0 w[0][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[3] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].2 addr2[0][3].3 
+ addr2[0][4].1 ar aw VSS cfg[3][0] cfg[3][1] cfg[3][2] cfg[3][3] cfg[3][4] 
+ cfg[3][5] cfg[3][6] cfg[3][7] cfg[3][8] cfg[3][9] cfg[3][10] cfg[3][11] 
+ LHIT[192] LHIT[193] LHIT[194] LHIT[195] LHIT[196] LHIT[197] LHIT[198] 
+ LHIT[199] LHIT[200] LHIT[201] LHIT[202] LHIT[203] LHIT[204] LHIT[205] 
+ LHIT[206] LHIT[207] LHIT[208] LHIT[209] LHIT[210] LHIT[211] LHIT[212] 
+ LHIT[213] LHIT[214] LHIT[215] LHIT[216] LHIT[217] LHIT[218] LHIT[219] 
+ LHIT[220] LHIT[221] LHIT[222] LHIT[223] LHIT[224] LHIT[225] LHIT[226] 
+ LHIT[227] LHIT[228] LHIT[229] LHIT[230] LHIT[231] LHIT[232] LHIT[233] 
+ LHIT[234] LHIT[235] LHIT[236] LHIT[237] LHIT[238] LHIT[239] LHIT[240] 
+ LHIT[241] LHIT[242] LHIT[243] LHIT[244] LHIT[245] LHIT[246] LHIT[247] 
+ LHIT[248] LHIT[249] LHIT[250] LHIT[251] LHIT[252] LHIT[253] LHIT[254] 
+ LHIT[255] lk[0] r[3][0].0 r[3][0].1 r[3][1].0 r[3][1].1 r[3][2].0 r[3][2].1 
+ r[3][3].0 r[3][3].1 r[3][4].0 r[3][4].1 r[3][5].0 r[3][5].1 r[3][6].0 
+ r[3][6].1 r[3][7].0 r[3][7].1 r[3][8].0 r[3][8].1 r[3][9].0 r[3][9].1 
+ r[3][10].0 r[3][10].1 r[3][11].0 r[3][11].1 r[3][12].0 r[3][12].1 r[3][13].0 
+ r[3][13].1 r[3][14].0 r[3][14].1 r[3][15].0 r[3][15].1 r[3][16].0 r[3][16].1 
+ r[3][17].0 r[3][17].1 r[3][18].0 r[3][18].1 r[3][19].0 r[3][19].1 r[3][20].0 
+ r[3][20].1 r[3][21].0 r[3][21].1 r[3][22].0 r[3][22].1 r[3][23].0 r[3][23].1 
+ r[3][24].0 r[3][24].1 r[3][25].0 r[3][25].1 r[3][26].0 r[3][26].1 r[3][27].0 
+ r[3][27].1 r[3][28].0 r[3][28].1 r[3][29].0 r[3][29].1 r[3][30].0 r[3][30].1 
+ r[3][31].0 r[3][31].1 r[3][32].0 r[3][32].1 r[3][33].0 r[3][33].1 r[3][34].0 
+ r[3][34].1 r[3][35].0 r[3][35].1 r[3][36].0 r[3][36].1 r[3][37].0 r[3][37].1 
+ r[3][38].0 r[3][38].1 r[3][39].0 r[3][39].1 cfg[4][0] cfg[4][1] cfg[4][2] 
+ cfg[4][3] cfg[4][4] cfg[4][5] cfg[4][6] cfg[4][7] cfg[4][8] cfg[4][9] 
+ cfg[4][10] cfg[4][11] RHIT[192] RHIT[193] RHIT[194] RHIT[195] RHIT[196] 
+ RHIT[197] RHIT[198] RHIT[199] RHIT[200] RHIT[201] RHIT[202] RHIT[203] 
+ RHIT[204] RHIT[205] RHIT[206] RHIT[207] RHIT[208] RHIT[209] RHIT[210] 
+ RHIT[211] RHIT[212] RHIT[213] RHIT[214] RHIT[215] RHIT[216] RHIT[217] 
+ RHIT[218] RHIT[219] RHIT[220] RHIT[221] RHIT[222] RHIT[223] RHIT[224] 
+ RHIT[225] RHIT[226] RHIT[227] RHIT[228] RHIT[229] RHIT[230] RHIT[231] 
+ RHIT[232] RHIT[233] RHIT[234] RHIT[235] RHIT[236] RHIT[237] RHIT[238] 
+ RHIT[239] RHIT[240] RHIT[241] RHIT[242] RHIT[243] RHIT[244] RHIT[245] 
+ RHIT[246] RHIT[247] RHIT[248] RHIT[249] RHIT[250] RHIT[251] RHIT[252] 
+ RHIT[253] RHIT[254] RHIT[255] r[4][0].0 r[4][0].1 r[4][1].0 r[4][1].1 
+ r[4][2].0 r[4][2].1 r[4][3].0 r[4][3].1 r[4][4].0 r[4][4].1 r[4][5].0 
+ r[4][5].1 r[4][6].0 r[4][6].1 r[4][7].0 r[4][7].1 r[4][8].0 r[4][8].1 
+ r[4][9].0 r[4][9].1 r[4][10].0 r[4][10].1 r[4][11].0 r[4][11].1 r[4][12].0 
+ r[4][12].1 r[4][13].0 r[4][13].1 r[4][14].0 r[4][14].1 r[4][15].0 r[4][15].1 
+ r[4][16].0 r[4][16].1 r[4][17].0 r[4][17].1 r[4][18].0 r[4][18].1 r[4][19].0 
+ r[4][19].1 r[4][20].0 r[4][20].1 r[4][21].0 r[4][21].1 r[4][22].0 r[4][22].1 
+ r[4][23].0 r[4][23].1 r[4][24].0 r[4][24].1 r[4][25].0 r[4][25].1 r[4][26].0 
+ r[4][26].1 r[4][27].0 r[4][27].1 r[4][28].0 r[4][28].1 r[4][29].0 r[4][29].1 
+ r[4][30].0 r[4][30].1 r[4][31].0 r[4][31].1 r[4][32].0 r[4][32].1 r[4][33].0 
+ r[4][33].1 r[4][34].0 r[4][34].1 r[4][35].0 r[4][35].1 r[4][36].0 r[4][36].1 
+ r[4][37].0 r[4][37].1 r[4][38].0 r[4][38].1 r[4][39].0 r[4][39].1 
+ SLICE_EN[3] VDD w[0][0].0 w[0][0].1 w[0][1].0 w[0][1].1 w[0][2].0 w[0][2].1 
+ w[0][3].0 w[0][3].1 w[0][4].0 w[0][4].1 w[0][5].0 w[0][5].1 w[0][6].0 
+ w[0][6].1 w[0][7].0 w[0][7].1 w[0][8].0 w[0][8].1 w[0][9].0 w[0][9].1 
+ w[0][10].0 w[0][10].1 w[0][11].0 w[0][11].1 w[0][12].0 w[0][12].1 w[0][13].0 
+ w[0][13].1 w[0][14].0 w[0][14].1 w[0][15].0 w[0][15].1 w[0][16].0 w[0][16].1 
+ w[0][17].0 w[0][17].1 w[0][18].0 w[0][18].1 w[0][19].0 w[0][19].1 w[0][20].0 
+ w[0][20].1 w[0][21].0 w[0][21].1 w[0][22].0 w[0][22].1 w[0][23].0 w[0][23].1 
+ w[0][24].0 w[0][24].1 w[0][25].0 w[0][25].1 w[0][26].0 w[0][26].1 w[0][27].0 
+ w[0][27].1 w[0][28].0 w[0][28].1 w[0][29].0 w[0][29].1 w[0][30].0 w[0][30].1 
+ w[0][31].0 w[0][31].1 w[0][32].0 w[0][32].1 w[0][33].0 w[0][33].1 w[0][34].0 
+ w[0][34].1 w[0][35].0 w[0][35].1 w[0][36].0 w[0][36].1 w[0][37].0 w[0][37].1 
+ w[0][38].0 w[0][38].1 w[0][39].0 w[0][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[5] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].2 addr2[0][3].3 
+ addr2[0][4].2 ar aw VSS cfg[5][0] cfg[5][1] cfg[5][2] cfg[5][3] cfg[5][4] 
+ cfg[5][5] cfg[5][6] cfg[5][7] cfg[5][8] cfg[5][9] cfg[5][10] cfg[5][11] 
+ LHIT[320] LHIT[321] LHIT[322] LHIT[323] LHIT[324] LHIT[325] LHIT[326] 
+ LHIT[327] LHIT[328] LHIT[329] LHIT[330] LHIT[331] LHIT[332] LHIT[333] 
+ LHIT[334] LHIT[335] LHIT[336] LHIT[337] LHIT[338] LHIT[339] LHIT[340] 
+ LHIT[341] LHIT[342] LHIT[343] LHIT[344] LHIT[345] LHIT[346] LHIT[347] 
+ LHIT[348] LHIT[349] LHIT[350] LHIT[351] LHIT[352] LHIT[353] LHIT[354] 
+ LHIT[355] LHIT[356] LHIT[357] LHIT[358] LHIT[359] LHIT[360] LHIT[361] 
+ LHIT[362] LHIT[363] LHIT[364] LHIT[365] LHIT[366] LHIT[367] LHIT[368] 
+ LHIT[369] LHIT[370] LHIT[371] LHIT[372] LHIT[373] LHIT[374] LHIT[375] 
+ LHIT[376] LHIT[377] LHIT[378] LHIT[379] LHIT[380] LHIT[381] LHIT[382] 
+ LHIT[383] lk[1] r[5][0].0 r[5][0].1 r[5][1].0 r[5][1].1 r[5][2].0 r[5][2].1 
+ r[5][3].0 r[5][3].1 r[5][4].0 r[5][4].1 r[5][5].0 r[5][5].1 r[5][6].0 
+ r[5][6].1 r[5][7].0 r[5][7].1 r[5][8].0 r[5][8].1 r[5][9].0 r[5][9].1 
+ r[5][10].0 r[5][10].1 r[5][11].0 r[5][11].1 r[5][12].0 r[5][12].1 r[5][13].0 
+ r[5][13].1 r[5][14].0 r[5][14].1 r[5][15].0 r[5][15].1 r[5][16].0 r[5][16].1 
+ r[5][17].0 r[5][17].1 r[5][18].0 r[5][18].1 r[5][19].0 r[5][19].1 r[5][20].0 
+ r[5][20].1 r[5][21].0 r[5][21].1 r[5][22].0 r[5][22].1 r[5][23].0 r[5][23].1 
+ r[5][24].0 r[5][24].1 r[5][25].0 r[5][25].1 r[5][26].0 r[5][26].1 r[5][27].0 
+ r[5][27].1 r[5][28].0 r[5][28].1 r[5][29].0 r[5][29].1 r[5][30].0 r[5][30].1 
+ r[5][31].0 r[5][31].1 r[5][32].0 r[5][32].1 r[5][33].0 r[5][33].1 r[5][34].0 
+ r[5][34].1 r[5][35].0 r[5][35].1 r[5][36].0 r[5][36].1 r[5][37].0 r[5][37].1 
+ r[5][38].0 r[5][38].1 r[5][39].0 r[5][39].1 cfg[6][0] cfg[6][1] cfg[6][2] 
+ cfg[6][3] cfg[6][4] cfg[6][5] cfg[6][6] cfg[6][7] cfg[6][8] cfg[6][9] 
+ cfg[6][10] cfg[6][11] RHIT[320] RHIT[321] RHIT[322] RHIT[323] RHIT[324] 
+ RHIT[325] RHIT[326] RHIT[327] RHIT[328] RHIT[329] RHIT[330] RHIT[331] 
+ RHIT[332] RHIT[333] RHIT[334] RHIT[335] RHIT[336] RHIT[337] RHIT[338] 
+ RHIT[339] RHIT[340] RHIT[341] RHIT[342] RHIT[343] RHIT[344] RHIT[345] 
+ RHIT[346] RHIT[347] RHIT[348] RHIT[349] RHIT[350] RHIT[351] RHIT[352] 
+ RHIT[353] RHIT[354] RHIT[355] RHIT[356] RHIT[357] RHIT[358] RHIT[359] 
+ RHIT[360] RHIT[361] RHIT[362] RHIT[363] RHIT[364] RHIT[365] RHIT[366] 
+ RHIT[367] RHIT[368] RHIT[369] RHIT[370] RHIT[371] RHIT[372] RHIT[373] 
+ RHIT[374] RHIT[375] RHIT[376] RHIT[377] RHIT[378] RHIT[379] RHIT[380] 
+ RHIT[381] RHIT[382] RHIT[383] r[6][0].0 r[6][0].1 r[6][1].0 r[6][1].1 
+ r[6][2].0 r[6][2].1 r[6][3].0 r[6][3].1 r[6][4].0 r[6][4].1 r[6][5].0 
+ r[6][5].1 r[6][6].0 r[6][6].1 r[6][7].0 r[6][7].1 r[6][8].0 r[6][8].1 
+ r[6][9].0 r[6][9].1 r[6][10].0 r[6][10].1 r[6][11].0 r[6][11].1 r[6][12].0 
+ r[6][12].1 r[6][13].0 r[6][13].1 r[6][14].0 r[6][14].1 r[6][15].0 r[6][15].1 
+ r[6][16].0 r[6][16].1 r[6][17].0 r[6][17].1 r[6][18].0 r[6][18].1 r[6][19].0 
+ r[6][19].1 r[6][20].0 r[6][20].1 r[6][21].0 r[6][21].1 r[6][22].0 r[6][22].1 
+ r[6][23].0 r[6][23].1 r[6][24].0 r[6][24].1 r[6][25].0 r[6][25].1 r[6][26].0 
+ r[6][26].1 r[6][27].0 r[6][27].1 r[6][28].0 r[6][28].1 r[6][29].0 r[6][29].1 
+ r[6][30].0 r[6][30].1 r[6][31].0 r[6][31].1 r[6][32].0 r[6][32].1 r[6][33].0 
+ r[6][33].1 r[6][34].0 r[6][34].1 r[6][35].0 r[6][35].1 r[6][36].0 r[6][36].1 
+ r[6][37].0 r[6][37].1 r[6][38].0 r[6][38].1 r[6][39].0 r[6][39].1 
+ SLICE_EN[5] VDD w[1][0].0 w[1][0].1 w[1][1].0 w[1][1].1 w[1][2].0 w[1][2].1 
+ w[1][3].0 w[1][3].1 w[1][4].0 w[1][4].1 w[1][5].0 w[1][5].1 w[1][6].0 
+ w[1][6].1 w[1][7].0 w[1][7].1 w[1][8].0 w[1][8].1 w[1][9].0 w[1][9].1 
+ w[1][10].0 w[1][10].1 w[1][11].0 w[1][11].1 w[1][12].0 w[1][12].1 w[1][13].0 
+ w[1][13].1 w[1][14].0 w[1][14].1 w[1][15].0 w[1][15].1 w[1][16].0 w[1][16].1 
+ w[1][17].0 w[1][17].1 w[1][18].0 w[1][18].1 w[1][19].0 w[1][19].1 w[1][20].0 
+ w[1][20].1 w[1][21].0 w[1][21].1 w[1][22].0 w[1][22].1 w[1][23].0 w[1][23].1 
+ w[1][24].0 w[1][24].1 w[1][25].0 w[1][25].1 w[1][26].0 w[1][26].1 w[1][27].0 
+ w[1][27].1 w[1][28].0 w[1][28].1 w[1][29].0 w[1][29].1 w[1][30].0 w[1][30].1 
+ w[1][31].0 w[1][31].1 w[1][32].0 w[1][32].1 w[1][33].0 w[1][33].1 w[1][34].0 
+ w[1][34].1 w[1][35].0 w[1][35].1 w[1][36].0 w[1][36].1 w[1][37].0 w[1][37].1 
+ w[1][38].0 w[1][38].1 w[1][39].0 w[1][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[7] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].2 addr2[0][3].3 
+ addr2[0][4].3 ar aw VSS cfg[7][0] cfg[7][1] cfg[7][2] cfg[7][3] cfg[7][4] 
+ cfg[7][5] cfg[7][6] cfg[7][7] cfg[7][8] cfg[7][9] cfg[7][10] cfg[7][11] 
+ LHIT[448] LHIT[449] LHIT[450] LHIT[451] LHIT[452] LHIT[453] LHIT[454] 
+ LHIT[455] LHIT[456] LHIT[457] LHIT[458] LHIT[459] LHIT[460] LHIT[461] 
+ LHIT[462] LHIT[463] LHIT[464] LHIT[465] LHIT[466] LHIT[467] LHIT[468] 
+ LHIT[469] LHIT[470] LHIT[471] LHIT[472] LHIT[473] LHIT[474] LHIT[475] 
+ LHIT[476] LHIT[477] LHIT[478] LHIT[479] LHIT[480] LHIT[481] LHIT[482] 
+ LHIT[483] LHIT[484] LHIT[485] LHIT[486] LHIT[487] LHIT[488] LHIT[489] 
+ LHIT[490] LHIT[491] LHIT[492] LHIT[493] LHIT[494] LHIT[495] LHIT[496] 
+ LHIT[497] LHIT[498] LHIT[499] LHIT[500] LHIT[501] LHIT[502] LHIT[503] 
+ LHIT[504] LHIT[505] LHIT[506] LHIT[507] LHIT[508] LHIT[509] LHIT[510] 
+ LHIT[511] lk[1] r[7][0].0 r[7][0].1 r[7][1].0 r[7][1].1 r[7][2].0 r[7][2].1 
+ r[7][3].0 r[7][3].1 r[7][4].0 r[7][4].1 r[7][5].0 r[7][5].1 r[7][6].0 
+ r[7][6].1 r[7][7].0 r[7][7].1 r[7][8].0 r[7][8].1 r[7][9].0 r[7][9].1 
+ r[7][10].0 r[7][10].1 r[7][11].0 r[7][11].1 r[7][12].0 r[7][12].1 r[7][13].0 
+ r[7][13].1 r[7][14].0 r[7][14].1 r[7][15].0 r[7][15].1 r[7][16].0 r[7][16].1 
+ r[7][17].0 r[7][17].1 r[7][18].0 r[7][18].1 r[7][19].0 r[7][19].1 r[7][20].0 
+ r[7][20].1 r[7][21].0 r[7][21].1 r[7][22].0 r[7][22].1 r[7][23].0 r[7][23].1 
+ r[7][24].0 r[7][24].1 r[7][25].0 r[7][25].1 r[7][26].0 r[7][26].1 r[7][27].0 
+ r[7][27].1 r[7][28].0 r[7][28].1 r[7][29].0 r[7][29].1 r[7][30].0 r[7][30].1 
+ r[7][31].0 r[7][31].1 r[7][32].0 r[7][32].1 r[7][33].0 r[7][33].1 r[7][34].0 
+ r[7][34].1 r[7][35].0 r[7][35].1 r[7][36].0 r[7][36].1 r[7][37].0 r[7][37].1 
+ r[7][38].0 r[7][38].1 r[7][39].0 r[7][39].1 cfg[8][0] cfg[8][1] cfg[8][2] 
+ cfg[8][3] cfg[8][4] cfg[8][5] cfg[8][6] cfg[8][7] cfg[8][8] cfg[8][9] 
+ cfg[8][10] cfg[8][11] RHIT[448] RHIT[449] RHIT[450] RHIT[451] RHIT[452] 
+ RHIT[453] RHIT[454] RHIT[455] RHIT[456] RHIT[457] RHIT[458] RHIT[459] 
+ RHIT[460] RHIT[461] RHIT[462] RHIT[463] RHIT[464] RHIT[465] RHIT[466] 
+ RHIT[467] RHIT[468] RHIT[469] RHIT[470] RHIT[471] RHIT[472] RHIT[473] 
+ RHIT[474] RHIT[475] RHIT[476] RHIT[477] RHIT[478] RHIT[479] RHIT[480] 
+ RHIT[481] RHIT[482] RHIT[483] RHIT[484] RHIT[485] RHIT[486] RHIT[487] 
+ RHIT[488] RHIT[489] RHIT[490] RHIT[491] RHIT[492] RHIT[493] RHIT[494] 
+ RHIT[495] RHIT[496] RHIT[497] RHIT[498] RHIT[499] RHIT[500] RHIT[501] 
+ RHIT[502] RHIT[503] RHIT[504] RHIT[505] RHIT[506] RHIT[507] RHIT[508] 
+ RHIT[509] RHIT[510] RHIT[511] r[8][0].0 r[8][0].1 r[8][1].0 r[8][1].1 
+ r[8][2].0 r[8][2].1 r[8][3].0 r[8][3].1 r[8][4].0 r[8][4].1 r[8][5].0 
+ r[8][5].1 r[8][6].0 r[8][6].1 r[8][7].0 r[8][7].1 r[8][8].0 r[8][8].1 
+ r[8][9].0 r[8][9].1 r[8][10].0 r[8][10].1 r[8][11].0 r[8][11].1 r[8][12].0 
+ r[8][12].1 r[8][13].0 r[8][13].1 r[8][14].0 r[8][14].1 r[8][15].0 r[8][15].1 
+ r[8][16].0 r[8][16].1 r[8][17].0 r[8][17].1 r[8][18].0 r[8][18].1 r[8][19].0 
+ r[8][19].1 r[8][20].0 r[8][20].1 r[8][21].0 r[8][21].1 r[8][22].0 r[8][22].1 
+ r[8][23].0 r[8][23].1 r[8][24].0 r[8][24].1 r[8][25].0 r[8][25].1 r[8][26].0 
+ r[8][26].1 r[8][27].0 r[8][27].1 r[8][28].0 r[8][28].1 r[8][29].0 r[8][29].1 
+ r[8][30].0 r[8][30].1 r[8][31].0 r[8][31].1 r[8][32].0 r[8][32].1 r[8][33].0 
+ r[8][33].1 r[8][34].0 r[8][34].1 r[8][35].0 r[8][35].1 r[8][36].0 r[8][36].1 
+ r[8][37].0 r[8][37].1 r[8][38].0 r[8][38].1 r[8][39].0 r[8][39].1 
+ SLICE_EN[7] VDD w[1][0].0 w[1][0].1 w[1][1].0 w[1][1].1 w[1][2].0 w[1][2].1 
+ w[1][3].0 w[1][3].1 w[1][4].0 w[1][4].1 w[1][5].0 w[1][5].1 w[1][6].0 
+ w[1][6].1 w[1][7].0 w[1][7].1 w[1][8].0 w[1][8].1 w[1][9].0 w[1][9].1 
+ w[1][10].0 w[1][10].1 w[1][11].0 w[1][11].1 w[1][12].0 w[1][12].1 w[1][13].0 
+ w[1][13].1 w[1][14].0 w[1][14].1 w[1][15].0 w[1][15].1 w[1][16].0 w[1][16].1 
+ w[1][17].0 w[1][17].1 w[1][18].0 w[1][18].1 w[1][19].0 w[1][19].1 w[1][20].0 
+ w[1][20].1 w[1][21].0 w[1][21].1 w[1][22].0 w[1][22].1 w[1][23].0 w[1][23].1 
+ w[1][24].0 w[1][24].1 w[1][25].0 w[1][25].1 w[1][26].0 w[1][26].1 w[1][27].0 
+ w[1][27].1 w[1][28].0 w[1][28].1 w[1][29].0 w[1][29].1 w[1][30].0 w[1][30].1 
+ w[1][31].0 w[1][31].1 w[1][32].0 w[1][32].1 w[1][33].0 w[1][33].1 w[1][34].0 
+ w[1][34].1 w[1][35].0 w[1][35].1 w[1][36].0 w[1][36].1 w[1][37].0 w[1][37].1 
+ w[1][38].0 w[1][38].1 w[1][39].0 w[1][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[0] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].0 addr2[0][3].1 
+ addr2[0][4].0 ar aw VSS CFG[0] CFG[1] CFG[2] CFG[3] CFG[4] CFG[5] CFG[6] 
+ CFG[7] CFG[8] CFG[9] CFG[10] CFG[11] LHIT[0] LHIT[1] LHIT[2] LHIT[3] LHIT[4] 
+ LHIT[5] LHIT[6] LHIT[7] LHIT[8] LHIT[9] LHIT[10] LHIT[11] LHIT[12] LHIT[13] 
+ LHIT[14] LHIT[15] LHIT[16] LHIT[17] LHIT[18] LHIT[19] LHIT[20] LHIT[21] 
+ LHIT[22] LHIT[23] LHIT[24] LHIT[25] LHIT[26] LHIT[27] LHIT[28] LHIT[29] 
+ LHIT[30] LHIT[31] LHIT[32] LHIT[33] LHIT[34] LHIT[35] LHIT[36] LHIT[37] 
+ LHIT[38] LHIT[39] LHIT[40] LHIT[41] LHIT[42] LHIT[43] LHIT[44] LHIT[45] 
+ LHIT[46] LHIT[47] LHIT[48] LHIT[49] LHIT[50] LHIT[51] LHIT[52] LHIT[53] 
+ LHIT[54] LHIT[55] LHIT[56] LHIT[57] LHIT[58] LHIT[59] LHIT[60] LHIT[61] 
+ LHIT[62] LHIT[63] lk[0] VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS 
+ VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS 
+ VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS 
+ VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS 
+ VSS VSS VSS VSS VSS VSS VSS VSS VSS VSS cfg[1][0] cfg[1][1] cfg[1][2] 
+ cfg[1][3] cfg[1][4] cfg[1][5] cfg[1][6] cfg[1][7] cfg[1][8] cfg[1][9] 
+ cfg[1][10] cfg[1][11] RHIT[0] RHIT[1] RHIT[2] RHIT[3] RHIT[4] RHIT[5] 
+ RHIT[6] RHIT[7] RHIT[8] RHIT[9] RHIT[10] RHIT[11] RHIT[12] RHIT[13] RHIT[14] 
+ RHIT[15] RHIT[16] RHIT[17] RHIT[18] RHIT[19] RHIT[20] RHIT[21] RHIT[22] 
+ RHIT[23] RHIT[24] RHIT[25] RHIT[26] RHIT[27] RHIT[28] RHIT[29] RHIT[30] 
+ RHIT[31] RHIT[32] RHIT[33] RHIT[34] RHIT[35] RHIT[36] RHIT[37] RHIT[38] 
+ RHIT[39] RHIT[40] RHIT[41] RHIT[42] RHIT[43] RHIT[44] RHIT[45] RHIT[46] 
+ RHIT[47] RHIT[48] RHIT[49] RHIT[50] RHIT[51] RHIT[52] RHIT[53] RHIT[54] 
+ RHIT[55] RHIT[56] RHIT[57] RHIT[58] RHIT[59] RHIT[60] RHIT[61] RHIT[62] 
+ RHIT[63] r[1][0].0 r[1][0].1 r[1][1].0 r[1][1].1 r[1][2].0 r[1][2].1 
+ r[1][3].0 r[1][3].1 r[1][4].0 r[1][4].1 r[1][5].0 r[1][5].1 r[1][6].0 
+ r[1][6].1 r[1][7].0 r[1][7].1 r[1][8].0 r[1][8].1 r[1][9].0 r[1][9].1 
+ r[1][10].0 r[1][10].1 r[1][11].0 r[1][11].1 r[1][12].0 r[1][12].1 r[1][13].0 
+ r[1][13].1 r[1][14].0 r[1][14].1 r[1][15].0 r[1][15].1 r[1][16].0 r[1][16].1 
+ r[1][17].0 r[1][17].1 r[1][18].0 r[1][18].1 r[1][19].0 r[1][19].1 r[1][20].0 
+ r[1][20].1 r[1][21].0 r[1][21].1 r[1][22].0 r[1][22].1 r[1][23].0 r[1][23].1 
+ r[1][24].0 r[1][24].1 r[1][25].0 r[1][25].1 r[1][26].0 r[1][26].1 r[1][27].0 
+ r[1][27].1 r[1][28].0 r[1][28].1 r[1][29].0 r[1][29].1 r[1][30].0 r[1][30].1 
+ r[1][31].0 r[1][31].1 r[1][32].0 r[1][32].1 r[1][33].0 r[1][33].1 r[1][34].0 
+ r[1][34].1 r[1][35].0 r[1][35].1 r[1][36].0 r[1][36].1 r[1][37].0 r[1][37].1 
+ r[1][38].0 r[1][38].1 r[1][39].0 r[1][39].1 SLICE_EN[0] VDD w[0][0].0 
+ w[0][0].1 w[0][1].0 w[0][1].1 w[0][2].0 w[0][2].1 w[0][3].0 w[0][3].1 
+ w[0][4].0 w[0][4].1 w[0][5].0 w[0][5].1 w[0][6].0 w[0][6].1 w[0][7].0 
+ w[0][7].1 w[0][8].0 w[0][8].1 w[0][9].0 w[0][9].1 w[0][10].0 w[0][10].1 
+ w[0][11].0 w[0][11].1 w[0][12].0 w[0][12].1 w[0][13].0 w[0][13].1 w[0][14].0 
+ w[0][14].1 w[0][15].0 w[0][15].1 w[0][16].0 w[0][16].1 w[0][17].0 w[0][17].1 
+ w[0][18].0 w[0][18].1 w[0][19].0 w[0][19].1 w[0][20].0 w[0][20].1 w[0][21].0 
+ w[0][21].1 w[0][22].0 w[0][22].1 w[0][23].0 w[0][23].1 w[0][24].0 w[0][24].1 
+ w[0][25].0 w[0][25].1 w[0][26].0 w[0][26].1 w[0][27].0 w[0][27].1 w[0][28].0 
+ w[0][28].1 w[0][29].0 w[0][29].1 w[0][30].0 w[0][30].1 w[0][31].0 w[0][31].1 
+ w[0][32].0 w[0][32].1 w[0][33].0 w[0][33].1 w[0][34].0 w[0][34].1 w[0][35].0 
+ w[0][35].1 w[0][36].0 w[0][36].1 w[0][37].0 w[0][37].1 w[0][38].0 w[0][38].1 
+ w[0][39].0 w[0][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[4] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].0 addr2[0][3].1 
+ addr2[0][4].2 ar aw VSS cfg[4][0] cfg[4][1] cfg[4][2] cfg[4][3] cfg[4][4] 
+ cfg[4][5] cfg[4][6] cfg[4][7] cfg[4][8] cfg[4][9] cfg[4][10] cfg[4][11] 
+ LHIT[256] LHIT[257] LHIT[258] LHIT[259] LHIT[260] LHIT[261] LHIT[262] 
+ LHIT[263] LHIT[264] LHIT[265] LHIT[266] LHIT[267] LHIT[268] LHIT[269] 
+ LHIT[270] LHIT[271] LHIT[272] LHIT[273] LHIT[274] LHIT[275] LHIT[276] 
+ LHIT[277] LHIT[278] LHIT[279] LHIT[280] LHIT[281] LHIT[282] LHIT[283] 
+ LHIT[284] LHIT[285] LHIT[286] LHIT[287] LHIT[288] LHIT[289] LHIT[290] 
+ LHIT[291] LHIT[292] LHIT[293] LHIT[294] LHIT[295] LHIT[296] LHIT[297] 
+ LHIT[298] LHIT[299] LHIT[300] LHIT[301] LHIT[302] LHIT[303] LHIT[304] 
+ LHIT[305] LHIT[306] LHIT[307] LHIT[308] LHIT[309] LHIT[310] LHIT[311] 
+ LHIT[312] LHIT[313] LHIT[314] LHIT[315] LHIT[316] LHIT[317] LHIT[318] 
+ LHIT[319] lk[1] r[4][0].0 r[4][0].1 r[4][1].0 r[4][1].1 r[4][2].0 r[4][2].1 
+ r[4][3].0 r[4][3].1 r[4][4].0 r[4][4].1 r[4][5].0 r[4][5].1 r[4][6].0 
+ r[4][6].1 r[4][7].0 r[4][7].1 r[4][8].0 r[4][8].1 r[4][9].0 r[4][9].1 
+ r[4][10].0 r[4][10].1 r[4][11].0 r[4][11].1 r[4][12].0 r[4][12].1 r[4][13].0 
+ r[4][13].1 r[4][14].0 r[4][14].1 r[4][15].0 r[4][15].1 r[4][16].0 r[4][16].1 
+ r[4][17].0 r[4][17].1 r[4][18].0 r[4][18].1 r[4][19].0 r[4][19].1 r[4][20].0 
+ r[4][20].1 r[4][21].0 r[4][21].1 r[4][22].0 r[4][22].1 r[4][23].0 r[4][23].1 
+ r[4][24].0 r[4][24].1 r[4][25].0 r[4][25].1 r[4][26].0 r[4][26].1 r[4][27].0 
+ r[4][27].1 r[4][28].0 r[4][28].1 r[4][29].0 r[4][29].1 r[4][30].0 r[4][30].1 
+ r[4][31].0 r[4][31].1 r[4][32].0 r[4][32].1 r[4][33].0 r[4][33].1 r[4][34].0 
+ r[4][34].1 r[4][35].0 r[4][35].1 r[4][36].0 r[4][36].1 r[4][37].0 r[4][37].1 
+ r[4][38].0 r[4][38].1 r[4][39].0 r[4][39].1 cfg[5][0] cfg[5][1] cfg[5][2] 
+ cfg[5][3] cfg[5][4] cfg[5][5] cfg[5][6] cfg[5][7] cfg[5][8] cfg[5][9] 
+ cfg[5][10] cfg[5][11] RHIT[256] RHIT[257] RHIT[258] RHIT[259] RHIT[260] 
+ RHIT[261] RHIT[262] RHIT[263] RHIT[264] RHIT[265] RHIT[266] RHIT[267] 
+ RHIT[268] RHIT[269] RHIT[270] RHIT[271] RHIT[272] RHIT[273] RHIT[274] 
+ RHIT[275] RHIT[276] RHIT[277] RHIT[278] RHIT[279] RHIT[280] RHIT[281] 
+ RHIT[282] RHIT[283] RHIT[284] RHIT[285] RHIT[286] RHIT[287] RHIT[288] 
+ RHIT[289] RHIT[290] RHIT[291] RHIT[292] RHIT[293] RHIT[294] RHIT[295] 
+ RHIT[296] RHIT[297] RHIT[298] RHIT[299] RHIT[300] RHIT[301] RHIT[302] 
+ RHIT[303] RHIT[304] RHIT[305] RHIT[306] RHIT[307] RHIT[308] RHIT[309] 
+ RHIT[310] RHIT[311] RHIT[312] RHIT[313] RHIT[314] RHIT[315] RHIT[316] 
+ RHIT[317] RHIT[318] RHIT[319] r[5][0].0 r[5][0].1 r[5][1].0 r[5][1].1 
+ r[5][2].0 r[5][2].1 r[5][3].0 r[5][3].1 r[5][4].0 r[5][4].1 r[5][5].0 
+ r[5][5].1 r[5][6].0 r[5][6].1 r[5][7].0 r[5][7].1 r[5][8].0 r[5][8].1 
+ r[5][9].0 r[5][9].1 r[5][10].0 r[5][10].1 r[5][11].0 r[5][11].1 r[5][12].0 
+ r[5][12].1 r[5][13].0 r[5][13].1 r[5][14].0 r[5][14].1 r[5][15].0 r[5][15].1 
+ r[5][16].0 r[5][16].1 r[5][17].0 r[5][17].1 r[5][18].0 r[5][18].1 r[5][19].0 
+ r[5][19].1 r[5][20].0 r[5][20].1 r[5][21].0 r[5][21].1 r[5][22].0 r[5][22].1 
+ r[5][23].0 r[5][23].1 r[5][24].0 r[5][24].1 r[5][25].0 r[5][25].1 r[5][26].0 
+ r[5][26].1 r[5][27].0 r[5][27].1 r[5][28].0 r[5][28].1 r[5][29].0 r[5][29].1 
+ r[5][30].0 r[5][30].1 r[5][31].0 r[5][31].1 r[5][32].0 r[5][32].1 r[5][33].0 
+ r[5][33].1 r[5][34].0 r[5][34].1 r[5][35].0 r[5][35].1 r[5][36].0 r[5][36].1 
+ r[5][37].0 r[5][37].1 r[5][38].0 r[5][38].1 r[5][39].0 r[5][39].1 
+ SLICE_EN[4] VDD w[1][0].0 w[1][0].1 w[1][1].0 w[1][1].1 w[1][2].0 w[1][2].1 
+ w[1][3].0 w[1][3].1 w[1][4].0 w[1][4].1 w[1][5].0 w[1][5].1 w[1][6].0 
+ w[1][6].1 w[1][7].0 w[1][7].1 w[1][8].0 w[1][8].1 w[1][9].0 w[1][9].1 
+ w[1][10].0 w[1][10].1 w[1][11].0 w[1][11].1 w[1][12].0 w[1][12].1 w[1][13].0 
+ w[1][13].1 w[1][14].0 w[1][14].1 w[1][15].0 w[1][15].1 w[1][16].0 w[1][16].1 
+ w[1][17].0 w[1][17].1 w[1][18].0 w[1][18].1 w[1][19].0 w[1][19].1 w[1][20].0 
+ w[1][20].1 w[1][21].0 w[1][21].1 w[1][22].0 w[1][22].1 w[1][23].0 w[1][23].1 
+ w[1][24].0 w[1][24].1 w[1][25].0 w[1][25].1 w[1][26].0 w[1][26].1 w[1][27].0 
+ w[1][27].1 w[1][28].0 w[1][28].1 w[1][29].0 w[1][29].1 w[1][30].0 w[1][30].1 
+ w[1][31].0 w[1][31].1 w[1][32].0 w[1][32].1 w[1][33].0 w[1][33].1 w[1][34].0 
+ w[1][34].1 w[1][35].0 w[1][35].1 w[1][36].0 w[1][36].1 w[1][37].0 w[1][37].1 
+ w[1][38].0 w[1][38].1 w[1][39].0 w[1][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[2] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].0 addr2[0][3].1 
+ addr2[0][4].1 ar aw VSS cfg[2][0] cfg[2][1] cfg[2][2] cfg[2][3] cfg[2][4] 
+ cfg[2][5] cfg[2][6] cfg[2][7] cfg[2][8] cfg[2][9] cfg[2][10] cfg[2][11] 
+ LHIT[128] LHIT[129] LHIT[130] LHIT[131] LHIT[132] LHIT[133] LHIT[134] 
+ LHIT[135] LHIT[136] LHIT[137] LHIT[138] LHIT[139] LHIT[140] LHIT[141] 
+ LHIT[142] LHIT[143] LHIT[144] LHIT[145] LHIT[146] LHIT[147] LHIT[148] 
+ LHIT[149] LHIT[150] LHIT[151] LHIT[152] LHIT[153] LHIT[154] LHIT[155] 
+ LHIT[156] LHIT[157] LHIT[158] LHIT[159] LHIT[160] LHIT[161] LHIT[162] 
+ LHIT[163] LHIT[164] LHIT[165] LHIT[166] LHIT[167] LHIT[168] LHIT[169] 
+ LHIT[170] LHIT[171] LHIT[172] LHIT[173] LHIT[174] LHIT[175] LHIT[176] 
+ LHIT[177] LHIT[178] LHIT[179] LHIT[180] LHIT[181] LHIT[182] LHIT[183] 
+ LHIT[184] LHIT[185] LHIT[186] LHIT[187] LHIT[188] LHIT[189] LHIT[190] 
+ LHIT[191] lk[0] r[2][0].0 r[2][0].1 r[2][1].0 r[2][1].1 r[2][2].0 r[2][2].1 
+ r[2][3].0 r[2][3].1 r[2][4].0 r[2][4].1 r[2][5].0 r[2][5].1 r[2][6].0 
+ r[2][6].1 r[2][7].0 r[2][7].1 r[2][8].0 r[2][8].1 r[2][9].0 r[2][9].1 
+ r[2][10].0 r[2][10].1 r[2][11].0 r[2][11].1 r[2][12].0 r[2][12].1 r[2][13].0 
+ r[2][13].1 r[2][14].0 r[2][14].1 r[2][15].0 r[2][15].1 r[2][16].0 r[2][16].1 
+ r[2][17].0 r[2][17].1 r[2][18].0 r[2][18].1 r[2][19].0 r[2][19].1 r[2][20].0 
+ r[2][20].1 r[2][21].0 r[2][21].1 r[2][22].0 r[2][22].1 r[2][23].0 r[2][23].1 
+ r[2][24].0 r[2][24].1 r[2][25].0 r[2][25].1 r[2][26].0 r[2][26].1 r[2][27].0 
+ r[2][27].1 r[2][28].0 r[2][28].1 r[2][29].0 r[2][29].1 r[2][30].0 r[2][30].1 
+ r[2][31].0 r[2][31].1 r[2][32].0 r[2][32].1 r[2][33].0 r[2][33].1 r[2][34].0 
+ r[2][34].1 r[2][35].0 r[2][35].1 r[2][36].0 r[2][36].1 r[2][37].0 r[2][37].1 
+ r[2][38].0 r[2][38].1 r[2][39].0 r[2][39].1 cfg[3][0] cfg[3][1] cfg[3][2] 
+ cfg[3][3] cfg[3][4] cfg[3][5] cfg[3][6] cfg[3][7] cfg[3][8] cfg[3][9] 
+ cfg[3][10] cfg[3][11] RHIT[128] RHIT[129] RHIT[130] RHIT[131] RHIT[132] 
+ RHIT[133] RHIT[134] RHIT[135] RHIT[136] RHIT[137] RHIT[138] RHIT[139] 
+ RHIT[140] RHIT[141] RHIT[142] RHIT[143] RHIT[144] RHIT[145] RHIT[146] 
+ RHIT[147] RHIT[148] RHIT[149] RHIT[150] RHIT[151] RHIT[152] RHIT[153] 
+ RHIT[154] RHIT[155] RHIT[156] RHIT[157] RHIT[158] RHIT[159] RHIT[160] 
+ RHIT[161] RHIT[162] RHIT[163] RHIT[164] RHIT[165] RHIT[166] RHIT[167] 
+ RHIT[168] RHIT[169] RHIT[170] RHIT[171] RHIT[172] RHIT[173] RHIT[174] 
+ RHIT[175] RHIT[176] RHIT[177] RHIT[178] RHIT[179] RHIT[180] RHIT[181] 
+ RHIT[182] RHIT[183] RHIT[184] RHIT[185] RHIT[186] RHIT[187] RHIT[188] 
+ RHIT[189] RHIT[190] RHIT[191] r[3][0].0 r[3][0].1 r[3][1].0 r[3][1].1 
+ r[3][2].0 r[3][2].1 r[3][3].0 r[3][3].1 r[3][4].0 r[3][4].1 r[3][5].0 
+ r[3][5].1 r[3][6].0 r[3][6].1 r[3][7].0 r[3][7].1 r[3][8].0 r[3][8].1 
+ r[3][9].0 r[3][9].1 r[3][10].0 r[3][10].1 r[3][11].0 r[3][11].1 r[3][12].0 
+ r[3][12].1 r[3][13].0 r[3][13].1 r[3][14].0 r[3][14].1 r[3][15].0 r[3][15].1 
+ r[3][16].0 r[3][16].1 r[3][17].0 r[3][17].1 r[3][18].0 r[3][18].1 r[3][19].0 
+ r[3][19].1 r[3][20].0 r[3][20].1 r[3][21].0 r[3][21].1 r[3][22].0 r[3][22].1 
+ r[3][23].0 r[3][23].1 r[3][24].0 r[3][24].1 r[3][25].0 r[3][25].1 r[3][26].0 
+ r[3][26].1 r[3][27].0 r[3][27].1 r[3][28].0 r[3][28].1 r[3][29].0 r[3][29].1 
+ r[3][30].0 r[3][30].1 r[3][31].0 r[3][31].1 r[3][32].0 r[3][32].1 r[3][33].0 
+ r[3][33].1 r[3][34].0 r[3][34].1 r[3][35].0 r[3][35].1 r[3][36].0 r[3][36].1 
+ r[3][37].0 r[3][37].1 r[3][38].0 r[3][38].1 r[3][39].0 r[3][39].1 
+ SLICE_EN[2] VDD w[0][0].0 w[0][0].1 w[0][1].0 w[0][1].1 w[0][2].0 w[0][2].1 
+ w[0][3].0 w[0][3].1 w[0][4].0 w[0][4].1 w[0][5].0 w[0][5].1 w[0][6].0 
+ w[0][6].1 w[0][7].0 w[0][7].1 w[0][8].0 w[0][8].1 w[0][9].0 w[0][9].1 
+ w[0][10].0 w[0][10].1 w[0][11].0 w[0][11].1 w[0][12].0 w[0][12].1 w[0][13].0 
+ w[0][13].1 w[0][14].0 w[0][14].1 w[0][15].0 w[0][15].1 w[0][16].0 w[0][16].1 
+ w[0][17].0 w[0][17].1 w[0][18].0 w[0][18].1 w[0][19].0 w[0][19].1 w[0][20].0 
+ w[0][20].1 w[0][21].0 w[0][21].1 w[0][22].0 w[0][22].1 w[0][23].0 w[0][23].1 
+ w[0][24].0 w[0][24].1 w[0][25].0 w[0][25].1 w[0][26].0 w[0][26].1 w[0][27].0 
+ w[0][27].1 w[0][28].0 w[0][28].1 w[0][29].0 w[0][29].1 w[0][30].0 w[0][30].1 
+ w[0][31].0 w[0][31].1 w[0][32].0 w[0][32].1 w[0][33].0 w[0][33].1 w[0][34].0 
+ w[0][34].1 w[0][35].0 w[0][35].1 w[0][36].0 w[0][36].1 w[0][37].0 w[0][37].1 
+ w[0][38].0 w[0][38].1 w[0][39].0 w[0][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
Xslice[6] addr2[0][0].0 addr2[0][0].1 addr2[0][0].2 addr2[0][0].3 
+ addr2[0][1].0 addr2[0][1].1 addr2[0][1].2 addr2[0][1].3 addr2[0][2].0 
+ addr2[0][2].1 addr2[0][2].2 addr2[0][2].3 addr2[0][3].0 addr2[0][3].1 
+ addr2[0][4].3 ar aw VSS cfg[6][0] cfg[6][1] cfg[6][2] cfg[6][3] cfg[6][4] 
+ cfg[6][5] cfg[6][6] cfg[6][7] cfg[6][8] cfg[6][9] cfg[6][10] cfg[6][11] 
+ LHIT[384] LHIT[385] LHIT[386] LHIT[387] LHIT[388] LHIT[389] LHIT[390] 
+ LHIT[391] LHIT[392] LHIT[393] LHIT[394] LHIT[395] LHIT[396] LHIT[397] 
+ LHIT[398] LHIT[399] LHIT[400] LHIT[401] LHIT[402] LHIT[403] LHIT[404] 
+ LHIT[405] LHIT[406] LHIT[407] LHIT[408] LHIT[409] LHIT[410] LHIT[411] 
+ LHIT[412] LHIT[413] LHIT[414] LHIT[415] LHIT[416] LHIT[417] LHIT[418] 
+ LHIT[419] LHIT[420] LHIT[421] LHIT[422] LHIT[423] LHIT[424] LHIT[425] 
+ LHIT[426] LHIT[427] LHIT[428] LHIT[429] LHIT[430] LHIT[431] LHIT[432] 
+ LHIT[433] LHIT[434] LHIT[435] LHIT[436] LHIT[437] LHIT[438] LHIT[439] 
+ LHIT[440] LHIT[441] LHIT[442] LHIT[443] LHIT[444] LHIT[445] LHIT[446] 
+ LHIT[447] lk[1] r[6][0].0 r[6][0].1 r[6][1].0 r[6][1].1 r[6][2].0 r[6][2].1 
+ r[6][3].0 r[6][3].1 r[6][4].0 r[6][4].1 r[6][5].0 r[6][5].1 r[6][6].0 
+ r[6][6].1 r[6][7].0 r[6][7].1 r[6][8].0 r[6][8].1 r[6][9].0 r[6][9].1 
+ r[6][10].0 r[6][10].1 r[6][11].0 r[6][11].1 r[6][12].0 r[6][12].1 r[6][13].0 
+ r[6][13].1 r[6][14].0 r[6][14].1 r[6][15].0 r[6][15].1 r[6][16].0 r[6][16].1 
+ r[6][17].0 r[6][17].1 r[6][18].0 r[6][18].1 r[6][19].0 r[6][19].1 r[6][20].0 
+ r[6][20].1 r[6][21].0 r[6][21].1 r[6][22].0 r[6][22].1 r[6][23].0 r[6][23].1 
+ r[6][24].0 r[6][24].1 r[6][25].0 r[6][25].1 r[6][26].0 r[6][26].1 r[6][27].0 
+ r[6][27].1 r[6][28].0 r[6][28].1 r[6][29].0 r[6][29].1 r[6][30].0 r[6][30].1 
+ r[6][31].0 r[6][31].1 r[6][32].0 r[6][32].1 r[6][33].0 r[6][33].1 r[6][34].0 
+ r[6][34].1 r[6][35].0 r[6][35].1 r[6][36].0 r[6][36].1 r[6][37].0 r[6][37].1 
+ r[6][38].0 r[6][38].1 r[6][39].0 r[6][39].1 cfg[7][0] cfg[7][1] cfg[7][2] 
+ cfg[7][3] cfg[7][4] cfg[7][5] cfg[7][6] cfg[7][7] cfg[7][8] cfg[7][9] 
+ cfg[7][10] cfg[7][11] RHIT[384] RHIT[385] RHIT[386] RHIT[387] RHIT[388] 
+ RHIT[389] RHIT[390] RHIT[391] RHIT[392] RHIT[393] RHIT[394] RHIT[395] 
+ RHIT[396] RHIT[397] RHIT[398] RHIT[399] RHIT[400] RHIT[401] RHIT[402] 
+ RHIT[403] RHIT[404] RHIT[405] RHIT[406] RHIT[407] RHIT[408] RHIT[409] 
+ RHIT[410] RHIT[411] RHIT[412] RHIT[413] RHIT[414] RHIT[415] RHIT[416] 
+ RHIT[417] RHIT[418] RHIT[419] RHIT[420] RHIT[421] RHIT[422] RHIT[423] 
+ RHIT[424] RHIT[425] RHIT[426] RHIT[427] RHIT[428] RHIT[429] RHIT[430] 
+ RHIT[431] RHIT[432] RHIT[433] RHIT[434] RHIT[435] RHIT[436] RHIT[437] 
+ RHIT[438] RHIT[439] RHIT[440] RHIT[441] RHIT[442] RHIT[443] RHIT[444] 
+ RHIT[445] RHIT[446] RHIT[447] r[7][0].0 r[7][0].1 r[7][1].0 r[7][1].1 
+ r[7][2].0 r[7][2].1 r[7][3].0 r[7][3].1 r[7][4].0 r[7][4].1 r[7][5].0 
+ r[7][5].1 r[7][6].0 r[7][6].1 r[7][7].0 r[7][7].1 r[7][8].0 r[7][8].1 
+ r[7][9].0 r[7][9].1 r[7][10].0 r[7][10].1 r[7][11].0 r[7][11].1 r[7][12].0 
+ r[7][12].1 r[7][13].0 r[7][13].1 r[7][14].0 r[7][14].1 r[7][15].0 r[7][15].1 
+ r[7][16].0 r[7][16].1 r[7][17].0 r[7][17].1 r[7][18].0 r[7][18].1 r[7][19].0 
+ r[7][19].1 r[7][20].0 r[7][20].1 r[7][21].0 r[7][21].1 r[7][22].0 r[7][22].1 
+ r[7][23].0 r[7][23].1 r[7][24].0 r[7][24].1 r[7][25].0 r[7][25].1 r[7][26].0 
+ r[7][26].1 r[7][27].0 r[7][27].1 r[7][28].0 r[7][28].1 r[7][29].0 r[7][29].1 
+ r[7][30].0 r[7][30].1 r[7][31].0 r[7][31].1 r[7][32].0 r[7][32].1 r[7][33].0 
+ r[7][33].1 r[7][34].0 r[7][34].1 r[7][35].0 r[7][35].1 r[7][36].0 r[7][36].1 
+ r[7][37].0 r[7][37].1 r[7][38].0 r[7][38].1 r[7][39].0 r[7][39].1 
+ SLICE_EN[6] VDD w[1][0].0 w[1][0].1 w[1][1].0 w[1][1].1 w[1][2].0 w[1][2].1 
+ w[1][3].0 w[1][3].1 w[1][4].0 w[1][4].1 w[1][5].0 w[1][5].1 w[1][6].0 
+ w[1][6].1 w[1][7].0 w[1][7].1 w[1][8].0 w[1][8].1 w[1][9].0 w[1][9].1 
+ w[1][10].0 w[1][10].1 w[1][11].0 w[1][11].1 w[1][12].0 w[1][12].1 w[1][13].0 
+ w[1][13].1 w[1][14].0 w[1][14].1 w[1][15].0 w[1][15].1 w[1][16].0 w[1][16].1 
+ w[1][17].0 w[1][17].1 w[1][18].0 w[1][18].1 w[1][19].0 w[1][19].1 w[1][20].0 
+ w[1][20].1 w[1][21].0 w[1][21].1 w[1][22].0 w[1][22].1 w[1][23].0 w[1][23].1 
+ w[1][24].0 w[1][24].1 w[1][25].0 w[1][25].1 w[1][26].0 w[1][26].1 w[1][27].0 
+ w[1][27].1 w[1][28].0 w[1][28].1 w[1][29].0 w[1][29].1 w[1][30].0 w[1][30].1 
+ w[1][31].0 w[1][31].1 w[1][32].0 w[1][32].1 w[1][33].0 w[1][33].1 w[1][34].0 
+ w[1][34].1 w[1][35].0 w[1][35].1 w[1][36].0 w[1][36].1 w[1][37].0 w[1][37].1 
+ w[1][38].0 w[1][38].1 w[1][39].0 w[1][39].1 _RESET / ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_slice64_40_4000
.ENDS

************************************************************************
* Library Name: ip736hs3p111dtcam_f736_prd
* Cell Name:    ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl
* View Name:    schematic
************************************************************************

.SUBCKT ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl ADDR[0] ADDR[1] ADDR[2] ADDR[3] 
+ ADDR[4] ADDR[5] ADDR[6] ADDR[7] ADDR[8] ADDR[9] CFG[0] CFG[1] CFG[2] CFG[3] 
+ CFG[4] CFG[5] CFG[6] CFG[7] CFG[8] CFG[9] CFG[10] CFG[11] CLK DATA[0] 
+ DATA[1] DATA[2] DATA[3] DATA[4] DATA[5] DATA[6] DATA[7] DATA[8] DATA[9] 
+ DATA[10] DATA[11] DATA[12] DATA[13] DATA[14] DATA[15] DATA[16] DATA[17] 
+ DATA[18] DATA[19] DATA[20] DATA[21] DATA[22] DATA[23] DATA[24] DATA[25] 
+ DATA[26] DATA[27] DATA[28] DATA[29] DATA[30] DATA[31] DATA[32] DATA[33] 
+ DATA[34] DATA[35] DATA[36] DATA[37] DATA[38] DATA[39] KEN LHIT[0] LHIT[1] 
+ LHIT[2] LHIT[3] LHIT[4] LHIT[5] LHIT[6] LHIT[7] LHIT[8] LHIT[9] LHIT[10] 
+ LHIT[11] LHIT[12] LHIT[13] LHIT[14] LHIT[15] LHIT[16] LHIT[17] LHIT[18] 
+ LHIT[19] LHIT[20] LHIT[21] LHIT[22] LHIT[23] LHIT[24] LHIT[25] LHIT[26] 
+ LHIT[27] LHIT[28] LHIT[29] LHIT[30] LHIT[31] LHIT[32] LHIT[33] LHIT[34] 
+ LHIT[35] LHIT[36] LHIT[37] LHIT[38] LHIT[39] LHIT[40] LHIT[41] LHIT[42] 
+ LHIT[43] LHIT[44] LHIT[45] LHIT[46] LHIT[47] LHIT[48] LHIT[49] LHIT[50] 
+ LHIT[51] LHIT[52] LHIT[53] LHIT[54] LHIT[55] LHIT[56] LHIT[57] LHIT[58] 
+ LHIT[59] LHIT[60] LHIT[61] LHIT[62] LHIT[63] LHIT[64] LHIT[65] LHIT[66] 
+ LHIT[67] LHIT[68] LHIT[69] LHIT[70] LHIT[71] LHIT[72] LHIT[73] LHIT[74] 
+ LHIT[75] LHIT[76] LHIT[77] LHIT[78] LHIT[79] LHIT[80] LHIT[81] LHIT[82] 
+ LHIT[83] LHIT[84] LHIT[85] LHIT[86] LHIT[87] LHIT[88] LHIT[89] LHIT[90] 
+ LHIT[91] LHIT[92] LHIT[93] LHIT[94] LHIT[95] LHIT[96] LHIT[97] LHIT[98] 
+ LHIT[99] LHIT[100] LHIT[101] LHIT[102] LHIT[103] LHIT[104] LHIT[105] 
+ LHIT[106] LHIT[107] LHIT[108] LHIT[109] LHIT[110] LHIT[111] LHIT[112] 
+ LHIT[113] LHIT[114] LHIT[115] LHIT[116] LHIT[117] LHIT[118] LHIT[119] 
+ LHIT[120] LHIT[121] LHIT[122] LHIT[123] LHIT[124] LHIT[125] LHIT[126] 
+ LHIT[127] LHIT[128] LHIT[129] LHIT[130] LHIT[131] LHIT[132] LHIT[133] 
+ LHIT[134] LHIT[135] LHIT[136] LHIT[137] LHIT[138] LHIT[139] LHIT[140] 
+ LHIT[141] LHIT[142] LHIT[143] LHIT[144] LHIT[145] LHIT[146] LHIT[147] 
+ LHIT[148] LHIT[149] LHIT[150] LHIT[151] LHIT[152] LHIT[153] LHIT[154] 
+ LHIT[155] LHIT[156] LHIT[157] LHIT[158] LHIT[159] LHIT[160] LHIT[161] 
+ LHIT[162] LHIT[163] LHIT[164] LHIT[165] LHIT[166] LHIT[167] LHIT[168] 
+ LHIT[169] LHIT[170] LHIT[171] LHIT[172] LHIT[173] LHIT[174] LHIT[175] 
+ LHIT[176] LHIT[177] LHIT[178] LHIT[179] LHIT[180] LHIT[181] LHIT[182] 
+ LHIT[183] LHIT[184] LHIT[185] LHIT[186] LHIT[187] LHIT[188] LHIT[189] 
+ LHIT[190] LHIT[191] LHIT[192] LHIT[193] LHIT[194] LHIT[195] LHIT[196] 
+ LHIT[197] LHIT[198] LHIT[199] LHIT[200] LHIT[201] LHIT[202] LHIT[203] 
+ LHIT[204] LHIT[205] LHIT[206] LHIT[207] LHIT[208] LHIT[209] LHIT[210] 
+ LHIT[211] LHIT[212] LHIT[213] LHIT[214] LHIT[215] LHIT[216] LHIT[217] 
+ LHIT[218] LHIT[219] LHIT[220] LHIT[221] LHIT[222] LHIT[223] LHIT[224] 
+ LHIT[225] LHIT[226] LHIT[227] LHIT[228] LHIT[229] LHIT[230] LHIT[231] 
+ LHIT[232] LHIT[233] LHIT[234] LHIT[235] LHIT[236] LHIT[237] LHIT[238] 
+ LHIT[239] LHIT[240] LHIT[241] LHIT[242] LHIT[243] LHIT[244] LHIT[245] 
+ LHIT[246] LHIT[247] LHIT[248] LHIT[249] LHIT[250] LHIT[251] LHIT[252] 
+ LHIT[253] LHIT[254] LHIT[255] LHIT[256] LHIT[257] LHIT[258] LHIT[259] 
+ LHIT[260] LHIT[261] LHIT[262] LHIT[263] LHIT[264] LHIT[265] LHIT[266] 
+ LHIT[267] LHIT[268] LHIT[269] LHIT[270] LHIT[271] LHIT[272] LHIT[273] 
+ LHIT[274] LHIT[275] LHIT[276] LHIT[277] LHIT[278] LHIT[279] LHIT[280] 
+ LHIT[281] LHIT[282] LHIT[283] LHIT[284] LHIT[285] LHIT[286] LHIT[287] 
+ LHIT[288] LHIT[289] LHIT[290] LHIT[291] LHIT[292] LHIT[293] LHIT[294] 
+ LHIT[295] LHIT[296] LHIT[297] LHIT[298] LHIT[299] LHIT[300] LHIT[301] 
+ LHIT[302] LHIT[303] LHIT[304] LHIT[305] LHIT[306] LHIT[307] LHIT[308] 
+ LHIT[309] LHIT[310] LHIT[311] LHIT[312] LHIT[313] LHIT[314] LHIT[315] 
+ LHIT[316] LHIT[317] LHIT[318] LHIT[319] LHIT[320] LHIT[321] LHIT[322] 
+ LHIT[323] LHIT[324] LHIT[325] LHIT[326] LHIT[327] LHIT[328] LHIT[329] 
+ LHIT[330] LHIT[331] LHIT[332] LHIT[333] LHIT[334] LHIT[335] LHIT[336] 
+ LHIT[337] LHIT[338] LHIT[339] LHIT[340] LHIT[341] LHIT[342] LHIT[343] 
+ LHIT[344] LHIT[345] LHIT[346] LHIT[347] LHIT[348] LHIT[349] LHIT[350] 
+ LHIT[351] LHIT[352] LHIT[353] LHIT[354] LHIT[355] LHIT[356] LHIT[357] 
+ LHIT[358] LHIT[359] LHIT[360] LHIT[361] LHIT[362] LHIT[363] LHIT[364] 
+ LHIT[365] LHIT[366] LHIT[367] LHIT[368] LHIT[369] LHIT[370] LHIT[371] 
+ LHIT[372] LHIT[373] LHIT[374] LHIT[375] LHIT[376] LHIT[377] LHIT[378] 
+ LHIT[379] LHIT[380] LHIT[381] LHIT[382] LHIT[383] LHIT[384] LHIT[385] 
+ LHIT[386] LHIT[387] LHIT[388] LHIT[389] LHIT[390] LHIT[391] LHIT[392] 
+ LHIT[393] LHIT[394] LHIT[395] LHIT[396] LHIT[397] LHIT[398] LHIT[399] 
+ LHIT[400] LHIT[401] LHIT[402] LHIT[403] LHIT[404] LHIT[405] LHIT[406] 
+ LHIT[407] LHIT[408] LHIT[409] LHIT[410] LHIT[411] LHIT[412] LHIT[413] 
+ LHIT[414] LHIT[415] LHIT[416] LHIT[417] LHIT[418] LHIT[419] LHIT[420] 
+ LHIT[421] LHIT[422] LHIT[423] LHIT[424] LHIT[425] LHIT[426] LHIT[427] 
+ LHIT[428] LHIT[429] LHIT[430] LHIT[431] LHIT[432] LHIT[433] LHIT[434] 
+ LHIT[435] LHIT[436] LHIT[437] LHIT[438] LHIT[439] LHIT[440] LHIT[441] 
+ LHIT[442] LHIT[443] LHIT[444] LHIT[445] LHIT[446] LHIT[447] LHIT[448] 
+ LHIT[449] LHIT[450] LHIT[451] LHIT[452] LHIT[453] LHIT[454] LHIT[455] 
+ LHIT[456] LHIT[457] LHIT[458] LHIT[459] LHIT[460] LHIT[461] LHIT[462] 
+ LHIT[463] LHIT[464] LHIT[465] LHIT[466] LHIT[467] LHIT[468] LHIT[469] 
+ LHIT[470] LHIT[471] LHIT[472] LHIT[473] LHIT[474] LHIT[475] LHIT[476] 
+ LHIT[477] LHIT[478] LHIT[479] LHIT[480] LHIT[481] LHIT[482] LHIT[483] 
+ LHIT[484] LHIT[485] LHIT[486] LHIT[487] LHIT[488] LHIT[489] LHIT[490] 
+ LHIT[491] LHIT[492] LHIT[493] LHIT[494] LHIT[495] LHIT[496] LHIT[497] 
+ LHIT[498] LHIT[499] LHIT[500] LHIT[501] LHIT[502] LHIT[503] LHIT[504] 
+ LHIT[505] LHIT[506] LHIT[507] LHIT[508] LHIT[509] LHIT[510] LHIT[511] 
+ MASK[0] MASK[1] MASK[2] MASK[3] MASK[4] MASK[5] MASK[6] MASK[7] MASK[8] 
+ MASK[9] MASK[10] MASK[11] MASK[12] MASK[13] MASK[14] MASK[15] MASK[16] 
+ MASK[17] MASK[18] MASK[19] MASK[20] MASK[21] MASK[22] MASK[23] MASK[24] 
+ MASK[25] MASK[26] MASK[27] MASK[28] MASK[29] MASK[30] MASK[31] MASK[32] 
+ MASK[33] MASK[34] MASK[35] MASK[36] MASK[37] MASK[38] MASK[39] READ_DATA[0] 
+ READ_DATA[1] READ_DATA[2] READ_DATA[3] READ_DATA[4] READ_DATA[5] 
+ READ_DATA[6] READ_DATA[7] READ_DATA[8] READ_DATA[9] READ_DATA[10] 
+ READ_DATA[11] READ_DATA[12] READ_DATA[13] READ_DATA[14] READ_DATA[15] 
+ READ_DATA[16] READ_DATA[17] READ_DATA[18] READ_DATA[19] READ_DATA[20] 
+ READ_DATA[21] READ_DATA[22] READ_DATA[23] READ_DATA[24] READ_DATA[25] 
+ READ_DATA[26] READ_DATA[27] READ_DATA[28] READ_DATA[29] READ_DATA[30] 
+ READ_DATA[31] READ_DATA[32] READ_DATA[33] READ_DATA[34] READ_DATA[35] 
+ READ_DATA[36] READ_DATA[37] READ_DATA[38] READ_DATA[39] REN RESET_N RHIT[0] 
+ RHIT[1] RHIT[2] RHIT[3] RHIT[4] RHIT[5] RHIT[6] RHIT[7] RHIT[8] RHIT[9] 
+ RHIT[10] RHIT[11] RHIT[12] RHIT[13] RHIT[14] RHIT[15] RHIT[16] RHIT[17] 
+ RHIT[18] RHIT[19] RHIT[20] RHIT[21] RHIT[22] RHIT[23] RHIT[24] RHIT[25] 
+ RHIT[26] RHIT[27] RHIT[28] RHIT[29] RHIT[30] RHIT[31] RHIT[32] RHIT[33] 
+ RHIT[34] RHIT[35] RHIT[36] RHIT[37] RHIT[38] RHIT[39] RHIT[40] RHIT[41] 
+ RHIT[42] RHIT[43] RHIT[44] RHIT[45] RHIT[46] RHIT[47] RHIT[48] RHIT[49] 
+ RHIT[50] RHIT[51] RHIT[52] RHIT[53] RHIT[54] RHIT[55] RHIT[56] RHIT[57] 
+ RHIT[58] RHIT[59] RHIT[60] RHIT[61] RHIT[62] RHIT[63] RHIT[64] RHIT[65] 
+ RHIT[66] RHIT[67] RHIT[68] RHIT[69] RHIT[70] RHIT[71] RHIT[72] RHIT[73] 
+ RHIT[74] RHIT[75] RHIT[76] RHIT[77] RHIT[78] RHIT[79] RHIT[80] RHIT[81] 
+ RHIT[82] RHIT[83] RHIT[84] RHIT[85] RHIT[86] RHIT[87] RHIT[88] RHIT[89] 
+ RHIT[90] RHIT[91] RHIT[92] RHIT[93] RHIT[94] RHIT[95] RHIT[96] RHIT[97] 
+ RHIT[98] RHIT[99] RHIT[100] RHIT[101] RHIT[102] RHIT[103] RHIT[104] 
+ RHIT[105] RHIT[106] RHIT[107] RHIT[108] RHIT[109] RHIT[110] RHIT[111] 
+ RHIT[112] RHIT[113] RHIT[114] RHIT[115] RHIT[116] RHIT[117] RHIT[118] 
+ RHIT[119] RHIT[120] RHIT[121] RHIT[122] RHIT[123] RHIT[124] RHIT[125] 
+ RHIT[126] RHIT[127] RHIT[128] RHIT[129] RHIT[130] RHIT[131] RHIT[132] 
+ RHIT[133] RHIT[134] RHIT[135] RHIT[136] RHIT[137] RHIT[138] RHIT[139] 
+ RHIT[140] RHIT[141] RHIT[142] RHIT[143] RHIT[144] RHIT[145] RHIT[146] 
+ RHIT[147] RHIT[148] RHIT[149] RHIT[150] RHIT[151] RHIT[152] RHIT[153] 
+ RHIT[154] RHIT[155] RHIT[156] RHIT[157] RHIT[158] RHIT[159] RHIT[160] 
+ RHIT[161] RHIT[162] RHIT[163] RHIT[164] RHIT[165] RHIT[166] RHIT[167] 
+ RHIT[168] RHIT[169] RHIT[170] RHIT[171] RHIT[172] RHIT[173] RHIT[174] 
+ RHIT[175] RHIT[176] RHIT[177] RHIT[178] RHIT[179] RHIT[180] RHIT[181] 
+ RHIT[182] RHIT[183] RHIT[184] RHIT[185] RHIT[186] RHIT[187] RHIT[188] 
+ RHIT[189] RHIT[190] RHIT[191] RHIT[192] RHIT[193] RHIT[194] RHIT[195] 
+ RHIT[196] RHIT[197] RHIT[198] RHIT[199] RHIT[200] RHIT[201] RHIT[202] 
+ RHIT[203] RHIT[204] RHIT[205] RHIT[206] RHIT[207] RHIT[208] RHIT[209] 
+ RHIT[210] RHIT[211] RHIT[212] RHIT[213] RHIT[214] RHIT[215] RHIT[216] 
+ RHIT[217] RHIT[218] RHIT[219] RHIT[220] RHIT[221] RHIT[222] RHIT[223] 
+ RHIT[224] RHIT[225] RHIT[226] RHIT[227] RHIT[228] RHIT[229] RHIT[230] 
+ RHIT[231] RHIT[232] RHIT[233] RHIT[234] RHIT[235] RHIT[236] RHIT[237] 
+ RHIT[238] RHIT[239] RHIT[240] RHIT[241] RHIT[242] RHIT[243] RHIT[244] 
+ RHIT[245] RHIT[246] RHIT[247] RHIT[248] RHIT[249] RHIT[250] RHIT[251] 
+ RHIT[252] RHIT[253] RHIT[254] RHIT[255] RHIT[256] RHIT[257] RHIT[258] 
+ RHIT[259] RHIT[260] RHIT[261] RHIT[262] RHIT[263] RHIT[264] RHIT[265] 
+ RHIT[266] RHIT[267] RHIT[268] RHIT[269] RHIT[270] RHIT[271] RHIT[272] 
+ RHIT[273] RHIT[274] RHIT[275] RHIT[276] RHIT[277] RHIT[278] RHIT[279] 
+ RHIT[280] RHIT[281] RHIT[282] RHIT[283] RHIT[284] RHIT[285] RHIT[286] 
+ RHIT[287] RHIT[288] RHIT[289] RHIT[290] RHIT[291] RHIT[292] RHIT[293] 
+ RHIT[294] RHIT[295] RHIT[296] RHIT[297] RHIT[298] RHIT[299] RHIT[300] 
+ RHIT[301] RHIT[302] RHIT[303] RHIT[304] RHIT[305] RHIT[306] RHIT[307] 
+ RHIT[308] RHIT[309] RHIT[310] RHIT[311] RHIT[312] RHIT[313] RHIT[314] 
+ RHIT[315] RHIT[316] RHIT[317] RHIT[318] RHIT[319] RHIT[320] RHIT[321] 
+ RHIT[322] RHIT[323] RHIT[324] RHIT[325] RHIT[326] RHIT[327] RHIT[328] 
+ RHIT[329] RHIT[330] RHIT[331] RHIT[332] RHIT[333] RHIT[334] RHIT[335] 
+ RHIT[336] RHIT[337] RHIT[338] RHIT[339] RHIT[340] RHIT[341] RHIT[342] 
+ RHIT[343] RHIT[344] RHIT[345] RHIT[346] RHIT[347] RHIT[348] RHIT[349] 
+ RHIT[350] RHIT[351] RHIT[352] RHIT[353] RHIT[354] RHIT[355] RHIT[356] 
+ RHIT[357] RHIT[358] RHIT[359] RHIT[360] RHIT[361] RHIT[362] RHIT[363] 
+ RHIT[364] RHIT[365] RHIT[366] RHIT[367] RHIT[368] RHIT[369] RHIT[370] 
+ RHIT[371] RHIT[372] RHIT[373] RHIT[374] RHIT[375] RHIT[376] RHIT[377] 
+ RHIT[378] RHIT[379] RHIT[380] RHIT[381] RHIT[382] RHIT[383] RHIT[384] 
+ RHIT[385] RHIT[386] RHIT[387] RHIT[388] RHIT[389] RHIT[390] RHIT[391] 
+ RHIT[392] RHIT[393] RHIT[394] RHIT[395] RHIT[396] RHIT[397] RHIT[398] 
+ RHIT[399] RHIT[400] RHIT[401] RHIT[402] RHIT[403] RHIT[404] RHIT[405] 
+ RHIT[406] RHIT[407] RHIT[408] RHIT[409] RHIT[410] RHIT[411] RHIT[412] 
+ RHIT[413] RHIT[414] RHIT[415] RHIT[416] RHIT[417] RHIT[418] RHIT[419] 
+ RHIT[420] RHIT[421] RHIT[422] RHIT[423] RHIT[424] RHIT[425] RHIT[426] 
+ RHIT[427] RHIT[428] RHIT[429] RHIT[430] RHIT[431] RHIT[432] RHIT[433] 
+ RHIT[434] RHIT[435] RHIT[436] RHIT[437] RHIT[438] RHIT[439] RHIT[440] 
+ RHIT[441] RHIT[442] RHIT[443] RHIT[444] RHIT[445] RHIT[446] RHIT[447] 
+ RHIT[448] RHIT[449] RHIT[450] RHIT[451] RHIT[452] RHIT[453] RHIT[454] 
+ RHIT[455] RHIT[456] RHIT[457] RHIT[458] RHIT[459] RHIT[460] RHIT[461] 
+ RHIT[462] RHIT[463] RHIT[464] RHIT[465] RHIT[466] RHIT[467] RHIT[468] 
+ RHIT[469] RHIT[470] RHIT[471] RHIT[472] RHIT[473] RHIT[474] RHIT[475] 
+ RHIT[476] RHIT[477] RHIT[478] RHIT[479] RHIT[480] RHIT[481] RHIT[482] 
+ RHIT[483] RHIT[484] RHIT[485] RHIT[486] RHIT[487] RHIT[488] RHIT[489] 
+ RHIT[490] RHIT[491] RHIT[492] RHIT[493] RHIT[494] RHIT[495] RHIT[496] 
+ RHIT[497] RHIT[498] RHIT[499] RHIT[500] RHIT[501] RHIT[502] RHIT[503] 
+ RHIT[504] RHIT[505] RHIT[506] RHIT[507] RHIT[508] RHIT[509] RHIT[510] 
+ RHIT[511] SLICE_EN[0] SLICE_EN[1] SLICE_EN[2] SLICE_EN[3] SLICE_EN[4] 
+ SLICE_EN[5] SLICE_EN[6] SLICE_EN[7] VDD VSS WEN
*.PININFO ADDR[1]:I ADDR[2]:I ADDR[3]:I ADDR[4]:I ADDR[5]:I ADDR[6]:I 
*.PININFO ADDR[7]:I ADDR[8]:I ADDR[9]:I CFG[0]:I CFG[1]:I CFG[2]:I CFG[3]:I 
*.PININFO CFG[4]:I CFG[5]:I CFG[6]:I CFG[7]:I CFG[8]:I CFG[9]:I CFG[10]:I 
*.PININFO CFG[11]:I CLK:I DATA[0]:I DATA[1]:I DATA[2]:I DATA[3]:I DATA[4]:I 
*.PININFO DATA[5]:I DATA[6]:I DATA[7]:I DATA[8]:I DATA[9]:I DATA[10]:I 
*.PININFO DATA[11]:I DATA[12]:I DATA[13]:I DATA[14]:I DATA[15]:I DATA[16]:I 
*.PININFO DATA[17]:I DATA[18]:I DATA[19]:I DATA[20]:I DATA[21]:I DATA[22]:I 
*.PININFO DATA[23]:I DATA[24]:I DATA[25]:I DATA[26]:I DATA[27]:I DATA[28]:I 
*.PININFO DATA[29]:I DATA[30]:I DATA[31]:I DATA[32]:I DATA[33]:I DATA[34]:I 
*.PININFO DATA[35]:I DATA[36]:I DATA[37]:I DATA[38]:I DATA[39]:I KEN:I 
*.PININFO LHIT[0]:I LHIT[1]:I LHIT[2]:I LHIT[3]:I LHIT[4]:I LHIT[5]:I 
*.PININFO LHIT[6]:I LHIT[7]:I LHIT[8]:I LHIT[9]:I LHIT[10]:I LHIT[11]:I 
*.PININFO LHIT[12]:I LHIT[13]:I LHIT[14]:I LHIT[15]:I LHIT[16]:I LHIT[17]:I 
*.PININFO LHIT[18]:I LHIT[19]:I LHIT[20]:I LHIT[21]:I LHIT[22]:I LHIT[23]:I 
*.PININFO LHIT[24]:I LHIT[25]:I LHIT[26]:I LHIT[27]:I LHIT[28]:I LHIT[29]:I 
*.PININFO LHIT[30]:I LHIT[31]:I LHIT[32]:I LHIT[33]:I LHIT[34]:I LHIT[35]:I 
*.PININFO LHIT[36]:I LHIT[37]:I LHIT[38]:I LHIT[39]:I LHIT[40]:I LHIT[41]:I 
*.PININFO LHIT[42]:I LHIT[43]:I LHIT[44]:I LHIT[45]:I LHIT[46]:I LHIT[47]:I 
*.PININFO LHIT[48]:I LHIT[49]:I LHIT[50]:I LHIT[51]:I LHIT[52]:I LHIT[53]:I 
*.PININFO LHIT[54]:I LHIT[55]:I LHIT[56]:I LHIT[57]:I LHIT[58]:I LHIT[59]:I 
*.PININFO LHIT[60]:I LHIT[61]:I LHIT[62]:I LHIT[63]:I LHIT[64]:I LHIT[65]:I 
*.PININFO LHIT[66]:I LHIT[67]:I LHIT[68]:I LHIT[69]:I LHIT[70]:I LHIT[71]:I 
*.PININFO LHIT[72]:I LHIT[73]:I LHIT[74]:I LHIT[75]:I LHIT[76]:I LHIT[77]:I 
*.PININFO LHIT[78]:I LHIT[79]:I LHIT[80]:I LHIT[81]:I LHIT[82]:I LHIT[83]:I 
*.PININFO LHIT[84]:I LHIT[85]:I LHIT[86]:I LHIT[87]:I LHIT[88]:I LHIT[89]:I 
*.PININFO LHIT[90]:I LHIT[91]:I LHIT[92]:I LHIT[93]:I LHIT[94]:I LHIT[95]:I 
*.PININFO LHIT[96]:I LHIT[97]:I LHIT[98]:I LHIT[99]:I LHIT[100]:I LHIT[101]:I 
*.PININFO LHIT[102]:I LHIT[103]:I LHIT[104]:I LHIT[105]:I LHIT[106]:I 
*.PININFO LHIT[107]:I LHIT[108]:I LHIT[109]:I LHIT[110]:I LHIT[111]:I 
*.PININFO LHIT[112]:I LHIT[113]:I LHIT[114]:I LHIT[115]:I LHIT[116]:I 
*.PININFO LHIT[117]:I LHIT[118]:I LHIT[119]:I LHIT[120]:I LHIT[121]:I 
*.PININFO LHIT[122]:I LHIT[123]:I LHIT[124]:I LHIT[125]:I LHIT[126]:I 
*.PININFO LHIT[127]:I LHIT[128]:I LHIT[129]:I LHIT[130]:I LHIT[131]:I 
*.PININFO LHIT[132]:I LHIT[133]:I LHIT[134]:I LHIT[135]:I LHIT[136]:I 
*.PININFO LHIT[137]:I LHIT[138]:I LHIT[139]:I LHIT[140]:I LHIT[141]:I 
*.PININFO LHIT[142]:I LHIT[143]:I LHIT[144]:I LHIT[145]:I LHIT[146]:I 
*.PININFO LHIT[147]:I LHIT[148]:I LHIT[149]:I LHIT[150]:I LHIT[151]:I 
*.PININFO LHIT[152]:I LHIT[153]:I LHIT[154]:I LHIT[155]:I LHIT[156]:I 
*.PININFO LHIT[157]:I LHIT[158]:I LHIT[159]:I LHIT[160]:I LHIT[161]:I 
*.PININFO LHIT[162]:I LHIT[163]:I LHIT[164]:I LHIT[165]:I LHIT[166]:I 
*.PININFO LHIT[167]:I LHIT[168]:I LHIT[169]:I LHIT[170]:I LHIT[171]:I 
*.PININFO LHIT[172]:I LHIT[173]:I LHIT[174]:I LHIT[175]:I LHIT[176]:I 
*.PININFO LHIT[177]:I LHIT[178]:I LHIT[179]:I LHIT[180]:I LHIT[181]:I 
*.PININFO LHIT[182]:I LHIT[183]:I LHIT[184]:I LHIT[185]:I LHIT[186]:I 
*.PININFO LHIT[187]:I LHIT[188]:I LHIT[189]:I LHIT[190]:I LHIT[191]:I 
*.PININFO LHIT[192]:I LHIT[193]:I LHIT[194]:I LHIT[195]:I LHIT[196]:I 
*.PININFO LHIT[197]:I LHIT[198]:I LHIT[199]:I LHIT[200]:I LHIT[201]:I 
*.PININFO LHIT[202]:I LHIT[203]:I LHIT[204]:I LHIT[205]:I LHIT[206]:I 
*.PININFO LHIT[207]:I LHIT[208]:I LHIT[209]:I LHIT[210]:I LHIT[211]:I 
*.PININFO LHIT[212]:I LHIT[213]:I LHIT[214]:I LHIT[215]:I LHIT[216]:I 
*.PININFO LHIT[217]:I LHIT[218]:I LHIT[219]:I LHIT[220]:I LHIT[221]:I 
*.PININFO LHIT[222]:I LHIT[223]:I LHIT[224]:I LHIT[225]:I LHIT[226]:I 
*.PININFO LHIT[227]:I LHIT[228]:I LHIT[229]:I LHIT[230]:I LHIT[231]:I 
*.PININFO LHIT[232]:I LHIT[233]:I LHIT[234]:I LHIT[235]:I LHIT[236]:I 
*.PININFO LHIT[237]:I LHIT[238]:I LHIT[239]:I LHIT[240]:I LHIT[241]:I 
*.PININFO LHIT[242]:I LHIT[243]:I LHIT[244]:I LHIT[245]:I LHIT[246]:I 
*.PININFO LHIT[247]:I LHIT[248]:I LHIT[249]:I LHIT[250]:I LHIT[251]:I 
*.PININFO LHIT[252]:I LHIT[253]:I LHIT[254]:I LHIT[255]:I LHIT[256]:I 
*.PININFO LHIT[257]:I LHIT[258]:I LHIT[259]:I LHIT[260]:I LHIT[261]:I 
*.PININFO LHIT[262]:I LHIT[263]:I LHIT[264]:I LHIT[265]:I LHIT[266]:I 
*.PININFO LHIT[267]:I LHIT[268]:I LHIT[269]:I LHIT[270]:I LHIT[271]:I 
*.PININFO LHIT[272]:I LHIT[273]:I LHIT[274]:I LHIT[275]:I LHIT[276]:I 
*.PININFO LHIT[277]:I LHIT[278]:I LHIT[279]:I LHIT[280]:I LHIT[281]:I 
*.PININFO LHIT[282]:I LHIT[283]:I LHIT[284]:I LHIT[285]:I LHIT[286]:I 
*.PININFO LHIT[287]:I LHIT[288]:I LHIT[289]:I LHIT[290]:I LHIT[291]:I 
*.PININFO LHIT[292]:I LHIT[293]:I LHIT[294]:I LHIT[295]:I LHIT[296]:I 
*.PININFO LHIT[297]:I LHIT[298]:I LHIT[299]:I LHIT[300]:I LHIT[301]:I 
*.PININFO LHIT[302]:I LHIT[303]:I LHIT[304]:I LHIT[305]:I LHIT[306]:I 
*.PININFO LHIT[307]:I LHIT[308]:I LHIT[309]:I LHIT[310]:I LHIT[311]:I 
*.PININFO LHIT[312]:I LHIT[313]:I LHIT[314]:I LHIT[315]:I LHIT[316]:I 
*.PININFO LHIT[317]:I LHIT[318]:I LHIT[319]:I LHIT[320]:I LHIT[321]:I 
*.PININFO LHIT[322]:I LHIT[323]:I LHIT[324]:I LHIT[325]:I LHIT[326]:I 
*.PININFO LHIT[327]:I LHIT[328]:I LHIT[329]:I LHIT[330]:I LHIT[331]:I 
*.PININFO LHIT[332]:I LHIT[333]:I LHIT[334]:I LHIT[335]:I LHIT[336]:I 
*.PININFO LHIT[337]:I LHIT[338]:I LHIT[339]:I LHIT[340]:I LHIT[341]:I 
*.PININFO LHIT[342]:I LHIT[343]:I LHIT[344]:I LHIT[345]:I LHIT[346]:I 
*.PININFO LHIT[347]:I LHIT[348]:I LHIT[349]:I LHIT[350]:I LHIT[351]:I 
*.PININFO LHIT[352]:I LHIT[353]:I LHIT[354]:I LHIT[355]:I LHIT[356]:I 
*.PININFO LHIT[357]:I LHIT[358]:I LHIT[359]:I LHIT[360]:I LHIT[361]:I 
*.PININFO LHIT[362]:I LHIT[363]:I LHIT[364]:I LHIT[365]:I LHIT[366]:I 
*.PININFO LHIT[367]:I LHIT[368]:I LHIT[369]:I LHIT[370]:I LHIT[371]:I 
*.PININFO LHIT[372]:I LHIT[373]:I LHIT[374]:I LHIT[375]:I LHIT[376]:I 
*.PININFO LHIT[377]:I LHIT[378]:I LHIT[379]:I LHIT[380]:I LHIT[381]:I 
*.PININFO LHIT[382]:I LHIT[383]:I LHIT[384]:I LHIT[385]:I LHIT[386]:I 
*.PININFO LHIT[387]:I LHIT[388]:I LHIT[389]:I LHIT[390]:I LHIT[391]:I 
*.PININFO LHIT[392]:I LHIT[393]:I LHIT[394]:I LHIT[395]:I LHIT[396]:I 
*.PININFO LHIT[397]:I LHIT[398]:I LHIT[399]:I LHIT[400]:I LHIT[401]:I 
*.PININFO LHIT[402]:I LHIT[403]:I LHIT[404]:I LHIT[405]:I LHIT[406]:I 
*.PININFO LHIT[407]:I LHIT[408]:I LHIT[409]:I LHIT[410]:I LHIT[411]:I 
*.PININFO LHIT[412]:I LHIT[413]:I LHIT[414]:I LHIT[415]:I LHIT[416]:I 
*.PININFO LHIT[417]:I LHIT[418]:I LHIT[419]:I LHIT[420]:I LHIT[421]:I 
*.PININFO LHIT[422]:I LHIT[423]:I LHIT[424]:I LHIT[425]:I LHIT[426]:I 
*.PININFO LHIT[427]:I LHIT[428]:I LHIT[429]:I LHIT[430]:I LHIT[431]:I 
*.PININFO LHIT[432]:I LHIT[433]:I LHIT[434]:I LHIT[435]:I LHIT[436]:I 
*.PININFO LHIT[437]:I LHIT[438]:I LHIT[439]:I LHIT[440]:I LHIT[441]:I 
*.PININFO LHIT[442]:I LHIT[443]:I LHIT[444]:I LHIT[445]:I LHIT[446]:I 
*.PININFO LHIT[447]:I LHIT[448]:I LHIT[449]:I LHIT[450]:I LHIT[451]:I 
*.PININFO LHIT[452]:I LHIT[453]:I LHIT[454]:I LHIT[455]:I LHIT[456]:I 
*.PININFO LHIT[457]:I LHIT[458]:I LHIT[459]:I LHIT[460]:I LHIT[461]:I 
*.PININFO LHIT[462]:I LHIT[463]:I LHIT[464]:I LHIT[465]:I LHIT[466]:I 
*.PININFO LHIT[467]:I LHIT[468]:I LHIT[469]:I LHIT[470]:I LHIT[471]:I 
*.PININFO LHIT[472]:I LHIT[473]:I LHIT[474]:I LHIT[475]:I LHIT[476]:I 
*.PININFO LHIT[477]:I LHIT[478]:I LHIT[479]:I LHIT[480]:I LHIT[481]:I 
*.PININFO LHIT[482]:I LHIT[483]:I LHIT[484]:I LHIT[485]:I LHIT[486]:I 
*.PININFO LHIT[487]:I LHIT[488]:I LHIT[489]:I LHIT[490]:I LHIT[491]:I 
*.PININFO LHIT[492]:I LHIT[493]:I LHIT[494]:I LHIT[495]:I LHIT[496]:I 
*.PININFO LHIT[497]:I LHIT[498]:I LHIT[499]:I LHIT[500]:I LHIT[501]:I 
*.PININFO LHIT[502]:I LHIT[503]:I LHIT[504]:I LHIT[505]:I LHIT[506]:I 
*.PININFO LHIT[507]:I LHIT[508]:I LHIT[509]:I LHIT[510]:I LHIT[511]:I 
*.PININFO MASK[0]:I MASK[1]:I MASK[2]:I MASK[3]:I MASK[4]:I MASK[5]:I 
*.PININFO MASK[6]:I MASK[7]:I MASK[8]:I MASK[9]:I MASK[10]:I MASK[11]:I 
*.PININFO MASK[12]:I MASK[13]:I MASK[14]:I MASK[15]:I MASK[16]:I MASK[17]:I 
*.PININFO MASK[18]:I MASK[19]:I MASK[20]:I MASK[21]:I MASK[22]:I MASK[23]:I 
*.PININFO MASK[24]:I MASK[25]:I MASK[26]:I MASK[27]:I MASK[28]:I MASK[29]:I 
*.PININFO MASK[30]:I MASK[31]:I MASK[32]:I MASK[33]:I MASK[34]:I MASK[35]:I 
*.PININFO MASK[36]:I MASK[37]:I MASK[38]:I MASK[39]:I READ_DATA[0]:I 
*.PININFO READ_DATA[1]:I READ_DATA[2]:I READ_DATA[3]:I READ_DATA[4]:I 
*.PININFO READ_DATA[5]:I READ_DATA[6]:I READ_DATA[7]:I READ_DATA[8]:I 
*.PININFO READ_DATA[9]:I READ_DATA[10]:I READ_DATA[11]:I READ_DATA[12]:I 
*.PININFO READ_DATA[13]:I READ_DATA[14]:I READ_DATA[15]:I READ_DATA[16]:I 
*.PININFO READ_DATA[17]:I READ_DATA[18]:I READ_DATA[19]:I READ_DATA[20]:I 
*.PININFO READ_DATA[21]:I READ_DATA[22]:I READ_DATA[23]:I READ_DATA[24]:I 
*.PININFO READ_DATA[25]:I READ_DATA[26]:I READ_DATA[27]:I READ_DATA[28]:I 
*.PININFO READ_DATA[29]:I READ_DATA[30]:I READ_DATA[31]:I READ_DATA[32]:I 
*.PININFO READ_DATA[33]:I READ_DATA[34]:I READ_DATA[35]:I READ_DATA[36]:I 
*.PININFO READ_DATA[37]:I READ_DATA[38]:I READ_DATA[39]:I REN:I RESET_N:I 
*.PININFO RHIT[0]:I RHIT[1]:I RHIT[2]:I RHIT[3]:I RHIT[4]:I RHIT[5]:I 
*.PININFO RHIT[6]:I RHIT[7]:I RHIT[8]:I RHIT[9]:I RHIT[10]:I RHIT[11]:I 
*.PININFO RHIT[12]:I RHIT[13]:I RHIT[14]:I RHIT[15]:I RHIT[16]:I RHIT[17]:I 
*.PININFO RHIT[18]:I RHIT[19]:I RHIT[20]:I RHIT[21]:I RHIT[22]:I RHIT[23]:I 
*.PININFO RHIT[24]:I RHIT[25]:I RHIT[26]:I RHIT[27]:I RHIT[28]:I RHIT[29]:I 
*.PININFO RHIT[30]:I RHIT[31]:I RHIT[32]:I RHIT[33]:I RHIT[34]:I RHIT[35]:I 
*.PININFO RHIT[36]:I RHIT[37]:I RHIT[38]:I RHIT[39]:I RHIT[40]:I RHIT[41]:I 
*.PININFO RHIT[42]:I RHIT[43]:I RHIT[44]:I RHIT[45]:I RHIT[46]:I RHIT[47]:I 
*.PININFO RHIT[48]:I RHIT[49]:I RHIT[50]:I RHIT[51]:I RHIT[52]:I RHIT[53]:I 
*.PININFO RHIT[54]:I RHIT[55]:I RHIT[56]:I RHIT[57]:I RHIT[58]:I RHIT[59]:I 
*.PININFO RHIT[60]:I RHIT[61]:I RHIT[62]:I RHIT[63]:I RHIT[64]:I RHIT[65]:I 
*.PININFO RHIT[66]:I RHIT[67]:I RHIT[68]:I RHIT[69]:I RHIT[70]:I RHIT[71]:I 
*.PININFO RHIT[72]:I RHIT[73]:I RHIT[74]:I RHIT[75]:I RHIT[76]:I RHIT[77]:I 
*.PININFO RHIT[78]:I RHIT[79]:I RHIT[80]:I RHIT[81]:I RHIT[82]:I RHIT[83]:I 
*.PININFO RHIT[84]:I RHIT[85]:I RHIT[86]:I RHIT[87]:I RHIT[88]:I RHIT[89]:I 
*.PININFO RHIT[90]:I RHIT[91]:I RHIT[92]:I RHIT[93]:I RHIT[94]:I RHIT[95]:I 
*.PININFO RHIT[96]:I RHIT[97]:I RHIT[98]:I RHIT[99]:I RHIT[100]:I RHIT[101]:I 
*.PININFO RHIT[102]:I RHIT[103]:I RHIT[104]:I RHIT[105]:I RHIT[106]:I 
*.PININFO RHIT[107]:I RHIT[108]:I RHIT[109]:I RHIT[110]:I RHIT[111]:I 
*.PININFO RHIT[112]:I RHIT[113]:I RHIT[114]:I RHIT[115]:I RHIT[116]:I 
*.PININFO RHIT[117]:I RHIT[118]:I RHIT[119]:I RHIT[120]:I RHIT[121]:I 
*.PININFO RHIT[122]:I RHIT[123]:I RHIT[124]:I RHIT[125]:I RHIT[126]:I 
*.PININFO RHIT[127]:I RHIT[128]:I RHIT[129]:I RHIT[130]:I RHIT[131]:I 
*.PININFO RHIT[132]:I RHIT[133]:I RHIT[134]:I RHIT[135]:I RHIT[136]:I 
*.PININFO RHIT[137]:I RHIT[138]:I RHIT[139]:I RHIT[140]:I RHIT[141]:I 
*.PININFO RHIT[142]:I RHIT[143]:I RHIT[144]:I RHIT[145]:I RHIT[146]:I 
*.PININFO RHIT[147]:I RHIT[148]:I RHIT[149]:I RHIT[150]:I RHIT[151]:I 
*.PININFO RHIT[152]:I RHIT[153]:I RHIT[154]:I RHIT[155]:I RHIT[156]:I 
*.PININFO RHIT[157]:I RHIT[158]:I RHIT[159]:I RHIT[160]:I RHIT[161]:I 
*.PININFO RHIT[162]:I RHIT[163]:I RHIT[164]:I RHIT[165]:I RHIT[166]:I 
*.PININFO RHIT[167]:I RHIT[168]:I RHIT[169]:I RHIT[170]:I RHIT[171]:I 
*.PININFO RHIT[172]:I RHIT[173]:I RHIT[174]:I RHIT[175]:I RHIT[176]:I 
*.PININFO RHIT[177]:I RHIT[178]:I RHIT[179]:I RHIT[180]:I RHIT[181]:I 
*.PININFO RHIT[182]:I RHIT[183]:I RHIT[184]:I RHIT[185]:I RHIT[186]:I 
*.PININFO RHIT[187]:I RHIT[188]:I RHIT[189]:I RHIT[190]:I RHIT[191]:I 
*.PININFO RHIT[192]:I RHIT[193]:I RHIT[194]:I RHIT[195]:I RHIT[196]:I 
*.PININFO RHIT[197]:I RHIT[198]:I RHIT[199]:I RHIT[200]:I RHIT[201]:I 
*.PININFO RHIT[202]:I RHIT[203]:I RHIT[204]:I RHIT[205]:I RHIT[206]:I 
*.PININFO RHIT[207]:I RHIT[208]:I RHIT[209]:I RHIT[210]:I RHIT[211]:I 
*.PININFO RHIT[212]:I RHIT[213]:I RHIT[214]:I RHIT[215]:I RHIT[216]:I 
*.PININFO RHIT[217]:I RHIT[218]:I RHIT[219]:I RHIT[220]:I RHIT[221]:I 
*.PININFO RHIT[222]:I RHIT[223]:I RHIT[224]:I RHIT[225]:I RHIT[226]:I 
*.PININFO RHIT[227]:I RHIT[228]:I RHIT[229]:I RHIT[230]:I RHIT[231]:I 
*.PININFO RHIT[232]:I RHIT[233]:I RHIT[234]:I RHIT[235]:I RHIT[236]:I 
*.PININFO RHIT[237]:I RHIT[238]:I RHIT[239]:I RHIT[240]:I RHIT[241]:I 
*.PININFO RHIT[242]:I RHIT[243]:I RHIT[244]:I RHIT[245]:I RHIT[246]:I 
*.PININFO RHIT[247]:I RHIT[248]:I RHIT[249]:I RHIT[250]:I RHIT[251]:I 
*.PININFO RHIT[252]:I RHIT[253]:I RHIT[254]:I RHIT[255]:I RHIT[256]:I 
*.PININFO RHIT[257]:I RHIT[258]:I RHIT[259]:I RHIT[260]:I RHIT[261]:I 
*.PININFO RHIT[262]:I RHIT[263]:I RHIT[264]:I RHIT[265]:I RHIT[266]:I 
*.PININFO RHIT[267]:I RHIT[268]:I RHIT[269]:I RHIT[270]:I RHIT[271]:I 
*.PININFO RHIT[272]:I RHIT[273]:I RHIT[274]:I RHIT[275]:I RHIT[276]:I 
*.PININFO RHIT[277]:I RHIT[278]:I RHIT[279]:I RHIT[280]:I RHIT[281]:I 
*.PININFO RHIT[282]:I RHIT[283]:I RHIT[284]:I RHIT[285]:I RHIT[286]:I 
*.PININFO RHIT[287]:I RHIT[288]:I RHIT[289]:I RHIT[290]:I RHIT[291]:I 
*.PININFO RHIT[292]:I RHIT[293]:I RHIT[294]:I RHIT[295]:I RHIT[296]:I 
*.PININFO RHIT[297]:I RHIT[298]:I RHIT[299]:I RHIT[300]:I RHIT[301]:I 
*.PININFO RHIT[302]:I RHIT[303]:I RHIT[304]:I RHIT[305]:I RHIT[306]:I 
*.PININFO RHIT[307]:I RHIT[308]:I RHIT[309]:I RHIT[310]:I RHIT[311]:I 
*.PININFO RHIT[312]:I RHIT[313]:I RHIT[314]:I RHIT[315]:I RHIT[316]:I 
*.PININFO RHIT[317]:I RHIT[318]:I RHIT[319]:I RHIT[320]:I RHIT[321]:I 
*.PININFO RHIT[322]:I RHIT[323]:I RHIT[324]:I RHIT[325]:I RHIT[326]:I 
*.PININFO RHIT[327]:I RHIT[328]:I RHIT[329]:I RHIT[330]:I RHIT[331]:I 
*.PININFO RHIT[332]:I RHIT[333]:I RHIT[334]:I RHIT[335]:I RHIT[336]:I 
*.PININFO RHIT[337]:I RHIT[338]:I RHIT[339]:I RHIT[340]:I RHIT[341]:I 
*.PININFO RHIT[342]:I RHIT[343]:I RHIT[344]:I RHIT[345]:I RHIT[346]:I 
*.PININFO RHIT[347]:I RHIT[348]:I RHIT[349]:I RHIT[350]:I RHIT[351]:I 
*.PININFO RHIT[352]:I RHIT[353]:I RHIT[354]:I RHIT[355]:I RHIT[356]:I 
*.PININFO RHIT[357]:I RHIT[358]:I RHIT[359]:I RHIT[360]:I RHIT[361]:I 
*.PININFO RHIT[362]:I RHIT[363]:I RHIT[364]:I RHIT[365]:I RHIT[366]:I 
*.PININFO RHIT[367]:I RHIT[368]:I RHIT[369]:I RHIT[370]:I RHIT[371]:I 
*.PININFO RHIT[372]:I RHIT[373]:I RHIT[374]:I RHIT[375]:I RHIT[376]:I 
*.PININFO RHIT[377]:I RHIT[378]:I RHIT[379]:I RHIT[380]:I RHIT[381]:I 
*.PININFO RHIT[382]:I RHIT[383]:I RHIT[384]:I RHIT[385]:I RHIT[386]:I 
*.PININFO RHIT[387]:I RHIT[388]:I RHIT[389]:I RHIT[390]:I RHIT[391]:I 
*.PININFO RHIT[392]:I RHIT[393]:I RHIT[394]:I RHIT[395]:I RHIT[396]:I 
*.PININFO RHIT[397]:I RHIT[398]:I RHIT[399]:I RHIT[400]:I RHIT[401]:I 
*.PININFO RHIT[402]:I RHIT[403]:I RHIT[404]:I RHIT[405]:I RHIT[406]:I 
*.PININFO RHIT[407]:I RHIT[408]:I RHIT[409]:I RHIT[410]:I RHIT[411]:I 
*.PININFO RHIT[412]:I RHIT[413]:I RHIT[414]:I RHIT[415]:I RHIT[416]:I 
*.PININFO RHIT[417]:I RHIT[418]:I RHIT[419]:I RHIT[420]:I RHIT[421]:I 
*.PININFO RHIT[422]:I RHIT[423]:I RHIT[424]:I RHIT[425]:I RHIT[426]:I 
*.PININFO RHIT[427]:I RHIT[428]:I RHIT[429]:I RHIT[430]:I RHIT[431]:I 
*.PININFO RHIT[432]:I RHIT[433]:I RHIT[434]:I RHIT[435]:I RHIT[436]:I 
*.PININFO RHIT[437]:I RHIT[438]:I RHIT[439]:I RHIT[440]:I RHIT[441]:I 
*.PININFO RHIT[442]:I RHIT[443]:I RHIT[444]:I RHIT[445]:I RHIT[446]:I 
*.PININFO RHIT[447]:I RHIT[448]:I RHIT[449]:I RHIT[450]:I RHIT[451]:I 
*.PININFO RHIT[452]:I RHIT[453]:I RHIT[454]:I RHIT[455]:I RHIT[456]:I 
*.PININFO RHIT[457]:I RHIT[458]:I RHIT[459]:I RHIT[460]:I RHIT[461]:I 
*.PININFO RHIT[462]:I RHIT[463]:I RHIT[464]:I RHIT[465]:I RHIT[466]:I 
*.PININFO RHIT[467]:I RHIT[468]:I RHIT[469]:I RHIT[470]:I RHIT[471]:I 
*.PININFO RHIT[472]:I RHIT[473]:I RHIT[474]:I RHIT[475]:I RHIT[476]:I 
*.PININFO RHIT[477]:I RHIT[478]:I RHIT[479]:I RHIT[480]:I RHIT[481]:I 
*.PININFO RHIT[482]:I RHIT[483]:I RHIT[484]:I RHIT[485]:I RHIT[486]:I 
*.PININFO RHIT[487]:I RHIT[488]:I RHIT[489]:I RHIT[490]:I RHIT[491]:I 
*.PININFO RHIT[492]:I RHIT[493]:I RHIT[494]:I RHIT[495]:I RHIT[496]:I 
*.PININFO RHIT[497]:I RHIT[498]:I RHIT[499]:I RHIT[500]:I RHIT[501]:I 
*.PININFO RHIT[502]:I RHIT[503]:I RHIT[504]:I RHIT[505]:I RHIT[506]:I 
*.PININFO RHIT[507]:I RHIT[508]:I RHIT[509]:I RHIT[510]:I RHIT[511]:I 
*.PININFO SLICE_EN[0]:I SLICE_EN[1]:I SLICE_EN[2]:I SLICE_EN[3]:I 
*.PININFO SLICE_EN[4]:I SLICE_EN[5]:I SLICE_EN[6]:I SLICE_EN[7]:I VDD:I VSS:I 
*.PININFO WEN:I ADDR[0]:O
Xtcam ADDR[0] ADDR[1] ADDR[2] ADDR[3] ADDR[4] ADDR[5] ADDR[6] ADDR[7] ADDR[8] 
+ ADDR[9] CFG[0] CFG[1] CFG[2] CFG[3] CFG[4] CFG[5] CFG[6] CFG[7] CFG[8] 
+ CFG[9] CFG[10] CFG[11] CLK DATA[0] DATA[1] DATA[2] DATA[3] DATA[4] DATA[5] 
+ DATA[6] DATA[7] DATA[8] DATA[9] DATA[10] DATA[11] DATA[12] DATA[13] DATA[14] 
+ DATA[15] DATA[16] DATA[17] DATA[18] DATA[19] DATA[20] DATA[21] DATA[22] 
+ DATA[23] DATA[24] DATA[25] DATA[26] DATA[27] DATA[28] DATA[29] DATA[30] 
+ DATA[31] DATA[32] DATA[33] DATA[34] DATA[35] DATA[36] DATA[37] DATA[38] 
+ DATA[39] KEN LHIT[0] LHIT[1] LHIT[2] LHIT[3] LHIT[4] LHIT[5] LHIT[6] LHIT[7] 
+ LHIT[8] LHIT[9] LHIT[10] LHIT[11] LHIT[12] LHIT[13] LHIT[14] LHIT[15] 
+ LHIT[16] LHIT[17] LHIT[18] LHIT[19] LHIT[20] LHIT[21] LHIT[22] LHIT[23] 
+ LHIT[24] LHIT[25] LHIT[26] LHIT[27] LHIT[28] LHIT[29] LHIT[30] LHIT[31] 
+ LHIT[32] LHIT[33] LHIT[34] LHIT[35] LHIT[36] LHIT[37] LHIT[38] LHIT[39] 
+ LHIT[40] LHIT[41] LHIT[42] LHIT[43] LHIT[44] LHIT[45] LHIT[46] LHIT[47] 
+ LHIT[48] LHIT[49] LHIT[50] LHIT[51] LHIT[52] LHIT[53] LHIT[54] LHIT[55] 
+ LHIT[56] LHIT[57] LHIT[58] LHIT[59] LHIT[60] LHIT[61] LHIT[62] LHIT[63] 
+ LHIT[64] LHIT[65] LHIT[66] LHIT[67] LHIT[68] LHIT[69] LHIT[70] LHIT[71] 
+ LHIT[72] LHIT[73] LHIT[74] LHIT[75] LHIT[76] LHIT[77] LHIT[78] LHIT[79] 
+ LHIT[80] LHIT[81] LHIT[82] LHIT[83] LHIT[84] LHIT[85] LHIT[86] LHIT[87] 
+ LHIT[88] LHIT[89] LHIT[90] LHIT[91] LHIT[92] LHIT[93] LHIT[94] LHIT[95] 
+ LHIT[96] LHIT[97] LHIT[98] LHIT[99] LHIT[100] LHIT[101] LHIT[102] LHIT[103] 
+ LHIT[104] LHIT[105] LHIT[106] LHIT[107] LHIT[108] LHIT[109] LHIT[110] 
+ LHIT[111] LHIT[112] LHIT[113] LHIT[114] LHIT[115] LHIT[116] LHIT[117] 
+ LHIT[118] LHIT[119] LHIT[120] LHIT[121] LHIT[122] LHIT[123] LHIT[124] 
+ LHIT[125] LHIT[126] LHIT[127] LHIT[128] LHIT[129] LHIT[130] LHIT[131] 
+ LHIT[132] LHIT[133] LHIT[134] LHIT[135] LHIT[136] LHIT[137] LHIT[138] 
+ LHIT[139] LHIT[140] LHIT[141] LHIT[142] LHIT[143] LHIT[144] LHIT[145] 
+ LHIT[146] LHIT[147] LHIT[148] LHIT[149] LHIT[150] LHIT[151] LHIT[152] 
+ LHIT[153] LHIT[154] LHIT[155] LHIT[156] LHIT[157] LHIT[158] LHIT[159] 
+ LHIT[160] LHIT[161] LHIT[162] LHIT[163] LHIT[164] LHIT[165] LHIT[166] 
+ LHIT[167] LHIT[168] LHIT[169] LHIT[170] LHIT[171] LHIT[172] LHIT[173] 
+ LHIT[174] LHIT[175] LHIT[176] LHIT[177] LHIT[178] LHIT[179] LHIT[180] 
+ LHIT[181] LHIT[182] LHIT[183] LHIT[184] LHIT[185] LHIT[186] LHIT[187] 
+ LHIT[188] LHIT[189] LHIT[190] LHIT[191] LHIT[192] LHIT[193] LHIT[194] 
+ LHIT[195] LHIT[196] LHIT[197] LHIT[198] LHIT[199] LHIT[200] LHIT[201] 
+ LHIT[202] LHIT[203] LHIT[204] LHIT[205] LHIT[206] LHIT[207] LHIT[208] 
+ LHIT[209] LHIT[210] LHIT[211] LHIT[212] LHIT[213] LHIT[214] LHIT[215] 
+ LHIT[216] LHIT[217] LHIT[218] LHIT[219] LHIT[220] LHIT[221] LHIT[222] 
+ LHIT[223] LHIT[224] LHIT[225] LHIT[226] LHIT[227] LHIT[228] LHIT[229] 
+ LHIT[230] LHIT[231] LHIT[232] LHIT[233] LHIT[234] LHIT[235] LHIT[236] 
+ LHIT[237] LHIT[238] LHIT[239] LHIT[240] LHIT[241] LHIT[242] LHIT[243] 
+ LHIT[244] LHIT[245] LHIT[246] LHIT[247] LHIT[248] LHIT[249] LHIT[250] 
+ LHIT[251] LHIT[252] LHIT[253] LHIT[254] LHIT[255] LHIT[256] LHIT[257] 
+ LHIT[258] LHIT[259] LHIT[260] LHIT[261] LHIT[262] LHIT[263] LHIT[264] 
+ LHIT[265] LHIT[266] LHIT[267] LHIT[268] LHIT[269] LHIT[270] LHIT[271] 
+ LHIT[272] LHIT[273] LHIT[274] LHIT[275] LHIT[276] LHIT[277] LHIT[278] 
+ LHIT[279] LHIT[280] LHIT[281] LHIT[282] LHIT[283] LHIT[284] LHIT[285] 
+ LHIT[286] LHIT[287] LHIT[288] LHIT[289] LHIT[290] LHIT[291] LHIT[292] 
+ LHIT[293] LHIT[294] LHIT[295] LHIT[296] LHIT[297] LHIT[298] LHIT[299] 
+ LHIT[300] LHIT[301] LHIT[302] LHIT[303] LHIT[304] LHIT[305] LHIT[306] 
+ LHIT[307] LHIT[308] LHIT[309] LHIT[310] LHIT[311] LHIT[312] LHIT[313] 
+ LHIT[314] LHIT[315] LHIT[316] LHIT[317] LHIT[318] LHIT[319] LHIT[320] 
+ LHIT[321] LHIT[322] LHIT[323] LHIT[324] LHIT[325] LHIT[326] LHIT[327] 
+ LHIT[328] LHIT[329] LHIT[330] LHIT[331] LHIT[332] LHIT[333] LHIT[334] 
+ LHIT[335] LHIT[336] LHIT[337] LHIT[338] LHIT[339] LHIT[340] LHIT[341] 
+ LHIT[342] LHIT[343] LHIT[344] LHIT[345] LHIT[346] LHIT[347] LHIT[348] 
+ LHIT[349] LHIT[350] LHIT[351] LHIT[352] LHIT[353] LHIT[354] LHIT[355] 
+ LHIT[356] LHIT[357] LHIT[358] LHIT[359] LHIT[360] LHIT[361] LHIT[362] 
+ LHIT[363] LHIT[364] LHIT[365] LHIT[366] LHIT[367] LHIT[368] LHIT[369] 
+ LHIT[370] LHIT[371] LHIT[372] LHIT[373] LHIT[374] LHIT[375] LHIT[376] 
+ LHIT[377] LHIT[378] LHIT[379] LHIT[380] LHIT[381] LHIT[382] LHIT[383] 
+ LHIT[384] LHIT[385] LHIT[386] LHIT[387] LHIT[388] LHIT[389] LHIT[390] 
+ LHIT[391] LHIT[392] LHIT[393] LHIT[394] LHIT[395] LHIT[396] LHIT[397] 
+ LHIT[398] LHIT[399] LHIT[400] LHIT[401] LHIT[402] LHIT[403] LHIT[404] 
+ LHIT[405] LHIT[406] LHIT[407] LHIT[408] LHIT[409] LHIT[410] LHIT[411] 
+ LHIT[412] LHIT[413] LHIT[414] LHIT[415] LHIT[416] LHIT[417] LHIT[418] 
+ LHIT[419] LHIT[420] LHIT[421] LHIT[422] LHIT[423] LHIT[424] LHIT[425] 
+ LHIT[426] LHIT[427] LHIT[428] LHIT[429] LHIT[430] LHIT[431] LHIT[432] 
+ LHIT[433] LHIT[434] LHIT[435] LHIT[436] LHIT[437] LHIT[438] LHIT[439] 
+ LHIT[440] LHIT[441] LHIT[442] LHIT[443] LHIT[444] LHIT[445] LHIT[446] 
+ LHIT[447] LHIT[448] LHIT[449] LHIT[450] LHIT[451] LHIT[452] LHIT[453] 
+ LHIT[454] LHIT[455] LHIT[456] LHIT[457] LHIT[458] LHIT[459] LHIT[460] 
+ LHIT[461] LHIT[462] LHIT[463] LHIT[464] LHIT[465] LHIT[466] LHIT[467] 
+ LHIT[468] LHIT[469] LHIT[470] LHIT[471] LHIT[472] LHIT[473] LHIT[474] 
+ LHIT[475] LHIT[476] LHIT[477] LHIT[478] LHIT[479] LHIT[480] LHIT[481] 
+ LHIT[482] LHIT[483] LHIT[484] LHIT[485] LHIT[486] LHIT[487] LHIT[488] 
+ LHIT[489] LHIT[490] LHIT[491] LHIT[492] LHIT[493] LHIT[494] LHIT[495] 
+ LHIT[496] LHIT[497] LHIT[498] LHIT[499] LHIT[500] LHIT[501] LHIT[502] 
+ LHIT[503] LHIT[504] LHIT[505] LHIT[506] LHIT[507] LHIT[508] LHIT[509] 
+ LHIT[510] LHIT[511] MASK[0] MASK[1] MASK[2] MASK[3] MASK[4] MASK[5] MASK[6] 
+ MASK[7] MASK[8] MASK[9] MASK[10] MASK[11] MASK[12] MASK[13] MASK[14] 
+ MASK[15] MASK[16] MASK[17] MASK[18] MASK[19] MASK[20] MASK[21] MASK[22] 
+ MASK[23] MASK[24] MASK[25] MASK[26] MASK[27] MASK[28] MASK[29] MASK[30] 
+ MASK[31] MASK[32] MASK[33] MASK[34] MASK[35] MASK[36] MASK[37] MASK[38] 
+ MASK[39] READ_DATA[0] READ_DATA[1] READ_DATA[2] READ_DATA[3] READ_DATA[4] 
+ READ_DATA[5] READ_DATA[6] READ_DATA[7] READ_DATA[8] READ_DATA[9] 
+ READ_DATA[10] READ_DATA[11] READ_DATA[12] READ_DATA[13] READ_DATA[14] 
+ READ_DATA[15] READ_DATA[16] READ_DATA[17] READ_DATA[18] READ_DATA[19] 
+ READ_DATA[20] READ_DATA[21] READ_DATA[22] READ_DATA[23] READ_DATA[24] 
+ READ_DATA[25] READ_DATA[26] READ_DATA[27] READ_DATA[28] READ_DATA[29] 
+ READ_DATA[30] READ_DATA[31] READ_DATA[32] READ_DATA[33] READ_DATA[34] 
+ READ_DATA[35] READ_DATA[36] READ_DATA[37] READ_DATA[38] READ_DATA[39] REN 
+ RESET_N RHIT[0] RHIT[1] RHIT[2] RHIT[3] RHIT[4] RHIT[5] RHIT[6] RHIT[7] 
+ RHIT[8] RHIT[9] RHIT[10] RHIT[11] RHIT[12] RHIT[13] RHIT[14] RHIT[15] 
+ RHIT[16] RHIT[17] RHIT[18] RHIT[19] RHIT[20] RHIT[21] RHIT[22] RHIT[23] 
+ RHIT[24] RHIT[25] RHIT[26] RHIT[27] RHIT[28] RHIT[29] RHIT[30] RHIT[31] 
+ RHIT[32] RHIT[33] RHIT[34] RHIT[35] RHIT[36] RHIT[37] RHIT[38] RHIT[39] 
+ RHIT[40] RHIT[41] RHIT[42] RHIT[43] RHIT[44] RHIT[45] RHIT[46] RHIT[47] 
+ RHIT[48] RHIT[49] RHIT[50] RHIT[51] RHIT[52] RHIT[53] RHIT[54] RHIT[55] 
+ RHIT[56] RHIT[57] RHIT[58] RHIT[59] RHIT[60] RHIT[61] RHIT[62] RHIT[63] 
+ RHIT[64] RHIT[65] RHIT[66] RHIT[67] RHIT[68] RHIT[69] RHIT[70] RHIT[71] 
+ RHIT[72] RHIT[73] RHIT[74] RHIT[75] RHIT[76] RHIT[77] RHIT[78] RHIT[79] 
+ RHIT[80] RHIT[81] RHIT[82] RHIT[83] RHIT[84] RHIT[85] RHIT[86] RHIT[87] 
+ RHIT[88] RHIT[89] RHIT[90] RHIT[91] RHIT[92] RHIT[93] RHIT[94] RHIT[95] 
+ RHIT[96] RHIT[97] RHIT[98] RHIT[99] RHIT[100] RHIT[101] RHIT[102] RHIT[103] 
+ RHIT[104] RHIT[105] RHIT[106] RHIT[107] RHIT[108] RHIT[109] RHIT[110] 
+ RHIT[111] RHIT[112] RHIT[113] RHIT[114] RHIT[115] RHIT[116] RHIT[117] 
+ RHIT[118] RHIT[119] RHIT[120] RHIT[121] RHIT[122] RHIT[123] RHIT[124] 
+ RHIT[125] RHIT[126] RHIT[127] RHIT[128] RHIT[129] RHIT[130] RHIT[131] 
+ RHIT[132] RHIT[133] RHIT[134] RHIT[135] RHIT[136] RHIT[137] RHIT[138] 
+ RHIT[139] RHIT[140] RHIT[141] RHIT[142] RHIT[143] RHIT[144] RHIT[145] 
+ RHIT[146] RHIT[147] RHIT[148] RHIT[149] RHIT[150] RHIT[151] RHIT[152] 
+ RHIT[153] RHIT[154] RHIT[155] RHIT[156] RHIT[157] RHIT[158] RHIT[159] 
+ RHIT[160] RHIT[161] RHIT[162] RHIT[163] RHIT[164] RHIT[165] RHIT[166] 
+ RHIT[167] RHIT[168] RHIT[169] RHIT[170] RHIT[171] RHIT[172] RHIT[173] 
+ RHIT[174] RHIT[175] RHIT[176] RHIT[177] RHIT[178] RHIT[179] RHIT[180] 
+ RHIT[181] RHIT[182] RHIT[183] RHIT[184] RHIT[185] RHIT[186] RHIT[187] 
+ RHIT[188] RHIT[189] RHIT[190] RHIT[191] RHIT[192] RHIT[193] RHIT[194] 
+ RHIT[195] RHIT[196] RHIT[197] RHIT[198] RHIT[199] RHIT[200] RHIT[201] 
+ RHIT[202] RHIT[203] RHIT[204] RHIT[205] RHIT[206] RHIT[207] RHIT[208] 
+ RHIT[209] RHIT[210] RHIT[211] RHIT[212] RHIT[213] RHIT[214] RHIT[215] 
+ RHIT[216] RHIT[217] RHIT[218] RHIT[219] RHIT[220] RHIT[221] RHIT[222] 
+ RHIT[223] RHIT[224] RHIT[225] RHIT[226] RHIT[227] RHIT[228] RHIT[229] 
+ RHIT[230] RHIT[231] RHIT[232] RHIT[233] RHIT[234] RHIT[235] RHIT[236] 
+ RHIT[237] RHIT[238] RHIT[239] RHIT[240] RHIT[241] RHIT[242] RHIT[243] 
+ RHIT[244] RHIT[245] RHIT[246] RHIT[247] RHIT[248] RHIT[249] RHIT[250] 
+ RHIT[251] RHIT[252] RHIT[253] RHIT[254] RHIT[255] RHIT[256] RHIT[257] 
+ RHIT[258] RHIT[259] RHIT[260] RHIT[261] RHIT[262] RHIT[263] RHIT[264] 
+ RHIT[265] RHIT[266] RHIT[267] RHIT[268] RHIT[269] RHIT[270] RHIT[271] 
+ RHIT[272] RHIT[273] RHIT[274] RHIT[275] RHIT[276] RHIT[277] RHIT[278] 
+ RHIT[279] RHIT[280] RHIT[281] RHIT[282] RHIT[283] RHIT[284] RHIT[285] 
+ RHIT[286] RHIT[287] RHIT[288] RHIT[289] RHIT[290] RHIT[291] RHIT[292] 
+ RHIT[293] RHIT[294] RHIT[295] RHIT[296] RHIT[297] RHIT[298] RHIT[299] 
+ RHIT[300] RHIT[301] RHIT[302] RHIT[303] RHIT[304] RHIT[305] RHIT[306] 
+ RHIT[307] RHIT[308] RHIT[309] RHIT[310] RHIT[311] RHIT[312] RHIT[313] 
+ RHIT[314] RHIT[315] RHIT[316] RHIT[317] RHIT[318] RHIT[319] RHIT[320] 
+ RHIT[321] RHIT[322] RHIT[323] RHIT[324] RHIT[325] RHIT[326] RHIT[327] 
+ RHIT[328] RHIT[329] RHIT[330] RHIT[331] RHIT[332] RHIT[333] RHIT[334] 
+ RHIT[335] RHIT[336] RHIT[337] RHIT[338] RHIT[339] RHIT[340] RHIT[341] 
+ RHIT[342] RHIT[343] RHIT[344] RHIT[345] RHIT[346] RHIT[347] RHIT[348] 
+ RHIT[349] RHIT[350] RHIT[351] RHIT[352] RHIT[353] RHIT[354] RHIT[355] 
+ RHIT[356] RHIT[357] RHIT[358] RHIT[359] RHIT[360] RHIT[361] RHIT[362] 
+ RHIT[363] RHIT[364] RHIT[365] RHIT[366] RHIT[367] RHIT[368] RHIT[369] 
+ RHIT[370] RHIT[371] RHIT[372] RHIT[373] RHIT[374] RHIT[375] RHIT[376] 
+ RHIT[377] RHIT[378] RHIT[379] RHIT[380] RHIT[381] RHIT[382] RHIT[383] 
+ RHIT[384] RHIT[385] RHIT[386] RHIT[387] RHIT[388] RHIT[389] RHIT[390] 
+ RHIT[391] RHIT[392] RHIT[393] RHIT[394] RHIT[395] RHIT[396] RHIT[397] 
+ RHIT[398] RHIT[399] RHIT[400] RHIT[401] RHIT[402] RHIT[403] RHIT[404] 
+ RHIT[405] RHIT[406] RHIT[407] RHIT[408] RHIT[409] RHIT[410] RHIT[411] 
+ RHIT[412] RHIT[413] RHIT[414] RHIT[415] RHIT[416] RHIT[417] RHIT[418] 
+ RHIT[419] RHIT[420] RHIT[421] RHIT[422] RHIT[423] RHIT[424] RHIT[425] 
+ RHIT[426] RHIT[427] RHIT[428] RHIT[429] RHIT[430] RHIT[431] RHIT[432] 
+ RHIT[433] RHIT[434] RHIT[435] RHIT[436] RHIT[437] RHIT[438] RHIT[439] 
+ RHIT[440] RHIT[441] RHIT[442] RHIT[443] RHIT[444] RHIT[445] RHIT[446] 
+ RHIT[447] RHIT[448] RHIT[449] RHIT[450] RHIT[451] RHIT[452] RHIT[453] 
+ RHIT[454] RHIT[455] RHIT[456] RHIT[457] RHIT[458] RHIT[459] RHIT[460] 
+ RHIT[461] RHIT[462] RHIT[463] RHIT[464] RHIT[465] RHIT[466] RHIT[467] 
+ RHIT[468] RHIT[469] RHIT[470] RHIT[471] RHIT[472] RHIT[473] RHIT[474] 
+ RHIT[475] RHIT[476] RHIT[477] RHIT[478] RHIT[479] RHIT[480] RHIT[481] 
+ RHIT[482] RHIT[483] RHIT[484] RHIT[485] RHIT[486] RHIT[487] RHIT[488] 
+ RHIT[489] RHIT[490] RHIT[491] RHIT[492] RHIT[493] RHIT[494] RHIT[495] 
+ RHIT[496] RHIT[497] RHIT[498] RHIT[499] RHIT[500] RHIT[501] RHIT[502] 
+ RHIT[503] RHIT[504] RHIT[505] RHIT[506] RHIT[507] RHIT[508] RHIT[509] 
+ RHIT[510] RHIT[511] SLICE_EN[0] SLICE_EN[1] SLICE_EN[2] SLICE_EN[3] 
+ SLICE_EN[4] SLICE_EN[5] SLICE_EN[6] SLICE_EN[7] VDD VSS WEN / 
+ ip736hs3p111dtcam_512x40m1b8wd_nnnnnnl_wwc_tcam_512_40_4000
.ENDS

