*
*simulator lang=spectre

*include "/p/hdk/cad/spectre/19.1.0.134.xps309/general_setting/xps_timing.ini_build309"

*include "/nfs/site/disks/compute_n3_downloads/N03/release/v0.5/logic_spice_model/cln3_1d2_sp_v0d5_1p1_usage.scs" section=TTGlobalCorner_LocalMC_MOS_MOSCAP
*include "/nfs/site/disks/compute_n3_downloads/N03/release/v0.5/logic_spice_model/cln3_1d2_sp_v0d5_1p1_usage.scs" section=TTGlobalCorner_LocalMC_BIP_DIO
*include "/nfs/site/disks/compute_n3_downloads/N03/release/v0.5/logic_spice_model/cln3_1d2_sp_v0d5_1p1_usage.scs" section=pre_simu
.OPTION FSDB=1
.OPTION PROBE=1
.OPTION POST
.OPTION MEASDGT=7
.OPTION LIST NODE
.OPTION CAPTAB
*.lib '/p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice/cln3e_1d2_sp_v0d5_2p2_usage.l' SSGlobalCorner_LocalMC_MOS_MOSCAP
*.lib '/p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice/cln3e_1d2_sp_v0d5_2p2_usage.l' SSGlobalCorner_LocalMC_BIP_DIO
*.lib '/p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice/cln3e_1d2_sp_v0d5_2p2_usage.l' SSGlobalCorner_LocalMC_RES_DISRES
*.lib '/p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice/cln3e_1d2_sp_v0d5_2p2_usage.l' SS_R_METAL
*simulator lang=spice
.inc /p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice/include-lib_SSGNP
*.inc /p/tech1/n3e/tech-release/v0.5.0/models/1P17M_1Xa_h_1Xb_v_1Xc_h_1Xd_v_1Ya_h_1Yb_v_6Y_hvhvhv_2Yy2R_shdmim_ut-alrdl/hspice/include_TT

.include "/nfs/site/disks/tfc_be_zsc7_01/swadkar/spice_sim/XFGHDSVT_LB2x36_SN_1_wl_slopes/XFGHDSVT_LB2x36_SN_1_125C_typical.spf"

xtop DN0  DN1  DN2  DN3  DN4  DN5  DN6  DN7  DN8  DN9  DN10  DN11  DN12  DN13  DN14  DN15  DN16  DN17  DN18  DN19  DN20  DN21  DN22  DN23  DN24  DN25  DN26  DN27  DN28  DN29  DN30  DN31  DN32  DN33  DN34  DN35  DN36  DN37  DN38  DN39  DN40  DN41  DN42  DN43  DN44  DN45  DN46  DN47  DN48  DN49  DN50  DN51  DN52  DN53  DN54  DN55  DN56  DN57  DN58  DN59  DN60  DN61  DN62  DN63  DN64  DN65  DN66  DN67  DN68  DN69  DN70  DN71  RWL0  RWL1  RWLN0  RWLN1  VBB  VDD  VPP  VSS  WWL0  WWL1  WWLN0  WWLN1  X0  X1  X2  X3  X4  X5  X6  X7  X8  X9  X10  X11  X12  X13  X14  X15  X16  X17  X18  X19  X20  X21  X22  X23  X24  X25  X26  X27  X28  X29  X30  X31  X32  X33  X34  X35  X36  X37  X38  X39  X40  X41  X42  X43  X44  X45  X46  X47  X48  X49  X50  X51  X52  X53  X54  X55  X56  X57  X58  X59  X60  X61  X62  X63  X64  X65  X66  X67  X68  X69  X70  X71  Z0  Z1  Z2  Z3  Z4  Z5  Z6  Z7  Z8  Z9  Z10  Z11  Z12  Z13  Z14  Z15  Z16  Z17  Z18  Z19  Z20  Z21  Z22  Z23  Z24  Z25  Z26  Z27  Z28  Z29  Z30  Z31  Z32  Z33  Z34  Z35  Z36  Z37  Z38  Z39  Z40  Z41  Z42  Z43  Z44  Z45  Z46  Z47  Z48  Z49  Z50  Z51  Z52  Z53  Z54  Z55  Z56  Z57  Z58  Z59  Z60  Z61  Z62  Z63  Z64  Z65  Z66  Z67  Z68  Z69  Z70  Z71 XFGHDSVT_LB2x36_SN_1

*.option checktoppinconn=warning $SFE-3023/25 errors
*.option dbdelete=default
*.option sim_opt_conly_check_tstart=0

.param VDD = 0.675
.param VSS = 0
.temp 125
vVSS VSS 0 0
vVDD  VDD  0 VDD
vVBB  VBB  0 0
vVPP  VPP  0 VDD

*#*ssw Cload0 rdbl_buf_p0[0] VSS 20f

*.ic v(x0) = VSS
*.ic v(xtop.mm270:d) = VSS

.param lpw = 250p
.param hpw = 250p
.param cyc = '(lpw+hpw)'
.param half_cyc = 'cyc/2'

*** TABLE ***


*** Assign input waveform ***
*ports DN0  DN1  RWL0  RWL1  RWLN0  RWLN1 WWL0  WWL1  WWLN0  WWLN1  X0  X1  Z0  Z1
vWWL0 WWL0 0 PWL (0n VSS 40ps VSS 50ps VDD 400ps VDD 410ps VSS 1500ps VSS)
vWWLN0 WWLN0 0 PWL (0n VDD 40ps VDD 50ps VSS 400ps VSS 410ps VDD 1500ps VDD)
vRWL0 RWL0 0 PWL (0n VSS 440ps VSS 450ps VDD 800ps VDD 810ps VSS 1500ps VSS)
vRWLN0 RWLN0 0 PWL (0n VDD 440ps VDD 450ps VSS 800ps VSS 810ps VDD 1500ps VDD)

vWWL1 WWL1 0 PWL (0n VSS 40ps VSS 50ps VDD 400ps VDD 410ps VSS 1500ps VSS)
vWWLN1 WWLN1 0 PWL (0n VDD 40ps VDD 50ps VSS 400ps VSS 410ps VDD 1500ps VDD)
vRWL1 RWL1 0 PWL (0n VSS 440ps VSS 450ps VDD 800ps VDD 810ps VSS 1500ps VSS)
vRWLN1 RWLN1 0 PWL (0n VDD 440ps VDD 450ps VSS 800ps VSS 810ps VDD 1500ps VDD)

*.inc /nfs/site/disks/tfc_be_zsc7_01/swadkar/spice_sim/input_waveform.sp

***** 
.tran 1e-13 '3*cyc'
*.probe v(*) exclude=[*:*]
.probe v(*:g)
.probe v(*:s)
.probe v(*:d)
*.print v(input)
*.print v(output)
*.print v(noise)
.probe v(*) filter='*:*' filter='*.*'
*ssw .print ac Cap(wwl0) Cap(xtop.mmnga1:g)
.param deltav = 0.675
*C_wwl0_f
.param t1_trig_wf = 400ps
.param t2_targ_wf = 410ps
.meas tran iwwl0_f integ i(vWWL0) from='t1_trig_wf' to='t2_targ_wf'
.meas tran C_wwl0_f  PARAM= 'iwwl0_f/deltav'
*C_wwl0_r
.param t1_trig_wr =40ps
.param t2_targ_wr =50ps
.meas tran iwwl0_r integ i(vWWL0) from='t1_trig_wr' to='t2_targ_wr'
.meas tran C_wwl0_r  PARAM= 'iwwl0_r/deltav'
*C_rwl0_f
.param t1_trig_rf =800ps
.param t2_targ_rf =810ps
.meas tran irwl0_f integ i(vRWL0) from='t1_trig_rf' to='t2_targ_rf'
.meas tran C_rwl0_f  PARAM= 'irwl0_f/deltav'
*C_rwl0_r
.param t1_trig_rr =440ps
.param t2_targ_rr =450ps 
.meas tran irwl0_r integ i(vRWL0) from='t1_trig_rr' to='t2_targ_rr'
.meas tran C_rwl0_r  PARAM= 'irwl0_r/deltav'


************************************************
*C_wwln0_f
.param t1_trig_wnf =40ps
.param t2_targ_wnf =50ps
.meas tran iwwln0_f integ i(vWWLN0) from='t1_trig_wnf' to='t2_targ_wnf'
.meas tran C_wwln0_f  PARAM= 'iwwln0_f/deltav'
*C_wwln0_r
.param t1_trig_wnr =400ps
.param t2_targ_wnr =410ps
.meas tran iwwln0_r integ i(vWWLN0) from='t1_trig_wnr' to='t2_targ_wnr'
.meas tran C_wwln0_r  PARAM= 'iwwln0_r/deltav'
*C_rwln0_f
.param t1_trig_rnf =440ps
.param t2_targ_rnf = 450ps
.meas tran irwln0_f integ i(vRWLN0) from='t1_trig_rnf' to='t2_targ_rnf'
.meas tran C_rwln0_f  PARAM= 'irwln0_f/deltav'
*C_rwln0_r
.param t1_trig_rnr =800ps
.param t2_targ_rnr =810ps
.meas tran irwln0_r integ i(vRWLN0) from='t1_trig_rnr' to='t2_targ_rnr'
.meas tran C_rwln0_r  PARAM= 'irwln0_r/deltav'

************************************************

*wwl0
.measure tran wwl0_delay_r TRIG v(wwl0) val="0.5*VDD" rise=1 TARG v(xtop.mmnga1:g) val="0.5*VDD" rise=1
.measure tran wwl0_slew_r TRIG v(xtop.mmnga1:g) val="0.1*VDD" rise=1 TARG v(xtop.mmnga1:g) val="0.9*VDD" rise=1
.measure tran wwl0_delay_f TRIG v(wwl0) val="0.5*VDD" fall=1 TARG v(xtop.mmnga1:g) val="0.5*VDD" fall=1
.measure tran wwl0_slew_f TRIG v(xtop.mmnga1:g) val="0.9*VDD" fall=1 TARG v(xtop.mmnga1:g) val="0.1*VDD" fall=1
*wwln0
.measure tran wwln0_delay_r TRIG v(wwln0) val="0.5*VDD" rise=1 TARG v(xtop.mmpgn1:g) val="0.5*VDD" rise=1
.measure tran wwln0_slew_r TRIG v(xtop.mmpgn1:g) val="0.1*VDD" rise=1 TARG v(xtop.mmpgn1:g) val="0.9*VDD" rise=1
.measure tran wwln0_delay_f TRIG v(wwln0) val="0.5*VDD" fall=1 TARG v(xtop.mmpgn1:g) val="0.5*VDD" fall=1
.measure tran wwln0_slew_f TRIG v(xtop.mmpgn1:g) val="0.9*VDD" fall=1 TARG v(xtop.mmpgn1:g) val="0.1*VDD" fall=1
*rwl0
.measure tran rwl0_delay_r TRIG v(rwl0) val="0.5*VDD" rise=1 TARG v(xtop.mm4:g) val="0.5*VDD" rise=1
.measure tran rwl0_slew_r TRIG v(xtop.mm4:g) val="0.1*VDD" rise=1 TARG v(xtop.mm4:g) val="0.9*VDD" rise=1
.measure tran rwl0_delay_f TRIG v(rwl0) val="0.5*VDD" fall=1 TARG v(xtop.mm4:g) val="0.5*VDD" fall=1
.measure tran rwl0_slew_f TRIG v(xtop.mm4:g) val="0.9*VDD" fall=1 TARG v(xtop.mm4:g) val="0.1*VDD" fall=1
*rwln0
.measure tran rwln0_delay_r TRIG v(rwln0) val="0.5*VDD" rise=1 TARG v(xtop.mm3:g) val="0.5*VDD" rise=1
.measure tran rwln0_slew_r TRIG v(xtop.mm3:g) val="0.1*VDD" rise=1 TARG v(xtop.mm3:g) val="0.9*VDD" rise=1
.measure tran rwln0_delay_f TRIG v(rwln0) val="0.5*VDD" fall=1 TARG v(xtop.mm3:g) val="0.5*VDD" fall=1
.measure tran rwln0_slew_f TRIG v(xtop.mm3:g) val="0.9*VDD" fall=1 TARG v(xtop.mm3:g) val="0.1*VDD" fall=1
*ssw *wwl0_tri_state
*ssw .measure tran wwl0_ts_delay_r TRIG v(wwl0) val="0.5*VDD" rise=1 TARG v(xtop.mm0:g) val="0.5*VDD" rise=1
*ssw .measure tran wwl0_ts_slew_r TRIG v(xtop.mm0:g) val="0.1*VDD" rise=1 TARG v(xtop.mm0:g) val="0.9*VDD" rise=1
*ssw .measure tran wwl0_ts_delay_f TRIG v(wwl0) val="0.5*VDD" fall=1 TARG v(xtop.mm0:g) val="0.5*VDD" fall=1
*ssw .measure tran wwl0_ts_slew_f TRIG v(xtop.mm0:g) val="0.9*VDD" fall=1 TARG v(xtop.mm0:g) val="0.1*VDD" fall=1
*ssw *wwln0_tri_state
*ssw .measure tran wwln0_ts_delay_r TRIG v(wwln0) val="0.5*VDD" rise=1 TARG v(xtop.mm1:g) val="0.5*VDD" rise=1
*ssw .measure tran wwln0_ts_slew_r TRIG v(xtop.mm1:g) val="0.1*VDD" rise=1 TARG v(xtop.mm1:g) val="0.9*VDD" rise=1
*ssw .measure tran wwln0_ts_delay_f TRIG v(wwln0) val="0.5*VDD" fall=1 TARG v(xtop.mm1:g) val="0.5*VDD" fall=1
*ssw .measure tran wwln0_ts_slew_f TRIG v(xtop.mm1:g) val="0.9*VDD" fall=1 TARG v(xtop.mm1:g) val="0.1*VDD" fall=1
*ssw *wwl1
*ssw .measure tran wwl1_delay_r TRIG v(wwl1) val="0.5*VDD" rise=1 TARG v(xtop.mm10:g) val="0.5*VDD" rise=1
*ssw .measure tran wwl1_slew_r TRIG v(xtop.mm10:g) val="0.1*VDD" rise=1 TARG v(xtop.mm10:g) val="0.9*VDD" rise=1
*ssw .measure tran wwl1_delay_f TRIG v(wwl1) val="0.5*VDD" fall=1 TARG v(xtop.mm10:g) val="0.5*VDD" fall=1
*ssw .measure tran wwl1_slew_f TRIG v(xtop.mm10:g) val="0.9*VDD" fall=1 TARG v(xtop.mm10:g) val="0.1*VDD" fall=1
*ssw *wwln1
*ssw .measure tran wwln1_delay_r TRIG v(wwln1) val="0.5*VDD" rise=1 TARG v(xtop.mm8:g) val="0.5*VDD" rise=1
*ssw .measure tran wwln1_slew_r TRIG v(xtop.mm8:g) val="0.1*VDD" rise=1 TARG v(xtop.mm8:g) val="0.9*VDD" rise=1
*ssw .measure tran wwln1_delay_f TRIG v(wwln1) val="0.5*VDD" fall=1 TARG v(xtop.mm8:g) val="0.5*VDD" fall=1
*ssw .measure tran wwln1_slew_f TRIG v(xtop.mm8:g) val="0.9*VDD" fall=1 TARG v(xtop.mm8:g) val="0.1*VDD" fall=1
*ssw *wwl1_tri_state
*ssw .measure tran wwl1_ts_delay_r TRIG v(wwl1) val="0.5*VDD" rise=1 TARG v(xtop.mm7:g) val="0.5*VDD" rise=1
*ssw .measure tran wwl1_ts_slew_r TRIG v(xtop.mm7:g) val="0.1*VDD" rise=1 TARG v(xtop.mm7:g) val="0.9*VDD" rise=1
*ssw .measure tran wwl1_ts_delay_f TRIG v(wwl1) val="0.5*VDD" fall=1 TARG v(xtop.mm7:g) val="0.5*VDD" fall=1
*ssw .measure tran wwl1_ts_slew_f TRIG v(xtop.mm7:g) val="0.9*VDD" fall=1 TARG v(xtop.mm7:g) val="0.1*VDD" fall=1
*ssw *wwln1_tri_state
*ssw .measure tran wwln1_ts_delay_r TRIG v(wwln1) val="0.5*VDD" rise=1 TARG v(xtop.mm13:g) val="0.5*VDD" rise=1
*ssw .measure tran wwln1_ts_slew_r TRIG v(xtop.mm13:g) val="0.1*VDD" rise=1 TARG v(xtop.mm13:g) val="0.9*VDD" rise=1
*ssw .measure tran wwln1_ts_delay_f TRIG v(wwln1) val="0.5*VDD" fall=1 TARG v(xtop.mm13:g) val="0.5*VDD" fall=1
*ssw .measure tran wwln1_ts_slew_f TRIG v(xtop.mm13:g) val="0.9*VDD" fall=1 TARG v(xtop.mm13:g) val="0.1*VDD" fall=1
*ssw *rwl1
*ssw .measure tran rwl1_delay_r TRIG v(rwl1) val="0.5*VDD" rise=1 TARG v(xtop.mm12:g) val="0.5*VDD" rise=1
*ssw .measure tran rwl1_slew_r TRIG v(xtop.mm12:g) val="0.1*VDD" rise=1 TARG v(xtop.mm12:g) val="0.9*VDD" rise=1
*ssw .measure tran rwl1_delay_f TRIG v(rwl1) val="0.5*VDD" fall=1 TARG v(xtop.mm12:g) val="0.5*VDD" fall=1
*ssw .measure tran rwl1_slew_f TRIG v(xtop.mm12:g) val="0.9*VDD" fall=1 TARG v(xtop.mm12:g) val="0.1*VDD" fall=1
*ssw *rwln1
*ssw .measure tran rwln1_delay_r TRIG v(rwln1) val="0.5*VDD" rise=1 TARG v(xtop.mm9:g) val="0.5*VDD" rise=1
*ssw .measure tran rwln1_slew_r TRIG v(xtop.mm9:g) val="0.1*VDD" rise=1 TARG v(xtop.mm9:g) val="0.9*VDD" rise=1
*ssw .measure tran rwln1_delay_f TRIG v(rwln1) val="0.5*VDD" fall=1 TARG v(xtop.mm9:g) val="0.5*VDD" fall=1
*ssw .measure tran rwln1_slew_f TRIG v(xtop.mm9:g) val="0.9*VDD" fall=1 TARG v(xtop.mm9:g) val="0.1*VDD" fall=1

