* Ring oscillator

.TEMP 85
.PARAM vtrue=0.3

.OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1
.option cmipath='/p/hdk/cad/pdk/pdk783_r0.9_23ww26.5_alpha/cmi/hspice/cmi/lnx86/64bit'
.option cmi02opt=1
.OPTION POST=fsdb PROBE=1
.OPTION XA_CMD="set_sim_level -level 6"
.OPTION XA_CMD="set_wildcard_rule -match* one"
.OPTION XA_CMD="set_message_option -limit 100"
.OPTION XA_CMD="enable_print_statement 1"
.OPTION XA_CMD="set_sim_case -case sensitive"
.option redefsub



* Monte Carlo stuff

.OPTION XA_CMD="set_monte_carlo_option -enable 1"     
.OPTION XA_CMD="set_monte_carlo_option -sample_output all"
.OPTION XA_CMD="set_monte_carlo_option -mc0_file 1"
.OPTION XA_CMD="set_monte_carlo_option -parameter_file 1"
.OPTION XA_CMD="set_monte_carlo_option -mc0_header 1"
.OPTION XA_CMD="set_monte_carlo_option -dump_waveform 1"

.option search='/nfs/site/disks/zsc9_fwr_sd_001/mnystroe/p1278_3x0p9eu1/2023ww43d5/models_core_hspice/m14_2x_1xa_1xb_6ya_2yb_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp'
.lib 'p1278_3.hsp' tttt

.option PDMI=1
.option PDMI_LIB='/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5//cmi/hspice/pdmi/lnx86/64bit/pdmi.so'

.include 'include.sp'
.include 'tff.sp'
.include 'dff.sp'
.include 'pulse_gen_logic_ff.cdl'

**********************************************************************

.subckt dlycell d db q qb vcc vssx

Xinv0 d  qb vcc vssx i0sinv000aa1n30x5
Xinv1 db q  vcc vssx i0sinv000aa1n30x5

Xinvc0 q  qb vcc vssx i0sinv000aa1n02x5
Xinvc1 qb q  vcc vssx i0sinv000aa1n02x5

.ends

**********************************************************************

.subckt dlycell_en d db en enb q qb vcc vssx

Xinv0 d  en  qb vcc vssx i0snand02aa1d16x5
Xinv1 db enb q  vcc vssx i0snor002aa1n16x5

Xinvc0 q  qb vcc vssx i0sinv000aa1n02x5
Xinvc1 qb q  vcc vssx i0sinv000aa1n02x5

.ends

**********************************************************************

.subckt interp e  eb  o  ob f0 f1 f2 f3 ck ckb  vcc vssx

* this thing needs to invert, unlike the paper's design

Xinv0 f0 f0b vcc vssx i0sinv000aa1n02x5
Xinv1 f1 f1b vcc vssx i0sinv000aa1n02x5
Xinv2 f2 f2b vcc vssx i0sinv000aa1n02x5
Xinv3 f3 f3b vcc vssx i0sinv000aa1n02x5


MMou0 vcc  f0b  oui ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4
MMou1 oui  o    ck ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4

MMeu0 vcc  f0   eui ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4
MMeu1 eui  e    ck ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4

MMod0 vssx f1   odi ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4
MMod1 odi  o    ck ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4

MMed0 vssx f1b  edi ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4
MMed1 edi  e    ck ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4


MMobu0 vcc  f2b  obui ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4
MMobu1 obui  ob    ckb ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4

MMebu0 vcc  f2   ebui ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4
MMebu1 ebui  eb    ckb ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=4

MMobd0 vssx f3   obdi ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4
MMobd1 obdi  ob    ckb ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4

MMebd0 vssx f3b  ebdi ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4
MMebd1 ebdi  eb    ckb ln_FAKE_MMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=4


Xinvc0 ck  ckb vcc vssx i0sinv000aa1n04x5
Xinvc1 ckb ck  vcc vssx i0sinv000aa1n04x5

.ends

**********************************************************************

.subckt passbuf en i ib o ob vcc vssx

Xinve en enb vcc vssx i0sinv000aa1n02x5

M0u0 vcc enb u0i  ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=8
M0u1 u0i i  ob  ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=8

M0d0 vssx en d0i ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=8
M0d1 d0i  i  ob   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=8

M1u0 vcc enb u1i  ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=8
M1u1 u1i ib  o   ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=8

M1d0 vssx en d1i ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=8
M1d1 d1i  ib  o    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=8

.ends

**********************************************************************

.subckt resetbox o ob e eb rstb vcc vssx

Xinv rstb reset vcc vssx i0sinv000aa1n02x5

M0 vcc rstb eb ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=1
M1 vcc rstb ob ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m=1
M2 vssx reset e ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=1
M3 vssx reset o ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=1

.ends

**********************************************************************

.subckt oscillator f0_0 f1_0 f2_0 f3_0 f0_1 f1_1 f2_1 f3_1 f0_2 f1_2 f2_2 f3_2 f0_3 f1_3 f2_3 f3_3 f0_4 f1_4 f2_4 f3_4 f0_5 f1_5 f2_5 f3_5 f0_6 f1_6 f2_6 f3_6 f0_7 f1_7 f2_7 f3_7  ev0 od0 ev1 od1 ev2 od2 en enb ck ckb rstb vcc vssx

Xinterp0 e eb o ob f0_0 f1_0 f2_0 f3_0 ck ckb vcc vssx interp
Xinterp1 e eb o ob f0_1 f1_1 f2_1 f3_1 ck ckb vcc vssx interp
Xinterp2 e eb o ob f0_2 f1_2 f2_2 f3_2 ck ckb vcc vssx interp
Xinterp3 e eb o ob f0_3 f1_3 f2_3 f3_3 ck ckb vcc vssx interp
Xinterp4 e eb o ob f0_4 f1_4 f2_4 f3_4 ck ckb vcc vssx interp
Xinterp5 e eb o ob f0_5 f1_5 f2_5 f3_5 ck ckb vcc vssx interp
Xinterp6 e eb o ob f0_6 f1_6 f2_6 f3_6 ck ckb vcc vssx interp
Xinterp7 e eb o ob f0_7 f1_7 f2_7 f3_7 ck ckb vcc vssx interp


Vck0 ck ck0 0
vckb0 ckb ckb0 0

Xscd0  ck0 ckb0 en enb ck1 ckb1 vcc vssx dlycell_en

Xscd1  ck1 ckb1 ck2 ckb2 vcc vssx dlycell
Xscd2  ck2 ckb2 ck3 ckb3 vcc vssx dlycell
Xscd3  ck3 ckb3 ck4 ckb4 vcc vssx dlycell
Xscd4  ck4 ckb4 ck5 ckb5 vcc vssx dlycell
Xscd5  ck5 ckb5 ck6 ckb6 vcc vssx dlycell
Xscd6  ck6 ckb6 ck7 ckb7 vcc vssx dlycell

Xpassev0 ev0 ck1 ckb1 e eb vcc vssx passbuf
Xpassod0 od0 ck2 ckb2 o ob vcc vssx passbuf
Xpassev1 ev1 ck3 ckb3 e eb vcc vssx passbuf
Xpassod1 od1 ck4 ckb4 o ob vcc vssx passbuf
Xpassev2 ev2 ck5 ckb5 e eb vcc vssx passbuf
Xpassod2 od2 ck6 ckb6 o ob vcc vssx passbuf

Xreset o ob e eb rstb vcc vssx resetbox

.ends

**********************************************************************

* note that ck and ckb are swapped here
* because of this, we want the oscillator to stop with ckb LOW.

Xdut
+f0_0 f1_0 f2_0 f3_0 f0_1
+f1_1 f2_1 f3_1 f0_2 f1_2
+f2_2 f3_2 f0_3 f1_3 f2_3
+f3_3 f0_4 f1_4 f2_4 f3_4
+f0_5 f1_5 f2_5 f3_5 f0_6
+f1_6 f2_6 f3_6 f0_7 f1_7 f2_7 f3_7
+ev0 od0 ev1 od1 ev2 od2 en enb
+ckb ck rstb vcc vssx oscillator

Vres rstb 0 DC=0 PWL 0 0 0.99ns 0 1.01ns vtrue

* enables
Ven en vcc 0 
Venb enb vssx 0 

* state machine

*Xstat0 ckdiv2 rst  rstb  s0 s1 s1b vcc vssx ssdff_rst1
*Xstat1 ckdiv2 rstb s1 s2 s2b vcc vssx ssdff_rst0
*Xstat2 ckdiv2 rstb s2 s3 s3b vcc vssx ssdff_rst0
*Xstat3 ckdiv2 rstb s3 s4 s4b vcc vssx ssdff_rst0

Xstat0 ckdiv2 s0b s1b s1  rstb vcc vssx pulse_gen_logic_ff
Xstat1 ckdiv2 s1  s2  s2b rstb vcc vssx pulse_gen_logic_ff
Xstat2 ckdiv2 s2  s3  s3b rstb vcc vssx pulse_gen_logic_ff
Xstat3 ckdiv2 s3  s4  s4b rstb vcc vssx pulse_gen_logic_ff

Vs0  s0  s4  0
Vs0b s0b s4b 0

Xres0 rstb s0b stop vcc vssx i0snand02aa1n06x5

Xsr eclk stop sren srenb vcc vssx lsr000   


**********************************************************************

.include 'settings.sp'


Xdiv0 ck     rst  rstb ckdiv2 vcc vssx sstff_rst0
Xdiv1 ckdiv2 rst  rstb ckdiv4 vcc vssx stff_rst0
Xdiv2 ckdiv4 rst  rstb ckdiv8 vcc vssx stff_rst0


* set max speed -- coarse tuning
Vev0 ev0 rstb 0
Vod0 od0 rstb 0
Vev1 ev1 0 0
Vod1 od1 0 0
Vev2 ev2 0 0
Vod2 od2 0 0
*
** fine tuning
*Vf0  f0 0 0
*Vf1  f1 0 0
*Vf2  f2 0 0
*Vf3  f3 0 0

Vxclk eclk 0 DC=0 PWL 0 0
+ 1.59ns     0 1.61ns vtrue
+ 1.99ns vtrue 2.01ns     0 


**********************************************************************

.PROBE TRAN v(ev*)
.PROBE TRAN v(od*)

.PROBE TRAN v(xdut.xreset.rstb)
.PROBE TRAN v(xdut.xreset.reset)

.PROBE TRAN v(rst*)

.PROBE TRAN v(xdiv0.a)
.PROBE TRAN v(xdiv0.b)
.PROBE TRAN v(xdiv0.c)

.PROBE TRAN v(xdiv1.a)
.PROBE TRAN v(xdiv1.b)
.PROBE TRAN v(xdiv1.c)

.PROBE TRAN v(xdiv2.a)
.PROBE TRAN v(xdiv2.b)
.PROBE TRAN v(xdiv2.c)

.PROBE TRAN v(xstat0.a)
.PROBE TRAN v(xstat0.b)
.PROBE TRAN v(xstat0.c)

.PROBE TRAN v(xstat1.a)
.PROBE TRAN v(xstat1.b)
.PROBE TRAN v(xstat1.c)

.PROBE TRAN v(xstat2.a)
.PROBE TRAN v(xstat2.b)
.PROBE TRAN v(xstat2.c)

.PROBE TRAN v(xstat3.a)
.PROBE TRAN v(xstat3.b)
.PROBE TRAN v(xstat3.c)

.PROBE TRAN v(xstat*)

.PROBE TRAN v(s*)
.PROBE TRAN v(eclk)



.PROBE TRAN v(f*)
.PROBE TRAN v(xdut.e)
.PROBE TRAN v(xdut.eb)
.PROBE TRAN v(xdut.o)
.PROBE TRAN v(xdut.ob)
.PROBE TRAN v(ck*)
.PROBE TRAN v(xdut.ck*)
.PROBE TRAN v(xdut.xscd0.d)
.PROBE TRAN v(xdut.xscd0.db)
.PROBE TRAN v(xdut.xscd0.q)
.PROBE TRAN v(xdut.xscd0.qb)
.PROBE TRAN v(xdut.xscd1.d)
.PROBE TRAN v(xdut.xscd1.db)
.PROBE TRAN v(xdut.xscd1.q)
.PROBE TRAN v(xdut.xscd1.qb)
.PROBE TRAN v(xdut.xpassev0.i*)
.PROBE TRAN v(xdut.xpassev0.u*)
.PROBE TRAN v(xdut.xpassev0.e*)
.PROBE TRAN v(xdut.xpassod0.i*)
.PROBE TRAN v(xdut.xpassod0.u*)
.PROBE TRAN v(xdut.xpassod0.e*)
.PROBE TRAN i(vvcc)



Vvcc    vcc   0 DC=vtrue
Vvssx   vssx  0 DC=0

.PARAM runtime=2n


* Simulate
.TRAN 1p '1n + runtime'
*.TRAN 1p '1n + runtime' sweep monte=10


* the purpose of the following .measure is to trigger AUTOSTOP during calibration
.measure tran Cycle  
+ trig V(ck) val='vtrue*0.5' td=1n rise=4
+ targ V(ck) val='vtrue*0.5' td=1n rise=5


.END
