* tech comparison
.TEMP 75
.PARAM vtrue=0.25

.OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1
.OPTION POST=fsdb PROBE=1
.OPTION XA_CMD="set_sim_level -level 6"
.OPTION XA_CMD="set_wildcard_rule -match* one"
.OPTION XA_CMD="set_message_option -limit 100"
.OPTION XA_CMD="enable_print_statement 1"
.OPTION XA_CMD="set_sim_case -case sensitive"

.OPTION AUTOSTOP

* Monte Carlo stuff

*.OPTION XA_CMD="set_monte_carlo_option -enable 1"                    * not needed
*.OPTION XA_CMD="set_monte_carlo_option -sample_output all"
*.OPTION XA_CMD="set_monte_carlo_option -mc0_file 1"
*.OPTION XA_CMD="set_monte_carlo_option -parameter_file 1"
*.OPTION XA_CMD="set_monte_carlo_option -mc0_header 1"
*.OPTION XA_CMD="set_monte_carlo_option -dump_waveform 1"

.option search='/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5/models/core/hspice/m15_2x_1xa_1xb_4ya_2yb_2yc_3yd__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp'
.lib 'p1278_3.hsp' tttt

* std cell library with parasitics (optional):
.include "@LIBDIR@/@CELL@.spf"


.SUBCKT nand_cell a b o1 vcc vssx 
* INPUT: a b 
* OUTPUT: o1 
* INOUT: vcc vssx 
*.PININFO a:I b:I 
*.PININFO o1:O 
*.PININFO vcc:B vssx:B 
Mqn1 o1 a n1   vssx nhpbulvt w=2 l=14e-9 m=1 nf=1 
Mqn2 n1 b vssx vssx nhpbulvt w=2 l=14e-9 m=1 nf=1 
Mqp1 o1 a vcc  vcc  phpbulvt w=2 l=14e-9 m=1 nf=1 
Mqp2 o1 b vcc  vcc  phpbulvt w=2 l=14e-9 m=1 nf=1 
.ENDS  nand_cell

**********************************************************************

* Generic ring stage (make appropriate in/out/power connections to dut_cell)
.subckt ring_stage in out vcc vssx

* FO3 per Andrew

*      A     B     OUT    <--PWR-->
X0     in    vcc  xi     vcc  vssx   @CELL@
X1     vcc  xi    out    vcc  vssx   @CELL@

* dummy loads on xi
xload0 xi    vcc  unc[0] vcc  vssx   @CELL@
xload1 xi    vcc  unc[1] vcc  vssx   @CELL@
*xload2 xi    vcc  unc[2] vcc  vssx   @CELL@

xload3 out    vcc  unc[3] vcc  vssx   @CELL@
xload4 out    vcc  unc[4] vcc  vssx   @CELL@
*xload5 out    vcc  unc[5] vcc  vssx   @CELL@

.ends

* Ring oscillator with 20 DUT stages and 1 NAND2 enable
X1  x[0]              x[1]       vcc  vload vssx ring_stage
X2  x[1]              x[2]       vcc  vload vssx ring_stage
X3  x[2]              x[3]       vcc  vload vssx ring_stage
X4  x[3]              x[4]       vcc  vload vssx ring_stage
X5  x[4]              x[5]       vcc  vload vssx ring_stage
X6  x[5]              x[6]       vcc  vload vssx ring_stage
X7  x[6]              x[7]       vcc  vload vssx ring_stage
X8  x[7]              x[8]       vcc  vload vssx ring_stage
X9  x[8]              x[9]       vcc  vload vssx ring_stage
X10 x[9]              x[10]      vcc  vload vssx ring_stage
X21 _RESET x[10]      x[0]       vcc   vssx nand_cell

* Probes (for debugging)
.probe tran v(*)
.probe tran v(*.*)
.PROBE TRAN v(x[0])
.PROBE TRAN i(Vvcc)
.PROBE TRAN v(_RESET)
.PROBE TRAN v(vcc)
.PROBE TRAN v(vload)
.PROBE TRAN v(vssx)
.PROBE TRAN v(vssy)
.PROBE TRAN v(vissx)
.PROBE TRAN v(vissy)
.PROBE TRAN v(x[1])
.PROBE TRAN v(x[2])
.PROBE TRAN v(x[3])
.PROBE TRAN v(x[4])
.PROBE TRAN v(x[5])
.PROBE TRAN v(x[6])
.PROBE TRAN v(x[7])
.PROBE TRAN v(x[8])
.PROBE TRAN v(x[9])
.PROBE TRAN v(x[10])
.PROBE TRAN v(x[11])
.PROBE TRAN v(x[12])
.PROBE TRAN v(x[13])
.PROBE TRAN v(x[14])
.PROBE TRAN v(x[15])
.PROBE TRAN v(x[16])
.PROBE TRAN v(x[17])
.PROBE TRAN v(x[18])
.PROBE TRAN v(x[19])
.PROBE TRAN v(x[20])

* Sources
Vres _RESET 0 DC=0 PWL 0 0 2ns 0 2.1ns vtrue
Vvcc    vcc 0 DC=vtrue
Vvload  vload 0 DC=vtrue
Vtgnd    tgnd 0 DC=0

Vvssx   vssx tgnd DC=0
Vvssy   vssy tgnd DC=0
* sense voltage source for the current mirror

Fvssx vissx 0 Vvssx 1e6
Fvssy vissy 0 Vvssy 1e6
* multiply by 1M current mirror

Rsensex vissx 0 1
Rsensey vissy 0 1
* 1-ohm resistor from vissx to ground

* Simulate
.TRAN 1ps 1000ns sweep monte=list(2:2)

.variation
        option block_name=extern_data
        option ignore_global_variation=yes
        option sampling_method=external
        option mcbrief=1
        option set_missing_values=zero
.end_variation

.include "var.sp"

* Measure
.measure tran Cycle
+ trig V(x[0]) val='vtrue*0.5' td=1ns rise=1
+ targ V(x[0]) val='vtrue*0.5' td=1ns fall=2
*.measure tran Freq          PARAM='(4/(Cycle))'
.measure tran IdleCurrent   avg i(Vvcc) from 1.0ns to 1.9ns
.measure tran IdlePower     PARAM='(-IdleCurrent*vtrue)'
*.measure tran ActiveCurrent avg i(Vvcc) from=30ns to=40ns
*.measure tran ActivePower   PARAM='(-ActiveCurrent*vtrue)'
*.measure tran ActiveEnergy  PARAM='(ActivePower/Freq)'

.END
