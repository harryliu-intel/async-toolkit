* tech comparison
.TEMP @TEMP@
.PARAM vtrue=@VOLT@

.OPTION CMIFLAG=1 CMIUSRFLAG=3 PDMI=1
.option cmipath='/p/hdk/cad/pdk/pdk783_r0.9_23ww26.5_alpha/cmi/hspice/cmi/lnx86/64bit'
.option cmi02opt=1
.OPTION POST=fsdb PROBE=1
.OPTION XA_CMD="set_sim_level -level 6"
.OPTION XA_CMD="set_wildcard_rule -match* one"
.OPTION XA_CMD="set_message_option -limit 100"
.OPTION XA_CMD="enable_print_statement 1"
.OPTION XA_CMD="set_sim_case -case sensitive"

.OPTION AUTOSTOP

* Monte Carlo stuff

.OPTION XA_CMD="set_monte_carlo_option -enable 1"     
.OPTION XA_CMD="set_monte_carlo_option -sample_output all"
.OPTION XA_CMD="set_monte_carlo_option -mc0_file 1"
.OPTION XA_CMD="set_monte_carlo_option -parameter_file 1"
.OPTION XA_CMD="set_monte_carlo_option -mc0_header 1"
.OPTION XA_CMD="set_monte_carlo_option -dump_waveform 1"

.option search='@HSPICE_MODEL_ROOT@'
.lib '@HSPICE_MODEL@' @CORNER@

.option PDMI=1
.option PDMI_LIB='/p/hdk/cad/pdk/pdk783_r0.5_22ww52.5//cmi/hspice/pdmi/lnx86/64bit/pdmi.so'


* std cell library with parasitics (optional):
@INCLUDELIB@


.SUBCKT nand_cell a b o1 vcc vssx 
* INPUT: a b 
* OUTPUT: o1 
* INOUT: vcc vssx 
*.PININFO a:I b:I 
*.PININFO o1:O 
*.PININFO vcc:B vssx:B 
Mqn1 o1 a n1   vssx n@TRANSUFX@ @TRANSIZE@ 
Mqn2 n1 b vssx vssx n@TRANSUFX@ @TRANSIZE@ 
Mqp1 o1 a vcc  vcc  p@TRANSUFX@ @TRANSIZE@ 
Mqp2 o1 b vcc  vcc  p@TRANSUFX@ @TRANSIZE@ 
.ENDS  nand_cell

**********************************************************************

* Generic ring stage (make appropriate in/out/power connections to dut_cell)
.subckt ring_stage in out vcc vssx

* FO3 per Andrew

*      A     B     OUT    <--PWR-->
X0     in    vcc  xi     vcc  vssx   @CELLNAME0@
X1     vcc  xi    out    vcc  vssx   @CELLNAME1@

* dummy loads on xi
xload0 xi    vcc  unc[0] vcc  vssx   @CELLNAME0@
xload1 xi    vcc  unc[1] vcc  vssx   @CELLNAME0@

xload3 out    vcc  unc[3] vcc  vssx   @CELLNAME0@
xload4 out    vcc  unc[4] vcc  vssx   @CELLNAME0@

.ends

* Ring oscillator with 20 DUT stages and 1 NAND2 enable
X1  x[0]              x[1]        vload vssx ring_stage
X2  x[1]              x[2]        vload vssx ring_stage
X3  x[2]              x[3]        vload vssx ring_stage
X4  x[3]              x[4]        vload vssx ring_stage
X5  x[4]              x[5]        vload vssx ring_stage
X6  x[5]              x[6]        vload vssx ring_stage
X7  x[6]              x[7]        vload vssx ring_stage
X8  x[7]              x[8]        vload vssx ring_stage
X9  x[8]              x[9]        vload vssx ring_stage
X10 x[9]              x[10]       vload vssx ring_stage
X21 _RESET x[10]      x[0]        vcc   vssx nand_cell

* Ring oscillator with 20 DUT stages and 1 NAND2 enable
X101  x[100]              x[101]        vload vssy ring_stage
X102  x[101]              x[102]        vload vssy ring_stage
X103  x[102]              x[103]        vload vssy ring_stage
X104  x[103]              x[104]        vload vssy ring_stage
X105  x[104]              x[105]        vload vssy ring_stage
X106  x[105]              x[106]        vload vssy ring_stage
X107  x[106]              x[107]        vload vssy ring_stage
X108  x[107]              x[108]        vload vssy ring_stage
X109  x[108]              x[109]        vload vssy ring_stage
X110  x[109]              x[110]        vload vssy ring_stage
X121  GND     x[110]      x[100]        vcc   vssy nand_cell

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
.PROBE TRAN i1(Vvssx)
.PROBE TRAN i1(Vvssy)

* Sources
Vres _RESET 0 DC=0 PWL 0 0 10ns 0 10.1ns vtrue
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
.TRAN @TIMESTEP@ @NANOSECONDS@ns sweep monte=list(2:2)

.variation
        option block_name=extern_data_run_@VARBLOCK@_0p9eu1
        option ignore_global_variation=yes
        option sampling_method=external
        option mcbrief=1
        option set_missing_values=zero
.end_variation

.include "ckt_varosc_data1273.sp"

* Measure
.param measure_cycles=8
.param start_cycle=4
.param measure_start=12ns

.measure tran FourCycle
+ trig V(x[0]) val='vtrue*0.5' td=measure_start rise='start_cycle'
+ targ V(x[0]) val='vtrue*0.5' td=measure_start rise='start_cycle+measure_cycles'
.measure tran Cycle         PARAM='FourCycle/4'
.measure tran Freq          PARAM='(1/(Cycle))'
.measure tran IdleCurrent   avg i(Vvcc) from 2.0ns to 10ns
.measure tran IdlePower     PARAM='(-IdleCurrent*vtrue)'

.END
