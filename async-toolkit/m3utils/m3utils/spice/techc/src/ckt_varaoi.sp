* tech comparison
.TEMP @TEMP@
.PARAM vtrue=@VOLT@

@OPTIONS@

@MC_OPTIONS@

.option search='@HSPICE_MODEL_ROOT@'

.OPTION AUTOSTOP

.OPTION XA_CMD="set_monte_carlo_option -enable 1"              * not needed
.OPTION XA_CMD="set_monte_carlo_option -sample_output all"
.OPTION XA_CMD="set_monte_carlo_option -mc0_file 1"
.OPTION XA_CMD="set_monte_carlo_option -parameter_file 1"
.OPTION XA_CMD="set_monte_carlo_option -mc0_header 1"
.OPTION XA_CMD="set_monte_carlo_option -dump_waveform 1"

.lib '@HSPICE_MODEL@' @CORNER@

* std cell library with parasitics (optional):
@INCLUDELIB@

.SUBCKT nand_cell a b o1 vcc vssx 
* INPUT: a b 
* OUTPUT: o1 
* INOUT: vcc vssx 
*.PININFO a:I b:I 
*.PININFO o1:O 
*.PININFO vcc:B vssx:B 

************************
Mqn1 o1 a n1   vssx n@TRANSUFX@ @TRANSIZE@ 
Mqn2 n1 b vssx vssx n@TRANSUFX@ @TRANSIZE@ 
Mqp1 o1 a vcc  vcc  p@TRANSUFX@ @TRANSIZE@ 
Mqp2 o1 b vcc  vcc  p@TRANSUFX@ @TRANSIZE@ 
.ENDS  nand_cell

**********************************************************************

* Generic ring stage (make appropriate in/out/power connections to dut_cell)
.subckt ring_stage in out vcc vssx

*      A     B C      OUT    <--PWR-->
X0 @T0A@ @T0B@ @T0C@   xi   vcc  vssx  @PLUGTEXT@ @CELLNAME0@
X1 @T1A@ @T1B@ @T1C@   out  vcc  vssx  @PLUGTEXT@ @CELLNAME1@

* dummy loads on xi
Xload0 @T0A@ @T0B@ @T0C@   xi   vcc  vssx  @PLUGTEXT@ @CELLNAME0@
Xload1 @T0A@ @T0B@ @T0C@   xi   vcc  vssx  @PLUGTEXT@ @CELLNAME0@
Xload2 @T0A@ @T0B@ @T0C@   xi   vcc  vssx  @PLUGTEXT@ @CELLNAME0@

.ends

* Ring stage with fanout=N
*   => N-1 extra copies of dut_cell as load driven by in, with separate supply

.subckt ring_stage_fo4 in out vcc vload vssx

xstage in out    vload vssx ring_stage

* dummy loads on input
xload0 in unc[0] vload vssx ring_stage
xload1 in unc[1] vload vssx ring_stage
xload2 in unc[2] vload vssx ring_stage

.ends

* Ring oscillator with 10 DUT stages and 1 NAND2 enable
X1  x[0]              x[1]       vcc  vload vssx ring_stage_fo4
X2  x[1]              x[2]       vcc  vload vssx ring_stage_fo4
X3  x[2]              x[3]       vcc  vload vssx ring_stage_fo4
X4  x[3]              x[4]       vcc  vload vssx ring_stage_fo4
X5  x[4]              x[5]       vcc  vload vssx ring_stage_fo4
X6  x[5]              x[6]       vcc  vload vssx ring_stage_fo4
X7  x[6]              x[7]       vcc  vload vssx ring_stage_fo4
X8  x[7]              x[8]       vcc  vload vssx ring_stage_fo4
X9  x[8]              x[9]       vcc  vload vssx ring_stage_fo4
X10 x[9]              x[10]      vcc  vload vssx ring_stage_fo4
X21 _RESET x[10]      x[0]       vcc   vssx nand_cell

* Ring oscillator with 10 DUT stages and 1 NAND2 enable (leakage)
X101  x[100]              x[101]       vcc  vload vssy ring_stage_fo4
X102  x[101]              x[102]       vcc  vload vssy ring_stage_fo4
X103  x[102]              x[103]       vcc  vload vssy ring_stage_fo4
X104  x[103]              x[104]       vcc  vload vssy ring_stage_fo4
X105  x[104]              x[105]       vcc  vload vssy ring_stage_fo4
X106  x[105]              x[106]       vcc  vload vssy ring_stage_fo4
X107  x[106]              x[107]       vcc  vload vssy ring_stage_fo4
X108  x[107]              x[108]       vcc  vload vssy ring_stage_fo4
X109  x[108]              x[109]       vcc  vload vssy ring_stage_fo4
X110  x[109]              x[110]       vcc  vload vssy ring_stage_fo4
X121 vssy x[110]      x[100]       vcc   vssy nand_cell

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
.TRAN @TIMESTEP@ @NANOSECONDS@ns @MC_SPEC@

.variation
*        option block_name=extern_data_@THRESH@_z@Z@
        option ignore_global_variation=yes
*        option sampling_method=external
        option set_missing_values=zero
.end_variation


* Measure
.measure tran Cycle
+ trig V(x[0]) val='vtrue*0.5' td=8ns rise=1
+ targ V(x[0]) val='vtrue*0.5' td=8ns rise=2
.measure tran Freq          PARAM='(10/(Cycle))'
.measure tran IdleCurrent   avg i(Vvcc) from 2ns to 10ns
.measure tran IdlePower     PARAM='(-IdleCurrent*vtrue)'
.measure tran ActiveCurrent avg i(Vvcc) from=20ns to=30ns
.measure tran ActivePower   PARAM='(-ActiveCurrent*vtrue)'
.measure tran ActiveEnergy  PARAM='(ActivePower/Freq)'


.END
