* TSPC tests

.TEMP 0
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

* no parasitics for now
.include '/p/hdk/cad/stdcells/lib783_i0s_160h_50pp/pdk080_r4v2p0_efv/base_ulvt/spice/lib783_i0s_160h_50pp_base_ulvt.sp'

.include 'tff.sp'


**********************************************************************

Vres rstb 0 DC=0 PWL 0 0 0.99ns 0 1.01ns vtrue
Xrinv rstb rst vcc vssx i0sinv000aa1n02x5

Vclk clk  0 DC=0 PWL 0 0
+ 1.99ns 0      2.01ns  vtrue
+ 2.49ns vtrue  2.51ns  0
+ 2.99ns 0      3.01ns  vtrue
+ 3.49ns vtrue  3.51ns  0
+ 3.99ns 0      4.01ns  vtrue
+ 4.49ns vtrue  4.51ns  0
+ 4.99ns 0      5.01ns  vtrue

Vvcc    vcc   0 DC=vtrue
Vvssx   vssx  0 DC=0


Xdutdyn0 clk rst  cdyn0 vcc vssx dyntff_rst0
Xdutdyn1 clk rstb cdyn1 vcc vssx dyntff_rst1

Xdutss0 clk rst  css0 vcc vssx sstff_rst0
Xdutss1 clk rstb css1 vcc vssx sstff_rst1

Xduts0 clk rst  cs0 vcc vssx stff_rst0
Xduts1 clk rstb cs1 vcc vssx stff_rst1

.probe tran v(*)
.TRAN 1p 10n

.END