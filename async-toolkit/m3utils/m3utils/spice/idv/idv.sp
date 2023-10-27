* IDV 18A
.OPTION   POST=1 BRIEF probe accurate=1
.option cmiflag=1
.option cmiusrflag=3
.option scale=1.0
.option pdmi=1
.width output=132
.option mixed_num_format=1
.save level=none
.global vss

* .option cmipath='/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/cmi/hspice/cmi/lnx86/64bit'
* .LIB '/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2//models/core/hspice/m12_2x_1xa_1xb_6ya_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp' tttt

.option cmipath='/p/hdk/cad/pdk/pdk783_r0.9e_23ww29.2_beta/cmi/hspice/cmi/lnx86/64bit'
.LIB '/p/hdk/cad/pdk/pdk783_r0.8_23ww24.2/models/core/hspice/m12_2x_1xa_1xb_6ya_2yc__bm5_1ye_1yf_2ga_mim3x_1gb__bumpp/p1278_3.hsp' tttt

.include 'idvosc.spf'
.param avdd=0.3 atemp=85 hvdd='avdd/2' tstart=20p tend=10n
.temp atemp
.tran 1p tend
.probe tran V(o*) I(V_*)
vdd vdd 0 avdd
vss vss 0 0
ven en 0 pwl 0 0 10p avdd

V_8aoiZ2ulvt vdd vdd_8aoiZ2ulvt 0
X8aoiZ2ulvt en vdd o8aoiZ2ulvt vdd_8aoiZ2ulvt vss i70idvmaoi112211aa2top
V_8aoiZ2lvt vdd vdd_8aoiZ2lvt 0
X8aoiZ2lvt en vdd o8aoiZ2lvt vdd_8aoiZ2lvt vss i70idvmaoi112211bb2top
V_8aoiZ3ulvt vdd vdd_8aoiZ3ulvt 0
X8aoiZ3ulvt en vdd o8aoiZ3ulvt vdd_8aoiZ3ulvt vss i70idvmaoi113311aa2top
V_8aoiZ3lvt vdd vdd_8aoiZ3lvt 0
X8aoiZ3lvt en vdd o8aoiZ3lvt vdd_8aoiZ3lvt vss i70idvmaoi113311bb2top
V_8bfnZ2ulvt vdd vdd_8bfnZ2ulvt 0
X8bfnZ2ulvt en vdd o8bfnZ2ulvt vdd_8bfnZ2ulvt vss i70idvmbfn112211aa2top
V_8bfnZ2lvt vdd vdd_8bfnZ2lvt 0
X8bfnZ2lvt en vdd o8bfnZ2lvt vdd_8bfnZ2lvt vss i70idvmbfn112211bb2top
V_8bfnZ3ulvt vdd vdd_8bfnZ3ulvt 0
X8bfnZ3ulvt en vdd o8bfnZ3ulvt vdd_8bfnZ3ulvt vss i70idvmbfn113311aa2top
V_8bfnZ3lvt vdd vdd_8bfnZ3lvt 0
X8bfnZ3lvt en vdd o8bfnZ3lvt vdd_8bfnZ3lvt vss i70idvmbfn113311bb2top
V_8invZ2ulvt vdd vdd_8invZ2ulvt 0
X8invZ2ulvt en vdd o8invZ2ulvt vdd_8invZ2ulvt vss i70idvminv112211aa2top
V_8invZ2lvt vdd vdd_8invZ2lvt 0
X8invZ2lvt en vdd o8invZ2lvt vdd_8invZ2lvt vss i70idvminv112211bb2top
V_8invZ3ulvt vdd vdd_8invZ3ulvt 0
X8invZ3ulvt en vdd o8invZ3ulvt vdd_8invZ3ulvt vss i70idvminv113311aa2top
V_8invZ3lvt vdd vdd_8invZ3lvt 0
X8invZ3lvt en vdd o8invZ3lvt vdd_8invZ3lvt vss i70idvminv113311bb2top
V_8mdnZ2ulvt vdd vdd_8mdnZ2ulvt 0
X8mdnZ2ulvt en vdd o8mdnZ2ulvt vdd_8mdnZ2ulvt vss i70idvmmdn112211aa2top
V_8mdnZ2lvt vdd vdd_8mdnZ2lvt 0
X8mdnZ2lvt en vdd o8mdnZ2lvt vdd_8mdnZ2lvt vss i70idvmmdn112211bb2top
V_8mdnZ3ulvt vdd vdd_8mdnZ3ulvt 0
X8mdnZ3ulvt en vdd o8mdnZ3ulvt vdd_8mdnZ3ulvt vss i70idvmmdn113311aa2top
V_8mdnZ3lvt vdd vdd_8mdnZ3lvt 0
X8mdnZ3lvt en vdd o8mdnZ3lvt vdd_8mdnZ3lvt vss i70idvmmdn113311bb2top
V_8nanZ2ulvt vdd vdd_8nanZ2ulvt 0
X8nanZ2ulvt en vdd o8nanZ2ulvt vdd_8nanZ2ulvt vss i70idvmnan112211aa2top
V_8nanZ2lvt vdd vdd_8nanZ2lvt 0
X8nanZ2lvt en vdd o8nanZ2lvt vdd_8nanZ2lvt vss i70idvmnan112211bb2top
V_8nanZ3ulvt vdd vdd_8nanZ3ulvt 0
X8nanZ3ulvt en vdd o8nanZ3ulvt vdd_8nanZ3ulvt vss i70idvmnan113311aa2top
V_8nanZ3lvt vdd vdd_8nanZ3lvt 0
X8nanZ3lvt en vdd o8nanZ3lvt vdd_8nanZ3lvt vss i70idvmnan113311bb2top
V_8norZ2ulvt vdd vdd_8norZ2ulvt 0
X8norZ2ulvt en vdd o8norZ2ulvt vdd_8norZ2ulvt vss i70idvmnor112211aa2top
V_8norZ2lvt vdd vdd_8norZ2lvt 0
X8norZ2lvt en vdd o8norZ2lvt vdd_8norZ2lvt vss i70idvmnor112211bb2top
V_8norZ3ulvt vdd vdd_8norZ3ulvt 0
X8norZ3ulvt en vdd o8norZ3ulvt vdd_8norZ3ulvt vss i70idvmnor113311aa2top
V_8norZ3lvt vdd vdd_8norZ3lvt 0
X8norZ3lvt en vdd o8norZ3lvt vdd_8norZ3lvt vss i70idvmnor113311bb2top
V_6aoiZ1ulvt vdd vdd_6aoiZ1ulvt 0
X6aoiZ1ulvt en vdd o6aoiZ1ulvt vdd_6aoiZ1ulvt vss i70idvsaoi111111aa2top
V_6aoiZ1lvt vdd vdd_6aoiZ1lvt 0
X6aoiZ1lvt en vdd o6aoiZ1lvt vdd_6aoiZ1lvt vss i70idvsaoi111111bb2top
V_6aoiZ2ulvt vdd vdd_6aoiZ2ulvt 0
X6aoiZ2ulvt en vdd o6aoiZ2ulvt vdd_6aoiZ2ulvt vss i70idvsaoi112211aa2top
V_6aoiZ2lvt vdd vdd_6aoiZ2lvt 0
X6aoiZ2lvt en vdd o6aoiZ2lvt vdd_6aoiZ2lvt vss i70idvsaoi112211bb2top
V_6aoiZ3ulvt vdd vdd_6aoiZ3ulvt 0
X6aoiZ3ulvt en vdd o6aoiZ3ulvt vdd_6aoiZ3ulvt vss i70idvsaoi113311aa2top
V_6aoiZ3lvt vdd vdd_6aoiZ3lvt 0
X6aoiZ3lvt en vdd o6aoiZ3lvt vdd_6aoiZ3lvt vss i70idvsaoi113311bb2top
V_6bfnZ1ulvt vdd vdd_6bfnZ1ulvt 0
X6bfnZ1ulvt en vdd o6bfnZ1ulvt vdd_6bfnZ1ulvt vss i70idvsbfn111111aa2top
V_6bfnZ1lvt vdd vdd_6bfnZ1lvt 0
X6bfnZ1lvt en vdd o6bfnZ1lvt vdd_6bfnZ1lvt vss i70idvsbfn111111bb2top
V_6bfnZ2ulvt vdd vdd_6bfnZ2ulvt 0
X6bfnZ2ulvt en vdd o6bfnZ2ulvt vdd_6bfnZ2ulvt vss i70idvsbfn112211aa2top
V_6bfnZ2lvt vdd vdd_6bfnZ2lvt 0
X6bfnZ2lvt en vdd o6bfnZ2lvt vdd_6bfnZ2lvt vss i70idvsbfn112211bb2top
V_6bfnZ3ulvt vdd vdd_6bfnZ3ulvt 0
X6bfnZ3ulvt en vdd o6bfnZ3ulvt vdd_6bfnZ3ulvt vss i70idvsbfn113311aa2top
V_6bfnZ3lvt vdd vdd_6bfnZ3lvt 0
X6bfnZ3lvt en vdd o6bfnZ3lvt vdd_6bfnZ3lvt vss i70idvsbfn113311bb2top
V_6invZ1ulvt vdd vdd_6invZ1ulvt 0
X6invZ1ulvt en vdd o6invZ1ulvt vdd_6invZ1ulvt vss i70idvsinv111111aa2top
V_6invZ1lvt vdd vdd_6invZ1lvt 0
X6invZ1lvt en vdd o6invZ1lvt vdd_6invZ1lvt vss i70idvsinv111111bb2top
V_6invZ2ulvt vdd vdd_6invZ2ulvt 0
X6invZ2ulvt en vdd o6invZ2ulvt vdd_6invZ2ulvt vss i70idvsinv112211aa2top
V_6invZ2lvt vdd vdd_6invZ2lvt 0
X6invZ2lvt en vdd o6invZ2lvt vdd_6invZ2lvt vss i70idvsinv112211bb2top
V_6invZ3ulvt vdd vdd_6invZ3ulvt 0
X6invZ3ulvt en vdd o6invZ3ulvt vdd_6invZ3ulvt vss i70idvsinv113311aa2top
V_6invZ3lvt vdd vdd_6invZ3lvt 0
X6invZ3lvt en vdd o6invZ3lvt vdd_6invZ3lvt vss i70idvsinv113311bb2top
V_6mdnZ1ulvt vdd vdd_6mdnZ1ulvt 0
X6mdnZ1ulvt en vdd o6mdnZ1ulvt vdd_6mdnZ1ulvt vss i70idvsmdn111111aa2top
V_6mdnZ1lvt vdd vdd_6mdnZ1lvt 0
X6mdnZ1lvt en vdd o6mdnZ1lvt vdd_6mdnZ1lvt vss i70idvsmdn111111bb2top
V_6mdnZ2ulvt vdd vdd_6mdnZ2ulvt 0
X6mdnZ2ulvt en vdd o6mdnZ2ulvt vdd_6mdnZ2ulvt vss i70idvsmdn112211aa2top
V_6mdnZ2lvt vdd vdd_6mdnZ2lvt 0
X6mdnZ2lvt en vdd o6mdnZ2lvt vdd_6mdnZ2lvt vss i70idvsmdn112211bb2top
V_6mdnZ3ulvt vdd vdd_6mdnZ3ulvt 0
X6mdnZ3ulvt en vdd o6mdnZ3ulvt vdd_6mdnZ3ulvt vss i70idvsmdn113311aa2top
V_6mdnZ3lvt vdd vdd_6mdnZ3lvt 0
X6mdnZ3lvt en vdd o6mdnZ3lvt vdd_6mdnZ3lvt vss i70idvsmdn113311bb2top
V_6nanZ1ulvt vdd vdd_6nanZ1ulvt 0
X6nanZ1ulvt en vdd o6nanZ1ulvt vdd_6nanZ1ulvt vss i70idvsnan111111aa2top
V_6nanZ1lvt vdd vdd_6nanZ1lvt 0
X6nanZ1lvt en vdd o6nanZ1lvt vdd_6nanZ1lvt vss i70idvsnan111111bb2top
V_6nanZ2ulvt vdd vdd_6nanZ2ulvt 0
X6nanZ2ulvt en vdd o6nanZ2ulvt vdd_6nanZ2ulvt vss i70idvsnan112211aa2top
V_6nanZ2lvt vdd vdd_6nanZ2lvt 0
X6nanZ2lvt en vdd o6nanZ2lvt vdd_6nanZ2lvt vss i70idvsnan112211bb2top
V_6nanZ3ulvt vdd vdd_6nanZ3ulvt 0
X6nanZ3ulvt en vdd o6nanZ3ulvt vdd_6nanZ3ulvt vss i70idvsnan113311aa2top
V_6nanZ3lvt vdd vdd_6nanZ3lvt 0
X6nanZ3lvt en vdd o6nanZ3lvt vdd_6nanZ3lvt vss i70idvsnan113311bb2top
V_6norZ1ulvt vdd vdd_6norZ1ulvt 0
X6norZ1ulvt en vdd o6norZ1ulvt vdd_6norZ1ulvt vss i70idvsnor111111aa2top
V_6norZ1lvt vdd vdd_6norZ1lvt 0
X6norZ1lvt en vdd o6norZ1lvt vdd_6norZ1lvt vss i70idvsnor111111bb2top
V_6norZ2ulvt vdd vdd_6norZ2ulvt 0
X6norZ2ulvt en vdd o6norZ2ulvt vdd_6norZ2ulvt vss i70idvsnor112211aa2top
V_6norZ2lvt vdd vdd_6norZ2lvt 0
X6norZ2lvt en vdd o6norZ2lvt vdd_6norZ2lvt vss i70idvsnor112211bb2top
V_6norZ3ulvt vdd vdd_6norZ3ulvt 0
X6norZ3ulvt en vdd o6norZ3ulvt vdd_6norZ3ulvt vss i70idvsnor113311aa2top
V_6norZ3lvt vdd vdd_6norZ3lvt 0
X6norZ3lvt en vdd o6norZ3lvt vdd_6norZ3lvt vss i70idvsnor113311bb2top

.measure tran I_8aoiZ2ulvt avg I(V_8aoiZ2ulvt) from=tstart to= tend
.measure tran per_8aoiZ2ulvt TRIG V(o8aoiZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o8aoiZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_8aoiZ2lvt avg I(V_8aoiZ2lvt) from=tstart to= tend
.measure tran per_8aoiZ2lvt TRIG V(o8aoiZ2lvt) VAL=hvdd  CROSS=2 TARG V(o8aoiZ2lvt) VAL=hvdd CROSS=4
.measure tran I_8aoiZ3ulvt avg I(V_8aoiZ3ulvt) from=tstart to= tend
.measure tran per_8aoiZ3ulvt TRIG V(o8aoiZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o8aoiZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_8aoiZ3lvt avg I(V_8aoiZ3lvt) from=tstart to= tend
.measure tran per_8aoiZ3lvt TRIG V(o8aoiZ3lvt) VAL=hvdd  CROSS=2 TARG V(o8aoiZ3lvt) VAL=hvdd CROSS=4
.measure tran I_8bfnZ2ulvt avg I(V_8bfnZ2ulvt) from=tstart to= tend
.measure tran per_8bfnZ2ulvt TRIG V(o8bfnZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o8bfnZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_8bfnZ2lvt avg I(V_8bfnZ2lvt) from=tstart to= tend
.measure tran per_8bfnZ2lvt TRIG V(o8bfnZ2lvt) VAL=hvdd  CROSS=2 TARG V(o8bfnZ2lvt) VAL=hvdd CROSS=4
.measure tran I_8bfnZ3ulvt avg I(V_8bfnZ3ulvt) from=tstart to= tend
.measure tran per_8bfnZ3ulvt TRIG V(o8bfnZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o8bfnZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_8bfnZ3lvt avg I(V_8bfnZ3lvt) from=tstart to= tend
.measure tran per_8bfnZ3lvt TRIG V(o8bfnZ3lvt) VAL=hvdd  CROSS=2 TARG V(o8bfnZ3lvt) VAL=hvdd CROSS=4
.measure tran I_8invZ2ulvt avg I(V_8invZ2ulvt) from=tstart to= tend
.measure tran per_8invZ2ulvt TRIG V(o8invZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o8invZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_8invZ2lvt avg I(V_8invZ2lvt) from=tstart to= tend
.measure tran per_8invZ2lvt TRIG V(o8invZ2lvt) VAL=hvdd  CROSS=2 TARG V(o8invZ2lvt) VAL=hvdd CROSS=4
.measure tran I_8invZ3ulvt avg I(V_8invZ3ulvt) from=tstart to= tend
.measure tran per_8invZ3ulvt TRIG V(o8invZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o8invZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_8invZ3lvt avg I(V_8invZ3lvt) from=tstart to= tend
.measure tran per_8invZ3lvt TRIG V(o8invZ3lvt) VAL=hvdd  CROSS=2 TARG V(o8invZ3lvt) VAL=hvdd CROSS=4
.measure tran I_8mdnZ2ulvt avg I(V_8mdnZ2ulvt) from=tstart to= tend
.measure tran per_8mdnZ2ulvt TRIG V(o8mdnZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o8mdnZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_8mdnZ2lvt avg I(V_8mdnZ2lvt) from=tstart to= tend
.measure tran per_8mdnZ2lvt TRIG V(o8mdnZ2lvt) VAL=hvdd  CROSS=2 TARG V(o8mdnZ2lvt) VAL=hvdd CROSS=4
.measure tran I_8mdnZ3ulvt avg I(V_8mdnZ3ulvt) from=tstart to= tend
.measure tran per_8mdnZ3ulvt TRIG V(o8mdnZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o8mdnZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_8mdnZ3lvt avg I(V_8mdnZ3lvt) from=tstart to= tend
.measure tran per_8mdnZ3lvt TRIG V(o8mdnZ3lvt) VAL=hvdd  CROSS=2 TARG V(o8mdnZ3lvt) VAL=hvdd CROSS=4
.measure tran I_8nanZ2ulvt avg I(V_8nanZ2ulvt) from=tstart to= tend
.measure tran per_8nanZ2ulvt TRIG V(o8nanZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o8nanZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_8nanZ2lvt avg I(V_8nanZ2lvt) from=tstart to= tend
.measure tran per_8nanZ2lvt TRIG V(o8nanZ2lvt) VAL=hvdd  CROSS=2 TARG V(o8nanZ2lvt) VAL=hvdd CROSS=4
.measure tran I_8nanZ3ulvt avg I(V_8nanZ3ulvt) from=tstart to= tend
.measure tran per_8nanZ3ulvt TRIG V(o8nanZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o8nanZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_8nanZ3lvt avg I(V_8nanZ3lvt) from=tstart to= tend
.measure tran per_8nanZ3lvt TRIG V(o8nanZ3lvt) VAL=hvdd  CROSS=2 TARG V(o8nanZ3lvt) VAL=hvdd CROSS=4
.measure tran I_8norZ2ulvt avg I(V_8norZ2ulvt) from=tstart to= tend
.measure tran per_8norZ2ulvt TRIG V(o8norZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o8norZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_8norZ2lvt avg I(V_8norZ2lvt) from=tstart to= tend
.measure tran per_8norZ2lvt TRIG V(o8norZ2lvt) VAL=hvdd  CROSS=2 TARG V(o8norZ2lvt) VAL=hvdd CROSS=4
.measure tran I_8norZ3ulvt avg I(V_8norZ3ulvt) from=tstart to= tend
.measure tran per_8norZ3ulvt TRIG V(o8norZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o8norZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_8norZ3lvt avg I(V_8norZ3lvt) from=tstart to= tend
.measure tran per_8norZ3lvt TRIG V(o8norZ3lvt) VAL=hvdd  CROSS=2 TARG V(o8norZ3lvt) VAL=hvdd CROSS=4
.measure tran I_6aoiZ1ulvt avg I(V_6aoiZ1ulvt) from=tstart to= tend
.measure tran per_6aoiZ1ulvt TRIG V(o6aoiZ1ulvt) VAL=hvdd  CROSS=2 TARG V(o6aoiZ1ulvt) VAL=hvdd CROSS=4
.measure tran I_6aoiZ1lvt avg I(V_6aoiZ1lvt) from=tstart to= tend
.measure tran per_6aoiZ1lvt TRIG V(o6aoiZ1lvt) VAL=hvdd  CROSS=2 TARG V(o6aoiZ1lvt) VAL=hvdd CROSS=4
.measure tran I_6aoiZ2ulvt avg I(V_6aoiZ2ulvt) from=tstart to= tend
.measure tran per_6aoiZ2ulvt TRIG V(o6aoiZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o6aoiZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_6aoiZ2lvt avg I(V_6aoiZ2lvt) from=tstart to= tend
.measure tran per_6aoiZ2lvt TRIG V(o6aoiZ2lvt) VAL=hvdd  CROSS=2 TARG V(o6aoiZ2lvt) VAL=hvdd CROSS=4
.measure tran I_6aoiZ3ulvt avg I(V_6aoiZ3ulvt) from=tstart to= tend
.measure tran per_6aoiZ3ulvt TRIG V(o6aoiZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o6aoiZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_6aoiZ3lvt avg I(V_6aoiZ3lvt) from=tstart to= tend
.measure tran per_6aoiZ3lvt TRIG V(o6aoiZ3lvt) VAL=hvdd  CROSS=2 TARG V(o6aoiZ3lvt) VAL=hvdd CROSS=4
.measure tran I_6bfnZ1ulvt avg I(V_6bfnZ1ulvt) from=tstart to= tend
.measure tran per_6bfnZ1ulvt TRIG V(o6bfnZ1ulvt) VAL=hvdd  CROSS=2 TARG V(o6bfnZ1ulvt) VAL=hvdd CROSS=4
.measure tran I_6bfnZ1lvt avg I(V_6bfnZ1lvt) from=tstart to= tend
.measure tran per_6bfnZ1lvt TRIG V(o6bfnZ1lvt) VAL=hvdd  CROSS=2 TARG V(o6bfnZ1lvt) VAL=hvdd CROSS=4
.measure tran I_6bfnZ2ulvt avg I(V_6bfnZ2ulvt) from=tstart to= tend
.measure tran per_6bfnZ2ulvt TRIG V(o6bfnZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o6bfnZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_6bfnZ2lvt avg I(V_6bfnZ2lvt) from=tstart to= tend
.measure tran per_6bfnZ2lvt TRIG V(o6bfnZ2lvt) VAL=hvdd  CROSS=2 TARG V(o6bfnZ2lvt) VAL=hvdd CROSS=4
.measure tran I_6bfnZ3ulvt avg I(V_6bfnZ3ulvt) from=tstart to= tend
.measure tran per_6bfnZ3ulvt TRIG V(o6bfnZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o6bfnZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_6bfnZ3lvt avg I(V_6bfnZ3lvt) from=tstart to= tend
.measure tran per_6bfnZ3lvt TRIG V(o6bfnZ3lvt) VAL=hvdd  CROSS=2 TARG V(o6bfnZ3lvt) VAL=hvdd CROSS=4
.measure tran I_6invZ1ulvt avg I(V_6invZ1ulvt) from=tstart to= tend
.measure tran per_6invZ1ulvt TRIG V(o6invZ1ulvt) VAL=hvdd  CROSS=2 TARG V(o6invZ1ulvt) VAL=hvdd CROSS=4
.measure tran I_6invZ1lvt avg I(V_6invZ1lvt) from=tstart to= tend
.measure tran per_6invZ1lvt TRIG V(o6invZ1lvt) VAL=hvdd  CROSS=2 TARG V(o6invZ1lvt) VAL=hvdd CROSS=4
.measure tran I_6invZ2ulvt avg I(V_6invZ2ulvt) from=tstart to= tend
.measure tran per_6invZ2ulvt TRIG V(o6invZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o6invZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_6invZ2lvt avg I(V_6invZ2lvt) from=tstart to= tend
.measure tran per_6invZ2lvt TRIG V(o6invZ2lvt) VAL=hvdd  CROSS=2 TARG V(o6invZ2lvt) VAL=hvdd CROSS=4
.measure tran I_6invZ3ulvt avg I(V_6invZ3ulvt) from=tstart to= tend
.measure tran per_6invZ3ulvt TRIG V(o6invZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o6invZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_6invZ3lvt avg I(V_6invZ3lvt) from=tstart to= tend
.measure tran per_6invZ3lvt TRIG V(o6invZ3lvt) VAL=hvdd  CROSS=2 TARG V(o6invZ3lvt) VAL=hvdd CROSS=4
.measure tran I_6mdnZ1ulvt avg I(V_6mdnZ1ulvt) from=tstart to= tend
.measure tran per_6mdnZ1ulvt TRIG V(o6mdnZ1ulvt) VAL=hvdd  CROSS=2 TARG V(o6mdnZ1ulvt) VAL=hvdd CROSS=4
.measure tran I_6mdnZ1lvt avg I(V_6mdnZ1lvt) from=tstart to= tend
.measure tran per_6mdnZ1lvt TRIG V(o6mdnZ1lvt) VAL=hvdd  CROSS=2 TARG V(o6mdnZ1lvt) VAL=hvdd CROSS=4
.measure tran I_6mdnZ2ulvt avg I(V_6mdnZ2ulvt) from=tstart to= tend
.measure tran per_6mdnZ2ulvt TRIG V(o6mdnZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o6mdnZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_6mdnZ2lvt avg I(V_6mdnZ2lvt) from=tstart to= tend
.measure tran per_6mdnZ2lvt TRIG V(o6mdnZ2lvt) VAL=hvdd  CROSS=2 TARG V(o6mdnZ2lvt) VAL=hvdd CROSS=4
.measure tran I_6mdnZ3ulvt avg I(V_6mdnZ3ulvt) from=tstart to= tend
.measure tran per_6mdnZ3ulvt TRIG V(o6mdnZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o6mdnZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_6mdnZ3lvt avg I(V_6mdnZ3lvt) from=tstart to= tend
.measure tran per_6mdnZ3lvt TRIG V(o6mdnZ3lvt) VAL=hvdd  CROSS=2 TARG V(o6mdnZ3lvt) VAL=hvdd CROSS=4
.measure tran I_6nanZ1ulvt avg I(V_6nanZ1ulvt) from=tstart to= tend
.measure tran per_6nanZ1ulvt TRIG V(o6nanZ1ulvt) VAL=hvdd  CROSS=2 TARG V(o6nanZ1ulvt) VAL=hvdd CROSS=4
.measure tran I_6nanZ1lvt avg I(V_6nanZ1lvt) from=tstart to= tend
.measure tran per_6nanZ1lvt TRIG V(o6nanZ1lvt) VAL=hvdd  CROSS=2 TARG V(o6nanZ1lvt) VAL=hvdd CROSS=4
.measure tran I_6nanZ2ulvt avg I(V_6nanZ2ulvt) from=tstart to= tend
.measure tran per_6nanZ2ulvt TRIG V(o6nanZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o6nanZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_6nanZ2lvt avg I(V_6nanZ2lvt) from=tstart to= tend
.measure tran per_6nanZ2lvt TRIG V(o6nanZ2lvt) VAL=hvdd  CROSS=2 TARG V(o6nanZ2lvt) VAL=hvdd CROSS=4
.measure tran I_6nanZ3ulvt avg I(V_6nanZ3ulvt) from=tstart to= tend
.measure tran per_6nanZ3ulvt TRIG V(o6nanZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o6nanZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_6nanZ3lvt avg I(V_6nanZ3lvt) from=tstart to= tend
.measure tran per_6nanZ3lvt TRIG V(o6nanZ3lvt) VAL=hvdd  CROSS=2 TARG V(o6nanZ3lvt) VAL=hvdd CROSS=4
.measure tran I_6norZ1ulvt avg I(V_6norZ1ulvt) from=tstart to= tend
.measure tran per_6norZ1ulvt TRIG V(o6norZ1ulvt) VAL=hvdd  CROSS=2 TARG V(o6norZ1ulvt) VAL=hvdd CROSS=4
.measure tran I_6norZ1lvt avg I(V_6norZ1lvt) from=tstart to= tend
.measure tran per_6norZ1lvt TRIG V(o6norZ1lvt) VAL=hvdd  CROSS=2 TARG V(o6norZ1lvt) VAL=hvdd CROSS=4
.measure tran I_6norZ2ulvt avg I(V_6norZ2ulvt) from=tstart to= tend
.measure tran per_6norZ2ulvt TRIG V(o6norZ2ulvt) VAL=hvdd  CROSS=2 TARG V(o6norZ2ulvt) VAL=hvdd CROSS=4
.measure tran I_6norZ2lvt avg I(V_6norZ2lvt) from=tstart to= tend
.measure tran per_6norZ2lvt TRIG V(o6norZ2lvt) VAL=hvdd  CROSS=2 TARG V(o6norZ2lvt) VAL=hvdd CROSS=4
.measure tran I_6norZ3ulvt avg I(V_6norZ3ulvt) from=tstart to= tend
.measure tran per_6norZ3ulvt TRIG V(o6norZ3ulvt) VAL=hvdd  CROSS=2 TARG V(o6norZ3ulvt) VAL=hvdd CROSS=4
.measure tran I_6norZ3lvt avg I(V_6norZ3lvt) from=tstart to= tend
.measure tran per_6norZ3lvt TRIG V(o6norZ3lvt) VAL=hvdd  CROSS=2 TARG V(o6norZ3lvt) VAL=hvdd CROSS=4
.alter
.param avdd=0.35 tend=8n
.alter
.param avdd=0.27 tend=20n
.end
