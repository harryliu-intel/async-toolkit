.timemax=2e-9;
.timestep=0.1e-9;
.poststep=0.1e-9;
.coupling_cutoff=0.5;

wire(GND,"")
voltage_source { TIME < 2e-9 ? TIME/2e-9 : 1 -> Vdd }

/** test strongly coupled resistor **/
res(Vdd,x)(10)
res(x,y)(2)
res(x,z)(2)
res(y,w)(2)
res(z,w)(2)
res(w,GND)(10)

/** test strongly coupled capacitor **/
cap(Vdd,a)(1e-15)
cap(a,b)(5e-15)
cap(a,c)(5e-15)
cap(b,d)(5e-15)
cap(c,d)(5e-15)
cap(d,GND)(1e-15)
