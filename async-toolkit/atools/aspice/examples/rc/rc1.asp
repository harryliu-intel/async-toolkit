.timemax=2e-9;
.timestep=0.1e-9;
.poststep=0.1e-9;
.coupling_cutoff=0.5;

wire(GND,"")
voltage_source { TIME < 2e-9 ? TIME/2e-9 : 1 -> Vdd }

/** test strongly coupled resistor **/
res(Vdd,x)(10)
res(x,y)(1)
res(y,GND)(10)

/** test strongly coupled capacitor **/
cap(Vdd,a)(1e-15)
cap(a,b)(10e-15)
cap(b,GND)(1e-15)
