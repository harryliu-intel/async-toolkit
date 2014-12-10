.true=1;
.timemax=10e-9;
.timestep=0.1e-12;
.poststep=0;
wire(GND,"")

/* voltage source driving capacitive load */
voltage_source {
  true*SIN(TIME/1e-9) -> x, Ix
}
cap(x)(1e-3)

/* current source driving capacitive load */
source {
  (true*SIN(TIME/1e-9) - y)/1e-8 -> y, Iy
}
res(y,GND)(1e-4)
cap(y)(1e-3)

/* voltage source driving capacitive load with sense resistor */
voltage_source {
  true*SIN(TIME/1e-9) -> z0, Iz
}
res zres(z0,z)(1e-8)
cap(z)(1e-3)

/* current source driving capacitive load with sense resistor */
source {
  (true*SIN(TIME/1e-9) - w0)/1e-8 -> w0, Iw
}
res wres(w0,w)(1e-8)
cap(w)(1e-3)
