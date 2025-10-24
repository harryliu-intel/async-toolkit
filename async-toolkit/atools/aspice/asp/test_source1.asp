.true=1;
.timemax=10e-9;
.timestep=0.1e-12;
.poststep=0;
wire(GND,"")

/* voltage source driving resistive load */
voltage_source {
  true*SIN(TIME/1e-9) -> x, Ix
}
res(x,GND)(1e-4)

/* current source driving resistive load */
source {
  (true*SIN(TIME/1e-9) - y)/1e-8 -> y, Iy
}
res(y,GND)(1e-4)

/* voltage source driving resistive load with sense resistor */
voltage_source {
  true*SIN(TIME/1e-9) -> z0, Iz
}
res zres(z0,z)(1e-8)
res(z,GND)(1e-4)

/* current source driving resistive load with sense resistor */
source {
  (true*SIN(TIME/1e-9) - w0)/1e-8 -> w0, Iw
}
res wres(w0,w)(1e-8)
res(w,GND)(1e-4)
