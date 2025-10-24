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

/* relative voltage source driving resistive load */
voltage_source {
  x+true/2 -> y, Iy
}
res(y,GND)(1e-4)
