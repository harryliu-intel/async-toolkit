* base definitions of TFFs

.param mult=2

**********************************************************************

.subckt tffcore clk a b c vcc vssx

MMau0  vcc   c    aui    ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMau1  aui   clk  a      ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMad   a     c    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'

MMbu   vcc   clk  b      ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMbd0  b     a    bdi    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMbd1  bdi   clk  vssx   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'

Mcu    vcc   b    c      ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
Mcd0   c     clk  cdi    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'
Mcd1   cdi   b    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'

.ends

**********************************************************************

.subckt dyntff_rst0 clk rst c vcc vssx

Xtff clk a b c vcc vssx tffcore

Mcr    vssx   rst    c    ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1

.ends

**********************************************************************

.subckt dyntff_rst1 clk rstb c vcc vssx

Xtff clk a b c vcc vssx tffcore

Mcr    vcc   rstb    c    ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1

.ends

**********************************************************************
* staticizers

.subckt statc clk rstb a b c vcc vssx

MMac0  vcc   a    cui    ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1
MMac1  cui   clk  c      ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1

MMdc0  c     a    cdi    ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1
MMdc1  cdi   b    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1
*MMdc2  cdj   rstb vssx   ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1

.ends

.subckt statb clk a c b vcc vssx

MMac0  vcc   a    bui    ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1
MMac1  bui   c    b      ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1

MMdb0  b     clk  bdi    ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1
MMdb1  bdi   c    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1

.ends

.subckt stata clk b c a vcc vssx

MMab0  a     clk  adi    ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1
MMab1  adi   ab    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=2 l=1.4e-08 nf=1 m=1
Xainv a ab vcc vssx i0sinv000aa1n02x5


.ends

**********************************************************************

* semi-static TFF, reset low

.subckt sstff_rst0 clk rst rstb c vcc vssx

Xtff     clk      a b c vcc vssx tffcore
Xstatc   clk rstb a b c vcc vssx statc

Mcr    vssx   rst    c    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=1


.ends

**********************************************************************

* semi-static TFF, reset high

.subckt sstff_rst1 clk rstb c vcc vssx

Xtff     clk a b c vcc vssx tffcore
Xstatc   clk rstb a b c vcc vssx statc

Mcr    vcc   rstb    c    ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1

.ends

**********************************************************************

* static TFF, reset low

.subckt stff_rst0 clk rst rstb c vcc vssx

Xtff     clk a b c vcc vssx tffcore
Xstata   clk b c a vcc vssx stata
Xstatb   clk a c b vcc vssx statb
Xstatc   clk rstb a b c vcc vssx statc

Mcr    vssx   rst    c    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=1

.ends

**********************************************************************

* static TFF, reset high

.subckt stff_rst1 clk rstb c vcc vssx

Xtff     clk a b c vcc vssx tffcore
Xstata   clk b c a vcc vssx stata
Xstatb   clk a c b vcc vssx statb
Xstatc   clk rstb a b c vcc vssx statc


Mcr    vcc   rstb    c    ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1

.ends
