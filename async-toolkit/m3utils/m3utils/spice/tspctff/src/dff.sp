* TSPC DFFs

.include 'tff.sp'

.subckt dffcore clk d a b c vcc vssx

MMau0  vcc   d    aui    ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMau1  aui   clk  a      ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMad   a     d    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'

MMbu   vcc   clk  b      ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMbd0  b     a    bdi    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'
MMbd1  bdi   clk  vssx   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'

Mcu    vcc   b    c      ln_FAKE_PMOS_BULK phpbulvt w=3 l=1.4e-08 nf=1 m='mult'
Mcd0   c     clk  cdi    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'
Mcd1   cdi   b    vssx   ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m='mult'

.ends

**********************************************************************

* semi-static DFF, reset high

.subckt ssdff_rst1 clk rst rstb d q qb vcc vssx

Xdff     clk d a b c vcc vssx dffcore
*Xstatc   clk rstb a b c vcc vssx statc

Mcr    vssx   rst    c    ln_FAKE_NMOS_BULK nhpbulvt w=3 l=1.4e-08 nf=1 m=1

Vqb qb c 0

Xqinv qb q vcc vssx i0sinv000aa1n06x5

.ends

**********************************************************************

* semi-static DFF, reset low

.subckt ssdff_rst0 clk rstb d q qb vcc vssx

Xdff     clk d a b c vcc vssx dffcore
*Xstatc   clk rstb a b c vcc vssx statc

Mcr    vcc   rstb    c    ln_FAKE_PMOS_BULK phpbulvt w=2 l=1.4e-08 nf=1 m=1

Vqb qb c 0

Xqinv qb q vcc vssx i0sinv000aa1n06x5

.ends

**********************************************************************

* SR latch

.subckt lsr000 s r q qb vcc vssx 

Xqb s q  qb vcc vssx i0snor002aa1n06x5
Xq  r qb q  vcc vssx i0snor002aa1n06x5

.ends


