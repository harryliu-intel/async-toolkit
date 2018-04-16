INTERFACE RegModula3Generators;
IMPORT RegAddrmap, RegRegfile, RegReg;
IMPORT RegGenState;
IMPORT OSError, Thread, Wr;

PROCEDURE GenAddrmap(a : RegAddrmap.T; state : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure };

PROCEDURE GenRegfile(rf : RegRegfile.T; state : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure };

PROCEDURE GenReg(r : RegReg.T; state : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure };

END RegModula3Generators.
