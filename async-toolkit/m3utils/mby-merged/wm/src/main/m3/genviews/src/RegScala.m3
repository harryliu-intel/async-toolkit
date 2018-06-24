MODULE RegScala EXPORTS RegScala, RegScalaGenerators;
IMPORT GenViewsScala;
IMPORT RegReg, RegGenState, RegRegfile, RegAddrmap;
IMPORT OSError, Thread, Wr;

REVEAL
  T = GenViewsScala.Compiler BRANDED Brand OBJECT
  OVERRIDES
  END;

PROCEDURE GenReg(r : RegReg.T; genState : RegGenState.T) =
  BEGIN
  END GenReg;
  
PROCEDURE GenAddrmap(map     : RegAddrmap.T; gsF : RegGenState.T) 
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
  END GenAddrmap;
  
PROCEDURE GenRegfile(rf       : RegRegfile.T;
                     genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  BEGIN
  END GenRegfile;
  
BEGIN END RegScala.
