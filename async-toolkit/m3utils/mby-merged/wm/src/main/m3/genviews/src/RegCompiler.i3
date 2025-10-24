INTERFACE RegCompiler;
IMPORT RegAddrmap, BigInt;

TYPE
  T <: Public;

  Public = OBJECT
    map  : RegAddrmap.T;
    addr : BigInt.T;
  METHODS
    init(map : RegAddrmap.T) : T;
  END;

CONST Brand = "RegCompiler";

END RegCompiler.
