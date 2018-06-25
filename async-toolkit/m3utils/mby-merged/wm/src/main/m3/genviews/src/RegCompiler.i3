INTERFACE RegCompiler;
IMPORT RegAddrmap;

TYPE
  T = OBJECT METHODS
    init(map : RegAddrmap.T) : T;
  END;

CONST Brand = "RegCompiler";

END RegCompiler.
