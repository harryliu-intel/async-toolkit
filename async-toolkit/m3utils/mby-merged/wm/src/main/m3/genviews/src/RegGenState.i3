INTERFACE RegGenState;
IMPORT TextSet, Pathname;

TYPE
  T = BRANDED OBJECT
    dumpSyms : TextSet.T;
    dirPath  : Pathname.T;
  END;

CONST Brand = "RegGenState";

END RegGenState.
  
