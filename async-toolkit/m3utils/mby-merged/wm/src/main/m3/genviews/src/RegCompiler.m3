MODULE RegCompiler;
IMPORT RegAddrmap, BigInt;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;
  
PROCEDURE Init(t : T; map : RegAddrmap.T) : T =
  BEGIN
    t.map := map;
    t.addr := BigInt.Zero;
    RETURN t
  END Init;

BEGIN END RegCompiler.
