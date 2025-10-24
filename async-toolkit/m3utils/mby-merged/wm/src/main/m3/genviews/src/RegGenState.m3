MODULE RegGenState;
IMPORT TextSet, TextSetDef;
IMPORT Pathname;

REVEAL
  T = Public BRANDED Brand OBJECT
    dumpSyms : TextSet.T := NIL;
  OVERRIDES
    newSymbol := NewSymbol;
    initF := InitF;
    init := Init;
  END;

PROCEDURE NewSymbol(t : T; nm : TEXT) : BOOLEAN =
  BEGIN
    IF t.dumpSyms = NIL THEN
      t.dumpSyms := NEW(TextSetDef.T).init()
    END;
    RETURN NOT t.dumpSyms.insert(nm)
  END NewSymbol;

PROCEDURE InitF(t : T; from : T) : T =
  BEGIN
    t.dirPath := from.dirPath;
    t.dumpSyms := from.dumpSyms;
    RETURN t
  END InitF;

PROCEDURE Init(t : T; dirPath : Pathname.T) : T =
  BEGIN
    t.dirPath := dirPath;
    t.dumpSyms := NEW(TextSetDef.T).init();
    RETURN t
  END Init;
  
BEGIN END RegGenState.
