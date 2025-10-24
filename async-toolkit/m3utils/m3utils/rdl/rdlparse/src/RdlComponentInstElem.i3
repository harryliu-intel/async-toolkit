INTERFACE RdlComponentInstElem;
IMPORT RdlArray, RdlNum;

TYPE
  T = OBJECT
    id : TEXT;
    array : RdlArray.T;
    eq, at, inc, mod : RdlNum.T;
  END;

CONST Brand = "RdlComponentInstElem";

CONST Equal : PROCEDURE(a, b : T) : BOOLEAN = NIL;
      
END RdlComponentInstElem.
