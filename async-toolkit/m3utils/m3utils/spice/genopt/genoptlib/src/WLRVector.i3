INTERFACE WLRVector;
IMPORT LRVector;

TYPE
  T = RECORD
    v : LRVector.T;
    y : LONGREAL;
    w : LONGREAL;
  END;

CONST Brand = "WLRVector";

PROCEDURE Format(READONLY a : T) : TEXT;
      
END WLRVector.
