(* $Id$ *)

INTERFACE SchemePair;
IMPORT SchemeUtils, Wx;
FROM Scheme IMPORT Object;

TYPE
  T <: Public;

  Public = OBJECT
    first, rest : REFANY;
  METHODS
    init(first, rest : Object) : T;
    equals(x : Object) : BOOLEAN;
    format() : TEXT;
    stringifyPair(quoted : BOOLEAN; buf : Wx.T);
  END;

CONST Brand = "SchemePair";

END SchemePair.
