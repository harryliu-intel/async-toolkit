(* $Id$ *)

INTERFACE SchemePair;
IMPORT Wx;
IMPORT SchemeObject;

TYPE
  T <: Public;

  Public = OBJECT
    first, rest : SchemeObject.T;
  METHODS
    init(first, rest : SchemeObject.T) : T;
    equals(x : SchemeObject.T) : BOOLEAN;
    format() : TEXT;
    stringifyPair(quoted : BOOLEAN; buf : Wx.T);
  END;

CONST Brand = "SchemePair";

END SchemePair.
