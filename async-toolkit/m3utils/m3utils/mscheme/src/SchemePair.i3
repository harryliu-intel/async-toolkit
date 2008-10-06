(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemePair;
IMPORT Wx;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE
  T <: Public;

  Public = OBJECT
    first, rest : SchemeObject.T;
  METHODS
    init(first, rest : SchemeObject.T) : T;
    equals(x : SchemeObject.T) : BOOLEAN;
    format() : TEXT RAISES { E };
    stringifyPair(quoted : BOOLEAN; buf : Wx.T) RAISES { E };
  END;

CONST Brand = "SchemePair";

END SchemePair.
