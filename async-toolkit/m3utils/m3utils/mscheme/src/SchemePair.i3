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
  T = BRANDED Brand REF RECORD
    first, rest : SchemeObject.T;
  END;

PROCEDURE StringifyPair(t : T; quoted : BOOLEAN; buf : Wx.T)  RAISES { E };

CONST Brand = "SchemePair";

END SchemePair.
