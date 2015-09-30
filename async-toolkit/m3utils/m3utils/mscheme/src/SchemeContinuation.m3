(* $Id: SchemeContinuation.m3,v 1.3 2008/10/06 08:12:46 mika Exp $ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeContinuation;
FROM Scheme IMPORT E, Object;
FROM SchemeUtils IMPORT First;
IMPORT Scheme;

REVEAL 
  T = Public BRANDED Brand OBJECT
    OVERRIDES
      init := Init;
      apply := Apply;
    END;

PROCEDURE Init(t : T; cc : TEXT) : T = 
  BEGIN t.cc := cc; RETURN t END Init;

PROCEDURE Apply(t : T; <*UNUSED*>interp : Scheme.T; args : Object) : Object 
  RAISES { E } =
  BEGIN
    t.value := First(args);
    RAISE E(t.cc)
  END Apply;

BEGIN END SchemeContinuation.
