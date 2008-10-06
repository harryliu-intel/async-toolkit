(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeBoolean;
FROM SchemeUtils IMPORT DebugFormat, Error;
FROM Scheme IMPORT Object;

REVEAL T = BRANDED Brand REF BOOLEAN;

VAR (* CONST *) LTrue, LFalse := NEW(T);

PROCEDURE Truth(x : BOOLEAN) : T =
  BEGIN IF x THEN RETURN LTrue ELSE RETURN LFalse END END Truth;

PROCEDURE TruthO(x : Object) : BOOLEAN =
  BEGIN RETURN x # LFalse END TruthO;

PROCEDURE True() : T = BEGIN RETURN LTrue END True;

PROCEDURE False() : T = BEGIN RETURN LFalse END False;

BEGIN 
  LTrue^ := TRUE; LFalse^ := FALSE
END SchemeBoolean.
