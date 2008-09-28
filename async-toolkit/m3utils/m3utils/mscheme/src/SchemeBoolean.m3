(* $Id$ *)

MODULE SchemeBoolean;
FROM SchemeUtils IMPORT DebugFormat, Error;
FROM Scheme IMPORT Object;

VAR (* CONST *) LTrue, LFalse := NEW(T);

PROCEDURE Truth(x : BOOLEAN) : T =
  BEGIN IF x THEN RETURN LTrue ELSE RETURN LFalse END END Truth;

PROCEDURE TruthO(x : Object) : T =
  BEGIN
    IF ISTYPE(x,T) THEN RETURN x 
    ELSE RETURN TruthO(Error("expected a boolean, got: " & DebugFormat(x)))
    END
  END TruthO;

PROCEDURE True() : T = BEGIN RETURN LTrue END True;

PROCEDURE False() : T = BEGIN RETURN LFalse END False;

BEGIN 
  LTrue^ := TRUE; LFalse^ := FALSE
END SchemeBoolean.
