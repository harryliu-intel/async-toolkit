(* $Id$ *)

MODULE SchemeProcedure;
IMPORT SchemeProcedureClass;
FROM SchemeUtils IMPORT Stringify, Error;
FROM Scheme IMPORT Object, E;

REVEAL
  T = SchemeProcedureClass.Private BRANDED Brand OBJECT
  OVERRIDES
    format := Format;
  END;

PROCEDURE Format(t : T) : TEXT =
  BEGIN RETURN "{" & t.name & "}" END Format;

PROCEDURE Proc(x : Object) : T RAISES { E } =
  BEGIN
    IF x # NIL AND ISTYPE(x,T) THEN RETURN x 
    ELSE RETURN Proc(Error("Not a procedure: " & Stringify(x))) 
    END
  END Proc;

BEGIN END SchemeProcedure.
