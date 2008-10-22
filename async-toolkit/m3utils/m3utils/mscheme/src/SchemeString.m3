(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeString;
IMPORT Text;

PROCEDURE FromText(txt : TEXT) : T =
  BEGIN
    IF txt = NIL THEN RETURN NIL END;

    VAR     str := NEW(T, Text.Length(txt));
    BEGIN
      FOR i := FIRST(str^) TO LAST(str^) DO
        str[i] := Text.GetChar(txt,i)
      END;
      RETURN str
    END
  END FromText;

PROCEDURE ToText(t : T) : TEXT =
  BEGIN RETURN Text.FromChars(t^) END ToText;

BEGIN END SchemeString.
