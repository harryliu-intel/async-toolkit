(* $Id$ *)

MODULE SchemeChar;
FROM SchemeUtils IMPORT DebugFormat, Error;
IMPORT SchemeObject;

REVEAL T = BRANDED Brand REF CHAR;

PROCEDURE Char(x : SchemeObject.T) : CHAR =
  BEGIN
    IF x # NIL AND ISTYPE(x, T) THEN RETURN NARROW(x,T)^ 
    ELSE RETURN Char(Error("expected a char, got: " & DebugFormat(x))) 
    END
  END Char;

PROCEDURE Chr(x : SchemeObject.T) : T =
  BEGIN
    IF x # NIL AND ISTYPE(x, T) THEN RETURN x
    ELSE RETURN Chr(Error("expected a char, got: " & DebugFormat(x))) 
    END
  END Chr;

PROCEDURE IChr(x : INTEGER) : T =
  BEGIN RETURN Array[VAL(x MOD NUMBER(CHAR),CHAR)] END IChr;
  
PROCEDURE Character(c : CHAR) : T =
  BEGIN RETURN Array[c] END Character;

VAR (* CONST *) Array := NEW(REF ARRAY CHAR OF T);

BEGIN 
  FOR i := FIRST(CHAR) TO LAST(CHAR) DO
    Array[i] := NEW(T);
    Array[i]^ := i
  END
END SchemeChar.
