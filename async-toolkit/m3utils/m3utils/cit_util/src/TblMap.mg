(* $Id$ *)

GENERIC MODULE TblMap(Tbl, Map);

TYPE
  T = Map.T OBJECT
    tbl         : Tbl.T;
    haveDefault := FALSE;
    default     : Map.Result;
  OVERRIDES
    eval := Eval;
  END;

PROCEDURE Wrap(tbl : Tbl.T) : Map.T =
  BEGIN RETURN NEW(T, tbl := tbl) END Wrap;

PROCEDURE WrapWithDefault(tbl : Tbl.T; default : Map.Result) : Map.T =
  BEGIN 
    RETURN NEW(T, tbl := tbl, haveDefault := TRUE, default := default) 
  END WrapWithDefault;

PROCEDURE Eval(t : T; from : Map.Argument) : Map.Result =
  VAR 
    res : Map.Result;
    fnd := t.tbl.get(from, res);
  BEGIN
    IF NOT fnd THEN
      IF t.haveDefault THEN RETURN t.default ELSE <*ASSERT FALSE*> END
    END;
    RETURN res
  END Eval;

BEGIN END TblMap.
