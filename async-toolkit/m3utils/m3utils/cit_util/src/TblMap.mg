(* $Id$ *)

GENERIC MODULE TblMap(Tbl, Map);

REVEAL
  T = Public BRANDED Brand OBJECT
    t           : Tbl.T;
    haveDefault := FALSE;
    default     : Map.Result;
  OVERRIDES
    eval := Eval;
    tbl := Tabl;
  END;

PROCEDURE Tabl(t : T) : Tbl.T = BEGIN RETURN t.t END Tabl;

PROCEDURE Wrap(tbl : Tbl.T) : Map.T =
  BEGIN RETURN NEW(T, t := tbl) END Wrap;

PROCEDURE WrapWithDefault(tbl : Tbl.T; default : Map.Result) : Map.T =
  BEGIN 
    RETURN NEW(T, t := tbl, haveDefault := TRUE, default := default) 
  END WrapWithDefault;

PROCEDURE Eval(t : T; from : Map.Argument) : Map.Result =
  VAR 
    res : Map.Result;
    fnd := t.t.get(from, res);
  BEGIN
    IF NOT fnd THEN
      IF t.haveDefault THEN RETURN t.default ELSE <*ASSERT FALSE*> END
    END;
    RETURN res
  END Eval;

BEGIN END TblMap.
