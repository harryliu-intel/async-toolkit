(* $Id$ *)

MODULE Matrix2RType;
IMPORT Fmt;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.Real(t,style := Fmt.Style.Sci, prec := 3) END Format;

BEGIN END Matrix2RType.
