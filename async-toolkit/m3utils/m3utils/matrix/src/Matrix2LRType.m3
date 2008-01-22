(* $Id$ *)

MODULE Matrix2LRType;
IMPORT Fmt;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.LongReal(t,style := Fmt.Style.Sci, prec := 3) END Format;

BEGIN END Matrix2LRType.
