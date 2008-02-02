(* $Id$ *)

MODULE Matrix2XType;
IMPORT Fmt;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.Extended(t,style := Fmt.Style.Sci, prec := 3) END Format;

BEGIN END Matrix2XType.
