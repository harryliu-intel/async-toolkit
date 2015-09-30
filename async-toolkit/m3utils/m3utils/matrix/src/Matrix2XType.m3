(* $Id: Matrix2XType.m3,v 1.2 2008/02/04 00:10:46 mika Exp $ *)

MODULE Matrix2XType;
IMPORT Fmt;
IMPORT Random;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.Extended(t,style := Fmt.Style.Sci, prec := 3) END Format;

PROCEDURE Rand(r : Random.T) : T = 
  BEGIN RETURN r.extended() END Rand;

BEGIN END Matrix2XType.
