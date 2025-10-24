(* $Id: Matrix2LRType.m3,v 1.2 2008/02/04 00:10:46 mika Exp $ *)

MODULE Matrix2LRType;
IMPORT Fmt;
IMPORT Random;

PROCEDURE Format(t : T) : TEXT = 
  BEGIN RETURN Fmt.LongReal(t) END Format;

PROCEDURE Rand(r : Random.T) : T = 
  BEGIN RETURN r.longreal() END Rand;

BEGIN END Matrix2LRType.
