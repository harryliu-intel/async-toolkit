(* $Id: FactorialDatum.m3,v 1.2 2005/04/05 09:09:52 mika Exp $ *)

MODULE FactorialDatum;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a.vi^ = b.vi^ AND a.r^ = b.r^ END Equal;

BEGIN END FactorialDatum.
