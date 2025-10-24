(* $Id: FactorialDatum.i3,v 1.3 2006/03/06 02:29:03 mika Exp $ *)

INTERFACE FactorialDatum;

TYPE
  T = RECORD
    vi : REF ARRAY OF CARDINAL;
    r :  REF ARRAY OF LONGREAL;
    src : TEXT
  END;

CONST Brand = "FactorialDatum";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

END FactorialDatum.
