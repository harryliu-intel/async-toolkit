(* $Id: BDDPair.i3,v 1.2 2000/11/21 05:47:46 mika Exp $ *)
INTERFACE BDDPair;
IMPORT BDD, Word;

TYPE
  T = ARRAY [0..1] OF BDD.T;

CONST
  Brand = "Pair of " & BDD.Brand;

PROCEDURE Equal( a, b : T ) : BOOLEAN;

PROCEDURE Hash( x : T ) : Word.T;

END BDDPair.
