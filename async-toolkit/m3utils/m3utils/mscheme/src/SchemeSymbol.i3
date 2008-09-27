(* $Id$ *)

INTERFACE SchemeSymbol;
IMPORT Atom;

TYPE T = Atom.T;

PROCEDURE SymEq(a : T; b : TEXT) : BOOLEAN;
  (* check if a symbol is equal to a TEXT *)

CONST Brand = "SchemeSymbol";

END SchemeSymbol.
