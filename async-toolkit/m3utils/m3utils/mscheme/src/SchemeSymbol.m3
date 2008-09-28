(* $Id$ *)

MODULE SchemeSymbol;
FROM Scheme IMPORT Object;

PROCEDURE SymEq(a : Object; b : TEXT) : BOOLEAN =
  BEGIN RETURN a = Symbol(b) END SymEq;

BEGIN END SchemeSymbol.
