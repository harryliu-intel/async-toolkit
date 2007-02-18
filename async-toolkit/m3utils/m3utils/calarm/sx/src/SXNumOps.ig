(* $Id$ *)

GENERIC INTERFACE SXNumOps(Elem);
IMPORT SXBool, SXInt;

PROCEDURE Times(a, b : Elem.T) : Elem.T;
PROCEDURE Plus(a, b : Elem.T) : Elem.T;
PROCEDURE Minus(a, b : Elem.T) : Elem.T;
PROCEDURE Div(a, b : Elem.T) : Elem.T;
PROCEDURE Mod(a, b : Elem.T) : Elem.T;

PROCEDURE Min(a, b : Elem.T) : Elem.T;
PROCEDURE Max(a, b : Elem.T) : Elem.T;

PROCEDURE Equal(a, b : Elem.T) : SXBool.T;
PROCEDURE Compare(a, b : Elem.T) : SXInt.T;

PROCEDURE GT(a, b : Elem.T) : SXBool.T;
PROCEDURE LT(a, b : Elem.T) : SXBool.T;
PROCEDURE GE(a, b : Elem.T) : SXBool.T;
PROCEDURE LE(a, b : Elem.T) : SXBool.T;

END SXNumOps.
