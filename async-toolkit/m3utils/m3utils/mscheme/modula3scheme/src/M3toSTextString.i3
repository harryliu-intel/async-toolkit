(* $Id: M3toSTextString.i3,v 1.1 2008/10/22 05:08:22 mika Exp $ *)

INTERFACE M3toSTextString;
IMPORT SchemeString, Scheme;

(* interface for M3 <--> Scheme converting generics *)

TYPE S = SchemeString.T;
TYPE T = TEXT;

CONST ToScheme = SchemeString.FromText;

PROCEDURE FromScheme(s : S) : T RAISES { Scheme.E };

CONST Brand = "M3toSTextString";

END M3toSTextString.
