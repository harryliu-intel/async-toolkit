(* $Id$ *)

GENERIC INTERFACE SXConvertOps(Elem);
IMPORT SXInt;

PROCEDURE Float(a : SXInt.T) : Elem.T;

PROCEDURE Round(a : Elem.T) : SXInt.T;
PROCEDURE Trunc(a : Elem.T) : SXInt.T;
PROCEDURE Floor(a : Elem.T) : SXInt.T;
PROCEDURE Ceiling(a : Elem.T) : SXInt.T;

END SXConvertOps.
