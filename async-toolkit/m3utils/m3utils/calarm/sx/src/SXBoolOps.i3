(* $Id: SXBoolOps.i3,v 1.2 2007/12/20 13:50:31 mika Exp $ *)

INTERFACE SXBoolOps;
IMPORT SXBool AS Bool;

PROCEDURE Not(a : Bool.T) : Bool.T;

PROCEDURE Equal(a, b : Bool.T) : Bool.T;
PROCEDURE And  (a, b : Bool.T; shortCircuit := TRUE) : Bool.T;
PROCEDURE Or   (a, b : Bool.T; shortCircuit := TRUE) : Bool.T;
PROCEDURE Xor  (a, b : Bool.T) : Bool.T;

END SXBoolOps.
