(* $Id$ *)

GENERIC INTERFACE SXFuncOps(Arg, Result);

TYPE F1 = PROCEDURE(a : Arg.Base) : Result.Base;
PROCEDURE UnaryFunc(a : Arg.T; f : F1) : Result.T;

TYPE F2 = PROCEDURE(a, b : Arg.Base) : Result.Base;
PROCEDURE BinaryFunc(a, b : Arg.T; f : F2) : Result.T;

END SXFuncOps.
