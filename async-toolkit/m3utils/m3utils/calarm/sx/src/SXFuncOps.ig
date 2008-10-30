(* $Id$ *)

GENERIC INTERFACE SXFuncOps(Arg, Result);
IMPORT SXInt;

(* everything returned here is really an OpResult *)

TYPE
  ArgBase = Arg.Base;
  ResultBase = Result.Base;
  ArgT = Arg.T;
  ResultT = Result.T;
  
  OpResult = Result.T BRANDED OBJECT
    opName : TEXT; (* for debugging *)
  END;

TYPE F1 = PROCEDURE(a : Arg.Base) : Result.Base;
PROCEDURE UnaryFunc(a : Arg.T; f : F1; opName : TEXT := NIL) : Result.T;

TYPE F2 = PROCEDURE(a, b : Arg.Base) : Result.Base;
PROCEDURE BinaryFunc(a, b : Arg.T; f : F2; opName : TEXT := NIL) : Result.T;
PROCEDURE BinarySymmetricShortCircuitFunc(a, b : Arg.T; 
                                          f : F2; 
                                          ssOp : Arg.Base; ssRes : Result.Base;
                                          opName : TEXT := NIL) : Result.T;

TYPE FN = PROCEDURE(READONLY a : ARRAY OF Arg.Base) : Result.Base;
PROCEDURE NAryFunc(READONLY a : ARRAY OF Arg.T; f : FN; opName : TEXT := NIL) : Result.T;

TYPE FI = PROCEDURE(i : INTEGER;
                    READONLY a : ARRAY OF Arg.Base) : Result.Base;
PROCEDURE IAryFunc(i : SXInt.T;
                   READONLY a : ARRAY OF Arg.T; f : FI; opName : TEXT := NIL) : Result.T;

END SXFuncOps.
