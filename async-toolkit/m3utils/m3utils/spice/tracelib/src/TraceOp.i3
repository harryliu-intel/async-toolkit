INTERFACE TraceOp;

(* abstract type that describes an operation on a Trace that leads to a 
   new waveform *)

IMPORT Trace;
IMPORT Rd;
IMPORT LRFunction;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    exec(trace : Trace.T; VAR result : ARRAY OF LONGREAL)
      RAISES { Rd.EndOfFile, Rd.Failure } ;
  END;

  NodeId = Trace.NodeId;

  GetNode <: T OBJECT
    nodeid : NodeId;
  END;

  Unary <: T OBJECT
    a : T;
  END;

  Func <: Unary OBJECT
    f : PROCEDURE(x : LONGREAL) : LONGREAL;
  END;

  LrFunc <: Unary OBJECT
    f : LRFunction.T;
  END;

  Integrate <: Unary;

  Binary <: T OBJECT
    a, b : T;
  END;

  Plus <: Binary;

  Times <: Binary;

  Divide <: Binary;

  Scalar = Unary OBJECT
    scalar : LONGREAL;
  END;

  Scale <: Scalar;
  
CONST Brand = "TraceOp";

(* utility procedures *)

PROCEDURE MakeGetNode(nodeid : NodeId) : T;

PROCEDURE MakeFunc(f : LRFunction.T) : T;

PROCEDURE MakePlus(a, b : T) : T;

PROCEDURE MakeTimes(a, b : T) : T;

PROCEDURE MakeDivide(a, b : T) : T;

PROCEDURE MakeScale(a : T; scalar : LONGREAL) : T;

END TraceOp.
