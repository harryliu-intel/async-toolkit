INTERFACE TraceOp;

(* abstract type that describes an operation on a Trace that leads to a 
   new waveform *)

IMPORT Trace;
IMPORT Rd;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    exec(trace : Trace.T; VAR result : ARRAY OF LONGREAL)
      RAISES { Rd.EndOfFile, Rd.Failure } ;
  END;

  NodeId = Trace.NodeId;

  Unary = T OBJECT
    a : NodeId;
  END;

  Func <: Unary OBJECT
    f : PROCEDURE(x : LONGREAL) : LONGREAL;
  END;

  Binary = T OBJECT
    a, b : NodeId;
  END;

  Plus <: Binary;

  Times <: Binary;

  Divide <: Binary;

  Scalar = T OBJECT
    a      : NodeId;
    scalar : LONGREAL;
  END;

  Scale <: Scalar;
  
CONST Brand = "TraceOp";

END TraceOp.
