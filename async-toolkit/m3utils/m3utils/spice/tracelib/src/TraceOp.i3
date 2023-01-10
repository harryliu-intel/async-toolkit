INTERFACE TraceOp;

(* abstract type that describes an operation on a Trace that leads to a 
   new waveform *)

IMPORT Trace;

TYPE
  T = OBJECT METHODS
    exec(trace : Trace.T; VAR result : ARRAY OF LONGREAL);
  END;

  NodeId = Trace.NodeId;

  BinOp = T OBJECT
    a, b : NodeId;
  END;

  Plus <: BinOp;

  Times <: BinOp;

  ScalarOp = T OBJECT
    a      : NodeId;
    scalar : LONGREAL;
  END;

  Scale <: ScalarOp;
  
CONST Brand = "TraceOp";

END TraceOp.
