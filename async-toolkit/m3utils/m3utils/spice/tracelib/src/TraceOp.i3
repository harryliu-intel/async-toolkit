INTERFACE TraceOp;

(* abstract type that describes an operation on a Trace that leads to a 
   new waveform *)

TYPE
  T = OBJECT
    init(trace : Trace.T) : T;
    exec(VAR result : ARRAY OF LONGREAL);
  END;

CONST Brand = "TraceOp";

END TraceOp.
