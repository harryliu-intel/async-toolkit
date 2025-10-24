INTERFACE TransitionPickler;
IMPORT TraceOp;
IMPORT TransitionSeq;

TYPE
  T <: Public;

  Public = TraceOp.Pickle OBJECT
    idx                : CARDINAL;
    time, of           : TraceOp.Array;
    thresh, hysteresis : LONGREAL;
  END;

  (* will leave a result of type TransitionSeq.T in T.result *)
  Result = TransitionSeq.T;
  
CONST Brand = "TransitionPickler";

END TransitionPickler.
