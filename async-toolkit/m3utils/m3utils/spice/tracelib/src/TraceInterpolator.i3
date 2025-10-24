INTERFACE TraceInterpolator;
IMPORT Trace;
IMPORT Rd;

TYPE
  T <: Public;

  Array = REF ARRAY OF LONGREAL;
  
  Public = OBJECT METHODS
    init(tr : Trace.T; idx : Trace.NodeId; scratch : REF Array := NIL) : T
    RAISES { Rd.EndOfFile, Rd.Failure } ;

    minT() : LONGREAL;
    maxT() : LONGREAL;
    
    eval(t : LONGREAL) : LONGREAL
      RAISES { OutOfBounds };

    integrate(a, b : LONGREAL) : LONGREAL
      RAISES { OutOfBounds };
  END;

EXCEPTION  OutOfBounds ;

CONST Brand = "TraceInterpolator";

END TraceInterpolator.
    
    
