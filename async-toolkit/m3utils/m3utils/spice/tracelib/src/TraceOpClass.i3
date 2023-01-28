INTERFACE TraceOpClass;
IMPORT TraceOp;
IMPORT Rd;

REVEAL
  TraceOp.T <: Private;
  TraceOp.Array <: PrivateArray;
  TraceOp.Pickle <: PrivatePickle;

  (* 
     implementers of concrete subtypes of TraceOp.T should
     provide an eval() method as declared below
  *)
  
TYPE
  Private = OBJECT
  METHODS
    eval() RAISES { Rd.EndOfFile, Rd.Failure };
  END;
  
  PrivateArray = TraceOp.PublicArray OBJECT
    result : REF ARRAY OF LONGREAL;
  END;

  PrivatePickle = TraceOp.PublicPickle OBJECT
    result : REFANY;
  END;

END TraceOpClass.
