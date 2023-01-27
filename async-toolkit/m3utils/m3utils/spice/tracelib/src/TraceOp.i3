INTERFACE TraceOp;

(* abstract type that describes an operation on a Trace that leads to a 
   new waveform *)

IMPORT Trace;
IMPORT Rd;
IMPORT LRFunction;
IMPORT Wr;

TYPE
  T <: ROOT;

  Array <: PublicArray;

  Pickle <: PublicPickle;
  
  PublicArray = T OBJECT METHODS
    exec(trace : Trace.T; VAR result : ARRAY OF LONGREAL)
      RAISES { Rd.EndOfFile, Rd.Failure } ;
  END;

  PublicPickle = T OBJECT METHODS
    exec(trace : Trace.T; target : Wr.T)
      RAISES { Rd.EndOfFile, Rd.Failure, Wr.Failure };
  END;

  NodeId = Trace.NodeId;

  GetNode <: Array OBJECT
    nodeid : NodeId;
  END;

  Unary <: Array OBJECT
    a : Array;
  END;

  Func <: Unary OBJECT
    f : PROCEDURE(x : LONGREAL) : LONGREAL;
  END;

  LrFunc <: Unary OBJECT
    f : LRFunction.T;
  END;

  Integrate <: Unary;

  Binary <: Array OBJECT
    a, b : Array;
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

PROCEDURE MakeGetNode(nodeid : NodeId) : GetNode;

PROCEDURE MakeFunc(f : LRFunction.T) : LrFunc;

PROCEDURE MakePlus(a, b : Array) : Plus;

PROCEDURE MakeTimes(a, b : Array) : Times;

PROCEDURE MakeDivide(a, b : Array) : Divide;

PROCEDURE MakeScale(a : Array; scalar : LONGREAL) : Scale;

END TraceOp.
