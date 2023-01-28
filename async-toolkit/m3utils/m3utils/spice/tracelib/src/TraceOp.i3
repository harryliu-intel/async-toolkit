INTERFACE TraceOp;

(* 
   Abstract type that describes an operation on a Trace that leads to a 
   new waveform 

   added also a "Pickle" format that allows an operation on traces to have
   an arbitrary data type.

   The point of this interface, broadly, is to capture a deferred operation
   (a.k.a. promise) on a trace file, and represent this in a way suitable
   for serialization over a network, also representing the results of said
   operation in a way suitable for serialization.  Raw binary is used for
   arrays of timestep data, and the Pickle interface is used for everything
   else.

   Note that since much of this interface talks about operations on waveforms,
   that is mostly separate from the Pickles.

   Waveform ops are presented here so that multiple ops can be performed 
   without storing the intermediate results in the trace file.

   mika.nystroem@intel.com
   January, 2023
*)

IMPORT Trace;
IMPORT Rd;
IMPORT LRFunction;

(*
  Note to implementers of subtypes of this interface:

  What you want to implement is the method eval() that's declared in
  the TraceOpClass interface.

  exec() below is provided by the default implementation and normally
  does not need to be overridden.
*)

TYPE
  T <: ROOT;

  Array <: PublicArray;

  Pickle <: PublicPickle;
  
  PublicArray = T OBJECT METHODS
    exec(trace : Trace.T; VAR result : ARRAY OF LONGREAL)
      RAISES { Rd.EndOfFile, Rd.Failure } ;
  END;

  PublicPickle = T OBJECT METHODS
    exec(trace : Trace.T; VAR result : REFANY)
      RAISES { Rd.EndOfFile, Rd.Failure };
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
