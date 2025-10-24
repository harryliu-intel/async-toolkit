INTERFACE ArithR;
IMPORT Arith, Atom;

TYPE  
  T = Arith.T BRANDED OBJECT
    provedProperties : T := NIL;
  END;

  R = T;

PROCEDURE NewLiteral(name : Atom.T) : R;        (* an unknown *)

PROCEDURE NewConstant(value : LONGREAL) : R;  (* a known value *)

PROCEDURE NewRange(x1, x2 : LONGREAL) : R;    (* a range, xs not ordered *)

TYPE LRPair = RECORD x1, x2 : LONGREAL END;

TYPE 
  F = OBJECT METHODS 
    eval (x : LONGREAL)    : R; (* returns a range *)
    evalDmin(x : LONGREAL) : R; (* a Constant, deriv of min of range *)
    evalDmax(x : LONGREAL) : R; (* a Constant, deriv of max of range *)
  END;

PROCEDURE NewFunc(f : F; of : R; debugName : TEXT) : R;

PROCEDURE TheEpoch() : R; (* the long-ago past *)

END ArithR.
