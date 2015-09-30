MODULE ArithR;
IMPORT ArithRep;
IMPORT Atom;

PROCEDURE NewLiteral(name : Atom.T) : T =
  BEGIN RETURN NEW(ArithRep.RLiteral, name := name) END NewLiteral;

PROCEDURE NewConstant(value : LONGREAL) : T =
  BEGIN RETURN NEW(ArithRep.RConstant, value := value) END NewConstant;

PROCEDURE NewRange(x1, x2 : LONGREAL) : T =
  BEGIN RETURN NEW(ArithRep.RRange, x1 := x1, x2 := x2) END NewRange;

PROCEDURE NewFunc(f : F; of : R; debugName : TEXT) : R =
  BEGIN 
    RETURN NEW(ArithRep.RFunc OBJECT name : TEXT END, 
               name := debugName,
               f := f, of := of, n := NFN) 
  END NewFunc;

PROCEDURE NFN(self : ArithRep.RFunc OBJECT name : TEXT END) : TEXT =
  BEGIN RETURN self.name END NFN;

VAR theEpoch := NewLiteral(Atom.FromText("TheEpoch"));

PROCEDURE TheEpoch() : R = BEGIN RETURN theEpoch END TheEpoch;

BEGIN END ArithR.
