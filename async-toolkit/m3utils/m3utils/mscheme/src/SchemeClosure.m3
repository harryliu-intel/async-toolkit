(* $Id$ *)

MODULE SchemeClosure;
IMPORT SchemeClosureClass;
IMPORT SchemeEnvironment;
FROM Scheme IMPORT Object;
FROM SchemeUtils IMPORT Cons, First, Rest;
FROM SchemeSymbol IMPORT Symbol;
IMPORT SchemePair;

TYPE Pair = SchemePair.T;

REVEAL
  T = SchemeClosureClass.Private BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t : T; 
               params, body : Object;
               env : SchemeEnvironment.T) : T =
  BEGIN
    t.params := params;
    t.env := env;
    IF body # NIL AND ISTYPE(body, Pair) AND Rest(body) # NIL THEN
      t.body := First(body)
    ELSE
      t.body := Cons(Symbol("begin"), body)
    END;
    RETURN t
  END Init;

BEGIN END SchemeClosure.
