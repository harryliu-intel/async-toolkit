(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
MODULE SchemeClosure;
IMPORT SchemeClosureClass;
IMPORT SchemeEnvironment;
FROM Scheme IMPORT Object, E;
FROM SchemeUtils IMPORT Cons, First, Rest;
FROM SchemeSymbol IMPORT Symbol;
IMPORT SchemePair, Scheme;

TYPE Pair = SchemePair.T;

REVEAL
  T = SchemeClosureClass.Private BRANDED Brand OBJECT
  OVERRIDES
    init  := Init;
    apply := Apply;
  END;

PROCEDURE Apply(t : T; interp : Scheme.T; args : Object) : Object 
  RAISES { E } =
  VAR dummy : BOOLEAN;
  BEGIN RETURN interp.eval(t.body, 
                         NEW(SchemeEnvironment.T).init(t.params, args, t.env,
                                                       dummy))
  END Apply;

PROCEDURE Init(t : T; 
               params, body : Object;
               env : SchemeEnvironment.T) : T =
  BEGIN
    env.assigned := TRUE;
    t.params := params;
    t.env := env;
    IF body # NIL AND ISTYPE(body, Pair) AND Rest(body) = NIL THEN
      t.body := First(body)
    ELSE
      t.body := Cons(Symbol("begin"), body)
    END;
    RETURN t
  END Init;

BEGIN END SchemeClosure.
