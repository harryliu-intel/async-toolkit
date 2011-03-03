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
IMPORT SchemeSymbol;

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
  BEGIN 
    IF interp = NIL THEN
      RAISE E ("Internal error: SchemeClosure.Apply: interp NIL")
    END;
    RETURN interp.eval(t.body, 
                       (* dont think this one can be shared, can it? *)
                       NEW(SchemeEnvironment.Unsafe).init(t.params, args, t.env,
                                                     dummy))
  END Apply;

PROCEDURE Init(t : T; 
               params, body : Object;
               env : SchemeEnvironment.Instance;
               isSpecialForm : IsSpecialFormProc) : T =
  BEGIN
    env.assigned := TRUE;
    t.params := params;
    t.env := env;
    IF body # NIL AND ISTYPE(body, Pair) AND Rest(body) = NIL THEN
      WITH first = First(body) DO

        (* we try some ad-hoc stuff first to see if it works,
           and do all cases later *)
        (* one of the tricky things here is that the parameters themselves
           are not bound at this point of the computation.
           
           what we should do is return some structure that can be used
           to bind the parameters quickly in Scheme.m3 later on.
           
           the names are in any case immaterial... 
        *)
        IF isSpecialForm # NIL THEN
          TRY
            TYPECASE first OF
              NULL => t.body := NIL; RETURN t
            |
              SchemeSymbol.T => t.body := env.bind(first); RETURN t
            |
              Pair(p) => 
              
              WITH ff = p.first DO
                (* the "command" *)
                IF ff # NIL                   AND 
                  ISTYPE(ff, SchemeSymbol.T) AND 
                  NOT isSpecialForm(ff)            THEN

                    t.body := NEW(Pair, first := env.bind(ff), rest := p.rest);
                    RETURN t

                END
              END
            ELSE (* skip *)
            END
          EXCEPT
            E => (* skip *)
          END
        END;
        t.body := first; RETURN t
      END
    ELSE
      t.body := Cons(Symbol("begin"), body); RETURN t
    END
  END Init;

BEGIN END SchemeClosure.
