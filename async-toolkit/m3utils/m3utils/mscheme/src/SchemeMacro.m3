(* $Id$ *)

MODULE SchemeMacro;
IMPORT Scheme, SchemeSymbol;
FROM Scheme IMPORT Object, Pair;
FROM SchemeUtils IMPORT Cons, First, Rest;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    expand := Expand;
  END;

PROCEDURE Expand(t : T; 
                 interpreter : Scheme.T; 
                 oldPair : Pair; args : Object) : Pair =
  BEGIN
    WITH expansion = t.apply(interpreter,args) DO
      TYPECASE expansion OF
        Pair(p) =>
        oldPair.first := p.first;
        oldPair.rest := p.rest
      ELSE
        oldPair.first := SchemeSymbol.Symbol("begin");
        oldPair.rest := Cons(expansion,NIL)
      END
    END;
    RETURN oldPair
  END Expand;

PROCEDURE MacroExpand(interpreter : Scheme.T; x : Object) : Object =
  BEGIN
    IF NOT ISTYPE(x,Pair) THEN RETURN x END;

    WITH fn = interpreter.evalInGlobalEnv(First(x)) DO
      IF NOT ISTYPE(fn,T) THEN RETURN x END;
      RETURN NARROW(fn,T).expand(interpreter, x, Rest(x))
    END
  END MacroExpand;
      

BEGIN END SchemeMacro.
