(* $Id$ *)

MODULE SchemeEnvironmentUnsafe EXPORTS SchemeEnvironment;

IMPORT SchemeEnvironmentInstanceRep;
IMPORT AtomRefTbl;
FROM Scheme IMPORT Object, Symbol;
IMPORT AtomList;
FROM SchemeEnvironmentInstanceRep IMPORT QuickMap;
FROM SchemeUtils IMPORT Warn, StringifyT;

REVEAL
  Unsafe = Instance BRANDED Brand & " Unsafe" OBJECT OVERRIDES
    init          := Init;
    initEmpty     := InitEmptyUnsafe;
    put           := UnsafePut;
    get           := UnsafeGet;
    getLocalNames := GetLocalNames;
  END;

PROCEDURE InitEmptyUnsafe(t : Unsafe; parent : T) : Instance =
  BEGIN 
    t.dictionary := NIL;
    t.parent := parent;
    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      t.quick[i] := QuickMap { NIL, NIL };
    END;

    RETURN t 
  END InitEmptyUnsafe;

PROCEDURE Init(t                   : Instance;
               vars, vals          : Object; 
               parent              : T;
               VAR canRecyclePairs : BOOLEAN) : Instance =
  BEGIN
    EVAL t.initEmpty(parent);
    IF NOT t.initDict(vars,vals,canRecyclePairs) THEN
      TRY
        EVAL Warn("wrong number of arguments: expected " &
          StringifyT(vars) & " got " & StringifyT(vals))
      EXCEPT
      ELSE
      END
    END;
    RETURN t
  END Init;

PROCEDURE GetLocalNames(e : Instance) : AtomList.T =
  VAR res : AtomList.T := NIL;
  BEGIN
    IF e.dictionary # NIL THEN
      WITH iter = e.dictionary.iterate() DO
        VAR a : Symbol; o : REFANY; BEGIN
          WHILE iter.next(a,o) DO
            res := AtomList.Cons(a,res)
          END
        END
      END
    ELSE
      FOR i := FIRST(e.quick) TO LAST(e.quick) DO
        IF e.quick[i].var # NIL THEN 
          res := AtomList.Cons(e.quick[i].var,res)
        ELSE
          EXIT
        END
      END
    END;
    RETURN res
  END GetLocalNames;
  
PROCEDURE UnsafeGet(t : Instance; var : Symbol; VAR val : Object) : BOOLEAN =
  BEGIN 
    IF var = NIL THEN RETURN FALSE END;

    IF t.dictionary # NIL THEN
      RETURN t.dictionary.get(var,val) 
    ELSE
      FOR i := FIRST(t.quick) TO LAST(t.quick) DO
        IF t.quick[i].var = var THEN val := t.quick[i].val; RETURN TRUE END
      END;
      RETURN FALSE
    END
  END UnsafeGet;

PROCEDURE UnsafePut(t : Instance; var : Symbol; READONLY val : Object) =
  BEGIN 
    IF t.dictionary # NIL THEN
      EVAL t.dictionary.put(var,val) 
    ELSE
      FOR i := FIRST(t.quick) TO LAST(t.quick) DO
        IF t.quick[i].var = var OR t.quick[i].var = NIL THEN 
          t.quick[i].var := var; 
          t.quick[i].val := val;
          RETURN
        END
      END;
      (* failed *)
      t.dictionary := NEW(AtomRefTbl.Default).init();
      FOR i := LAST(t.quick) TO FIRST(t.quick) BY -1 DO
        UnsafePut(t,t.quick[i].var, t.quick[i].val)
      END;

      UnsafePut(t, var, val)
    END
  END UnsafePut;

BEGIN END SchemeEnvironmentUnsafe.
