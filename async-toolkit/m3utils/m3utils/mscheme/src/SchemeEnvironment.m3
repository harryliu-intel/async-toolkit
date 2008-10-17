(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeEnvironment;

(* One of the few things not taken directly from Norvig... *)

IMPORT AtomRefTbl;
IMPORT SchemePrimitive, SchemeSymbol, SchemeProcedure;
IMPORT SchemeProcedureClass;
FROM Scheme IMPORT Symbol, Object, E;
FROM SchemeUtils IMPORT Error, Warn, StringifyT;
IMPORT SchemeUtils;
IMPORT Text;
IMPORT SchemePair;
IMPORT Scheme;

TYPE Pair = SchemePair.T;

CONST TE = Text.Equal;

CONST QuickVars = 5;

TYPE QuickMap = RECORD var : Symbol; val : Object END;

REVEAL
  T = Public BRANDED Brand OBJECT
    mu : MUTEX := NIL; (* always NIL in Unsafe *)
    (* vars, vals not necessary *)
    dictionary : AtomRefTbl.T;

    quick : ARRAY [0..QuickVars - 1] OF QuickMap;

    parent : T;

    dead := FALSE; (* for debugging *)
  METHODS
    initDict(vars, vals : Object) : BOOLEAN := InitDict;
    initDictEval(vars, argsToEval : Object;
                 evalEnv : T;
                 interp : Scheme.T) : BOOLEAN RAISES { E } := InitDictEval2;
    
    put(var : Symbol; READONLY val : Object) := SafePut;
    get(var : Symbol; VAR val : Object) : BOOLEAN := SafeGet;
  OVERRIDES
    initEmpty :=  InitEmpty;
    init      :=  Init;
    initEval  :=  InitEval;
    lookup    :=  Lookup;
    define    :=  Define;
    set       :=  Set;
    defPrim   :=  DefPrim;
    markAsDead:=  MarkAsDead;
  END;

  Unsafe = T BRANDED Brand & " Unsafe" OBJECT OVERRIDES
    initEmpty := InitEmptyUnsafe;
    put := UnsafePut;
    get := UnsafeGet;
  END;

PROCEDURE UnsafeGet(t : T; var : Symbol; VAR val : Object) : BOOLEAN =
  BEGIN 
    IF t.dictionary # NIL THEN
      RETURN t.dictionary.get(var,val) 
    ELSE
      FOR i := FIRST(t.quick) TO LAST(t.quick) DO
        IF t.quick[i].var = var THEN val := t.quick[i].val; RETURN TRUE END
      END;
      RETURN FALSE
    END
  END UnsafeGet;

PROCEDURE UnsafePut(t : T; var : Symbol; READONLY val : Object) =
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

PROCEDURE SafeGet(t : T; var : Symbol; VAR val : Object) : BOOLEAN =
  BEGIN 
    LOCK t.mu DO RETURN UnsafeGet(t,var,val) END
  END SafeGet;

PROCEDURE SafePut(t : T; var : Symbol; READONLY val : Object) =
  BEGIN
    LOCK t.mu DO UnsafePut(t,var,val) END
  END SafePut;

(**********************************************************************)

PROCEDURE MarkAsDead(t : T) = BEGIN t.dead := TRUE END MarkAsDead;

PROCEDURE InitEmptyUnsafe(t : T) : T =
  BEGIN 
    t.mu := NIL;
    (* why lock it? well if it's a safe version, it might still
       be accessed from other threads *)
    t.dictionary := NIL;
    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      t.quick[i] := QuickMap { NIL, NIL };
    END;

    RETURN t 
  END InitEmptyUnsafe;

PROCEDURE InitEmpty(t : T) : T =
  BEGIN 
    IF t.mu = NIL THEN t.mu := NEW(MUTEX) END;

    (* why lock it? well if it's a safe version, it might still
       be accessed from other threads *)
    LOCK t.mu DO
      t.dictionary := NIL;
      FOR i := FIRST(t.quick) TO LAST(t.quick) DO
        t.quick[i] := QuickMap { NIL, NIL };
      END
    END;

    RETURN t 
  END InitEmpty;

PROCEDURE Init(t : T; vars, vals : Object; parent : T) : T =
  BEGIN
    EVAL t.initEmpty();
    t.parent := parent;
    IF NOT t.initDict(vars,vals) THEN
      TRY
        EVAL Warn("wrong number of arguments: expected " &
          StringifyT(vars) & " got " & StringifyT(vals))
      EXCEPT
      ELSE
      END
    END;
    RETURN t
  END Init;

PROCEDURE InitEval(t : T; vars, argsToEval : Object;
                   evalEnv : T; 
                   interp : Scheme.T;
                   parent : T) : T RAISES { E } =
  BEGIN
    EVAL t.initEmpty();
    t.parent := parent;
    IF NOT t.initDictEval(vars, argsToEval, evalEnv, interp) THEN
      TRY
        EVAL Warn("wrong number of arguments: expected " &
          StringifyT(vars) & " got " & StringifyT(interp.evalList(argsToEval,evalEnv)))
      EXCEPT
      ELSE
      END
    END;
    RETURN t
  END InitEval;

PROCEDURE InitDict(t : T; vars, vals : Object) : BOOLEAN =
  BEGIN
    IF vars = NIL AND vals = NIL THEN 
      RETURN TRUE 
    ELSIF vars # NIL AND ISTYPE(vars, Symbol) THEN
      t.put(vars,vals);
      RETURN TRUE
    ELSIF vars # NIL AND vals # NIL AND 
          ISTYPE(vars, Pair) AND ISTYPE(vals, Pair) THEN
      WITH varp = NARROW(vars,Pair), valp = NARROW(vals,Pair) DO
        IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
          t.put(varp.first, valp.first);
        END;
        RETURN InitDict(t, varp.rest, valp.rest)
      END
    ELSE
      RETURN FALSE
    END
  END InitDict;

PROCEDURE InitDictEval(t : T; 
                       vars, argsToEval : Object; 
                       evalEnv : T;
                       interp : Scheme.T) : BOOLEAN RAISES { E }=
  BEGIN
    IF vars = NIL AND argsToEval = NIL THEN 
      RETURN TRUE 
    ELSIF vars # NIL AND ISTYPE(vars, Symbol) THEN
      t.put(vars,interp.eval(argsToEval,evalEnv));
      RETURN TRUE
    ELSIF vars # NIL AND argsToEval # NIL AND 
          ISTYPE(vars, Pair) AND ISTYPE(argsToEval, Pair) THEN
      WITH varp = NARROW(vars,Pair), argp = NARROW(argsToEval,Pair) DO
        IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
          t.put(varp.first, interp.eval(argp.first,evalEnv));
        END;
        RETURN InitDictEval(t, varp.rest, argp.rest, evalEnv, interp)
      END
    ELSE
      RETURN FALSE
    END
  END InitDictEval;

PROCEDURE InitDictEval2(t : T; 
                        vars, argsToEval : Object; 
                        evalEnv : T;
                        interp : Scheme.T) : BOOLEAN RAISES { E }=
  BEGIN
    LOOP
      IF vars = NIL AND argsToEval = NIL THEN 
        RETURN TRUE 
      ELSIF vars # NIL AND ISTYPE(vars, Symbol) THEN
        t.put(vars,interp.eval(argsToEval,evalEnv));
        RETURN TRUE
      ELSIF vars # NIL AND argsToEval # NIL AND 
        ISTYPE(vars, Pair) AND ISTYPE(argsToEval, Pair) THEN
        WITH varp = NARROW(vars,Pair), argp = NARROW(argsToEval,Pair) DO
          IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
            t.put(varp.first, interp.eval(argp.first,evalEnv));
          END;
          vars := varp.rest ; argsToEval := argp.rest
        END
      ELSE
        RETURN FALSE
      END
    END
  END InitDictEval2;

PROCEDURE Lookup(t : T; symbol : Symbol) : Object RAISES { E } =
  VAR o : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF t.get(symbol,o) THEN
      RETURN o
    END;

    IF t.parent # NIL THEN 
      RETURN t.parent.lookup(symbol) 
    ELSE 
      RETURN Error("Unbound variable: " & SchemeSymbol.ToText(symbol)) 
    END
  END Lookup;

PROCEDURE Define(t : T; var, val : Object) : Object =
  BEGIN
    <*ASSERT NOT t.dead*>
    IF var = NIL OR NOT ISTYPE(var, Symbol) THEN RETURN var END;

    t.put(var,val);

    TYPECASE val OF
      NULL => (* skip *)
    |
      SchemeProcedure.T(p) => 
      IF TE(p.name, SchemeProcedureClass.DefaultName) THEN
        p.name := SchemeSymbol.ToText(var)
      END
    ELSE (* skip *)
    END;

    RETURN var
  END Define;

PROCEDURE Set(t : T; var, val : Object) : Object RAISES { E } =
  VAR dummy : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF var = NIL OR NOT ISTYPE(var, Symbol) THEN
      RETURN Error("Attempt to set a non-symbol: " &
             SchemeUtils.Stringify(var))
    END;

    IF t.get( var, dummy) THEN
      t.put(var, val);
      RETURN val (* ?? *)
    END;

    IF t.parent # NIL THEN
      RETURN t.parent.set(var, val)
    ELSE
      RETURN Error("Unbound variable: " & SchemeSymbol.ToText(var))
    END
  END Set;

PROCEDURE DefPrim(t : T; 
                  name : TEXT; 
                  id : INTEGER; minArgs, maxArgs : CARDINAL) : T =
  BEGIN
    <*ASSERT NOT t.dead*>
    EVAL t.define(SchemeSymbol.Symbol(name), 
                  NEW(SchemePrimitive.T).init(id, minArgs, maxArgs));
    RETURN t
  END DefPrim;
    
BEGIN END SchemeEnvironment.
