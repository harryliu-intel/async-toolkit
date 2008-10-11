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
FROM SchemeUtils IMPORT Error, Warn, DebugFormat;
IMPORT SchemeUtils;
IMPORT Text;
IMPORT SchemePair;

TYPE Pair = SchemePair.T;

CONST TE = Text.Equal;

CONST QuickVars = 5;

TYPE QuickMap = RECORD var : Symbol; val : Object END;

REVEAL
  T = Public BRANDED Brand OBJECT
    (* vars, vals not necessary *)
    dictionary : AtomRefTbl.T;

    quick : ARRAY [0..QuickVars - 1] OF QuickMap;

    parent : T;

    dead := FALSE; (* for debugging *)
  METHODS
    initDict(vars, vals : Object) : BOOLEAN := InitDict;
  OVERRIDES
    initEmpty :=  InitEmpty;
    init      :=  Init;
    lookup    :=  Lookup;
    define    :=  Define;
    set       :=  Set;
    defPrim   :=  DefPrim;
    markAsDead:=  MarkAsDead;
  END;

PROCEDURE MarkAsDead(t : T) = BEGIN t.dead := TRUE END MarkAsDead;

PROCEDURE InitEmpty(t : T) : T =
  BEGIN 
    t.dictionary := NIL;
    FOR i := FIRST(t.quick) TO LAST(t.quick) DO
      t.quick[i] := QuickMap { NIL, NIL };
    END;
    RETURN t 
  END InitEmpty;

PROCEDURE Get(t : T; var : Symbol; VAR val : Object) : BOOLEAN =
  BEGIN 
    IF t.dictionary # NIL THEN
      RETURN t.dictionary.get(var,val) 
    ELSE
      FOR i := FIRST(t.quick) TO LAST(t.quick) DO
        IF t.quick[i].var = var THEN val := t.quick[i].val; RETURN TRUE END
      END;
      RETURN FALSE
    END
  END Get;

PROCEDURE Put(t : T; var : Symbol; READONLY val : Object) =
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
        Put(t, t.quick[i].var, t.quick[i].val)
      END;

      Put(t, var, val)
    END
  END Put;

PROCEDURE Init(t : T; vars, vals : Object; parent : T) : T =
  BEGIN
    EVAL t.initEmpty();
    t.parent := parent;
    IF NOT t.initDict(vars,vals) THEN
      EVAL Warn("wrong number of arguments: expected " &
                DebugFormat(vars) & " got " & DebugFormat(vals))
    END;
    RETURN t
  END Init;

PROCEDURE InitDict(t : T; vars, vals : Object) : BOOLEAN =
  BEGIN
    IF vars = NIL AND vals = NIL THEN 
      RETURN TRUE 
    ELSIF vars # NIL AND ISTYPE(vars, Symbol) THEN
      Put(t,vars,vals);
      RETURN TRUE
    ELSIF vars # NIL AND vals # NIL AND 
          ISTYPE(vars, Pair) AND ISTYPE(vals, Pair) THEN
      WITH varp = NARROW(vars,Pair), valp = NARROW(vals,Pair) DO
        IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
          Put(t, varp.first, valp.first);
        END;
        RETURN InitDict(t, varp.rest, valp.rest)
      END
    ELSE
      RETURN FALSE
    END
  END InitDict;

PROCEDURE Lookup(t : T; symbol : Symbol) : Object RAISES { E } =
  VAR o : Object;
  BEGIN
    <*ASSERT NOT t.dead*>
    IF Get(t,symbol,o) THEN
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

    Put(t,var,val);

    TYPECASE val OF
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

    IF Get(t, var, dummy) THEN
      Put(t,var, val);
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
