(* $Id$ *)

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

REVEAL
  T = Public BRANDED Brand OBJECT
    (* vars, vals not necessary *)
    dictionary : AtomRefTbl.T;
    parent : T := NIL;
  METHODS
    initDict(vars, vals : Object) : BOOLEAN := InitDict;
  OVERRIDES
    initEmpty :=  InitEmpty;
    init      :=  Init;
    lookup    :=  Lookup;
    define    :=  Define;
    set       :=  Set;
    defPrim   :=  DefPrim;
  END;

PROCEDURE InitEmpty(t : T) : T =
  BEGIN t.dictionary := NEW(AtomRefTbl.Default).init(); RETURN t END InitEmpty;

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
      EVAL t.dictionary.put(vars, vals);
      RETURN TRUE
    ELSIF vars # NIL AND vals # NIL AND 
          ISTYPE(vars, Pair) AND ISTYPE(vals, Pair) THEN
      WITH varp = NARROW(vars,Pair), valp = NARROW(vals,Pair) DO
        IF varp.first # NIL AND ISTYPE(varp.first,Symbol) THEN
          EVAL t.dictionary.put(varp.first, valp.first)
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
    IF t.dictionary.get(symbol,o) THEN 
      RETURN o
    END;

    IF t.parent # NIL THEN RETURN t.parent.lookup(symbol) 
    ELSE RETURN Error("Unbound variable: " & SchemeSymbol.ToText(symbol)) 
    END
  END Lookup;

PROCEDURE Define(t : T; var, val : Object) : Object =
  BEGIN
    IF var = NIL OR NOT ISTYPE(var, Symbol) THEN RETURN var END;

    EVAL t.dictionary.put(var,val);

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
    IF var = NIL OR NOT ISTYPE(var, Symbol) THEN
      RETURN Error("Attempt to set a non-symbol: " &
             SchemeUtils.Stringify(var))
    END;

    IF t.dictionary.get(var, dummy) THEN
      EVAL t.dictionary.put(var, val);
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
    EVAL t.define(SchemeSymbol.Symbol(name), 
                  NEW(SchemePrimitive.T).init(id, minArgs, maxArgs));
    RETURN t
  END DefPrim;
    
BEGIN END SchemeEnvironment.
