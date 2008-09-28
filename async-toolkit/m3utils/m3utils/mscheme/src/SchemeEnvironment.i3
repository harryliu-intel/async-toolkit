(* $Id$ *)

INTERFACE SchemeEnvironment;
IMPORT SchemeObject, SchemeSymbol;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(vars, vals : SchemeObject.T; parent : T) : T;
    initEmpty() : T;
    lookup(sym : SchemeSymbol.T) : SchemeObject.T;
    define(var, val : SchemeObject.T) : SchemeObject.T;
    set(var, val : SchemeObject.T) : SchemeObject.T;
    defPrim(nam : TEXT;
            id : INTEGER; 
            minArgs : CARDINAL; 
            maxArgs : CARDINAL := LAST(CARDINAL)) : T;
  END;

CONST Brand = "SchemeEnvironment";

END SchemeEnvironment.
