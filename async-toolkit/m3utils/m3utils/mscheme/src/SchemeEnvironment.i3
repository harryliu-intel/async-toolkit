(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeEnvironment;
IMPORT SchemeObject, SchemeSymbol, SchemeEnvironmentSuper;
FROM Scheme IMPORT E;

TYPE
  T <: Public;

  Public = SchemeEnvironmentSuper.T OBJECT METHODS
    init(vars, vals : SchemeObject.T; parent : T) : T;
    initEmpty() : T;
    lookup(sym : SchemeSymbol.T) : SchemeObject.T RAISES { E };
    define(var, val : SchemeObject.T) : SchemeObject.T;
    set(var, val : SchemeObject.T) : SchemeObject.T RAISES { E };
    defPrim(nam : TEXT;
            id : INTEGER; 
            minArgs : CARDINAL; 
            maxArgs : CARDINAL := LAST(CARDINAL)) : T;
    markAsDead(); (* a debugging thing *)
  END;

CONST Brand = "SchemeEnvironment";

END SchemeEnvironment.
