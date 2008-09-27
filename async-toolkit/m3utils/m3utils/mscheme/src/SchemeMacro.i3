(* $Id$ *)

INTERFACE SchemeMacro;
IMPORT SchemeClosure, Scheme;
FROM Scheme IMPORT Pair, Object;

TYPE
  T <: Public;

  Public = SchemeClosure.T OBJECT METHODS
    expand(interpreter : Scheme.T; oldPair : Pair; args : Object) : Pair;
  END;

PROCEDURE MacroExpand(interpreter : Scheme.T; x : Object) : Object;

CONST Brand = "SchemeMacro";

END SchemeMacro.
