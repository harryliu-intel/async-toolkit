(* $Id$ *)

INTERFACE SchemeClosure;
IMPORT SchemeProcedure, SchemeEnvironment;

TYPE
  T <: Public;

  Public = SchemeProcedure.T OBJECT
    init(parms, body : Object; env : SchemeEnvironment.T) : T;
  END;

CONST Brand = "SchemeClosure";

END SchemeClosure.
