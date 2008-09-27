(* $Id$ *)

INTERFACE SchemeClosure;
IMPORT SchemeProcedure, SchemeEnvironment;
IMPORT SchemeObject;

TYPE
  T <: Public;

  Public = SchemeProcedure.T OBJECT METHODS
    init(parms, body : SchemeObject.T; env : SchemeEnvironment.T) : T;
  END;

CONST Brand = "SchemeClosure";

END SchemeClosure.
