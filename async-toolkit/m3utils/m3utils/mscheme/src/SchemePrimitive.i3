(* $Id$ *)

INTERFACE SchemePrimitive;
IMPORT SchemeEnvironment;

TYPE 
  T <: Public;

  Public = SchemeProcedure.T OBJECT
    init(id : INTEGER; minArgs, maxArgs : CARDINAL) : T;
  END;

PROCEDURE InstallPrimitives(env : SchemeEnvironment.T) : SchemeEnvironment.T;

CONST Brand = "SchemePrimitive";

END SchemePrimitive.
