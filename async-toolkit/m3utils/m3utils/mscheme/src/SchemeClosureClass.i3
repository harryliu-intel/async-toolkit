(* $Id$ *)

INTERFACE SchemeClosureClass;
IMPORT SchemeClosure;
IMPORT SchemeObject, SchemeEnvironment;

REVEAL
  SchemeClosure.T <: Private;

TYPE
  Private = SchemeClosure.Public OBJECT
    params, body : SchemeObject.T;
    env : SchemeEnvironment.T;
  END;

END SchemeClosureClass.
