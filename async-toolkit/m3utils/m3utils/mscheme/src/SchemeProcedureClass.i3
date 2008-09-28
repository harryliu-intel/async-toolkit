(* $Id$ *)

INTERFACE SchemeProcedureClass;
IMPORT SchemeProcedure;

REVEAL SchemeProcedure.T <: Private;

TYPE
  Private = SchemeProcedure.Public OBJECT
    name := DefaultName
  END;

CONST DefaultName = "anonymous procedure";

END SchemeProcedureClass.
