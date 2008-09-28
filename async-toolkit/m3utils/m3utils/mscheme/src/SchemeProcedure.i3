(* $Id$ *)

INTERFACE SchemeProcedure;
IMPORT Scheme; 
FROM Scheme IMPORT Object;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    format() : TEXT;
    apply(interp : Scheme.T; args : Object) : Object; (* abstract *)
  END;

CONST Brand = "SchemeProcedure";

PROCEDURE Proc(x : Object) : T; 

END SchemeProcedure.
    
    
