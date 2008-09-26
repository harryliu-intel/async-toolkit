(* $Id$ *)

INTERFACE SchemeProcedure;
IMPORT Scheme; 
FROM Scheme IMPORT Object;

TYPE
  T = OBJECT METHODS
    format() : TEXT;
    apply(interp : Scheme.T; args : Object) : Object; (* abstract *)
    proc(x : Object) : T; (* init method *)
  END;

CONST Brand = "SchemeProcedure";

END SchemeProcedure.
    
    
