(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeProcedure;
IMPORT Scheme; 
FROM Scheme IMPORT Object, E;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    format() : TEXT;

    apply(interp : Scheme.T; args : Object) : Object RAISES { E }; 
    (* abstract *)
  END;

CONST Brand = "SchemeProcedure";

PROCEDURE Proc(x : Object) : T RAISES { E }; 

END SchemeProcedure.
    
    
