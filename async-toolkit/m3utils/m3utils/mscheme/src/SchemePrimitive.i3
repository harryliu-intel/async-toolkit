(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemePrimitive;
IMPORT SchemeEnvironment, SchemeProcedure;

TYPE 
  T <: Public;

  Public = SchemeProcedure.T OBJECT METHODS
    init(id : INTEGER; minArgs, maxArgs : CARDINAL) : T;
  END;

PROCEDURE InstallPrimitives(env : SchemeEnvironment.T) : SchemeEnvironment.T;

CONST Brand = "SchemePrimitive";

END SchemePrimitive.
