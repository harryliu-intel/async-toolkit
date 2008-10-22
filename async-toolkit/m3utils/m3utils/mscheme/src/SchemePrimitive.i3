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
    init(id : CARDINAL; 
         definer : Definer;  (* used for extension primitives *)
         minArgs, maxArgs : CARDINAL) : T;
  END;

  Definer <: PubDefiner;

  PubDefiner = OBJECT METHODS
    installPrimitives(env : SchemeEnvironment.T) : SchemeEnvironment.T;
  END;

  DefaultDefiner <: Definer;

  ExtDefiner <: PubExtensibleDefiner;

  PubExtensibleDefiner = DefaultDefiner OBJECT METHODS
    init() : ExtDefiner;
    addPrim(name : TEXT; 
            proc : SchemeProcedure.T; 
            minArgs, maxArgs : CARDINAL);
  END;

CONST Brand = "SchemePrimitive";

END SchemePrimitive.
