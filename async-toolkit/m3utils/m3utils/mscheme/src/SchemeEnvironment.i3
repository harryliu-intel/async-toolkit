(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeEnvironment;
IMPORT SchemeObject, SchemeSymbol, SchemeEnvironmentSuper;
FROM Scheme IMPORT E;
IMPORT Scheme;

(* XXX
   
   The type structure here should be modified.

   A SchemeEnvironment.T should probably just be a prototype, so that
   it can be subtyped in various interesting ways (say, why not an
   SQL-based version?)

   Then a Common could include various of the methods in this
   interface.

   Finally, concrete implementations Safe (= the current T) and Unsafe
   would be the ones allocated (or else user-implemented versions).

   The methods that need to be handled most carefully are:

   put, get (not revealed here yet, internal to implementation, but
             really should be in a Class interface)

   lookup

*)
   

TYPE
  T <: Public;

  Public = SchemeEnvironmentSuper.T OBJECT METHODS
    init(vars, vals : SchemeObject.T; parent : T; 
         VAR canRecyclePairs : BOOLEAN) : T;

    (* canRecyclePairs is set to FALSE if the pairs sent in via 
       vals are stored in the environment; if it's unchanged,
       that means that the environment did not keep references to
       the pairs (and so they can probably be freed) *)

    initEval(vars : SchemeObject.T; 
             argsToEval : SchemeObject.T;
             evalEnv : T;
             interp : Scheme.T;
             parent : T) : T RAISES { E };

    initEmpty() : T;

    lookup(sym : SchemeSymbol.T) : SchemeObject.T RAISES { E };

    define(var, val : SchemeObject.T) : SchemeObject.T;
    set(var, val : SchemeObject.T) : SchemeObject.T RAISES { E };
    defPrim(nam : TEXT;
            id : INTEGER; 
            definer : REFANY (* SchemePrimitive.Definer *);
            minArgs : CARDINAL; 
            maxArgs : CARDINAL := LAST(CARDINAL)) : T;
    markAsDead(); (* a debugging thing *)
  END;

  Unsafe <: T; (* unsynchronized version *)

CONST Brand = "SchemeEnvironment";

END SchemeEnvironment.
