(* $Id$ *)

INTERFACE SchemeEnvironmentBinding;
IMPORT SchemeSymbol, SchemeObject;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    name()  : SchemeSymbol.T; (* debug? *)
    env()   : SchemeObject.T; (* actually an environment *)
    get()   : SchemeObject.T;
    setB(to : SchemeObject.T);
  END;

CONST Brand = "SchemeEnvironmentBinding"; 

END SchemeEnvironmentBinding.
