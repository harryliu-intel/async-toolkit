(* $Id: SchemeEnvironmentBinding.i3,v 1.2 2011/03/03 20:42:47 mika Exp $ *)

INTERFACE SchemeEnvironmentBinding;
IMPORT SchemeSymbol, SchemeObject;

TYPE
  T = OBJECT METHODS
    name()  : SchemeSymbol.T; (* debug? *)
    env()   : SchemeObject.T; (* actually an environment *)
    get()   : SchemeObject.T;
    setB(to : SchemeObject.T);
  END;

CONST Brand = "SchemeEnvironmentBinding"; 

END SchemeEnvironmentBinding.
