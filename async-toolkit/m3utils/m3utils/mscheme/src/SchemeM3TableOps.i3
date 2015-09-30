(* $Id: SchemeM3TableOps.i3,v 1.1 2008/10/10 06:01:20 mika Exp $ *)

INTERFACE SchemeM3TableOps;
IMPORT Scheme, RT0;

TYPE 
  T <: Public;

  Public = OBJECT METHODS
    apply(args : Scheme.Object) : Scheme.Object RAISES { Scheme.E };
  END;

PROCEDURE DefaultOps() : T;

PROCEDURE Register(tc       :  RT0.Typecode; 
                   opName   :  Scheme.Symbol; 
                   proc     :  Proc);

TYPE Proc = PROCEDURE(obj, arg : Scheme.Object;
                      opName : Scheme.Symbol) : Scheme.Object RAISES { Scheme.E };

CONST Brand = "SchemeM3TableOps";

END SchemeM3TableOps.
