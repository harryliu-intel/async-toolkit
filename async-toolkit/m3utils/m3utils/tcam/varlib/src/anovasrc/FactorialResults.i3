(* $Id: FactorialResults.i3,v 1.2 2005/03/24 11:08:35 mika Exp $ *)

INTERFACE FactorialResults;
IMPORT Rd, OSError;
IMPORT FactorialSuper;

TYPE
  T <: Public;
  
  Public = FactorialSuper.T OBJECT METHODS
    init(directory, name : TEXT) : T RAISES { OSError.E, Rd.Failure };

    aggregateResponse(interaction : REF ARRAY OF BOOLEAN;
                      order :       REF ARRAY OF CARDINAL;
                      VAR resp : LONGREAL;
                      fixedValues : REF ARRAY OF [-1..LAST(INTEGER)] := NIL);
    (* calculate aggregate response for a certain interaction:

       interaction[i] is TRUE for every variable included.

       order is the order of the interaction (linear, quadratic, etc.) per var.

       fixedValues are held fixed if different from -1; it is an error for
       fixedValues[i] # -1 AND interaction[i] to be TRUE
    *)
    
  END;

CONST Brand = "FactorialResults";

END FactorialResults.
  

