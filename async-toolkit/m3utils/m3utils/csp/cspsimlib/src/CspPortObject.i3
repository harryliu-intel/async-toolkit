INTERFACE CspPortObject;

(* 
   generic "thing" that is a port.
   
   can be a CspChannel or a CspNode 
*)

TYPE
  T = OBJECT
    nm        : TEXT;
    surrogate         := FALSE;
  METHODS
    makeSurrogate() : T;
  END;

CONST Brand = "CspPortObject";

END CspPortObject.
