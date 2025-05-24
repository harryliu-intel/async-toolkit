INTERFACE CspPortObject;

(* 
   generic "thing" that is a port.
   
   can be a CspChannel or a CspNode 
*)

TYPE
  T = OBJECT
    nm : TEXT;
  END;

CONST Brand = "CspPortObject";

END CspPortObject.
