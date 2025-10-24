INTERFACE CspPortObject;
IMPORT CspFrame;

(* 
   generic "thing" that is a port.
   
   can be a CspChannel or a CspNode 
*)

TYPE
  T = OBJECT
    nm        : TEXT;
    id        : CARDINAL;
    surrogate         := FALSE;
  METHODS
    makeSurrogate() : T;

    markWriter(frame : CspFrame.T);
    markReader(frame : CspFrame.T);
  END;

CONST Brand = "CspPortObject";

END CspPortObject.
