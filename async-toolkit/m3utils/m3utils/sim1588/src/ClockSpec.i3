INTERFACE ClockSpec;
IMPORT Tyme;

TYPE
  T = RECORD
    f : Tyme.T;
    e : Tyme.T; (* for now, should be Allan diagram? *)

    (* f1588:fi::d:n *)
    d : CARDINAL; 
    n : CARDINAL;
  END;

CONST Brand = "ClockSpec";

END ClockSpec.
