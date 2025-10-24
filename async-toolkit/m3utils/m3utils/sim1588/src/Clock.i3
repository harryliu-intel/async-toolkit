INTERFACE Clock;

IMPORT ClockSpec;
IMPORT Random;
IMPORT LongRealSeq;
IMPORT Tyme;

TYPE
  T <: Public;

  Public = OBJECT
    m    : Callback;
    seq  : LongRealSeq.T;
  METHODS
    init(spec : ClockSpec.T; nm : TEXT; jitter : Tyme.T) : T;
    nextEdge() : Tyme.T;  (* call to get next edge *)
    tick(tm : Tyme.T);    (* call when next edge arrives *)
    getNm() : TEXT;
  END;

  Callback <: PublicCallback;

  PublicCallback = OBJECT 
    clock : T;
  METHODS
    do(tm : Tyme.T);
  END;


CONST Brand = "Clock";

END Clock.
