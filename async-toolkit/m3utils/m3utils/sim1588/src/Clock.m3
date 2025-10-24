MODULE Clock;
IMPORT Random;
IMPORT ClockSpec;
IMPORT Tyme;
IMPORT LongRealSeq;

REVEAL
  T = Public BRANDED Brand OBJECT 
    next : Tyme.T;
    spec : ClockSpec.T;
    rand : Random.T;
    nm   : TEXT;
    drift:= 0.0d0;
    jitter : Tyme.T;
    lastJitter := 0.0d0;
  OVERRIDES
    init := Init;
    nextEdge := NextEdge;
    tick := Tick;
    getNm := GetNm;
  END;

PROCEDURE Tick(cl : T; tm : Tyme.T) =
  BEGIN cl.seq.addhi(tm) END Tick;

PROCEDURE Init(cl : T; spec : ClockSpec.T; nm : TEXT; jitter : Tyme.T) : T =
  BEGIN 
    cl.spec  := spec;
    cl.rand  := NEW(Random.Default).init();
    cl.nm    := nm;
    cl.drift := cl.rand.longreal(-spec.e,+spec.e);
    cl.seq   := NEW(LongRealSeq.T).init();
    cl.next  := cl.rand.longreal(0.0d0, 1.0d0/cl.spec.f*(1.0d0 + cl.spec.e));
    cl.jitter:= jitter;
    RETURN cl 
  END Init;

PROCEDURE NextEdge(cl : T) : Tyme.T =
  VAR 
    cur := cl.next-cl.lastJitter;
    err := cl.rand.longreal(-cl.spec.e-cl.drift,cl.spec.e-cl.drift)+cl.drift;
    step := 1.0d0/(cl.spec.f * (1.0d0+err));
    thisJitter := cl.rand.longreal(-cl.jitter/2.0d0, cl.jitter/2.0d0);
  BEGIN
    cl.next := cur + step + thisJitter;
    cl.lastJitter := thisJitter;
    RETURN cl.next
  END NextEdge;

REVEAL
  Callback = PublicCallback BRANDED Brand & " Callback" OBJECT
  OVERRIDES
    do := DefDo;
  END;

PROCEDURE DefDo(c : Callback; tm : Tyme.T) =
  BEGIN c.clock.tick(tm) END DefDo;

PROCEDURE GetNm(t : T) : TEXT = BEGIN RETURN t.nm END GetNm;

BEGIN END Clock.
