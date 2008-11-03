(* $Id$ *)

MODULE SXTime;
IMPORT Time, SXLongReal, Thread;
FROM Math IMPORT log, pow;

PROCEDURE log2(x : LONGREAL) : LONGREAL = 
  BEGIN RETURN log(x) / log(2.0d0) END log2;

TYPE
  Closure = Thread.Closure OBJECT
    interval, next : Time.T;
    sx : SXLongReal.Var;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  BEGIN
    LOOP
      WITH now = Time.Now() DO
        IF now < cl.next THEN
          WITH pause = cl.next-now-0.01d0 DO
            IF pause > 0.0d0 THEN
              Thread.Pause(pause)
            END
          END
        ELSE
          (* set the sx *)
          cl.sx.set(now);

          (* and find the next wakeup time *)
          WHILE cl.next < now DO 
            cl.next := cl.next + cl.interval
          END
        END
      END
    END
  END Apply;

PROCEDURE Next(interval, offset : Time.T) : Time.T =
  VAR
    now := Time.Now();
    steps := now/interval;
    frac := steps;
  BEGIN
    IF offset = CurrentOffset THEN
      WITH next0 = Next(interval,0.0d0),
           prev0 = next0 - interval DO
        offset := now - prev0
      END
    END;

    now := now - offset;
    WHILE frac >= 1.0d0 DO
      frac := frac - pow(2.0d0, FLOAT(FLOOR(log2(frac)),LONGREAL))
    END;
    WITH whole = steps - frac,
         next = whole * interval + interval DO
      RETURN next + offset
    END
  END Next;

PROCEDURE New(interval, offset : Time.T) : SXLongReal.T =
  BEGIN
    WITH sx = NEW(SXLongReal.Var).initVal(Time.Now()) DO 
      EVAL Thread.Fork(NEW(Closure, 
                           interval := interval, 
                           next := Next(interval,offset),
                           sx := sx));
      RETURN sx
    END
  END New;

BEGIN END SXTime.
