(* $Id$ *)

MODULE XTime;
IMPORT Time;

PROCEDURE Now() : T =
  BEGIN
    IF active THEN
      RETURN FakeNow()
    ELSE
      RETURN Time.Now()
    END
  END Now;

(**********************************************************************)

PROCEDURE CurrentOffset(now : T) : T =
  (* mu must be LOCKed *)
  BEGIN
    IF    now > breaks[End.End].start THEN
      RETURN breaks[End.End].offset
    ELSE
      WITH astart = breaks[End.Begin].start,
           astop  = breaks[End.End].start,
           ooffset = breaks[End.Begin].offset,
           noffset = breaks[End.End].offset DO
        
        <* ASSERT now >= astart *>
        RETURN (now - astart)/(astop - astart) * (noffset-ooffset) + 
               ooffset
      END
    END
  END CurrentOffset;

PROCEDURE FakeNow() : T =
  BEGIN
    LOCK mu DO
      WITH now = Time.Now() DO
        RETURN now + CurrentOffset(now)
      END
    END
  END FakeNow;

TYPE 
  Break = RECORD
    offset : T;
    start  : T;
  END;

  End = { Begin, End };

VAR
  active := FALSE;
  breaks    : ARRAY End OF Break;
  mu     := NEW(MUTEX);

PROCEDURE SetOffset(to : T) =
  BEGIN
    LOCK mu DO
      breaks[End.End].offset := to;
      breaks[End.End].start  := FIRST(T);
      active := TRUE;
    END
  END SetOffset;

PROCEDURE AdjustOffset(to : T; absRate := 0.1d0; maxDelta := 30.0d0) 
  RAISES { CantAdjust } =
  BEGIN
    LOCK mu DO
      WITH now = Time.Now(),
           curOff = CurrentOffset(now),
           adjInterval = ABS(to-curOff)/absRate DO
        
        IF NOT adjInterval <= maxDelta THEN 
          (* careful about overflow / NaN here! *)
          RAISE CantAdjust
        END;

        breaks[End.Begin] :=  Break { curOff, now };
        breaks[End.End]   :=  Break { to    , now + adjInterval }
      END
    END
  END AdjustOffset;

BEGIN
  Grain := Time.Grain
END XTime.
