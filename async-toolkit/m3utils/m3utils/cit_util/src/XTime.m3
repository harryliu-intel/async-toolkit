(* $Id$ *)

MODULE XTime;
IMPORT Time;
IMPORT TextReader, Lex, FloatMode, IP, RTParams, Scan;
IMPORT Fmt;
IMPORT Wr, Stdio, Env, Thread;
IMPORT TCP, AL, Process;
(* were not allowed to import Debug, since Debug imports us *)

PROCEDURE Now() : T =
  BEGIN
    <*ASSERT initialized*>
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

(**********************************************************************)

VAR dLevelT := Env.Get("DEBUGLEVEL");
    dLevel := 0;

PROCEDURE DebugOut(txt : TEXT) =
  BEGIN
    IF dLevel >= 10 THEN
      TRY
        Wr.PutText(Stdio.stderr, txt & "\n");
        Wr.Flush(Stdio.stderr)
      EXCEPT
        Thread.Alerted, Wr.Failure => (* just debug out, skip *)
      END
    END
  END DebugOut;

TYPE 
  Closure = Thread.Closure OBJECT
    rd : Rd.T;
    mu : MUTEX;
    c  : Thread.Condition;
    initialized := FALSE;
  OVERRIDES
    apply := ClApply;
  END;

PROCEDURE CLApply(cl : Closure) : REFANY =
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(cl.rd) DO
          (* set the time ! *)
        END;
        cl.initialized := TRUE
      END
    EXCEPT
    END
  END CLApply;

<*FATAL TextReader.NoMore, Lex.Error, FloatMode.Trap*>
VAR
  initialized := FALSE;
BEGIN
  Grain := Time.Grain;

  IF dLevelT # NIL THEN dLevel := Scan.Int(dLevelT) END;

  (* search for an arg of type @M3xtime=<host>:<port> *)
  WITH xtimespec = RTParams.Value("xtime") DO
    IF xtimespec # NIL THEN
      WITH sReader = NEW(TextReader.T).init(xtimespec),
           host = sReader.nextE(":"),
           port = Scan.Int(sReader.nextE("")) DO
        DebugOut("Connecting to XTime source at "& 
          host & ", port " & Fmt.Int(port));
        
        <*FATAL Thread.Alerted*>
        VAR
          ip : IP.Address;
          success := FALSE;
          conn : TCP.T;
        BEGIN
          TRY
            success := IP.GetHostByName(host, ip)
          EXCEPT
            IP.Error => (* skip *)
          END;

          IF NOT success THEN
            Process.Crash("Couldn't connect to XTime source at \"" & 
              host & "\": host name lookup failure")
          END;

          WITH ep = IP.Endpoint { ip, port } DO
            TRY
              conn := TCP.Connect(ep);
              
              WITH cl = NEW(Closure, 
                            rd := ConnRW.NewRd(conn), 
                            mu := NEW(MUTEX), 
                            c  := NEW(Thread.Condition)) DO
                EVAL Thread.Fork(cl);

                DebugOut("Awaiting XTime initialization via TCP...");
                LOCK mu DO
                  WHILE NOT cl.initialized DO
                    Thread.Wait(cl.mu, cl.c)
                  END
                END
              END
            EXCEPT
              IP.Error(e) =>
              Process.Crash("Couldn't connect to XTime source at \"" & 
                host & "\": TCP connection failure : " & AL.Format(e))
            END
          END
          
        END
      END
    END
  END;

  initialized := TRUE
END XTime.
