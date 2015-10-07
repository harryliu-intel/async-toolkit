MODULE NBAvail EXPORTS Main;
IMPORT ProcUtils;
IMPORT Debug;
IMPORT Text, Rd, TextReader, TextRd;
IMPORT Scan;
IMPORT TextList;
IMPORT Time, TZ;
IMPORT Date;
IMPORT IO;
IMPORT Thread;
IMPORT Env;
FROM Fmt IMPORT F, Int, Bool;
IMPORT TextUtils;

CONST TE = Text.Equal;

VAR
  cmd := "nbstatus jobs --target sc_normal --fi jobid:count,status,qslot::40 --gr status qslot=='/sse/rrc/layout'";

  status : TEXT;
  waiters, runners : CARDINAL;
  user := Env.Get("USER");


PROCEDURE NowIsWorkingHours() : BOOLEAN =
  VAR
  now := Time.Now();
  tz := TZ.New("America/Los_Angeles");
  nowD := tz.localtime(now);

  CONST 
    DayNames = ARRAY Date.WeekDay OF TEXT 
    { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
  BEGIN
    Debug.Out(F("The time is %02s:%02s:%02s on a %s", 
                Int(nowD.hour), Int(nowD.minute), Int(nowD.second), 
                DayNames[nowD.weekDay]));

    RETURN nowD.hour >= 6 AND nowD.hour <= 17 
          AND 
           nowD.weekDay >= Date.WeekDay.Mon AND nowD.weekDay <= Date.WeekDay.Fri
  END NowIsWorkingHours;

VAR
  workingHours : BOOLEAN;

PROCEDURE Contains(p : TextList.T; w : TEXT) : BOOLEAN =
  BEGIN
    WHILE p # NIL DO
      IF TE(p.head,w) THEN RETURN TRUE END;
      p := p.tail
    END;
    RETURN FALSE
  END Contains;

PROCEDURE UserJobs() : CARDINAL =
  VAR    
    cmd := "nbqstat user=" & user;
    data := ProcUtils.ToText(cmd);
    rd   := TextRd.New(data);
    cnt := 0;
    dummy : CARDINAL;
  BEGIN
    TRY
      LOOP
        WITH line = Rd.GetLine(rd) DO
          IF TextUtils.FindSub(line, user, dummy) THEN
            INC(cnt)
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    RETURN cnt
  END UserJobs; 

BEGIN
  IF Env.Get("DEBUGNBAVAIL") = NIL THEN
    Debug.SetLevel(0) (* turn off debugging regardless of env. *)
  END;

  workingHours := NowIsWorkingHours();
  Debug.Out("cmd : " & cmd);

  LOOP
    TRY
      Debug.Out("attempt");
      status := ProcUtils.ToText(cmd);
      Debug.Out("cmd done");
      EXIT
    EXCEPT
      ProcUtils.ErrorExit => 
      Debug.Out("error exit!");
      Thread.Pause(1.0d0)
    END
  END;

  Debug.Out("result : " & status);
  
  waiters := 0;

  WITH rd = NEW(TextRd.T).init(status) DO
    TRY
      LOOP
        WITH line   = Rd.GetLine(rd),
             reader = NEW(TextReader.T).init(line),
             words  = reader.shatter(" ","",skipNulls := TRUE) DO
          IF    TextList.Length(words) >= 2 THEN
            IF    Contains(words, "Wait") THEN
              INC(waiters,Scan.Int(words.head))
            ELSIF TE(TextList.Nth(words,1), "Run") THEN
              runners := Scan.Int(words.head)
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END;

  Debug.Out("waiting      " & Int(waiters));
  Debug.Out("runners      " & Int(runners));
  Debug.Out("workingHours " & Bool(workingHours));

  VAR
    maxRunning, maxWaiting, maxUser : CARDINAL;

  BEGIN
    IF workingHours THEN
      maxRunning :=  600;
      maxWaiting :=    1 + 
                           MAX(ROUND(FLOAT (maxRunning - runners,REAL)/FLOAT(maxRunning,REAL) * 100.0),0)
    ELSE
      maxRunning := 2000;
      maxWaiting :=   50
    END;

    maxUser := maxRunning;
    
    WITH envMaxRun  = Env.Get("NBAVAIL_MAXRUN"),
         envMaxWait = Env.Get("NBAVAIL_MAXWAIT"),
         envMaxUser = Env.Get("NBAVAIL_MAXUSER") DO
      IF envMaxRun  # NIL THEN maxRunning := Scan.Int(envMaxRun ) END;      
      IF envMaxWait # NIL THEN maxWaiting := Scan.Int(envMaxWait) END;
      IF envMaxUser # NIL THEN maxUser    := Scan.Int(envMaxUser) END;
    END;

    WITH totalJobs = waiters + runners,
         userJobs  = UserJobs(),
         runSlack  = maxRunning - totalJobs,
         waitSlack = maxWaiting - waiters,
         userSlack = maxUser    - userJobs DO
      IO.Put(Int(MAX(0, MIN(runSlack,MIN(waitSlack,userSlack)))) & "\n")
    END
  END
END NBAvail.
