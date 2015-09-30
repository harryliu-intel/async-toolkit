MODULE NBAvail EXPORTS Main;
IMPORT ProcUtils;
IMPORT Debug;
IMPORT Text, Rd, TextReader, TextRd;
IMPORT Scan;
IMPORT TextList;
IMPORT Time, TZ;
IMPORT Date;
IMPORT IO;
FROM Fmt IMPORT F, Int, Bool;

CONST TE = Text.Equal;

VAR
  cmd := "nbstatus jobs --target sc_normal --fi jobid:count,status,qslot::40 --gr status qslot=='/sse/rrc/layout'";

  status : TEXT;
  waiters, runners : CARDINAL;


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

BEGIN
  Debug.SetLevel(0); (* turn off debugging regardless of env. *)

  workingHours := NowIsWorkingHours();
  Debug.Out("cmd : " & cmd);
  status := ProcUtils.ToText(cmd);
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

  CONST
    OutOfHoursMultiplier =    2;
  VAR
    maxRunning          := 1000;
    maxWaiting          :=  100;
  BEGIN
    IF NOT workingHours THEN
      maxRunning := maxRunning * OutOfHoursMultiplier;
      maxWaiting := maxWaiting (* * OutOfHoursMultiplier *);
    END;

    WITH totalJobs = waiters + runners,
         runSlack  = maxRunning - totalJobs,
         waitSlack = maxWaiting - waiters DO
      IO.Put(Int(MAX(0, MIN(runSlack,waitSlack))) & "\n")
    END
  END
END NBAvail.
