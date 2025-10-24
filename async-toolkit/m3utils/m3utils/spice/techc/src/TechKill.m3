(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TechKill;
IMPORT Pathname, Rd, OSError;
IMPORT RegEx;
IMPORT FloatMode, Lex;
IMPORT FileRd;
IMPORT CitTextUtils;
IMPORT Debug;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT Scan;
IMPORT Usignal;
IMPORT Thread;

<*FATAL Thread.Alerted*>

PROCEDURE Kill(id : CARDINAL) =
  CONST
    DoDebug = TRUE;
  BEGIN
    (* INT needs to be the first signal sent *)
    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: INT (%s,%s)",
                  Int(id), Int(Usignal.SIGINT)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGINT); (* INTerrupt *)

    Thread.Pause(5.0d0); (* give it time to write out the log file *)

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: TERM (%s,%s)",
                  Int(id), Int(Usignal.SIGTERM)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGTERM); (* TERMinate *)
    
    Thread.Pause(1.0d0);

    IF DoDebug THEN
      Debug.Out(F("Abort killing sub: KILL (%s,%s)",
                  Int(id), Int(Usignal.SIGKILL)))
    END;
    EVAL Usignal.kill(id, Usignal.SIGKILL); (* KILL *)
  END Kill;

PROCEDURE FromLogFile(logFn : Pathname.T)
  RAISES { Rd.Failure, OSError.E } =
  
  <*FATAL RegEx.Error*>
  <*FATAL FloatMode.Trap, Lex.Error*>
  VAR
    numPat := RegEx.Compile("^[1-9][0-9]*$");
    rd := FileRd.Open(logFn);
  BEGIN
    TRY
      LOOP
        WITH line  = Rd.GetLine(rd),
             match = CitTextUtils.HaveSub(line, " pid ") DO

          Debug.Out(F("logfileline %s match %s", line, Fmt.Bool(match)));
          
          IF match THEN
            VAR
              sh := CitTextUtils.Shatter(line,
                                         delims := " |\t",
                                         endDelims := "",
                                         skipNulls := TRUE);
              p := sh;
            BEGIN
              WHILE p # NIL DO
                IF RegEx.Execute(numPat, p.head) # -1 THEN
                  WITH pid = Scan.Int(p.head) DO
                    Debug.Out(F("Regex match %s, attempting to kill",
                              Int(pid)));
                    Kill(pid)
                  END
                END;
                p := p.tail
              END
            END
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END
  END FromLogFile;

BEGIN END TechKill.
