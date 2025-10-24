(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TechSimulate;
IMPORT OSError, TextWr;
IMPORT ProcUtils;

IMPORT TechConfig;
IMPORT Watchdog;
FROM Fmt IMPORT F;
IMPORT Debug;
FROM TechConfig IMPORT Simu;
IMPORT Thread;
IMPORT SimWatcher;
IMPORT Pathname;
IMPORT Rd;
IMPORT AL;
IMPORT TechKill;
FROM TechConvert IMPORT MyCb;


CONST DefProcDeadline = 30.0d0 * 60.0d0;
      (* give it 30 minutes for each subprocess step (circuit sim and aspice
         data conversion) *)
      

TYPE
  MyKillCb = MyCb OBJECT
    simRoot : Pathname.T;
    myKill  : REF BOOLEAN; (* set to TRUE if killing *)
  OVERRIDES
    do := MyCbDoKillSim;
  END;

PROCEDURE MyCbDoKillSim(cb : MyKillCb) =
  BEGIN
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" --- Watchdog expired!  Attempting to kill simulator!",
                cb.cmd));

    WITH logFn = cb.simRoot & ".log" DO
      TRY
        cb.myKill^ := TRUE;
        TechKill.FromLogFile(logFn);

        Debug.Out(F("Command output was\n====>\n%s\n<====\n\nWatchdog expired!",
                TextWr.ToText(cb.wr)));

        
      EXCEPT
        OSError.E(e) => Debug.Warning(F("Couldn't open log file \"%s\" : OSError.E : %s",
                                      logFn, AL.Format(e)))
      |
        Rd.Failure(e) =>
        Debug.Warning(F("Couldn't read log file \"%s\" : Rd.Failure : %s",
                      logFn, AL.Format(e)))
      END
    END;
    
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" with output\n====>\n%s\n<====\n\nWatchdog expired!  Exiting!",
                cb.cmd,
                TextWr.ToText(cb.wr)))
  END MyCbDoKillSim;
  
PROCEDURE DoSimulate(READONLY c : Config) =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  CONST
    Affirmation = "y\ny\ny\ny\ny\n";
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd : TEXT;
    myKill := NEW(REF BOOLEAN);
  BEGIN

    myKill^ := FALSE;
    
    (*Wr.Close(wrIn);*)
    CASE c.simu OF
      Simu.Xa =>

      cmd := F("%s/xa %s.sp -o %s", c.xaPath, c.simRoot, c.simRoot)
    |
      Simu.Hspice =>

      cmd := F("%s/hspice -case 1 -i %s.sp -o %s", c.hspicePath, c.simRoot, c.simRoot);
    END;

    Debug.Out("DoSimulate: " & cmd);
    WITH wd = NEW(Watchdog.T).init(ProcDeadline),
         cm = ProcUtils.RunText(cmd,
                                stdout := stdout,
                                stderr := stderr,
                                stdin  := ProcUtils.ReadThis(Affirmation)),

         (* we need to feed in the affirmation, because when we hit xa with
            kill -INT, it stops and asks the y/n question whether it should
            exit... *)
         
         cb = NEW(MyKillCb,
                  myKill  := myKill,
                  cmd     := cmd,
                  wr      := wr,
                  simRoot := c.simRoot) DO
      
      wd.setExpireAction(cb);
      EVAL Thread.Fork(NEW(SimWatcher.T,
                           c       := c,
                           myKill  := myKill,
                           cm      := cm,
                           simRoot := c.simRoot));
      TRY
        cm.wait()
      EXCEPT
        ProcUtils.ErrorExit(err) =>
        WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                     cmd,
                     TextWr.ToText(wr),
                     ProcUtils.FormatError(err)) DO
          IF myKill^ THEN
            Debug.Warning(msg)
          ELSE
            Debug.Error(msg)
          END
        END
      END;
      wd.kill()
    END;
    
    Debug.Out("DoSimulate output :\n" & TextWr.ToText(wr))
  END DoSimulate;

BEGIN

  ProcDeadline := DefProcDeadline;

END TechSimulate.
