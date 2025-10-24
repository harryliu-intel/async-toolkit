(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SimWatcher;
IMPORT Time, Thread;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal;
IMPORT AL;
IMPORT Rd;
IMPORT OSError;
IMPORT TechKill;
IMPORT TechConvert;
IMPORT TechMeasure;
IMPORT TechProgress;

CONST LR = LongReal;
  
REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    apply := SwApply;
  END;
  
CONST FirstProgressDelay = 2.0d0 * 60.0d0;
      (* when to check for progress / two minutes *)
      
      ProgressDelayMultiplier = 1.414d0;
      (* how much to back off each progress measurement *)



PROCEDURE SwApply(sw : T) : REFANY =
  CONST
    FirstDelay   = FirstProgressDelay;
  VAR
    d := FirstDelay;
    now : Time.T;
  BEGIN
    LOOP
      now := Time.Now();
      WITH deadLine = now + d DO
        REPEAT
          Thread.Pause(1.0d0);
          now := Time.Now()
        UNTIL now >= deadLine
      END;
      
      Debug.Out(F("SimWatcher waking up at %s, d=%s", LR(Time.Now()), LR(d)));

      (* trial conversion *)
      IF TechConvert.DoConvert(sw.c,
                               TechProgress.Root,
                               exitOnError := FALSE) AND
         TechMeasure.DoMeasure(sw.c,
                               TechProgress.Root,
                               TechProgress.Root & ".dat",
                               sw.simRoot) THEN
        (* succeeded, we have the output! *)
        
        Debug.Out("Progress measurement succeeded, will try to kill simulator!");


        WITH logFn = sw.simRoot & ".log" DO

          TRY
            sw.myKill^ := TRUE;
            TechKill.FromLogFile(logFn)

          EXCEPT
            OSError.E(e) => Debug.Error(F("Couldn't open log file \"%s\" : OSError.E : %s",
                                          logFn, AL.Format(e)))
          |
            Rd.Failure(e) =>
            Debug.Error(F("Couldn't read log file \"%s\" : Rd.Failure : %s",
                          logFn, AL.Format(e)))
          END;
        END;
        RETURN NIL
      END(*IF*);
      
      d := d * ProgressDelayMultiplier
    END
  END SwApply;

BEGIN END SimWatcher.
