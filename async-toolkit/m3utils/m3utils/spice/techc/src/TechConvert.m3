(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE TechConvert;
IMPORT Thread;
IMPORT OSError;
IMPORT TextWr;
IMPORT ProcUtils;
IMPORT Debug;
IMPORT Watchdog;
FROM Fmt IMPORT F, LongReal, FN;
IMPORT Pathname;
IMPORT Process;
IMPORT TechConfig;
FROM TechConfig IMPORT Simu;
IMPORT FS;
IMPORT CitTextUtils;
IMPORT Env;

CONST LR = LongReal;

CONST DefProcDeadline = 30.0d0 * 60.0d0;
      (* give it 30 minutes for each subprocess step (circuit sim and aspice
         data conversion) *)

VAR ProcDeadline := DefProcDeadline;
          
REVEAL
  MyCb = PublicCb BRANDED OBJECT
  OVERRIDES
    do := MyCbDo;
  END;

PROCEDURE MyCbDo(cb : MyCb) =
  BEGIN
    Debug.Out(F("\n!!! WOOF WOOF !!!\nCommand \"%s\" with output\n====>\n%s\n<====\n\nWatchdog expired!  Exiting!",
                cb.cmd,
                TextWr.ToText(cb.wr)));
    Process.Exit(1)
  END MyCbDo;

(* semaphore for Convert phase *)
VAR
  convertMu := NEW(MUTEX);
  convertC  := NEW(Thread.Condition);
  convertS  : [0..1] := 0;

(* Dijkstra's P() and V() *)
PROCEDURE ConvertP() =
  BEGIN
    LOCK convertMu DO
      WHILE convertS = 1 DO
        Thread.Wait(convertMu, convertC)
      END;
      INC(convertS)
    END
  END ConvertP;

PROCEDURE ConvertV() =
  BEGIN
    LOCK convertMu DO
      DEC(convertS);
      Thread.Signal(convertC)
    END
  END ConvertV;

PROCEDURE FindFsdbInDir(dir : Pathname.T) : Pathname.T RAISES { OSError.E } =
  VAR
    iter := FS.Iterate(dir);
    bn : Pathname.T;
  BEGIN
    WHILE iter.next(bn) DO
      IF CitTextUtils.HaveSuffix(bn, ".fsdb") THEN
        RETURN dir & "/" & bn
      END
    END;
    RETURN NIL
  END FindFsdbInDir;
  
PROCEDURE DoConvert(READONLY c : TechConfig.T;
                    traceRoot : Pathname.T; exitOnError : BOOLEAN) : BOOLEAN =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    (* need to add a feature where we can request a minimum number of steps
       in the output aspice file ... 

       OR we could take a measurement and after having measured, ensure
       that we have enough timesteps, and if not, re-sample the file at 
       a finer timestep.

       May be easiest to just have an option to converttrace that fixes
       the # of timesteps in the file, either as a fixed count or as a 
       minimum # of steps.
    *)
    fsdbPath := Debug.UnNil(FindFsdbInDir(c.workDir));
    
    cmd := FN("%s -nochop -fsdb %s -compress %s -threads 4 -wthreads 1 -format CompressedV1 -R %s %s %s",
             ARRAY OF TEXT {
    M3Utils & "/" & CtPath,
    M3Utils & "/" & NanosimrdPath,
    M3Utils & "/" & SpicestreamPath,
    
    LR(MAX(c.timestep, 50.0d-12)), fsdbPath, traceRoot
    });
    res : BOOLEAN := FALSE;
  BEGIN 
    (*Wr.Close(wrIn);*)
    ConvertP();
    
    TRY
      CASE c.simu OF
        Simu.Xa, Simu.Hspice =>
        Debug.Out("DoConvert: " & cmd);
        WITH wd = NEW(Watchdog.T).init(ProcDeadline),
             c  = ProcUtils.RunText(cmd,
                                    stdout := stdout,
                                    stderr := stderr,
                                    stdin  := NIL),
             cb = NEW(MyCb, cmd := cmd, wr := wr) DO
               wd.setExpireAction(cb);
               TRY
                 c.wait();
                 res := TRUE
               EXCEPT
                 ProcUtils.ErrorExit(err) =>
                 WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                              cmd,
                              TextWr.ToText(wr),
                              ProcUtils.FormatError(err)) DO
                   IF exitOnError THEN
                     Debug.Error(msg)
                   ELSE
                     Debug.Warning(msg);
                     res := FALSE
                   END
                 END
               END;
               wd.kill()
             END
      END;
      Debug.Out("DoConvert output :\n" & TextWr.ToText(wr));
    FINALLY
      ConvertV()
    END;
    RETURN res
  END DoConvert;

VAR
  M3Utils := Env.Get("M3UTILS");
BEGIN
  <*ASSERT M3Utils # NIL*>
END TechConvert.
