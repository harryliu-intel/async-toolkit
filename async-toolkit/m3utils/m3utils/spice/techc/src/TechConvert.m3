MODULE TechConvert;
IMPORT Thread;
IMPORT OSError;
IMPORT TextWr;
IMPORT ProcUtils;
IMPORT Debug;
IMPORT Watchdog;
FROM Fmt IMPORT F, LongReal;
IMPORT Pathname;
IMPORT Process;
IMPORT TechConfig;
FROM TechConfig IMPORT Simu;

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
  
PROCEDURE DoConvert(READONLY c : TechConfig.T;
                    traceRoot : Pathname.T; exitOnError : BOOLEAN) : BOOLEAN =
  <*FATAL OSError.E*> (* this pertains to the TextWr *)
  VAR
    wr := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);

    cmd := F("/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/ct/AMD64_LINUX/ct -fsdb /nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/fsdb/src/nanosimrd -threads 4 -wthreads 1 -R %s %s.fsdb %s",
             LR(MAX(c.timestep, 50.0d-12)), c.simRoot, traceRoot);
    res : BOOLEAN := FALSE;
  BEGIN 
    (*Wr.Close(wrIn);*)
    ConvertP();
    
    TRY
      CASE c.simu OF
        Simu.Xa =>
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
      |
        Simu.Hspice => <*ASSERT FALSE*>
      END;
      Debug.Out("DoConvert output :\n" & TextWr.ToText(wr));
    FINALLY
      ConvertV()
    END;
    RETURN res
  END DoConvert;

BEGIN END TechConvert.
