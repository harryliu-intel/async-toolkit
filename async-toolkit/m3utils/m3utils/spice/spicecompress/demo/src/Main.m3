(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT AL;
IMPORT OSError;
IMPORT Rd;
IMPORT Process;
IMPORT Pathname;
FROM Fmt IMPORT F, Int, LongReal, Pad;
IMPORT FS;
IMPORT Trace;
IMPORT CardSeq;
IMPORT Matrix AS MatrixE;
FROM SpiceCompress IMPORT Norm, CompressArray;
IMPORT Time;
IMPORT Rep16;

CONST
  LR       = LongReal;
CONST
  Usage    = "";
  
CONST <*NOWARN*>Ks = ARRAY OF CARDINAL { 1, 63, 64, 77, 91, 99         };
      <*NOWARN*>Km = ARRAY OF CARDINAL { 1,         77    , 99         };
      <*NOWARN*>Kq = ARRAY OF CARDINAL {            77                 };
      <*NOWARN*>Kg = ARRAY OF CARDINAL {            77        , 108091 };
                K  = Kg;
                
PROCEDURE DoDemo(targMaxDev : LONGREAL;
                 KK         : REF ARRAY OF CARDINAL;
                 trace      : Trace.T;
                 doAllDumps : BOOLEAN) =

<*FATAL Rd.Failure, Rd.EndOfFile*> 
<*FATAL MatrixE.Singular*>
  VAR
    nSteps := trace.getSteps();
    darr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    rarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    norm   : Norm;
  BEGIN

    FOR p := 1 TO 3 DO
      FOR s := -5 TO 5 DO
        Debug.Out(F("ToFloat(%s, %s) = %s", Int(s), Int(p), LR(Rep16.ToFloat(s, p))))
      END
    END;
    
    FOR k := FIRST(KK^) TO LAST(KK^) DO
      WITH i  = KK[k],
           rn = Pad(Int(i), 6, padChar := '0')
       DO
        Debug.Out("Loading trace");
        trace.getNodeData(i, darr^);
        Debug.Out("Compressing trace");
        WITH now = Time.Now() DO
          CompressArray(rn, darr^, rarr^, targMaxDev, doAllDumps, norm := norm,
                        wr := NIL, mem := NIL, doDump := TRUE);
          Debug.Out("Done compressing trace, time " & LR(Time.Now() - now));
        END
      END
    END
  END DoDemo;

VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  traceRt       : Pathname.T     := "xa";
  outDir        : Pathname.T     := "out";
  createOutDir  : BOOLEAN;
  trace         : Trace.T;
  KK            : REF ARRAY OF CARDINAL;
  wf                             := NEW(CardSeq.T).init();
  doAllDumps    : BOOLEAN;
  relPrec                        := 0.005d0;
BEGIN

  TRY

    doAllDumps := pp.keywordPresent("-dodumpall");
    
    createOutDir := pp.keywordPresent("-C");

    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;

    IF pp.keywordPresent("-w") THEN
      wf.addhi(pp.getNextInt())
    END;

    IF pp.keywordPresent("-prec") THEN
      relPrec := pp.getNextLongReal()
    END;

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF wf.size() = 0 THEN
    KK := NEW(REF ARRAY OF CARDINAL, NUMBER(K));
    KK^ := K
  ELSE
    KK := NEW(REF ARRAY OF CARDINAL, wf.size());
    
    FOR i := 0 TO wf.size() - 1 DO
      KK[i] := wf.get(i)
    END
  END;

  TRY
    trace := NEW(Trace.T).init(traceRt)
  EXCEPT
    OSError.E(x) =>
    Debug.Error(F("Trouble opening input trace %s : OSError.E : %s",
                  traceRt, AL.Format(x)))
  |
    Rd.Failure(x) => Debug.Error("Trouble opening input trace : Rd.Failure : " & AL.Format(x))
  |
    Rd.EndOfFile =>
    Debug.Error(F("Short read opening input trace"))
  END;
  
  IF outDir # NIL THEN
    TRY
      IF createOutDir THEN
        TRY FS.CreateDirectory(outDir) EXCEPT ELSE END
      END;
      Process.SetWorkingDirectory(outDir)
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                    outDir, AL.Format(e)))
    END
  END;

  DoDemo(relPrec, KK, trace, doAllDumps)

END Main.
