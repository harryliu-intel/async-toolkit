MODULE Main;
IMPORT Wr;
IMPORT Stdio;
FROM Fmt IMPORT Int, LongReal, F;
IMPORT Thread;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Scan;
IMPORT FS;
IMPORT FileRd;
IMPORT FileWr;
IMPORT Pathname;
IMPORT Rd;
IMPORT OSError;
IMPORT AL;
IMPORT TextWr;
IMPORT ProcUtils;
IMPORT Process;
IMPORT TextReader;
IMPORT Text;
IMPORT Lex, FloatMode;
FROM RingOsc IMPORT XaVar, XaTran, N;

<*FATAL Thread.Alerted*>

CONST
  TE = Text.Equal;
  
TYPE
  P = ARRAY [ 0 .. N-1 ] OF LONGREAL;

VAR
  Var  := XaVar;
  Tran := XaTran;

PROCEDURE WriteVar(wr : Wr.T) RAISES { Wr.Failure } =
  BEGIN
  Wr.PutText(wr, ".data extern_data\n");
  Wr.PutText(wr, "index ");
  FOR stage := 1 TO 10 DO
    pidx := 0;
    FOR sub := 0 TO 1 DO
      FOR tran := FIRST(Tran) TO LAST(Tran) DO
        FOR var := FIRST(Var) TO LAST(Var) DO
          Wr.PutText(wr, F("X%s.xstage.X%s.%s:@:%s:@:ILN",
                                     Int(stage),
                                     Int(sub),
                                     Tran[tran],
                                     Var[var]));
          Wr.PutText(wr, " ");
          INC(pidx)
        END
      END
    END
  END;

  Wr.PutText(wr, "\n");
  
  Wr.PutText(wr, "2 ");
  FOR stage := 1 TO 10 DO
    pidx := 0;
    FOR sub := 0 TO 1 DO
      FOR tran := FIRST(Tran) TO LAST(Tran) DO
        FOR var := FIRST(Var) TO LAST(Var) DO
          Wr.PutText(wr, LongReal(p[pidx]));
          Wr.PutText(wr, " ");
          INC(pidx)
        END
      END
    END
  END;

  Wr.PutText(wr, "\n");
  Wr.PutText(wr, ".enddata\n")

END WriteVar;

PROCEDURE CopyTemplate(templatePath, rundirPath : Pathname.T) =
  VAR
    tpRd : Rd.T;
    spWr : Wr.T;
    buff : ARRAY [0..8191] OF CHAR;
    cnt : CARDINAL;
  BEGIN
    TRY
      tpRd := FileRd.Open(templatePath);
    EXCEPT
      OSError.E(x) => Debug.Error("Opening template " & templatePath & " : caught error : OSError.E : " & AL.Format(x))
    END;

    TRY
      spWr := FileWr.Open(rundirPath & "/ckt.sp");
    EXCEPT
      OSError.E(x) => Debug.Error("Opening target " & rundirPath & "/ckt.sp" & " : caught error : OSError.E : " & AL.Format(x))
    END;
    TRY
      LOOP
        cnt := Rd.GetSub(tpRd, buff);
        IF cnt = 0 THEN EXIT END;
        Wr.PutString(spWr, SUBARRAY(buff, 0, cnt))
      END;
      Rd.Close(tpRd);
      Wr.Close(spWr)
    EXCEPT
      Wr.Failure(x) => Debug.Error("I/O error (Wr.Failure) while copying template : " & AL.Format(x))
    |
      Rd.Failure(x) => Debug.Error("I/O error (Rd.Failure) while copying template : " & AL.Format(x))
    END
  END CopyTemplate;

PROCEDURE RunSimulation() RAISES { ProcUtils.ErrorExit } =
  <*FATAL OSError.E*>
  VAR
    simPath        := "/p/hdk/cad/xa/U-2023.03-1-T/bin";
    simRoot        := "ckt";
    wr             := NEW(TextWr.T).init();
    stdout, stderr := ProcUtils.WriteHere(wr);
    cmd            := F("%s/xa %s.sp -o %s -mt 4", simPath, simRoot, simRoot);
    cm             := ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL);
  BEGIN
    TRY
      Debug.Out("RunSimulation : running : " & cmd);
      cm.wait()
    EXCEPT
      ProcUtils.ErrorExit(err) =>
      WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                   cmd,
                   TextWr.ToText(wr),
                   ProcUtils.FormatError(err)) DO
        Debug.Warning(msg);
        RAISE ProcUtils.ErrorExit(err)
      END
    END
  END RunSimulation;

PROCEDURE DoMeasure() : LONGREAL
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile } =
  VAR
    rd : Rd.T;
  BEGIN
    TRY

      WITH path = "ckt.meas" DO
        TRY
          rd := FileRd.Open(path)
        EXCEPT
          OSError.E(x) => Debug.Error("DoMeasure : opening " & path & " : caught error : OSError.E : " & AL.Format(x))
        END
      END;         

      TRY
        LOOP
          WITH line   = Rd.GetLine(rd),
               reader = NEW(TextReader.T).init(line),
               lst    = reader.shatter(" ", endDelims := "", skipNulls := TRUE) DO
            IF lst # NIL AND lst.head # NIL AND TE(lst.head, "Cycle") THEN
              RETURN Scan.LongReal(lst.tail.tail.head)
            END
          END
        END
      EXCEPT
        FloatMode.Trap, Lex.Error =>  Debug.Error("DoMeasure : error parsing number in output file"); <*ASSERT FALSE*>
      END
    FINALLY
      Rd.Close(rd)
    END
  END DoMeasure;
  
TYPE
  Phase = { Mkdir, Copy, CreateVar, RunSim, DoMeasure };

CONST
  AllPhases = SET OF Phase { FIRST(Phase) .. LAST(Phase) };
  PhaseNames = ARRAY Phase OF TEXT { "mkdir", "copy", "createvar", "runsim", "measure" };
  
VAR
  p := P { 0.1d0, .. };
  pidx : CARDINAL;
  exact : BOOLEAN;
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
  templatePath : Pathname.T;
  rundirPath   : Pathname.T;
  phases := SET OF Phase {};
  gotPhase := FALSE;
  
BEGIN
  TRY
    FOR ph := FIRST(Phase) TO LAST(Phase) DO
      IF pp.keywordPresent("-" & PhaseNames[ph]) THEN
        phases := phases + SET OF Phase { ph };
        gotPhase := TRUE;
      END
    END;

    IF NOT gotPhase THEN phases := AllPhases END;
    
    exact := pp.keywordPresent("-exact");

    IF pp.keywordPresent("-T") OR pp.keywordPresent("-template") THEN
      templatePath := pp.getNext()
    END;

    IF pp.keywordPresent("-r") THEN
      rundirPath := pp.getNext()
    END;
    
    pp.skipParsed();

    WITH argdims = NUMBER(pp.arg^) - pp.next DO
      IF exact AND argdims # N THEN
        Debug.Error("Wrong dims count : " & Int(argdims) & " # " & Int(N))
      END;
      FOR i := 0 TO argdims - 1 DO
        WITH arg = pp.getNext() DO
          TRY
            p[i] := Scan.LongReal(arg)
          EXCEPT
            FloatMode.Trap, Lex.Error => Debug.Error("Trouble parsing command-line argument as number : " & arg)
          END
        END
      END
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF templatePath = NIL OR rundirPath = NIL THEN
    Debug.Error("Must specify template [-T] and rundir [-r]")
  END;

  IF Phase.Mkdir IN phases THEN
    TRY
      FS.CreateDirectory(rundirPath)
    EXCEPT
      OSError.E(x) => Debug.Error("FS.CreateDirectory : caught error : OSError.E : " & AL.Format(x))
    END
  END;

  IF Phase.Copy IN phases THEN
    CopyTemplate(templatePath, rundirPath)
  END;

  IF Phase.CreateVar IN phases THEN
    TRY
      WITH wr = FileWr.Open(rundirPath & "/var.sp") DO
        WriteVar(wr);
        Wr.Close(wr)
      END
    EXCEPT
      OSError.E(x) => Debug.Error("Creating var.sp : caught error : OSError.E : " & AL.Format(x))
    |
      Wr.Failure(x) => Debug.Error("Creating var.sp : caught error : Wr.Failure : " & AL.Format(x))
    END
  END;

  TRY
    Process.SetWorkingDirectory(rundirPath)
  EXCEPT
    OSError.E(x) => Debug.Error("Setting working directory : caught error : OSError.E : " & AL.Format(x))
  END;

  IF Phase.RunSim IN phases THEN
    VAR
      success := FALSE;
    CONST
      MaxAttempts = 5;
    BEGIN
      FOR i := 1 TO MaxAttempts DO
        TRY
          RunSimulation();
          success := TRUE;
          EXIT
        EXCEPT
          ProcUtils.ErrorExit => (* loop *)
        END
      END;
      IF NOT success THEN
        Debug.Error("Too many attempts running simulation");
        <*ASSERT FALSE*>
      END;
    END
  END;

  IF Phase.DoMeasure IN phases THEN
    WITH meas = DoMeasure() DO
      Wr.PutText(Stdio.stdout, LongReal(meas));
      Wr.PutChar(Stdio.stdout, '\n')
    END
  END
END Main.


   

 
