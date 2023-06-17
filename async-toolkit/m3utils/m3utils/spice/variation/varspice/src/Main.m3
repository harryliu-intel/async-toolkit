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
FROM XorRingOsc IMPORT XaVar, XaTranSpec, NV, LibDir, CellName;
IMPORT TextTextTbl;
IMPORT CitTextUtils;
FROM TechConfig IMPORT Tran, TranNames;
FROM TechLookup IMPORT Lookup;
IMPORT TechTemplate;
IMPORT TextLRPair;
IMPORT TextLRPairArraySort;
IMPORT NormalDeviate;
IMPORT Random;

<*FATAL Thread.Alerted*>

CONST
  TE = Text.Equal;
  N  = NV * 2; (* there's an up-gate and a down-gate in the ckt *)
  
TYPE
  P = ARRAY [ 0 .. N-1 ] OF LONGREAL;

VAR
  Var      := XaVar;
  TranSpec := XaTranSpec;

PROCEDURE MapText(txt : TEXT; map : TextTextTbl.T) : TEXT =
  VAR
    iter := map.iterate();
    k, v : TEXT;
  BEGIN
    WHILE iter.next(k, v) DO
      IF v = NIL THEN
        Debug.Error("NIL mapping for " & k)
      END;
      txt := CitTextUtils.Replace(txt, k, v)
    END;
    RETURN txt
  END MapText;
  
PROCEDURE WriteVar(wr : Wr.T; map : TextTextTbl.T) RAISES { Wr.Failure } =
  CONST
    Nstages = 10;
    Nsubs   =  2;
    Ntrans  = NUMBER(TranSpec);
    Nvar    = NUMBER(Var);

    N       = Nstages * Nsubs * Ntrans * Nvar;

  VAR
    data    : ARRAY [ 0 .. N - 1 ] OF TextLRPair.T;
    pidx    : CARDINAL;
    qidx    : CARDINAL;
  BEGIN
    Wr.PutText(wr, ".data extern_data\n");
    Wr.PutText(wr, "index ");
    qidx := 0;
    FOR stage := 1 TO Nstages DO
      pidx := 0;
      FOR sub := 0 TO Nsubs - 1 DO
        FOR tran := FIRST(TranSpec) TO LAST(TranSpec) DO
          FOR var := FIRST(Var) TO LAST(Var) DO
            WITH nam =
                 MapText(F("X%s.X%s.%s:@:%s:@:ILN",
                           Int(stage),
                           Int(sub),
                           TranSpec[tran],
                           Var[var]), map) DO
              Wr.PutText(wr, nam);
              Wr.PutText(wr, " ");
              data[qidx].k1 := nam
            END;
            INC(pidx); INC(qidx)
          END
        END
      END
    END;
    <*ASSERT qidx = N*>
    
    Wr.PutText(wr, "\n");
    
    Wr.PutText(wr, "2 ");
    qidx := 0;
    FOR stage := 1 TO 10 DO
      pidx := 0;
      FOR sub := 0 TO 1 DO
        FOR tran := FIRST(TranSpec) TO LAST(TranSpec) DO
          FOR var := FIRST(Var) TO LAST(Var) DO
            VAR
              val : LONGREAL;
            BEGIN
              IF single AND stage # 4 THEN
                val := 0.0d0;
              ELSE
                val := p[pidx];
              END;
              Wr.PutText(wr, LongReal(val));
              data[qidx].k2 := ABS(val)
            END;
            Wr.PutText(wr, " ");
            INC(pidx); INC(qidx)
          END
        END
      END
    END;
    
    Wr.PutText(wr, "\n");
    Wr.PutText(wr, ".enddata\n");

    TextLRPairArraySort.Sort(data, cmp := TextLRPair.CompareK2K1);

    FOR i := LAST(data) TO FIRST(data) BY -1 DO
      IF data[i].k2 = 0.0d0 THEN EXIT END;
      Debug.Out(F("%50s %s", data[i].k1, LongReal(data[i].k2)))
    END
  END WriteVar;

PROCEDURE CopyTemplate(templatePath, rundirPath : Pathname.T;
                       map                      : TextTextTbl.T;) =
  VAR
    tmpl : TechTemplate.T;
  BEGIN
    TRY
      tmpl := TechTemplate.LoadTemplate(templatePath);
    EXCEPT
      OSError.E(x) => Debug.Error("Opening template " & templatePath & " : caught error : OSError.E : " & AL.Format(x));
    |
      Rd.Failure(x) => Debug.Error("I/O error (Rd.Failure) while copying template : " & AL.Format(x))
    END;

    TechTemplate.ModifyTemplate(tmpl, map);
    
    TRY
      TechTemplate.WriteTemplate(tmpl, rundirPath & "/ckt.sp");
    EXCEPT
      OSError.E(x) => Debug.Error("Opening target " & rundirPath & "/ckt.sp" & " : caught error : OSError.E : " & AL.Format(x))
    |
      Wr.Failure(x) => Debug.Error("I/O error (Wr.Failure) while copying template : " & AL.Format(x))
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
  Phase = { Mkdir, Copy, CreateVar, RunSim, DoMeasure, Clean };

PROCEDURE MakeMap() : TextTextTbl.T =
  VAR
    map := NEW(TextTextTbl.Default).init();
  BEGIN
    EVAL map.put("%THRESH%", TranNames[thresh]);
    EVAL map.put("%Z%", Int(z));
    EVAL map.put("@LIBDIR@", LibDir[z][thresh]);
    EVAL map.put("@CELL@", CellName[z][thresh]);
    RETURN map
  END MakeMap;
  
CONST
  AllPhases = SET OF Phase { FIRST(Phase) .. LAST(Phase) };
  PhaseNames = ARRAY Phase OF TEXT { "mkdir", "copy", "createvar", "runsim", "measure", "clean" };
  
VAR
  p                 := P { 0.1d0, .. };
  pp                := NEW(ParseParams.T).init(Stdio.stderr);
  phases            := SET OF Phase {};
  gotPhase          := FALSE;
  z                 := LAST(CARDINAL);

  exact        : BOOLEAN;
  templatePath : Pathname.T;
  rundirPath   : Pathname.T;
  single       : BOOLEAN;
  thresh       : Tran;
  doNormal     : BOOLEAN;
  
BEGIN
  TRY

    doNormal := pp.keywordPresent("-N");
    
    FOR ph := FIRST(Phase) TO LAST(Phase) DO
      IF pp.keywordPresent("-" & PhaseNames[ph]) THEN
        phases := phases + SET OF Phase { ph };
        gotPhase := TRUE;
      END
    END;

    IF pp.keywordPresent("-thresh") THEN
      thresh := VAL(Lookup(pp.getNext(), TranNames), Tran)
    ELSE
      Debug.Error("Must provide -thresh")
    END;

    IF NOT gotPhase THEN phases := AllPhases END;
    
    exact := pp.keywordPresent("-exact");

    single := pp.keywordPresent("-single");
    (* just make a single stage slow *)

    IF pp.keywordPresent("-T") OR pp.keywordPresent("-template") THEN
      templatePath := pp.getNext()
    END;

    IF pp.keywordPresent("-r") THEN
      rundirPath := pp.getNext()
    END;

    IF pp.keywordPresent("-z") THEN
      z := pp.getNextInt()
    ELSE
      Debug.Error("Must provide -z")
    END;

    TranSpec := XaTranSpec;
    
    pp.skipParsed();

    IF doNormal THEN
      VAR
        rand         : Random.T := NEW(Random.Default).init();
      BEGIN
        FOR i := FIRST(p) TO LAST(p) DO
          p[i] := NormalDeviate.Get(rand, 0.0d0, 1.0d0)
        END
      END
    ELSE
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
    END;

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF z = LAST(CARDINAL) THEN
    Debug.Error("Must specify -z")
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
    WITH map = MakeMap() DO
      CopyTemplate(templatePath, rundirPath, map)
    END
  END;

  IF Phase.CreateVar IN phases THEN
    TRY
      WITH map = MakeMap(),
           wr  = FileWr.Open(rundirPath & "/var.sp") DO
        WriteVar(wr, map);
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
      Wr.PutChar(Stdio.stdout, '\n');

      WITH rwr = FileWr.Open("result.csv") DO
        Wr.PutText(rwr, LongReal(meas));
        FOR i := FIRST(p) TO LAST(p) DO
          Wr.PutChar(rwr, ',');
          Wr.PutText(rwr, LongReal(p[i]));
        END;
        Wr.PutChar(rwr, '\n');
        Wr.Close(rwr)
      END
    END
  END;

  CONST
    CleanFiles = ARRAY OF Pathname.T {
                    "ckt.mc", "ckt.mc.csv", "ckt.log", "var.sp" };
  BEGIN
    IF Phase.Clean IN phases THEN
      FOR i := FIRST(CleanFiles) TO LAST(CleanFiles) DO
        TRY
          FS.DeleteFile(CleanFiles[i])
        EXCEPT ELSE END
      END
    END
  END
END Main.


   

 
