MODULE Main EXPORTS Main, GenOpt;

(* 
   Generic optimizer
*)

IMPORT ParseParams;
IMPORT Debug;
IMPORT Pathname;
IMPORT Stdio;
FROM Fmt IMPORT F, LongReal, Int, Pad;
IMPORT Params;
IMPORT TextSeq;
IMPORT Thread;
IMPORT OSError;
IMPORT Scheme, SchemeM3;
IMPORT FS;
FROM SchemaGraph IMPORT ReadSchema, ReadData, EvalFormulas;
IMPORT Wr;
IMPORT Env;
IMPORT SchemeStubs;
IMPORT ReadLine, SchemeReadLine;
IMPORT NewUOAs;
IMPORT LRScalarField;
IMPORT LRScalarFieldPll;
IMPORT LRVector;
IMPORT ProcUtils;
IMPORT Rd;
IMPORT TextWr;
IMPORT FileWr;
IMPORT TextRd;
IMPORT OptVar;
IMPORT OptVarSeq;
IMPORT Wx;
IMPORT SchemeSymbol;
IMPORT LongRealSeq;
IMPORT OptCallback;
IMPORT Process;
IMPORT AL;
IMPORT SchemeObject;
IMPORT SchemeUtils;
IMPORT SchemeLongReal;
IMPORT TextTextTbl;

<*FATAL Thread.Alerted*>

VAR
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  NbOpts  := Env.Get("NBRUNOPTIONS");
  
CONST
  MyM3UtilsSrcPath = "spice/genopt/src";

VAR
  params := NEW(TextTextTbl.Default).init();
  
VAR
  vseq : OptVarSeq.T;
  rhoBeg, rhoEnd : LONGREAL;
  scmCb  : OptCallback.T;
  schemaPath : Pathname.T;
  schemaScmPaths : TextSeq.T;
  schemaDataFn : Pathname.T;
  schemaEval : SchemeObject.T;
  
VAR
  pMu := NEW(MUTEX);
  p : LongRealSeq.T;

  
PROCEDURE OptInit() =
  BEGIN
    vseq   := NEW(OptVarSeq.T).init();
    rhoBeg := FIRST(LONGREAL);
    rhoEnd := LAST(LONGREAL);
    p      := NEW(LongRealSeq.T).init();
    schemaPath := NIL;
    schemaScmPaths := NEW(TextSeq.T).init();
    schemaDataFn := NIL;
    schemaEval := NIL;
  END OptInit;

PROCEDURE DefSchemaPath(path : Pathname.T) =
  BEGIN schemaPath := path END DefSchemaPath;

PROCEDURE DefLoadScm(scmPath : Pathname.T) =
  BEGIN
    schemaScmPaths.addhi(scmPath)
  END DefLoadScm;

PROCEDURE DefDataFilename(fnm : Pathname.T) =
  BEGIN schemaDataFn := fnm END DefDataFilename;

PROCEDURE DefEval(obj : SchemeObject.T) =
  BEGIN schemaEval := obj END DefEval;
  
PROCEDURE DefOptVar(nm : SchemeSymbol.T; defval, defstep : LONGREAL) =
  BEGIN
    vseq.addlo(OptVar.T { SchemeSymbol.ToText(nm), defval, defstep });
    p.addlo(defval / defstep)
  END DefOptVar;

PROCEDURE SetRhoBeg(to : LONGREAL) =
  BEGIN
    rhoBeg := to
  END SetRhoBeg;

PROCEDURE SetRhoEnd(to : LONGREAL) =
  BEGIN
    rhoEnd := to
  END SetRhoEnd;

PROCEDURE GetCoords() : LongRealSeq.T =
  BEGIN
    RETURN p
  END GetCoords;

PROCEDURE SetCallback(obj : OptCallback.T) =
  BEGIN
    scmCb := obj
  END SetCallback;

PROCEDURE SetNetbatch(to : BOOLEAN) =
  BEGIN
    doNetbatch := to
  END SetNetbatch;

TYPE
  BaseEvaluator = LRScalarField.T OBJECT
  METHODS
    init()                      : LRScalarField.T := InitDummy;
  OVERRIDES
    eval     := BaseEval;
    evalHint := BaseEvalHint;
  END;

  PllEvaluator = LRScalarFieldPll.T OBJECT
  METHODS
    init() : LRScalarField.T := InitPll;
  END;

PROCEDURE InitDummy(base : BaseEvaluator) : LRScalarField.T =
  BEGIN
    RETURN base
  END InitDummy;
  
PROCEDURE InitPll(pll : PllEvaluator) : LRScalarField.T =
  BEGIN
    RETURN LRScalarFieldPll.T.init(pll, NEW(BaseEvaluator));
  END InitPll;

PROCEDURE BaseEvalHint(base : BaseEvaluator; p : LRVector.T) =
  BEGIN
    EVAL base.eval(p)
  END BaseEvalHint;

PROCEDURE FmtP(p : LRVector.T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      Wx.PutText(wx, LongReal(p[i]));
      Wx.PutChar(wx, ' ')
    END;
    RETURN Wx.ToText(wx)
  END FmtP;

PROCEDURE BaseEval(base : BaseEvaluator; p : LRVector.T) : LONGREAL =
  CONST
    MaxAttempts = 3;
  BEGIN
    FOR i := 1 TO MaxAttempts DO
      TRY
        RETURN AttemptEval(base, p)
      EXCEPT
        ProcUtils.ErrorExit, Rd.EndOfFile =>
        (* loop *)
        Thread.Pause(10.0d0)
      END
    END;
    Debug.Error("BaseEval : Too many attempts p=" & FmtP(p));
    RETURN 0.0d0
  END BaseEval;
  
PROCEDURE AttemptEval(base : BaseEvaluator; q : LRVector.T) : LONGREAL
  RAISES { Rd.EndOfFile, ProcUtils.ErrorExit } =
  VAR
    Owr            := NEW(TextWr.T).init();
    Ewr            := NEW(TextWr.T).init();
    stdout         := ProcUtils.WriteHere(Owr);
    stderr         := ProcUtils.WriteHere(Ewr);

    sdIdx          := NextIdx();
    subdirPath     := F("%s/%s",
                        rundirPath,
                        Pad(Int(sdIdx), 6, padChar := '0'));

    nbopts     : TEXT;
    cmd        : TEXT;
    cm         : ProcUtils.Completion;
  BEGIN

    IF NbOpts # NIL THEN
      nbopts := NbOpts
    ELSE
      nbopts := F("--target %s --class 4C --class SLES12", NbPool);
    END;

    LOCK pMu DO
      FOR i := FIRST(q^) TO LAST(q^) DO
        p.put(i, q[i])
      END;
      cmd            := scmCb.command()
    END;

    TRY
      FS.CreateDirectory(subdirPath);
    EXCEPT
      OSError.E(x) =>
      Debug.Warning(F("creating directory \"%s\" : OSError.E : %s",
                      subdirPath,
                      AL.Format(x)))
    END;
    
    WITH nbCmd = F("nbjob run %s --mode interactive %s",
                   nbopts,
                   cmd) DO
      VAR
        runCmd : TEXT;
      BEGIN
        IF doNetbatch THEN
          runCmd := nbCmd
        ELSE
          runCmd := cmd
        END;
        
        cm             := ProcUtils.RunText(runCmd,
                                            stdout := stdout,
                                            stderr := stderr,
                                            stdin  := NIL,
                                            wd0    := subdirPath)
      END
    END;
      
    TRY

      Debug.Out(F("BaseEval : q = %s\nrunning : %s", FmtP(q), cmd));
      
      cm.wait();

      WITH output = TextWr.ToText(Owr),
           rd     = TextRd.New(output) DO
        Debug.Out("Ran command output was " & output);

      END;

      (* read the schema *)
      WITH dataPath  = subdirPath & "/" & schemaDataFn,
           schemaRes = SchemaReadResult(schemaPath,
                                        dataPath,
                                        schemaScmPaths,
                                        schemaEval) DO
        Debug.Out("Schema-processed result : " & LongReal(schemaRes));
        RETURN schemaRes
      END
      
    EXCEPT
      ProcUtils.ErrorExit(err) =>
      WITH msg = F("command \"%s\" with output\n====>\n%s\n<====\n\nraised ErrorExit : %s",
                   cmd,
                   TextWr.ToText(Ewr),
                   ProcUtils.FormatError(err)) DO
        Debug.Warning(msg);
        RAISE ProcUtils.ErrorExit(err)
      END
    |
      Rd.EndOfFile =>
      Debug.Warning("Can't parse program output");
      RAISE Rd.EndOfFile
    END;
  END AttemptEval;

PROCEDURE DoIt() =
  VAR
    N               := vseq.size();
    pr : LRVector.T := NEW(LRVector.T, N);
    evaluator       := NEW(PllEvaluator).init();
  BEGIN
    IF rhoEnd = LAST(LONGREAL) THEN
      rhoEnd := rhoBeg / 1.0d-6
    END;

    FOR i := FIRST(pr^) TO LAST(pr^) DO
      pr[i] := 0.0d0
    END;

    Debug.Out(F("Ready to call Minimize : pr=%s rhoBeg=%s rhoEnd=%s",
                FmtP(pr), LongReal(rhoBeg), LongReal(rhoEnd)));
    
    WITH output = NewUOAs.Minimize(pr,
                                   evaluator,
                                   rhoBeg,
                                   rhoEnd,
                                   FIRST(LONGREAL)) DO
      Debug.Out(F("Minimize : %s iters, %s funcc; f = %s; %s",
                  Int(output.iterations),
                  Int(output.funcCount),
                  LongReal(output.f),
                  output.message));
      Wr.PutText(Stdio.stdout, FmtP(output.x));
      Wr.PutChar(Stdio.stdout, '\n');
      
      Wr.Flush(Stdio.stdout);
      WITH wr = FileWr.Open("genopt.opt"),
           cwr = FileWr.Open("genopt.cols") DO
        FOR i := vseq.size() - 1 TO 0 BY -1 DO
          WITH v = vseq.get(i),
               xi = output.x[i],
               pi = xi * v.defstep DO
            Wr.PutText(wr, LongReal(pi));
            Wr.PutText(cwr, v.nm);
            VAR c : CHAR; BEGIN
              IF i = 0 THEN c := '\n' ELSE c := ',' END;
              Wr.PutChar(wr, c);
              Wr.PutChar(cwr, c)
            END
          END
        END;
        Wr.Close(wr); Wr.Close(cwr)
      END
    END
  END DoIt;

VAR
  sdcnt        := 0;
  mu           := NEW(MUTEX);

PROCEDURE NextIdx() : CARDINAL =
  BEGIN
    LOCK mu DO
      TRY
        RETURN sdcnt
      FINALLY
        INC(sdcnt)
      END
    END
  END NextIdx;
  
PROCEDURE SchemaReadResult(schemaPath ,
                           dataPath : Pathname.T;
                           scmFiles : TextSeq.T;
                           schemaEval : SchemeObject.T) : LONGREAL =
  (* code from the schemaeval program *)
  VAR
    dataFiles := NEW(TextSeq.T).init();
    schemaScm : Scheme.T;
  BEGIN
    Debug.Out(F("SchemaReadResult : schemaPath %s , dataPath %s , eval %s",
                schemaPath, dataPath, SchemeUtils.Stringify(schemaEval)));
    
    dataFiles.addhi(dataPath);
    
    WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
      FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
        scmarr[i] := scmFiles.get(i)
      END;
      TRY
        schemaScm := NEW(SchemeM3.T).init(scmarr^)
      EXCEPT
        Scheme.E(x) =>
        Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
      END
    END;

    Debug.Out("SchemaReadResult : set up interpreter");
    
    TRY
      WITH schema = ReadSchema(schemaPath),
           data   = ReadData(schema, dataFiles) DO
        Debug.Out("SchemaReadResult : schema and data loaded");

        EvalFormulas(schemaScm, schema, data);

        Debug.Out("SchemaReadResult : formulas evaluated");

        WITH schemaRes = schemaScm.evalInGlobalEnv(schemaEval) DO
          Debug.Out("schema eval returned " & SchemeUtils.Stringify(schemaRes));
          RETURN SchemeLongReal.FromO(schemaRes)
        END
      END
    EXCEPT
      Scheme.E(x) =>
      Debug.Error("?error in Scheme interpreter : " & x);
      <*ASSERT FALSE*>
    END
  END SchemaReadResult;
  
VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  scm           : Scheme.T;
  rundirPath                := Process.GetWorkingDirectory();
  myFullSrcPath : Pathname.T;
  doNetbatch := TRUE;
  scmFiles := NEW(TextSeq.T).init();
  interactive : BOOLEAN;
  
BEGIN
  scmFiles.addhi("require");
  scmFiles.addhi("m3");
  TRY
    interactive := pp.keywordPresent("-i") OR pp.keywordPresent("-interactive");
    
    IF NbPool = NIL THEN
      Debug.Error("Must set NBPOOL env var.")
    END;
    IF NbQslot = NIL THEN
      Debug.Error("Must set NBQSLOT env var.")
    END;
    IF pp.keywordPresent("-m3utils") THEN
      M3Utils                   := pp.getNext()
    END;
    IF M3Utils = NIL THEN
      Debug.Error("? must specify M3UTILS---either on command line or in environment.")
    END;

    myFullSrcPath := M3Utils & "/" & MyM3UtilsSrcPath;
    scmFiles.addhi(myFullSrcPath & "/genopt.scm");
    
    WHILE pp.keywordPresent("-scminit") OR pp.keywordPresent("-S") OR pp.keywordPresent("-scm") DO
      scmFiles.addhi(pp.getNext())
    END;

    WHILE pp.keywordPresent("-setparam") DO
      WITH pnm  = pp.getNext(),
           pval = pp.getNext() DO
        Debug.Out(F("Overriding param %s <- %s", pnm, pval));
        EVAL params.put(pnm, pval)
      END
    END;

    pp.skipParsed();

    scmFiles.addhi(pp.getNext());

    pp.finish()
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & "\n" )
  END;

  WITH scmarr = NEW(REF ARRAY OF Pathname.T, scmFiles.size()) DO
    FOR i := FIRST(scmarr^) TO LAST(scmarr^) DO
      scmarr[i] := scmFiles.get(i)
    END;
    SchemeStubs.RegisterStubs();
    TRY
      scm := NEW(SchemeM3.T).init(scmarr^)
    EXCEPT
      Scheme.E(x) =>
      Debug.Error(F("EXCEPTION! Scheme.E \"%s\" when initializing interpreter", x))
    END
  END;

  IF interactive THEN
    SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
  ELSE
    DoIt()
  END
  
END Main.
