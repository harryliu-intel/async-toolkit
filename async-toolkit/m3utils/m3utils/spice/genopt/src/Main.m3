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
FROM SchemaGraph IMPORT ReadSchema, ReadData, EvalFormulas, DoSweeps;
IMPORT Wr;
IMPORT SchemeString;
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
IMPORT Lex, FloatMode;
IMPORT Scan;
IMPORT FileWr;
IMPORT TextRd;
IMPORT OptVar;
IMPORT OptVarSeq;
IMPORT Wx;

VAR
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  NbOpts  := Env.Get("NBRUNOPTIONS");
  
CONST
  MyM3UtilsSrcPath = "spice/genopt/src";

VAR
  vseq := NEW(OptVarSeq.T).init();
  rhoBeg := FIRST(LONGREAL);
  rhoEnd := LAST(LONGREAL);
  
PROCEDURE DefOptVar(nm : TEXT; defval, defstep : LONGREAL) =
  BEGIN
    vseq.addhi(OptVar.T { nm, defval, defstep })
  END DefOptVar;

PROCEDURE SetRhoBeg(to : LONGREAL) =
  BEGIN
    rhoBeg := to
  END SetRhoBeg;

PROCEDURE SetRhoEnd(to : LONGREAL) =
  BEGIN
    rhoEnd := to
  END SetRhoEnd;

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
    
    cmd            := "";

    cm             := ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL);

    TRY

      Debug.Out(F("BaseEval : q = %s\nrunning : %s", FmtP(q), cmd));
      
      cm.wait();

      WITH output = TextWr.ToText(Owr),
           rd     = TextRd.New(output) DO
        Debug.Out("Ran command output was " & output);

        LOOP
          TRY
            WITH line = Rd.GetLine(rd) DO
              WITH res = Scan.LongReal(line) DO

                WITH wr = FileWr.Open(subdirPath & "/compres.dat") DO
                  Wr.PutText(wr, F("%s %s\n", LongReal(res), subdirPath));
                  Wr.Close(wr)
                END;
                
                RETURN res
              END
            END
          EXCEPT
            FloatMode.Trap, Lex.Error => (* skip *)
          END
        END
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
      Wr.PutText(Stdio.stdout, FmtP(output.x))
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

VAR
  pp                        := NEW(ParseParams.T).init(Stdio.stderr);
  schemaFn      : Pathname.T;
  dataFiles                 := NEW(TextSeq.T).init();
  scmFiles                  := NEW(TextSeq.T).init();
  scm           : Scheme.T;
  toEval        : TEXT;
  rundirPath                := ".";
  myFullSrcPath : Pathname.T;
BEGIN
  scmFiles.addhi("require");
  scmFiles.addhi("m3");
  TRY
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

  TRY
    SchemeReadLine.MainLoop(NEW(ReadLine.Default).init(), scm)
  EXCEPT
    Scheme.E(x) =>
    Debug.Error("?error in Scheme interpreter : " & x)
  END
  
END Main.
