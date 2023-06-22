MODULE Main;
FROM XorRingOsc IMPORT NV;
IMPORT LRScalarField;
IMPORT Wx;
FROM Fmt IMPORT Int, LongReal, F, FN, Pad;
IMPORT LRVector;
IMPORT ProcUtils;
IMPORT Debug;
IMPORT Scan;
IMPORT TextWr;
IMPORT FloatMode, Lex;
IMPORT LRScalarFieldPll;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Pathname;
IMPORT OSError;
IMPORT FS;
IMPORT AL;
FROM Math IMPORT sqrt, exp, tanh;
IMPORT Wr;
IMPORT NewUOAs;
IMPORT Rd;
IMPORT TextRd;
IMPORT TextReader;
IMPORT Thread;
FROM TechConfig IMPORT Tran, TranNames;
FROM TechLookup IMPORT Lookup;
IMPORT Env;

TYPE
  T = LRVector.T;

CONST
  DefMult     = -1.0d0; (* we want to maximize the cycle time *)
  Sqrt10      =  3.1623d0;
  DefMaxSumSq =  5.3d0; (* \approx sqrt(2) * erf^-1 ( 1 - 1/(10 * 1e6) ) *)
  DefRhoBeg   =  4.0d0;
  N           =  NV * 2; (* variations per gate times 2 gates *)

VAR
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");
  
TYPE
  BaseEvaluator = LRScalarField.T OBJECT
    mult     := DefMult;
  METHODS
    init() : LRScalarField.T := InitDummy;
    mapP(inPlaceP : LRVector.T) : LRVector.T := BaseMapP;
  OVERRIDES
    eval     := BaseEval;
    evalHint := BaseEvalHint;
  END;

  PllEvaluator = LRScalarFieldPll.T OBJECT
  METHODS
    init() : LRScalarField.T := InitPll;
  END;

PROCEDURE BaseMapP(<*UNUSED*>base : BaseEvaluator;
                   pa   : LRVector.T) : LRVector.T =
  VAR
    sumSq := 0.0d0;
    p := NEW(LRVector.T, NUMBER(pa^));
  BEGIN
    p^ := pa^;
    
    FOR i := FIRST(p^) TO LAST(p^) DO
      sumSq := sumSq + p[i] * p[i]
    END;

    IF sumSq > maxSumSq THEN
      WITH ratio = sqrt(maxSumSq / sumSq) DO
        FOR i := FIRST(p^) TO LAST(p^) DO
          p[i] := p[i] * ratio
        END
      END
    END;

    RETURN p
  END BaseMapP;

PROCEDURE SumSq(p : LRVector.T) : LONGREAL =
  VAR
    sumSq := 0.0d0;
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      sumSq := sumSq + p[i] * p[i]
    END;
    RETURN sumSq
  END SumSq;

PROCEDURE InitDummy(base : BaseEvaluator) : LRScalarField.T =
  BEGIN
    RETURN base
  END InitDummy;
  
PROCEDURE InitPll(pll : PllEvaluator) : LRScalarField.T =
  BEGIN
    RETURN LRScalarFieldPll.T.init(pll, NEW(BaseEvaluator));
  END InitPll;
  
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

PROCEDURE BaseEvalHint(base : BaseEvaluator; p : LRVector.T) =
  BEGIN
    EVAL base.eval(p)
  END BaseEvalHint;
  
PROCEDURE BaseEval(base : BaseEvaluator; p : LRVector.T) : LONGREAL =
  CONST
    MaxAttempts = 500;
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
    <*ASSERT FALSE*>
  END BaseEval;
  
PROCEDURE AttemptEval(base : BaseEvaluator; q : LRVector.T) : LONGREAL
  RAISES { Rd.EndOfFile, ProcUtils.ErrorExit } =
  CONST
    Fudge = 0.2d0; (* don't ask... *)
  VAR
    pos            := FmtP(q);
    sumSqQ         := SumSq(q);
    rmsQ           := sqrt(sumSqQ);

    (* this is a function that peaks at around maxSumSq and falls off
       slowly to the left and quickly to the right *)
    corr           := exp((rmsQ - maxSumSq) / maxSumSq) *
                      (tanh(10.0d0 * (maxSumSq + Fudge - rmsQ)) + 1.0d0) /
                      2.0d0;
        
        
    bin            := M3Utils & "/" & VarSpiceBin;

    Owr            := NEW(TextWr.T).init();
    Ewr            := NEW(TextWr.T).init();
    stdout         := ProcUtils.WriteHere(Owr);
    stderr         := ProcUtils.WriteHere(Ewr);

    sdIdx          := NextIdx();
    subdirPath     := F("%s/%s",
                        rundirPath,
                        Pad(Int(sdIdx), 6, padChar := '0'));

    tranStr        := "-thresh " & TranNames[tran];
    zStr           := "-z " & Int(z);

    opt            := ARRAY BOOLEAN OF TEXT { "", "-single" } [ single ];
                        
    cmd            := FN("nbjob run --target %s --class 4C --class SLES12 --mode interactive %s %s %s %s -T %s -r %s %s", ARRAY OF TEXT { NbPool, bin, opt, tranStr, zStr, templatePath, subdirPath, pos } );
    cm             := ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL);
  BEGIN
    TRY

      Debug.Out(F("BaseEval : q = %s\nrunning : %s", FmtP(q), cmd));
      
      cm.wait();

      WITH output = TextWr.ToText(Owr),
           rd     = TextRd.New(output) DO
        Debug.Out("Ran command output was " & output);

        LOOP
          TRY
            WITH line = Rd.GetLine(rd) DO
              WITH progVal = Scan.LongReal(line),
                   res     = corr * progVal * base.mult DO
                Debug.Out(F("BaseEval: progVal=%s corr=%s base.mult=%s -> f(%s) = %s",
                            LongReal(progVal),
                            LongReal(corr),
                            LongReal(base.mult),
                            FmtP(q),
                            LongReal(res)));
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
      Debug.Warning("Can't parse variation output");
      RAISE Rd.EndOfFile
    END;
  END AttemptEval;

CONST
  VarSpiceBin = "spice/variation/varspice/AMD64_LINUX/vary";

VAR RhoBeg := FIRST(LONGREAL); (* set in the main body *)
    
PROCEDURE DoIt() =
  VAR
    pr : LRVector.T := NEW(T, N);
    evaluator       := NEW(PllEvaluator).init();
  CONST
    RhoEnd = 1.0d-6;
  BEGIN
    IF FALSE THEN
      (* *)
    ELSIF single AND doSkip THEN
      ParseResult(pr^, ResultSingle[z][tran]) 
    ELSIF doSkip THEN
      <*ASSERT FALSE*>
    ELSE
      pr^ := ARRAY [ 0 .. N-1 ] OF LONGREAL { 0.0d0, .. };
    END;
    
    WITH output = NewUOAs.Minimize(pr,
                                   evaluator,
                                   RhoBeg,
                                   RhoEnd,
                                   FIRST(LONGREAL)) DO
      Debug.Out(F("Minimize : %s iters, %s funcc; f = %s; %s",
                  Int(output.iterations),
                  Int(output.funcCount),
                  LongReal(output.f),
                  output.message));
      Wr.PutText(Stdio.stdout, FmtP(output.x))
    END
  END DoIt;

PROCEDURE DoFirstOnly() =
  VAR
    pr : LRVector.T := NEW(T, N);
    evaluator       := NEW(BaseEvaluator).init();
  BEGIN

    IF FALSE THEN
      (* *)
    ELSIF single AND doSkip THEN
      ParseResult(pr^, ResultSingle[z][tran]) 
    ELSIF doSkip THEN
      <*ASSERT FALSE*>
    ELSE
      pr^ := ARRAY [ 0 .. N-1 ] OF LONGREAL { 0.0d0, .. };
    END;
    WITH output = evaluator.eval(pr) DO
      Debug.Out(F("DoFirstOnly : f = %s", LongReal(output)));
      Wr.PutText(Stdio.stdout, FmtP(pr))
    END
  END DoFirstOnly;
  
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

PROCEDURE ParseResult(VAR tgt : ARRAY OF LONGREAL; str : TEXT) =
  VAR
    reader := NEW(TextReader.T).init(str);
    shatter := reader.shatter(" ", "", skipNulls := TRUE);
    p := shatter;
  BEGIN
    FOR i := FIRST(tgt) TO LAST(tgt) DO
      tgt[i] := Scan.LongReal(p.head);
      p := p.tail
    END
  END ParseResult;
  
CONST (* shortcut values *)
  ResultSingle = ARRAY [ 1 .. 2 ] OF ARRAY Tran OF TEXT {
  ResultZ1Single,
  ResultZ2Single
  };

  ResultZ1Single = ARRAY Tran OF TEXT {
  NIL,
  "0 0 0 0 0 0 0 0 0.0625 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.00048828125 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.0625 -0.03125 0 0 0 0 0 0 -0.125 0.375 1.75 0 5 0 -0.015625 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
  NIL,
  "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.000518798828125 0 0 0 0 0 0 0 0 0 0.015625 0.0625 0 0 0 0 0 0 0 0 0 0 0 0 -0.0009765625 0 0 0 0 0 0.00390625 0 0 0 0.015625 0 0 0 0 0.1875 1.625 0 5.0625 0 0.0000152587890625 0 0 0 0 0 0 0 0 0 0 0.00091552734375 0 0 0",
  NIL,
  NIL,
  NIL
  };

  ResultZ2Single = ARRAY Tran OF TEXT {
  NIL,
  "0 0 0 0 0 0 0 0 0 0 0 0.125 0 0 0 0 -0.015625 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1.75 0 5.0001220703125 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
  NIL,
  "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.00006103515625 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.0001220703125 0 0 0 0 0 0 0 -0.000030517578125 0 0 0 0 0 0 0 0.0078125 0 0 0 0 0 0 0 0 0 0 0.125 1.75 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
  NIL,
  NIL,
  NIL
  };

CONST
  single = TRUE;

VAR
  templatePath : Pathname.T;
  rundirPath   : Pathname.T;
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
  doSkip       : BOOLEAN;
  firstOnly    : BOOLEAN;

  tran         := Tran.Ulvt;
  maxSumSq     : LONGREAL;
  z            : CARDINAL;
  MaxSumSq     := DefMaxSumSq;
  
BEGIN
  IF NbPool = NIL THEN
    Debug.Error("Must set NBPOOL env var.")
  END;
  IF NbQslot = NIL THEN
    Debug.Error("Must set NBQSLOT env var.")
  END;
  IF M3Utils = NIL THEN
    Debug.Error("Must set M3UTILS env var.")
  END;
  
  TRY
    IF pp.keywordPresent("-T") OR pp.keywordPresent("-template") THEN
      templatePath := pp.getNext()
    END;
    
    IF pp.keywordPresent("-r") THEN
      rundirPath := pp.getNext()
    END;
    
    IF pp.keywordPresent("-thresh") THEN
      tran := VAL(Lookup(pp.getNext(), TranNames), Tran)
    ELSE
      Debug.Error("Must provide -thresh")
    END;

    doSkip := pp.keywordPresent("-skip");

    firstOnly := pp.keywordPresent("-firstonly");

    IF pp.keywordPresent("-z") THEN
      z := pp.getNextInt()
    ELSE
      Debug.Error("Must provide -z")
    END;

    IF pp.keywordPresent("-sumsq") THEN
      MaxSumSq := pp.getNextLongReal()
    END;

    pp.skipParsed();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;

  IF single THEN
    maxSumSq := MaxSumSq
  ELSE
    maxSumSq := MaxSumSq / Sqrt10
  END;

  RhoBeg := DefRhoBeg * maxSumSq / DefMaxSumSq;

  Debug.Out("RhoBeg=" & LongReal(RhoBeg));
  
  IF templatePath = NIL OR rundirPath = NIL THEN
    Debug.Error("Must specify template [-T] and rundir [-r]")
  END;

  TRY
    FS.CreateDirectory(rundirPath)
  EXCEPT
    OSError.E(x) => Debug.Error("FS.CreateDirectory : caught error : OSError.E : " & AL.Format(x))
  END;

  IF firstOnly THEN
    DoFirstOnly()
  ELSE
    DoIt()
  END
END Main.
