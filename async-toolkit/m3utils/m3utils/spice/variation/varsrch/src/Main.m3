MODULE Main;
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
FROM Math IMPORT sqrt, exp, tanh, atan, gamma, erfc, log;
IMPORT Wr;
IMPORT NewUOAs;
IMPORT Rd;
IMPORT TextRd;
IMPORT TextReader;
IMPORT Thread;
IMPORT Env;
IMPORT NormalDeviate;
IMPORT LongrealArraySort AS LRArraySort;
IMPORT Random;
IMPORT Histogram;
IMPORT Solve;
IMPORT LRFunction;
IMPORT FileWr;

TYPE
  T  = LRVector.T;
  TA = ARRAY OF TEXT;
  
CONST
  LR          =  LongReal;
  DefMult     = -1.0d0; (* we want to maximize the cycle time *)
  Sqrt10      =  3.1623d0;
  DefMaxSumSq =  5.3d0; (* \approx sqrt(2) * erf^-1 ( 1 - 1/(10 * 1e6) ) *)
  DefRhoBeg   =  4.0d0;
  N           =  4; (* dimensions *)
  
VAR
  NbPool  := Env.Get("NBPOOL");
  NbQslot := Env.Get("NBQSLOT");
  M3Utils := Env.Get("M3UTILS");

  Pi      := 4.0d0 * atan(1.0d0);
  Ln2     := log(2.0d0);
  Sqrt2   := sqrt(2.0d0);
  
TYPE
  BaseEvaluator = LRScalarField.T OBJECT
    maxSumSq    : LONGREAL;
    mult     := DefMult;
  METHODS
    init() : LRScalarField.T := InitDummy;
    mapP(inPlaceP : LRVector.T) : LRVector.T := BaseMapP;
  OVERRIDES
    eval     := BaseEval;
    evalHint := BaseEvalHint;
  END;

  PllEvaluator = LRScalarFieldPll.T OBJECT
    maxSumSq : LONGREAL;
  METHODS
    init() : LRScalarField.T := InitPll;
  END;

PROCEDURE VRef(READONLY a : ARRAY OF LONGREAL) : LRVector.T =
  VAR
    res := NEW(LRVector.T, NUMBER(a));
  BEGIN
    res^ := a;
    RETURN res
  END VRef;
  
PROCEDURE BaseMapP(base : BaseEvaluator;
                   pa   : LRVector.T) : LRVector.T =
  VAR
    sumSq := 0.0d0;
    p := NEW(LRVector.T, NUMBER(pa^));
  BEGIN
    p^ := pa^;
    
    FOR i := FIRST(p^) TO LAST(p^) DO
      sumSq := sumSq + p[i] * p[i]
    END;

    IF sumSq > base.maxSumSq THEN
      WITH ratio = sqrt(base.maxSumSq / sumSq) DO
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
    RETURN LRScalarFieldPll.T.init(pll,
                                   NEW(BaseEvaluator,
                                       maxSumSq := pll.maxSumSq));
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
  
PROCEDURE AttemptEval(base : BaseEvaluator; q : LRVector.T) : LONGREAL =
  CONST
    Fudge = 0.2d0; (* don't ask... *)
  VAR
    pos            := FmtP(q);
    sumSqQ         := SumSq(q);
    rmsQ           := sqrt(sumSqQ);

    (* this is a function that peaks at around maxSumSq and falls off
       slowly to the left and quickly to the right *)
    corr           := exp((rmsQ - base.maxSumSq) / base.maxSumSq) *
                      (tanh(10.0d0 * (base.maxSumSq + Fudge - rmsQ)) + 1.0d0) /
                      2.0d0;
        

    x              := q[0];
    y              := q[1];
    fval           := -y / (x*x + 1.0d0);

    res            := fval * corr;
  BEGIN
    Debug.Out(FN("AttemptEval : x=%s y=%s ; maxSumSq=%s corr=%s fval=%s res=%s",
                TA{LR(x), LR(y), LR(base.maxSumSq), LR(corr), LR(fval), LR(res)}));

    RETURN res
  END AttemptEval;

VAR RhoBeg := FIRST(LONGREAL); (* set in the main body *)

PROCEDURE CaptureSamples(r     : Random.T;
                         e     : LRScalarField.T;
                         VAR s : ARRAY OF LONGREAL) =
  VAR
    p := NEW(REF ARRAY OF ARRAY OF LONGREAL, NUMBER(s), N);

  BEGIN
    (* fill s with samples across the dimensions *)
    FOR i := FIRST(s) TO LAST(s) DO
      FOR j := 0 TO N - 1 DO
        p[i, j] := NormalDeviate.Get(r, 0.0d0, 1.0d0)
      END
    END;

    FOR i := FIRST(s) TO LAST(s) DO
      e.evalHint(VRef(p[i]))
    END;
    
    FOR i := FIRST(s) TO LAST(s) DO
      s[i] := e.eval(VRef(p[i]))
    END;

    LRArraySort.Sort(s)
  END CaptureSamples;

TYPE
  Stats = RECORD
    mu, sigma, median : LONGREAL;
  END;

PROCEDURE StatSorted(READONLY a : ARRAY OF LONGREAL) : Stats =
  VAR
    sum, sumsq := 0.0d0;
    res : Stats;
    nf := FLOAT(NUMBER(a), LONGREAL);
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      sum   := sum + a[i];
      sumsq := sumsq + a[i] * a[i]
    END;

    res.mu := sum / nf;
    WITH sampVar = sumsq / nf - res.mu * res.mu DO
      res.sigma := sqrt(nf / (nf - 1.0d0) * sampVar)
    END;

    WITH l = NUMBER(a) DIV 2 DO
      IF NUMBER(a) MOD 2 = 0 THEN
        res.median := 0.5d0 * (a[l] + a[l + 1])
      ELSE
        res.median := a[l]
      END
    END;

    RETURN res
  END StatSorted;

CONST
  Stages    = 10;
  Instances = 1000 * 1000;

TYPE
  Erfc = LRFunction.T OBJECT
    tgt : LONGREAL;
  OVERRIDES
    eval := EvalErfc;
  END;

PROCEDURE EvalErfc(self : Erfc; x : LONGREAL) : LONGREAL =
  BEGIN
    RETURN erfc(x) - self.tgt
  END EvalErfc;

PROCEDURE Erfcn(s : LONGREAL) : LONGREAL =
  VAR
    res : LONGREAL;
  BEGIN
    Debug.Out(F("Erfcn(%s)", LR(s)));
    <*ASSERT s >= 0.0d0*>
    <*ASSERT s <= 2.0d0*>
    IF    s = 2.0d0 THEN res := FIRST(LONGREAL)
    ELSIF s = 0.0d0 THEN res := LAST(LONGREAL)
    ELSE
      res := Solve.WDB(NEW(Erfc, tgt := s), -4.0d0, +10.0d0, tol := s * 0.0001d0)
    END;
    Debug.Out(F("Erfcn(%s)=%s", LR(s), LR(res)));
    RETURN res
  END Erfcn;
  
PROCEDURE DoIt() =
  VAR
    pr : LRVector.T := NEW(T, N);
    evaluator       := NEW(PllEvaluator, maxSumSq := maxSumSq).init();

    s               := NEW(REF ARRAY OF LONGREAL, startSamples);
    stats           : Stats;

    K               := TargetK(1.0d6); (* target for 10^6 samples *)

    target          : LONGREAL;
  CONST
    RhoEnd = 1.0d-6;
  BEGIN

    WITH r    = NEW(Random.Default).init(),
         dlev = Debug.GetLevel() DO
      TRY
        Debug.SetLevel(0);
        CaptureSamples(r, evaluator, s^);
      FINALLY
        Debug.SetLevel(dlev)
      END
    END;
    stats := StatSorted(s^);

    Histogram.Do("s", s^, TRUE, H := 50);

    Debug.Out(F("mu=%s median=%s sigma=%s",
                LR(stats.mu), LR(stats.median), LR(stats.sigma)));


    target := stats.mu + K * stats.sigma;
    
    Debug.Out(F("K=%s, target=%s", LR(K), LR(target)));
    
    pr^ := ARRAY [ 0 .. N-1 ] OF LONGREAL { 0.0d0, .. };
    
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
    evaluator       := NEW(BaseEvaluator, maxSumSq := maxSumSq).init();
  BEGIN

    pr^ := ARRAY [ 0 .. N-1 ] OF LONGREAL { 0.0d0, .. };

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

PROCEDURE TargetK(nf : LONGREAL) : LONGREAL =
  (* target for normal distribution *)
  BEGIN
    RETURN Sqrt2 * Erfcn(2.0d0 / nf * Ln2)
  END TargetK;
  
CONST
  single = TRUE;

VAR
  pp       := NEW(ParseParams.T).init(Stdio.stderr);

  maxSumSq     : LONGREAL;
  MaxSumSq     := DefMaxSumSq;

  startSamples := 1000;
  
BEGIN
  TRY
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

  RhoBeg := DefRhoBeg * maxSumSq / DefMaxSumSq + 0.01d0;

  Debug.Out("RhoBeg=" & LongReal(RhoBeg));

  VAR
    wr := FileWr.Open("erfc.dat");
  BEGIN
    FOR n := -100 TO 100 DO
      WITH nf = FLOAT(n, LONGREAL) / 10.0d0 DO
        Wr.PutText(wr, F("%s %s\n", LR(nf), LR(erfc(nf))))
      END
    END;
    Wr.Close(wr)
  END;
  
  VAR
    nf := 1.0d0;
    wr := FileWr.Open("targetK.dat");
  BEGIN
    FOR i := 0 TO 65 DO
      Wr.PutText(wr, F("%s %s\n", LR(nf), LR(TargetK(nf))));
      nf := nf * Sqrt2
    END;
    Wr.Close(wr)
  END;
  
  DoIt()
END Main.
