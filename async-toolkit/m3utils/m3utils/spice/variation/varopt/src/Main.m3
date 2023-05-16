MODULE Main;
FROM RingOsc IMPORT N;
IMPORT LRScalarField;
IMPORT Wx;
FROM Fmt IMPORT Int, LongReal, F, Pad;
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
IMPORT Math;
IMPORT Wr;
IMPORT NewUOAs;
IMPORT Rd;
IMPORT TextRd;

TYPE
  T = LRVector.T;

CONST
  DefMult     = -1.0d0; (* we want to maximize the cycle time *)
  DefMaxSumSq =  5.3d0; (* \approx sqrt(2) * erf^-1 ( 1 - 1/(10 * 1e6) ) *)
  
TYPE
  BaseEvaluator = LRScalarField.T OBJECT
    mult     := DefMult;
    maxSumSq := DefMaxSumSq;
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
      WITH ratio = Math.sqrt(base.maxSumSq / sumSq) DO
        FOR i := FIRST(p^) TO LAST(p^) DO
          p[i] := p[i] * ratio
        END
      END
    END;

    RETURN p
  END BaseMapP;

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
  VAR
    q              := base.mapP(p);
    pos            := FmtP(q);
    
    bin            := VarSpiceBin;

    Owr            := NEW(TextWr.T).init();
    Ewr            := NEW(TextWr.T).init();
    stdout         := ProcUtils.WriteHere(Owr);
    stderr         := ProcUtils.WriteHere(Ewr);

    sdIdx          := NextIdx();
    subdirPath     := F("%s/%s",
                        rundirPath,
                        Pad(Int(sdIdx), 6, padChar := '0'));
                        
    cmd            := F("nbjob run --class 4C --mode interactive %s -T %s -r %s %s", bin, templatePath, subdirPath, pos);
    cm             := ProcUtils.RunText(cmd,
                                        stdout := stdout,
                                        stderr := stderr,
                                        stdin  := NIL);
  BEGIN
    TRY

      Debug.Out(F("BaseEval : p = %s\nrunning : %s", FmtP(p), cmd));
      
      cm.wait();

      WITH output = TextWr.ToText(Owr),
           rd     = TextRd.New(output) DO
        Debug.Out("Ran command output was " & output);

        LOOP
          TRY
            WITH line = Rd.GetLine(rd) DO
              WITH res = base.mult * Scan.LongReal(line) DO
                Debug.Out(F("BaseEval: f(%s) = %s", FmtP(q), LongReal(res)));
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
        Debug.Error(msg)
      END
    |
      Rd.EndOfFile =>
      Debug.Error("Can't parse variation output")
    END;
    <*ASSERT FALSE*>
  END BaseEval;

CONST
  VarSpiceBin = "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/variation/varspice/AMD64_LINUX/vary";

PROCEDURE DoIt() =
  VAR
    pr : LRVector.T := NEW(T, N);
    evaluator := NEW(PllEvaluator).init();
  CONST
    RhoBeg = 1.0d+0;
    RhoEnd = 1.0d-6;
  BEGIN
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
  templatePath : Pathname.T;
  rundirPath   : Pathname.T;
  pp       := NEW(ParseParams.T).init(Stdio.stderr);
BEGIN
  TRY
    IF pp.keywordPresent("-T") OR pp.keywordPresent("-template") THEN
      templatePath := pp.getNext()
    END;
    
    IF pp.keywordPresent("-r") THEN
      rundirPath := pp.getNext()
    END;
    pp.skipParsed();
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command line")
  END;
  
  IF templatePath = NIL OR rundirPath = NIL THEN
    Debug.Error("Must specify template [-T] and rundir [-r]")
  END;

  TRY
    FS.CreateDirectory(rundirPath)
  EXCEPT
    OSError.E(x) => Debug.Error("FS.CreateDirectory : caught error : OSError.E : " & AL.Format(x))
  END;
    
  DoIt()
END Main.
