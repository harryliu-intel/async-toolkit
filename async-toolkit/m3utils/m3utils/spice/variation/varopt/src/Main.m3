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
IMPORT TextReader;
IMPORT Thread;

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
  
PROCEDURE AttemptEval(base : BaseEvaluator; p : LRVector.T) : LONGREAL
  RAISES { Rd.EndOfFile, ProcUtils.ErrorExit } =
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
  VarSpiceBin = "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/variation/varspice/AMD64_LINUX/vary";

PROCEDURE DoIt() =
  VAR
    pr : LRVector.T := NEW(T, N);
    evaluator := NEW(PllEvaluator).init();
  CONST
    RhoBeg = 1.0d+0;
    RhoEnd = 1.0d-6;
  BEGIN
    IF TRUE THEN
      ParseResult(pr^, Result1)
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
  
CONST Result1 = "-3.388583488155353e-14 -6.865901067393858e-14 0 -1.3833178080301372e-14 1.0120781901267004e-14 -2.1334215236620252e-14 0 -1.0388289652191492e-13 0.001933585122473632 0.003867170245053995 0 0.03093736196221466 1.1208148855280724e-13 -1.7480788315586293e-14 0 0.2010928527542289 -2.442198221684334e-13 -9.369285217803131e-14 0 -1.1047683915347033e-13 1.003416204379963e-13 4.847448469443447e-14 0 -4.349041543877273e-14 1.831156605346575e-13 -9.21053707805858e-14 0 -1.351661350555897e-14 -4.7123773539441876e-14 3.629109645928372e-14 0 1.305768182843192e-15 2.3286871891017447e-13 -4.7846776807222406e-14 0 0.9899955827896085 -5.547986423110827e-14 -3.167219306160521e-13 0 -6.653777942415295e-14 -3.5459358549164147e-14 -9.506720675995244e-14 0 0.2474988956974257 5.003910510863805e-14 1.2858167365786465e-13 0 -2.1598458848462872e-13 -1.153179491200635e-13 1.0342177103626273e-13 0 7.140589494267514e-14 -1.4177568174391415e-13 -1.3094508127884382e-15 0 -0.00003021226766526162 2.6931077640618436e-14 -1.783535089815597e-14 0 1.0111800247965439e-13 0.22757689457257885 0.49499779139478184 0 1.9799911655787394 1.389785036557078e-13 -1.5552825118558536e-13 0 1.3843093922218177e-14 -8.687236704106671e-16 -9.153961876803676e-14 0 0.0077343404906152205 -5.2564061946123633e-14 1.4115839291619902e-13 0 5.3789118671221604e-14 -8.793575270734838e-14 -5.795059797796883e-14 0 1.457266981458238e-13";
  
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
