MODULE Main;

(* linear regression for characterizing lambs *)

IMPORT ParseParams;
IMPORT FS;
IMPORT Debug;
IMPORT Pathname;
IMPORT Stdio;
IMPORT RegEx;
FROM Fmt IMPORT Int, LongReal, F;
IMPORT Text;
IMPORT Scan;
<*NOWARN*>IMPORT Process;
IMPORT RefList;
IMPORT FileRd;
IMPORT Rd;
IMPORT TextReader;
IMPORT LRRegression AS Regression;
IMPORT LRMatrix2 AS Matrix;
IMPORT TextRefTbl;
IMPORT Wr, FileWr;
IMPORT Thread;
IMPORT FloatMode, Lex;

CONST LR = LongReal;
      TE = Text.Equal;

<*FATAL Thread.Alerted*>   

<*FATAL RegEx.Error*>

VAR
  fsIter : FS.Iterator;
  workDir : Pathname.T;
  pp := NEW(ParseParams.T).init(Stdio.stderr);

  pat := RegEx.Compile("\\.avg$");

  path : Pathname.T;
  vdd := FIRST(LONGREAL);

VAR
  depthPat := RegEx.Compile("_\\([0-9][0-9]*\\)d");
  widthPat := RegEx.Compile("\\([0-9][0-9]*\\)b");
  speedPat := RegEx.Compile("\\([0-9][0-9]*e[0-9][0-9]*\\)");
  progPat  := RegEx.Compile("_\\([a-z][a-z]*\\)_");

CONST
  BlockSize = 16;

PROCEDURE ExtractMatch(text : TEXT;
                       pat  : RegEx.Pattern) : TEXT =
  VAR
    mem := NEW(REF RegEx.Memory);
  BEGIN
    WITH res = RegEx.Execute(pat, text, mem := mem) DO
      IF res = -1 THEN
        Debug.Error("No match in " & text & " for " & RegEx.Decompile(pat))
      END;

     WITH start = mem[1].start,
          stop  = mem[1].stop,
          sub   = Text.Sub(text,
                           start, 
                           stop - start) DO
       Debug.Out(F("text %s res %s start %s stop %s sub %s",
                   text,
                   Int(res), 
                   Int(start),
                   Int(stop),
                   sub),
                 1000);
       RETURN sub
     END
    END
  END ExtractMatch;

TYPE
  Result = RECORD
    fn           : Pathname.T;
    depth, width : CARDINAL;
    speed        : LONGREAL;
    prog         : TEXT;
    power        : LONGREAL
  END;

PROCEDURE Predict(prog         : TEXT;
                  depth, width : CARDINAL;
                  speed        : LONGREAL) : Prediction =
  VAR
    ref : REFANY;
    x   : X;
    res : Prediction;
  BEGIN
    IF NOT models.get(prog, ref) THEN
      Debug.Error("No model for program " & prog)
    END;

    WITH model = NARROW(ref, Model) DO
      MakeIndeps(Result { depth := depth,
                          width := width,
                          
                         fn := NIL,
                         speed := FIRST(LONGREAL),
                         prog := NIL, 
                         power := FIRST(LONGREAL) },
                 x);

      WITH totAtRef    = Mul(x, model.totP^),
           noLeakAtRef = Mul(x, model.noLeakP^),
           leak        = totAtRef - noLeakAtRef,

           (* hack! *)
           (*leak  = x[3] * (model.totP[3, 0] - model.noLeakP[3, 0]),*)
           
           noLeakAtSpd = speed / model.refFreq * noLeakAtRef,
           totAtSpd    = noLeakAtSpd + leak DO
        res.predP := totAtSpd;
        res.predLeakP := leak;
        res.noLeakP    := noLeakAtSpd;
        FOR i := FIRST(x) TO LAST(x) DO
          res.contribs[i] := model.noLeakP[i, 0] * x[i] / noLeakAtRef
        END
      END
    END;
    RETURN res
  END Predict;

PROCEDURE Mul(READONLY x : X; READONLY m : Matrix.M) : LONGREAL =
  VAR
    sum := 0.0d0;
  BEGIN
    FOR i := FIRST(x) TO LAST(x) DO
      sum := sum + x[i] * m[i, 0]
    END;
    RETURN sum
  END Mul;

TYPE
  Prediction = RECORD
    predP, predLeakP, noLeakP : LONGREAL;
    contribs         : X;
  END;
  
VAR
  results : RefList.T := NIL;
  
PROCEDURE DoOneFile(fn : Pathname.T) =
  <*FATAL FloatMode.Trap, Lex.Error*>
  VAR
    depth := Scan.Int(ExtractMatch(fn, depthPat));
    width := Scan.Int(ExtractMatch(fn, widthPat));
    speed := Scan.LongReal(ExtractMatch(fn, speedPat));
    prog  := ExtractMatch(fn, progPat);
  BEGIN
    WITH rd      = FileRd.Open(workDir & "/" & fn),
         line    = Rd.GetLine(rd),
         reader  = NEW(TextReader.T).init(line),
         <*UNUSED*>w1      = reader.nextE(" \t", skipNulls := TRUE),
         w2      = reader.nextE(" \t", skipNulls := TRUE),
         current = -Scan.LongReal(w2),
         power   = vdd * current DO
      (* format is that w2 is the average current *)
      results := RefList.Cons(
                     NEW(REF Result,
                         fn    := fn,
                         depth := depth,
                         width := width,
                         speed := speed,
                         prog  := prog,
                         power := power),
                     results);
      Debug.Out(F("DoOneFile(%s) depth %s width %s speed %s prog %s",
                  fn, Int(depth), Int(width), LR(speed), prog)&
                  F(" power %s", LR(power)));
    END
  END DoOneFile;

TYPE
  Condition = RECORD
    prog  : TEXT;
    speed : LONGREAL;
  END;

  Array = ARRAY OF ARRAY OF LONGREAL;

PROCEDURE DoOneEquation(READONLY cond : Condition) : REF Matrix.M =
  VAR
    p := results;
    m : RefList.T := NIL;

    r := NEW(Regression.T);

    responseHat : REF Array;
  BEGIN
    WHILE p # NIL DO
      IF CondMatch(NARROW(p.head, REF Result)^, cond) THEN
        m := RefList.Cons(p.head, m)
      END;
      p := p.tail
    END;

    WITH n           = RefList.Length(m),
         matrix      = NEW(REF Array, n, N),
         response    = NEW(REF Array, n, 1) DO
      FOR i := 0 TO n - 1 DO
        MakeIndeps(NARROW(m.head, REF Result)^, matrix[i]);
        response[i, 0] := NARROW(m.head, REF Result).power;
        m := m.tail
      END;

      Regression.Run(matrix, response, responseHat, FALSE, r, h := ridgeCoeff)

    END;

    FOR i := 0 TO NUMBER(r.b^) - 1 DO
      FOR j := 0 TO NUMBER(r.b[i]) - 1 DO
        Debug.Out(F("r.b[%s,%s] = %s", Int(i), Int(j), LR(r.b[i,j])))
      END
    END;
    Debug.Out("R_sq = " & LR(r.R_sq));
    RETURN r.b
  END DoOneEquation;

CONST
    N = 5;
  (*N = 4;*)
  
TYPE
  X      = ARRAY [ 0 .. N - 1 ] OF LONGREAL;

PROCEDURE CondMatch(READONLY result : Result;
                    READONLY cond   : Condition) : BOOLEAN =
  BEGIN
    RETURN TE(result.prog, cond.prog) AND result.speed = cond.speed
  END CondMatch;
  
PROCEDURE MakeIndeps(result : Result; VAR x : X) =
  BEGIN
    WITH d      = FLOAT(result.depth, LONGREAL),
         w      = FLOAT(result.width, LONGREAL),
         blocks = result.depth MOD BlockSize + 1,
         b      = FLOAT(blocks,       LONGREAL) DO
      
      x := X { 1.0d0, d, w, d*w , b*w  }
      
    END
  END MakeIndeps;

TYPE
  Model = OBJECT
    prog      : TEXT;
    refFreq   : LONGREAL;
    totP      : REF Matrix.M;
    noLeakP   : REF Matrix.M;
  END;

VAR models := NEW(TextRefTbl.Default).init();

PROCEDURE DumpOut() =

  PROCEDURE DumpOne(d, w : CARDINAL) =
    BEGIN
      Wr.PutText(wr, F("%s, %s, ", Int(d), Int(w)));
      WITH pred = Predict(Progs[FIRST(Progs)], d, w, 1.0d0) DO
        Wr.PutText(wr, F("%s", LR(pred.predLeakP / 1.0d-3)))
      END;

      FOR p := FIRST(Progs) TO LAST(Progs) DO
        WITH pred = Predict(Progs[p], d, w, 1.0d0) DO
          Wr.PutText(wr, F(", %s", LR(pred.noLeakP / 1.0d-12)))
        END
      END;
      Wr.PutChar(wr, '\n')
    END DumpOne;
    
  VAR
    wr := FileWr.Open("allsizes.csv");
  BEGIN
    Wr.PutText(wr, "depth/words, width/bits, leak/mW");
    FOR p := FIRST(Progs) TO LAST(Progs) DO
      Wr.PutText(wr, ", dyn " & Progs[p] & "/pJ")
    END;
    Wr.PutChar(wr, '\n');
    FOR d := 1 TO 256 DO
      FOR w := 2 TO 144 DO
        DumpOne(d, w)
      END
    END;
    Wr.Close(wr)
  END DumpOut;
    
CONST
  Progs = ARRAY OF TEXT { "idle", "read", "write", "rw" };


PROCEDURE GetMatch(d, w : CARDINAL;
                   spd : LONGREAL;
                   prog : TEXT;
                   VAR res : Result) : BOOLEAN =
  VAR
    p := results;
  BEGIN
    WHILE p # NIL DO
      WITH cur = NARROW(p.head, REF Result)^ DO
        IF cur.depth = d AND cur.width = w AND cur.speed = spd AND
           TE(cur.prog, prog) THEN
          res := cur;
          RETURN TRUE
        END
      END;
      p := p.tail
    END;
    RETURN FALSE
  END GetMatch;
    
PROCEDURE GenLeakageData(oneCond, halfCond : Condition) =
  VAR
    p := results;
    halfResult : Result;
    leakR : REF Result;
  BEGIN
    WHILE p # NIL DO
      IF CondMatch(NARROW(p.head, REF Result)^, oneCond) THEN
        WITH oneResult = NARROW(p.head, REF Result)^,
             haveHalf  = GetMatch(oneResult.depth,
                                  oneResult.width,
                                  halfCond.speed,
                                  halfCond.prog,
                                  halfResult) DO
          IF haveHalf THEN
            leakR := NEW(REF Result);
            WITH halfP = halfResult.power,
                 oneP  = oneResult.power,
                 leakP = 2.0d0 * halfP - oneP DO
              leakR^      := oneResult;
              (* copy oneResult and update it for leak power*)
              
              leakR.prog  := "leak";
              leakR.power := leakP;
              leakR.speed := 0.0d0;
              
              results     := RefList.Cons(leakR, results);

              Debug.Out(F("Building leakage for d %s w %s; halfP %s oneP %s leakP %s",
                          Int(oneResult.depth),
                          Int(oneResult.width),
                          LR(halfP),
                          LR(oneP),
                          LR(leakP)))
            END
          ELSE
            Debug.Warning(F("no match for d %s w %s speed %s prog %s",
                            Int(oneResult.depth),
                            Int(oneResult.width),
                            LR(halfCond.speed),
                            halfCond.prog));
          END
        END
      END;
      p := p.tail
    END
  END GenLeakageData;

CONST RegressLeak = FALSE;
      
PROCEDURE DoRegressions() =
  VAR
    idleOne  := DoOneEquation(Condition { "idle", 1.0d9 });
    idleHalf := DoOneEquation(Condition { "idle", 5.0d8 });
    leak     : REF Matrix.M;

    zero, leakage, scaledLeak, eqScaledLeak, doubleHalf, processScaledTot, processScaledMinusLeak := Matrix.NewM(Matrix.GetDim(idleOne^));
  BEGIN

    IF RegressLeak THEN
      leak := DoOneEquation(Condition { "leak", 0.0d0 });
    END;
    
    Matrix.MulSM(2.0d0, idleHalf^, doubleHalf^);
    Matrix.SubM(doubleHalf^, idleOne^, leakage^);

    Matrix.Zero(zero^);

    Debug.Out("idleHalf:");
    Debug.Out(Matrix.FormatM(idleHalf^));
    Debug.Out("doubleHalf:");
    Debug.Out(Matrix.FormatM(doubleHalf^));
    Debug.Out("idleOne:");
    Debug.Out(Matrix.FormatM(idleOne^));
    Debug.Out("computed leakage:");
    Debug.Out(Matrix.FormatM(leakage^));
    IF RegressLeak THEN
      Debug.Out("regressed leakage:");
      Debug.Out(Matrix.FormatM(leak^));
    END;

    (* try erasing every coefficient of leakage except the highest *)
    FOR i := 0 TO N - 2 DO
      leakage[i, 0] := 0.0d0
    END;


    EVAL models.put("leak",
                    NEW(Model,
                        prog    := "leak",
                        refFreq := 0.0d0,
                        totP    := leakage,
                        noLeakP := zero));
    
    FOR i := FIRST(Progs) TO LAST(Progs) DO
      WITH prog = Progs[i],
           refFreq = 1.0d9,
           cond = Condition { prog, refFreq } DO
        VAR
          eq := DoOneEquation(cond);
          eqMinusLeak := Matrix.NewM(Matrix.GetDim(idleOne^));
        BEGIN
          Matrix.SubM(eq^, leakage^, eqMinusLeak^);

          Debug.Out("program " & prog & ":");
          Debug.Out(Matrix.FormatM(eq^));
          
          Debug.Out("program " & prog & " w/o leakage:");
          Debug.Out(Matrix.FormatM(eqMinusLeak^));

          Matrix.MulSM(leakScale, leakage^, scaledLeak^);
          Matrix.AddM(scaledLeak^, eqMinusLeak^, eqScaledLeak^);
          Debug.Out("program " & prog & " w/ scaled leakage:");
          Debug.Out(Matrix.FormatM(eqScaledLeak^));
          
          Matrix.MulSM(processScale, eqScaledLeak^, processScaledTot^);
          Matrix.MulSM(processScale, eqMinusLeak^, processScaledMinusLeak^);

          Debug.Out("proc-scaled program " & prog & " w/o leakage:");
          Debug.Out(Matrix.FormatM(processScaledMinusLeak^));
          Debug.Out("proc-scaled program " & prog & " w/ scaled leakage:");
          Debug.Out(Matrix.FormatM(processScaledTot^));

          WITH model = NEW(Model,
                           prog    := prog,
                           refFreq := refFreq,
                           totP    := processScaledTot,
                           noLeakP := processScaledMinusLeak) DO
            EVAL models.put(prog, model)
          END
        END
      END
    END
  END DoRegressions;

PROCEDURE DumpDebugCsv() =
  VAR
    wr := FileWr.Open("lambpower.csv");
    p  := results;
  BEGIN

    Wr.PutText(wr, "name, depth, width, program, speed, measP, predP, deltaP, predLeakP");
    FOR i := 0 TO N - 1 DO
      Wr.PutText(wr, ", predR" & Int(i))
    END;
    Wr.PutChar(wr, '\n');
    
    WHILE p # NIL DO
      WITH res = NARROW(p.head, REF Result)^ DO
        Wr.PutText(wr, F("%s, %s, %s, %s, %s,", res.fn, Int(res.depth), Int(res.width), res.prog, LR(res.speed)));
        WITH pred = Predict(res.prog, res.depth, res.width, res.speed),
             predP = pred.predP,
             measP = res.power,
             deltaP = predP - measP,
             predLeakP = pred.predLeakP DO
          Wr.PutText(wr, F("%s, %s, %s, %s",
                           LR(res.power), LR(predP), LR(deltaP), LR(predLeakP)));
          FOR i := 0 TO N - 1 DO
            Wr.PutText(wr, F(", %s", LR(pred.contribs[i])))
          END;

          Wr.PutChar(wr, '\n')
        END
      END;
      p := p.tail
    END;
    Wr.Close(wr)
  END DumpDebugCsv;

VAR
  leakScale, processScale := 1.0d0;
  ridgeCoeff := 0.0d0;
  
BEGIN
  TRY
    IF pp.keywordPresent("-d") THEN
      workDir := pp.getNext()
    END;

    IF pp.keywordPresent("-pattern") THEN
      pat := RegEx.Compile(pp.getNext())
    END;

    IF pp.keywordPresent("-leakscale") THEN
      leakScale := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-processscale") THEN
      processScale := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-ridge") THEN
      ridgeCoeff := pp.getNextLongReal()
    END;

    WITH haveIt = pp.keywordPresent("-vdd") DO
      IF NOT haveIt THEN
        Debug.Error("?must specify -vdd <voltage>")
      END;
      vdd := Scan.LongReal(pp.getNext())
    END
      
  EXCEPT
    ParseParams.Error => Debug.Error("?couldnt parse parameters")
  END;

  IF workDir = NIL THEN
    Debug.Error("?must specify -d")
  END;
  
  fsIter := FS.Iterate(workDir);

  WHILE fsIter.next(path) DO
    IF RegEx.Execute(pat, path) # -1 THEN
      DoOneFile(path)
    END
  END;

  IF RegressLeak THEN
    GenLeakageData(Condition { "idle", 1.0d9 },
                   Condition { "idle", 5.0d8 });
  END;
  
  DoRegressions();

  DumpDebugCsv();
  
  DumpOut()
END Main.
