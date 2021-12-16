MODULE Main;
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
                   sub));
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
           noLeakAtSpd = speed / model.refFreq * noLeakAtRef,
           totAtSpd    = noLeakAtSpd + leak DO
        res.predP := totAtSpd;
        res.predLeakP := leak;
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
    predP, predLeakP : LONGREAL;
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
    Debug.Out(F("DoOneFile(%s) depth %s width %s speed %s prog %s",
                fn, Int(depth), Int(width), LR(speed), prog));
    WITH rd     = FileRd.Open(workDir & "/" & fn),
         line   = Rd.GetLine(rd),
         reader = NEW(TextReader.T).init(line),
         w1     = reader.nextE(" \t", skipNulls := TRUE),
         w2     = reader.nextE(" \t", skipNulls := TRUE) DO
      (* format is that w2 is the average current *)
      results := RefList.Cons(
                     NEW(REF Result,
                         fn    := fn,
                         depth := depth,
                         width := width,
                         speed := speed,
                         prog  := prog,
                         power := -Scan.LongReal(w2)),
                     results)
    END
  END DoOneFile;

TYPE
  Condition = RECORD
    prog : TEXT;
    speed : LONGREAL;
  END;

  Array = ARRAY OF ARRAY OF LONGREAL;

PROCEDURE DoOneEquation(READONLY cond : Condition) : REF Matrix.M =
  VAR
    p := results;
    m : RefList.T := NIL;

    r := NEW(Regression.T);

    responseHat : REF Array;
    res : X;
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

      Regression.Run(matrix, response, responseHat, FALSE, r)

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
      
      x := X { 1.0d0, d, w, d*w, b*w }
      
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
    
BEGIN
  TRY
    IF pp.keywordPresent("-d") THEN
      workDir := pp.getNext()
    END;

    IF pp.keywordPresent("-pattern") THEN
      pat := RegEx.Compile(pp.getNext())
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

  CONST
    Progs = ARRAY OF TEXT { "idle", "read", "write", "rw" };
   
  VAR
    idleOne  := DoOneEquation(Condition { "idle", 1.0d9 });
    idleHalf := DoOneEquation(Condition { "idle", 5.0d8 });

    zero, leakage, doubleHalf := Matrix.NewM(Matrix.GetDim(idleOne^));
  BEGIN
    Matrix.MulSM(2.0d0, idleHalf^, doubleHalf^);
    Matrix.SubM(doubleHalf^, idleOne^, leakage^);

    Matrix.Zero(zero^);

    Debug.Out("idleHalf:");
    Debug.Out(Matrix.FormatM(idleHalf^));
    Debug.Out("doubleHalf:");
    Debug.Out(Matrix.FormatM(doubleHalf^));
    Debug.Out("idleOne:");
    Debug.Out(Matrix.FormatM(idleOne^));
    Debug.Out("leakage:");
    Debug.Out(Matrix.FormatM(leakage^));


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

          WITH model = NEW(Model,
                           prog := prog,
                           refFreq := refFreq,
                           totP := eq,
                           noLeakP := eqMinusLeak) DO
            EVAL models.put(prog, model)
          END
        END
      END
    END
  END;

  VAR
    wr := FileWr.Open("lambpower.csv");
    p  := results;
  BEGIN

    Wr.PutText(wr, "name, depth, width, program, speed, measP, predP, deltaP, predLeakP, predR0, predR1, predR2, predR3, predR4\n");
    WHILE p # NIL DO
      WITH res = NARROW(p.head, REF Result)^ DO
        Wr.PutText(wr, F("%s, %s, %s, %s, %s,", res.fn, Int(res.depth), Int(res.width), res.prog, LR(res.speed)));
        WITH pred = Predict(res.prog, res.depth, res.width, res.speed),
             predP = pred.predP,
             measP = res.power,
             deltaP = predP - measP,
             predLeakP = pred.predLeakP DO
          Wr.PutText(wr, F("%s, %s, %s, %s,",
                           LR(res.power), LR(predP), LR(deltaP), LR(predLeakP)));
          Wr.PutText(wr, F("%s, %s, %s, %s, %s",
                           LR(pred.contribs[0]),
                           LR(pred.contribs[1]),
                           LR(pred.contribs[2]),
                           LR(pred.contribs[3]),
                           LR(pred.contribs[4])
          ));

          Wr.PutChar(wr, '\n')
        END
      END;
      p := p.tail
    END;
    Wr.Close(wr)
  END
END Main.
