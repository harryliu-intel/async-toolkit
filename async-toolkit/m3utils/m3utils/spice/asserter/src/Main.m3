UNSAFE MODULE Main;
IMPORT Params, TextSeq;
IMPORT Rd, FileRd;
IMPORT TextIntTbl;
IMPORT Debug;
FROM Fmt IMPORT Int, F; IMPORT Fmt;
IMPORT LongrealPQ;
IMPORT Text;
IMPORT Scan;
IMPORT TextReader;
IMPORT Lex, FloatMode;
IMPORT IO, Process;
IMPORT OSError, AL;
IMPORT Pathname;
IMPORT Thread;
IMPORT TextUtils;
IMPORT RefSeq;
IMPORT SimMeasurement;
IMPORT FileWr, Wr;
IMPORT TextRd;

<*FATAL Thread.Alerted*>

CONST TE = Text.Equal;
CONST LR = Fmt.LongReal;

VAR
  pfx   := Params.Get(1);
  assFn := Params.Get(2);

  trFn := pfx & ".trace";
  nmFn := pfx & ".names";
  nmSeq := NEW(TextSeq.T).init();
  nmTab := NEW(TextIntTbl.Default).init();

  assertions : REF ARRAY OF LongrealPQ.T;
  measurements : RefSeq.T;
  
TYPE
  Assertion = RECORD
    nm         : TEXT;
    tm         : LONGREAL;
    minV, maxV : LONGREAL;
  END;

  MyElt = LongrealPQ.Elt OBJECT
    ass : Assertion;
  END;

PROCEDURE FileRdOpen(fn : Pathname.T) : Rd.T =
  BEGIN
    TRY
      RETURN FileRd.Open(fn) 
    EXCEPT
      OSError.E(x) => 
      Debug.Error("Can't open \"" & fn & "\" : OSError.E : " & AL.Format(x));
      <*ASSERT FALSE*>
    END
  END FileRdOpen;

PROCEDURE GetLR(tRd : Rd.T) : LONGREAL RAISES { Rd.Failure } =
  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    WITH got = Rd.GetSub(tRd, buff^) DO
      <*ASSERT got = 4*>
      WITH r = LOOPHOLE(buff, REF ARRAY OF REAL) DO
        RETURN FLOAT(r[0],LONGREAL)
      END;
    END
  END GetLR;

PROCEDURE Parse() RAISES { Rd.Failure, Wr.Failure } =
  <*FATAL LongrealPQ.Empty*>
  VAR
    n           := nmSeq.size();
    tRd         := FileRdOpen(trFn);
    ibuff       := NEW(REF ARRAY OF CHAR, 4);
    start, end  : CARDINAL;
    steps       : CARDINAL;
    time        : REF ARRAY OF LONGREAL;
  BEGIN
    EVAL Rd.GetSub(tRd, ibuff^);
    Debug.Out("int=" & Int(LOOPHOLE(ibuff,REF ARRAY OF INTEGER)[0]));
    EVAL Rd.GetSub(tRd, ibuff^);
    Debug.Out("int=" & Int(LOOPHOLE(ibuff,REF ARRAY OF INTEGER)[0]));
    EVAL Rd.GetSub(tRd, ibuff^);
    Debug.Out("int=" & Int(LOOPHOLE(ibuff,REF ARRAY OF INTEGER)[0]));

    start := Rd.Index(tRd);
    Rd.Seek(tRd, LAST(CARDINAL));
    end   := Rd.Index(tRd);
    
    WITH len = end-start,
         samp = (end-start) DIV 4 DO
      steps := samp DIV n;
      
      Debug.Out(F("start %s end %s len %s samples %s steps %s",
                  Int(start), Int(end), Int(len), Int(samp),
                  Int(steps)) & F(" rem %s", Int(samp MOD n)))
    END;
    
    Rd.Seek(tRd, start);

    time := NEW(REF ARRAY OF LONGREAL, steps);
    FOR i := FIRST(time^) TO LAST(time^) DO
      time[i] := GetLR(tRd)
    END;

    IF NUMBER(time^) >= 2 THEN
      Debug.Out(F("%s timesteps min %s step %s max %s", 
                  Int(steps), 
                  LR(time[0]), LR(time[1]-time[0]), LR(time[LAST(time^)])))
    END;

    FOR j := 1 TO n-1 DO
      IF j MOD 1000 = 0 THEN Debug.Out(Int(j) & " names") END;
      
      IF assertions[j] # NIL THEN
        Debug.Out(F("node %s \"%s\" %s assertions",
                    Int(j), nmSeq.get(j), Int(assertions[j].size())));
        Rd.Seek(tRd, 12 + steps * 4 * j);
        
        FOR i := FIRST(time^) TO LAST(time^)-1 DO
          WHILE assertions[j].size() # 0 AND
                assertions[j].min().priority >= time[i] AND
                assertions[j].min().priority <  time[i+1] DO
            VAR
              rec : MyElt := assertions[j].deleteMin();
              v1, v2 : LONGREAL;
              t1 := time[i];
              t2 := time[i+1];
              tx := rec.priority;
            BEGIN
              Rd.Seek(tRd, 12 + 4 * (steps * j + i));
              v1 := GetLR(tRd);

              Rd.Seek(tRd, 12 + 4 * (steps * j + i + 1));
              v2 := GetLR(tRd);
              
              WITH vx = (tx-t1)/(t2-t1)*(v2-v1) + v1 DO
                IF vx >= rec.ass.minV AND vx <= rec.ass.maxV THEN
                  Debug.Out(F("pass: %s @ %s : %s < V=%s < %s",
                              rec.ass.nm, 
                              LR(tx), 
                              LR(rec.ass.minV), LR(vx), LR(rec.ass.maxV)))
                ELSE
                  fail := TRUE;
                  Debug.Out(F("FAIL: %s @ %s :  V=%s out of range %s,%s",
                              rec.ass.nm, 
                              LR(tx), 
                              LR(vx),
                              LR(rec.ass.minV), LR(rec.ass.maxV)))
                END
              END
            END
          END
        END
      END (* IF assertions *)
    END;

    DoMeasurements(time, tRd, steps);
    
    Rd.Close(tRd);
    Wr.Close(mWr)
  END Parse;

EXCEPTION MeasurementFailure;

PROCEDURE DoMeasurements(time  : REF ARRAY OF LONGREAL;
                         rd    : Rd.T;
                         steps : CARDINAL)
  RAISES { Wr.Failure } =
  BEGIN
    FOR i := 0 TO measurements.size()-1 DO
      
      WITH s   = NARROW(measurements.get(i),SimMeasurement.T) DO
        TRY
          WITH res = DoMeasurement(time, s, steps, rd) DO
            
            Wr.PutText(mWr,
                       F("%s %s\n",
                         LR(res.d[LAST(res.d^)]),
                         s.format("", NIL)))
          END
        EXCEPT
          Rd.Failure, MeasurementFailure =>
          Wr.PutText(mWr,
                     F("%s %s\n", "FAIL", s.format("", NIL)))
        END
      END        
    END
  END DoMeasurements;

TYPE
  Result = RECORD
    d   : REF ARRAY OF LONGREAL;
    off : CARDINAL;
  END;

PROCEDURE DoMeasurement(time  : REF ARRAY OF LONGREAL;
                        s     : SimMeasurement.T;
                        steps : CARDINAL;
                        tRd   : Rd.T) : Result
  RAISES { MeasurementFailure, Rd.Failure } =

  PROCEDURE GetData(node     : TEXT;
                    quantity : SimMeasurement.ProbeType) : REF ARRAY OF LONGREAL
    RAISES { MeasurementFailure, Rd.Failure } =
    VAR
      res := NEW(REF ARRAY OF LONGREAL, steps);
      idx : INTEGER;
      sigNm : TEXT;
    BEGIN
      CASE quantity OF
        SimMeasurement.ProbeType.Voltage =>
        sigNm := TextUtils.ToLower(node)
      |
        SimMeasurement.ProbeType.Current =>
        sigNm := F("i(%s)",TextUtils.ToLower(node))
      END;
      
      WITH hadIt = nmTab.get(TextUtils.ToLower(sigNm), idx) DO
        IF NOT hadIt THEN
          Debug.Warning(F("Unknown signal name \"%s\"", sigNm));
          RAISE MeasurementFailure
        END
      END;
      Rd.Seek(tRd, 12 + steps * 4 * idx);
      FOR i := FIRST(res^) TO LAST(res^) DO
        res[i] := GetLR(tRd)
      END;
      RETURN res
    END GetData;

  PROCEDURE GetTriggerIdx(trg : SimMeasurement.Trigger;
                          start : BOOLEAN) : CARDINAL
    RAISES { MeasurementFailure, Rd.Failure } =
    TYPE
      Op = SimMeasurement.Op;
    VAR
      str : TEXT;
      res := LAST(CARDINAL);
    BEGIN
      IF trg = NIL THEN
        str := "**NIL**"
      ELSE
        str := F("%s %s %s",
                 LR(trg.val),
                 SimMeasurement.OpNames[trg.op],
                 trg.spec.format("",NIL))
      END;

      IF trg = NIL THEN
        IF start THEN res := 0 ELSE res := LAST(time^) END
      ELSE
        WITH result = DoMeasurement(time, trg.spec, steps, tRd) DO
          Debug.Out("Working on trigger \"" & str & "\"");
          Debug.Out(F("Measurement returned result off %s sz %s time=[%s,%s]",
                      Int(result.off),Int(NUMBER(result.d^)),
                      LR(time[result.off]), LR(time[result.off+LAST(result.d^)])));

          FOR i := FIRST(result.d^) TO LAST(result.d^) DO
            CASE trg.op OF
              Op.Le =>
              WITH p = result.d[i] DO
                IF p <= trg.val THEN res := i + result.off; EXIT END
              END
            |
              Op.Ge =>
              WITH p = result.d[i] DO
                IF p >= trg.val THEN res := i + result.off; EXIT END
              END
            |
              Op.Up =>
              IF i # FIRST(result.d^) THEN
                WITH p = result.d[i-1],
                     q = result.d[i] DO
                  IF p < trg.val AND q >= trg.val THEN
                    res := i + result.off; EXIT
                  END
                END
              END
            |
              Op.Dn =>
              IF i # FIRST(result.d^) THEN
                WITH p = result.d[i-1],
                     q = result.d[i] DO
                  IF p > trg.val AND q <= trg.val THEN
                    res := i + result.off; EXIT
                  END
                END
              END
            END
          END
        END
      END;
      IF res = LAST(CARDINAL) THEN
        Debug.Out("Trigger failed");
        RAISE MeasurementFailure
      ELSE
        Debug.Out("Trigger value is " & Int(res));
        RETURN res
      END
    END GetTriggerIdx;

  BEGIN
    TYPECASE s OF
      SimMeasurement.Clock =>
      Debug.Error("Dont know how to measure Clock in this program, sorry.");
      <*ASSERT FALSE*> (*NOTREACHED*)
    |
      SimMeasurement.Time =>
      RETURN Result { d := time, off := 0 }
    |
      SimMeasurement.Default(def) =>
      WITH base    = GetData(def.nodeNm,def.quantity),
           sIdx    = GetTriggerIdx(def.from, start := TRUE),
           eIdx    = GetTriggerIdx(def.to, start := FALSE),
           resData = NEW(REF ARRAY OF LONGREAL, eIdx-sIdx+1) DO
        PerformOp(def.type, SUBARRAY(base^, sIdx, eIdx-sIdx+1), resData^);
        RETURN Result { d := resData, off := sIdx }
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END DoMeasurement;

PROCEDURE InitMeasurement(tp : SimMeasurement.Type) : LONGREAL =
  BEGIN
    CASE tp OF
      SimMeasurement.Type.Max  => RETURN FIRST(LONGREAL)
    |
      SimMeasurement.Type.Min  => RETURN LAST(LONGREAL)
    |
      SimMeasurement.Type.Ave  => RETURN 0.0d0
    |
      SimMeasurement.Type.Last => RETURN 0.0d0
    END
  END InitMeasurement;

PROCEDURE Accumulate(tp : SimMeasurement.Type; VAR res : LONGREAL; v : LONGREAL) =
  BEGIN
    CASE tp OF
      SimMeasurement.Type.Max => res := MAX(res,v)
    |
      SimMeasurement.Type.Min => res := MIN(res,v)
    |
      SimMeasurement.Type.Ave => res := res + v
    |
      SimMeasurement.Type.Last => res := v
    END
  END Accumulate;

PROCEDURE PerformOp(tp           : SimMeasurement.Type;
                    READONLY in  : ARRAY OF LONGREAL;
                    VAR      out : ARRAY OF LONGREAL) =
  VAR
    v := InitMeasurement(tp);
  BEGIN
    <*ASSERT NUMBER(in) = NUMBER(out)*>
    FOR i := FIRST(in) TO LAST(in) DO
      Accumulate(tp, v, in[i]);
      out[i] := v
    END
  END PerformOp;

VAR 
  idx : INTEGER;
  acnt := 0;
  fail        := FALSE;
  mWr : Wr.T;
  
BEGIN
  TRY
    mWr := FileWr.Open("asserter.measure");
  EXCEPT
    OSError.E => Debug.Error("Couldnt open measure file for writing")
  END;
  WITH namesRd = FileRdOpen(nmFn) DO
    TRY
      LOOP
        WITH nm = Rd.GetLine(namesRd) DO
          EVAL nmTab.put(TextUtils.ToLower(nm), nmSeq.size());
          nmSeq.addhi(nm)
        END
      END
    EXCEPT
      Rd.Failure(x) => Debug.Error("Couldnt read names file, Rd.Failure : "&
        AL.Format(x))
    |
      Rd.EndOfFile => TRY Rd.Close(namesRd) EXCEPT ELSE END
    END
  END;

  Debug.Out(Int(nmSeq.size()) & " names");

  assertions   := NEW(REF ARRAY OF LongrealPQ.T, nmSeq.size());
  measurements := NEW(RefSeq.T).init();

  VAR
    assRd : Rd.T;
  BEGIN
    TRY
      assRd := FileRdOpen(assFn);
      LOOP
        WITH ln  = Rd.GetLine(assRd),
             rdr = NEW(TextReader.T).init(ln),
             c   = rdr.nextE(" ") DO
          IF TE(c,"ASSERTRANGE") THEN
            TRY
              WITH nm = rdr.nextE(" "),
                   hadIt = nmTab.get(TextUtils.ToLower(nm), idx),
                   tm = Scan.LongReal(rdr.nextE(" ")),
                   lo = Scan.LongReal(rdr.nextE(" ")),
                   hi = Scan.LongReal(rdr.nextE(" ")),
                   elt = NEW(MyElt, 
                             ass      := Assertion { nm, tm, lo, hi },
                             priority := tm) DO
                IF NOT hadIt THEN
                  Debug.Error(F("asserter cant find node \"%s\"",nm))
                END;
                IF assertions[idx] = NIL THEN
                  assertions[idx] := NEW(LongrealPQ.Default).init()
                END;
                assertions[idx].insert(elt);
                INC(acnt)
              END
            EXCEPT
              Lex.Error, FloatMode.Trap => Debug.Error("Couldnt parse ASSERTRANGE statement")
            END
          ELSIF TE(c, "MEASURE") THEN
            TRY
              WITH m = NARROW(SimMeasurement.Parse(NEW(TextRd.T).init(ln)),
                              SimMeasurement.Default),
                   hadIt = nmTab.get(TextUtils.ToLower(m.nodeNm), idx) DO
                IF NOT hadIt THEN
                  Debug.Error(F("asserter cant find node \"%s\"",m.nodeNm))
                END;
                measurements.addhi(m)
              END
            EXCEPT
              SimMeasurement.ParseError(txt) =>
              Debug.Error("Couldnt parse MEASURE statement : " & txt)
            END
          ELSE
            Debug.Error(F("Unknown asserter command \"%s\"", c))
          END(*IF*)
        END
      END
    EXCEPT
      Rd.Failure(x) => Debug.Error("Cant read ass file : Rd.Failure : " &
        AL.Format(x))
    |
      Rd.EndOfFile => TRY Rd.Close(assRd) EXCEPT ELSE END
    |
      TextReader.NoMore => Debug.Error("Not enough tokens on line")
    END
  END;

  Debug.Out(Int(acnt) & " assertions");

  TRY
    Parse()
  EXCEPT
    Rd.Failure => Debug.Error("Rd.Failure reading input data file")
  |
    Wr.Failure => Debug.Error("Wr.Failure writing output")
  END;
  
  VAR
    remAss := 0;
  BEGIN
    FOR i := FIRST(assertions^) TO LAST(assertions^) DO
      IF assertions[i] # NIL THEN
        INC(remAss, assertions[i].size())
      END
    END;
    Debug.Out(Int(remAss) & " assertions remaining");

    IF fail THEN
      IO.Put("FAIL\n");
      Process.Exit(1)
    ELSIF remAss > 0 THEN
      IO.Put("NOTYET\n");
      Process.Exit(0)
    ELSE
      IO.Put("PASS\n");
      Process.Exit(0)
    END
  END
END Main.
