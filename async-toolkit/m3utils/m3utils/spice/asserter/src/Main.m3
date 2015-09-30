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

<*FATAL Lex.Error, FloatMode.Trap, Thread.Alerted*>

CONST TE = Text.Equal;
CONST R  = Fmt.Real;
CONST LR = Fmt.LongReal;

VAR
  pfx   := Params.Get(1);
  assFn := Params.Get(2);

  trFn := pfx & ".trace";
  nmFn := pfx & ".names";
  nmSeq := NEW(TextSeq.T).init();
  nmTab := NEW(TextIntTbl.Default).init();

  assertions : REF ARRAY OF LongrealPQ.T;

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

VAR 
  idx : INTEGER;
  acnt := 0;
  fail        := FALSE;

BEGIN
  WITH namesRd = FileRdOpen(nmFn) DO
    TRY
      LOOP
        WITH nm = Rd.GetLine(namesRd) DO
          EVAL nmTab.put(TextUtils.ToLower(nm), nmSeq.size());
          nmSeq.addhi(nm)
        END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(namesRd)
    END
  END;

  Debug.Out(Int(nmSeq.size()) & " names");

  assertions := NEW(REF ARRAY OF LongrealPQ.T, nmSeq.size());

  WITH assRd = FileRd.Open(assFn) DO
    TRY
      LOOP
        WITH ln  = Rd.GetLine(assRd),
             rdr = NEW(TextReader.T).init(ln),
             c   = rdr.nextE(" ") DO
          IF TE(c,"ASSERTRANGE") THEN
            WITH nm = rdr.nextE(" "),
                 hadIt = nmTab.get(TextUtils.ToLower(nm), idx),
                 tm = Scan.LongReal(rdr.nextE(" ")),
                 lo = Scan.LongReal(rdr.nextE(" ")),
                 hi = Scan.LongReal(rdr.nextE(" ")),
                 elt = NEW(MyElt, 
                           ass      := Assertion { nm, tm, lo, hi },
                           priority := tm) DO
              IF assertions[idx] = NIL THEN
                assertions[idx] := NEW(LongrealPQ.Default).init()
              END;
              assertions[idx].insert(elt);
              INC(acnt)
            END          
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => Rd.Close(assRd)
    END
  END;

  Debug.Out(Int(acnt) & " assertions");

  PROCEDURE GetLR() : LONGREAL =
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

  VAR
    n           := nmSeq.size();
    tRd         := FileRdOpen(trFn);
    i           := 0;
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
      time[i] := GetLR()
    END;
    Debug.Out(F("%s timesteps min %s step %s max %s", 
                Int(steps), 
                LR(time[0]), LR(time[1]-time[0]), LR(time[LAST(time^)])));
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
              v1 := GetLR();

              Rd.Seek(tRd, 12 + 4 * (steps * j + i + 1));
              v2 := GetLR();
              
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
      END;
    END;
    Rd.Close(tRd);

  END;
  IF fail THEN
    IO.Put("FAIL\n");
    Process.Exit(1)
  ELSE
    IO.Put("PASS\n");
    Process.Exit(0)
  END
END Main.
