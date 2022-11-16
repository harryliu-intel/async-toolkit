MODULE SpiceCompress EXPORTS Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT AL;
IMPORT OSError;
IMPORT Rd;
IMPORT Process;
IMPORT Pathname;
FROM Fmt IMPORT F, Int, LongReal, Pad;
IMPORT FS;
IMPORT Trace;
IMPORT LongrealArraySort;
IMPORT TransitionFinder;
IMPORT Wr, FileWr;
IMPORT Transition;
IMPORT TransitionSeq;
IMPORT Wavelet;
IMPORT Word;
IMPORT Math;
IMPORT SparseLR;
IMPORT LRRegression AS Regression;

IMPORT Thread;

<*FATAL Thread.Alerted*>
<*FATAL Wr.Failure*>
<*FATAL OSError.E*>

CONST
  WaveCnt = 100;
  Usage   = "";
  LR      = LongReal;

TYPE
  LReal = LONGREAL;

PROCEDURE Wdebug(msg : TEXT) : LONGREAL =
  BEGIN
    Debug.Out(msg);
    RETURN 0.0d0
  END Wdebug;

CONST <*NOWARN*>Ks = ARRAY OF CARDINAL { 1, 63, 64, 77, 91, 99         };
      <*NOWARN*>Km = ARRAY OF CARDINAL { 1,         77    , 99         };
      <*NOWARN*>Kq = ARRAY OF CARDINAL {            77                 };
      <*NOWARN*>Kg = ARRAY OF CARDINAL {            77        , 108091 };
      K  = Kg;

      dirs   = ARRAY [-1..1] OF Transition.Dir { -1, 0, 1 };

TYPE MedTrans = ARRAY [FIRST(dirs)..LAST(dirs)] OF
                    REF ARRAY OF LONGREAL;


PROCEDURE NextPow2(c : CARDINAL) : CARDINAL =
  VAR
    r : Word.T := 1;
  BEGIN
    WHILE r < c DO
      r := Word.Shift(r, 1)
    END;
    RETURN r
  END NextPow2;

PROCEDURE BuildCartoon(allSeq         : TransitionSeq.T;
                       medTrans       : MedTrans;
                       READONLY darr  : ARRAY OF LONGREAL;
                       VAR carr       : ARRAY OF LONGREAL;
                       MatchFirst     : BOOLEAN) =
  VAR
    window  : ARRAY [FIRST(dirs)..LAST(dirs)] OF CARDINAL;
    tSeq    : ARRAY [FIRST(dirs)..LAST(dirs)] OF TransitionSeq.T;
    firstPt : BOOLEAN;
    off     : LONGREAL;

  BEGIN
    FOR d := FIRST(dirs) TO LAST(dirs) DO
      IF medTrans[d] # NIL THEN
        window[d] := NUMBER(medTrans[d]^) DIV 2
      END;
      tSeq[d] := TransitionFinder.FilterDir(allSeq, dirs[d])
    END;

    FOR i := FIRST(carr) TO LAST(carr) DO
      carr[i] := 0.0d0
    END;

    (* overlay all the transitions *)
    FOR i := 0 TO allSeq.size() - 1 DO
      WITH tr = allSeq.get(i),
           mt = medTrans[tr.dir],
           mw = window[tr.dir] DO
        IF mt # NIL THEN
          WITH start = tr.at - FLOAT(mw, LONGREAL) DO

            Debug.Out(F("BuildCartoon i %s start %s LAST(carr) %s",
                        Int(i), LR(start), Int(LAST(carr))));

            firstPt := TRUE;

            (* write the current waveform *)
            FOR j := MAX(0,CEILING(start)) TO LAST(carr) DO
              
              WITH medx = FLOAT(j, LONGREAL) - start DO
                IF medx < FLOAT(2 * mw, LONGREAL) THEN
                  WITH y    = Interpolate(mt^, medx),
                       yhat = carr[j] + y DO
                    IF firstPt THEN
                      IF NOT MatchFirst THEN
                        off := 0.0d0
                      ELSE
                        off := darr[j] - yhat
                      END;
                      firstPt := FALSE
                    END;
                    carr[j] := carr[j] + y + off
                  END
                ELSE
                  carr[j] := carr[j] + mt[LAST(mt^)] + off
                END
              END
            END
          END
        END
      END
    END
  END BuildCartoon;
  
PROCEDURE FindTransWidth(READONLY darr : ARRAY OF LONGREAL;
                         tr            : Transition.T) : LONGREAL =
  VAR
    p := tr.at;
    lo, hi := LAST(LONGREAL);
    dir := FLOAT(tr.dir, LONGREAL);
  BEGIN
    WHILE p > 0.0d0 AND p < FLOAT(NUMBER(darr), LONGREAL) DO
      IF Interpolate(darr, p) <= 0.10d0 THEN
        lo := p;
        EXIT
      END;
      p := p - dir
    END;

    p := tr.at;
    WHILE p > 0.0d0 AND p < FLOAT(NUMBER(darr), LONGREAL) DO
      IF Interpolate(darr, p) >= 0.90d0 THEN
        hi := p;
        EXIT
      END;
      p := p + dir
    END;

    IF lo = LAST(LONGREAL) OR hi = LAST(LONGREAL) THEN
      Debug.Out("Trans width not found");
      RETURN LAST(LONGREAL)
    ELSE
      Debug.Out(F("Trans lo %s hi %s width %s",
                  LR(lo), LR(hi), LR(ABS(hi - lo))));
      RETURN ABS(hi - lo)
    END
  END FindTransWidth;

PROCEDURE Pad2(VAR      to : ARRAY OF LONGREAL;
               READONLY fr : ARRAY OF LONGREAL) =
  BEGIN
    SUBARRAY(to, 0, NUMBER(fr)) := fr;
    FOR i := NUMBER(fr) TO LAST(to) DO
      to[i] := fr[LAST(fr)]
    END
  END Pad2;

PROCEDURE AttemptCompress(pfx        : TEXT;
                          READONLY a : ARRAY OF LONGREAL;
                          levels     : CARDINAL;
                          targMaxDev : LONGREAL) =
  VAR
    n2       := NextPow2(NUMBER(a));

    w,
    y        := NEW(REF ARRAY OF LONGREAL, n2);
    
    s        := NEW(REF ARRAY OF SparseLR.T, n2);
  BEGIN
    Debug.Out(F("AttemptCompress %s : %s", pfx, Int(levels)));
    Pad2(w^, a);
    Wavelet.Wavelet(w^, y^, TRUE);

    Wavelet.ToSparse(w^, s^);

    FOR lev := 0 TO levels - 1 DO
      WITH coeffs = Word.Shift(1, lev),
           myPfx = pfx & ".z" & Int(coeffs) DO
        IF coeffs <= NUMBER(s^) THEN
          Wavelet.FromSparse(SUBARRAY(s^, 0, coeffs), w^);

          Wavelet.Wavelet(w^, y^, FALSE);

          DumpVec(myPfx, w^);
          
          Evaluate(myPfx & ".errs", coeffs, a, w^, targMaxDev)
        END
      END
    END
  END AttemptCompress;

PROCEDURE Evaluate(fn            : TEXT;
                   coeffs        : CARDINAL;
                   READONLY a, b : ARRAY OF LONGREAL;
                   targMaxDev    : LONGREAL) =
  VAR
    maxAbsDiff   := 0.0d0;
    sumDiff      := 0.0d0;
    sumDiffSq    := 0.0d0;
    n            := NUMBER(a);
    e            := NEW(REF ARRAY OF LONGREAL, n);
    fails        := 0;
  BEGIN
    <*ASSERT NUMBER(b) >= NUMBER(a)*>
    FOR i := FIRST(a) TO LAST(a) DO
      e[i] := a[i] - b[i];
      
      WITH diff    = a[i] - b[i],
           absDiff = ABS(diff),
           diffSq  = diff * diff DO
        sumDiff := sumDiff + diff;
        maxAbsDiff := MAX(absDiff, maxAbsDiff);
        IF absDiff > targMaxDev THEN INC(fails) END;
        sumDiffSq := sumDiffSq + diffSq
      END
    END;
    DumpVec(fn, e^);
    WITH meanSq = sumDiffSq / FLOAT(n, LONGREAL),
         rms    = Math.sqrt(meanSq) DO
      
      Debug.Out(F("%s coeffs maxAbsDiff %s RMS %s fails %s cost %s",
                  Int(coeffs),
                  LR(maxAbsDiff),
                  LR(rms),
                  Int(fails),
                  Int(fails + coeffs)))
    END
  END Evaluate;

PROCEDURE Zero(VAR a : ARRAY OF LONGREAL) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := 0.0d0
    END
  END Zero;
  
PROCEDURE DoIt(targMaxDev : LONGREAL) =

  PROCEDURE DoTrans(rn   : Pathname.T;
                    dir  : Transition.Dir;
                    tSeq : TransitionSeq.T) : REF ARRAY OF LONGREAL =
    CONST
      Window = 2.0d0; (* one-sided window width *)
    VAR
      nTrans := tSeq.size();
      bTrans := 0;
      widths := NEW(REF ARRAY OF LONGREAL, nTrans);
      off : LONGREAL;
    BEGIN
      
      FOR j := 0 TO nTrans - 1 DO
        widths[j] := FindTransWidth(darr^, tSeq.get(j));
        IF widths[j] = LAST(LONGREAL) THEN
          INC(bTrans)
        END
      END;
      
      (* bTrans should be checked and used here *)
      
      WITH medWidth  = Interpolate(widths^, 0.5d0 * FLOAT(nTrans, LONGREAL)),
           fwindow   = medWidth * Window, (* n.b. one-sided window *)
           window    = CEILING(fwindow),
           tarr      = NEW(REF ARRAY OF ARRAY OF LONGREAL, window * 2, nTrans),
           med       = NEW(REF ARRAY OF LONGREAL, window * 2)
       DO

        Debug.Out(F("DoTrans(%s,%s) : nTrans %s medWidth %s window %s",
                    rn, Int(dir), Int(nTrans), LR(medWidth), Int(window)));
        
        FOR j := 0 TO nTrans - 1 DO
          FOR w := 0 TO 2 * window - 1 DO
            WITH tr  = tSeq.get(j),
                 off = FLOAT(w - window, LONGREAL),
                 abs = tr.at + off DO
              tarr[w, j] := Interpolate(darr^, MAX(abs, 0.0d0)) (* not quite right *)
            END
          END
        END;

        WITH wr = FileWr.Open(F("%s.medtrans%s.dat", rn, Int(dir))) DO
          FOR w := 0 TO 2 * window - 1 DO
            LongrealArraySort.Sort(tarr[w]);
            med[w] := Interpolate(tarr[w], FLOAT(nTrans, LONGREAL) / 2.0d0)
          END;

          off := med[0];

          FOR w := 0 TO 2 * window - 1 DO
            med[w] := med[w] - off
          END;
          
          FOR w := 0 TO 2 * window - 1 DO
            Wr.PutText(wr, F("%s %s\n", Int(w), LR(med[w])))
          END;
          Wr.PutText(wr, "\n");
          Wr.Close(wr)
        END;

        WITH wr = FileWr.Open(F("%s.trans%s.dat", rn, Int(dir))) DO
          FOR j := 0 TO nTrans - 1 DO
            FOR w := 0 TO 2 * window - 1 DO
              Wr.PutText(wr, F("%s %s\n", Int(w), LR(tarr[w, j] - off)))
            END;
            Wr.PutText(wr, "\n")
          END;
          Wr.Close(wr)
        END;

        RETURN med
      END
    END DoTrans;
  
  PROCEDURE DoOne(i : CARDINAL) =
    VAR
      medTrans := MedTrans { NIL, .. };
      rn       := Pad(Int(i), 6, padChar := '0');

    BEGIN
      trace.getNodeData(i, darr^);
      norm := Normalize(darr^);
      
      (* darr^ is normalized *)

      PolyCompress(rn & ".poly", darr^);
      
      sarr^ := darr^;
      LongrealArraySort.Sort(sarr^);
      
      (* sarr^ is sorted *)
      
      WITH t10idx = FLOAT(nSteps,LReal) * 0.10d0,
           t50idx = FLOAT(nSteps,LReal) * 0.50d0,
           t90idx = FLOAT(nSteps,LReal) * 0.90d0,
           
           t10    = Interpolate(sarr^, t10idx),
           t50    = Interpolate(sarr^, t50idx),
           t90    = Interpolate(sarr^, t90idx),
           
           range  = norm.max - norm.min,
           
           thresh = (norm.max + norm.min) / 2.0d0,
           hyster = (norm.max - norm.min) / 10.0d0,
           (*
             dbg0   = Wdebug(F("t10 %s t90 %s thresh %s hyster %s",
             LR(t10), LR(t10), LR(thresh), LR(hyster))),
           *)
           
           tSeq   = TransitionFinder.Find(iarr^, darr^, thresh, hyster),
           
           nTrans = tSeq.size(),
           

           carr0    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^)),
           carr1    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^)),
           carr2    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^)),
           earr0    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^)),
           earr1    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^))
       
       DO
        Debug.Out(F("i %s trans %s range %s range*trans %s",
                    Int(i), Int(nTrans),
                    LR(range),
                    LR(FLOAT(nTrans, LONGREAL) * range)));
        DumpOne(rn & ".darr", iarr^, darr^);
        
        FOR d := FIRST(dirs) TO LAST(dirs) DO
          VAR
            dSeq := TransitionFinder.FilterDir(tSeq, dirs[d]);
          BEGIN
            IF dSeq.size() # 0 THEN
              medTrans[d] := DoTrans(rn, dirs[d], dSeq);
            END
          END
        END;

        
        BuildCartoon(tSeq, medTrans, darr^, carr0^, FALSE);

        DumpOne(rn & ".carr0", iarr^, carr0^);

        BuildCartoon(tSeq, medTrans, darr^, carr2^, TRUE);

        DumpOne(rn & ".carr2", iarr^, carr2^);

        WITH finalErr = carr0[LAST(carr0^)] - darr[LAST(darr^)],
             nf       = FLOAT(NUMBER(darr^) - 1, LONGREAL) DO
          FOR i := FIRST(carr1^) TO LAST(carr1^) DO
            carr1[i] := carr0[i] - finalErr / nf * FLOAT(i, LONGREAL)
          END
        END;

        DumpOne(rn & ".carr1", iarr^, carr1^);

        FOR i := FIRST(carr1^) TO LAST(carr1^) DO
          earr0[i] := carr0[i] - darr[i];
          earr1[i] := carr1[i] - darr[i]
        END;
        
        DumpOne(rn & ".earr0", iarr^, earr0^);
        DumpOne(rn & ".earr1", iarr^, earr1^);

        (* wavelet stuff *)
        WITH
          n2       = NextPow2(NUMBER(darr^)),
          
          wdarr    = NEW(REF ARRAY OF LONGREAL, n2),
          wdarri   = NEW(REF ARRAY OF LONGREAL, n2),
          wearr0   = NEW(REF ARRAY OF LONGREAL, n2),
          wearr0i   = NEW(REF ARRAY OF LONGREAL, n2),
          wearr1   = NEW(REF ARRAY OF LONGREAL, n2),
          wearr1i   = NEW(REF ARRAY OF LONGREAL, n2),
          s2       = NEW(REF ARRAY OF LONGREAL, n2),

          wiarr    = NEW(REF ARRAY OF LONGREAL, n2),
          q        = NEW(REF ARRAY OF LONGREAL, n2)

         DO
          Integers(wiarr^);

          (****************************************)
          
          Pad2(wdarr^, darr^);
          Wavelet.Wavelet(wdarr^, s2^, TRUE);
          wdarri^ := wdarr^;
          Wavelet.Wavelet(wdarri^, s2^, FALSE);

          DumpOne(rn & ".wdarr", wiarr^, wdarr^);
          DumpOne(rn & ".wdarri", wiarr^, wdarri^);

          (****************************************)

          Pad2(wearr0^, earr0^);
          Wavelet.Wavelet(wearr0^, s2^, TRUE);
          wearr0i^ := wearr0^;
          Wavelet.Wavelet(wearr0i^, s2^, FALSE);

          DumpOne(rn & ".wearr0", wiarr^, wearr0^);
          DumpOne(rn & ".wearr0i", wiarr^, wearr0i^);

          (****************************************)

          Pad2(wearr1^, earr1^);
          Wavelet.Wavelet(wearr1^, s2^, TRUE);
          wearr1i^ := wearr1^;
          Wavelet.Wavelet(wearr1i^, s2^, FALSE);

          DumpOne(rn & ".wearr1", wiarr^, wearr1^);
          DumpOne(rn & ".wearr1i", wiarr^, wearr1i^);

          (****************************************)

          Zero(q^);
          FOR i := 0 TO 20 DO
            WITH z = Word.Shift(1, i) - 1 DO
              IF z <= LAST(q^) THEN
                Zero(q^);
                q[z] := 1.0d0;
                Wavelet.Wavelet(q^, s2^, FALSE);
                DumpVec("q." & Int(z), q^)
              END
            END
          END;
          
          (****************************************)

          
          
          AttemptCompress(rn & ".earr0", earr0^, 20, targMaxDev);
          AttemptCompress(rn & ".earr1", earr1^, 20, targMaxDev);
          AttemptCompress(rn & ".darr", darr^ , 20, targMaxDev);
        END
      END
    END DoOne;
    
  VAR
    nSteps := trace.getSteps();
    tarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    iarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    darr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    sarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    nWav   := MIN(WaveCnt, trace.getNodes());
    norm : Norm;
  BEGIN
      
    (* iarr is integer timesteps *)
    Integers(iarr^);
    
    trace.getTimeData(tarr^);
    
    FOR k := FIRST(K) TO LAST(K) DO
      WITH i = K[k] DO
        DoOne(i)
      END
    END
  END DoIt;

PROCEDURE Integers(VAR a : ARRAY OF LONGREAL) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := FLOAT(i, LONGREAL)
    END
  END Integers;


PROCEDURE DumpOne(nm : Pathname.T;
                  READONLY ta, da : ARRAY OF LONGREAL) =
  VAR
    wr := FileWr.Open(nm);
  BEGIN
    FOR i := FIRST(ta) TO LAST(ta) DO
      Wr.PutText(wr, LR(ta[i]));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, LR(da[i]));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END DumpOne;

PROCEDURE DumpVec(nm          : Pathname.T;
                  READONLY da : ARRAY OF LONGREAL) =
  VAR
    wr := FileWr.Open(nm);
  BEGIN
    FOR i := FIRST(da) TO LAST(da) DO
      Wr.PutText(wr, Int(i));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, LR(da[i]));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END DumpVec;

PROCEDURE DumpCol(nm : Pathname.T;
                  READONLY a : Array;
                  col : CARDINAL) =
  VAR
    wr := FileWr.Open(nm);
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      Wr.PutText(wr, Int(i));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, LR(a[i, col]));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END DumpCol;
  
TYPE
  Norm = RECORD
    min, max : LONGREAL;
  END;
  
PROCEDURE Normalize(VAR a : ARRAY OF LONGREAL) : Norm =
  VAR
    min := LAST(LONGREAL);
    max := FIRST(LONGREAL);
    range : LONGREAL;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      min := MIN(min, a[i]);
      max := MAX(max, a[i])
    END;
    range := max - min;

    IF range = 0.0d0 THEN range := 1.0d0 END; (* avoid divide by zero *)
    
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := (a[i] - min) / range
    END;
    RETURN Norm { min, max }
  END Normalize;

PROCEDURE Interpolate(READONLY a : ARRAY OF LONGREAL; x : LONGREAL) : LONGREAL =
  VAR
    xf := FLOOR(x);
    xc := xf + 1;
    fa := a[MIN(xf,LAST(a))];
    ca := a[MIN(xc,LAST(a))];
    cp := x - FLOAT(xf, LONGREAL);
    fp := 1.0d0 - cp;
  BEGIN
    RETURN fp * fa + cp * ca
  END Interpolate;

TYPE
  Array = ARRAY OF ARRAY OF LONGREAL;

PROCEDURE PolyCompress(fn : TEXT; READONLY a : ARRAY OF LONGREAL) =
  VAR
    dims := 2;
    n    := NUMBER(a);
    x    := NEW(REF Array, n, dims);
    response := NEW(REF Array, n, 1);
    responseHat : REF Array;
    r := NEW(Regression.T);
  BEGIN
    FOR i := 0 TO n - 1 DO
      response[i, 0] := a[i]
    END;
    MakeIndeps(x^);

    Regression.Run(x, response, responseHat, FALSE, r, h := 0.0d0);

    DumpCol(fn, responseHat^, 0)
    
  END PolyCompress;

PROCEDURE MakeIndeps(VAR a : ARRAY OF ARRAY OF LONGREAL) =
  BEGIN
    FOR ix := FIRST(a) TO LAST(a) DO
      a[ix, 0] := 1.0d0;
      WITH x  = a[ix],
           fx = FLOAT(ix, LONGREAL) DO
        FOR px := 1 TO LAST(x) DO
          x[px] := fx * x[px - 1]
        END
      END
    END
  END MakeIndeps;
  
VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  traceRt       : Pathname.T     := "xa";
  outDir        : Pathname.T     := "out";
  createOutDir  : BOOLEAN;
  trace         : Trace.T;
  
BEGIN

  TRY
    createOutDir := pp.keywordPresent("-C");

    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  TRY
    trace := NEW(Trace.T).init(traceRt)
  EXCEPT
    OSError.E(x) =>
    Debug.Error(F("Trouble opening input trace %s : OSError.E : %s",
                  traceRt, AL.Format(x)))
  |
    Rd.Failure(x) => Debug.Error("Trouble opening input trace : Rd.Failure : " & AL.Format(x))
  |
    Rd.EndOfFile =>
    Debug.Error(F("Short read opening input trace"))
  END;
  
  IF outDir # NIL THEN
    TRY
      IF createOutDir THEN
        TRY FS.CreateDirectory(outDir) EXCEPT ELSE END
      END;
      Process.SetWorkingDirectory(outDir)
    EXCEPT
      OSError.E(e) =>
      Debug.Error(F("Couldn't set working directory to \"%s\" : OSError.E : %s",
                    outDir, AL.Format(e)))
    END
  END;

  DoIt(0.005d0);
END SpiceCompress.
