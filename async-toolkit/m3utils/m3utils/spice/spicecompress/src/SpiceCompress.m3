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
IMPORT Wr, FileWr;
IMPORT Transition;
IMPORT Word;
IMPORT Math;
IMPORT LRRegression AS Regression;
IMPORT CardSeq;
IMPORT PolySegment, PolySegmentSeq;
IMPORT PolySegment16, PolySegment16Seq;
IMPORT Rep16;

IMPORT Thread;

<*FATAL Thread.Alerted*>

(* for now we turn off the other exceptions too (will fix later) *)
<*FATAL Wr.Failure*>
<*FATAL OSError.E*>
<*FATAL Rd.Failure, Rd.EndOfFile*> 

CONST
  Usage    = "";
  LR       = LongReal;
  DefOrder = 2;
  
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


TYPE
  Evaluation = RECORD
    fails, cost : CARDINAL;
    maxAbsDiff, rms : LONGREAL;
  END;
  
PROCEDURE Evaluate(fn            : TEXT;
                   coeffs        : CARDINAL;
                   READONLY a, b : ARRAY OF LONGREAL;
                   targMaxDev    : LONGREAL) : Evaluation =
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
    IF fn # NIL THEN
      DumpVec(fn, e^)
    END;
    WITH meanSq = sumDiffSq / FLOAT(n, LONGREAL),
         rms    = Math.sqrt(meanSq) DO
      
      Debug.Out(F("%s coeffs maxAbsDiff %s RMS %s fails %s cost %s",
                  Int(coeffs),
                  LR(maxAbsDiff),
                  LR(rms),
                  Int(fails),
                  Int(fails + coeffs)));
      
      RETURN Evaluation { fails,
                          fails + coeffs,
                          maxAbsDiff,
                          rms }

    END;
  END Evaluate;



<*NOWARN*>PROCEDURE Zero(VAR a : ARRAY OF LONGREAL) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := 0.0d0
    END
  END Zero;
  
PROCEDURE DoIt(targMaxDev : LONGREAL) =

  PROCEDURE DoOne(i : CARDINAL) =
    VAR
      rn       := Pad(Int(i), 6, padChar := '0');
      segments := NEW(PolySegment16Seq.T).init();
      attemptOrder := MIN(DefOrder, NUMBER(darr^) - 1);
    BEGIN
      trace.getNodeData(i, darr^);
      norm := Normalize(darr^);

      AttemptPoly16(rn & ".poly16_0_",
                    SUBARRAY(darr^, 0, 1),
                    0,
                    targMaxDev,
                    segments,
                    0,
                    0);
           
      WITH  seg    = segments.get(0),
            firstY = seg.r.c0 DO

        <*ASSERT seg.r.order = 0*>
        <*ASSERT seg.r.count = 1*>
        
        AttemptPoly16(rn & ".poly16_",
                      darr^,
                      0,
                      targMaxDev,
                      segments,
                      firstY,
                      attemptOrder
        )
      END
    END DoOne;

  VAR
    nSteps := trace.getSteps();
    tarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    iarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    darr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    norm : Norm;
  BEGIN
      
    (* iarr is integer timesteps *)
    Integers(iarr^);
    
    trace.getTimeData(tarr^);
    
    FOR k := FIRST(KK^) TO LAST(KK^) DO
      WITH i = KK[k] DO
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

<*NOWARN*>PROCEDURE DumpOne(nm : Pathname.T;
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

PROCEDURE ExtractCol(READONLY a : Array; col : CARDINAL) : REF ARRAY OF LONGREAL =
  VAR
    res := NEW(REF ARRAY OF LONGREAL, NUMBER(a));
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO
      res[i] := a[i, col]
    END;
    RETURN res
  END ExtractCol;
  
PROCEDURE DumpCol(nm : Pathname.T;
                  READONLY a : Array;
                  col : CARDINAL;
                  base : CARDINAL) =
  VAR
    wr := FileWr.Open(nm);
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      Wr.PutText(wr, Int(base + i));
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

PROCEDURE CleanPoly(fn             : TEXT;
                    VAR poly       : PolySegmentSeq.T;
                    READONLY a     : ARRAY OF LONGREAL;
                    targMaxDev     : LONGREAL;
                    dims           : CARDINAL) =
  VAR
    cur, prv : PolySegment.T;
  BEGIN
    FOR i := 1 TO poly.size() - 1 DO
      (* attempt to merge left hand poly into current poly *)
      cur := poly.get(i);
      prv := poly.get(i - 1);
      
      WITH xlo = prv.lo,
           xn  = prv.n + cur.n,
           sub = SUBARRAY(a, xlo, xn),
           r   = NEW(Regression.T),
           eval = PolyCompress(fn & ".clean" & Int(i), sub, targMaxDev, dims, xlo, r) DO
        IF eval.fails = 0 THEN
          Debug.Out(F("CleanPoly successfully merged %s -> %s", Int(i-1), Int(i)));
          cur.lo := prv.lo;
          cur.n  := xn;
          cur.r  := r;
          poly.put(i, cur);
          prv.n  := 0;
          poly.put(i - 1, prv)
        END
      END
    END;

    WITH new = NEW(PolySegmentSeq.T).init() DO
      FOR i := 0 TO poly.size() - 1 DO
        WITH e = poly.get(i) DO
          IF e.n # 0 THEN new.addhi(e) END
        END
      END;
      poly := new
    END
     
  END CleanPoly;

PROCEDURE AttemptPoly16(fn         : TEXT;
                        READONLY a : ARRAY OF LONGREAL;
                        base       : CARDINAL;
                        targMaxDev : LONGREAL;
                        segments   : PolySegment16Seq.T;
                        firstY     : Rep16.Unsigned;
                        order      : Rep16.Order) =
  (* this routine adds a number of segments to fit to the function,
     respecting targMaxDev and of order no more than given (targeting
     the given order, for later optimization *)
  VAR
    poly : Rep16.T;
  BEGIN
    Debug.Out(F("AttemptPoly16(%s), NUMBER(a)=%s", fn, Int(NUMBER(a))));

    IF NUMBER(a) = 1 THEN
      (* single point is a special case:
         we MUST fit that as a single zero-order polynomial *)
      
      segments.addhi(PolySegment16.T { Rep16.FromSingle(a[0]),
                                   base,
                                   1 })
      
    ELSE
      <*ASSERT NUMBER(a) >= order + 1 *>

      (* this is our ansatz

         do a single polynomial fit at the given order, using the
         previous last point for a reference *)
      
      WITH eval = PolyFit16(fn, a, targMaxDev, order, base, poly, firstY) DO
        Debug.Out(F("AttemptPoly16(%s), fails = %s", fn, Int(eval.fails)));
        
        IF eval.fails = 0 THEN
          (* ansatz succeeded, return from here *)
          segments.addhi(PolySegment16.T { poly, base, NUMBER(a) })
        ELSE

          (* ansatz failed, we need to split
             because of how the polynomials generally need to overlap,
             the midpoint is included in both polynomials

             until we get down to 2 and fail, then the midpoint is not.
          *)

          
          WITH n      = NUMBER(a) DO
            IF n = 2 THEN
              segments.addhi(PolySegment16.T { Rep16.FromSingle(a[0]),
                                               base,
                                               1 });
              segments.addhi(PolySegment16.T { Rep16.FromSingle(a[1]),
                                               base + 1,
                                               1 });
            ELSE
              WITH nover2 = n DIV 2,
                   n0     = nover2 + 1,
                   n1     = n - nover2,
                   o0     = MIN(order, n0 - 1),
                   o1     = MIN(order, n1 - 1),
                   a0     = SUBARRAY(a, 0, n0),
                   a1     = SUBARRAY(a, nover2, n1) DO
            
                AttemptPoly16(fn & "0",
                              a0,
                              base,
                              targMaxDev,
                              segments,
                              firstY,
                              o0);
                
                WITH nsegs     = segments.size(),
                     lastSeg   = segments.get(nsegs - 1),
                     lastY     = Rep16.EvalPoly(lastSeg.r,
                                                lastSeg.r.count - 1) DO
                  AttemptPoly16(fn & "1",
                                a1,
                                base + nover2,
                                targMaxDev,
                                segments,
                                lastY,
                                o1
                  )
                END
              END
            END
          END
        END
      END
    END
  END AttemptPoly16;
  
PROCEDURE PolyFit16(fn             : TEXT;
                    READONLY a     : ARRAY OF LONGREAL;
                    targMaxDev     : LONGREAL;
                    order          : CARDINAL;
                    base           : CARDINAL;
                    VAR poly       : Rep16.T;
                    firstY         : Rep16.Unsigned
  ) : Evaluation =
  VAR
    n        := NUMBER(a);
    coeffs   := MAX(1, order);
    x        := NEW(REF Array, n, coeffs);
    (* 0th order has 1 coefficient, 1st order also, higher orders have 
       n coefficients *)
    
    response := NEW(REF Array, n, 1);
    responseHat : REF Array;
    efn : TEXT;
    r := NEW(Regression.T);
    
  BEGIN
    (* attempt to fit a waveform of stated dimension to the data set in a *)

    (* if dims is 0, what we're seeking is a constant waveform;
       if dims is > 0, we need to use the first point to set the 0
       coefficient *)

    FOR i := 0 TO n - 1 DO
      IF order = 0 THEN
        response[i, 0] := a[i]
      ELSE
           
        response[i, 0] := a[i] - Rep16.ToFloat0(firstY)
      END
    END;
    MakeIndeps16(x^, order);

    Debug.Out(F("PolyFit16 n=%s order=%s firstY=%s (%s)",
                Int(n),
                Int(order),
                Int(firstY),
                LR(Rep16.ToFloat0(firstY))));
    
    Regression.Run(x, response, responseHat, FALSE, r, h := 0.0d0);

    IF fn # NIL THEN DumpCol(fn, responseHat^, 0, base) END;

    IF fn = NIL THEN
      efn := NIL
    ELSE
      efn := fn & ".errs"
    END;
    
    RETURN Evaluate(efn,
                    coeffs,
                    a,
                    ExtractCol(responseHat^, 0)^,
                    targMaxDev);
    
  END PolyFit16;
  
PROCEDURE PolyCompress(fn         : TEXT;
                       READONLY a : ARRAY OF LONGREAL;
                       targMaxDev : LONGREAL;
                       dims       : CARDINAL;
                       base       : CARDINAL;
                       r          : Regression.T
  ) : Evaluation =
  VAR
    n        := NUMBER(a);
    x        := NEW(REF Array, n, dims);
    response := NEW(REF Array, n, 1);
    responseHat : REF Array;
    efn : TEXT;
  BEGIN
    FOR i := 0 TO n - 1 DO
      response[i, 0] := a[i]
    END;
    MakeIndeps(x^);

    Regression.Run(x, response, responseHat, FALSE, r, h := 0.0d0);

    IF fn # NIL THEN DumpCol(fn, responseHat^, 0, base) END;

    IF fn = NIL THEN
      efn := NIL
    ELSE
      efn := fn & ".errs"
    END;
    
    RETURN Evaluate(efn,
                    dims,
                    a,
                    ExtractCol(responseHat^, 0)^,
                    targMaxDev);
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

PROCEDURE MakeIndeps16(VAR a : ARRAY OF ARRAY OF LONGREAL; order : CARDINAL) =
  BEGIN
    FOR ix := FIRST(a) TO LAST(a) DO
      IF order = 0 THEN
        a[ix, 0] := 1.0d0
      ELSE
        WITH x  = a[ix],
             fx = FLOAT(ix, LONGREAL) DO
          x[0] := fx;
          FOR px := 1 TO LAST(x) DO
            x[px] := fx * x[px - 1]
          END
        END
      END
    END
  END MakeIndeps16;

VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  traceRt       : Pathname.T     := "xa";
  outDir        : Pathname.T     := "out";
  createOutDir  : BOOLEAN;
  trace         : Trace.T;
  KK            : REF ARRAY OF CARDINAL;
  wf                             := NEW(CardSeq.T).init();
  
BEGIN

  TRY
    createOutDir := pp.keywordPresent("-C");

    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;

    IF pp.keywordPresent("-w") THEN
      wf.addhi(pp.getNextInt())
    END;

  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;

  IF wf.size() = 0 THEN
    KK := NEW(REF ARRAY OF CARDINAL, NUMBER(K));
    KK^ := K
  ELSE
    KK := NEW(REF ARRAY OF CARDINAL, wf.size());
    
    FOR i := 0 TO wf.size() - 1 DO
      KK[i] := wf.get(i)
    END
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
