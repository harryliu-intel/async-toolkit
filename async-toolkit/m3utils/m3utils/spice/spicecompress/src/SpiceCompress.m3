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
IMPORT Math;
IMPORT LRRegression AS Regression;
IMPORT CardSeq;
IMPORT PolySegment16, PolySegment16Seq;
IMPORT Rep16;
IMPORT LRMatrix2 AS Matrix;
IMPORT Word;
IMPORT TextWr;
IMPORT TextRd;
IMPORT PolySegment16Serial;
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
  
CONST <*NOWARN*>Ks = ARRAY OF CARDINAL { 1, 63, 64, 77, 91, 99         };
      <*NOWARN*>Km = ARRAY OF CARDINAL { 1,         77    , 99         };
      <*NOWARN*>Kq = ARRAY OF CARDINAL {            77                 };
      <*NOWARN*>Kg = ARRAY OF CARDINAL {            77        , 108091 };
      K  = Kg;

TYPE
  Evaluation = RECORD
    fails, cost     : CARDINAL;
    maxAbsDiff, rms : LONGREAL;
  END;
 
PROCEDURE Evaluate(fn            : TEXT;
                   coeffs        : CARDINAL;
                   READONLY a, b : ARRAY OF LONGREAL;
                   targMaxDev    : LONGREAL;
                   base          := 0) : Evaluation =
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
    IF fn # NIL AND doAllDumps THEN
      DumpVec(fn, e^, base)
    END;
    WITH meanSq = sumDiffSq / FLOAT(n, LONGREAL),
         rms    = Math.sqrt(meanSq) DO
      
      Debug.Out(F("%s order maxAbsDiff %s RMS %s fails %s cost %s",
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
    CONST
      MaxOrder = 5;
    VAR
      rn       := Pad(Int(i), 6, padChar := '0');
      segments := NEW(PolySegment16Seq.T).init();
      attemptOrder := MIN(DefOrder, NUMBER(darr^) - 1);
    BEGIN
      trace.getNodeData(i, darr^);
      norm := Normalize(darr^);

      DumpVec(rn & ".darr.dat", darr^, 0);

      AttemptPoly16(rn & ".poly16_0_",
                    SUBARRAY(darr^, 0, 1),
                    0,
                    targMaxDev,
                    segments,
                    0.0d0,
                    0);
           
      WITH  seg    = segments.get(0),
            firstY = Rep16.ToFloat0(seg.r.c0) DO

        <*ASSERT seg.r.order = 0*>
        <*ASSERT seg.r.count = 1*>

        (*********************  BUILD POLYS  *********************)
        
        AttemptPoly16(rn & ".poly16_",
                      darr^,
                      0,
                      targMaxDev,
                      segments,
                      firstY,
                      attemptOrder
        );

        Debug.Out(F("dirty %s : %s segments (%s/%s points)",
                    rn, Int(segments.size()), Int(PolyPoints(segments)),
                    Int(PolyPointsSerial(segments))
        ));

        Reconstruct(segments, rarr^);
        DumpVec(rn & ".rarr_dirty.dat", rarr^, 0);

        (*********************  CLEAN POLYS  *********************)

        CleanPoly16(rn, segments, darr^, targMaxDev, MaxOrder);

        Debug.Out(F("clean %s : %s segments (%s/%s points)",
                    rn, Int(segments.size()), Int(PolyPoints(segments)),
                    Int(PolyPointsSerial(segments))
        ));

        Reconstruct(segments, rarr^);
        DumpVec(rn & ".rarr_clean.dat", rarr^, 0);

        (*********************  ZERO POLYS  *********************)
        
        ZeroPoly16(rn, segments, darr^, targMaxDev);
        DumpVec(rn & ".rarr_zeroed.dat", rarr^, 0);

        Debug.Out(F("zeroed %s : %s segments (%s/%s points)",
                    rn, Int(segments.size()), Int(PolyPoints(segments)),
                    Int(PolyPointsSerial(segments))
        ));

        (* try encoding *)

        Debug.Out(F("NUMBER(darr^) %s", Int(NUMBER(darr^))));
        
        TRY
          VAR
            wr := NEW(TextWr.T).init();
            newSegments := NEW(PolySegment16Seq.T).init();
            header : Rep16.Header;
          BEGIN
            PolySegment16Serial.Write(wr, segments, norm.min, norm.max);
            
            WITH txt = TextWr.ToText(wr),
                 rd  = TextRd.New(txt) DO
              PolySegment16Serial.Read(rd, newSegments, header);
              
              Debug.Out(F("Readback of segments (wrote min=%s max=%s segments=%s) : %s",
                          LR(norm.min), LR(norm.max), Int(segments.size()),
                          Rep16.FormatHeader(header)))
            END
            
          END
        EXCEPT
          PolySegment16Serial.Error(x) =>
          Debug.Error(F("Error handling PolySegment16 serialization : %s", x))
        END
      END
    END DoOne;

  VAR
    nSteps := trace.getSteps();
    tarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    iarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    darr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    rarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    norm : Norm;
  BEGIN

    FOR p := 1 TO 3 DO
      FOR s := -5 TO 5 DO
        Debug.Out(F("ToFloat(%s, %s) = %s", Int(s), Int(p), LR(Rep16.ToFloat(s, p))))
      END
    END;
    
    (* iarr is integer timesteps *)
    Integers(iarr^);
    
    trace.getTimeData(tarr^);
    
    FOR k := FIRST(KK^) TO LAST(KK^) DO
      WITH i = KK[k] DO
        DoOne(i)
      END
    END
  END DoIt;

PROCEDURE PolyPoints(seq : PolySegment16Seq.T) : CARDINAL =
  BEGIN
    WITH last = seq.get(seq.size() - 1) DO
      RETURN last.lo + last.n
    END;
  END PolyPoints;

PROCEDURE PolyPointsSerial(seq : PolySegment16Seq.T) : CARDINAL =
  VAR
    hi := -1;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH seg = seq.get(i) DO
        IF seg.r.order = 0 THEN
          IF seg.lo # hi + 1 THEN
            Debug.Error(
                F("Segment consistency check for segment %s order 0: seg.lo (%s) # hi + 1 (%s)",
                  Int(i), Int(seg.lo), Int(hi + 1)))
          END;
          hi := hi + seg.r.count
        ELSE
          IF seg.lo # hi THEN
            Debug.Error(
                F("Segment consistency check for segment %s order %s: seg.lo (%s) # hi (%s)",
                  Int(i), Int(seg.r.order), Int(seg.lo), Int(hi)))
          END;
          hi := hi + seg.r.count - 1
          (* if order of current seg is not 0, 
             there is one point overlap w/ previous *)
        END
      END
    END;
    RETURN hi + 1
  END PolyPointsSerial;

PROCEDURE Integers(VAR a : ARRAY OF LONGREAL) =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      a[i] := FLOAT(i, LONGREAL)
    END
  END Integers;

<*NOWARN*>PROCEDURE DumpOne(nm              : Pathname.T;
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
                  READONLY da : ARRAY OF LONGREAL;
                  base        : CARDINAL) =
  VAR
    wr := FileWr.Open(nm);
  BEGIN
    FOR i := FIRST(da) TO LAST(da) DO
      Wr.PutText(wr, Int(base + i));
      Wr.PutChar(wr, ' ');
      Wr.PutText(wr, LR(da[i]));
      Wr.PutChar(wr, '\n')
    END;
    Wr.Close(wr)
  END DumpVec;

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
    min := LAST(LONGREAL);
    max := FIRST(LONGREAL);
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

<*UNUSED*>
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

PROCEDURE ZeroPoly16(fn             : TEXT;
                     VAR poly       : PolySegment16Seq.T;
                     READONLY a     : ARRAY OF LONGREAL;
                     targMaxDev     : LONGREAL) =
  CONST
    Try = ARRAY OF CARDINAL { 8, 4, 2, 1 }; (* LSBs to attempt to zero *)

  VAR
    <*NOWARN*>y : ARRAY Rep16.Count OF LONGREAL; (* 64K scratch space *)
    success : BOOLEAN;  (* did we touch the segment ? *)
    zeroBits := 0;
    
  BEGIN
    (* attempt to zero as many bits as possible *)
    FOR i := 0 TO poly.size() - 1 DO
      success := FALSE;
      
      VAR
        cur := poly.get(i);
        try : Rep16.T;
      BEGIN
        FOR o := 0 TO cur.r.order DO
          FOR bi := FIRST(Try) TO LAST(Try) DO
            WITH b    = Try[bi],
                 mask = Word.Not(Word.Minus(Word.Shift(1, b), 1)) DO
              (* 
                 cur is the current segment
                 o   is the current order to try to zero
                 b   is the current number of bits to zero 
              *)
              try := cur.r;

              IF o = 0 THEN
                try.c0   := Word.And(try.c0  , mask)
              ELSE
                try.c[o] := Word.And(try.c[o], mask)
              END;

              FOR i := 0 TO try.count - 1 DO
                y[i] := Rep16.EvalPoly(try, i)
              END;

              WITH eval = Evaluate(NIL,
                                   try.order,
                                   SUBARRAY(a, cur.lo, cur.n),
                                   SUBARRAY(y,      0, cur.n),
                                   targMaxDev,
                                   cur.lo) DO
                IF eval.fails = 0 THEN
                  success := TRUE;
                  cur.r := try;
                  INC(zeroBits, b);
                  EXIT (* skip following bits *)
                END
              END
            END
          END
        END; (* FOR o ... *)

        IF success THEN
          (* we found an improvement, finalize it *)
          poly.put(i, cur);
          FixupNextC0(cur.r, poly, i) 
        END
        
      END
    END;
    Debug.Out(F("ZeroPoly16 : zeroed %s bits", Int(zeroBits)))
  END ZeroPoly16;

PROCEDURE FixupNextC0(READONLY new : Rep16.T;
                      segs         : PolySegment16Seq.T;
                      i            : CARDINAL  (* touched this seg *)
  ) =

  (* 
     if we modify segment i of segs, we really need to call this routine

     it fixes up c0 of the next segment
  *)
  BEGIN
    IF i # segs.size() - 1 THEN
      VAR nxt := segs.get(i + 1);
      BEGIN
        IF nxt.r.order # 0 THEN
          nxt.r.c0 := Rep16.FromFloat0(Rep16.EvalPoly(new, new.count - 1));
          segs.put(i + 1, nxt)
        END
      END
    END
  END FixupNextC0;
  
PROCEDURE CleanPoly16(fn             : TEXT;
                      VAR poly       : PolySegment16Seq.T;
                      READONLY a     : ARRAY OF LONGREAL;
                      targMaxDev     : LONGREAL;
                      dims           : CARDINAL) =
  VAR
    cur, prv, pprv           : PolySegment16.T;
    new                      : Rep16.T;
    success0, success1       : BOOLEAN;
    j             := 0;
  BEGIN
    (* repeat until fixed point *)
    (* two phases:

       first phase merges to the right

       second phase pushes down power of every segment

       we repeat:
       {
         we repeat first phase until fixed point

         we repeat second phase until fixed point
       }
       until fixed point
    *)
    
    REPEAT
      success1 := FALSE;

      (* try to merge segments *)
      REPEAT 
        success0 := FALSE; 

        FOR i := 2 TO poly.size() - 1 DO
          (* attempt to merge left hand poly into current poly -- do NOT merge seg 0 *)
          cur  := poly.get(i);
          prv  := poly.get(i - 1);

          WITH xlo    = prv.lo,
               xn     = cur.lo + cur.n - xlo, (* remember cur and prv may overlap *)

               sub    = SUBARRAY(a, xlo, xn),
               (* note this is the array from one point BEFORE the 
                  first point in prv to last point in cur *)

               
               ok     = AttemptMergeRight16(F("%s.clean_%s_%s", fn, Int(i), Int(j)),
                                            sub,
                                            xlo,
                                            prv, cur, new,
                                            targMaxDev,
                                            dims) DO
            
            IF ok THEN
              Debug.Out(F("CleanPoly16 successfully merged %s -> %s", Int(i-1), Int(i)));
              cur.r  := new;
              cur.lo := xlo;
              cur.n  := xn;
              
              poly.put(i, cur);

              (* fix up c0 in next poly if need be *)
              FixupNextC0(new, poly, i);
              
              prv.n := 0;
              poly.put(i - 1, prv);
              success0 := TRUE; success1 := TRUE;
            END
          END
        END;
        
        IF success0 THEN
          (* remove any zero-length segments *)
          WITH new = NEW(PolySegment16Seq.T).init() DO
            FOR i := 0 TO poly.size() - 1 DO
              WITH e = poly.get(i) DO
                IF e.n # 0 THEN new.addhi(e) END
              END
            END;
            poly := new
          END
        END;

        EVAL PolyPointsSerial(poly); (* assert point sequence *)

        INC(j)
      UNTIL NOT success0;

      (* try to push down order of each segment *)
      REPEAT
        success0 := FALSE;

        FOR i := 1 TO poly.size() - 1 DO (* do not touch first segment *)
          cur := poly.get(i);

          WITH xlo = cur.lo,
               sub = SUBARRAY(a, xlo, cur.n),
               ok  = AttemptLowerOrder16(F("%s.cleanlow_%s_%s", fn, Int(i), Int(j)),
                                         sub,
                                         xlo,
                                         cur,
                                         new,
                                         targMaxDev) DO
            IF ok THEN
              Debug.Out(F("CleanPoly16 successfully lowered %s -> %s", Int(i), Int(i)));
              cur.r := new;
              poly.put(i, cur);
              FixupNextC0(new, poly, i);
              success0 := TRUE; success1 := TRUE
            END;

            EVAL PolyPointsSerial(poly); (* assert point sequence *)
            
          END
        END
      UNTIL NOT success0;

    UNTIL NOT success1;

    WITH firstSeg = poly.get(0) DO
      <*ASSERT firstSeg.r.order = 0*>
    END
    
  END CleanPoly16;

PROCEDURE AttemptLowerOrder16(fn                  : TEXT;
                              READONLY sub        : ARRAY OF LONGREAL;
                              base                : CARDINAL;
                              READONLY seg0       : PolySegment16.T;
                              VAR new             : Rep16.T;
                              targMaxDev          : LONGREAL) : BOOLEAN =
  BEGIN

    (* check that we have a live segment that we can actually lower the order of *)
    IF seg0.n = 0 OR seg0.r.order = 0 THEN RETURN FALSE END;

    WITH newOrder = seg0.r.order - 1 DO
      IF newOrder = 0 THEN
        WITH eval     = PolyFit16(fn & "_" & Int(newOrder),
                                  SUBARRAY(sub, 1, NUMBER(sub) - 1),
                                  targMaxDev,
                                  newOrder,
                                  base + 1,
                                  new,
                                  Rep16.ToFloat0(seg0.r.c0)) DO
          RETURN eval.fails = 0
        END
      ELSE
        WITH eval     = PolyFit16(fn & "_" & Int(newOrder),
                                  sub,
                                  targMaxDev,
                                  newOrder,
                                  base,
                                  new,
                                  Rep16.ToFloat0(seg0.r.c0)) DO
          RETURN eval.fails = 0
        END
      END
    END
      
  END AttemptLowerOrder16;
  
PROCEDURE AttemptMergeRight16(fn                  : TEXT;
                              READONLY sub        : ARRAY OF LONGREAL;
                              base                : CARDINAL;
                              READONLY seg0, seg1 : PolySegment16.T;
                              VAR new             : Rep16.T;
                              targMaxDev          : LONGREAL;
                              maxOrder            : CARDINAL) : BOOLEAN =
  (* 
     attempt to merge two poly segs, if successful, put new seg in new and return TRUE 
  *)

  PROCEDURE Try(order : CARDINAL) : BOOLEAN =
    (* note this depends on maintaining seg0.r.c0 correctly *)
    BEGIN
      WITH eval = PolyFit16(fn & "_" & Int(order),
                            sub,
                            targMaxDev,
                            order,
                            base,
                            new,
                            Rep16.ToFloat0(seg0.r.c0)) DO
        IF eval.fails = 0 THEN
          Debug.Out(F("AttemptMergeRight16.Try success seg0.order %s seg1.order %s order %s",
                      Int(seg0.r.order), Int(seg1.r.order), Int(order)));
          RETURN TRUE
        ELSE
          RETURN FALSE
        END
      END
    END Try;
    
  VAR
    maxOrder0 := MAX(seg0.r.order, seg1.r.order);
  BEGIN
    <*ASSERT seg0.n # 0*>
    <*ASSERT seg1.n # 0*>

    (* if order is same, try to merge at same order *)

    IF    seg0.r.order = seg1.r.order THEN
      (* in this case we are not changing the starting point since the order 
         cannot change *)
      IF Try(seg0.r.order) THEN RETURN TRUE END
    ELSE
      (* dont raise seg0 order from 0 *)
      IF seg0.r.order # 0 AND Try(maxOrder0) THEN RETURN TRUE END
    END;

    (* didn't succeed at existing order(s) -- again dont raise seg0 order from 0 *)
    IF seg0.r.order # 0 AND maxOrder0 # maxOrder THEN
      RETURN Try(maxOrder)
    END;

    RETURN FALSE
  END AttemptMergeRight16;

PROCEDURE GetLastX(seq : PolySegment16Seq.T) : [ -1 .. LAST(CARDINAL) ] =
  BEGIN
    IF seq.size() = 0 THEN
      RETURN -1
    ELSE
      WITH last = seq.get(seq.size() - 1) DO
        RETURN last.lo + last.n - 1
      END
    END
  END GetLastX;
  
PROCEDURE AttemptPoly16(fn         : TEXT;
                        READONLY a : ARRAY OF LONGREAL;
                        base       : CARDINAL;
                        targMaxDev : LONGREAL;
                        segments   : PolySegment16Seq.T;
                        firstY     : LONGREAL;
                        order      : Rep16.Order) =
  (* 
     this routine adds a number of segments to fit to the function,
     respecting targMaxDev and of order no more than given (targeting
     the given order, for later optimization 
  *)
  PROCEDURE CheckPostconditions() =
    BEGIN
      <*ASSERT GetLastX(segments) = base + NUMBER(a) - 1 *>
    END CheckPostconditions;
    
  VAR
    poly, poly0 : Rep16.T;
    lastX := GetLastX(segments);
  BEGIN
    Debug.Out(F("AttemptPoly16(%s), NUMBER(a)=%s", fn, Int(NUMBER(a))));

    IF NUMBER(a) = 1 THEN
      (* single point is a special case:
         we MUST fit that as a single zero-order polynomial *)

      <* ASSERT base = lastX + 1 *>
      
      segments.addhi(PolySegment16.T { Rep16.FromSingle(a[0]),
                                       base,
                                       1 });

      CheckPostconditions()
      
    ELSE
      <*ASSERT NUMBER(a) >= order + 1 *>

      (* this is our ansatz

         do a single polynomial fit at the given order, using the
         previous last point for a reference *)
      
      WITH eval0 = PolyFit16(fn & "_0"            ,
                             SUBARRAY(a, 1, NUMBER(a) - 1),
                             targMaxDev,
                             0    ,
                             base + 1,
                             poly0,
                             firstY),
           
           eval  = PolyFit16(fn & "_" & Int(order),
                             a,
                             targMaxDev,
                             order,
                             base,
                             poly ,
                             firstY) DO
        
        Debug.Out(F("AttemptPoly16(%s), fails = %s 0.fails = %s",
                    fn,
                    Int(eval.fails),
                    Int(eval0.fails)));
        
        IF    eval0.fails = 0 THEN
          <* ASSERT base + 1 = lastX + 1 *>
          segments.addhi(PolySegment16.T { poly0, base + 1, NUMBER(a) - 1 });
          CheckPostconditions()

        ELSIF eval.fails  = 0 THEN
          (* ansatz succeeded, return from here *)
          <* ASSERT base = lastX *>
          segments.addhi(PolySegment16.T { poly, base, NUMBER(a) });
          CheckPostconditions()

        ELSE

          (* ansatz failed, we need to split
             because of how the polynomials generally need to overlap,
             the midpoint is included in both polynomials

             until we get down to 2 and fail, then the midpoint is not.
          *)

          
          WITH n      = NUMBER(a) DO
            IF n = 2 THEN
              <* ASSERT base = lastX + 1 *>
              segments.addhi(PolySegment16.T { Rep16.FromSingle(a[0]),
                                               base,
                                               1 });
              segments.addhi(PolySegment16.T { Rep16.FromSingle(a[1]),
                                               base + 1,
                                               1 });
            ELSE (* n > 2 *)
              VAR
                nover2 := n DIV 2;
                n0     := nover2 + 1;
                n1     := n - nover2;
                o0     := MIN(order, n0 - 1);
                o1     := MIN(order, n1 - 1);
              BEGIN

                IF o0 = 0 THEN
                  <* ASSERT base = lastX + 1 *>
                ELSE
                  <* ASSERT base = lastX *>
                END;
                  
                WITH a0     = SUBARRAY(a,      0, n0),
                     a1     = SUBARRAY(a, nover2, n1) DO

                  AttemptPoly16(fn & "0",
                                a0,
                                base,
                                targMaxDev,
                                segments,
                                firstY,
                                o0);

                  <*ASSERT GetLastX(segments) = base + n0 - 1*>
                  
                  WITH nsegs     = segments.size(),
                       lastSeg   = segments.get(nsegs - 1),
                       lastY     = Rep16.EvalPoly(lastSeg.r,
                                                  lastSeg.r.count - 1),
                       lastXX    = GetLastX(segments),
                       nextX     = base + nover2 DO
                    
                    IF o1 = 0 THEN
                      <* ASSERT nextX = lastXX + 1 *>
                    ELSE
                      <* ASSERT nextX = lastXX *>
                    END;

                    AttemptPoly16(fn & "1",
                                  a1,
                                  nextX,
                                  targMaxDev,
                                  segments,
                                  lastY,
                                  o1
                    );

                    CheckPostconditions()
                  END
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
                    firstY         : LONGREAL
  ) : Evaluation =

  PROCEDURE Fail() : Evaluation =
    (* return utter & dismal failure *)
    BEGIN
      RETURN Evaluation { fails      := NUMBER(a),
                          cost       := NUMBER(a),
                          maxAbsDiff := LAST(LONGREAL),
                          rms        := LAST(LONGREAL) }
    END Fail;
    
  VAR
    n            := NUMBER(a);
    coeffs       := MAX(1, order);
    x            := NEW(REF Array, n, coeffs);
    (* 0th order has 1 coefficient, 1st order also, higher orders have 
       n coefficients *)
    
    response     := NEW(REF Array, n, 1);
    responseHat : REF Array;
    r            := NEW(Regression.T);
    y            := NEW(REF ARRAY OF LONGREAL, n);
  BEGIN
    (* attempt to fit a waveform of stated dimension to the data set in a *)

    (* if dims is 0, what we're seeking is a constant waveform;
       if dims is > 0, we need to use the first point to set the 0
       coefficient *)

    FOR i := 0 TO n - 1 DO
      IF order = 0 THEN
        response[i, 0] := a[i]
      ELSE
        response[i, 0] := a[i] - firstY
      END
    END;
    MakeIndeps16(x^, order);

    Debug.Out(F("PolyFit16 base=%s n=%s order=%s firstY=%s",
                Int(base),
                Int(n),
                Int(order),
                LR(firstY)));

    
    Regression.Run(x, response, responseHat, FALSE, r, h := 0.0d0);

    WITH b = r.b DO
      (* regression coefficients are b[*,0] *)
      Debug.Out("PolyFit16 coeffs:\n" & Matrix.FormatM(r.b^));

      (* build the poly attempt *)
      poly.order := order;

      IF NUMBER(a) > LAST(Rep16.Count) THEN
        (* can't make a poly that long *)
        RETURN Fail() 
      END;
      
      poly.count := NUMBER(a);

      IF order = 0 THEN
        poly.c0 := Rep16.FromFloat0(b[0,0])
      ELSE
        poly.c0 := Rep16.FromFloat0(firstY)
      END;

      poly.c  := Rep16.Zero;
      FOR i := 1 TO order DO
        poly.c[i] := Rep16.FromFloat(b[i - 1, 0], i)
      END;

      Debug.Out("PolyFit16 poly:\n" & Rep16.Format(poly))
    END;

    <*ASSERT NUMBER(y^) = poly.count*>
    FOR i := 0 TO NUMBER(y^) - 1 DO
      y[i] := Rep16.EvalPoly(poly, i)
    END;

    IF doAllDumps THEN
      DumpVec(fn & "_y.dat", y^, base)
    END;

    RETURN Evaluate(NIL,
                    coeffs,
                    a,
                    y^,
                    targMaxDev);
    
  END PolyFit16;

PROCEDURE Reconstruct(seq   : PolySegment16Seq.T;
                      VAR a : ARRAY OF LONGREAL) =
  BEGIN
    FOR j := 0 TO seq.size() - 1 DO
      WITH seg = seq.get(j) DO
        <*ASSERT seg.r.count = seg.n*>
        FOR i := 0 TO seg.n - 1 DO
          WITH y = Rep16.EvalPoly(seg.r, i) DO
            a[seg.lo + i] := y
          END
        END
      END
    END
  END Reconstruct;
  
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
  doAllDumps    : BOOLEAN;
  relPrec                        := 0.005d0;
BEGIN

  TRY

    doAllDumps := pp.keywordPresent("-dodumpall");
    
    createOutDir := pp.keywordPresent("-C");

    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;

    IF pp.keywordPresent("-w") THEN
      wf.addhi(pp.getNextInt())
    END;

    IF pp.keywordPresent("-prec") THEN
      relPrec := pp.getNextLongReal()
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

  DoIt(relPrec)
END SpiceCompress.
