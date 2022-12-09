MODULE SpiceCompress;
IMPORT Debug;
IMPORT OSError;
IMPORT Rd;
IMPORT Pathname;
FROM Fmt IMPORT F, Int, LongReal, Pad;
IMPORT Trace;
IMPORT Wr, FileWr;
IMPORT Math;
IMPORT LRRegression AS Regression;
IMPORT PolySegment16, PolySegment16Seq;
IMPORT Rep16;
IMPORT Matrix AS MatrixE;
IMPORT LRMatrix2 AS Matrix;
IMPORT Word;
IMPORT TextWr;
IMPORT TextRd;
IMPORT PolySegment16Serial;
IMPORT Thread;
IMPORT Text;
IMPORT Time;
IMPORT Env;
IMPORT TripleRefTbl;
IMPORT IntTriple;

<*FATAL Thread.Alerted*>

CONST
  LR       = LongReal;
  DefOrder = 2;

VAR
  DoDebug := Debug.DebugThis("SpiceCompress");
  AssertAll := Env.Get("SpiceCompressAssert") # NIL;
  
TYPE
  Evaluation = RECORD
    fails, cost     : CARDINAL;
    maxAbsDiff, rms : LONGREAL;
  END;
 
PROCEDURE Evaluate(fn            : TEXT;
                   coeffs        : CARDINAL;
                   READONLY a, b : ARRAY OF LONGREAL;
                   targMaxDev    : LONGREAL;
                   doAllDumps    : BOOLEAN;
                   base          := 0) : Evaluation =
  <*FATAL Wr.Failure, OSError.E*>
  VAR
    maxAbsDiff   := 0.0d0;
    sumDiff      := 0.0d0;
    sumDiffSq    := 0.0d0;
    n            := NUMBER(a);
    e            : REF ARRAY OF LONGREAL;
    fails        := 0;
    doDump       := fn # NIL AND doAllDumps;
  BEGIN
    IF doDump THEN
      e := NEW(REF ARRAY OF LONGREAL, n);
    END;
    
    <*ASSERT NUMBER(b) >= NUMBER(a)*>
    FOR i := FIRST(a) TO LAST(a) DO
      IF doDump THEN
        e[i] := a[i] - b[i]
      END;
      
      WITH diff    = a[i] - b[i],
           absDiff = ABS(diff),
           diffSq  = diff * diff DO
        sumDiff := sumDiff + diff;
        maxAbsDiff := MAX(absDiff, maxAbsDiff);
        IF absDiff > targMaxDev THEN INC(fails) END;
        sumDiffSq := sumDiffSq + diffSq
      END
    END;
    IF doDump THEN
      DumpVec(fn, e^, base)
    END;
    WITH meanSq = sumDiffSq / FLOAT(n, LONGREAL),
         rms    = Math.sqrt(meanSq) DO

      IF DoDebug THEN
        Debug.Out(F("%s order maxAbsDiff %s RMS %s fails %s cost %s",
                    Int(coeffs),
                    LR(maxAbsDiff),
                    LR(rms),
                    Int(fails),
                    Int(fails + coeffs)))
      END;
      
      RETURN Evaluation { fails,
                          fails + coeffs,
                          maxAbsDiff,
                          rms }

    END;
  END Evaluate;

PROCEDURE CompressArray(rn         : TEXT; (* for debug *)

                        VAR darr   : ARRAY OF LONGREAL;
                        (* input data---will be normalized in place *)

                        VAR rarr   : ARRAY OF LONGREAL;
                        (* workspace *)
                        
                        targMaxDev : LONGREAL;
                        doAllDumps : BOOLEAN;
                        wr         : Wr.T;
                        VAR norm   : Norm;
                        mem        : TripleRefTbl.T;
                        doDump     : BOOLEAN;
  )
  RAISES { MatrixE.Singular } =
  <*FATAL OSError.E, Wr.Failure*>
  CONST
    MaxOrder = 5;
  VAR
    segments     := NEW(PolySegment16Seq.T).init();
    attemptOrder := MIN(DefOrder, NUMBER(darr) - 1);
  BEGIN
    norm := Normalize(darr);

    IF doDump THEN
      DumpVec(rn & ".darr.dat", darr, 0)
    END;

    AttemptPoly16(rn & ".poly16_0_",
                  SUBARRAY(darr, 0, 1),
                  0,
                  targMaxDev,
                  segments,
                  0.0d0,
                  0,
                  doAllDumps,
                  mem);
    
    WITH  seg    = segments.get(0),
          firstY = Rep16.ToFloat0(seg.r.c0) DO

      <*ASSERT seg.r.order = 0*>
      <*ASSERT seg.r.count = 1*>

      (*********************  BUILD POLYS  *********************)
      
      AttemptPoly16(rn & ".poly16_",
                    darr,
                    0,
                    targMaxDev,
                    segments,
                    firstY,
                    attemptOrder,
                    doAllDumps,
                    mem
      );

      IF DoDebug THEN
        Debug.Out(F("dirty %s : %s segments (%s/%s points)",
                    rn, Int(segments.size()), Int(PolyPoints(segments)),
                    Int(PolyPointsSerial(segments))
        ))
      END;

      Reconstruct(segments, rarr);

      IF doDump THEN
        DumpVec(rn & ".rarr_dirty.dat", rarr, 0)
      END;

      (*********************  CLEAN POLYS  *********************)

      CleanPoly16(rn, segments, darr, targMaxDev, MaxOrder, doAllDumps, mem);

      IF DoDebug THEN
        Debug.Out(F("clean %s : %s segments (%s/%s points)",
                    rn, Int(segments.size()), Int(PolyPoints(segments)),
                    Int(PolyPointsSerial(segments))
        ))
      END;

      Reconstruct(segments, rarr);
      IF doDump THEN
        DumpVec(rn & ".rarr_clean.dat", rarr, 0)
      END;

      (*********************  ZERO POLYS  *********************)

      ZeroPoly16(segments, darr, targMaxDev, doAllDumps);

      IF doDump THEN
        DumpVec(rn & ".rarr_zeroed.dat", rarr, 0)
      END;

      IF DoDebug THEN

        (***********   produce some detailed debug info   ***********)
        
        Debug.Out(F("zeroed %s : %s segments (%s/%s points)",
                    rn, Int(segments.size()), Int(PolyPoints(segments)),
                    Int(PolyPointsSerial(segments))
        ));

        (* try encoding *)
        
        Debug.Out(F("NUMBER(darr) %s", Int(NUMBER(darr))));
        
        TRY
          <*FATAL Wr.Failure, Rd.Failure, Rd.EndOfFile*>
          VAR
            wr := NEW(TextWr.T).init();
            newSegments := NEW(PolySegment16Seq.T).init();
            header : Rep16.Header;
          BEGIN
            PolySegment16Serial.Write(wr, segments, norm.min, norm.max);
            
            WITH txt = TextWr.ToText(wr),
                 ent = ComputeEntropy(txt),
                 rd  = TextRd.New(txt) DO
              PolySegment16Serial.Read(rd, newSegments, header);
              
              Debug.Out(F("Readback of segments (wrote min=%s max=%s segments=%s) : %s, entropy %s",
                          LR(norm.min), LR(norm.max), Int(segments.size()),
                          Rep16.FormatHeader(header),
                          LR(ent)))
            END
            
          END
        EXCEPT
          PolySegment16Serial.Error(x) =>
          Debug.Error(F("Error handling PolySegment16 serialization : %s", x))
        END
      END;

      IF wr # NIL THEN
        PolySegment16Serial.Write(wr, segments, norm.min, norm.max);
      END
      
    END
  END CompressArray;
  
PROCEDURE DoDemo(targMaxDev : LONGREAL;
                 KK         : REF ARRAY OF CARDINAL;
                 trace      : Trace.T;
                 doAllDumps : BOOLEAN) =

<*FATAL Rd.Failure, Rd.EndOfFile*> 
<*FATAL MatrixE.Singular*>
  VAR
    nSteps := trace.getSteps();
    darr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    rarr   := NEW(REF ARRAY OF LONGREAL, nSteps);
    norm   : Norm;
  BEGIN

    FOR p := 1 TO 3 DO
      FOR s := -5 TO 5 DO
        Debug.Out(F("ToFloat(%s, %s) = %s", Int(s), Int(p), LR(Rep16.ToFloat(s, p))))
      END
    END;
    
    FOR k := FIRST(KK^) TO LAST(KK^) DO
      WITH i  = KK[k],
           rn = Pad(Int(i), 6, padChar := '0')
       DO
        Debug.Out("Loading trace");
        trace.getNodeData(i, darr^);
        Debug.Out("Compressing trace");
        WITH now = Time.Now() DO
          CompressArray(rn, darr^, rarr^, targMaxDev, doAllDumps, norm := norm,
                        wr := NIL, mem := NIL, doDump := TRUE);
          Debug.Out("Done compressing trace, time " & LR(Time.Now() - now));
        END
      END
    END
  END DoDemo;

PROCEDURE PolyPoints(seq : PolySegment16Seq.T) : CARDINAL =
  BEGIN
    WITH last = seq.get(seq.size() - 1) DO
      RETURN last.lo + last.n
    END;
  END PolyPoints;

PROCEDURE AssertLinkage(seq : PolySegment16Seq.T; i : CARDINAL) =
  BEGIN
    WITH seg = seq.get(i) DO
      IF    i         = 0 THEN
        <*ASSERT seg.r.order = 0*>
      ELSE
        WITH prv = seq.get(i - 1) DO
          IF DoDebug THEN
            Debug.Out(F("AssertLinkage prv.lo %s prv.n %s seg.r.order %s seg.lo %s",
                        Int(prv.lo), Int(prv.n), Int(seg.r.order), Int(seg.lo)))
          END;
          
          IF seg.r.order = 0 THEN
            <*ASSERT seg.lo = prv.lo + prv.n*>
          ELSE
            <*ASSERT seg.lo = prv.lo + prv.n - 1*>
          END
        END
      END
    END
  END AssertLinkage;
  
PROCEDURE PolyPointsSerial(seq : PolySegment16Seq.T) : CARDINAL =
  VAR
    hi := -1;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH seg = seq.get(i) DO
        <*ASSERT seg.n = seg.r.count*>
        IF seg.r.order = 0 THEN
          IF seg.lo # hi + 1 THEN
            Debug.Error(
                F("Segment consistency check (A) for segment %s order 0: seg.lo (%s) # hi + 1 (%s)",
                  Int(i), Int(seg.lo), Int(hi + 1)))
          END;
          hi := hi + seg.r.count
        ELSE
          IF seg.lo # hi THEN
            Debug.Error(
                F("Segment consistency check (B) for segment %s order %s: seg.lo (%s) # hi (%s)",
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

<*NOWARN*>PROCEDURE DumpOne(nm              : Pathname.T;
                            READONLY ta, da : ARRAY OF LONGREAL)
  RAISES { OSError.E, Wr.Failure } =
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
                  base        : CARDINAL) 
  RAISES { OSError.E, Wr.Failure } =
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

PROCEDURE ZeroPoly16(VAR poly       : PolySegment16Seq.T;
                     READONLY a     : ARRAY OF LONGREAL;
                     targMaxDev     : LONGREAL;
                     doAllDumps     : BOOLEAN) =
  CONST
    Try = ARRAY OF CARDINAL { 8, 6, 4, 2, 1 }; (* LSBs to attempt to zero *)

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
                                   doAllDumps,
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
    IF DoDebug THEN
      Debug.Out(F("ZeroPoly16 : zeroed %s bits", Int(zeroBits)))
    END
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
                      dims           : CARDINAL;
                      doAllDumps     : BOOLEAN;
                      mem            : TripleRefTbl.T)
  RAISES { MatrixE.Singular } =

  PROCEDURE RemoveZeroLengthSegments() =
    BEGIN
      (* remove any zero-length segments *)
      WITH new = NEW(PolySegment16Seq.T).init() DO
        FOR i := 0 TO poly.size() - 1 DO
          WITH e = poly.get(i) DO
            IF e.n # 0 THEN new.addhi(e) END
          END
        END;
        poly := new
      END
    END RemoveZeroLengthSegments;
    
  VAR
    cur, prv                        : PolySegment16.T;
    new                             : Rep16.T;
    success0, success1, success2    : BOOLEAN;
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
                                            dims,
                                            doAllDumps,
                                            mem) DO
            
            IF ok THEN
              IF DoDebug THEN
                Debug.Out(F("CleanPoly16 successfully merged %s -> %s", Int(i-1), Int(i)))
              END;
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
          RemoveZeroLengthSegments()
        END;

        IF AssertAll THEN
          EVAL PolyPointsSerial(poly) (* assert point sequence *)
        END;
        
        success2 := FALSE;
        FOR i := 2 TO poly.size() - 1 DO
          (* attempt to merge order-0 left hand poly into current poly 
             -- do NOT merge seg 0 *)
          prv  := poly.get(i - 1);
          cur  := poly.get(i);

          IF prv.r.order = 0 THEN
            WITH pprv  = poly.get(i - 2),
                 xlo   = prv.lo - 1,  (* total length of relevant points *)
                 xn    = cur.lo + cur.n - xlo,
                 sub   = SUBARRAY(a, xlo, xn),
                 lastY = Rep16.EvalPoly(pprv.r, pprv.r.count - 1),
                 ok    = AttemptLift0Right16(F("%s.lift_%s_%s", fn, Int(i), Int(j)),
                                             sub,
                                             xlo + 1, (* start of prv *)
                                             prv, cur,
                                             new,
                                             dims,
                                             targMaxDev,
                                             lastY,
                                             doAllDumps,
                                             mem)
             DO
              IF ok THEN
                IF DoDebug THEN
                  Debug.Out(F("CleanPoly16 successfully lift-merged %s -> %s", Int(i-1), Int(i)))
                END;
                cur.r  := new;
                cur.lo := xlo;
                cur.n  := xn;
                
                poly.put(i, cur);
                
                (* fix up c0 in next poly if need be *)
                FixupNextC0(new, poly, i);
                
                prv.n := 0;
                poly.put(i - 1, prv);
                success0 := TRUE; success1 := TRUE; success2 := TRUE;
              END
            END
          END
        END;
        
        IF success2 THEN
          RemoveZeroLengthSegments()
        END;

        IF AssertAll THEN
          EVAL PolyPointsSerial(poly) (* assert point sequence *)
        END;

        INC(j)
      UNTIL NOT success0;

      (* try to push down order of each segment *)
      REPEAT
        success0 := FALSE;

        FOR i := 1 TO poly.size() - 1 DO (* do not touch first segment *)
          cur := poly.get(i);

          IF AssertAll THEN
            AssertLinkage(poly, i)
          END;
          
          WITH xlo = cur.lo,
               sub = SUBARRAY(a, xlo, cur.n),
               ok  = AttemptLowerOrder16(F("%s.cleanlow_%s_%s", fn, Int(i), Int(j)),
                                         sub,
                                         xlo,
                                         cur,
                                         new,
                                         targMaxDev,
                                         doAllDumps,
                                         mem) DO
            IF ok THEN
              IF DoDebug THEN
                Debug.Out(F("CleanPoly16 successfully lowered %s -> %s / order %s -> %s",
                            Int(i), Int(i), Int(cur.r.order), Int(new.order)))
              END;
              cur.r := new;
              IF new.order = 0 THEN
                INC(cur.lo);
                DEC(cur.n)
              END;
              poly.put(i, cur);
              FixupNextC0(new, poly, i);
              success0 := TRUE; success1 := TRUE
            END;

            IF AssertAll THEN
              AssertLinkage(poly, i);
              EVAL PolyPointsSerial(poly) (* assert point sequence *)
            END
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
                              targMaxDev          : LONGREAL;
                              doAllDumps          : BOOLEAN;
                              mem                 : TripleRefTbl.T) : BOOLEAN
  RAISES { MatrixE.Singular } =
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
                                  Rep16.ToFloat0(seg0.r.c0),
                                  doAllDumps,
                                  mem) DO
          RETURN eval.fails = 0
        END
      ELSE
        WITH eval     = PolyFit16(fn & "_" & Int(newOrder),
                                  sub,
                                  targMaxDev,
                                  newOrder,
                                  base,
                                  new,
                                  Rep16.ToFloat0(seg0.r.c0),
                                  doAllDumps,
                                  mem) DO
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
                              maxOrder            : CARDINAL;
                              doAllDumps          : BOOLEAN;
                              mem                 : TripleRefTbl.T) : BOOLEAN
  RAISES { MatrixE.Singular } =
  (* 
     attempt to merge two poly segs, if successful, put new seg in new and return TRUE 
  *)

  PROCEDURE Try(order : CARDINAL) : BOOLEAN
    RAISES { MatrixE.Singular } =
    (* note this depends on maintaining seg0.r.c0 correctly *)
    BEGIN
      WITH eval = PolyFit16(fn & "_" & Int(order),
                            sub,
                            targMaxDev,
                            order,
                            base,
                            new,
                            Rep16.ToFloat0(seg0.r.c0),
                            doAllDumps,
                            mem) DO
        IF eval.fails = 0 THEN
          IF DoDebug THEN
            Debug.Out(F("AttemptMergeRight16.Try success seg0.order %s seg1.order %s order %s",
                        Int(seg0.r.order), Int(seg1.r.order), Int(order)))
          END;
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

PROCEDURE AttemptLift0Right16(fn                  : TEXT;
                              READONLY sub        : ARRAY OF LONGREAL;
                              (* goes one left of seg0 *)
                              
                              base                : CARDINAL;
                              (* points to one right of left edge of sub *)
                              
                              READONLY seg0, seg1 : PolySegment16.T;
                              VAR new             : Rep16.T;
                              order               : CARDINAL;
                              targMaxDev          : LONGREAL;
                              lastY               : LONGREAL;
                              doAllDumps          : BOOLEAN;
                              mem                 : TripleRefTbl.T) : BOOLEAN
  RAISES { MatrixE.Singular } =
  (* 
     attempt to merge two poly segs, where seg0 has order 0, to maxOrder,
     if successful, put new seg in new and return TRUE 
  *)
    
  BEGIN
    <*ASSERT seg0.r.order = 0*>
    <*ASSERT order # 0*>
    <*ASSERT seg0.n # 0*>
    <*ASSERT seg1.n # 0*>

    WITH eval = PolyFit16(fn & "_" & Int(order),
                          sub,
                          targMaxDev,
                          order,
                          base - 1,
                          new,
                          lastY,
                          doAllDumps,
                          mem) DO
      IF eval.fails = 0 THEN
        IF DoDebug THEN
          Debug.Out(F("AttemptLift0Right success seg0.order %s seg1.order %s order %s",
                      Int(seg0.r.order), Int(seg1.r.order), Int(order)))
        END;
        RETURN TRUE
      ELSE
        RETURN FALSE
      END
    END
  END AttemptLift0Right16;

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
                        order      : Rep16.Order;
                        doAllDumps : BOOLEAN;
                        mem        : TripleRefTbl.T)
  RAISES { MatrixE.Singular } =
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
    IF DoDebug THEN
      Debug.Out(F("AttemptPoly16(%s), NUMBER(a)=%s", fn, Int(NUMBER(a))))
    END;

    IF NUMBER(a) = 1 THEN
      (* single point is a special case:
         we MUST fit that as a single zero-order polynomial *)

      <* ASSERT base = lastX + 1 *>
      
      segments.addhi(PolySegment16.T { Rep16.FromSingle(a[0]),
                                       base,
                                       1 });
      IF AssertAll THEN
        CheckPostconditions()
      END
      
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
                             firstY,
                             doAllDumps,
                             mem),
           
           eval  = PolyFit16(fn & "_" & Int(order),
                             a,
                             targMaxDev,
                             order,
                             base,
                             poly ,
                             firstY,
                             doAllDumps,
                             mem) DO

        IF DoDebug THEN
          Debug.Out(F("AttemptPoly16(%s), fails = %s 0.fails = %s",
                      fn,
                      Int(eval.fails),
                      Int(eval0.fails)))
        END;
        
        IF    eval0.fails = 0 THEN
          <* ASSERT base + 1 = lastX + 1 *>
          segments.addhi(PolySegment16.T { poly0, base + 1, NUMBER(a) - 1 });
          IF AssertAll THEN
            CheckPostconditions()
          END

        ELSIF eval.fails  = 0 THEN
          (* ansatz succeeded, return from here *)
          <* ASSERT base = lastX *>
          segments.addhi(PolySegment16.T { poly, base, NUMBER(a) });

          IF AssertAll THEN
            CheckPostconditions()
          END
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
                                o0,
                                doAllDumps,
                                mem);

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
                                  o1,
                                  doAllDumps,
                                  mem
                    );

                    IF AssertAll THEN
                      CheckPostconditions()
                    END
                  END
                END
              END
            END
          END
        END
      END
    END
  END AttemptPoly16;

TYPE Maker1 = PROCEDURE(sz : CARDINAL) : REFANY;
TYPE Maker2 = PROCEDURE(sz0, sz1 : CARDINAL) : REFANY;

     (* 
        tag is used to ensure there are enough of the type for all possible
        call sites that are simultaneously active,

        each call site should use its own tag 

        This module is single-threaded, but re-entrant as long as you use multiple
        mems, one for each thread (or no mem at all).
     *)
     
PROCEDURE GetMem1(maker       : Maker1;
                  sz          : CARDINAL;
                  tag         : CARDINAL;
                  mem         : TripleRefTbl.T
  ) : REFANY =
  VAR
    res : REFANY;
  BEGIN
    IF mem = NIL THEN
      res := maker(sz)
    ELSE
      WITH ident = IntTriple.T { sz, 0, tag } DO
        IF NOT mem.get(ident, res) THEN
          res := maker(sz);
          EVAL mem.put(ident, res)
        END
      END
    END;
    RETURN res
  END GetMem1;
                 
PROCEDURE GetMem2(maker       : Maker2;
                  sz0, sz1    : CARDINAL;
                  tag         : CARDINAL;
                  mem         : TripleRefTbl.T
  ) : REFANY =
  VAR
    res : REFANY;
  BEGIN
    IF mem = NIL THEN
      res := maker(sz0, sz1)
    ELSE
      WITH ident = IntTriple.T { sz0, sz1, tag } DO
        IF NOT mem.get(ident, res) THEN
          res := maker(sz0, sz1);
          EVAL mem.put(ident, res)
        END
      END
    END;
    RETURN res
  END GetMem2;

PROCEDURE MkArray2(sz0, sz1 : CARDINAL) : REFANY =
  BEGIN
    RETURN NEW(REF Array, sz0, sz1)
  END MkArray2;
                 
PROCEDURE MkLRArray1(sz : CARDINAL) : REFANY =
  BEGIN
    RETURN NEW(REF ARRAY OF LONGREAL, sz);
  END MkLRArray1;

PROCEDURE PolyFit16(fn             : TEXT;
                    READONLY a     : ARRAY OF LONGREAL;
                    targMaxDev     : LONGREAL;
                    order          : CARDINAL;
                    base           : CARDINAL;
                    VAR poly       : Rep16.T;
                    firstY         : LONGREAL;
                    doAllDumps     : BOOLEAN;
                    mem            : TripleRefTbl.T
  ) : Evaluation
  RAISES { MatrixE.Singular } =

  PROCEDURE Fail() : Evaluation =
    (* return utter & dismal failure *)
    BEGIN
      RETURN Evaluation { fails      := NUMBER(a),
                          cost       := NUMBER(a),
                          maxAbsDiff := LAST(LONGREAL),
                          rms        := LAST(LONGREAL) }
    END Fail;

  <*FATAL Wr.Failure, OSError.E*>
  VAR
    n            := NUMBER(a);
    coeffs       := MAX(1, order);
    x            : REF Array := GetMem2(MkArray2, n, coeffs, 0, mem);
    (* 0th order has 1 coefficient, 1st order also, higher orders have 
       n coefficients *)
    
    response     : REF Array := GetMem2(MkArray2, n, 1, 1, mem);
    responseHat : REF Array;
    r            := NEW(Regression.T);
    y            : REF ARRAY OF LONGREAL := GetMem1(MkLRArray1, n, 0, mem);
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

    IF DoDebug THEN
      Debug.Out(F("PolyFit16 base=%s n=%s order=%s firstY=%s",
                  Int(base),
                  Int(n),
                  Int(order),
                  LR(firstY)))
    END;

    
    Regression.Run(x, response, responseHat, FALSE, r, h := 0.0d0);

    WITH b = r.b DO
      (* regression coefficients are b[*,0] *)
      IF DoDebug THEN
        Debug.Out("PolyFit16 coeffs:\n" & Matrix.FormatM(r.b^))
      END;

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

      IF DoDebug THEN
        Debug.Out("PolyFit16 poly:\n" & Rep16.Format(poly))
      END
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
                    targMaxDev,
                    doAllDumps);
    
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

PROCEDURE ComputeEntropy(txt : TEXT) : LONGREAL =
  VAR
    ent  := 0.0d0;
    occ  := ARRAY CHAR OF CARDINAL { 0, .. };
    n    := Text.Length(txt);
    nf   := FLOAT(n, LONGREAL);
    log2 := Math.log(2.0d0);
  BEGIN
    FOR i := 0 TO n - 1 DO
      WITH c = Text.GetChar(txt, i) DO
        INC(occ[c])
      END
    END;

    FOR c := FIRST(occ) TO LAST(occ) DO
      WITH p = FLOAT(occ[c],LONGREAL) / nf DO
        IF p # 0.0d0 THEN
          ent := ent - p * Math.log(p) / log2
        END
      END
    END;
    
    RETURN ent / 8.0d0
  END ComputeEntropy;

BEGIN END SpiceCompress.
