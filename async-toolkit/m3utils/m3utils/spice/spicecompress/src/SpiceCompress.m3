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

CONST K = ARRAY OF CARDINAL { 1, 63, 64, 77, 91, 99 };

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
                       medTrans     : MedTrans;
                       VAR carr     : ARRAY OF LONGREAL) =
  VAR
    window : ARRAY [FIRST(dirs)..LAST(dirs)] OF CARDINAL;
    tSeq   : ARRAY [FIRST(dirs)..LAST(dirs)] OF TransitionSeq.T;
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
            
            FOR j := MAX(0,CEILING(start)) TO LAST(carr) DO
              WITH medx = FLOAT(j, LONGREAL) - start DO
                IF medx < FLOAT(2 * mw, LONGREAL) THEN
                  WITH y = Interpolate(mt^, medx) DO
                    carr[j] := carr[j] + y
                  END
                ELSE
                  carr[j] := carr[j] + mt[LAST(mt^)]
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
      
PROCEDURE DoIt() =

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

    BEGIN
      trace.getNodeData(i, darr^);
      norm := Normalize(darr^);
      
      (* darr^ is normalized *)
      
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
           
           rn     = Pad(Int(i), 5, padChar := '0'),

           carr0    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^)),
           carr1    = NEW(REF ARRAY OF LONGREAL, NUMBER(darr^)),
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

        
        BuildCartoon(tSeq, medTrans, carr0^);

        DumpOne(rn & ".carr0", iarr^, carr0^);

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

          wiarr    = NEW(REF ARRAY OF LONGREAL, n2)
          
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
    OSError.E(x) => Debug.Error("Trouble opening input trace : OSError.E : " & AL.Format(x))
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

  DoIt();
END SpiceCompress.
