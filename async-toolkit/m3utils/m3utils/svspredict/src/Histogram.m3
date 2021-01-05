MODULE Histogram;
FROM Fmt IMPORT LongReal, F, Int;
IMPORT FileWr, Wr;
IMPORT Math;
IMPORT Debug;
IMPORT Thread, OSError;

CONST LR = LongReal;

PROCEDURE Do(ofn          : TEXT;
             READONLY res : ARRAY OF LONGREAL; (* sorted *)
             low          : BOOLEAN;
             H            : CARDINAL)
  RAISES { OSError.E, Wr.Failure, Thread.Alerted } =
  VAR
    min := res[FIRST(res)];
    max := res[LAST (res)];
    hw  := (max - min) / FLOAT(H, LONGREAL);
    hcnt := NEW(REF ARRAY OF CARDINAL, H);
    hmax := 0.0d0;
    sum, sumsq := 0.0d0;
    n := FLOAT(NUMBER(res),LONGREAL);
  BEGIN
    FOR i := FIRST(hcnt^) TO LAST(hcnt^) DO
      hcnt[i] := 0
    END;
    FOR i := FIRST(res) TO LAST(res) DO
      sum := sum + res[i];
      sumsq := sumsq + res[i] * res[i];
      WITH h  = (res[i] - min) / hw,
           ht = TRUNC(h) DO
        hmax := MAX(h,hmax);
        INC(hcnt[MIN(ht,LAST(hcnt^))])
        (* the MAX is FOR a round-off possibility pushing us up... *)
      END
    END;
    
    Debug.Out(F("hmax = %s, H = %s", LR(hmax), Int(H)));
    
    (* dump the histogram *)
    WITH wr  = FileWr.Open(ofn & "_hist.dat") DO
      FOR i := FIRST(hcnt^) TO LAST(hcnt^) DO
        WITH lo = min + hw * FLOAT(i    , LONGREAL),
             hi = min + hw * FLOAT(i + 1, LONGREAL),
             c  = hcnt[i] DO
          Debug.Out(F("hist bin %s from %s to %s cnt %s",
                      Int(i),
                      LR(lo),
                      LR(hi),
                      Int(c)));
          
          Wr.PutText(wr, F("%s %s\n", LR(lo), "0.0"));
          Wr.PutText(wr, F("%s %s\n", LR(hi), "0.0"));
          Wr.PutText(wr, F("%s %s\n", LR(hi), Int(c)));
          Wr.PutText(wr, F("%s %s\n", LR(lo), Int(c)));
          Wr.PutText(wr, F("%s %s\n", LR(lo), "0.0"));
          Wr.PutText(wr, "\n");
        END
      END;
      Wr.Close(wr)
    END;
    
    WITH lwr = FileWr.Open(ofn & "_loss.dat") DO
      FOR i := FIRST(res) TO LAST(res) DO
        VAR rem : LONGREAL; BEGIN
          IF low THEN
            rem := 1.0d0 - FLOAT(i+1,LONGREAL)/ n
          ELSE
            rem := FLOAT(i+1,LONGREAL)/ n
          END;
          Wr.PutText(lwr, F("%s %s\n", LR(res[i]), LR(rem)))
        END
      END;
      Wr.Close(lwr)
    END;
    
    WITH mean   = sum   / n,
         meansq = sumsq / n,
         var    = n / (n - 1.0d0) * (meansq - mean * mean),
         sdev   = Math.sqrt(var) DO
      Debug.Out(F("mean %s sdev %s", LR(mean), LR(sdev)))
    END
  END Do;

BEGIN END Histogram.
