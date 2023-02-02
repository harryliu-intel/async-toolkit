MODULE SpiceValues;
IMPORT CheckDir;
IMPORT CardSeq;
IMPORT Debug;
FROM Fmt IMPORT Bool, LongReal, F, Int;
IMPORT TransitionFinder;

CONST LR = LongReal;

VAR Verbose := Debug.DebugThis("SpiceValues");
    
PROCEDURE FindIdxBefore(READONLY a : ARRAY OF LONGREAL;
                        tgt        : LONGREAL) : CARDINAL =
  VAR
    lo := 0;
    hi := NUMBER(a);
  BEGIN
    WHILE lo < hi DO
      WITH i  = lo + (hi - lo) DIV 2,
           ai = a[i] DO
        IF tgt = ai THEN
          RETURN i
        ELSIF tgt > ai THEN
          lo := i + 1
        ELSE
          hi := i
        END
      END
    END;
    <*ASSERT lo = hi*>
    WITH res = lo - 1 DO
      <*ASSERT res <= LAST(a)*>

      <*ASSERT a[res] <= tgt*>

      <*ASSERT res = LAST(a) OR a[res + 1] >= tgt*>
      (*note that a[res + 1] > tgt is more correct, but would depend
        on making an assumption on the contents of a*)
      
      RETURN res
    END
  END FindIdxBefore;

PROCEDURE GetValues(clkDirs           : CheckDir.T;
                    clkTrIdx          : CARDINAL;
                    clkNm             : TEXT;
                    thresh            : LONGREAL;
                    READONLY ta, da   : ARRAY OF LONGREAL;
                    tranFinder        : TransitionFinder.T;
                    resetTime         : LONGREAL) : CardSeq.T =
  BEGIN
    WITH res = NEW(CardSeq.T).init(),
         clkTrans = TransitionFinder.FilterTime(
                            tranFinder.forNode(clkTrIdx),
                            resetTime, LAST(LONGREAL)) DO
      Debug.Out(F("GetValues : clk  %s : %s active transitions",
                  clkNm, Int(clkTrans.size())));

      FOR i := 0 TO clkTrans.size() - 1 DO
        WITH ct = clkTrans.get(i) DO
          IF ct.dir IN clkDirs THEN
            WITH t      = ct.at,
                 dtidx  = FindIdxBefore(ta, t),
                 dval   = da[dtidx],
                 dlogic = dval > thresh DO
              IF Verbose THEN
                Debug.Out(F("GetValues at %s dtidx %s dval %s dlogic %5s (thresh %s)",
                            LR(t), Int(dtidx), LR(dval), Bool(dlogic), LR(thresh)))
              END;
              
              IF dlogic THEN
                res.addhi(1)
              ELSE
                res.addhi(0)
              END
            END
          END
        END
      END;
      RETURN res
    END
  END GetValues;

BEGIN END SpiceValues.
