MODULE TransitionPickler;
IMPORT TraceOpClass;
IMPORT Rd;
IMPORT TransitionFinder;
IMPORT Transition;
IMPORT Debug;
FROM Fmt IMPORT F, Int;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    eval := TPEval;
  END;

PROCEDURE TPEval(tp : T)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  CONST
    doDebug = TRUE;
  BEGIN
    IF tp.time.result = NIL THEN
      tp.time.trace := tp.trace;
      tp.time.eval()
    END;
    IF tp.of.result = NIL THEN
      tp.of.trace := tp.trace;
      tp.of.eval();
    END;
    
    (* tp.of.result should be valid here *)
    <*ASSERT tp.time.result # NIL*>
    <*ASSERT tp.of.result # NIL*>

    (* build transition sequence and assign to tp.result *)

    WITH seq = TransitionFinder.Find(tp.time.result^,
                                     tp.of.result^,
                                     tp.thresh,
                                     tp.hysteresis,
                                     doSlew := TRUE) DO
      IF doDebug THEN
        VAR
          tstr := "";
        BEGIN
          IF seq.size() = 0 THEN
            tstr := "not found"
          ELSE
            tstr := Transition.Format(seq.get(0))
          END;
          Debug.Out(F("TPEval, idx %s : %s transitions, first %s",
                      Int(tp.idx),
                      Int(seq.size()),
                      tstr))
        END
      END;
      
      tp.result := seq
    END
  END TPEval;

BEGIN END TransitionPickler.
