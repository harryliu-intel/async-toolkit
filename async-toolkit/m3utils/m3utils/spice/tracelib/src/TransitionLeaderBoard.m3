MODULE TransitionLeaderBoard;
IMPORT Transition;
IMPORT TraceOpClass;
IMPORT Rd;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT TransitionArraySort;
IMPORT TransitionSeq;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    eval := TLBEval;
  END;

PROCEDURE SeqToArr(seq       : TransitionSeq.T;
                   resetTime : LONGREAL;
                   maxCount  : CARDINAL) : REF ARRAY OF Transition.T =
  VAR
    startIdx := 0;
  BEGIN
    FOR i := 0 TO seq.size() - 1 DO
      WITH tr = seq.get(i) DO
        IF tr.at < resetTime THEN
          startIdx := i + 1
        ELSE
          EXIT
        END
      END
    END;

    WITH nTrans = seq.size() - startIdx,
         nKeep  = MIN(nTrans, maxCount),
         arr    = NEW(REF ARRAY OF Transition.T, nKeep) DO
      FOR i := FIRST(arr^) TO LAST(arr^) DO
        arr[i] := seq.get(i + startIdx)
      END;
      RETURN arr
    END
  END SeqToArr;

PROCEDURE TLBEval(tlb : T)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  CONST
    doDebug = TRUE;
  BEGIN
    IF tlb.transitions.result = NIL THEN
      tlb.transitions.trace := tlb.trace;
      tlb.transitions.eval()
    END;
    <*ASSERT tlb.transitions.result # NIL*>
    WITH seq = NARROW(tlb.transitions.result, TransitionSeq.T),
         arr = SeqToArr(seq, tlb.resetTime, tlb.maxCount) DO
      IF doDebug THEN
        Debug.Out(F("TLBEval, %s transitions", Int(NUMBER(arr^))))
      END;
      TransitionArraySort.Sort(arr^, cmp := Transition.CompareBySlew);
      tlb.result := NEW(TLB, idx := tlb.transitions.idx, arr := arr)
    END
  END TLBEval;
  
BEGIN END TransitionLeaderBoard.
