MODULE LibertyComponentSeqBuilder;
IMPORT LibertyComponentSeq;
IMPORT LibertyComponent;
IMPORT LibertyAttrValSeq;
IMPORT LibertyStatementSeq;
IMPORT LibertyParamList;

PROCEDURE BuildSeq(c0, c1, c2, c3, c4, c5 : REFANY) : LibertyComponentSeq.T =
  VAR
    res := NEW(LibertyComponentSeq.T).init();
  BEGIN
    IF c0 # NIL THEN Stuff(res, c0) END;
    IF c1 # NIL THEN Stuff(res, c0) END;
    IF c2 # NIL THEN Stuff(res, c0) END;
    IF c3 # NIL THEN Stuff(res, c0) END;
    IF c4 # NIL THEN Stuff(res, c0) END;
    IF c5 # NIL THEN Stuff(res, c0) END;
    RETURN res
  END BuildSeq;

PROCEDURE Stuff(res : LibertyComponentSeq.T; x : REFANY) =
  BEGIN
    TYPECASE x OF
      NULL => <*ASSERT FALSE*>
    |
      LibertyComponent.T(c)    => res.addhi(c)
    |
      LibertyAttrValSeq.T(s)   =>
      FOR i := 0 TO s.size() - 1 DO
        VAR x := s.get(i); BEGIN res.addhi(x) END
      END
    |
      LibertyStatementSeq.T(s) =>
      FOR i := 0 TO s.size() - 1 DO
        VAR x := s.get(i); BEGIN res.addhi(x) END
      END
    |
      LibertyParamList.T(l)    => Stuff(res, l.params)
    ELSE
      <*ASSERT FALSE*>
    END
  END Stuff;

BEGIN END LibertyComponentSeqBuilder.
