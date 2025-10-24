MODULE VectorSeq;
IMPORT Pathname;
IMPORT MultiEvalLRVector;
IMPORT LRMatrix2;
IMPORT LRVector;
IMPORT MELRVectorType;
IMPORT Word;

PROCEDURE NewZeroV(n : CARDINAL) : LRVector.T =
  VAR
    z := NEW(LRVector.T, n);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i] := 0.0d0
    END;
    RETURN z
  END NewZeroV;

VAR
  idMu          := NEW(MUTEX);
  idNx : Word.T := 0;

PROCEDURE ToMulti(seq        : T;
                  subdirPath : Pathname.T) : MultiEvalLRVector.Result =
  VAR
    dims := NUMBER(seq.get(0)^); 
    s, ss : LRVector.T;
    n    := seq.size();
    res : MultiEvalLRVector.Result;
  BEGIN
    res.subdirPath := subdirPath;
    FOR d := 0 TO dims - 1 DO
      s  := NewZeroV(dims);
      ss := NewZeroV(dims);
      FOR i := 0 TO seq.size() - 1 DO
        WITH x = seq.get(i) DO
          LRMatrix2.AddV(s^, x^, s^);
          LRMatrix2.AddV(ss^, MELRVectorType.Times(x, x)^, ss^)
        END
      END;
      
    END;

    LOCK idMu DO
      TRY
        <*ASSERT s # NIL*>
        <*ASSERT ss # NIL*>
        res :=  MultiEvalLRVector.Result { id         := idNx,
                                           n          := n,
                                           sum        := s,
                                           sumsq      := ss,
                                           extra      := NIL,
                                           subdirPath := subdirPath,
                                           seq        := seq }
      FINALLY
        INC(idNx)
      END
    END;
    
    RETURN res
  END ToMulti;

BEGIN END VectorSeq.
