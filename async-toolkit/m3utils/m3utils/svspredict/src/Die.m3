MODULE Die;
IMPORT LongReal AS LR;
IMPORT Word;
IMPORT DieSeq AS Seq;

PROCEDURE ComparePower(READONLY a, b : T) : CompRes =
  BEGIN RETURN CompareV(Value.Power, a, b) END ComparePower;
  
PROCEDURE CompareSigma(READONLY a, b : T) : CompRes =
  BEGIN RETURN CompareV(Value.Sigma, a, b) END CompareSigma;
  
PROCEDURE CompareDeltaV(READONLY a, b : T) : CompRes =
  BEGIN RETURN CompareV(Value.DeltaV, a, b) END CompareDeltaV;
  
PROCEDURE CompareV(value : Value; READONLY a, b : T) : CompRes =
  BEGIN RETURN LR.Compare(a[value], b[value]) END CompareV;
    
PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;
  
PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      res := Word.Plus(res, LR.Hash(a[i]))
    END;
    RETURN res
  END Hash;

PROCEDURE ExtractValues(READONLY arr : Array; val : Value) : REF ARRAY OF LONGREAL=
  VAR
    res := NEW(REF ARRAY OF LONGREAL, NUMBER(arr));
  BEGIN
    FOR i := FIRST(arr) TO LAST(arr) DO
      res[i] := arr[i][val]
    END;
    RETURN res
  END ExtractValues;
  
PROCEDURE CutoffArr(READONLY a : Array;
                    value      : Value;
                    max        : LONGREAL ) : REF Array =
  VAR
    seq := NEW(Seq.T).init();
    res : REF Array;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      WITH p = a[i] DO
        IF p[value] <= max THEN
          seq.addhi(p)
        END
      END
    END;
    res := NEW(REF Array, seq.size());
    FOR i := 0 TO seq.size() - 1 DO
      res[i] := seq.get(i)
    END;
    RETURN res
  END CutoffArr;

BEGIN END Die.
