MODULE MELRVectorType;
IMPORT Math;
FROM GenOptUtils IMPORT FmtP;
IMPORT LRMatrix2;

PROCEDURE Format(READONLY a : T) : TEXT = BEGIN RETURN FmtP(a) END Format;

PROCEDURE Plus(READONLY a, b : T) : T =
  VAR
    res := NEW(T, NUMBER(a^));
  BEGIN
    LRMatrix2.AddV(a^, b^, res^);
    RETURN res
  END Plus;

PROCEDURE Minus(READONLY a, b : T) : T =
  VAR
    res := NEW(T, NUMBER(a^));
  BEGIN
    LRMatrix2.SubV(a^, b^, res^);
    RETURN res
  END Minus;

PROCEDURE ScalarMul(a : LONGREAL; READONLY b : T) : T =
  VAR
    res := NEW(T, NUMBER(b^));
  BEGIN
    LRMatrix2.MulSV(a, b^, res^);
    RETURN res
  END ScalarMul;

PROCEDURE Times(READONLY a, b : T) : T =
  VAR
    res := NEW(T, NUMBER(a^));
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      res[i] := a[i] * b[i]
    END;
    RETURN res
  END Times;
  

PROCEDURE Abs(READONLY a : T) : T =
  VAR
    res := NEW(T, NUMBER(a^));
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      res[i] := ABS(a[i])
    END;
    RETURN res
  END Abs;

PROCEDURE Sqrt(READONLY a : T) : T =
  VAR
    res := NEW(T, NUMBER(a^));
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      res[i] := Math.sqrt(a[i])
    END;
    RETURN res
  END Sqrt;


BEGIN END MELRVectorType.
