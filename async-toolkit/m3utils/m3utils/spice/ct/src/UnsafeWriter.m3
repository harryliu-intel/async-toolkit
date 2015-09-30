UNSAFE MODULE UnsafeWriter;
IMPORT Wr;

PROCEDURE WriteI(wr : Wr.T; q : INTEGER) =
  VAR
    ibuff := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    LOOPHOLE(ibuff, REF ARRAY OF INTEGER)[0] := q;
    Wr.PutString(wr, ibuff^)
  END WriteI;

PROCEDURE WriteLRA(wr : Wr.T; READONLY q : ARRAY OF LONGREAL) =
  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    FOR i := FIRST(q) TO LAST(q) DO
      LOOPHOLE(buff, REF ARRAY OF REAL)[0] := FLOAT(q[i],REAL);
      Wr.PutString(wr, buff^)
    END
  END WriteLRA;

BEGIN END UnsafeWriter.
