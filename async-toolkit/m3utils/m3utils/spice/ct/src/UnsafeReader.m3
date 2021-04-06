UNSAFE MODULE UnsafeReader;
IMPORT Rd;

PROCEDURE ReadI(rd : Rd.T) : INTEGER =
  VAR
    ibuff := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    WITH n = Rd.GetSub(rd, ibuff^) DO
      <*ASSERT n = 4*>
    END;
    RETURN LOOPHOLE(ibuff, REF ARRAY OF INTEGER)[0]
  END ReadI;

PROCEDURE ReadLRA(rd : Rd.T; VAR q : ARRAY OF LONGREAL) =
  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    FOR i := FIRST(q) TO LAST(q) DO
      WITH n = Rd.GetSub(rd, buff^) DO
        <*ASSERT n = 4*>
      END;
      q[i] := FLOAT(LOOPHOLE(buff, REF ARRAY OF REAL)[0], LONGREAL);
    END
  END ReadLRA;

BEGIN END UnsafeReader.
