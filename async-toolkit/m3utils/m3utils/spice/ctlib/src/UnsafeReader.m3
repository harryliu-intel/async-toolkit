UNSAFE MODULE UnsafeReader;
IMPORT Rd;
IMPORT Debug;
IMPORT Thread;
IMPORT Fmt;

VAR doDebug := Debug.DebugThis("UnsafeReader");
  

PROCEDURE ReadI(rd : Rd.T) : INTEGER
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    ibuff := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    WITH n = Rd.GetSub(rd, ibuff^) DO
      <*ASSERT n <= 4*>
      IF n # 4 THEN RAISE Rd.EndOfFile END
    END;
    RETURN LOOPHOLE(ibuff, REF ARRAY OF INTEGER)[0]
  END ReadI;

PROCEDURE ReadLRA(rd : Rd.T; VAR q : ARRAY OF LONGREAL)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    IF doDebug THEN
      Debug.Out("ReadLRA start, NUMBER(q)=" & Fmt.Int(NUMBER(q)))
    END;
    FOR i := FIRST(q) TO LAST(q) DO
      IF doDebug THEN
        Debug.Out("ReadLRA attempting read of 4 bytes")
      END;
      WITH n = Rd.GetSub(rd, buff^) DO
        <*ASSERT n <= 4*>
        IF n # 4 THEN RAISE Rd.EndOfFile END
      END;
      q[i] := FLOAT(LOOPHOLE(buff, REF ARRAY OF REAL)[0], LONGREAL);
    END
  END ReadLRA;

BEGIN END UnsafeReader.
