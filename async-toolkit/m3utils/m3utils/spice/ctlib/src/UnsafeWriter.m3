UNSAFE MODULE UnsafeWriter;
IMPORT Wr;
IMPORT UnsafeWr;
IMPORT Thread;
IMPORT TransferC;

PROCEDURE WriteI(wr : Wr.T; q : INTEGER)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    ibuff := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    LOOPHOLE(ibuff, REF ARRAY OF INTEGER)[0] := q;
    Wr.PutString(wr, ibuff^)
  END WriteI;

PROCEDURE WriteLRA(wr : Wr.T; READONLY q : ARRAY OF LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    buffD : ARRAY [ 0 .. 4 - 1] OF CHAR;
  BEGIN
    LOCK wr DO
      FOR i := FIRST(q) TO LAST(q) DO

        TransferC.d2c(q[i], ADR(buffD[0]));
        
        UnsafeWr.FastPutString(wr, buffD)
      END
    END
  END WriteLRA;

PROCEDURE WriteLRAAt(wr : Wr.T; READONLY q : ARRAY OF LONGREAL; at : CARDINAL)
  RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    buffD : ARRAY [ 0 .. 4 - 1] OF CHAR;
  BEGIN
    Wr.Seek(wr, at);
    LOCK wr DO
      FOR i := FIRST(q) TO LAST(q) DO

        TransferC.d2c(q[i], ADR(buffD[0]));
        
        UnsafeWr.FastPutString(wr, buffD)
      END
    END
  END WriteLRAAt;

BEGIN END UnsafeWriter.
