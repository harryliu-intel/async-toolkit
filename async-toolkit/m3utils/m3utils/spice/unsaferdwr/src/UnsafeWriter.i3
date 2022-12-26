INTERFACE UnsafeWriter;
IMPORT Wr;
IMPORT Thread;

PROCEDURE WriteI(wr : Wr.T; q : INTEGER)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteLRA(wr : Wr.T; READONLY q : ARRAY OF LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteLR(wr : Wr.T; q : LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteLRAAt(wr : Wr.T; READONLY q : ARRAY OF LONGREAL; index : CARDINAL)
  RAISES { Wr.Failure, Thread.Alerted };

END UnsafeWriter.
