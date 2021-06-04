MODULE DataBlock;
IMPORT Wr, Rd;
IMPORT UnsafeWriter, UnsafeReader;
IMPORT Thread;

<*FATAL Thread.Alerted*>
(* format assumes the readers and writers pertain to (seekable) disk files *)

PROCEDURE WriteData(wr                 : Wr.T;
                    tag                : CARDINAL; (* written if nonzero *)
                    READONLY block     : ARRAY OF LONGREAL)
  RAISES { Wr.Failure } =
  BEGIN
    IF tag = 0 THEN
      UnsafeWriter.WriteLRA(wr,block)
    ELSE
      UnsafeWriter.WriteI  (wr, tag);     
      UnsafeWriter.WriteI  (wr, NUMBER(block));
      UnsafeWriter.WriteLRA(wr, block)
    END
  END WriteData;

PROCEDURE DataCount(rd : Rd.T; tag : CARDINAL) : CARDINAL
  RAISES { Rd.Failure } =
  BEGIN
    <*ASSERT tag = 0*>
    (* only call this for TIME, for now *)
    WITH pos = Rd.Index(rd) DO
      Rd.Seek(rd, LAST(CARDINAL));
      WITH len = Rd.Index(rd) DO
        Rd.Seek(rd, pos);
        RETURN len DIV 4
      END
    END
  END DataCount;

PROCEDURE ReadData(rd        : Rd.T;
                   tag       : CARDINAL; (* must match if nonzero *)
                   VAR data  : ARRAY OF LONGREAL) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF tag = 0 THEN
      UnsafeReader.ReadLRA(rd, data);
      RETURN NUMBER(data)
    ELSE
      WITH readTag = UnsafeReader.ReadI(rd),
           count   = UnsafeReader.ReadI(rd) DO
        <*ASSERT count <= NUMBER(data)*>
        IF tag = readTag THEN
          UnsafeReader.ReadLRA(rd, SUBARRAY(data, 0, count));
          RETURN count
        ELSE
          WITH pos = Rd.Index(rd) DO
            Rd.Seek(rd, pos + 4 * count)
          END;
          RETURN 0
        END
      END
    END
  END ReadData;

BEGIN END DataBlock.
  
