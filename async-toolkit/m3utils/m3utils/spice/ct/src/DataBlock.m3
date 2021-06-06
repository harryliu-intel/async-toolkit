MODULE DataBlock;
IMPORT Wr, Rd;
IMPORT UnsafeWriter, UnsafeReader;
IMPORT Thread;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Bool;

<*FATAL Thread.Alerted*>
(* format assumes the readers and writers pertain to (seekable) disk files *)

VAR doDebug := Debug.DebugThis("DataBlock");

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
      (* first read block header, whch consists of Tag and Hollerith length *)

      IF doDebug THEN
        Debug.Out(F("DataBlock.ReadData tag=%s NUMBER(data)=%s",
                    Int(tag),
                    Int(NUMBER(data))))
      END;
      
      WITH readTag = UnsafeReader.ReadI(rd),
           count   = UnsafeReader.ReadI(rd) DO

        IF doDebug THEN
          Debug.Out(F("DataBlock.ReadData readTag=%s match=%s count=%s",
                      Int(readTag),
                      Bool(readTag = tag),
                      Int(count)))
        END;
        
        (* decide whether to skip or not *)
        IF tag = readTag THEN
          (* correct tag, report result back *)

          IF count > NUMBER(data) THEN
            Debug.Error(F("DataBlock format error : count %s > NUMBER(data) %s",
                          Int(count), Int(NUMBER(data))))
          END;

          UnsafeReader.ReadLRA(rd, SUBARRAY(data, 0, count));
          (* if we get Rd.EndOfFile here, then 
             the temp file that we are reading is corrupted *)
          
          RETURN count
        ELSE
          (* incorrect tag, just skip it *)
          WITH pos = Rd.Index(rd) DO
            Rd.Seek(rd, pos + 4 * count)
          END;
          RETURN 0
        END
      END
    END
  END ReadData;

BEGIN END DataBlock.
  
