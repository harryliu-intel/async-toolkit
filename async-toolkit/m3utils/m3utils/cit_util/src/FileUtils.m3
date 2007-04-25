(* $Id$ *)

MODULE FileUtils;
IMPORT Wr, Rd, Thread, OSError, FileRd, FileWr;

PROCEDURE Copy(from, to : TEXT) RAISES { OSError.E, Thread.Alerted, Rd.Failure, Wr.Failure } =
  CONST
    BufSize = 16*1024;
  VAR
    buffer : ARRAY [0..BufSize-1] OF CHAR;
    rd := FileRd.Open(from);
    wr := FileWr.Open(to);
  BEGIN
    REPEAT
      WITH chunk = MIN(NUMBER(buffer), Rd.CharsReady(rd)),
           data = SUBARRAY(buffer,0,chunk) DO
        IF chunk = 0 THEN 
          Thread.Pause(0.001d0) (* stupid *)
        ELSE
          EVAL Rd.GetSub(rd, data); Wr.PutString(wr, data)
        END
      END
    UNTIL Rd.EOF(rd);
 
    Wr.Close(wr); Rd.Close(rd)
  END Copy;

BEGIN END FileUtils.
