INTERFACE PolySegment16Serial;
IMPORT PolySegment16Seq AS Seq;
IMPORT Rd, Wr;
IMPORT Rep16;
IMPORT Thread;

EXCEPTION Error(TEXT);

PROCEDURE Write(wr : Wr.T; seq : Seq.T; min, max : LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE Read(rd : Rd.T; (* OUT *)seq : Seq.T; VAR header : Rep16.Header)
  RAISES { Rd.Failure, Rd.EndOfFile, Error, Thread.Alerted };

END PolySegment16Serial.
  
