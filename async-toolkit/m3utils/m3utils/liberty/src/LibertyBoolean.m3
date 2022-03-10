MODULE LibertyBoolean;
IMPORT Thread;
IMPORT Wr;


PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Wr.PutText(wr, pfx);
    CASE t OF
      T.F => Wr.PutText(wr, "false")
    |
      T.T => Wr.PutText(wr, "true")
    END
  END Write;
  
BEGIN END LibertyBoolean.

  
