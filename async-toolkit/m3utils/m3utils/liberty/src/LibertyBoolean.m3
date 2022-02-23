MODULE LibertyBoolean;
IMPORT Thread;
IMPORT Wr;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    CASE t.val OF
      FALSE => Wr.PutText(wr, "false")
    |
      TRUE => Wr.PutText(wr, "true")
    END
  END Write;
  
BEGIN END LibertyBoolean.

  
