MODULE LibertyComplexAttr;
IMPORT LibertyHead;
IMPORT Wr;
IMPORT Thread;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    t.head.write(wr, "");
    IF t.semi THEN
      Wr.PutChar(wr, ';')
    END
  END Write;

BEGIN END LibertyComplexAttr.
