MODULE LibertyComplexAttr;
IMPORT LibertyComponentChildren;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
    children := Children;
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

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    RETURN SeqBuilder.BuildSeq(t.head)
  END Children;
  
BEGIN END LibertyComplexAttr.
