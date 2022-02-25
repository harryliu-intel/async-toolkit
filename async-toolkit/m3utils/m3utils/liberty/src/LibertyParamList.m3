MODULE LibertyParamList;
IMPORT LibertyComponentChildren;
IMPORT LibertyAttrValSeq;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
    children := Children;
  END;

PROCEDURE New() : T =
  BEGIN
    RETURN NEW(T,
               sep    := NIL,
               params := NEW(LibertyAttrValSeq.T).init())
  END New;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);

    FOR i := 0 TO t.params.size() - 1 DO
      t.params.get(i).write(wr, "");
      IF i # t.params.size() - 1 THEN
        Wr.PutText(wr, t.sep);
        Wr.PutChar(wr, ' ')
      END
    END
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    RETURN SeqBuilder.BuildSeq(t)
  END Children;
  
BEGIN END LibertyParamList.
