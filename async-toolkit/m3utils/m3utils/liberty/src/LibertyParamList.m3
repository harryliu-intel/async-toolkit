MODULE LibertyParamList;
IMPORT LibertyAttrValSeq;
IMPORT Wr;
IMPORT Thread;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
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
  
BEGIN END LibertyParamList.
