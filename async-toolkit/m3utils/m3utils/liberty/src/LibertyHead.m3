MODULE LibertyHead;
IMPORT LibertyComponentChildren;
IMPORT LibertyParamList;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;
IMPORT LibertyComponentSeq;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write    := Write;
    children := Children;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    Wr.PutText(wr, t.ident);
    Wr.PutText(wr, " (");

    FOR i := 0 TO t.params.size() - 1 DO
      t.params.get(i).write(wr, "");
      IF i # t.params.size() - 1 THEN
        Wr.PutText(wr, t.sep);
        Wr.PutChar(wr, ' ')
      END
    END;
    
    Wr.PutText(wr, ")");
  END Write;

PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T =
  BEGIN
    RETURN NEW(T, ident := ident, params := params.params, sep := params.sep)
  END New;

PROCEDURE Children(t : T) : SeqBuilder.T =
  VAR
    res := NEW(LibertyComponentSeq.T).init();
  BEGIN
    FOR i := 0 TO t.params.size() - 1 DO
      res.addhi(t.params.get(i))
    END;
    RETURN res
  END Children;

BEGIN END LibertyHead.
