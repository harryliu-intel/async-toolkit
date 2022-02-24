MODULE LibertyHead;
IMPORT LibertyParamList;
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
    Wr.PutText(wr, t.ident);
    Wr.PutText(wr, " (");
    t.params.write(wr, "");
    Wr.PutText(wr, ")");
  END Write;


PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T =
  BEGIN
    RETURN NEW(T, ident := ident, params := params)
  END New;

BEGIN END LibertyHead.
