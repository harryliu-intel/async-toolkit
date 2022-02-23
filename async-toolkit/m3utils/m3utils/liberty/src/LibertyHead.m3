MODULE LibertyHead;
IMPORT LibertyComponent;
IMPORT LibertyParamList;
IMPORT Wr;
IMPORT Thread;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
    ident  : TEXT;
    params : LibertyParamList.T;
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
