MODULE LibertyHead;
IMPORT LibertyComponentChildren;
IMPORT LibertyParamList;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

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
    t.params.write(wr, "");
    Wr.PutText(wr, ")");
  END Write;


PROCEDURE New(ident : TEXT; params : LibertyParamList.T) : T =
  BEGIN
    RETURN NEW(T, ident := ident, params := params)
  END New;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    RETURN SeqBuilder.BuildSeq(t.params)
  END Children;

BEGIN END LibertyHead.
