MODULE LibertyGroup;
IMPORT LibertyComponentChildren;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    write      := Write;
    children   := Children;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    <*ASSERT t.head # NIL*>
    <*ASSERT t.statements # NIL*>
    <*ASSERT wr # NIL*>
    <*ASSERT pfx # NIL*>

    Wr.PutChar(wr, '\n');
    Wr.PutText(wr, pfx);
    t.head.write(wr, "");
    Wr.PutText(wr, " { \n");
    FOR i := 0 TO t.statements.size() - 1 DO
      WITH s = t.statements.get(i) DO
        <*ASSERT s # NIL*>
        s.write(wr, pfx & "  ");
        Wr.PutChar(wr, '\n')
      END
    END;
    Wr.PutText(wr, pfx);
    Wr.PutText(wr, "}")
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    RETURN SeqBuilder.BuildSeq(t.head, t.statements)
  END Children;

BEGIN END LibertyGroup.
