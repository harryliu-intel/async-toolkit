MODULE LibertyGroup;
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
    <*ASSERT t.head # NIL*>
    <*ASSERT t.statements # NIL*>
    <*ASSERT wr # NIL*>
    <*ASSERT pfx # NIL*>
    
    Wr.PutText(wr, pfx);
    t.head.write(wr, "");
    Wr.PutText(wr, " {\n");
    FOR i := 0 TO t.statements.size() - 1 DO
      WITH s = t.statements.get(i) DO
        <*ASSERT s # NIL*>
        s.write(wr, pfx & "  ");
        Wr.PutChar(wr, '\n')
      END
    END;
    Wr.PutText(wr, pfx);
    Wr.PutText(wr, "}\n")
  END Write;

BEGIN END LibertyGroup.
