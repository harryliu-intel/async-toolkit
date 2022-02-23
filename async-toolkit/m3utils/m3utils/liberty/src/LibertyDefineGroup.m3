MODULE LibertyDefineGroup;
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
    Wr.PutText(wr, "define_group (");
    t.s[0].write(wr, "");
    Wr.PutText(wr, ", ");
    t.s[1].write(wr, "");
    Wr.PutText(wr, ");");
  END Write;

BEGIN END LibertyDefineGroup.

  
