MODULE LibertyAttrVal;
IMPORT LibertyComponent;
IMPORT Wr;
IMPORT Thread;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    TYPECASE t OF
      Num(n) => n.val.write(wr, "")
    |
      SorI(s)=> s.val.write(wr, "")
    |
      Colon(c) => c.x.write(wr, ""); Wr.PutText(wr, " : "); c.y.write(wr, "")
    |
      Boolean(b) => b.val.write(wr, "")
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;

BEGIN END LibertyAttrVal.

  
