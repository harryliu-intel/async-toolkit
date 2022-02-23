MODULE LibertyAttrValExpr;
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
      String(s) =>
      Wr.PutChar(wr, '"'); Wr.PutText(wr, s.val); Wr.PutChar(wr, '"')
    |
      Boolean(b) => b.val.write(wr, "")
    |
      Expr(x) => x.val.write(wr, "")
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;

BEGIN END LibertyAttrValExpr.

  
