MODULE LibertySimpleAttr;
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
    CASE t.syntax OF
      Syntax.ColonSemi, Syntax.Colon => Wr.PutText(wr, " : ")
    |
      Syntax.Eq                      => Wr.PutText(wr, " = ")
    END;
    t.attrValExpr.write(wr, "");
    CASE t.syntax OF
      Syntax.ColonSemi, Syntax.Eq    => Wr.PutText(wr, ";")
    |
      Syntax.Colon                   => (* skip *)
    END
  END Write;

BEGIN END LibertySimpleAttr.
