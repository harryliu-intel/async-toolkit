MODULE LibertyAttrVal;
IMPORT LibertyComponentChildren;
IMPORT LibertyComponent;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;
IMPORT LibertyBoolean;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    write    := Write;
    children := Children;
  END;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    TYPECASE t OF
      Num(n)     => n.val.write(wr, "")
    |
      SorI(s)    => s.val.write(wr, "")
    |
      Colon(c)   => c.x.write(wr, ""); Wr.PutText(wr, " : "); c.y.write(wr, "")
    |
      Boolean(b) => LibertyBoolean.Write(b.val, wr, "")
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    TYPECASE t OF
      Num(n)     => RETURN SeqBuilder.BuildSeq(n.val)
    |
      SorI(s)    => RETURN SeqBuilder.BuildSeq(s.val)
    |
      Colon(c)   => RETURN SeqBuilder.BuildSeq(c.x, c.y)
    |
      Boolean    => RETURN SeqBuilder.BuildSeq()
    ELSE
      <*ASSERT FALSE*>
    END
  END Children;

BEGIN END LibertyAttrVal.

  
