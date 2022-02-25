MODULE LibertyExpr;
IMPORT LibertyComponentChildren;
IMPORT LibertyComponent;
IMPORT LibertyNumber;
IMPORT Wr;
IMPORT Thread;
IMPORT LibertyComponentSeqBuilder AS SeqBuilder;

REVEAL
  T = LibertyComponent.T BRANDED Brand OBJECT
  OVERRIDES
    write    := Write;
    children := Children;
  END;

PROCEDURE Plus(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Plus, a := a, b := b)
  END Plus;
  
PROCEDURE Minus(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Minus, a := a, b := b)
  END Minus;
  
PROCEDURE Times(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Times, a := a, b := b)
  END Times;

PROCEDURE Div(a, b : T) : T =
  BEGIN
    RETURN NEW(Binary, op := Op.Div, a := a, b := b)
  END Div;

PROCEDURE Uminus(a : T) : T =
  BEGIN
    RETURN NEW(Unary, op := Op.Uminus, a := a)
  END Uminus;
  
PROCEDURE Uplus(a : T) : T =
  BEGIN
    RETURN NEW(Unary, op := Op.Uplus, a := a)
  END Uplus;
  
PROCEDURE Num(n : LibertyNumber.T) : T =
  BEGIN
    RETURN NEW(Const, type := Type.Num, val := n)
  END Num;

PROCEDURE Ident(n : TEXT) : T =
  BEGIN
    RETURN NEW(Const, type := Type.Ident, val := n)
  END Ident;

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted }=
  BEGIN
    Wr.PutText(wr, pfx);
    TYPECASE t OF
      Binary(b) =>
      Wr.PutChar(wr, '(');
      b.a.write(wr, "");
      Wr.PutChar(wr, OpSym[b.op]);
      b.b.write(wr, "");
      Wr.PutChar(wr, ')');
    |
      Unary(u)  =>
      Wr.PutChar(wr, OpSym[u.op]);
      u.a.write(wr, "")
    |
      Const(c)  =>
      CASE c.type OF
        Type.Num =>
        WITH num = NARROW(c.val, LibertyNumber.T) DO
          num.write(wr, "")
        END
      |
        Type.Ident =>
        Wr.PutText(wr, c.val)
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END Write;

PROCEDURE Children(t : T) : SeqBuilder.T =
  BEGIN
    TYPECASE t OF
      Binary(b) => RETURN SeqBuilder.BuildSeq(b.a, b.b)
    |
      Unary(u)  => RETURN SeqBuilder.BuildSeq(u.a)
    |
      Const(c) =>
      CASE c.type OF
        Type.Num => RETURN SeqBuilder.BuildSeq(c.val)
      |
        Type.Ident => RETURN SeqBuilder.BuildSeq()
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END Children;

BEGIN END LibertyExpr.
