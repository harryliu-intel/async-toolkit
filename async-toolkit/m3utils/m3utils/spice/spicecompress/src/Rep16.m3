MODULE Rep16;
IMPORT Word;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Wx;

CONST LR = LongReal;

PROCEDURE ExpandFixed(READONLY a : Array; VAR b : Array) =

  PROCEDURE Control() =
    (* read polynomial's control word from a *)
    BEGIN
      WITH cw = a[j] DO
        poly.order := Word.Extract(cw, 0, OrderBits);
        poly.count  := j + Word.RightShift(cw, OrderBits);
      END;
      INC(j)
    END Control;

  PROCEDURE SignExtend(x : Base) : INTEGER =
    CONST
      TopMask = Word.Shift(Word.RightShift(-1, Bits), Bits);
    BEGIN
      WITH sign = Word.Extract(x, Bits - 1, 1) DO
        RETURN Word.Or(x, sign * TopMask)
      END
    END SignExtend;
    
  PROCEDURE ReadPoly() =
    (* read polynomial from a array *)
    BEGIN
      IF poly.order = 0 THEN
        poly.c0 := a[j];
        INC(j)
      ELSE
        poly.c0 := y;
        FOR i := 1 TO poly.order DO
          poly.c[i] := SignExtend(a[j]);
          INC(j)
        END
      END
    END ReadPoly;
    
  PROCEDURE WritePoly() =
    (* write polynomial to b array *)
    BEGIN
      IF poly.order = 0 THEN
        (* if order is 0 we start a new freestanding segment *)
        FOR i := x TO x + poly.count - 1 DO
          b[i] := poly.c0
        END;
        INC(x, poly.count)
      ELSE
        (* order is not zero, our first new point is at 1 *)
        FOR i := 1 TO poly.count - 1 DO
          b[x - 1 + i] := FromFloat0(EvalPoly(poly, i))
        END
      END
   END WritePoly;

  VAR
    j     := 0;       (* pointer into a *)
    x     := 0;       (* pointer into b *)
    y     : INTEGER;  (* last value written into b *)

    poly  : T;
  BEGIN
    IF NUMBER(a) = 0 THEN RETURN END;

    Control();
    <*ASSERT poly.order = 0*> (* maybe should be exception, not assertion *)
    ReadPoly();
    WritePoly();

    WHILE j < NUMBER(a) DO
      Control();
      ReadPoly();
      WritePoly()
    END
  END ExpandFixed;

PROCEDURE Expand(READONLY t : T; VAR b : Array) =
  BEGIN
    <*ASSERT t.count = NUMBER(b)*>
     IF t.order = 0 THEN
       (* if order is 0 we start a new freestanding segment *)
       FOR i := 0 TO t.count - 1 DO
         b[i] := t.c0
       END
     ELSE
       (* order is not zero, our first new point is at 1 *)
       FOR i := 1 TO t.count - 1 DO
         b[i] := FromFloat0(EvalPoly(t, i))
       END
     END
  END Expand;

PROCEDURE EvalPoly(READONLY t : T; x0 : CARDINAL) : LONGREAL =
  VAR
    xf := FLOAT(x0, LONGREAL);
    yf := 0.0d0;
    y0f := ToFloat0(t.c0);
  BEGIN
    FOR p := t.order TO 1 BY -1 DO
      yf := yf * xf + ToFloat(t.c[p], p) 
    END;

    yf := yf * xf + y0f;

    RETURN MAX(0.0d0, MIN(yf, 1.0d0))
  END EvalPoly;
  
PROCEDURE FromSingle(x : LONGREAL) : T =
  BEGIN
    RETURN T { 1, 0, FromFloat0(x), Zero }
  END FromSingle;

  (**********************************************************************)
  
CONST Max0 = FLOAT(LAST(Base),LONGREAL);
      Min0 = FLOAT(FIRST(Base),LONGREAL);
  
PROCEDURE FromFloat0(x : LONGREAL) : Unsigned =
  BEGIN
    (* x in range [0,1] *)
    WITH try = ROUND(x * Max0 + Min0) DO
      RETURN MIN(MAX(try, FIRST(Unsigned)), LAST(Unsigned))
    END
  END FromFloat0;

PROCEDURE ToFloat0(x : Base) : LONGREAL =
  BEGIN
    WITH f = FLOAT(x, LONGREAL) DO
      RETURN (f - Min0) / Max0
    END
  END ToFloat0;


CONST
  Range = ARRAY [ 1 .. LAST(Order) ] OF LONGREAL {
  2.0d0,
  1.0d0,
  1.0d0 / FLOAT(Word.Shift(1, 1), LONGREAL),
  1.0d0 / FLOAT(Word.Shift(1, 2), LONGREAL),
  1.0d0 / FLOAT(Word.Shift(1, 3), LONGREAL),
  1.0d0 / FLOAT(Word.Shift(1, 4), LONGREAL),
  1.0d0 / FLOAT(Word.Shift(1, 5), LONGREAL)
  };

  SRange = FLOAT(LAST(Signed) - FIRST(Signed), LONGREAL);
  SMin   = FLOAT(FIRST(Signed), LONGREAL);
  
  (* bottom of Signed range is -1, top of Signed range is +1 *)
      
PROCEDURE ToFloat(x : Signed; pow : [1..LAST(Order)]) : LONGREAL =
  BEGIN
    WITH f      = FLOAT(x, LONGREAL),
         normed = (f - SMin) / SRange, (* [0,1] *)
         signed = normed * 2.0d0 - 1.0d0 (* [-1,1] *)
     DO
      RETURN signed * Range[pow]
    END
  END ToFloat;

PROCEDURE FromFloat(x : LONGREAL; pow : [1..LAST(Order)]) : Signed =
  BEGIN
    WITH normed  = (x / Range[pow] + 1.0d0) / 2.0d0, (* [0,1] *)
         sranged = normed * SRange + SMin,
         try     = ROUND(sranged) DO
      RETURN MIN(MAX(try, FIRST(Signed)), LAST(Signed))
    END
  END FromFloat;

PROCEDURE Format(READONLY a : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    Wx.PutText(wx, F("{ count=%s order=%s ", Int(a.count), Int(a.order)));
    IF a.order = 0 THEN
      Wx.PutText(wx, F("c0=%s (%s) ", Int(a.c0), LR(ToFloat0(a.c0))))
    ELSE
      Wx.PutText(wx, "{ ");
      FOR i := 1 TO a.order DO
        Wx.PutText(wx, F("%s (%s) ", Int(a.c[i]), LR(ToFloat(a.c[i], i))))
      END;
      Wx.PutText(wx, "} ");
    END;
    Wx.PutText(wx, "} ");
    RETURN Wx.ToText(wx)
  END Format;

BEGIN END Rep16.  
