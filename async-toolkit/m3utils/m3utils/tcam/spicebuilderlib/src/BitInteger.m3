MODULE BitInteger;
IMPORT Word, Debug;
FROM Fmt IMPORT Int, F;
IMPORT Bit;
IMPORT BigInt;

REVEAL
  SmallPromise = PubSmallPromise BRANDED OBJECT 
  OVERRIDES
    force := ForceSmallPromise;
  END;

PROCEDURE Small(z : INTEGER) : T =
  BEGIN RETURN NEW(SmallPromise, v := z) END Small;

PROCEDURE ForceSmallPromise(p    : SmallPromise; 
                            bits : CARDINAL) : Concrete =
  VAR
    res := NEW(Concrete, bits := NEW(REF ARRAY OF Bit.T, bits));
    w : Word.T := p.v; (* unsafe cast *)
    dbgStr := "";
  BEGIN
    FOR i := FIRST(res.bits^) TO LAST(res.bits^) DO
      IF i >= Word.Size THEN
        res.bits[i] := Word.Extract(w, Word.Size-1, 1)
      ELSE
        res.bits[i] := Word.Extract(w,           i, 1)
      END
    END;

    FOR i := LAST(res.bits^) TO FIRST(res.bits^) BY -1 DO
      dbgStr := dbgStr & " " & Int(res.bits[i])
    END;
    Debug.Out(F("Forcing small promise %s -> %s", Int(w), dbgStr));

    RETURN res
  END ForceSmallPromise;

PROCEDURE Big(z : BigInt.T; width : CARDINAL) : Concrete =
  VAR
    res := NEW(Concrete, bits := NEW(REF ARRAY OF Bit.T, width));
  BEGIN
    <* ASSERT BigInt.Sign(z) # -1 *> (* for now -- not sure what neg. would do *)
    
    FOR i := 0 TO width - 1 DO
      res.bits[i] := BigInt.ToInteger(BigInt.Mod(z, BigInt.Two));
      z           := BigInt.Div(z, BigInt.Two)
    END;
    RETURN res
  END Big;

PROCEDURE Format(t : T; base : CARDINAL) : TEXT =
  BEGIN
    RETURN BigInt.Format(ToBigInt(t), base)
  END Format;

PROCEDURE ToBigInt(t : T) : BigInt.T =
  BEGIN
    TYPECASE t OF
      SmallPromise(sp) =>
      RETURN BigInt.New(sp.v)
    |
      Concrete(c) =>
      VAR
        res := BigInt.Zero;
      BEGIN
        FOR i := NUMBER(c.bits^) - 1 TO 0 BY -1 DO
          CASE c.bits[i] OF
            0 => res := BigInt.Mul(res, BigInt.Two)
          |
            1 => res := BigInt.Add(BigInt.Mul(res, BigInt.Two),
                                   BigInt.One)
          END
        END;
        RETURN res
      END
    ELSE
      <*ASSERT FALSE*>
    END
  END ToBigInt;

BEGIN END BitInteger.
