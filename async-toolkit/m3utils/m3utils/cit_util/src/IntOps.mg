GENERIC MODULE IntOps(Int);
IMPORT Text;
IMPORT Scan AS yScan;
IMPORT Random;
IMPORT FloatMode, Lex;

<* FATAL FloatMode.Trap, Lex.Error *>

PROCEDURE ExtendedGCD(a, b: T; VAR aCoeff, bCoeff: T): T =
  BEGIN
    CASE Compare(a,b) OF
    | -1 => RETURN EGCD(a,b,aCoeff,bCoeff);
    | 0 => aCoeff:=Int.One; bCoeff:=Int.Zero; RETURN a;
    | 1 => RETURN EGCD(b,a,bCoeff,aCoeff);
    END;
  END ExtendedGCD;

(* assumes a<b *)
PROCEDURE EGCD(a,b: T; VAR aCoeff,bCoeff: T): T =
  BEGIN
    IF Equal(a, Int.Zero) THEN
      aCoeff := Int.Zero;
      bCoeff := Int.One;
      RETURN b;
    END;
    VAR
      r,q,g,rCoeff: T;
    BEGIN
      (* r := Mod(b, a);
         q := Div(b, a); *)
      Int.Divide(b, a, q, r);
      g := EGCD(r, a, rCoeff, aCoeff);
      bCoeff := rCoeff;
      aCoeff := Sub(aCoeff,Mul(rCoeff,q));
      RETURN g;
      (* g = aCoeff*a + rCoeff*r
           = aCoeff*a + rCoeff*(b - q*a)
           = (aCoeff-q*rCoeff)*a + rCoeff*b *)
    END;
  END EGCD;

PROCEDURE Random1(source: Random.T; largerThan: T) : T =
  VAR
    chunkBase := Int.New(16_100000);
    r := Int.New(source.integer(0, 16_100000));
  BEGIN
    IF Compare(largerThan, chunkBase) = -1 THEN
      RETURN r;
    END;
    RETURN Add(r,Mul(chunkBase,Random1(source, Div(largerThan,chunkBase))));
  END Random1;

PROCEDURE Rand(source: Random.T; lessThan: T) : T =
  BEGIN
    RETURN Mod(Random1(source, Int.Mul(lessThan, Int.New(99999))), lessThan);
  END Rand;

PROCEDURE Scan(t: TEXT; base : CARDINAL := 10) : T =
  VAR
    chunkDigits := 5;
    sb:=base*base;
    chunkBase := Int.New(sb*sb*base);
    l := Text.Length(t);
  BEGIN
    IF l <= chunkDigits THEN
      RETURN Int.New(yScan.Int(t, base));
    END;
    IF Text.GetChar(t, 0) = '-' THEN
      RETURN Negate(Scan(Text.Sub(t, 1), base));
    END;
    DEC(l, chunkDigits);
    RETURN Add(Mul(chunkBase, Scan(Text.Sub(t, 0, l),base)),
               Int.New(yScan.Int(Text.Sub(t, l),base)));
  END Scan;

PROCEDURE Exp(base, exp: T): T =
  BEGIN
    RETURN MultiExp(base, exp, Int.Zero, FALSE);
  END Exp;

PROCEDURE ModExp(base, exp, mod: T): T =
  <* FATAL NoneExists *>
  BEGIN
    IF Sign(exp) = -1 THEN
      RETURN ModExp(ModInverse(base, mod), Negate(exp), mod);
    END;
    RETURN MultiExp(base, exp, mod, TRUE);
  END ModExp;

PROCEDURE MultiExp(base, exp, mod: T; useMod: BOOLEAN): T =
  BEGIN
    CASE Sign(exp) OF
    | 0 => RETURN Int.One;
    | -1 => <* ASSERT FALSE *>
    | 1 =>
      VAR
        t: T;
      BEGIN
        IF Odd(exp) THEN
          t := Mul(base, MultiExp(base, Int.Add(exp, Int.New(-1)), 
                                  mod, useMod));
        ELSE
          t := Square(MultiExp(base, Int.Div(exp, Int.New(2)),
                               mod, useMod));
        END;
        IF useMod THEN
          t := Mod(t, mod);
        END;
        RETURN t;
      END;
    END;
  END MultiExp;

PROCEDURE ProbablyPrime(p: T): BOOLEAN =
  BEGIN
    RETURN Equal(ModExp(Int.New(19), Int.Add(p, Int.New(-1)), p), Int.One);
  END ProbablyPrime;

PROCEDURE Negate(a: T): T =
  BEGIN
    RETURN Mul(Int.New(-1), a);
  END Negate;

PROCEDURE Sub(a, b: T): T =
  BEGIN
    RETURN Add(a, Negate(b));
  END Sub;

PROCEDURE Square(a: T): T = BEGIN RETURN Mul(a,a); END Square;

PROCEDURE ModInverse(a, mod: T): T RAISES {NoneExists} =
  VAR
    aCoeff, bCoeff, g: T;
  BEGIN
    g := ExtendedGCD(a,mod,aCoeff,bCoeff);
    IF NOT Equal(g, Int.One) THEN RAISE NoneExists; END;
    RETURN aCoeff;
  END ModInverse;

PROCEDURE Odd(a: T): BOOLEAN =
  BEGIN
    RETURN Equal(Mod(a, Int.New(2)),Int.One);
  END Odd;

BEGIN END IntOps.
