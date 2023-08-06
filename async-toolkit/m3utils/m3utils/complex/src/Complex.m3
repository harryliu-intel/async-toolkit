MODULE Complex;
IMPORT Math;

VAR Pi := 4.0d0 * Math.atan(1.0d0);

PROCEDURE Real(READONLY t : T) : LONGREAL = BEGIN RETURN t.re END Real;

PROCEDURE Imag(READONLY t : T) : LONGREAL = BEGIN RETURN t.im END Imag;

PROCEDURE Construct(re, im : LONGREAL := 0.0d0) : T =
  BEGIN RETURN T { re, im } END Construct;

PROCEDURE Polar(arg, mag : LONGREAL) : T =
  BEGIN
    RETURN T { mag * Math.cos(arg), mag * Math.sin(arg) }
  END Polar;

(**********************************************************************)

PROCEDURE Conj(READONLY t : T) : T =
  BEGIN
    RETURN T { t.re, -t.im }
  END Conj;

PROCEDURE Arg(READONLY t : T) : LONGREAL =
  VAR
    ang : LONGREAL;
  BEGIN
    IF t.re = 0.0d0 THEN
      ang := Pi / 2.0d0
    ELSE
      ang := Math.atan(t.im / t.re)
    END;

    IF t.re < 0.0d0 THEN
      RETURN 2.0d0 * Pi - ang
    ELSE
      RETURN ang
    END
  END Arg;

PROCEDURE Mag(READONLY t : T) : LONGREAL =
  BEGIN
    RETURN Math.sqrt(t.re * t.re + t.im * t.im)
  END Mag;

PROCEDURE MagSq(READONLY t : T) : LONGREAL =
  BEGIN
    RETURN t.re * t.re + t.im * t.im
  END MagSq;

(**********************************************************************)

PROCEDURE Plus(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a.re + b.re, a.im + b.im }
  END Plus;

PROCEDURE Minus(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a.re - b.re, a.im - b.im }
  END Minus;

PROCEDURE Times(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a.re * b.re - a.im * b.im,
               a.im * b.re + a.re * b.im }
  END Times;

PROCEDURE Recip(READONLY t : T) : T =
  BEGIN
    (* 1/(a + ib) = (a - ib) / ((a + ib) * (a - ib)) 
                  = 1/(a^2 + b^2) * (a - ib) 
    *)
    RETURN Times(Construct(re := 1.0d0 / MagSq(t)), Conj(t))
  END Recip;
  
PROCEDURE Divide(READONLY a, b : T) : T =
  BEGIN
    RETURN Times(a, Recip(b))
  END Divide;

PROCEDURE Pow(READONLY a, b : T) : T =
  BEGIN
    RETURN Exp(Times(b, Log(a)))
  END Pow;

  
PROCEDURE Log(READONLY t : T) : T =
  BEGIN
    (* Log (e^a * cis b) = a + ib *)
    RETURN T { Math.log(MagSq(t)) / 2.0d0, Arg(t) } 
  END Log;

PROCEDURE Exp(READONLY t : T) : T =
  BEGIN
    (* e^(a + ib) = e^a * e^ib = e^a * cis b *)
    RETURN Times(Construct(re := Math.exp(t.re)),
                 Cis(t.im))
  END Exp;

PROCEDURE Cis(x : LONGREAL) : T =
  BEGIN
    RETURN T { Math.cos(x), Math.sin(x) }
  END Cis;


BEGIN END Complex.
