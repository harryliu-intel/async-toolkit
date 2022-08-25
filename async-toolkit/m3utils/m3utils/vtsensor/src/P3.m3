MODULE P3;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT Math;
IMPORT Random;
IMPORT LongrealType;
IMPORT Word;

CONST LR = Fmt.LongReal;
      
PROCEDURE Format(READONLY p : T; prec : CARDINAL) : TEXT =
  BEGIN
    RETURN F("(%s %s %s)", LR(p[0], prec := prec), LR(p[1], prec := prec), LR(p[2], prec := prec))
  END Format;

PROCEDURE Minus(READONLY a, b : T) : T =
  VAR
    res : T;
  BEGIN
    FOR i := FIRST(T) TO LAST(T) DO
      res[i] := a[i] - b[i]
    END;
    RETURN res
  END Minus;

PROCEDURE Plus(READONLY a, b : T) : T =
  VAR
    res : T;
  BEGIN
    FOR i := FIRST(T) TO LAST(T) DO
      res[i] := a[i] + b[i]
    END;
    RETURN res
  END Plus;

PROCEDURE Dot(READONLY a, b : T) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(T) TO LAST(T) DO
      res := res + a[i] * b[i]
    END;
    RETURN res
  END Dot;    

PROCEDURE Cross(READONLY a, b : T) : T =
  BEGIN
    RETURN T { a[1] * b[2] - a[2] * b[1],
               a[2] * b[0] - a[0] * b[2],
               a[0] * b[1] - a[1] * b[0] }
  END Cross;

PROCEDURE ScalarMul(m : LONGREAL; a : T) : T =
  BEGIN
    RETURN T { m * a[0], m * a[1], m * a[2] }
  END ScalarMul;
  
PROCEDURE Norm(READONLY a : T) : LONGREAL =
  BEGIN
    RETURN Math.sqrt(Dot(a,a))
  END Norm;

PROCEDURE RandomDirection(rand : Random.T) : T =
  VAR
    h, theta, radius, x, y : LONGREAL;
  BEGIN
    IF rand = NIL THEN rand := defRand END;

    (* pick a random point on the surface of Archimedes's cylinder *)
    h     := rand.longreal(-1.0d0, +1.0d0);
    theta := rand.longreal(0.0d0, 2.0d0 * Pi);

    (* map the point from the cylinder to the unit sphere *)
    radius := Math.sqrt(1.0d0 - h * h);
    x := radius * Math.cos(theta);
    y := radius * Math.sin(theta);

    RETURN T { x, y, h }
  END RandomDirection;

PROCEDURE FormatGnu(READONLY p : T) : TEXT =
  BEGIN
    RETURN F("%s %s %s", LR(p[0]), LR(p[1]), LR(p[2]))
  END FormatGnu;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b
  END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  BEGIN
    RETURN Word.Plus(LongrealType.Hash(a[0]),
                     Word.Plus(LongrealType.Hash(a[1]),
                               LongrealType.Hash(a[2])))
  END Hash;
  
VAR
  defRand := NEW(Random.Default).init();
  Pi := 4.0d0 * Math.atan(1.0d0);
  
BEGIN END P3.
