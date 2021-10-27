MODULE ZeroOneX;

PROCEDURE FromBool(bool : BOOLEAN) : T =
  BEGIN
    IF bool THEN RETURN T.V1 ELSE RETURN T.V0 END
  END FromBool;

PROCEDURE Format(t : T) : TEXT =
  BEGIN RETURN Names[t] END Format;

BEGIN END ZeroOneX.
