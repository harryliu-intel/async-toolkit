MODULE Triangle3;
IMPORT P3;
FROM Fmt IMPORT F;

PROCEDURE MaxCos(READONLY t : T) : LONGREAL =
  VAR
    res := 0.0d0;
  BEGIN
    FOR i := FIRST(t) TO LAST(t) DO
      WITH s1 = P3.Minus(t[(i+1) MOD NUMBER(T)], t[i]),
           s2 = P3.Minus(t[(i+2) MOD NUMBER(T)], t[(i+1) MOD NUMBER(T)]),
           dot = P3.Dot(s1, s2),
           cos = ABS(dot / P3.Norm(s1) / P3.Norm(s2)) DO
        res := MAX(res, cos)
      END
    END;
    RETURN res
  END MaxCos;

PROCEDURE Format(READONLY t : T; prec : CARDINAL) : TEXT =
  BEGIN
    RETURN F("(%s %s %s)",
             P3.Format(t[0], prec := prec),
             P3.Format(t[1], prec := prec),
             P3.Format(t[2], prec := prec))
  END Format;

PROCEDURE FormatGnu(READONLY t : T) : TEXT =
  BEGIN
    RETURN F("%s\n%s\n%s\n%s\n\n",
             P3.FormatGnu(t[0]),
             P3.FormatGnu(t[1]),
             P3.FormatGnu(t[2]),
             P3.FormatGnu(t[0]))
  END FormatGnu;

PROCEDURE IntersectLine(lineOrigin    : P3.T;
                        lineDirection : P3.T;
                        VAR triangle  : T) : IntersectionResult =
  VAR
    E1 := P3.Minus(triangle[1], triangle[0]);
    E2 := P3.Minus(triangle[2], triangle[0]);
    det, invdet : LONGREAL;
    AO, DAO : P3.T;
    res : IntersectionResult;
  BEGIN
    res.N := P3.Cross(E1, E2);
    det := -P3.Dot(lineDirection, res.N);
    invdet := 1.0d0 / det;
    AO := P3.Minus(lineOrigin, triangle[0]);
    DAO := P3.Cross(AO, lineDirection);
    res.u := P3.Dot(E2, DAO) * invdet;
    res.v := -P3.Dot(E1, DAO) * invdet;
    res.t := P3.Dot(AO, res.N) * invdet;
    res.intersect := ABS(det) >= 1.0d-6 AND
                     res.u > 0.0d0 AND res.v > 0.0d0 AND res.u + res.v <= 1.0d0;
    RETURN res
  END IntersectLine;

BEGIN END Triangle3.
